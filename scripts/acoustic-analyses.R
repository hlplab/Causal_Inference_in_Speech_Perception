# Last updated 01-10-25 snc


# Packages
library(brms)
library(lme4)
library(rlang)

library(tidyverse)


setwd("/Users/shawncummings/Documents/GitHub/Causal_Inference_in_Speech_Perception/")

source("scripts/functions.R")

# Data import
# acoustics
s <- read_tsv("./materials/Annotated/s_segments.txt")
sh <- read_tsv("./materials/Annotated/sh_segments.txt")
test <- read_tsv("./materials/Annotated/test_segments.txt")

# perception from all experiments
d.perception <- read.csv("./data/CISP_data.csv") %>%
  # get rid of cues, we don't trust those 
  select(!starts_with("cue")) %>%
  # remove occluder manipulations
  filter(!Experiment.internalName %in% c("NORM D", "NORM E")) %>%
  # include an index for the step a la original liu & jaeger synth,
  # 1-31 S high
  mutate(LJ_step = as.numeric(gsub("_", "", str_sub(Item.Filename, 0, 2))))

# dataframe including just CoG for now
d.acoustics <- rbind(s, sh, test) %>%
  filter(segment %in% c("S", "SH", "?")) %>%
  mutate(type = case_when(
    grepl("50", source) == T ~ "shifted", 
    grepl("_0", source) == T ~ "typical",
    grepl("test", source) == T ~ "test"),
         cog = as.numeric(cog)) %>%
  select(segment, word, cog, type)

# quick sanity check
d.acoustics %>%
  group_by(segment, type) %>%
  summarise(mean = mean(cog),
            sd = sd(cog))

p1 <- d.acoustics %>%
  ggplot(aes(x = cog,
             fill = segment)) +
  geom_histogram(bins = 20) +
  facet_wrap(~type)
p1
# as expected, typical SH is lowest, then both shifted, and typical S is highest.

## add cog from test items 
d.acoustics.test <- d.acoustics %>%
  filter(type == "test") %>%
  rename(LJ_step = word) %>%
  select(cog, LJ_step)

d.test <- merge(filter(d.perception, Phase == "test"), d.acoustics.test,
                by = "LJ_step")

# Visualize effect of pen
# across all experiments
p2 <- d.test %>%
  ggplot(aes(x = cog,
             y = Response.ASHI,
             linetype = Condition.Test.Pen)) +
  stat_summary(geom = "pointrange",
               fun.data = mean_cl_boot) +
  facet_wrap(~Experiment.internalName)
p2

# and aggregate over 1a-c
p3 <- d.test %>%
  filter(Experiment.internalName %in% c("NORM A", "NORM B", "NORM C")) %>%
  ggplot(aes(x = cog,
             y = Response.ASHI,
             linetype = Condition.Test.Pen)) +
  stat_summary(geom = "pointrange",
               fun.data = mean_cl_boot)
p3

# models ###
d.test$cog.scaled <- scale(d.test$cog)



# full model structure
# m1 <- brm(Response.ASHI ~
#             Condition.Test.OriginalLabel * Condition.Test.Pen * mo(Block) * mo(round(cog, 0)) * Experiment +
#             (1 + Condition.Test.OriginalLabel * Condition.Test.Pen * mo(round(cog, 0)) | ParticipantID),
#           data = d.test %>%
#             filter(Experiment.internalName %in% c("NORM A", "NORM B", "NORM C")) %>%
#             prep_for_analysis(),
#           family = "bernoulli",
#           prior = my_priors,
#           sample_prior = "yes",
#           backend = "cmdstanr",
#           chains = 4,
#           warmup = 1000,
#           iter = 2000,
#           control = .9,
#           cores = min(parallel::detectCores(), 4),
#           threads = threading(threads = 4))
# summary(m1)

# abbreviated model, including just acoustics (linear, not monotonic) and pen
m2 <- brm(Response.ASHI ~ 
            Condition.Test.Pen * cog.scaled +
            (1 + Condition.Test.Pen * cog.scaled | ParticipantID),
          data = d.test %>%
            filter(Experiment.internalName %in% c("NORM A", "NORM B", "NORM C")) %>%
            prep_for_analysis(),
          family = "bernoulli",
          prior = my_priors,
          sample_prior = "yes",
          backend = "cmdstanr",
          chains = 4, 
          warmup = 1000,
          iter = 2000,
          control = .9,
          cores = min(parallel::detectCores(), 4), 
          threads = threading(threads = 4))
summary(m2)

save(m2, file = "models/Exp-CISP-1a-c-acoustics.rds")
# for some reason this gave us crap results with bit Rhats...

# moving to frequentist model just to establish the pipeline
m3 <- glm(Response.ASHI ~ Condition.Test.Pen * cog,
            data = d.test %>%
              filter(Experiment.internalName %in% c("NORM A", "NORM B", "NORM C")) %>%
              prep_for_analysis(),
            family="binomial")
summary(m3)
# singularity... but whatever


# m4 <- glmer(Response.ASHI ~
#               Condition.Test.OriginalLabel * Condition.Test.Pen * Block * cog.scaled * Experiment +
#               (1 + Condition.Test.OriginalLabel * Condition.Test.Pen * cog.scaled | ParticipantID),
#             data = d.test %>%
#               filter(Experiment.internalName %in% c("NORM A", "NORM B", "NORM C")) %>%
#               prep_for_analysis(),
#             family="binomial",
#             control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=800000)))
# summary(m4)

# create sample exposure data to test
# we want to check each exposure item with and without PIM
d.exp <- d.acoustics %>%
  filter(type != "test") %>%
  mutate(Condition.Test.Pen = "H")

d.exp2 <- d.exp %>%
  mutate(Condition.Test.Pen = "M")

d.exp <- rbind(d.exp, d.exp2) %>%
  arrange(Condition.Test.Pen, cog)


predict <- as.data.frame(
  predict(m3,
          newdata = d.exp))


p4 <- cbind(d.exp, predict) %>%
  rename(predict = `predict(m3, newdata = d.exp)`) %>%
  ggplot(aes(x = cog,
             y = predict,
             color = Condition.Test.Pen)) +
  geom_point()
p4





