###

library(brms)



source("scripts/functions.R")

setwd("/Users/shawncummings/Documents/GitHub/Causal_Inference_in_Speech_Perception/")


s <- read_tsv("./materials/Annotated/s_segments.txt")
sh <- read_tsv("./materials/Annotated/sh_segments.txt")
test <- read_tsv("./materials/Annotated/test_segments.txt")

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

# read in data from all experiments
d.perception <- read.csv("./data/CISP_data.csv") %>%
  # get rid of cues, we don't trust those 
  select(!starts_with("cue")) %>%
  # remove occluder manipulations
  filter(!Experiment.internalName %in% c("NORM D", "NORM E")) %>%
  # include an index for the step a la original liu & jaeger synth,
  # 1-31 S high
  mutate(LJ_step = as.numeric(gsub("_", "", str_sub(Item.Filename, 0, 2))))

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


d.test2 <- d.test %>%
  prep_for_analysis() %>%
  filter(Experiment.internalName %in% c("NORM A", "NORM B", "NORM C"))

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

# abbreviated model, including just acoustics and pen
m2 <- brm(Response.ASHI ~ 
            Condition.Test.Pen * mo(round(cog, 0)) +
            (1 + Condition.Test.Pen * mo(round(cog, 0)) | ParticipantID),
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
summary(m1)


