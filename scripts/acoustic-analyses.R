# Last updated 01-13-25 snc


### STATUS/TODO ###

# Pipeline currently runs!
# 1. use perception from 1a-c to predict effects of cog and pen
# 2. predict likely effects on exposure
# 3. use that data to inform some ideal adaptors

# Problems!

# what model structure to use? I had some weird rhat issues with the brm()?
# also, predict() seems not to allow newdata which doesn't have columns
# for each effect of the model (including re structure)

# For all thresholds I've checked (see lines 200ish), there are as many 
# implausible acoustics in the PIH as PIM condition, contra our a priori prediction

# The MVBeliefUpdatr functions to plot categorization curves aren't working for me.
# I think something to do with needing to feed in noise_treatment = "no_noise"?
# anyway, many errors of the type "Input x and m are not of compatible dimensions."






rm(list=ls(all.names=TRUE))
# Packages
library(brms)
library(lme4)
library(rlang)
library(assertthat)

library(tidyverse)

library(MVBeliefUpdatr)

library(cowplot)

setwd("/Users/shawncummings/Documents/GitHub/Causal_Inference_in_Speech_Perception/")

# just for prep_for_analysis()
source("scripts/functions.R")

# Data import
# acoustics
s <- read_tsv("./materials/Annotated/s_segments.txt")
sh <- read_tsv("./materials/Annotated/sh_segments.txt")
test <- read_tsv("./materials/Annotated/test_segments.txt")

# perception from all experiments
d.perception <- read.csv("./data/CISP_data.csv") %>%
  # get rid of old cues, I don't trust those 
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

# and aggregate just over 1a-c
p3 <- d.test %>%
  filter(Experiment.internalName %in% c("NORM A", "NORM B", "NORM C")) %>%
  ggplot(aes(x = cog,
             y = Response.ASHI,
             linetype = Condition.Test.Pen)) +
  stat_summary(geom = "pointrange",
               fun.data = mean_cl_boot)
p3

# now let's use this data do get a model of the effect of pen!

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
            Condition.Test.Pen * cog +
            (1 + Condition.Test.Pen * cog | ParticipantID),
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
# (which should leave cog unaffected by will change the prediction)
d.exp <- d.acoustics %>%
  filter(type != "test") %>%
  mutate(Condition.Test.Pen = "H")

d.exp2 <- d.exp %>%
  mutate(Condition.Test.Pen = "M")

d.exp <- rbind(d.exp, d.exp2)

# predict responses
# when running a bigger model, or any with random slopes by subject,
# this function complains that subjects aren't in the new data...
predict <- as.data.frame(
  1 / (1 + exp(-predict(m3,
                       newdata = d.exp))))

d.exp <- cbind(d.exp, predict) %>%
  rename(predict.acoustics = `1/(1 + exp(-predict(m3, newdata = d.exp)))`,
         segment.lexical = segment) %>%
  mutate(
    # define some cutoff probabilities
    cutoff_s = 0.6,
    cutoff_sh = 0.4,
    Condition = case_when(
      segment.lexical == "S" & type == "typical" ~ "SH-bias",
      segment.lexical == "S" & type == "shifted" ~ "S-bias",
      segment.lexical == "SH" & type == "typical" ~ "S-bias",
      segment.lexical == "SH" & type == "shifted" ~ "SH-bias"),
    believable = case_when(
      segment.lexical == "S" & predict.acoustics > cutoff_s ~ "no",
      segment.lexical == "S" & predict.acoustics <= cutoff_s ~ "yes",
      segment.lexical == "SH" & predict.acoustics < cutoff_sh ~ "no",
      segment.lexical == "SH" & predict.acoustics >= cutoff_sh ~ "yes"
    ))

d.exp %>%
  group_by(Condition.Test.Pen, believable, Condition) %>%
  tally()

# 
p4 <- d.exp %>%
  ggplot(aes(x = cog,
           y = predict.acoustics,
           color = Condition.Test.Pen,
           shape = type)) +
  scale_shape_manual(values = c(1, 19)) +
  geom_point() +
  facet_wrap(~segment.lexical,
             nrow = 2)
p4

p6 <- d.exp  %>%
  ggplot(aes(x = cog,
             y = predict.acoustics,
             color = believable,
             shape = type)) +
  scale_shape_manual(values = c(1, 19)) +
  geom_point() +
  facet_wrap(Condition.Test.Pen~segment.lexical)
p6

# filter d.exp to only include believable tokens
d.exp <- filter(d.exp, believable == "yes")

# make an IO
IO <- make_MVG_ideal_observer_from_data(
  data = d.exp %>%
    filter(Condition.Test.Pen == "H",
           type == "typical"),
  category = "segment.lexical",
  cues = "cog",
  verbose = T)

# and make an IA based on it
# and a random kappa and nu... could/should try others!
# pretty low for now just so effects are more visually evident
IA <- IO %>%
  mutate(kappa = 64,
         nu = 64,
         prior_kappa = 64,
         prior_nu = 64,
         S = get_S_from_expected_Sigma(Sigma, nu)) %>%
  rename(m = mu) %>%
  select(!Sigma) %>%
  arrange(category)

is.NIW_belief(IA)

# check classic effect
posterior_s_bias <- update_NIW_ideal_adaptor_incrementally(
  prior = IA,
  exposure = filter(d.exp,
                    Condition == "S-bias",
                    Condition.Test.Pen == "H"),
  exposure.category = "segment.lexical",
  exposure.cues = "cog")

posterior_sh_bias <- update_NIW_ideal_adaptor_incrementally(
  prior = IA,
  exposure = filter(d.exp,
                    Condition == "SH-bias",
                    Condition.Test.Pen == "H"),
  exposure.category = "segment.lexical",
  exposure.cues = "cog")

p_priors <- plot_expected_categories_density_1D(filter(IA),
                                                xlim = c(4000, 7000))

p_sh <- plot_expected_categories_density_1D(filter(posterior_sh_bias, observation.n == max(observation.n)),
                                          xlim = c(4000, 7000))

p_s <- plot_expected_categories_density_1D(filter(posterior_s_bias, observation.n == max(observation.n)),
                                            xlim = c(4000, 7000))

p7 <- plot_grid(p_priors,
                p_sh,
                p_s,
                labels = c("priors", "sh-bias", "s-bias"),
                ncol = 1)
p7

# and now with PIM
posterior_s_bias_pim <- update_NIW_ideal_adaptor_incrementally(
  prior = IA,
  exposure = filter(d.exp,
                    Condition == "S-bias",
                    Condition.Test.Pen == "M"),
  exposure.category = "segment.lexical",
  exposure.cues = "cog")

posterior_sh_bias_pim <- update_NIW_ideal_adaptor_incrementally(
  prior = IA,
  exposure = filter(d.exp,
                    Condition == "SH-bias",
                    Condition.Test.Pen == "M"),
  exposure.category = "segment.lexical",
  exposure.cues = "cog")

p_sh_pim <- plot_expected_categories_density_1D(filter(posterior_sh_bias_pim, observation.n == max(observation.n)),
                                            xlim = c(4000, 7000))

p_s_pim <- plot_expected_categories_density_1D(filter(posterior_s_bias_pim, observation.n == max(observation.n)),
                                           xlim = c(4000, 7000))


p8 <- plot_grid(p_priors,
                p_s,
                p_s_pim,
                labels = c("priors", "s-bias-PIH", "s-bias-PIM"),
                ncol = 1)
p8

p9 <- plot_grid(p_priors,
                p_sh,
                p_sh_pim,
                labels = c("priors", "sh-bias-PIH", "sh-bias-PIM"),
                ncol = 1)
p9

summary <- rbind(
  mutate(IA,
         bias = "prior", pen = "NA",
         observation.n = 40),
  mutate(posterior_s_bias_pim,
         bias = "s", pen = "m"),
  mutate(posterior_sh_bias_pim,
         bias = "sh", pen = "m"),
  mutate(posterior_s_bias,
         bias = "s", pen = "h"),
  mutate(posterior_sh_bias,
         bias = "sh", pen = "h")) %>%
  select(category, m, S, bias, pen)
summary



# this doesn't work... 
plot_expected_categorization_function_1D(IA,
                                         data.test = d.acoustics.test,
                                         xlim = c(1000, 10000),
                                         target_category = 2)


# let's do it the long way 


IA_func <- get_categorization_function_from_NIW_ideal_adaptor(IA,
                                                              noise_treatment = "no_noise")

IA_linspace <- as.data.frame(c(5000:7000)) %>%
  rename(cog = `c(5000:7000)`) %>%
  mutate(prob = as.numeric(IA_func(cog)))


ggplot(aes(x = ))

ssss <- as.numeric(IA_func(5500))


plot_IA_func <- ggplot() +
  geom_function()
  IA_func()

# let's try manual
IA_func2 <- get_NIW_categorization_function(
  ms = c(6541, 4465),
  Ss = c(9709768, 3326227),
  kappas = c(64, 64),
  nus = c(64, 64),
  noise_treatment = "no_noise")

IA_func2(5500)


