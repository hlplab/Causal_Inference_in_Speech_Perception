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
library(magrittr)

library(MVBeliefUpdatr)

library(cowplot)

setwd("/Users/shawncummings/Documents/GitHub/Causal_Inference_in_Speech_Perception/scripts/")
# setwd("scripts/")

# just for prep_for_analysis()
source("functions.R")

# Data import acoustics
# including just CoG for now
d.acoustics <- 
  rbind(
    read_tsv("../materials/Annotated/s_segments.txt"),
    read_tsv("../materials/Annotated/sh_segments.txt"),
    read_tsv("../materials/Annotated/test_segments.txt")) %>%
  filter(segment %in% c("S", "SH", "?")) %>%
  mutate(type = case_when(
    grepl("50", source) == T ~ "shifted", 
    grepl("_0", source) == T ~ "typical",
    grepl("test", source) == T ~ "test"),
    cog = as.numeric(cog)) %>%
  select(segment, word, cog, type)

# perception from all experiments
d.perception <- 
  read.csv("../data/CISP_data.csv") %>%
  # run formatting from SI
  mutate(
    across(
      .cols = c(starts_with("Participant"), -Participant.Age,
                Phase, starts_with("Condition"), -Condition.Test.Audio,
                starts_with("Item"), starts_with("Talker"),
                Response, Task, Exclude_Participant.Reason),
      .fns = factor),
    across(
      .cols = c(Participant.Age, Condition.Test.Audio, Response.RT, starts_with("Duration")),
      .fns = as.numeric),
    across(
      .cols = c(Item.isCatchTrial, Response.CatchTrial),
      .fns = as.logical)) %>%
  # get rid of old cues, I don't trust those 
  select(!starts_with("cue")) %>%
  # remove occluder manipulations
  filter(!Experiment.internalName %in% c("NORM D", "NORM E")) %>%
  # include an index for the step a la original liu & jaeger synth,
  # 1-31 S high
  mutate(LJ_step = as.numeric(gsub("_", "", str_sub(Item.Filename, 0, 2))))


# quick sanity check
d.acoustics %>%
  group_by(segment, type) %>%
  summarise(mean = mean(cog),
            sd = sd(cog))

p1 <- d.acoustics %>%
  ggplot(aes(x = cog,
             fill = segment)) +
  geom_histogram(bins = 20, position = "identity", alpha = .5) +
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
             y = Response.ASHI)) +
  stat_summary(geom = "pointrange",
               fun.data = mean_cl_boot, aes(shape = Condition.Test.Pen)) +
  stat_summary(geom = "line",
               fun = mean, aes(linetype = Condition.Test.Pen)) +
  facet_wrap(~Experiment.internalName)
p2

# and aggregate just over 1a-c
d.test %<>%
  run_exclusions(c("CISP-1a", "CISP-1b", "CISP-1c")) %>%
  excludeData() %>%
  filter(!is.na(Response.ASHI)) %>%
  filter(Experiment.internalName %in% c("NORM A", "NORM B", "NORM C"))

# check relation between continuum steps and cog
d.test %>%
  ggplot(aes(x = Condition.Test.Audio,
             y = cog)) +
  geom_point() 

p3 <- 
  d.test %>%
  ggplot(aes(x = cog,
             y = Response.ASHI,
             linetype = Condition.Test.Pen)) +
  stat_summary(geom = "pointrange",
               fun.data = mean_cl_boot) +
  stat_summary(geom = "line",
               fun = mean, aes(linetype = Condition.Test.Pen)) 
p3

# now let's use this data to get a model of the effect of pen!

# abbreviated model, including just acoustics (linear, not monotonic) and pen
m1 <- 
  brm(
    formula = 
      Response.ASHI ~ 1 + Experiment * Condition.Test.Pen * cog_gs +
      # Including experiment since there's clear evidence that the 
      # selection of test steps affects how participants interpret the input 
      # (presumable incl. cog). 
      (1 + Condition.Test.Pen * cog_gs | ParticipantID),
    data = 
      d.test %>%
      # data is already filtered to Exp 1a-c but just making sure
      ungroup() %>%
      filter(Experiment %in% c("CISP-1a", "CISP-1b", "CISP-1c")) %>%
      prep_for_analysis() %>%
      # scaling cog to keep effect of priors same as for other predictors
      mutate(cog_gs = (cog - mean(cog)) / (2 * sd(cog))),
    family = "bernoulli",
    prior = my_priors,
    sample_prior = "yes",
    backend = "cmdstanr",
    chains = 4, 
    warmup = 2000,
    iter = 4000,
    thin = 2,
    control = list(adapt_delta = .95, max_treedepth = 15),
    cores = min(parallel::detectCores(), 4), 
    threads = threading(threads = 2),
    file = "../models/Exp-CISP-1a-c-acoustics")
summary(m1)

# create sample exposure data to test
# we want to check each exposure item with and without PIM
# (which should leave cog unaffected but will change the prediction)
d.exp <- d.acoustics %>%
  filter(type != "test") %>%
  mutate(Condition.Test.Pen = "H")

d.exp2 <- d.exp %>%
  mutate(Condition.Test.Pen = "M")


d.exp <- rbind(d.exp, d.exp2) %>%
  mutate(cog_gs = (cog - mean(cog)) / (2 * sd(cog)),
         ParticipantID = "1",
         Experiment = "CISP-1c")

# predict responses
# scaling cog_gs separately for each dataframe is potentially an issue?
# the model we are predicting from has random effects for Participant,
# for now let's just set ParticipantID to a constant.
# The model is also making different predictions for different levels of 
# Experiment... for now using CISP-1c. 

predict <- as.data.frame(
  1 / (1 + exp(-predict(m1,
                       newdata = d.exp,
                       allow_new_levels = T))))

d.exp <- cbind(d.exp, predict) %>%
  rename(predict.acoustics = Estimate,
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
                                                xlim = c(4000, 8000))

p_sh <- plot_expected_categories_density_1D(filter(posterior_sh_bias, observation.n == max(observation.n)),
                                          xlim = c(4000, 8000))

p_s <- plot_expected_categories_density_1D(filter(posterior_s_bias, observation.n == max(observation.n)),
                                            xlim = c(4000, 8000))

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
                                            xlim = c(4000, 8000))

p_s_pim <- plot_expected_categories_density_1D(filter(posterior_s_bias_pim, observation.n == max(observation.n)),
                                           xlim = c(4000, 8000))


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
                                         xlim = c(0, 10000),
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


