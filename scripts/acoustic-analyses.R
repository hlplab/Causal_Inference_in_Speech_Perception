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

# devtools::install_github("hlplab/MVBeliefUpdatr")
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

# now let's use this data do get a model of the effect of pen!

# abbreviated model, including just acoustics (linear, not monotonic) and pen
d.test %<>%
  # data is already filtered to Exp 1a-c but just making sure
  ungroup() %>%
  filter(Experiment %in% c("CISP-1a", "CISP-1b", "CISP-1c")) %>%
  # scaling cog to keep effect of priors same as for other predictors
  mutate(cog_gs = (cog - mean(cog)) / (2 * sd(cog)))

stats.cog <- 
  d.test %>%
  summarise(mean = mean(cog), sd = sd(cog))

m1 <- 
  brm(
    formula = 
      Response.ASHI ~ 1 + Experiment * Condition.Test.Pen * Condition.Test.OriginalLabel * cog_gs +
      # Including experiment since there's clear evidence that the 
      # selection of test steps affects how participants interpret the input 
      # (presumable incl. cog). 
      (1 + Condition.Test.Pen * Condition.Test.OriginalLabel * cog_gs | ParticipantID),
    data = 
      d.test %>%
      prep_for_analysis(),
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
#
# NOTE: THIS APPROACH MODELS THE PREDICTED EFFECT OF THE PEN ON THE PROBABILITY OF 
# PERCEIVED A SEGMENT AS INTENDED. HOWEVER, IT MIGHT BE BETTER TO INSTEAD MODEL THE
# EFFECT ON THE PERCEIVED COG?
d.exp <- 
  d.acoustics %>%
  filter(type != "test") %>%
  # Adding information about the visual bias, which---for exposure items---was always
  # the same as the intended category (i.e., segment)
  rename(Condition.Test.OriginalLabel = segment) %>%
  # scale cog based on statistics used for fitted model
  mutate(cog_gs = (cog - stats.cog$mean) / (2 * stats.cog$sd)) %>%
  crossing(
    Condition.Test.Pen = c("H", "M"),
    # Create one version of the data frame for each experiment 
    # (since we included experiment in the model fit, we need 
    # to include it in the newdata, and average over it below)
    Experiment = c("CISP-1a", "CISP-1b", "CISP-1c")) %>%
  # Get predictions ignoring random effects
  bind_cols(predict(m1, newdata = ., re_formula = NA)) %>%
  rename(Condition.Pen = Condition.Test.Pen, Condition.OriginalLabel = Condition.Test.OriginalLabel) %>%
  # average prediction across experiments (since we can't really
  # model the effects of selected continuum steps for the exposure 
  # data anyway)
  group_by(word, type, Condition.OriginalLabel, Condition.Pen, cog, cog_gs) %>%
  summarise(predicted_probability.SH = mean(Estimate)) %>%
  # Define cutoff probabilities and determine whether segment would be 
  # perceived as intended.
  mutate(
    # Cutoff values are the minimum probability that the segment must 
    # have to still be perceived as intended. Lower values thus imply
    # stronger word superiority effects (less evidence is needed to 
    # still be willing to accept the segment as intended).
    cutoff_s = 0.4,
    cutoff_sh = 0.4,
    Condition.Exposure = 
      case_when(
        Condition.OriginalLabel == "S" & type == "typical" ~ "SH-bias",
        Condition.OriginalLabel == "S" & type == "shifted" ~ "S-bias",
        Condition.OriginalLabel == "SH" & type == "typical" ~ "S-bias",
        Condition.OriginalLabel == "SH" & type == "shifted" ~ "SH-bias"),
    segment.perceived_as_intended = 
      case_when(
        Condition.OriginalLabel == "S" & 1 - predicted_probability.SH < cutoff_s ~ "no",
        Condition.OriginalLabel == "S" & 1 - predicted_probability.SH >= cutoff_s ~ "yes",
        Condition.OriginalLabel == "SH" & predicted_probability.SH < cutoff_sh ~ "no",
        Condition.OriginalLabel == "SH" & predicted_probability.SH >= cutoff_sh ~ "yes")) 

# d.exp %>%
#   group_by(Condition.Exposure, Condition.OriginalLabel, Condition.Pen, type, segment.perceived_as_intended) %>%
#   tally()

d.exp %>%
  ggplot(
    aes(
      x = cog,
      y = predicted_probability.SH,
      color = Condition.Pen)) +
  scale_shape_manual(values = c(1, 19)) +
  geom_line(aes(linetype = type)) +
  facet_grid(Condition.Exposure ~ Condition.OriginalLabel)

d.exp  %>%
  ggplot(aes(x = cog,
             y = predicted_probability.SH,
             color = segment.perceived_as_intended,
             shape = type)) +
  scale_shape_manual(values = c(1, 19)) +
  scale_color_manual(values = c("red", "green")) +
  geom_point() +
  facet_grid(Condition.Pen + Condition.Exposure ~ Condition.OriginalLabel)

# make an IO based on the typical tokens (and no pen in mouth)
IO <- 
  make_MVG_ideal_observer_from_data(
  data = d.exp %>% filter(Condition.Pen == "H", type == "typical"),
  category = "Condition.OriginalLabel",
  cues = "cog",
  verbose = T)

# and make an IA based on it
# and a random kappa and nu... could/should try others!
# pretty low for now just so effects are more visually evident
kappa_nu <- 10
IA <- 
  IO %>%
  mutate(kappa = kappa_nu,
         nu = kappa_nu,
         prior_kappa = kappa_nu,
         prior_nu = kappa_nu,
         S = get_S_from_expected_Sigma(Sigma, nu)) %>%
  rename(m = mu) %>%
  select(!Sigma) %>%
  arrange(category)

is.NIW_belief(IA)

# check classic exposure effect (pen in hand)
posterior_s_bias.PiH <- 
  update_NIW_ideal_adaptor_incrementally(
    prior = IA,
    exposure = 
      filter(
        d.exp,
        # filter d.exp to only include believable tokens
        # NOTE: THIS MIGHT BE TOO SIMPLISTIC. WE MIGHT HAVE TO MODEL THE *PROBABILITY* WITH WHICH A TOKEN 
        # IS STILL RECOGNIZED AS INTENDED. 
        segment.perceived_as_intended == "yes",
        Condition.Exposure == "S-bias",
        Condition.Pen == "H"),
    exposure.category = "Condition.OriginalLabel",
    exposure.cues = "cog")

posterior_sh_bias.PiH <- 
  update_NIW_ideal_adaptor_incrementally(
    prior = IA,
    exposure = 
      filter(
        d.exp,
        # filter d.exp to only include believable tokens
        segment.perceived_as_intended == "yes",
        Condition.Exposure == "SH-bias",
        Condition.Pen == "H"),
    exposure.category = "Condition.OriginalLabel",
    exposure.cues = "cog")

xlim <- c(4000, 7000)
plot_grid(
  p_priors <- plot_expected_categories_density_1D(filter(IA), xlim = xlim),
  p_sh_PiH <- plot_expected_categories_density_1D(filter(posterior_sh_bias.PiH, observation.n == max(observation.n)), xlim = xlim) + theme(legend.position = "none"),
  p_s_PiH <- plot_expected_categories_density_1D(filter(posterior_s_bias.PiH, observation.n == max(observation.n)), xlim = xlim) + theme(legend.position = "none"),
  labels = c("priors", "after SH-biased exposure (PiH)", "after S-biased exposure (PiH)"),
  hjust = 0,
  ncol = 1, 
  align = "hv")

# and now with PIM
posterior_s_bias.PiM <- 
  update_NIW_ideal_adaptor_incrementally(
    prior = IA,
    exposure = 
      filter(
        d.exp,
        # filter d.exp to only include believable tokens
        segment.perceived_as_intended == "yes",
        Condition.Exposure == "S-bias",
        Condition.Pen == "M"),
    exposure.category = "Condition.OriginalLabel",
    exposure.cues = "cog")

posterior_sh_bias.PiM <- 
  update_NIW_ideal_adaptor_incrementally(
    prior = IA,
    exposure = 
      filter(
        d.exp,
        # filter d.exp to only include believable tokens
        segment.perceived_as_intended == "yes",
        Condition.Exposure == "SH-bias",
        Condition.Pen == "M"),
    exposure.category = "Condition.OriginalLabel",
    exposure.cues = "cog")

plot_grid(
  p_priors,
  p_sh_PiM <- plot_expected_categories_density_1D(filter(posterior_sh_bias.PiM, observation.n == max(observation.n)), xlim = xlim) + theme(legend.position = "none"),
  p_s_PiM <- plot_expected_categories_density_1D(filter(posterior_s_bias.PiM, observation.n == max(observation.n)), xlim = xlim) + theme(legend.position = "none"),
  labels = c("priors", "after SH-biased exposure (PiM)", "after S-biased exposure (PiM)"),
  hjust = 0,
  ncol = 1, 
  align = "hv")

# Compare effect of SH-biased exposure for PiH vs. PiM
plot_grid(p_priors, p_sh_PiH, p_sh_PiM, 
          ggplot(mapping = aes(color = category)) +
            bind_rows(
              filter(posterior_sh_bias.PiH, observation.n == max(observation.n)) %>% 
                mutate(posterior = "PiH"),
              filter(posterior_sh_bias.PiM, observation.n == max(observation.n)) %>% 
                mutate(posterior = "PiM")) %>%
            mutate(
              mu = get_expected_mu_from_m(m), 
              Sigma = get_expected_Sigma_from_S(S, nu)) %>% 
            group_by(category, .add = T) %>% 
            group_map(.keep = T, .f = function(.x, .y) 
              stat_function(data = .x, mapping = aes(color = category), 
                            fun = function(x, mean1, sd1, mean2, sd2) dnorm(x, mean1, sd1) - dnorm(x, mean2, sd2), 
                            args = list(mean1 = .x$mu[[1]], sd1 = .x$Sigma[[1]]^0.5, mean2 = .x$mu[[2]], sd2 = .x$Sigma[[2]]^0.5))) +
            scale_x_continuous(get_cue_labels_from_model(IA), limits = xlim, expand = c(0, 0)) + 
            scale_y_continuous("Density") +
            theme(legend.position = "none"),
          labels = c("prior", "after SH-biased exposure (PiH)", "after SH-biased exposure (PiM)", "difference between PiH - PiM"),
          ncol = 1, hjust = 0, align = "hv")

summary <- 
  rbind(
    mutate(IA,
           bias = "prior", pen = "NA",
           observation.n = 40),
    mutate(posterior_s_bias.PiM,
           bias = "S", pen = "M"),
    mutate(posterior_sh_bias.PiM,
           bias = "SH", pen = "M"),
    mutate(posterior_s_bias.PiH,
           bias = "S", pen = "H"),
    mutate(posterior_sh_bias.PiH,
           bias = "SH", pen = "H")) %>%
  select(category, m, S, bias, pen)
summary


data.exposure = NULL
data.test = NULL
target_category = 1
logit = F
category.ids = NULL
category.labels = NULL
category.colors = NULL
category.linetypes = NULL
facet_rows_by = NULL
facet_cols_by = NULL
facet_wrap_by = NULL
animate_by = NULL
animation_follow = F
x.expand = c(0, 0)
xlim = c(1000, 10000)
ylim = NULL
x <- IA

get_categorization_function_from_NIW_ideal_adaptor(x)(5000)

facet_rows_by <- enquo(facet_rows_by)
facet_cols_by <- enquo(facet_cols_by)
facet_wrap_by <- enquo(facet_wrap_by)
animate_by <- enquo(animate_by)

MVBeliefUpdatr:::check_compatibility_between_NIW_belief_and_data(x, data.exposure, data.test,
                                                !! facet_rows_by, !! facet_cols_by, !! facet_wrap_by, !! animate_by)

cue.labels = get_cue_labels_from_model(x)
assert_that(length(cue.labels) == 1, msg = "Expecting exactly one cue for plotting.")

if (is_missing(xlim)) {
  if (!is.null(data.exposure) & !is.null(data.test))
    xlim = range(range(data.exposure[[cue.labels[1]]]), range(data.test[[cue.labels[1]]])) else
      if (!is.null(data.exposure))
        xlim = range(data.exposure[[cue.labels[1]]]) else
          if (!is.null(data.test))
            xlim = range(data.test[[cue.labels[1]]])
}
assert_that(!is_missing(xlim), msg = "`xlim` must be specified")

# Setting aes defaults
if (is.null(category.ids)) category.ids = levels(x$category)
if (is.null(category.labels)) category.labels = levels(x$category)
if (is.null(category.colors)) category.colors = get_default_colors("category", category.ids)
if (is.null(category.linetypes)) category.linetypes = rep(1, length(category.ids))

if (any(!quo_is_null(facet_rows_by),
        !quo_is_null(facet_cols_by),
        !quo_is_null(animate_by))) x %<>% group_by(!! facet_rows_by, !! facet_cols_by, !! facet_wrap_by, !! animate_by,
                                                   .add = TRUE)

stat_functions <-
  x %>%
  group_map(
    .keep = T,
    .f = function(.x, .y) {
      cat_function <- get_categorization_function_from_NIW_ideal_adaptor(.x, logit = logit)
      stat_function(
        data = .x,
        fun = cat_function) })

p <-
  ggplot() +
  stat_functions +
  { if (!is.null(data.test))
    add_test_data_to_1D_plot(data = data.test, cue.labels = cue.labels) } +
  { if (!is.null(data.exposure))
    add_exposure_data_to_1D_plot(data = data.exposure, cue.labels = cue.labels,
                                 category.ids = category.ids, category.labels = category.labels, category.colors) } +
  scale_x_continuous(name = cue.labels, limits = xlim, expand = x.expand) +
  scale_y_continuous(name = if (logit)
    paste0("log-odds(resp = ", category.labels[target_category], ")") else
      paste0("p(resp = ", category.labels[target_category], ")")) +
  coord_cartesian(ylim = ylim)

p <- facet_or_animate(p, !!facet_rows_by, !!facet_cols_by, !! facet_wrap_by, !!animate_by, animation_follow)


# this doesn't work... 
plot_expected_categorization_function_1D(IA,
                                         data.test = d.acoustics.test,
                                         xlim = c(1000, 10000),
                                         target_category = 2, verbose = T)


add_ibbu_stanfit_draws(
  IA,
  ndraws = 10,
  wide = F,
  nest = T,
  untransform_cues = T)

# let's do it the long way 


IA_func <- get_categorization_function_from_NIW_ideal_adaptor(IA,
                                                              noise_treatment = "no_noise")

IA_linspace <- 
  tibble(cog = c(5000:7000)) %>%
  mutate(prob = as.numeric(IA_func(cog)))



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


