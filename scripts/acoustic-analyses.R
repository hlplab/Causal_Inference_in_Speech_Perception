# Last updated 03-03-25 snc.

# Packages
library(tidyverse)
library(magrittr)
library(rlang)
library(assertthat)
library(cowplot)
library(gganimate)

library(brms)

# TODO
# make sure to calculate CoG effect based on aligned vis. label
# update beliefs based on penned(cog) (which takes cog and label and y/n pen as inputs)
# optim()

#devtools::install_github("hlplab/MVBeliefUpdatr")
library(MVBeliefUpdatr)


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
  geom_line()

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

# simulate the acoustics in various conditions
sampledata <- data_frame(
  crossing(Condition.Test.OriginalLabel = c("S", "SH"),
           Condition.Test.Pen = c("H", "M"),
           Experiment = c("CISP-1a", "CISP-1b", "CISP-1c"),
           # oversample middle of continuum
           # this is a bad approach, just relying on really high
           # n of samples to get over how sparse the space is
           cog = c(seq(3500, 7300, length.out = 6000), rnorm(mean = 5400, sd = 300, n = 5000)))) %>%
  mutate(cog_gs = (cog - stats.cog$mean) / (2 * stats.cog$sd)) %>%
  # Get predictions ignoring random effects
  bind_cols(predict(m1, newdata = ., re_formula = NA)) %>%
  group_by(Condition.Test.OriginalLabel,
           Condition.Test.Pen,
           cog_gs) %>%
  summarize(estimate_rounded = round(mean(Estimate), 3)) %>%
  # match data by estimate
  pivot_wider(names_from = "Condition.Test.Pen",
              values_from = "cog_gs",
              values_fn = mean)

# save this df because it takes forever to generate by brute force
save(sampledata, file = "sample_data_compensated_by_PIM.RData")

sd2 <- sampledata %>%
  filter(!is.na(H),
         !is.na(M)) %>%
  mutate(pen_diff = H - M)

sd2plot <- sd2 %>%
  ggplot(aes(y = estimate_rounded)) +
  geom_point(aes(x = H)) +
  geom_point(aes(x = M),
             color = "red") +
  theme_cowplot() +
  facet_wrap(~Condition.Test.OriginalLabel)
sd2plot

# pretty noisy...
sd2plot2 <- sd2 %>%
  ggplot(aes(x = estimate_rounded,
             y = pen_diff)) +
  geom_point() +
  theme_cowplot() +
  facet_wrap(~Condition.Test.OriginalLabel)
sd2plot2

d.exp.compensated <- d.exp %>%
  mutate(estimate_rounded = round(predicted_probability.SH, 3)) %>%
  merge(sd2 %>%
          rename(Condition.OriginalLabel = Condition.Test.OriginalLabel),
        all.x = T) %>%
  mutate(
    cog_gs_compensated = case_when(
      # add the pen's effect on cog only to PIM tokens
      Condition.Pen == "M" ~ cog_gs + pen_diff,
      Condition.Pen == "H" ~ cog_gs),
    # also comp non-scaled cog
    cog = (cog_gs_compensated * (2 * stats.cog$sd)) + stats.cog$mean) %>%
  rename(cog_gs_original = cog_gs,
         cog_gs = cog_gs_compensated,
         Condition.Test.Pen = Condition.Pen,
         Condition.Test.OriginalLabel = Condition.OriginalLabel) %>%
  crossing(Experiment = c("CISP-1a", "CISP-1b", "CISP-1c")) %>%
  # re-predict based on these new cogs
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


d.exp %>%
  ggplot(
    aes(
      x = cog,
      y = predicted_probability.SH,
      color = Condition.Pen)) +
  scale_shape_manual(values = c(1, 19)) +
  geom_line(aes(linetype = type)) +
  facet_grid(Condition.Exposure ~ Condition.OriginalLabel)

d.exp.compensated %>%
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

d.exp.compensated  %>%
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
kappa_nu <- 20
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
      d.exp %>%
      # Make sure token that aren't perceived as intended are not used for updating
      mutate(cog = ifelse(segment.perceived_as_intended == "no", NA, cog)) %>%
      filter(
        Condition.Exposure == "S-bias",
        Condition.Pen == "H"),
    exposure.category = "Condition.OriginalLabel",
    exposure.cues = "cog")


posterior_sh_bias.PiH <-
  update_NIW_ideal_adaptor_incrementally(
    prior = IA,
    exposure =
      d.exp %>%
      # Make sure token that aren't perceived as intended are not used for updating
      mutate(cog = ifelse(segment.perceived_as_intended == "no", NA, cog)) %>%
      filter(
        Condition.Exposure == "SH-bias",
        Condition.Pen == "H"),
    exposure.category = "Condition.OriginalLabel",
    exposure.cues = "cog")

xlim <- c(4000, 8000)
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
      d.exp %>%
      # Make sure token that aren't perceived as intended are not used for updating
      mutate(cog = ifelse(segment.perceived_as_intended == "no", NA, cog)) %>%
      filter(
        Condition.Exposure == "S-bias",
        Condition.Pen == "M"),
    exposure.category = "Condition.OriginalLabel",
    exposure.cues = "cog")

posterior_sh_bias.PiM <-
  update_NIW_ideal_adaptor_incrementally(
    prior = IA,
    exposure =
      d.exp %>%
      # Make sure token that aren't perceived as intended are not used for updating
      mutate(cog = ifelse(segment.perceived_as_intended == "no", NA, cog)) %>%
      filter(
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
           bias = "prior", pen = "H") %>%
      # Copy prior (for animation below)
      crossing(observation.n = 0:max(posterior_s_bias.PiH$observation.n)),
    mutate(posterior_s_bias.PiH,
           bias = "S", pen = "H"),
    mutate(posterior_sh_bias.PiH,
           bias = "SH", pen = "H"),
    mutate(posterior_s_bias.PiM,
           bias = "S", pen = "M"),
    mutate(posterior_sh_bias.PiM,
           bias = "SH", pen = "M"))

## and all of the same with the 'compensated' dataframe ####
# check classic exposure effect (pen in hand)

posterior_s_bias.PiM.compensated <-
  update_NIW_ideal_adaptor_incrementally(
    prior = IA,
    exposure =
      d.exp.compensated %>%
      # Make sure token that aren't perceived as intended are not used for updating
      mutate(cog = ifelse(segment.perceived_as_intended == "no", NA, cog)) %>%
      filter(
        Condition.Exposure == "S-bias",
        Condition.Pen == "M"),
    exposure.category = "Condition.OriginalLabel",
    exposure.cues = "cog")

posterior_sh_bias.PiM.compensated <-
  update_NIW_ideal_adaptor_incrementally(
    prior = IA,
    exposure =
      d.exp.compensated %>%
      # Make sure token that aren't perceived as intended are not used for updating
      mutate(cog = ifelse(segment.perceived_as_intended == "no", NA, cog)) %>%
      filter(
        Condition.Exposure == "SH-bias",
        Condition.Pen == "M"),
    exposure.category = "Condition.OriginalLabel",
    exposure.cues = "cog")

plot_grid(
  p_priors,
  p_sh_PiM.compensated <- plot_expected_categories_density_1D(filter(posterior_sh_bias.PiM, observation.n == max(observation.n)), xlim = xlim) + theme(legend.position = "none"),
  p_s_PiM.compensated <- plot_expected_categories_density_1D(filter(posterior_s_bias.PiM, observation.n == max(observation.n)), xlim = xlim) + theme(legend.position = "none"),
  labels = c("priors", "after SH-biased exposure (PiM)", "after S-biased exposure (PiM)"),
  hjust = 0,
  ncol = 1,
  align = "hv")

# Compare effect of SH-biased exposure for PiH vs. PiM
plot_grid(p_priors, p_sh_PiH, p_sh_PiM.compensated,
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
           compensated = "NA", bias = "prior", pen = "NA",
           observation.n = 40),
    mutate(posterior_s_bias.PiM,
           compensated = "no", bias = "S", pen = "M"),
    mutate(posterior_sh_bias.PiM,
           compensated = "no", bias = "SH", pen = "M"),
    mutate(posterior_s_bias.PiH,
           compensated = "no", bias = "S", pen = "H"),
    mutate(posterior_sh_bias.PiH,
           compensated = "no", bias = "SH", pen = "H"),
    mutate(posterior_s_bias.PiM.compensated,
           compensated = "yes", bias = "S", pen = "M"),
    mutate(posterior_sh_bias.PiM.compensated,
           compensated = "yes", bias = "SH", pen = "M"))

p <-
  plot_expected_categorization_function_1D(
  summary %>% group_by(bias, pen),
  data.test = d.acoustics.test,
  xlim = xlim,
  target_category = 1) +
  aes(color = paste(bias, compensated),
      linetype = pen) +
  scale_color_manual(values = c("black", "red", "darkgreen", "blue", "darkgreen")) +
  theme_cowplot()
p

a <- animate(p, width = 500, height = 500, fps = 5, renderer = av_renderer())
anim_save("../figures/updating.webm", a)
