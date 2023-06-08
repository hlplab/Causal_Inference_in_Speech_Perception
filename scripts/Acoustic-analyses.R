# Packages
library(tidyverse)

# Absolute filepath
#setwd("../shawncummings/Documents/GitHub/")

Exp <- read_csv("./Causal_Inference_in_Speech_Perception/data/acoustics/fricative_cues_extracted_by_praat.csv")
Test <- read_csv("./Causal_Inference_in_Speech_Perception/data/acoustics/CISP_teststeps.csv.csv")

CISP_Exp_CoG <- Exp %>%
  filter(study == "Liu-Jaeger-2018",
         fricative %in% c("S", "SH")) %>%
  select(word, fricative, shift_percent, cue_raw_M1) %>%
  mutate(bias = case_when(
    fricative == "S" & shift_percent == 0 ~ "SH",
    fricative == "S" & shift_percent == 50 ~ "S",
    fricative == "SH" & shift_percent == 0 ~ "S",
    fricative == "SH" & shift_percent == 50 ~ "SH",
    fricative == "?SSH" ~ "test")) %>%
  rename(CoG = cue_raw_M1)

Exposure_graph <- CISP_CoG %>%
  filter(fricative != "?SSH") %>%
  ggplot(aes(x = CoG,
             color = fricative)) +
  geom_density() +
  facet_wrap(~bias) +
  theme_bw()
Exposure_graph

CISP_Test_CoG <- Test %>%
  select(word, cue_raw_M1) %>%
  rename(CoG = cue_raw_M1)
  

Test_graph <- CISP_Test_CoG %>%
  ggplot(aes(x = as.numeric(word),
             y = CoG,
             label = word)) +
  geom_label() +
  theme_bw()
Test_graph

CoG_Exp_measurements <- CISP_CoG %>%
  group_by(fricative, shift_percent) %>%
  summarise(mean = mean(CoG),
            sd = sd(CoG))
CoG_measurements

CISP_Test_CoG
