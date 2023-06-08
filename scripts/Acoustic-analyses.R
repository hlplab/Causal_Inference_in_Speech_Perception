# Packages
library(tidyverse)


# Absolute filepath
#setwd("../shawncummings/Documents/GitHub/")

acoustics <- read_csv("./Causal_Inference_in_Speech_Perception/data/acoustics/fricative_cues_extracted_by_praat.csv")

CISP_CoG <- acoustics %>%
  filter(study == "Liu-Jaeger-2018",
         fricative %in% c("S", "SH", "?SSH")) %>%
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
  facet_wrap(~bias)
Exposure_graph

CoG_measurements <- CISP_CoG %>%
  group_by(fricative, shift_percent) %>%
  summarise(mean = mean(CoG),
            sd = sd(CoG))
CoG_measurements
