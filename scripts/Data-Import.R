### Libraries
library(tidyverse)
library(magrittr)    # pipes
library(lubridate)   # date conversion, etc.

library(brms)        # Bayesian GL(M)Ms
library(sjPlot)      # tables for Bayesian GL(M)Ms
library(broom)       # extracting information from GL(M)Ms

rm(list = ls())

### this moves to a new directory within the private repo
### that repo needs to have a readme which clearly says DO NOT MAKE PUBLIC
### "they contain sensitive info and therefore cannot be shared"

### LOOK AT SUPPLEMENTARY INFO DOC!!

# Clunky absolute filepathing to where these live on snc machine (private repo)
setwd("../../../../GitHub/Causal_Inference_in_Speech_Perception/scripts/")
source("constants.R")

setwd("../../../GitHub/Causal_inference_in_speech/data/raw/")

### Raw data import from private repo
# includes initial formatting
# and now includes add_exclusions() within formatdata()

# For experiments run on MTurk, a single .tab file is returned
d.Exp1a <- read_tsv("./experiment-NORM-A/production/experiment-NORM-A-URLPARAMS-results.tab") %>%
  # The raw file has a glitch where an additional column appeared in some participants' data.
  # This has been repaired manually, resulting in two data frames.
  # Report-Production-Norming-Experiment-A.Rmd
  select(-Answer.Submit) %>%
  rbind(read_tsv("./experiment-NORM-A/production/experiment-NORM-A-URLPARAMS-results-repair.tab")) %>%
  formatData(experiment = "NORM A") %>%
  mutate(Experiment = "1a",
         Experiment_internal = "NORM A")

d.Exp1b <- read_tsv("./experiment-NORM-B/production/experiment-NORM-B-URLPARAMS-results.tab") %>%
  formatData(experiment = "NORM B") %>%
  mutate(Experiment = "1b",
         Experiment_internal = "NORM B")

d.Exp1c <- read_tsv("./experiment-NORM-C/production/experiment-NORM-C-URLPARAMS-results.tab") %>%
  formatData(experiment = "NORM C") %>%
  mutate(Experiment = "1c",
         Experiment_internal = "NORM C")

d.Exp2a <- read_tsv("./experiment-NORM-D/production/experiment-NORM-D-URLPARAMS-results.tab") %>%
  formatData(experiment = "NORM D") %>%
  mutate(Experiment = "2a",
         Experiment_internal = "NORM D")

d.Exp2b <- read_tsv("./experiment-NORM-E/production/experiment-NORM-E-URLPARAMS-results.tab") %>%
  formatData(experiment = "NORM E") %>%
  mutate(Experiment = "2b",
         Experiment_internal = "NORM E")

# For experiments run on Prolific and Proliferate, a .csv for each column of data is returned
# these experiments therefore require binding first, before formatting
d3_file_list <- list.files("./experiment-Prolific-E/Production/")

d.Exp3 <- read.csv(paste("./experiment-Prolific-E/Production/", d3_file_list[1], sep="")) %>%
  { if("error" %in% names(.)) {
    if(all(is.na(.$error))) select(., -error) else
      rename(., !! sym(paste("error", d3_file_list[i+1], sep = ".")) := error)} else .}

for (i in 1:length(d3_file_list)-1){
  nextvar <- read.csv(paste("./experiment-Prolific-E/Production/", d3_file_list[i+1], sep="")) %>%
    { if("error" %in% names(.)) {
      if(all(is.na(.$error))) select(., -error) else
        rename(., !! sym(paste("error", d3_file_list[i+1], sep = ".")) := error)} else .}
  d.Exp3 %<>% left_join(nextvar)
}

d.Exp3 %<>% 
  separate(proliferate.condition, 
           into = c("trash1", "trash2", "ExpResp", "TestResp"), sep = "&") %>%
  mutate(Condition.Test.Keybindings = ifelse(str_sub(TestResp, -1) == 0,
                                             "X-ASI;M-ASHI",
                                             "X-ASHI;M-ASI"),
         Exclude_Participant.because_of_TechnicalDifficulty = FALSE,
         WorkerID = as.numeric(factor(workerid))) %>%
  formatData(experiment = "C") %>%
  mutate(Experiment = "3",
         Experiment_internal = "Prolific-E",
         Duration.Assignment = "Not Captured")

#Exp4
d4_file_list <- list.files("./experiment-2a/Production/")

d.Exp4 <- read.csv(paste("./experiment-2a/Production/", d4_file_list[1], sep="")) %>%
  { if("error" %in% names(.)) {
    if(all(is.na(.$error))) select(., -error) else
      rename(., !! sym(paste("error", d4_file_list[i+1], sep = ".")) := error)} else .}

for (i in 1:length(d4_file_list)-1){
  nextvar <- read.csv(paste("./experiment-2a/Production/", d4_file_list[i+1], sep="")) %>%
    { if("error" %in% names(.)) {
      if(all(is.na(.$error))) select(., -error) else
        rename(., !! sym(paste("error", d4_file_list[i+1], sep = ".")) := error)} else .}
  d.Exp4 %<>% left_join(nextvar)
}

d.Exp4 %<>% 
  separate(proliferate.condition, 
           into = c("trash1", "trash2", "ExpResp", "TestResp"), sep = "&") %>%
  mutate(Condition.Test.Keybindings = ifelse(str_sub(TestResp, -1) == 0,
                                             "X-ASI;M-ASHI",
                                             "X-ASHI;M-ASI"),
         Exclude_Participant.because_of_TechnicalDifficulty = FALSE,
         WorkerID = as.numeric(factor(workerid))) %>%
  formatData("C") %>%
  mutate(Experiment = "4",
         Experiment_internal = "2a",
         Duration.Assignment = "Not Captured")

### Binding and export
CISP_data <- rbind(d.Exp1a, d.Exp1b, d.Exp1c,
                   d.Exp2a, d.Exp2b,
                   d.Exp3, d.Exp4)

write_csv(CISP_data, "../../../Causal_Inference_in_Speech_Perception/data/CISP_data.csv")


