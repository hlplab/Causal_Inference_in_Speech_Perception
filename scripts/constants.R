# Constants ------------------------------------------------------------------
fig.base_width <- 1.75
fig.base_height <- 1.85

theme_set(
  theme_bw(base_size = 10) + 
    theme(
      legend.position = "top",
      legend.key.width = unit(1, "cm"),
      panel.grid = element_blank(),
      strip.background = element_rect(fill = "black"),
      strip.text = element_text(color = "white")))

levels.ItemID_exposure <- unlist(map(c("S", "SH", "F", "FN"), ~ paste0(.x, 1:200)))
levels.ItemID_test.A <- paste0("Frame", c(1:12))
levels.ItemID_test.B <- paste0("Frame", c(1:4, 13, 6:12))
levels.Item.Type <- c("typical", "shifted", "filler")
levels.Phase <- c("practice", "exposure", "test")
levels.Sex <- c("Female", "Male")
levels.Ethnicity <- c("Hispanic", "Non-Hispanic")
levels.Race <- c("American Indian", "Asian", "Black", "other", "White", "multiple")

levels.response <- c("ASI", "ASHI")
levels.exposure.lexical_labels <- c("S", "SH")
levels.exposure.pen_locations <- c("H", "M")
levels.test.visual_labels <- c("S", "SH")
levels.test.pen_locations <- c("H", "M")

labels.response <- c("ASI", "ASHI")
labels.exposure.lexical_labels <- c("S", "SH")
labels.exposure.pen_locations <- c("hand", "mouth")
labels.test.visual_labels <- c("S", "SH")
labels.test.pen_locations <- c("hand", "mouth")

colors.exposure.lexical_labels <- c("red", "blue")
colors.test.visual_labels <- c("darkred", "darkblue")
colors.test.pen_locations <- c("orange", "green")
# color.FILLER <- "gray"
linetypes.exposure.pen_locations <- c(3, 4)

shapes.exposure.pen_locations <- c(20, 21)
linetypes.test.pen_locations <- c(1, 2)     # not yet thought through
shapes.test.pen_locations <- c(15, 17)      # not yet thought through
linetypes.test.visual_labels <- c(1, 2)
shapes.test.visual_labels <- c(17, 19)

## STAN / BRMS constants ---------------------------------------------------------------
chains <- 4
options(
  width = 110,
  mc.cores = min(chains, parallel::detectCores()))

require(brms)
my_priors <- c(
  prior(student_t(3, 0, 2.5), class = "b"),
  prior(cauchy(0, 2.5), class = "sd"),
  prior(lkj(1), class = "cor")
)

# Functions ------------------------------------------------------------------
## General functions ---------------------------------------------------------
percent <- function(p) 
  paste0(round(p * 100, 1), "%")


scale_Gelman <- function(x) {
  (x - mean(x)) / (2 * sd(x))
}


emplog = function(p, n) {
  log( (p * n + .5) / (n - p * n + .5) )
}

## Data preparation ------------------------------------------------------- 
simplifyAnswer <- function(x) {
  x = gsub("[|]", ";", x)
  x = ifelse(str_ends(x, ";"), x, paste0(x, ";"))
  ifelse(str_count(x, ";") > 1, "multiple", str_remove(x, ";"))
}

## create functions for mturk
formatData <- function(.data, experiment, exclude_based_on_catch_trials = T) {
  require(tidyverse)
  require(assertthat)
  require(lubridate)
  
  assert_that(
    experiment %in% c("NORM A", "NORM B", "NORM C", "NORM D","NORM E", "A", "B","C", "PRAC-A"),
    msg = "It looks like you're trying to process a new experiment. First edit formatData to accommodate that experiment.")
  
  expSet <- case_when(
    experiment == "A" ~ "A",
    experiment %in% c("B", "C") ~ "B",
    experiment == "PRAC-A" ~ NA_character_,
    T ~ NA_character_
  )
  testSet <- case_when(
    experiment == "NORM A" ~ "A",
    experiment %in% c("NORM B", "A") ~ "B",
    experiment %in% c("NORM C", "NORM D", "NORM E", "B", "C") ~ "C",
    experiment == "PRAC-A" ~ NA_character_,
    T ~ NA_character_
  )
  testOccluder <- case_when(
    experiment %in% c("NORM A", "NORM B", "NORM C", "A", "B", "C") ~ F,
    experiment == "PRAC-A" ~ NA,
    experiment %in% c("NORM D", "NORM E") ~ T,
    T ~ NA
  )
  
  .data %<>%
    # check for hittypeid, which indicates this is an MTurk dataframe. If exists, do some renaming/removal/simplification.
    { if ("hittypeid" %in% names(.)) {
      # initialize ParticipantID specific to MTurk
      anonymize_workers(., experiment) %>%
      select(-c(
               # variables that are constant and INFORMATIVE across all rows
               hittypeid, hitgroupid, title, description, keywords, creationtime,
               assignments, assignmentduration, autoapprovaldelay, autoapprovaltime, reward, hitlifetime, viewhit,
               starts_with("Qualification"),
               # variables that are constant and UNinformative across all row
               assignmentstatus, hitstatus, reviewstatus, numavailable, numpending, numcomplete, annotation,
               # variables that are NAs across all rows
               assignmentapprovaltime, assignmentrejecttime, deadline, feedback, reject)) %>%
        rename_all(~gsub("^(Answer.)", "", .x)) %>%
        rename(Assignment.Accept.DateTime.UTC = assignmentaccepttime,
               Assignment.Submit.DateTime.UTC = assignmentsubmittime) %>%
        mutate(Platform = "MTurk")
      } else {
          # if hittypeid doesn't exist, this is a prolific file, and should be formatted as such
        rename(., 
               assignmentid = experiment_id,
               ParticipantID = workerid) %>%
          mutate(Platform = "Prolific")
      }} %>%
    # Remove variables that are no longer needed
    select(-any_of(c(      
      # Item sets are now hard coded above (but this info could be useful for debugging when available)
      # Keybindings are now inferred by code (but this info could be useful for debugging when available)
      "expSet", "testSet", 
      "respKeyExp", "respKeyTest"))) %>%
    # Separate the practice, exposure, and test columns into one column per trial
    # (each with one more parts then there are trials because the last element is also followed by a ";". 
    # This last empty element should then be removed. If you get a warning, however, that means that at 
    # least one participant has more trials than expected)
    { if ("practResp" %in% names(.))     
      separate(., 
               practResp,
               # Making many practice trials so that several repetitions of the trials can be stored
               # Unused trials are discarded below
               into = paste0("Practice_Trial", 1:200), 
               sep = ";",
               fill = "right") else . } %>%
    { if ("practiceResp" %in% names(.))     
        separate(., 
                 practiceResp,
                 # Making many practice trials so that several repetitions of the trials can be stored
                 # Unused trials are discarded below
                 into = paste0("Practice_Trial", 1:200),
                 sep = ";",
                 fill = "right") else . } %>%
    { if ("lexicaldecisionResp" %in% names(.)) 
      separate(.,
               lexicaldecisionResp,
               into = paste0("Exposure_Trial", 1:101),
               sep = ";") else . } %>%
    { if ("exposureResp" %in% names(.))
      separate(.,
               exposureResp,
               # Making up to 120 exposure trials to accommodate double-block exposure (CISP 2A)
               # Unused trials are discarded below
               into = paste0("Exposure_Trial", 1:121),
               sep = ";",
               fill = "right") else . } %>%
    { if ("testResp" %in% names(.)) 
    separate(.,
      testResp,
      into = paste0("Test_Trial", 1:73),
      sep = ";") else . } %>%
    pivot_longer(
      cols = contains("_Trial"), 
      names_to = c("Phase", "Trial"),
      names_pattern = "(.*)_Trial(.*)"
    ) %>%
    # Remove empty final trial from each phase, as well as all unused practice trials (which are NA)
    filter(value != "" & !is.na(value)) %>%
    # Separate trial-level information into multiple columns
    separate(
      value,
      into = c("Task", 
               "CHECK.Trial", "REMOVE1", 
               "Item.Filename", 
               "Response.Keycode", "Response", 
               "Time.StartOfStimulus", "Time.EndOfTrial", "Response.RT",
               "Item.isCatchTrial", "Response.CatchTrial"),
      sep = ",") %>%
    # Add Experiment information
    mutate(Experiment = experiment) %>%
    # Renaming
    dplyr::rename(
      Experiment.Protocol = rsrb.protocol,
      AssignmentID = assignmentid,
      Assignment.Comment = comments) %>%
    { if ("condition" %in% names(.)) 
      dplyr::rename(., Condition.Exposure.Pen = condition) else 
        mutate(., Condition.Exposure.Pen = NA) } %>% 
    { if ("label" %in% names(.)) 
      dplyr::rename(., Condition.Exposure.LexicalLabel = label) else 
        mutate(., Condition.Exposure.LexicalLabel = NA) } %>% 
    rename_at(
      vars(starts_with("rsrb")), 
      ~ gsub("rsrb\\.([a-z])", "Participant\\.\\U\\1", .x, perl = T)) %>%
    # Make Trial numerical
    mutate(Trial = as.numeric(Trial)) %>%
    # Create block variable (1-10 for exposure, 1-6 for test)
    group_by(Phase) %>% 
    mutate(
      Phase = factor(tolower(Phase), levels = levels.Phase),
      Phase.TestSet = case_when(
        Phase == "exposure" ~ expSet,
        Phase == "test" ~ testSet,
        T ~ NA_character_),
      Block = if (first(Phase) == "practice") 1 else 
        cut(Trial, if (first(Phase) == "exposure") 10 else 6, labels = FALSE)) %>%
    ungroup() %>%
    mutate(
      # Extract item information
      REMOVE.Item = ifelse(
        grepl("\\-occluder", Item.Filename),
        gsub("^(.*)\\-occluder\\.(webm|mp4)$", "\\1", Item.Filename),
        gsub("^(.*)\\.(webm|mp4)$", "\\1", Item.Filename)),
      CHECK.Item.isCatchTrial = ifelse(grepl("-CATCH", Item.Filename), T, F),
      Item.Pen = ifelse(grepl("M", REMOVE.Item), "mouth", "hand"),
      Item.Type = ifelse(Phase == "test", "test", gsub("^([A-Z]+)[0-9]+.*$", "\\1", REMOVE.Item)),
      CHECK.Item.Label = case_when(
        Item.Type == "AS" ~ "S",
        Item.Type == "ASH" ~ "SH", 
        T ~ NA_character_),
      Item.WordStatus = ifelse(Item.Type %in% c("test", "FN"), "non-word", "word"),
      Item.Type = case_when(
        Item.Type %in% c("FN", "F") ~ "filler",
        Item.Type %in% c("S", "SH") ~ "typical",
        Item.Type %in% c("AS", "ASH") ~ "shifted",
        T ~ Item.Type),
      ItemID = ifelse(Phase == "test", 
                      gsub("^.*(Frame[0-9]+)\\-.*$", "\\1", REMOVE.Item), 
                      gsub("^A?([A-Z]+[0-9]+)[A-Z].*$", "\\1", REMOVE.Item)),
      Condition.Test.Audio = ifelse(Phase == "test", gsub("^([0-9]+)\\_.*$", "\\1", REMOVE.Item), NA),
      Condition.Test.Pen = ifelse(Phase == "test", gsub("^.*Frame[0-9]+\\-([A-Z])\\-.*$", "\\1", REMOVE.Item), NA),
      Condition.Test.OriginalLabel = ifelse(Phase == "test", gsub("^.*\\-([A-Z]+)$", "\\1", REMOVE.Item), NA),
      Task = case_when(
        Task %in% c("lexicaldecision", "exposure") ~ "lexical decision",
        Task %in% c("pract", "practice") ~ "lexical decision",
        Task == "test" ~ "identification",
        T ~ NA_character_
      )) %>%
    # read in item information about words during exposure.
    left_join(
      # load information about items
      read_csv("../../materials/Video/Mapping_filename_to_word.csv") %>%
        dplyr::rename(
          Item.Word = Word,
          ItemID = StrippedName)) %>%
    # Get key character based on Gevher's reading of the JS code (labelingBlock.js)
    # (and make sure that "B" responses lead to NAs in the Response variable)
    mutate(
      Response = ifelse(Response == "", NA, Response),
      Response.Keycode.Character = case_when(
        Response.Keycode == "66" ~ "B",
        Response.Keycode == "77" ~ "M",
        Response.Keycode == "88" ~ "X",
        T ~ NA_character_),
      Response.CorrectWordStatus = case_when(
        Phase == "test" ~ NA_real_,
        is.na(Response) ~ NA_real_,
        as.character(Response) == as.character(Item.WordStatus) ~ 1,
        as.character(Response) != as.character(Item.WordStatus) ~ 0,
        T ~ NA_real_)) %>%
    # Add time information based on assignment submit time
    { if ("userDateTimeOffset" %in% names(.)) 
      dplyr::rename(., Assignment.Submit.DateTime.UserLocalTime.OffsetFromUTC = userDateTimeOffset) else 
        mutate(., Assignment.Submit.DateTime.UserLocalTime.OffsetFromUTC = NA) } %>%
    { if ("us.timezone" %in% names(.)) 
      dplyr::rename(., Assignment.Submit.US_TimeZone = us.timezone) else 
        mutate(., Assignment.Submit.US_TimeZone = NA) } %>%
    { if ("Assignment.Submit.DateTime.UTC" %in% names(.)) 
      mutate(.,
             Assignment.Submit.DateTime.UserLocalTime = Assignment.Submit.DateTime.UTC - minutes(Assignment.Submit.DateTime.UserLocalTime.OffsetFromUTC),
             Assignment.Submit.DuringDayTime = ifelse(between(hour(Assignment.Submit.DateTime.UserLocalTime), 7, 21), T, F)) %>%
        # Get durational measures (in minutes)
        group_by(ParticipantID) %>%
        mutate(
          Duration.Assignment = difftime(Assignment.Submit.DateTime.UTC, Assignment.Accept.DateTime.UTC, units = "mins")) %>%
        ungroup()
      else . } %>%
    
    # Variable typing
    mutate_at(vars(CHECK.Trial, Response.Keycode, Time.StartOfStimulus, Time.EndOfTrial, Response.RT),
              as.numeric) %>%
    mutate_at(vars(ParticipantID, Phase, starts_with("Condition"), starts_with("Item"),
                   Response, Response.Keycode.Character, Task,
                   AssignmentID),
              factor) %>%
    mutate_at(vars(Item.isCatchTrial, Response.CatchTrial),
              as.logical) %>%
    # Compute whether response was correct in terms of catch trial identification
    # (*also* count as correct catch performance if "B" was pressed on trial too late)
    group_by(Experiment, ParticipantID, Phase) %>%
    arrange(Trial) %>%
    mutate(
      Response.CountAsAccurateCatch = case_when(
        Response.CatchTrial == Item.isCatchTrial ~ 1,
        # count as correct when trial is catch trial and B is pressed on next trial
        Item.isCatchTrial & lead(Response.CatchTrial) ~ 1, 
        # count as correct when trial is not catch trial, but previous trial was catch trial and B is pressed now
        !Item.isCatchTrial & lag(Item.isCatchTrial) & Response.CatchTrial ~ 1,  
        Response.CatchTrial != Item.isCatchTrial ~ 0,
        T ~ NA_real_),
      Response.CorrectCatch = case_when(
        Response.CatchTrial == Item.isCatchTrial ~ 1,
        # DONT count as correct when trial is catch trial and B is pressed on next trial
        # DONT count as correct when trial is not catch trial, but previous trial was catch trial and B is pressed now
        T ~ NA_real_)) %>%
    ungroup() %>%
    mutate(
      # Make audio condition numerical and flip the order
      Condition.Test.Audio = 32 - as.numeric(as.character(Condition.Test.Audio)),
      # Make factor levels
      ItemID = factor(
        ItemID, 
        levels = c(
          case_when(
            testSet == "A" ~ levels.ItemID_test.A,
            testSet %in% c("B", "C") ~ levels.ItemID_test.B,
            T ~ NA_character_),
          levels.ItemID_exposure)),
      Item.Type = factor(Item.Type, levels = levels.Item.Type),
      Participant.Sex = factor(Participant.Sex, levels.Sex),
      Participant.Race = factor(
        plyr::mapvalues(
          simplifyAnswer(Participant.Race), 
          c("amerind", "asian", "black", "multiple", "other", "white"),
          c("American Indican", "Asian", "Black", "multiple", "other", "White")), 
        levels.Race),
      Participant.Ethnicity = factor(
        plyr::mapvalues(
          simplifyAnswer(Participant.Ethnicity),
          c("Hisp", "NonHisp"),
          c("Hispanic", "Non-Hispanic")),
        levels.Ethnicity)) %>%
    group_by(ParticipantID) %>%
    mutate(
      Duration.AllPhases = (max(Time.EndOfTrial) - min(Time.StartOfStimulus)) / 60000) %>%
    ungroup() %>%
    # Infer key bindings
    # for backward compatibility, use hitid and only run if from mturk
    { if ("hitid" %in% names(.))
      group_by(., hitid) %>%
        mutate(
          Condition.Exposure.Keybindings = factor(paste(sort(na.omit(unique(ifelse(Phase == "exposure" & !is.na(Response), 
                                                                                   paste(Response.Keycode.Character, Response, sep = "-"), 
                                                                                   NA))), decreasing = T), collapse = ";")),
          Condition.Test.Keybindings = factor(paste(sort(na.omit(unique(ifelse(Phase == "test" & !is.na(Response), 
                                                                               paste(Response.Keycode.Character, Response, sep = "-"), 
                                                                               NA))), decreasing = T), collapse = ";"))) %>%
        select(-hitid) %>%
        ungroup() else . } %>%
    # Make sure that differences in survey questions across experiments are taken care of
    # We removed the audio_stall question after Experiment NORM A, as it was uninformative
    mutate(audio_qual = NULL) %>%
    # We added questions about smoking starting at Experiment NORM B
    { if(!("social1" %in% names(.))) 
      mutate(
        ., 
        social1 = NA, 
        social2 = NA, 
        social3 = NA)
      else . } %>%
    # Remove unnecessary columns and order remaining columns
    select(
      -starts_with("CHECK"),
      -starts_with("REMOVE")) %>%
    rename_with(~ gsub("Answer.", "", .x)) %>%
    rename(
      Participant.AudioType = audio_type,
      Participant.AudioStall = audio_stall,
      Participant.VideoStall = video_stall,
      Talker.Sex = sex,
      Talker.PronunciationShift = ssh2,
      Talker.PronunciationProperties = pronun,
      Talker.SpeechDescription = speaker) %>%
    add_exclusions(exclude_based_on_catch_trials = exclude_based_on_catch_trials) %>%
    # Adding missing columns for test-only experiments
    { if (!("Condition.Exposure.Pen" %in% names(.))) mutate(., Condition.Exposure.Pen = NA) else . } %>%
    { if (!("Condition.Exposure.LexicalLabel" %in% names(.))) mutate(., Condition.Exposure.LexicalLabel = NA) else . } %>%
    mutate(Response.ASHI = ifelse(Response == "ASHI", 1, 0)) %>%
    select(
      Experiment, ParticipantID, starts_with("Participant."),
      Condition.Exposure.Pen, Condition.Exposure.LexicalLabel, 
      Condition.Test.Audio, Condition.Test.Pen, Condition.Test.OriginalLabel, Condition.Test.Keybindings,
      Phase, Block, Trial, Task, ItemID, 
      Response, Response.ASHI, Response.RT, Response.CatchTrial,
      Platform,
      starts_with("Talker"),
      starts_with("Duration"),
      starts_with("Exclude"),
      starts_with("Item.")) %>%
    arrange(Experiment, ParticipantID, Phase, Block, Trial) %>%
    sortVars()
  #TODO make variable names more transparent (e.g. Experiment.Platform)
  #TODO if we find any more columns we need, just add "Experiment." or "Participant."

  return(.data)
}


sortVars <- function(.data) {
  .data %>% 
    relocate(
      Experiment,
      starts_with("Experiment."),
      ParticipantID,
      starts_with("Participant."), 
      starts_with("Condition.Exposure"), 
      starts_with("Condition.Test"), 
      Phase, Block, Trial, Task,
      ItemID,
      starts_with("Item."),
      Response,
      starts_with("Response"),
      starts_with("Duration"),
      starts_with("Time"),
      starts_with("Assignment"),
      starts_with("Hit"),
      starts_with("Answer"),
      everything())
}

add_exclusions <- function(data, exclude_based_on_catch_trials = T) {
  # Get rid of any pre-existing exclusion variables, except for exclusion for technical reasons
  data %<>%
    select(-starts_with("Exclude_Participant"), Exclude_Participant.because_of_TechnicalDifficulty)
  # Multiple HITs
  data %<>%
    # Remove all but chronologically first instance of experiments by the same participant
    group_by(Experiment, WorkerID, Platform) %>%
    mutate(
      Exclude_Participant.because_of_MultipleExperiments = ifelse(Platform == "MTurk",
        ifelse(Assignment.Submit.DateTime.UTC > min(Assignment.Submit.DateTime.UTC), T, F),
        ifelse(userDateTime > min(userDateTime), T, F))) %>%
    ungroup()
   # this will not catch workers who take part in multiple *different* experiments
   # (once each of, e.g. Exp1b and later Exp2a.) We mitigated this possibility via blocking
   # WorkerIDs within each platform (MTurk and Prolific) such that the same workerID could not
   # take part in subsequent studies. 
  
  # Lexical decision accuracy
  data %<>%
    group_by(Experiment, ParticipantID) %>%
    mutate(Accuracy.LD = mean(Response.CorrectWordStatus,
                         na.rm = T)) %>%
    ungroup() %>%
    mutate(Exclude_Participant.because_of_Accuracy.LexicalDecision.Normal = ifelse(
             Accuracy.LD >= .85, FALSE, TRUE))

  # Failure to follow instructions
  data %<>%
    mutate(
      Exclude_Participant.because_of_IgnoredInstructions = ifelse(Participant.AudioType %in% c("in-ear", "over-ear"), FALSE, TRUE))
  
  # Exclusion based on catch question
  data %<>%
    mutate(
      Exclude_Participant.because_of_CatchQuestion = ifelse(Talker.Sex == "woman", FALSE, TRUE))
  
  # Exclusion based on catch trials
  data %<>%
    group_by(Experiment, ParticipantID) %>%
    mutate(
      Accuracy.CatchTrials.onCatchTrial = mean(
        case_when(
          Phase != "exposure" ~ NA_real_,
          !Item.isCatchTrial ~ NA_real_,
          Item.isCatchTrial == Response.CatchTrial ~ 1,
          Item.isCatchTrial != Response.CatchTrial ~ 0,
          T ~ NA_real_),
        na.rm = T),
      Accuracy.CatchTrials.onNonCatchTrial = mean(
        case_when(
          #Phase != "exposure" ~ NA_real_,
          Item.isCatchTrial ~ NA_real_,
          Item.isCatchTrial == Response.CatchTrial ~ 1,
          Item.isCatchTrial != Response.CatchTrial ~ 0,
          T ~ NA_real_),
        na.rm = T)) %>%
    ungroup() %>%
    mutate(
      Exclude_Participant.because_of_CatchTrials = case_when(
        !exclude_based_on_catch_trials ~ FALSE,
        Accuracy.CatchTrials.onCatchTrial >= .8 & Accuracy.CatchTrials.onNonCatchTrial > .9 ~ FALSE,
        ## If Accuracy.CatchTrials.onCatchTrial is NaN, then there were no catch trials
        ## in entire experiment (this is expected of 1a-c, 2a-b). 
        is.na(Accuracy.CatchTrials.onCatchTrial) & Accuracy.CatchTrials.onNonCatchTrial > .9 ~ FALSE,
        T ~ TRUE))
  
  # Exclude based on RTs
  data %<>%
    group_by(Experiment, ParticipantID) %>%
    # Get mean and SD of log-transformed RTs for each participant
    mutate(
      Response.log_RT = log10(ifelse(Response.RT < 0, NA_real_, Response.RT)),
      Response.log_RT_scaled = scale(Response.log_RT),
      Response.mean_log_RT = mean(Response.log_RT, na.rm = T)) %>%
    group_by(Trial) %>%
    mutate(Exclude_Trial.because_of_RT = ifelse(is.na(Response.log_RT_scaled) | abs(scale(Response.log_RT_scaled)) > 3, T, F)) %>%
    ungroup() %>%
    mutate(Exclude_Participant.because_of_RT = ifelse(abs(scale(Response.mean_log_RT)) > 3, T, F))
  
  # Exclude based on too many missing trials
  data %<>%
    group_by(Experiment, ParticipantID) %>%
    mutate(Exclude_Participant.because_of_MissingTrials = ifelse(sum(Phase == "test" & (Exclude_Trial.because_of_RT | is.na(Response))) > 7, T, F)) %>%
    ungroup() 
  
  # Swapped keys
  data %<>%
    group_by(Experiment, ParticipantID) %>%
    nest() %>%
    mutate(
      IdentificationModel = 
        map(
          data, 
          # Fit GLM if there are at least 24 responses in test
          ~ if ((.x %>% filter(Phase == "test" & !is.na(Response)) %>% nrow(.)) > 24) {
            glm(Response == "ASHI" ~ Condition.Test.Audio, data = .x %>% filter(Phase == "test"), family = binomial) %>% 
              tidy()
          } else { 
            # Otherwise just add a data frame with NAs.
            data.frame(term = c("(Intercept)", "Condition.Test.Audio"), estimate = NA, p.value = NA) }) ,
      IdentificationSlope = 
        map(
          IdentificationModel, 
          ~ .x %>% 
            filter(term == "Condition.Test.Audio") %>% 
            pull(estimate)) %>% 
        unlist(),
      Exclude_Participant.because_of_SwappedKeys = 
        ifelse(
          IdentificationSlope < 0 &
            map(
              IdentificationModel, ~ .x %>% 
                filter(term == "Condition.Test.Audio") %>% 
                pull(p.value) < .05) %>% 
            unlist(), 
          T, F)) %>%
    select(-c(IdentificationModel, IdentificationSlope)) %>%
    unnest(data) %>%
    ungroup()
  
  # Create 'reason' variable
  data %<>%
    { if (!("Exclude_Participant.because_of_Accuracy.LexicalDecision.Normal" %in% names(.))) 
      mutate(., Exclude_Participant.because_of_Accuracy.LexicalDecision.Normal = F) else . } %>%
    mutate(
      Exclude_Participant.Reason = case_when(
        Exclude_Participant.because_of_TechnicalDifficulty ~ "Technical difficulty",
        Exclude_Participant.because_of_MultipleExperiments ~ "Repeat participant",
        Exclude_Participant.because_of_IgnoredInstructions ~ "No headphones",
        Exclude_Participant.because_of_CatchQuestion ~ "Catch question or trials",
        exclude_based_on_catch_trials & Exclude_Participant.because_of_CatchTrials ~ "Catch question or trials",
        Exclude_Participant.because_of_Accuracy.LexicalDecision.Normal ~ "Lexical decision accuracy",
        Exclude_Participant.because_of_SwappedKeys ~ "Swapped response keys",
        Exclude_Participant.because_of_RT ~ "Reaction time",
        Exclude_Participant.because_of_MissingTrials ~ "Too many missing trials",
        T ~ "none"))
  
  # Handle LJ18
  data %<>%
    mutate(
      across(contains("because"), ~ ifelse(Experiment == "LJ18-NORM", NA, .x)),
      Exclude_Participant.Reason = factor(ifelse(Experiment == "LJ18-NORM", "none", Exclude_Participant.Reason)))
  
  return(data)
}


exclusionPlot <- function(data) {
  p <- 
    data %>%
    droplevels() %>%
    group_by(Experiment, ParticipantID, Exclude_Participant.Reason) %>%
    mutate(Response.log_RT = log10(Response.RT)) %>%
    summarise_at("Response.log_RT", .funs = list("mean" = mean, "sd" = sd), na.rm = T) %>%
    ggplot(aes(x = mean, y = sd)) +
    geom_point(aes(color = Exclude_Participant.Reason, shape = Exclude_Participant.Reason)) +
    geom_rug() +
    geom_text(
      data = 
        . %>% 
        group_by(Experiment) %>% 
        summarise(
          mean = max(mean),
          sd = max(sd),
          label = paste0(
            "Excl. N=",
            sum(Exclude_Participant.Reason != "none"),
            " (",
            percent(sum(Exclude_Participant.Reason != "none") / length(Exclude_Participant.Reason)),
            ")")) %>%
        ungroup() %>%
        mutate(mean = max(mean), sd = max(sd)),
      aes(label = label), color = "red", hjust = 1, vjust = 1) +
    scale_x_continuous("mean log-RT (in msec)") +
    scale_y_continuous("SD of log-RT") +
    scale_color_manual(
      "Reason for exclusion",
      breaks = c("none", 
                 "Technical difficulty", "Repeat participant", "No headphones", "Catch question", "Catch trials", "Lexical decision accuracy", "Swapped response keys", "Reaction time", "Too many missing trials"),
      values = c("black", rep("red", 9))) +
    scale_shape_manual(
      "Reason for exclusion",
      breaks = c("none", 
                 "Technical difficulty", "Repeat participant", "No headphones", "Catch question", "Catch trials", "Lexical decision accuracy", "Swapped response keys", "Reaction time", "Too many missing trials"),
      values = c(16, 15, 17, 14, 10, 3, 4, 8, 9, 13)) +
    facet_wrap(~ Experiment, nrow = 1)
  
  n.experiment <- length(unique(data$Experiment))
  ggsave(p,
         filename = paste0("../figures/exclusions-", paste(unique(data$Experiment), collapse = "-"), ".png"),
         width = unit(n.experiment * 2.5, "cm"), height = unit(2.5 + 1, "cm"))
  
  plot(p)
}


excludeData <- function(data) {
  data %<>%
    filter(
      Exclude_Participant.Reason == "none" | Experiment == "LJ18-NORM",
      !Exclude_Trial.because_of_RT | Experiment == "LJ18-NORM")
  
  return(data)
}

run_exclusions <- function(data, experiment) {
  data %<>% filter(Experiment %in% experiment)
  print(
    data %>% 
      distinct(Experiment, ParticipantID, Exclude_Participant.Reason) %>% 
      group_by(Experiment, Exclude_Participant.Reason) %>% 
      tally() %>% 
      group_by(Experiment) %>%
      mutate(Percent = percent(n / sum(n))))
  
  message(
    data %>% 
      distinct(Experiment, ParticipantID, Duration.Assignment) %>% 
      group_by(Experiment) %>%
      summarise(across(Duration.Assignment, list("mean" = mean, "sd" = sd))) %>%
      mutate(msg = paste0(    
        "Participants in ",
        Experiment,
        " took on average ", 
        round(Duration.Assignment_mean, 1),
        " minutes to complete the experiment (SD = ",
        round(Duration.Assignment_sd, 1),
        ").")) %>% 
      pull(msg) %>%
      paste(., collapse = "\n"))
  exclusionPlot(data)
  
  data %<>% 
    excludeData()
  
  message(
    "\nData submitted for analysis contains ", 
    nrow(data %>% filter(is.na(Response.ASHI))),
    " missing observations (",
    percent(nrow(data %>% filter(is.na(Response.ASHI))) / nrow(data)),
    "), leaving ",
    nrow(data %>% filter(!is.na(Response.ASHI))),
    " observations from ",
    data %>% filter(!is.na(Response.ASHI)) %>% pull(ParticipantID) %>% unique() %>% length(),
    " participants from ",
    data %>% filter(!is.na(Response.ASHI)) %>% pull(Experiment) %>% unique() %>% length(),
    " experiment(s).")
  
  data %>%
    filter(!is.na(Response.ASHI))
}
## Data summaries ------------------------------------------------------- 

getParticipantsPerList <- function(d) {
  d %>%
    excludeData() %>%
    select(ParticipantID, HitID, starts_with("Condition.Exposure"), Condition.Test.Keybindings) %>%
    distinct() %>%
    group_by(
      Condition.Exposure.Pen, Condition.Exposure.LexicalLabel, Condition.Exposure.Keybindings,
      Condition.Test.Keybindings) %>% 
    tally() %>%
    arrange(vars(everything()))
}


getMissingParticipantsPerList <- function(d, targetedParticipantsPerList) {
  d %>%
    excludeData() %>%
    select(ParticipantID, HitID, starts_with("Condition.Exposure"), Condition.Test.Keybindings) %>%
    distinct() %>%
    group_by(
      Condition.Exposure.Pen, Condition.Exposure.LexicalLabel, Condition.Exposure.Keybindings,
      Condition.Test.Keybindings) %>% 
    tally() %>%
    mutate(n = targetedParticipantsPerList - n) %>%
    filter(n > 0) %>%
    arrange(vars(everything()))
}


plotDemographicInformation <- function(
  d, 
  rows = "Condition.Exposure.Pen",
  cols = "Condition.Exposure.LexicalLabel"
) {
  p.answer <- d %>%
    group_by(Experiment, ParticipantID) %>%
    select(starts_with("Participant."), starts_with("Condition"), "Exclude_Participant.Reason") %>%
    summarize_all(first) %>%
    ggplot(aes(x = Participant.Age, fill = ifelse(Exclude_Participant.Reason == "none", "no", "yes"))) +
    scale_fill_manual("Excluded", values = c("black", "red")) +
    facet_grid(
      cols = vars(!!! syms(cols)), 
      rows = vars(!!! syms(rows)), 
      labeller = label_both)
  
  plot(p.answer + geom_histogram(color = NA, position = position_stack()) + xlab("reported age"))
  plot(p.answer + geom_bar() + aes(x = Participant.Sex) + xlab("reported sex"))
  plot(last_plot() + aes(x = Participant.Ethnicity) + 
         xlab("reported ethnicity") +
         theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  plot(last_plot() + aes(x = Participant.Race) +
    xlab("reported race")) 
}

## Model fitting and summarizing ----------------------------------------------------------------
# Prep for analysis
prep_for_analysis <- function(data) {
  require(rlang)
  
  data %<>%
    { if (length(unique(.$Experiment)) == 2) {
      mutate(
        .,
        Experiment = 
          "contrasts<-"(factor(Experiment), , cbind(c(-.5, .5)))) 
    } else if (length(unique(.$Experiment)) > 1) {
      mutate(
        .,
        Experiment = 
          "contrasts<-"(factor(Experiment), , contr.sum(length(unique(Experiment))) * .5)) 
    } else . } %>%
    { if (length(unique(data$Condition.Exposure.LexicalLabel)) > 1) {
      mutate(
        .,
        Condition.Exposure.Pen = 
          "contrasts<-"(factor(Condition.Exposure.Pen), , cbind("M" = c(-0.5, 0.5))),
        Condition.Exposure.LexicalLabel = 
          "contrasts<-"(factor(Condition.Exposure.LexicalLabel), , cbind("SH" = c(-0.5, 0.5))))
    } else . } %>%
    { if (all(!is.na(.$Condition.Test.Pen), !is.na(.$Condition.Test.OriginalLabel))) 
      mutate(
        .,
        Condition.Test.Pen = 
          "contrasts<-"(factor(Condition.Test.Pen), , cbind("M" = c(-0.5, 0.5))),
        Condition.Test.OriginalLabel = 
          "contrasts<-"(factor(Condition.Test.OriginalLabel), , cbind("SH" = c(-0.5, 0.5)))) else . } %>%
    mutate(Block = Block - 1) 
  
  if (length(unique(data$Experiment)) == 2)
    dimnames(contrasts(data$Experiment))[[2]] = sort(unique(data$Experiment))[2]
  
  return(data)
}

fit_test_model <- function(data, experiment, formula = NULL, file = NULL) {
  data %<>% filter(Experiment %in% experiment)
  multiple_experiments <- length(unique(data$Experiment)) > 1 
  exposure_experiment <- length(unique(data$Condition.Exposure.LexicalLabel)) > 1
  
  formula <- if (is.null(formula)) {
    bf(paste(
      "Response.ASHI ~",
      "1 +", 
      if (exposure_experiment) "Condition.Exposure.LexicalLabel * Condition.Exposure.Pen *" else "",
      "Condition.Test.OriginalLabel * Condition.Test.Pen * mo(Block) * mo(Condition.Test.Audio)",
      if (multiple_experiments) "* Experiment +" else "+",
      "(1 + Condition.Test.OriginalLabel * Condition.Test.Pen * mo(Condition.Test.Audio) | ParticipantID)"))
  } else formula
  
  m <- brm(
    formula,
    data = 
      data %>% 
      prep_for_analysis(),
    family = "bernoulli",
    prior = my_priors,
    sample_prior = "yes",
    backend = "cmdstanr",
    chains = 4, 
    warmup = if (multiple_experiments | exposure_experiment) 2000 else 1000,
    iter = if (multiple_experiments | exposure_experiment) 3000 else 2000,
    control = list(adapt_delta = if (multiple_experiments | exposure_experiment) .95 else .9),
    cores = min(parallel::detectCores(), 4), 
    threads = threading(threads = 4),
    file = if (is.null(file)) paste("../models/Exp", paste(experiment, collapse = "-"), sep = "-") else file)
  
  return(m)
}

format_hypothesis_tables <- function(table, experiment, BF.max = 4000) {
  table %>%
  rename(BF = Evid.Ratio) %>%
  mutate(
    Experiment = experiment,
    across(
      c("Estimate", "Est.Error", starts_with("CI"), "Post.Prob"),
      ~ signif(.x, 3)),
    BF = ifelse(is.infinite(BF), paste(">", BF.max), as.character(round(BF, 1)))) %>% 
  relocate(Experiment, everything())
}

my_hypotheses <- function(m, experiment, plot = F) { 
  exposure_experiment <- length(unique(m$data$Condition.Exposure.LexicalLabel)) > 1
  
  # mo() operators imply that the effects of the other variables are assessed at the reference level of the
  # monotonic predictor. For Block, this is exactly what we want: evaluation of effects in the first Block.
  # However, for the continuum, we'd like to assess effects in the middle of the continuum. This is taken 
  # into account below.
  h <- list()
  l <- list()
  if (exposure_experiment) {
    l[["exposure.label"]] <- 
      { h[["exposure.label"]] <- hypothesis(
        m, 
        c(
          "b_Condition.Exposure.LexicalLabelSH + 2.5 * bsp_moCondition.Test.Audio:Condition.Exposure.LexicalLabelSH > 0",
          "b_Condition.Exposure.LexicalLabelSH:Condition.Exposure.PenM + 2.5 * bsp_moCondition.Test.Audio:Condition.Exposure.LexicalLabelSH:Condition.Exposure.PenM < 0",
          "b_Condition.Exposure.LexicalLabelSH:Condition.Exposure.PenM:Condition.Test.PenM + 2.5 * bsp_moCondition.Test.Audio:Condition.Exposure.LexicalLabelSH:Condition.Exposure.PenM:Condition.Test.PenM > 0"),
#          "b_Condition.Exposure.PenM + 2.5 * bsp_moCondition.Test.Audio:Condition.Exposure.PenM < 0"),
        class = NULL) } %>%
      .[["hypothesis"]] %>% 
      mutate(Hypothesis = c(
        "More SH responses after SH-biased exposure",
        "Reduced effect of SH-bias when pen-in-mouth during critical exposure",
        "Enhanced effect of SH-bias when pen location matches in exposure & test")) %>%
      #        "Less SH responses after pen-in-mouth exposure")) %>%
      format_hypothesis_tables(experiment, BF.max = ndraws(m)) %>%
      kable(caption = "Effects of exposure.")
  }
  
  l[["test.pen"]] <- 
    { h[["test.pen"]] <- hypothesis(
      m, 
      c(
        # There are 5 continuum steps above the baseline, so we add 2.5 * the interaction of continuum and the 
        # effect of interest to the effect of interest. (a more precise estimate could be obtained by following
        # Figure 1 in Bürkner & Charpentier, which takes into account the specific simo estimates).
        "b_Condition.Test.PenM  + 2.5 * bsp_moCondition.Test.Audio:Condition.Test.PenM < 0",
        "bsp_moCondition.Test.Audio:Condition.Test.PenM < 0",
        "b_Condition.Test.OriginalLabelSH:Condition.Test.PenM + 2.5 * bsp_moCondition.Test.Audio:Condition.Test.OriginalLabelSH:Condition.Test.PenM < 0",
        "bsp_moCondition.Test.Audio:Condition.Test.OriginalLabelSH:Condition.Test.PenM < 0"),
      class = NULL) } %>%
    .[["hypothesis"]] %>% 
    mutate(Hypothesis = c(
      "Pen location Mouth -> fewer ASHI-responses",
      "Pen effect increases for more ASHI-like acoustic input",
      "Pen effect increases for visually ASHI-biased input",
      "Pen effect increases even more when acoustic and visual input is ASHI-biased")) %>%
    format_hypothesis_tables(experiment, BF.max = ndraws(m)) %>%
    kable(caption = "Effects of pen location.")
  
  l[["test.cues"]] <- 
    { h[["test.cues"]] <- hypothesis(
      m, 
      c("bsp_moCondition.Test.Audio > 0", 
        "b_Condition.Test.OriginalLabelSH + 2.5 * bsp_moCondition.Test.Audio:Condition.Test.OriginalLabelSH > 0",
        "bsp_moCondition.Test.Audio:Condition.Test.OriginalLabelSH = 0"), 
      class = NULL, scope = "standard") } %>%
    .[["hypothesis"]] %>% 
    mutate(Hypothesis = c(
      "Acoustic continuum more ASHI-like -> more ASHI-responses",
      "Visual bias ASHI -> more ASHI-responses",
      "Acoustic and visual bias effects are independent")) %>% 
    format_hypothesis_tables(experiment, BF.max = ndraws(m)) %>%
    kable(caption = "Effects of acoustic continuum and visual bias.")
  
  l[["test.block"]] <- 
    { h[["test.block"]] <- hypothesis(
      m, 
      c(
        if (exposure_experiment) "bsp_moBlock:Condition.Exposure.LexicalLabelSH + 2.5 * bsp_moBlock:moCondition.Test.Audio:Condition.Exposure.LexicalLabelSH < 0" else NULL,
        "bsp_moBlock:Condition.Test.PenM + 2.5 * bsp_moBlock:moCondition.Test.Audio:Condition.Test.PenM = 0",
        "bsp_moBlock:moCondition.Test.Audio = 0", 
        "bsp_moBlock:Condition.Test.OriginalLabelSH + 2.5 * bsp_moBlock:moCondition.Test.Audio:Condition.Test.OriginalLabelSH = 0"),
      class = NULL) } %>%
    .[["hypothesis"]] %>% 
    mutate(Hypothesis = c(
      if (exposure_experiment) "Effect of SH-biased exposure reduces over blocks" else NULL,
      "Pen effect is stable over blocks",
      "Acoustic effect is stable over blocks",
      "Visual bias effect is stable over blocks")) %>%
    format_hypothesis_tables(experiment, BF.max = ndraws(m)) %>%
    kable(caption = "Changes across blocks.")
  
  if (plot) for (H in h) plot(H)
  return(l)
}


## Functions for formatting knitr output 
summarize_effect <- function(model, effect, parentheses = T) {
  h <- hypothesis(model, effect)$hypothesis
  op <- gsub("^.*(<|>|=).*$", "\\1", h$Hypothesis)
  
  m <- paste0("$\\hat{\\beta} = ", round(h$Estimate, 3), "; BF_{", op, "0} = ", round(h$Evid.Ratio, 1), "; p_{posterior} = ", round(h$Post.Prob, 2), "$")
  if (parentheses) m <- paste0("(", m, ")")
  
  return(m)
}


simplifyPredictorNames <- function(model) {
  # get all fixed effects predictors, parsed into their parts
  preds <- strsplit(gsub("^b_", "", grep("^b_", parnames(model), value = T)), ":") %>%
    # substitute 
    map(~gsub(".*Condition\\.([A-Za-z]+)\\.(Lexical|Original|Pen|Audio).*$", "\\2 \\(\\1\\)", .x)) %>%
    map(~gsub(" \\(Test\\)", "", .x)) %>%
    map(~paste(.x, collapse = " : ")) %>%
    unlist()
}





## Model visualization ------------------------------------------------------- 
plot_data <- function(data, experiment, background_experiment = NULL) {
  require(dplyr)
  require(ggplot2)
  require(cowplot)
  
  data %<>% filter(Experiment %in% append(experiment, background_experiment))
  exposure_experiment <- length(unique(data[data$Experiment == experiment,]$Condition.Exposure.LexicalLabel)) > 1
  if (!is.null(background_experiment)) exposure_background_experiment <- length(unique(data[data$Experiment == background_experiment,]$Condition.Exposure.LexicalLabel)) > 1
  # If experiment is exposure experiment but background experiment is not, make the background 
  # experiment data available for each facet of the experiment data.
  if (!is.null(background_experiment) & exposure_experiment) {
    if (!exposure_background_experiment) 
      data %<>%
      filter(Experiment == experiment) %>%
      bind_rows(
        data %>%
          filter(Experiment == background_experiment) %>%
          select(-Condition.Exposure.Pen) %>%
          crossing(Condition.Exposure.Pen = unique(data[data$Experiment == experiment,]$Condition.Exposure.Pen)))
  }
  if (exposure_experiment)
    data %<>%
    mutate(Condition.Exposure.Pen = plyr::mapvalues(Condition.Exposure.Pen, c("M", "H"), c("pen-in-**mouth** during critical exposure", "pen-in-**hand** during critical exposure")))
  
  shared_stats <- function(data = NULL, color = "black", dodge = .5, treat_as_exposure_experiment = exposure_experiment) {
    if (treat_as_exposure_experiment) {
      list(
        stat_summary(
          data = data, fun = mean, geom = "line", 
          position = position_dodge(dodge), alpha = .5),
        stat_summary(
          data = data, fun = mean, geom = "point", 
          position = position_dodge(dodge), size = 1),
        stat_summary(
          data = data, fun.data = mean_cl_boot, geom = "linerange", 
          position = position_dodge(dodge), linetype = 1, alpha = .65))
    } else {
      list(
        stat_summary(
          data = data, fun = mean, geom = "line", 
          position = position_dodge(dodge), alpha = .5, color = color),
        stat_summary(
          data = data, fun = mean, geom = "point", 
          position = position_dodge(dodge), size = 1, color = color),
        stat_summary(
          data = data, fun.data = mean_cl_boot, geom = "linerange", 
          position = position_dodge(dodge), linetype = 1, alpha = .65, color = color))
    }
  }
  shared_formatting <- 
    list(
      scale_y_continuous('Proportion "ASHI"-responses'),
      scale_shape_manual(
        "Pen location", 
        breaks = levels.test.pen_locations, labels = labels.test.pen_locations, values = shapes.test.pen_locations),
      scale_linetype_manual(
        "Pen location", 
        breaks = levels.test.pen_locations, labels = labels.test.pen_locations, values = linetypes.test.pen_locations),
      facet_wrap(~ Experiment, ncol = 1),
      theme(strip.text = element_markdown()))
  
  .groups <- c("Experiment", "ParticipantID", "Condition.Test.Pen")
  if (exposure_experiment) {
    message("Detected more than one exposure condition. Treating this as an exposure experiment.")
    .groups = append(.groups, c("Condition.Exposure.LexicalLabel", "Condition.Exposure.Pen"))
    shared_formatting[[length(shared_formatting) + 1]] <- 
    scale_color_manual("Label",
      breaks = levels.exposure.lexical_labels,
      values = colors.exposure.lexical_labels,
      aesthetics = c("color", "fill"))
  }
  
  aggregate <- function(data, ..., e = experiment) {
    groups = enquos(...)
    data %>%
      filter(Experiment %in% e) %>%
      mutate(Experiment = gsub("CISP-", "Exp ", Experiment)) %>%
      droplevels() %>%
      mutate(Condition.Test.Pen = ifelse(Condition.Test.Pen == "audio-only", "H", as.character(Condition.Test.Pen))) %>%
      group_by(!!! groups) %>%
      summarise(Response.ASHI = mean(Response.ASHI)) 
  }
  
  p <- list()
  p[[1]] <- 
    data %>% 
    aggregate(!!! syms(.groups), Condition.Test.Audio) %>%
    ggplot(aes(x = Condition.Test.Audio, y = Response.ASHI, shape = Condition.Test.Pen, linetype = Condition.Test.Pen)) +
    scale_x_continuous(
      'Acoustic continuum', 
      breaks = as.integer(append(range(data$Condition.Test.Audio), mean(range(data$Condition.Test.Audio))))) +
    shared_stats() +
    shared_formatting + 
    coord_cartesian(xlim = range(data$Condition.Test.Audio), ylim = c(.1, .9))
  
  # Potentially move to shared_formatting
  if (exposure_experiment) 
    p[[1]] <- 
    p[[1]] + 
    aes(color = Condition.Exposure.LexicalLabel, fill = Condition.Exposure.LexicalLabel) +
    facet_wrap(~ Condition.Exposure.Pen, nrow = 2)
  
  if (!is.null(background_experiment))
    p[[1]] <- 
    p[[1]] + 
    shared_stats(
      data = data %>%
        aggregate(!!! syms(.groups), Condition.Test.Audio, 
                  e = background_experiment) %>% 
        ungroup() %>%
        select(-Experiment) %>%
        crossing(Experiment = gsub("CISP-", "Exp ", experiment)), 
      dodge = 1, 
      color = "gray75",
      treat_as_exposure_experiment = exposure_background_experiment)
  
  # shorter facet labels for visual label plot
  if (exposure_experiment)
    data %<>%
    mutate(Condition.Exposure.Pen = plyr::mapvalues(Condition.Exposure.Pen, c("pen-in-**mouth** during critical exposure", "pen-in-**hand** during critical exposure"), c("pen-in-**mouth**", "pen-in-**hand**")))
  p[[2]] <- 
    data %>%
    aggregate(!!! syms(.groups), Condition.Test.OriginalLabel) %>%
    ggplot(aes(x = Condition.Test.OriginalLabel, y = Response.ASHI, 
               shape = Condition.Test.Pen, linetype = Condition.Test.Pen,
               group = Condition.Test.Pen)) +
    shared_stats(dodge = .25) +
    shared_formatting + 
    scale_x_discrete('Visual bias') +
    # scale_color_manual(
    #   'Visual bias', 
    #   breaks = levels.test.visual_labels, labels = labels.test.visual_labels, values = colors.test.visual_labels) +
    coord_cartesian(ylim = c(.1, .9)) +
    theme(legend.position = "none")
  
  if (exposure_experiment) 
    p[[2]] <- 
    p[[2]] + 
    aes(color = Condition.Exposure.LexicalLabel, fill = Condition.Exposure.LexicalLabel, 
        group = interaction(Condition.Exposure.LexicalLabel, Condition.Test.Pen)) +
    facet_wrap(~ Condition.Exposure.Pen, nrow = 2)

  if (!is.null(background_experiment) & !("LJ18-NORM" %in% background_experiment))
    p[[2]] <- 
    p[[2]] + 
    shared_stats(
      data = data %>%
        aggregate(!!! syms(.groups), Condition.Test.OriginalLabel, 
                  e = background_experiment) %>% 
        ungroup() %>%
        select(-Experiment) %>%
        crossing(Experiment = gsub("CISP-", "Exp ", experiment)), 
      dodge = .5, 
      color = "gray75",
      treat_as_exposure_experiment = exposure_background_experiment)
  
  # p[[3]] <- 
  #   data %>%
  #   aggregate(Experiment, ParticipantID, Condition.Test.Pen, Condition.Test.Audio, Condition.Test.OriginalLabel) %>%
  #   ggplot(aes(x = Condition.Test.Audio, y = Response.ASHI, color = Condition.Test.OriginalLabel, 
  #              shape = Condition.Test.Pen, linetype = Condition.Test.Pen)) +
  #   scale_x_continuous('Acoustic continuum', limits = range(data$Condition.Test.Audio)) +
  #   scale_color_manual(
  #     'Visual bias', 
  #     breaks = levels.test.visual_labels, labels = labels.test.visual_labels, values = colors.test.visual_labels) +
  #   shared_components +
  #   coord_cartesian(xlim = range(data$Condition.Test.Audio))
  #   theme(legend.position = "none")
  
  cowplot::plot_grid(
    plotlist = p, 
    align = "hv", axis = "btrl", 
    ncol = length(p), labels = paste0(LETTERS[1:length(p)], ")"),
    rel_widths = c(.7, .3), rel_heights = 1.5)
}


plot_model_predictions <- function(m) {
  plot(conditional_effects(m, method = "posterior_linpred"), ask = F)
}

plot_model_predictions_for_paper <- function(m) {
  conditional_effects(
    m, 
    method = "posterior_linpred")
}




get_scale_name <- function(variable) {
  case_when(
    variable == "Condition.Exposure.LexicalLabel" ~ "Shifted sound (exposure)",
    variable == "Condition.Exposure.Pen" ~ "Pen location (exposure)",
    variable %in% c("Condition.Test.Audio", "cCondition.Test.Audio") ~ "Acoustic continuum",
    variable == "Condition.Test.OriginalLabel" ~ "Visual label (test)",
    variable == "Condition.Test.Pen" ~ "Pen location (test)",
    T ~ variable
  )
}

get_scale_values <- function(variable, scale) {
  x <- case_when(
    variable == "Condition.Exposure.LexicalLabel" & scale %in% c("color", "colour", "fill") ~ colors.exposure.lexical_labels,
    variable == "Condition.Test.OriginalLabel" & scale %in% c("color", "colour", "fill") ~ colors.test.visual_labels,
    variable == "Condition.Test.Pen" & scale %in% c("color", "colour", "fill") ~ colors.test.pen_locations,
    variable == "Condition.Exposure.Pen" & scale %in% ("linetype") ~ as.character(linetypes.exposure.pen_locations),
    variable == "Condition.Test.OriginalLabel" & scale %in% ("linetype") ~ as.character(linetypes.test.visual_labels),
    variable == "Condition.Test.Pen" & scale %in% ("linetype") ~ as.character(linetypes.test.pen_locations),
    variable == "Condition.Exposure.Pen" & scale %in% ("shape") ~ as.character(shapes.exposure.pen_locations),
    variable == "Condition.Test.OriginalLabel" & scale %in% ("shape") ~ as.character(shapes.test.visual_labels),
    variable == "Condition.Test.Pen" & scale %in% ("shape") ~ as.character(shapes.test.pen_locations),
    T ~ NA_character_
  )
  
  if (scale %in% c("shape", "linetype")) x <- as.numeric(x)
  return(x)
}

get_continuum_breaks <- function(exp) {
  return(
    case_when(
      exp == "NORM A" ~ c(13,15,16,17,18,20),
      exp %in% c("A", "NORM B") ~ c(10,13,14,15,16,20),
      exp %in% c("NORM C") ~ c(12,17,18,19,20,24),
      T ~ NA_real_))
}

get_continuum_labels <- function(exp, sep = " ") {
  steps <- get_continuum_breaks(exp)
  return(paste(
    steps,
    c("most /s/-like", rep("", length(steps) - 2), "most /ʃ/-like"),
    sep = sep))
}

plot_test_predictions <- function(
  model,
  data = model$data,
  add_data = T,
  exp,
  predict_at = list(),
  mapping = aes(),
  facets = aes(rows = NULL, cols = NULL),
  breaks = list(
    Condition.Exposure.LexicalLabel = levels.exposure.lexical_labels,
    Condition.Exposure.Pen = levels.exposure.pen_locations,
    Block = 1:6,
    cCondition.Test.Audio = sort(unique(model$data$cCondition.Test.Audio)),
    Condition.Test.OriginalLabel = levels.test.visual_labels,
    Condition.Test.Pen = levels.test.pen_locations
  ),
  labels = list(
    Condition.Exposure.LexicalLabel = labels.exposure.lexical_labels,
    Condition.Exposure.Pen = labels.exposure.pen_locations,
    Block = 1:6,
    cCondition.Test.Audio = get_continuum_labels(exp),
    Condition.Test.OriginalLabel = labels.test.visual_labels,
    Condition.Test.Pen = labels.test.pen_locations
  ),
  response_scale = "linear",
  resolution_along_x = 51,
  ndraws = NULL,
  CIs = c(.95),
  alpha = .9
) {
  assert_that(!is.null(mapping[["x"]]), msg = "You must specify a mapping for the x-aesthetic.")
  
  unquoted_mapping <- map(mapping, ~ if (is_quosure(.x)) { as_name(.x) } else .x)
  unquoted_facets <- map(facets, ~ if (is_quosure(.x)) { as_name(.x) } else .x)
  mapped_predictors <- unique(c(as.character(unlist(unquoted_mapping)), as.character(unlist(unquoted_facets))))
  unmapped_predictors <- setdiff(names(data), c(mapped_predictors, "Response", "ParticipantID", "ItemID"))
  
  message(paste0("Plotting predictions for ", 
                 paste(mapped_predictors, collapse = " * "), 
                 ".\nPredictions will be generated for all existing values of predictors unless otherwise specified, as in the predict_at values for ", 
                 paste(names(predict_at), collapse = ", "),
                 ".\nPredictors that are neither mapped onto aesthetics nor specified in predict_at will be marginalized over."))
  
  d.grid <-
    data %>%
    data_grid(
      # add every variable that is not already specified in predict_at
      !!! syms(setdiff(c(mapped_predictors, unmapped_predictors), names(predict_at)))) %>%
    crossing(
      Block = predict_at[["Block"]],
      cCondition.Test.Audio = predict_at[["cCondition.Test.Audio"]])
  
  d.grid %<>%
    add_fitted_draws(
      model, 
      scale = "linear",
      n = ndraws, 
      value = "fitted", 
      re_formula = NA) %>% # set to NULL to include random effects (must then add them to data_grid), NA to ignore REs
    # average out unmapped predictors (I originally tried to do so before add_fitted_draws, but that requires making a
    # a choice for factor predictors as to which *level* to predict)
    group_by(!!! syms(mapped_predictors), .draw) %>%
    summarise(fitted = mean(fitted)) %>%
    { if (response_scale == "response") mutate(., fitted = plogis(fitted)) else . }
  
  # get labels for facets
  if (length(unquoted_facets) > 0) {
    my_row_labeller <- as_labeller(function(string) { 
      paste(
        get_scale_name(unquoted_facets[["rows"]]), 
        labels[[unquoted_facets[["rows"]]]][which(breaks[[unquoted_facets[["rows"]]]] == string)], 
        sep = ": ") })
    my_col_labeller <- as_labeller(function(string) { 
      paste(
        get_scale_name(unquoted_facets[["cols"]]), 
        labels[[unquoted_facets[["cols"]]]][which(breaks[[unquoted_facets[["cols"]]]] == string)], 
        sep = ": ") })
  }
  
  p <- 
    d.grid %>%
    ggplot(mapping = mapping) +
    # Facet if there's something to facet
    { if (length(unquoted_facets) > 0) facet_grid(
      rows = vars(!! facets[["rows"]]), 
      cols = vars(!! facets[["cols"]]), 
      labeller = labeller(
        .rows = my_row_labeller,
        .cols = my_col_labeller)) } +
    stat_lineribbon(mapping = aes(y = fitted), inherit.aes = T, .width = CIs, alpha = .9, ) +
    # Add the raw data?
    { if (add_data) 
      stat_summary(
        # Display all data that matches the predict_at values.
        data = model$data %>%
          filter(across(
            names(predict_at), 
            ~ if (is.numeric(.x)) { 
              between(.x, min(predict_at[[cur_column()]]), max(predict_at[[cur_column()]])) 
            } else .x %in% predict_at[[cur_column()]])) %>%
          group_by(!!! syms(mapped_predictors), ParticipantID) %>%
          summarise(Response = mean(ifelse(Response == "ASHI", 1, 0))),
        mapping = aes(y = Response),
        fun.data = function(x) { 
          y <- mean_cl_boot(x)
          if (response_scale == "linear") y %<>% mutate_all(qlogis) 
          return(y) },
        # summarise(
        #   Response.proportion = mean(ifelse(Response == "ASHI", 1, 0)),
        #   Response.emplog = emplog(Response.proportion, length(Response))),
        # mapping = aes(y = if (response_scale == "linear") Response.emplog else Response.proportion),
        # fun.data = mean_cl_boot,
        geom = "pointrange", 
        position = position_dodge(.05)) } +
    # Set up x-axis
    scale_x_continuous(
      name = get_scale_name(unquoted_mapping[["x"]]),
      breaks = breaks[[unquoted_mapping[["x"]]]],                  
      labels = labels[[unquoted_mapping[["x"]]]],
      expand = c(0,0)) +
    scale_y_continuous(paste('Fitted', if (response_scale == "linear") "log-odds" else "proportion", 'of "ASHI" responses'))
  
  # Go through all aesthetics except for x and y (which has already been taken care of)
  if (!is.null(unquoted_mapping[["colour"]])) {
    p <- 
      p + 
      scale_color_manual(
        name = get_scale_name(unquoted_mapping[["colour"]]),
        breaks = breaks[[unquoted_mapping[["colour"]]]],
        labels = labels[[unquoted_mapping[["colour"]]]],
        values = get_scale_values(unquoted_mapping[["colour"]], scale = "color")) }
  if (!is.null(unquoted_mapping[["fill"]])) {
    p <- 
      p + 
      scale_fill_manual(
        name = get_scale_name(unquoted_mapping[["fill"]]),
        breaks = breaks[[unquoted_mapping[["fill"]]]],
        labels = labels[[unquoted_mapping[["fill"]]]],
        values = get_scale_values(unquoted_mapping[["fill"]], scale = "fill"))  
  } else p <- p + scale_fill_brewer(palette = "Greys")
  
  if (!is.null(unquoted_mapping[["shape"]])) { 
    p <- 
      p + 
      scale_shape_manual(
        name = get_scale_name(unquoted_mapping[["shape"]]),
        breaks = breaks[[unquoted_mapping[["shape"]]]],
        labels = labels[[unquoted_mapping[["shape"]]]],
        values = get_scale_values(unquoted_mapping[["shape"]], scale = "shape")) }
  if (!is.null(unquoted_mapping[["linetype"]])) {
    p <- 
      p + 
      scale_linetype_manual(
        name = get_scale_name(unquoted_mapping[["linetype"]]),
        breaks = breaks[[unquoted_mapping[["linetype"]]]],
        labels = labels[[unquoted_mapping[["linetype"]]]],
        values = get_scale_values(unquoted_mapping[["linetype"]], scale = "linetype")) }
  
  p <-  
    p +
    guides(linetype = guide_legend(override.aes = list(size = .6))) +
    theme(
      legend.position = "top", 
      legend.key.width = unit(1.25, "lines"), 
      aspect.ratio = .5) +
    { if (unquoted_mapping[["x"]] %in% c("cCondition.Test.Audio", "Condition.Test.Audio")) 
      theme(axis.text.x = element_text(angle = 33, hjust = 1)) }
  
  return(p)
}


plot_test_predictions_by_continuum <- function(
  model,
  data = model$data,
  predict_at = list("Block" = 1, "cCondition.Test.Audio" = seq_range(data$cCondition.Test.Audio, n = resolution_along_x)),
  mapping = aes(
    x = cCondition.Test.Audio, 
    color = Condition.Test.Pen,
    linetype = Condition.Test.OriginalLabel,
    shape = Condition.Test.OriginalLabel),
  resolution_along_x = 51,
  ...
) {
  plot_test_predictions(model, data, predict_at = predict_at, mapping = mapping, resolution_along_x = resolution_along_x, ...)
}

plot_test_predictions_by_block <- function(
  model,
  data = model$data,
  predict_at = list("Block" = seq_range(data$Block, n = resolution_along_x)),
  mapping = aes(
    x = cCondition.Test.Audio, 
    color = Condition.Test.Pen,
    linetype = Condition.Test.OriginalLabel,
    shape = Condition.Test.OriginalLabel),
  resolution_along_x = 51,
  ...
) {
  plot_test_predictions(model, data, predict_at = predict_at, mapping = mapping, resolution_along_x = resolution_along_x, ...)
}

