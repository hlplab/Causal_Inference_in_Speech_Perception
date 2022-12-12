levels.Phase <- c("practice", "exposure", "test")
levels.Sex <- c("Female", "Male")
levels.Ethnicity <- c("Hispanic", "Non-Hispanic")
levels.Race <- c("American Indian", "Asian", "Black", "other", "White", "multiple")

# While R can handle unicode, stan cannot. Thus using IPA as values does not work for models
levels.response.NORM_A <- c("dill", "till", "dim", "tim", "din", "tin", "dip", "tip")
levels.correct.response.NORM_A <- NA
levels.response.voicing <- c("voiced", "voiceless")
levels.cue.names = c("Prevoicing", "VOT", "f0")

## Data preparation ------------------------------------------------------- 
simplifyAnswer <- function(x) {
  x = gsub("[|]", ";", x)
  x = ifelse(str_ends(x, ";"), x, paste0(x, ";"))
  ifelse(str_count(x, ";") > 1, "multiple", str_remove(x, ";"))
}


formatData <- function(.data, experiment) {
  require(assertthat)
  require(lubridate)
  
  if (experiment %in% "NORM-A") {
    n.practice_stims <- NA
    n.exposure_stims <- NA
    n.test_stims <- 96 * 2 + 1
    levels.response <- levels.response.NORM_A
    levels.correct.response <- levels.correct.response.NORM_A
  }
 
  .data %<>%
    # Remove any variables that are all NA
    mutate_at(
      vars(
        # variables that are constant and INFORMATIVE across all rows
        hittypeid, hitgroupid, title, description, keywords, assignments, assignmentduration, autoapprovaldelay, reward,
        starts_with("Qualification"),
        # variables that are constant and UNinformative across all row
        assignmentstatus, hitstatus, reviewstatus, numavailable, numpending, numcomplete, annotation,
        # variables that are NAs across all rows
        assignmentapprovaltime, assignmentrejecttime, deadline, feedback, reject),  
      function(x) x <- NULL) %>%
    # Renaming
    rename(
      Experiment.Protocol = Answer.rsrb.protocol,
      List = Answer.list_test,
      AssignmentID = assignmentid,
      Assignment.Accept.DateTime.UTC = assignmentaccepttime,
      Assignment.Submit.DateTime.UTC = assignmentsubmittime,
      # Was mean to store user time in GMT format. Not helpful since it's not actually the local time of the user.
      # Assignment.Submit.DateTime.UserLocalTime = Answer.userDateTime, 
      Assignment.Submit.DateTime.UserLocalTime.OffsetFromUTC = Answer.userDateTimeOffset) %>%
    # Add user local time (for now ignoring day light savings etc.)
    mutate(
      Assignment.Submit.DateTime.UserLocalTime = Assignment.Submit.DateTime.UTC - minutes(Assignment.Submit.DateTime.UserLocalTime.OffsetFromUTC),
      Assignment.Submit.DuringDayTime = ifelse(between(hour(Assignment.Submit.DateTime.UserLocalTime), 7, 21), T, F)) %>%
    { if ("Answer.us.timezone" %in% names(.)) 
      rename(., Assignment.Submit.US_TimeZone = Answer.us.timezone) else 
        mutate(., Assignment.Submit.US_TimeZone = NA) } %>%
    # Separate the practice, exposure, and test columns into one column per trial
    # (each with one more parts then there are trials because the last element is also followed by a ";". 
    # This last empty element should then be removed. If you get a warning, however, that means that at 
    # least one participant has more trials than expected)
    { if (all(c("Answer.practResp") %in% names(.)))     
      separate(., 
               Answer.practResp,
               into = paste0("Practice_Trial", 1:n_practice_stims),
               sep = ";") else . } %>%
    { if ("Answer.exposureResp" %in% names(.)) 
      separate(.,
               Answer.exposureResp,
               into = paste0("Exposure_Trial", 1:n.exposure_stims),
               sep = ";") else . } %>%
    separate(
      Answer.testResp,
      into = paste0("Test_Trial", 1:n.test_stims),
      sep = "\\;") %>%
    pivot_longer(
      cols = contains("_Trial"), 
      names_to = c("Phase", "Trial"),
      names_pattern = "(.*)_Trial(.*)"
    ) %>%
    # Remove empty final trial from each phase
    filter(value != "") %>%
    # Separate trial-level information into multiple columns
    separate(
      value,
      into = c("CHECK.Phase", "Trial.ProvideFeedback",
               "CHECK.Trial", "ItemID", 
               "Item.Filename", "Item.CorrectResponse",
               "Item.ImageOrder", "CHECK.Trial.ProvideFeedback",
               "Item.Repetitions", "Response",
               "Response.ClickPosition", "Response.ClickPosition.X", "Response.ClickPosition.Y",
               "Time.StartOfStimulus", "Time.EndOfTrial", "Response.RT"),
      sep = ",") %>%
    # Add Experiment information
    mutate(Experiment = experiment) %>%
    { if ("Answer.cond_exp" %in% names(.)) 
      rename(., Condition.Exposure = Answer.cond_exp) else 
        mutate(., Condition.Exposure = NA) } %>%
    rename_at(
      vars(starts_with("Answer.rsrb")), 
      ~ gsub("Answer\\.rsrb\\.([a-z])", "Participant\\.\\U\\1", .x, perl = T)) %>%
    # Make Trial numerical
    mutate(Trial = as.numeric(Trial)) %>%
    mutate(
      # Anonymize workers
      ParticipantID = as.numeric(factor(paste(Assignment.Submit.DateTime.UTC, workerid, AssignmentID))),
      # Extract item information
      Response = factor(Response, levels = levels.response),
      Item.CorrectResponse = factor(Item.CorrectResponse, levels = levels.correct.response),
      Response.Voicing = factor(
        case_when(
          Response %in% c("dill", "din", "dim", "dip") ~ "voiced",
          Response %in% c("till", "tin", "tim", "tip") ~ "voiceless",
          T ~ NA_character_),
        levels = levels.response.voicing),
      REMOVE.Item = gsub("^(.*)\\.(wav)$", "\\1", Item.Filename),
    ) %>%
    separate(REMOVE.Item, into = c("REMOVE.Word", "Item.VOT", "Item.Prevoicing", "Item.F0"), sep = "_") %>%
    mutate_at(vars(Item.VOT, Item.Prevoicing, Item.F0), ~ as.numeric(gsub("[A-Za-z]+", "", .x))) %>%
    # For repeated items, is this the first, second, ... instance of that item?
    group_by(Experiment, ParticipantID, ItemID) %>%
    mutate(Item.InstanceInList = as.numeric(as.factor(Trial))) %>%
    ungroup() %>%
    # Variable typing
    mutate_at(vars(CHECK.Trial, ItemID, Item.Repetitions, Response.ClickPosition.X, Response.ClickPosition.Y, Time.StartOfStimulus, Time.EndOfTrial, Response.RT),
              as.numeric) %>%
    mutate_at(vars(ParticipantID, workerid, Phase, Item.Filename, Item.CorrectResponse, Item.ImageOrder,
                   Response, Response.ClickPosition, Trial.ProvideFeedback, CHECK.Trial.ProvideFeedback,
                   hitid, AssignmentID),
              factor) %>%
    mutate(
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
    # Get durational measures (in minutes)
    group_by(ParticipantID) %>%
    mutate(
      Duration.Assignment = difftime(Assignment.Submit.DateTime.UTC, Assignment.Accept.DateTime.UTC, units = "mins"),
      Duration.AllPhases = (max(Time.EndOfTrial) - min(Time.StartOfStimulus)) / 60000,
    ) %>%
    ungroup() %>%
    # Remove unnecessary columns and order remaining columns
    select(
      -starts_with("CHECK"),
      -starts_with("REMOVE")) %>%
    arrange(Experiment, ParticipantID, Phase, Trial) 
  
  return(.data)
}


sortVars <- function(.data) {
  .data %>% 
    relocate(
      Experiment,
      starts_with("Experiment."),
      List,
      ParticipantID,
      starts_with("Participant."), 
      starts_with("Condition"), 
      Phase, Trial,
      ItemID,
      starts_with("Item."),
      Response,
      starts_with("Response"),
      starts_with("Duration"),
      starts_with("Time"),
      starts_with("Assignment"),
      starts_with("Answer"),
      everything())
}


addExclusionSummary <- function(
  d, 
  plot = T, 
  return_data = T
) {
  d %<>%
    mutate(
      Exclude_Participant.Reason = factor(case_when(
        Exclude_Participant.because_of_MultipleExperiments ~ "Repeat participant",
        Exclude_Participant.because_of_IgnoredInstructions ~ "No headphones",
        Exclude_Participant.because_of_RT ~ "Reaction time",
        Exclude_Participant.because_of_missing_trials ~ "Too many missing trials",
        T ~ "none"
      )))
  
  p <- d %>%
    group_by(Experiment, ParticipantID, Exclude_Participant.Reason, Assignment.Submit.DuringDayTime) %>%
    summarise_at("Response.log_RT", .funs = list("mean" = mean, "sd" = sd), na.rm = T) %>%
    ggplot(aes(x = mean, y = sd)) +
    geom_point(aes(color = Exclude_Participant.Reason, shape = Exclude_Participant.Reason, alpha = Assignment.Submit.DuringDayTime)) +
    geom_rug() +
    scale_x_continuous("mean log-RT (in msec)") +
    scale_y_continuous("SD of log-RT") +
    scale_color_manual(
      "Reason for exclusion",
      breaks = c("none", 
                 "Repeat participant", "No headphones", "Reaction time", "Too many missing trials"),
      values = c("black", rep("red", 8))) +
    scale_shape_manual(
      "Reason for exclusion",
      breaks = c("none", 
                 "Repeat participant", "No headphones", "Reaction time", "Too many missing trials"),
      values = c(16, 15, 17, 10, 3, 4, 8, 9, 13)) +
    scale_alpha_manual(
      "Completed during\ndaytime (EST)?",
      breaks = c(T, F),
      values = c(1, .3)) +
    facet_grid(. ~ Experiment)
  
  if (plot) { plot(p) }
  if (return_data) return(d) else return(p)
}


excludeData <- function(data) {
  data %<>%
    filter(
      Exclude_Participant.Reason == "none",
      !Exclude_Trial.because_of_RT)
  
  return(data)
}



## Data summaries ------------------------------------------------------- 

getParticipantsPerList <- function(d) {
  d %>%
    excludeData() %>%
    select(ParticipantID, List, hitid, starts_with("Condition.Exposure")) %>%
    distinct() %>%
    group_by(List) %>% 
    tally() %>%
    arrange(vars(everything()))
}


getMissingParticipantsPerList <- function(d, targetedParticipantsPerList) {
  d %>%
    excludeData() %>%
    select(ParticipantID, List, hitid, starts_with("Condition.Exposure")) %>%
    distinct() %>%
    group_by(List) %>% 
    tally() %>%
    mutate(n = targetedParticipantsPerList - n) %>%
    filter(n > 0) %>%
    arrange(vars(everything()))
}


plotDemographicInformation <- function(
  d, 
  rows = NULL,
  cols = NULL
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
  plot(p.answer + geom_bar() + aes(x = Participant.Ethnicity) + 
         xlab("reported ethnicity") +
         theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  plot(p.answer + geom_bar() + aes(x = Participant.Race) +
         xlab("reported race")) 
}

