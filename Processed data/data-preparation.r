## ---------------------------
## Script name: data_preparation.r
##
## Purpose of script:
##  Creates a long data.frame with error rates, rts and ratings.
##  For each combination of five cutoff-criteria for slow outliers
##  (none, M + 2SD, 2500 ms, 2000 ms, 1500 ms), and three transformation
##  criteria (none, log, inverse) a column with reaction times is created
##  with the outliers being replaced by NAs.
##
## Author: Carsten Sander
##
## Date Created: 2022-07-20
##
## Copyright (c) Carsten Sander, 2022
## Email: carsten.sander@uni-hamburg.de
## ---------------------------
## Notes:
##  Before running this script, make sure your working directory
##  is set to a folder containing the /Processed data/ folder from
##  the github repository
##
##  R Version -- 4.1.2
## ---------------------------

# Load packages
library(tidyverse)
library(readxl, include.only = "read_excel")
library(lubridate, include.only = "ymd_hms")

# Download raw data from Gitlab
paths <- read.csv("Processed data/raw-data-paths.csv", header = FALSE)[, 1]
pav <- lapply(paths, read_csv)
qua <- read.csv("https://gitlab.pavlovia.org/csander/sii3-falserecog-2f/raw/master/additional%20data/qualtrics.csv")[-c(1, 2), ] # nolint
download.file("https://gitlab.pavlovia.org/csander/sii3-falserecog-2f/raw/master/stimuli/stimuli.xlsx", destfile = "./stimuli.xlsx", cacheOK = TRUE) # nolint
stims <- readxl::read_excel("./stimuli.xlsx")

# ---------------------------

# Process pavlovia data
trials <- data.frame()
ratings <- data.frame()
subjects <- data.frame()

# Loop over all individual data.frames
for (i in seq(length(pav))) {
  # Select relevant data for analysis
  names(pav[[i]])[names(pav[[i]]) == "STIMULUS_SET...10"] <- "STIMULUS_SET"
  trials_i <- pav[[i]] %>%
    filter(
      trial_type == "test",
      probe_type %in% c("implied", "implied_other")
    ) %>%
    select(
      subject_id = ID, item_id, reason_type, label = probe, probe_type,
      is_correct = c_probe_resp.corr, rt = c_probe_resp.rt,
      stimulus_set = STIMULUS_SET
    ) %>%
    mutate(
      probe_type = recode(probe_type,
        "implied" = "im", "implied_other" = "io"),
      rt = rt * 1000)
  # Calculate individual cutoff (M + 2 * SD) based on correct responses
  rts <- trials_i$rt[trials_i$is_correct == 1]
  m2sd <- mean(rts) + 2 * sd(rts)
  # Define matrix of cutoff/transformation combinations
  corrections <- data.frame(
    cutoff = rep(c("none", "M2SD", "f80", "f60", "f40"), 3),
    cutoff_value = rep(c(Inf, m2sd, 8000, 6000, 4000), 3),
    trans = c(rep("none", 5), rep("log", 5), rep("inv", 5)),
    trans_function = c(rep("{}", 5), rep("log({})", 5), rep("(1 / {}) * -1", 5))) # nolint
  # For each combination of cutoff criteria and transformations
  for (c in seq(NROW(corrections))) {
    # Get values for current correction
    var_name <- paste0("rt_", corrections$cutoff[c], "_", corrections$trans[c]) # nolint
    cutoff_value <- corrections$cutoff_value[c]
    trans_function <- corrections$trans_function[c]
    trans_function <- gsub("\\{\\}", "rts", trans_function)
    # Apply corrections to rts
    rts <- trials_i$rt
    rts[rts > cutoff_value] <- NA
    rts <- eval(parse(text = trans_function))
    # Append data to data.frame
    trials_i[, var_name] <- rts
  }
  # Build data.frame from individual datasets
  trials <- dplyr::bind_rows(trials, trials_i)

  # Get summary data on subjects
  # suspicious responding: responded with always the same key
  subjects[i, "ID"] <- trials_i$subject_id[1]
  sus <- pav[[i]] %>%
    filter(trial_type %in% c("test", "filler")) %>%
    select(response_key = c_probe_resp.keys)
  subjects[i, "suspicious_responding"] <- ifelse(
    test = all(sus == "f") | all(sus == "j"),
    yes = TRUE, no = FALSE)

  # Get sufficiency ratings
  ratings_i <- pav[[i]] %>%
    filter(rating_block == "sufficiency") %>%
    select(
      subject_id = ID,
      sufficiency = rating_response,
      item_id = rated_item_id,
      reason_type = rated_reason_type
    )
  ratings <- dplyr::bind_rows(ratings, ratings_i)
}

# ---------------------------

subjects <- subjects %>%
  # Merge summary data with qualtrics data
  left_join(qua, by = "ID") %>%
  filter(part == 2) %>%
  # Recode survey items
  mutate(
    consent_given = factor(ifelse(
      grepl("Ja", informed_consent.) |
      grepl("Nein", informed_consent_dc.),
      1, 0)),
    age = as.numeric(str_extract(age, "\\d*")),
    gender = factor(recode(gender,
      "anderes (z.B. nicht-binär)" = "other",
      "männlich" = "male", "weiblich" = "female",
      "keine Angabe" = "not specified")),
    pol_orientation = as.numeric(recode(pol_orientation,
      "links" = "1", "rechts" = "10")),
    pol_interest = as.numeric(recode(pol_interest.,
      "sehr stark" = "10", "überhaupt nicht" = "1"))
  # Exclude participants
  ) %T>% {
    assign(x = "last_n", value = nrow(.), envir = .GlobalEnv)
    cat("Exclusion according to the pre-registered exclusion criteria\n\n")
    cat("Participants before exclusion:", nrow(.), "\n")

  } %>%
  filter(ymd_hms(StartDate) > ymd_hms("2022-07-21 13:09:38")) %T>% {
    cat("x. exclusion due to programming error (wrong presentation\n",
      "  time and unbalanced cell distribution):", last_n - nrow(.), "\n")
    cat("a. participants who did not complete the task: 0\n",
      "  (necessarily not met due to the manner of data collection)\n")
    assign(x = "last_n", value = nrow(.), envir = .GlobalEnv)
  } %>%
  filter(consent_given == 1) %T>% {
    cat("b. participants who withdrew their consent to\n",
      "  data analysis after full debriefing:", last_n - nrow(.), "\n")
    assign(x = "last_n", value = nrow(.), envir = .GlobalEnv)
  } %>%
  filter(suspicious_responding == FALSE) %T>% {
    cat("c. participants who give the same response in\n",
      "  all of the 36 test trials:", last_n - nrow(.), "\n")
    assign(x = "last_n", value = nrow(.), envir = .GlobalEnv)
  } %>%
  filter(data_quality. == "ja") %T>% {
    cat("d. participants who rate their own data to be\n",
      "  unfit for analyses:", last_n - nrow(.), "\n")
    cat("Participants after exclusion:", nrow(.), "\n")
  } %>%
  select(subject_id = ID, age, gender, pol_interest, pol_orientation, assumptions)

# ---------------------------

# Select behavior, reason and ratings from stimulus file
stims <- stims %>%
  filter(trial_type == "test") %>%
  select(
    item_id, behavior,
    reason_sufficient = reason,
    reason_control = ctrl_reason,
    score_sufficient = reason_score,
    score_control = control_score,
    sconsensus, cconsensus, label_score, reason_diff
  ) %>%
  pivot_longer(
    cols = c(reason_sufficient, reason_control, score_sufficient, score_control), # nolint
    names_to = c(".value", "reason_type"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  rename(reason_score = score)

# ---------------------------

# Merge all data.frames
d_long <- trials %>%
  left_join(ratings) %>%
  filter(subject_id %in% subjects$subject_id) %>%
  left_join(subjects) %>%
  left_join(stims) %>%
  select(-rt) %>%
  mutate(probe_type = recode(probe_type, "im" = "implied", "io" = "implied other")) %>% # nolint
  mutate_at(c("subject_id", "item_id", "reason_type",
        "probe_type", "stimulus_set"), factor)

# ---------------------------

# Export data
saveRDS(d_long, file = "Processed data/d-long-new.rds")
