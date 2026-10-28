#---- 01_analysis.R #----
# Assessing the relationship between social contact and mobility patterns
# among adults in four US states
#
# Data preparation, exclusions, survey weights, travel variables, travel
# clusters, cluster comparisons, and negative binomial regression models.
#
# Run from the repository root:
#   Rscript scripts/01_analysis.R
#
# Inputs
#   data/analysis_data/wave1_2_analysis.csv  survey variables used in the analysis
#   data/2023 ACS 1 year/ACS_2023_*.csv      2023 ACS 1-year margins used for raking
# Outputs
#   data/derived/analysis_data.rds         one row per respondent in the analytic sample
#   data/derived/model_fits.rds            fitted survey-weighted negative binomial models
#   outputs/analysis/*.csv                 sample flow, weights, clusters, tests, model results,
#                                          holiday models, holiday travel windows

#---- Set up #----
library(tidyverse)
library(survey) # for weights
library(sjstats) # for svyglm.nb()

raw_file <- file.path("data", "analysis_data", "wave1_2_analysis.csv")
acs_dir  <- file.path("data", "2023 ACS 1 year")
derived_dir <- file.path("data", "derived")
out_dir  <- file.path("outputs", "analysis")

# Full-sample sensitivity analysis (Figure S7, Table S5): 04_supplemental.R
# reruns this script with apply_exclusions <- FALSE, which skips the two sample
# exclusions and saves to data/derived/full_sample and outputs/analysis/full_sample
if (!exists("apply_exclusions")) apply_exclusions <- TRUE
if (!apply_exclusions) {
  derived_dir <- file.path(derived_dir, "full_sample")
  out_dir <- file.path(out_dir, "full_sample")
}
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

wave1_2 <- read.csv(raw_file)
stopifnot(!anyNA(wave1_2$survey_id), !anyDuplicated(wave1_2$survey_id))

# To track exclusions throughout 
sample_flow <- tibble(step = "Surveys collected", n = nrow(wave1_2))


#---- Prep demographics #----
# AGE
wave1_2 <- wave1_2 %>%
  mutate(age_group = factor(age_group, levels = c("18-34", "35-54", "55-64", "65+")))

# HH SIZE
wave1_2 <- wave1_2 %>%
  mutate(
    hh_size_cat = factor(
      case_when(
        numb_hh_memb == 1 ~ "One",
        numb_hh_memb == 2 ~ "Two",
        numb_hh_memb == 3 ~ "Three",
        numb_hh_memb == 4 ~ "Four",
        numb_hh_memb >= 5 ~ "Five Plus",
        TRUE ~ NA_character_
      ),
      levels = c("One", "Two", "Three", "Four", "Five Plus")
    )
  )

# RACE/ETH
# one race/ethnicity variable; anyone selecting more than one race is multiple race
wave1_2 <- wave1_2 %>%
  mutate(
    race_eth_clean = factor(
      case_when(
        rowSums(select(., race_asian, race_white, race_black, race_other, race_native), na.rm = TRUE) > 1 ~ "Multiple Race",
        race_hispanic == 1 ~ "Hispanic",
        race_asian == 1 ~ "Non-Hispanic Asian/PI",
        race_white == 1 ~ "Non-Hispanic White",
        race_black == 1 ~ "Non-Hispanic Black",
        race_native == 1 ~ "Non-Hispanic AI/AN",
        race_other == 1 ~ "Non-Hispanic Other Race",
        race_nr == 1 ~ "Prefer not to say",
        TRUE ~ NA_character_
      ),
      levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic",
                 "Non-Hispanic Asian/PI", "Non-Hispanic AI/AN",
                 "Non-Hispanic Other Race", "Multiple Race", "Prefer not to say")
    )
  )

# GENDER
wave1_2 <- wave1_2 %>%
  mutate(
    gender = factor(gender,
                    levels = c("Woman", "Man", "Transgender/Trans man",
                               "Transgender/Trans woman", "Non-Binary",
                               "Other", "Prefer not to say")),
    # merge all groups other than woman / man into other
    gender_binned = factor(
      case_when(
        gender == "Man" ~ "Man",
        gender == "Woman" ~ "Woman",
        gender == "Prefer not to say" ~ "Prefer not to say",
        TRUE ~ "Other"),
      levels = c("Woman", "Man", "Other", "Prefer not to say")
    )
  )

# EDUCATION LEVEL (binned to match ACS categories)
educ_levels <- c("Less than high school graduate",
                 "High school graduate (includes equivalency)",
                 "Some college or associate's degree",
                 "Bachelor's degree",
                 "Graduate or professional degree")
wave1_2 <- wave1_2 %>%
  mutate(
    educ_level_binned = case_when(
      educ_level == "Less than high school degree" ~ "Less than high school graduate",
      educ_level == "High school degree or equivalent (e.g., GED)" ~ "High school graduate (includes equivalency)",
      educ_level %in% c("Some college but no degree", "Associate degree") ~ "Some college or associate's degree",
      educ_level == "Bachelor degree" ~ "Bachelor's degree",
      educ_level == "Graduate degree" ~ "Graduate or professional degree",
      educ_level == "Prefer not to say" ~ "Prefer not to say",
      TRUE ~ NA_character_
    ),
    educ_level_binned = factor(educ_level_binned, levels = c(educ_levels, "Prefer not to say"))
  )

# HH INCOME (binned similarly to ACS)
income_levels <- c("Less than $10,000", "$10,000 to $49,999", "$50,000 to $99,999",
                   "$100,000 to $149,999", "$150,000 to $199,999", "$200,000 or more")
wave1_2 <- wave1_2 %>%
  mutate(
    hh_income_binned = factor(
      case_when(
        hh_income == "Less than $10,000" ~ "Less than $10,000",
        hh_income %in% c("$10,000 to $19,999", "$20,000 to $29,999",
                         "$30,000 to $39,999", "$40,000 to $49,999") ~ "$10,000 to $49,999",
        hh_income %in% c("$50,000 to $69,999", "$70,000 to $84,999",
                         "$85,000 to $99,999") ~ "$50,000 to $99,999",
        hh_income == "$100,000 to 149,999" ~ "$100,000 to $149,999",
        hh_income == "$150,000 to $199,999" ~ "$150,000 to $199,999",
        hh_income == "$200,000 or more" ~ "$200,000 or more",
        hh_income == "Prefer not to say" ~ "Prefer not to say",
        TRUE ~ NA_character_
      ),
      levels = c(income_levels, "Prefer not to say")
    )
  )

# WORK OUTSIDE HOME
# Respondents were asked if they worked outside the household; retired or
# unemployed respondents should have skipped the occupation question, but some
# answered anyway. Anyone who said retired (or unemployed) in either question is
# set to retired (or unemployed) in both.
wave1_2 <- wave1_2 %>%
  mutate(
    work_outside_hh_unchanged = work_outside_hh,
    occ_unchanged = occ,
    work_outside_hh = case_when(
      occ == "Retired" | work_outside_hh == "Retired" ~ "Retired",
      TRUE ~ work_outside_hh),
    occ = case_when(
      occ == "Retired" | work_outside_hh == "Retired" ~ "Retired",
      TRUE ~ occ)
  ) %>%
  mutate(
    work_outside_hh = case_when(
      occ != "Retired" & (occ == "Unemployed" | work_outside_hh == "Unemployed") ~ "Unemployed",
      TRUE ~ work_outside_hh),
    occ = case_when(
      occ != "Retired" & (occ == "Unemployed" | work_outside_hh == "Unemployed") ~ "Unemployed",
      TRUE ~ occ)
  )

# OCCUPATION
wave1_2 <- wave1_2 %>%
  mutate(
    occ = factor(occ,
                 levels = c("Government employee",
                            "Private sector employee: for-profit organization",
                            "Private sector employee: non-profit organization",
                            "Self-employed", "Retired", "Unemployed",
                            "Not in labor force", "Prefer not to say")),
    # merge retired, unemployed and not in labor force for work outside hh
    work_outside_hh = case_when(
      work_outside_hh %in% c("Retired", "Unemployed") ~ "Retired, unemployed, or not in labor force",
      occ == "Not in labor force" ~ "Retired, unemployed, or not in labor force",
      TRUE ~ work_outside_hh),
    work_outside_hh = factor(work_outside_hh,
                             levels = c("Yes", "No",
                                        "Retired, unemployed, or not in labor force",
                                        "Prefer not to say"))
  )

# STATE
wave1_2 <- wave1_2 %>%
  mutate(state_residence = factor(state_residence, levels = c("FL", "GA", "MD", "NC")))

# NEIGHBORHOOD AND POLITICAL PARTY (only described in Figure S3, not in models)
wave1_2 <- wave1_2 %>%
  mutate(
    neighborhood = factor(neighborhood,
                          levels = c("Urban", "Suburban", "Rural", "Other", "Prefer not to say")),
    political_party = factor(political_party,
                             levels = c("Democrat", "Republican", "Independent", "Something else",
                                        "Prefer not to say"))
  )


#---- Contact totals #----
# Contacts were reported by type (physical or non-physical), contact age group
# (9 groups) and setting (home, work, school, other): 72 count columns named
# {any_physical|non_physical}_{age bin}_{setting}. Totals are computed once,
# directly from these 72 columns.
contact_age_bins <- c("0_1", "1_4", "5_9", "10_17", "18_29", "30_39", "40_49", "50_59", "60_plus")
settings <- c("home", "work", "school", "other")

for (s in settings) {
  wave1_2[[paste0("physical_", s)]] <-
    rowSums(wave1_2[paste("any_physical", contact_age_bins, s, sep = "_")], na.rm = TRUE)
  wave1_2[[paste0("non_physical_", s)]] <-
    rowSums(wave1_2[paste("non_physical", contact_age_bins, s, sep = "_")], na.rm = TRUE)
  wave1_2[[paste0("overall_", s)]] <-
    wave1_2[[paste0("physical_", s)]] + wave1_2[[paste0("non_physical_", s)]]
}

wave1_2 <- wave1_2 %>%
  mutate(
    physical_total     = physical_home + physical_work + physical_school + physical_other,
    non_physical_total = non_physical_home + non_physical_work + non_physical_school + non_physical_other,
    overall_total      = physical_total + non_physical_total
  )


#---- Sample exclusions #----
if (apply_exclusions) {
  # Remove people who say they are retired, unemployed or not in the labor force
  # but report work contacts
  wave1_2 <- wave1_2 %>%
    filter(!(work_outside_hh == "Retired, unemployed, or not in labor force" &
               overall_work != 0))
  sample_flow <- add_row(sample_flow, step = "After removing work contacts reported by people not working",
                         n = nrow(wave1_2))

  # Remove overall contacts above the 95th percentile (46 contacts)
  quantile(wave1_2$overall_total, 0.95)
  wave1_2 <- wave1_2 %>%
    filter(overall_total <= 46)
  sample_flow <- add_row(sample_flow, step = "After removing more than 46 overall contacts (analytic sample)",
                         n = nrow(wave1_2))
}
sample_flow


#---- Survey weights #----
# Raking (iterative proportional fitting) to 2023 ACS 1-year margins of
# education, age, sex, and race/ethnicity, separately for each time period
# (wave) and state. Base weights are 1. Respondents missing any raking variable
# are given the mean weight of similar respondents (see below).

# Weighting variables: "Prefer not to say" and non-binary genders are NA here,
# so those respondents are imputed rather than raked
weighting_vars <- wave1_2 %>%
  transmute(
    survey_id,
    wave = as.character(wave),
    state = tolower(as.character(state_residence)),
    age_group_weighting = factor(age_group, levels = c("18-34", "35-54", "55-64", "65+")),
    educ_level_weighting = factor(if_else(educ_level_binned == "Prefer not to say",
                                          NA_character_, as.character(educ_level_binned)),
                                  levels = educ_levels),
    sex_weighting = factor(case_when(gender == "Woman" ~ "female",
                                     gender == "Man" ~ "male",
                                     TRUE ~ NA_character_),
                           levels = c("male", "female")),
    total_selected = (race_white == 1) + (race_black == 1) + (race_hispanic == 1) +
      (race_native == 1) + (race_asian == 1) + (race_other == 1) + (race_nr == 1),
    race_eth_weighting = case_when(
      race_hispanic == 1 ~ "Hispanic",
      race_hispanic == 0 & total_selected > 1 ~ "Non-Hispanic Multiple Race",
      race_hispanic == 0 & race_asian == 1 ~ "Non-Hispanic Asian/PI",
      race_hispanic == 0 & race_white == 1 ~ "Non-Hispanic White",
      race_hispanic == 0 & race_black == 1 ~ "Non-Hispanic Black",
      race_hispanic == 0 & race_native == 1 ~ "Non-Hispanic AI/AN",
      race_hispanic == 0 & race_other == 1 ~ "Non-Hispanic Other Race",
      TRUE ~ NA_character_)
  )
race_levels_weighting <- c("Hispanic", "Non-Hispanic Multiple Race", "Non-Hispanic Asian/PI",
                           "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic AI/AN",
                           "Non-Hispanic Other Race")
weighting_vars$race_eth_weighting <- factor(weighting_vars$race_eth_weighting,
                                            levels = race_levels_weighting)

##---- ACS 2023 margins ----
state_codes <- c(florida = "fl", georgia = "ga", maryland = "md", "north carolina" = "nc",
                 fl = "fl", ga = "ga", md = "md", nc = "nc")

read_acs <- function(file, category_col) {
  read.csv(file.path(acs_dir, file)) %>%
    rename(category = all_of(category_col)) %>%
    filter(!str_detect(tolower(category), "^total")) %>%  # drop total population rows
    transmute(state = state_codes[tolower(State)], category,
              Freq = as.numeric(gsub(",", "", estimate)))
}

# Adults 18+ in four groups (18-34 includes ages 18-19)
acs_age <- read_acs("ACS_2023_age.csv", "age_group") %>%
  mutate(age_group_weighting = recode(category,
                                      "18 to 34 years" = "18-34",
                                      "35 to 54 years" = "35-54",
                                      "55 to 64 years" = "55-64",
                                      "65 years and over" = "65+"))
acs_educ <- read_acs("ACS_2023_education.csv", "educ_level") %>%
  mutate(educ_level_weighting = category)
acs_sex <- read_acs("ACS_2023_sex.csv", "group") %>%
  mutate(sex_weighting = category)
acs_race <- read_acs("ACS_2023_race.csv", "group") %>%
  mutate(race_eth_weighting = recode(category,
                                     "Hispanic or Latino (of any race)" = "Hispanic",
                                     "NH White or Caucasian" = "Non-Hispanic White",
                                     "NH Black or African American" = "Non-Hispanic Black",
                                     "NH American Indian or Alaska Native" = "Non-Hispanic AI/AN",
                                     "NH Asian or Pacific Islander" = "Non-Hispanic Asian/PI",
                                     "NH Other" = "Non-Hispanic Other Race",
                                     "NH Multiple Races" = "Non-Hispanic Multiple Race"))

# every category in the survey must have exactly one ACS row per state
stopifnot(
  all(acs_age$age_group_weighting %in% levels(weighting_vars$age_group_weighting)),
  all(acs_educ$educ_level_weighting %in% educ_levels),
  all(acs_sex$sex_weighting %in% c("male", "female")),
  all(acs_race$race_eth_weighting %in% race_levels_weighting),
  !anyDuplicated(acs_age[c("state", "age_group_weighting")]),
  !anyDuplicated(acs_educ[c("state", "educ_level_weighting")]),
  !anyDuplicated(acs_sex[c("state", "sex_weighting")]),
  !anyDuplicated(acs_race[c("state", "race_eth_weighting")])
)

acs_margin <- function(acs, var, st) {
  acs %>%
    filter(state == st) %>%
    select(all_of(var), Freq) %>%
    mutate(!!var := factor(.data[[var]], levels = levels(weighting_vars[[var]])))
}

##---- Rake, trim, and impute by wave ----
compute_ess <- function(weights) sum(weights)^2 / sum(weights^2)

states <- c("fl", "ga", "md", "nc")
weights_by_wave <- list()
weight_diagnostics <- list()

for (w in sort(unique(weighting_vars$wave))) {

  wave_df <- filter(weighting_vars, wave == w) %>% mutate(base_weight = 1)

  # complete cases used for raking
  wave_cc <- wave_df %>%
    filter(!is.na(age_group_weighting), !is.na(educ_level_weighting),
           !is.na(sex_weighting), !is.na(race_eth_weighting))
  wave_cc$raked_weight <- NA_real_

  full_design <- svydesign(ids = ~1, strata = ~state, data = wave_cc, weights = ~base_weight)

  for (st in states) {
    sample_design_state <- subset(full_design, state == st)

    raked_state <- rake(
      design = sample_design_state,
      sample.margins = list(~educ_level_weighting, ~age_group_weighting,
                            ~sex_weighting, ~race_eth_weighting),
      population.margins = list(acs_margin(acs_educ, "educ_level_weighting", st),
                                acs_margin(acs_age, "age_group_weighting", st),
                                acs_margin(acs_sex, "sex_weighting", st),
                                acs_margin(acs_race, "race_eth_weighting", st))
    )

    # normalize to mean 1, trim to the 5th and 95th percentiles, renormalize to mean 1
    weights_vec_norm <- weights(raked_state) / mean(weights(raked_state))
    floor_05 <- quantile(weights_vec_norm, 0.05)
    cap_95 <- quantile(weights_vec_norm, 0.95)
    weights_vec_trimmed <- pmin(pmax(weights_vec_norm, floor_05), cap_95)
    weights_vec_trimmed_norm <- weights_vec_trimmed / mean(weights_vec_trimmed)

    survey_ids <- raked_state$variables$survey_id
    wave_cc$raked_weight[match(survey_ids, wave_cc$survey_id)] <- weights_vec_trimmed_norm

    weight_diagnostics[[paste(w, st)]] <- tibble(
      wave = w, state = st, n_raked = length(weights_vec_norm),
      ess_before_trimming = compute_ess(weights_vec_norm),
      ess_after_trimming = compute_ess(weights_vec_trimmed_norm))
  }

  # Respondents missing a raking variable get the mean weight of raked
  # respondents with the same available characteristics, relaxing the match
  # step by step (all four variables in state -> fewer variables -> age and
  # sex across all states -> state mean)

  # add raked weight back to full dataset using survey_id
  wave_full <- wave_df %>%
    left_join(
      wave_cc %>% select(survey_id, raked_weight),
      by = "survey_id"
    )

  # compute group means dropping different variables
  # 5-variable
  group_mean_5 <- wave_full %>%
    filter(!is.na(raked_weight)) %>%
    group_by(state, age_group_weighting, sex_weighting, educ_level_weighting, race_eth_weighting) %>%
    summarise(mean_wt_5 = mean(raked_weight), .groups = "drop")

  # 4-variable drops
  group_mean_4a <- wave_full %>%
    filter(!is.na(raked_weight)) %>%
    group_by(state, age_group_weighting, sex_weighting, educ_level_weighting) %>%
    summarise(mean_wt_4a = mean(raked_weight), .groups = "drop")

  group_mean_4b <- wave_full %>%
    filter(!is.na(raked_weight)) %>%
    group_by(state, age_group_weighting, sex_weighting, race_eth_weighting) %>%
    summarise(mean_wt_4b = mean(raked_weight), .groups = "drop")

  group_mean_4c <- wave_full %>%
    filter(!is.na(raked_weight)) %>%
    group_by(state, age_group_weighting, educ_level_weighting, race_eth_weighting) %>%
    summarise(mean_wt_4c = mean(raked_weight), .groups = "drop")

  group_mean_4d <- wave_full %>%
    filter(!is.na(raked_weight)) %>%
    group_by(state, sex_weighting, educ_level_weighting, race_eth_weighting) %>%
    summarise(mean_wt_4d = mean(raked_weight), .groups = "drop")

  # 3-variable (age + sex + state)
  group_mean_3 <- wave_full %>%
    filter(!is.na(raked_weight)) %>%
    group_by(state, age_group_weighting, sex_weighting) %>%
    summarise(mean_wt_3 = mean(raked_weight), .groups = "drop")

  # 2-variable (age + sex only, across all states)
  group_mean_2 <- wave_full %>%
    filter(!is.na(raked_weight)) %>%
    group_by(age_group_weighting, sex_weighting) %>%
    summarise(mean_wt_2 = mean(raked_weight), .groups = "drop")

  # 1-variable (state only)
  group_mean_1 <- wave_full %>%
    filter(!is.na(raked_weight)) %>%
    group_by(state) %>%
    summarise(mean_wt_1 = mean(raked_weight), .groups = "drop")

  # join hierarchically
  wave_full <- wave_full %>%
    left_join(group_mean_5, by = c("state", "age_group_weighting", "sex_weighting", "educ_level_weighting", "race_eth_weighting")) %>%
    left_join(group_mean_4a, by = c("state", "age_group_weighting", "sex_weighting", "educ_level_weighting")) %>%
    left_join(group_mean_4b, by = c("state", "age_group_weighting", "sex_weighting", "race_eth_weighting")) %>%
    left_join(group_mean_4c, by = c("state", "age_group_weighting", "educ_level_weighting", "race_eth_weighting")) %>%
    left_join(group_mean_4d, by = c("state", "sex_weighting", "educ_level_weighting", "race_eth_weighting")) %>%
    left_join(group_mean_3, by = c("state", "age_group_weighting", "sex_weighting")) %>%
    left_join(group_mean_2, by = c("age_group_weighting", "sex_weighting")) %>%
    left_join(group_mean_1, by = "state") %>%
    mutate(
      provisional_weight = coalesce(
        raked_weight,
        mean_wt_5,
        mean_wt_4a,
        mean_wt_4b,
        mean_wt_4c,
        mean_wt_4d,
        mean_wt_3,
        mean_wt_2,
        mean_wt_1
      )
    )

  # check missing
  stopifnot(!anyNA(wave_full$provisional_weight))

  # normalize the imputed weights only to mean by state for consistency
  wave_full <- wave_full %>%
    group_by(state) %>%
    mutate(
      mean_raked_state = mean(raked_weight, na.rm = TRUE),
      mean_imputed_state = mean(provisional_weight[is.na(raked_weight)], na.rm = TRUE),
      final_weight = case_when(
        !is.na(raked_weight) ~ raked_weight,  # retain exactly
        TRUE ~ provisional_weight * mean_raked_state / mean_imputed_state
      )
    ) %>%
    ungroup() %>%
    select(-mean_raked_state, -mean_imputed_state) # clean up

  weights_by_wave[[w]] <- wave_full %>%
    transmute(survey_id, weight = final_weight, weight_imputed = is.na(raked_weight))
}

wave1_2 <- wave1_2 %>%
  left_join(bind_rows(weights_by_wave), by = "survey_id")
stopifnot(!anyNA(wave1_2$weight), all(wave1_2$weight > 0))

weight_summary <- wave1_2 %>%
  group_by(wave, state_residence) %>%
  summarise(n = n(),
            n_imputed = sum(weight_imputed),
            mean = mean(weight), sd = sd(weight), min = min(weight),
            p05 = quantile(weight, 0.05), median = median(weight),
            p95 = quantile(weight, 0.95), max = max(weight),
            .groups = "drop") %>%
  left_join(bind_rows(weight_diagnostics) %>% mutate(state = toupper(state), wave = as.integer(wave)),
            by = c("wave", "state_residence" = "state"))
weight_summary


#---- Travel variables #----
# Work/school and vacation/other travel in the past two weeks. Travel variables
# are NA for anyone who did not report travel for that purpose.
# The two panels asked days per week differently: dynata asked once, the other
# panel asked per destination, which are summed and capped at 7.
wave1_2 <- wave1_2 %>%
  mutate(
    # WORK/SCHOOL
    # travel reported ("Prefer not to say" is missing)
    work_school_travel_bin = case_when(reg_travel == "Yes" ~ 1, reg_travel == "No" ~ 0),
    # days per week traveling for work/school
    work_school_travel_sum7 = ifelse(
      reg_travel != "Yes", NA,
      ifelse(source == "dynata",
             numb_days_reg_travel,
             pmin(rowSums(across(c(numb_days_reg_travel1, numb_days_reg_travel2,
                                   numb_days_reg_travel3, numb_days_reg_travel4,
                                   numb_days_reg_travel5)), na.rm = TRUE), 7))),
    # number of locations reported (up to 5)
    work_school_trips_2wk = if_else(reg_travel == "Yes", reg_travel_n_destinations, NA),
    # mean distance from home and summed distance across trips (km)
    reg_trip_mean_km_test = case_when(reg_travel != "Yes" ~ NA, TRUE ~ reg_trip_mean_km),
    reg_trip_sum_km_test = case_when(reg_travel == "Yes" & !is.na(reg_trip_mean_km_test) ~ reg_trip_sum_km,
                                     TRUE ~ NA),

    # VACATION/OTHER
    # travel reported (blank is "Prefer not to say", missing)
    other_travel_bin = case_when(other_travel == "Yes" ~ 1, other_travel == "No" ~ 0),
    other_trips_2wk = if_else(other_travel == "Yes", other_travel_n_destinations, NA),
    other_trip_mean_km_test = case_when(other_travel != "Yes" ~ NA, TRUE ~ other_trip_mean_km),
    other_trip_sum_km_test = case_when(!is.na(other_trip_mean_km_test) ~ other_trip_sum_km, TRUE ~ NA),
    # nights spent away, summed over trips and capped at 14 (two-week window)
    other_travel_nights_sum14 = ifelse(
      other_travel != "Yes", NA,
      pmin(rowSums(across(c(travel_dur1, travel_dur2, travel_dur3, travel_dur4, travel_dur5)),
                   na.rm = TRUE), 14))
  )


#---- Travel clusters #----
# Two-stage k-means on the unweighted, z-scored travel variables. People who
# did not travel are given 0 for the clustering variables.
travel_vars <- c(
  "work_school_travel_sum7",   # days per week traveling for work/school
  "work_school_trips_2wk",     # number of work/school locations
  "reg_trip_sum_km_test",      # summed distance for work/school (km)
  "other_trips_2wk",           # number of vacation/other locations
  "other_travel_nights_sum14", # nights away (capped at 14)
  "other_trip_sum_km_test"     # summed distance for vacation/other (km)
)

# Only respondents who answered both travel questions (yes or no) are clustered;
# the others are kept for descriptive tables but have no travel cluster
wave1_2 <- wave1_2 %>%
  mutate(travel_answered = !is.na(work_school_travel_bin) & !is.na(other_travel_bin))
sample_flow <- add_row(sample_flow, step = "Answered both travel questions (clusters and models)",
                       n = sum(wave1_2$travel_answered))

# ANY TRAVEL in the past two weeks (work/school or vacation/other), among
# respondents who answered both travel questions. Counts are unweighted and
# percentages are weighted, as in Table 2 and Table S2.
any_travel_summary <- wave1_2 %>%
  filter(travel_answered) %>%
  mutate(any_travel = work_school_travel_bin == 1 | other_travel_bin == 1) %>%
  summarise(
    n_answered = n(),
    n_any_travel = sum(any_travel),
    weighted_pct_any_travel = 100 * sum(weight[any_travel]) / sum(weight),
    n_work_school_travel = sum(work_school_travel_bin == 1),
    weighted_pct_work_school_travel = 100 * sum(weight[work_school_travel_bin == 1]) / sum(weight),
    n_other_travel = sum(other_travel_bin == 1),
    weighted_pct_other_travel = 100 * sum(weight[other_travel_bin == 1]) / sum(weight)
  )
any_travel_summary

# sort by survey_id so k-means is reproducible
wave1_2 <- wave1_2 %>% arrange(survey_id)
clustered_rows <- which(wave1_2$travel_answered)
travel_scaled <- wave1_2[clustered_rows, ] %>%
  select(all_of(travel_vars)) %>%
  mutate(across(everything(), ~ replace_na(as.numeric(.x), 0))) %>%
  scale()

set.seed(123)
# Chosen based on elbow plot
k4 <- kmeans(travel_scaled, centers = 4, nstart = 10000)

# Label clusters from their centroids (see outputs/analysis/cluster_centroids_main.csv). Labels are
# matched to cluster size, which identifies the study clusters; stop if the
# sizes change so the labels can be reviewed.
main_labels <- c("14627" = "Low mobility",
                 "5601"  = "Routine local commuters",
                 "1699"  = "Long-duration non-work travelers",
                 "157"   = "High-mobility long-distance travelers")

# In the full-sample sensitivity analysis the cluster sizes differ, so each
# cluster is given the label of the study cluster with the closest mean travel
# (cluster means of the six travel variables, in standard deviation units)
if (!apply_exclusions) {
  study_data <- readRDS(file.path("data", "derived", "analysis_data.rds"))

  closest_label <- function(rows, km_cluster, study_rows, study_label) {
    x <- wave1_2[rows, ] %>%
      select(all_of(travel_vars)) %>%
      mutate(across(everything(), ~ replace_na(as.numeric(.x), 0)))
    x_study <- study_data[study_rows, ] %>%
      select(all_of(travel_vars)) %>%
      mutate(across(everything(), ~ replace_na(as.numeric(.x), 0)))
    sds <- sapply(x, sd)
    means <- apply(x, 2, function(v) tapply(v, km_cluster, mean))
    means_study <- apply(x_study, 2, function(v) tapply(v, study_label, mean))
    labs <- apply(means, 1, function(m) {
      d <- colSums((t(means_study) - m)^2 / sds^2)
      rownames(means_study)[which.min(d)]
    })
    print(labs); print(table(km_cluster))
    stopifnot(!anyDuplicated(labs))  # one cluster per label
    unname(labs[as.character(km_cluster)])
  }
}

wave1_2$travel_cluster <- NA_character_
if (apply_exclusions) {
  if (!all(as.character(k4$size) %in% names(main_labels))) {
    print(k4$size); print(k4$centers)
    stop("Cluster sizes differ from the study clusters; review the centroids and update the labels.")
  }
  wave1_2$travel_cluster[clustered_rows] <- unname(main_labels[as.character(k4$size[k4$cluster])])
} else {
  wave1_2$travel_cluster[clustered_rows] <- closest_label(
    clustered_rows, k4$cluster,
    which(study_data$travel_answered), study_data$travel_cluster[study_data$travel_answered])
}

# The low mobility cluster held two thirds of the sample, so it was split into
# three subclusters
lowmob_rows <- which(wave1_2$travel_cluster == "Low mobility")
lowmob_scaled <- wave1_2[lowmob_rows, ] %>%
  select(all_of(travel_vars)) %>%
  mutate(across(everything(), ~ replace_na(as.numeric(.x), 0))) %>%
  scale()

set.seed(123)
km_lowmob <- kmeans(lowmob_scaled, centers = 3, nstart = 1000)

sub_labels <- c("11638" = "Subcluster stay at home",
                "2021"  = "Subcluster occasional vacation travel",
                "968"   = "Subcluster low-frequency work commuter")
if (apply_exclusions && !all(as.character(km_lowmob$size) %in% names(sub_labels))) {
  print(km_lowmob$size); print(km_lowmob$centers)
  stop("Subcluster sizes differ from the study subclusters; review the centroids and update the labels.")
}

# Six clusters used in the analysis: three low mobility subclusters plus the
# three other main clusters
cluster_levels <- c("Subcluster stay at home",
                    "Subcluster occasional vacation travel",
                    "Subcluster low-frequency work commuter",
                    "Routine local commuters",
                    "Long-duration non-work travelers",
                    "High-mobility long-distance travelers")
wave1_2$travel_cluster_merged <- wave1_2$travel_cluster
if (apply_exclusions) {
  wave1_2$travel_cluster_merged[lowmob_rows] <- unname(sub_labels[as.character(km_lowmob$size[km_lowmob$cluster])])
} else {
  study_lowmob <- which(study_data$travel_cluster == "Low mobility")
  wave1_2$travel_cluster_merged[lowmob_rows] <- closest_label(
    lowmob_rows, km_lowmob$cluster,
    study_lowmob, as.character(study_data$travel_cluster_merged[study_lowmob]))
}
wave1_2$travel_cluster_merged <- factor(wave1_2$travel_cluster_merged, levels = cluster_levels)
table(wave1_2$travel_cluster_merged, useNA = "ifany")

# Unweighted cluster summaries (means and medians, non-travelers as 0)
cluster_summary <- wave1_2 %>%
  filter(travel_answered) %>%
  mutate(across(all_of(travel_vars), ~ replace_na(.x, 0))) %>%
  group_by(travel_cluster_merged) %>%
  summarise(n = n(),
            across(all_of(travel_vars), list(mean = mean, median = median)),
            .groups = "drop")
cluster_summary

# Within-cluster sums of squares for k = 1 to 10, for the elbow plots
# (Figures S13 and S14, drawn in 04_supplemental.R). Not needed for the
# full-sample sensitivity analysis.
if (apply_exclusions) {
  set.seed(123)
  elbow <- bind_rows(
    tibble(stage = "Full sample", k = 1:10,
           wss = sapply(1:10, function(k) kmeans(travel_scaled, centers = k, nstart = 10, iter.max = 50)$tot.withinss)),
    tibble(stage = "Low mobility cluster", k = 1:10,
           wss = sapply(1:10, function(k) kmeans(lowmob_scaled, centers = k, nstart = 10, iter.max = 50)$tot.withinss))
  )
}


#---- Compare cluster demographics #----
# Not needed for the full-sample sensitivity analysis
if (apply_exclusions) {
  # Each cluster versus everyone else (unweighted). Chi-squared tests, or Fisher's
  # exact test when any expected count is <1 or more than 20% are <5.
  # Bonferroni correction across all comparisons.
  vars_tested <- c("age_group", "hh_size_cat", "gender_binned", "race_eth_clean",
                   "educ_level_binned", "hh_income_binned", "occ", "work_outside_hh",
                   "state_residence")

  cluster_tests <- list()
  for (cl in cluster_levels) {
    for (v in vars_tested) {
      dat_sub <- wave1_2 %>%
        filter(travel_answered, !is.na(.data[[v]])) %>%
        mutate(group = if_else(travel_cluster_merged == cl, "cluster", "other"))
      tab <- table(dat_sub$group, dat_sub[[v]])

      if (nrow(tab) < 2 || ncol(tab) < 2) {
        cluster_tests[[length(cluster_tests) + 1]] <-
          tibble(cluster = cl, variable = v, test = "Not tested", p_value = NA_real_)
        next
      }
      chi <- suppressWarnings(chisq.test(tab))
      if (any(chi$expected < 1) || mean(chi$expected < 5) > 0.20) {
        fisher_result <- tryCatch(fisher.test(tab), error = function(e) NULL)
        p_val <- if (is.null(fisher_result)) NA_real_ else fisher_result$p.value
        test_used <- if (is.null(fisher_result)) "Not tested (Fisher failed)" else "Fisher"
      } else {
        p_val <- chi$p.value
        test_used <- "Chi-square"
      }
      cluster_tests[[length(cluster_tests) + 1]] <-
        tibble(cluster = cl, variable = v, test = test_used, p_value = p_val)
    }
  }
  cluster_tests <- bind_rows(cluster_tests) %>%
    mutate(bonferroni_p = p.adjust(p_value, method = "bonferroni", n = n()))
  cluster_tests
}


#---- Regression variables #----
wave1_2 <- wave1_2 %>%
  mutate(
    # Contacts are reported for the day before the survey, so surveys started
    # on Sunday or Monday describe a weekend day
    day_of_week = factor(if_else(survey_weekday %in% c("Sun", "Mon"), "Weekend", "Weekday"),
                         levels = c("Weekday", "Weekend")),
    # HOLIDAYS
    # contact_date is the day the contacts describe (the day before the survey)
    contact_date = as.Date(contact_date),
    holiday = factor(
      case_when(
        contact_date == as.Date("2024-11-28") ~ "Thanksgiving",
        contact_date == as.Date("2024-12-25") ~ "Christmas",
        contact_date == as.Date("2025-01-01") ~ "New Year's Day",
        contact_date == as.Date("2025-01-20") ~ "Martin Luther King Jr. Day",
        contact_date == as.Date("2025-04-20") ~ "Easter",
        TRUE ~ "Non-holiday"),
      levels = c("Non-holiday", "Thanksgiving", "Christmas", "New Year's Day",
                 "Martin Luther King Jr. Day", "Easter")),

    # "Prefer not to say" is missing in the models
    gender_model = factor(if_else(gender_binned == "Prefer not to say", NA_character_,
                                  as.character(gender_binned)),
                          levels = c("Woman", "Man", "Other")),
    educ_level_model = factor(if_else(educ_level_binned == "Prefer not to say", NA_character_,
                                      as.character(educ_level_binned)),
                              levels = educ_levels),
    # Small race/ethnicity groups are merged into Non-Hispanic Other.
    # "Prefer not to say" is missing, as for gender and education
    race_eth_collapsed = factor(
      case_when(
        race_eth_clean == "Prefer not to say" | is.na(race_eth_clean) ~ NA_character_,
        race_eth_clean == "Non-Hispanic White" ~ "Non-Hispanic White",
        race_eth_clean == "Non-Hispanic Black" ~ "Non-Hispanic Black",
        race_eth_clean == "Hispanic" ~ "Hispanic",
        TRUE ~ "Non-Hispanic Other"),
      levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Non-Hispanic Other"))
  )


#---- Negative binomial models #----
# Survey-weighted negative binomial regression, state as strata. Travel cluster
# with stay at home as the reference, adjusted for age group, household size,
# gender, race/ethnicity, education, and weekday/weekend.
outcomes <- c(Overall = "overall_total", Physical = "physical_total",
              `Non-physical` = "non_physical_total")
predictors <- c("travel_cluster_merged", "age_group", "hh_size_cat", "gender_model",
                "race_eth_collapsed", "educ_level_model", "day_of_week")

design <- svydesign(ids = ~1, strata = ~state_residence, weights = ~weight, data = wave1_2)

tidy_nb <- function(fit, outcome, model, terms) {
  b  <- coef(fit)
  se <- sqrt(diag(vcov(fit)))
  n_model <- sum(complete.cases(wave1_2[c(outcomes[[outcome]], terms)]))
  map_dfr(terms, function(p) {
    levs <- levels(wave1_2[[p]])
    tibble(outcome = outcome, model = model, predictor = p, level = levs,
           reference = seq_along(levs) == 1,
           beta = c(0, unname(b[paste0("eta.", p, levs[-1])])),
           se   = c(0, unname(se[paste0("eta.", p, levs[-1])])))
  }) %>%
    mutate(crr = exp(beta),
           lower = exp(beta - qnorm(0.975) * se),
           upper = exp(beta + qnorm(0.975) * se),
           p_value = if_else(reference, NA_real_, 2 * pnorm(abs(beta / se), lower.tail = FALSE)),
           n = n_model)
}

model_fits <- list()
model_results <- list()
for (o in names(outcomes)) {
  # adjusted
  f <- reformulate(predictors, response = outcomes[[o]])
  fit <- svyglm.nb(f, design = design)
  model_fits[[paste(o, "adjusted")]] <- fit
  model_results[[paste(o, "adjusted")]] <- tidy_nb(fit, o, "Adjusted", predictors)

  # unadjusted, one predictor at a time (not needed for the full-sample
  # sensitivity analysis)
  if (!apply_exclusions) next
  for (p in predictors) {
    fit <- svyglm.nb(reformulate(p, response = outcomes[[o]]), design = design)
    model_fits[[paste(o, "unadjusted", p)]] <- fit
    model_results[[paste(o, "unadjusted", p)]] <- tidy_nb(fit, o, "Unadjusted", p)
  }
}
model_results <- bind_rows(model_results)


#---- Holiday models #----
# Do people report more or fewer contacts on a holiday?
# For each holiday, compare respondents who reported contacts for that holiday
# with respondents in the same time period (wave) who reported contacts for a
# non-holiday day. Unadjusted survey-weighted negative binomial models, with the
# same design as the models above (state as strata, survey weights).
# Not needed for the full-sample sensitivity analysis.

# check how many respondents reported contacts on each holiday, by wave
table(wave1_2$holiday, wave1_2$wave)

# no one reported contacts for New Year's Day, so it is not modelled
holidays_to_model <- c("Thanksgiving", "Christmas", "Martin Luther King Jr. Day", "Easter")

holiday_models <- list()
if (apply_exclusions) {
  for (h in holidays_to_model) {

    # the time period (wave) this holiday fell in
    holiday_wave <- unique(wave1_2$wave[wave1_2$holiday == h])

    # respondents in that wave who reported contacts for this holiday or for a
    # non-holiday day (other holidays are left out)
    holiday_data <- wave1_2 %>%
      filter(wave == holiday_wave,
             holiday %in% c("Non-holiday", h)) %>%
      mutate(holiday_day = factor(if_else(holiday == h, "Holiday", "Non-holiday"),
                                  levels = c("Non-holiday", "Holiday")))

    holiday_design <- svydesign(ids = ~1, strata = ~state_residence, weights = ~weight,
                                data = holiday_data)

    # one model for each outcome (overall, physical, non-physical)
    for (o in names(outcomes)) {
      fit <- svyglm.nb(reformulate("holiday_day", response = outcomes[[o]]),
                       design = holiday_design)

      # holiday vs non-holiday coefficient and its standard error
      beta <- coef(fit)[["eta.holiday_dayHoliday"]]
      se   <- sqrt(diag(vcov(fit)))[["eta.holiday_dayHoliday"]]

      holiday_models[[paste(h, o)]] <- tibble(
        holiday = h,
        outcome = o,
        wave = holiday_wave,
        n_holiday = sum(holiday_data$holiday_day == "Holiday"),
        n_non_holiday = sum(holiday_data$holiday_day == "Non-holiday"),
        crr = exp(beta),
        lower = exp(beta - qnorm(0.975) * se),
        upper = exp(beta + qnorm(0.975) * se),
        p_value = 2 * pnorm(abs(beta / se), lower.tail = FALSE)
      )
    }
  }
}
holiday_models <- bind_rows(holiday_models)
holiday_models


#---- Holiday travel windows (time period 1) #----
# Travel questions asked about the past two weeks, so a respondent's travel
# window included Thanksgiving if the survey was within two weeks after it.
# Contacts were asked about the day before the survey (contact_date), so the
# travel window is contact_date and the 13 days before it.
# Not needed for the full-sample sensitivity analysis.
holiday_window_tables <- list()
thanksgiving_models <- list()
if (apply_exclusions) {

  # TIME PERIOD 1 respondents who have a travel cluster
  wave1 <- wave1_2 %>%
    filter(wave == 1, !is.na(travel_cluster_merged)) %>%
    mutate(
      days_since_thanksgiving = as.numeric(contact_date - as.Date("2024-11-28")),
      days_since_christmas = as.numeric(contact_date - as.Date("2024-12-25")),
      days_since_new_year = as.numeric(contact_date - as.Date("2025-01-01")),

      # which holiday (if any) was in the two-week travel window
      holiday_window = factor(
        case_when(
          days_since_thanksgiving >= 0 & days_since_thanksgiving <= 13 ~ "Thanksgiving window",
          days_since_christmas >= 0 & days_since_christmas <= 13 ~ "Christmas/New Year window",
          days_since_new_year >= 0 & days_since_new_year <= 13 ~ "Christmas/New Year window",
          TRUE ~ "Neither holiday window"),
        levels = c("Neither holiday window", "Thanksgiving window", "Christmas/New Year window")),

      # Thanksgiving window split by whether the contact day was Thanksgiving
      thanksgiving_group = factor(
        case_when(
          holiday_window == "Neither holiday window" ~ "No holiday in past two weeks",
          days_since_thanksgiving == 0 ~ "Thanksgiving was yesterday",
          holiday_window == "Thanksgiving window" ~ "Thanksgiving in past two weeks, not yesterday",
          TRUE ~ NA_character_),  # Christmas/New Year window is left out
        levels = c("No holiday in past two weeks", "Thanksgiving in past two weeks, not yesterday",
                   "Thanksgiving was yesterday"))
    )

  # check the group sizes
  table(wave1$holiday_window)
  table(wave1$thanksgiving_group, useNA = "ifany")

  # 1) Unweighted % in each travel cluster by holiday window. Unweighted to
  # match the unweighted clustering and the cluster comparisons in Table 3.
  cluster_by_window <- table(wave1$travel_cluster_merged, wave1$holiday_window)
  cluster_by_window_pct <- round(100 * prop.table(cluster_by_window, margin = 2), 1)
  cluster_by_window_pct

  # Unweighted chi-square tests: each holiday window compared with neither holiday window
  thanksgiving_vs_neither <- wave1 %>%
    filter(holiday_window != "Christmas/New Year window") %>%
    mutate(holiday_window = droplevels(holiday_window))
  test_thanksgiving <- chisq.test(table(thanksgiving_vs_neither$travel_cluster_merged,
                                        thanksgiving_vs_neither$holiday_window))

  christmas_vs_neither <- wave1 %>%
    filter(holiday_window != "Thanksgiving window") %>%
    mutate(holiday_window = droplevels(holiday_window))
  test_christmas <- chisq.test(table(christmas_vs_neither$travel_cluster_merged,
                                     christmas_vs_neither$holiday_window))
  test_thanksgiving
  test_christmas

  # check expected counts (Fisher's exact test would be needed if any are <1
  # or more than 20% are <5, as in Table 3)
  min(test_thanksgiving$expected)
  mean(test_christmas$expected < 5)

  # chi-square statistics, degrees of freedom and p-values for reporting
  holiday_window_tests <- tibble(
    comparison = c("Thanksgiving window vs neither", "Christmas/New Year window vs neither"),
    chi_square = c(unname(test_thanksgiving$statistic), unname(test_christmas$statistic)),
    df = c(unname(test_thanksgiving$parameter), unname(test_christmas$parameter)),
    p_value = c(test_thanksgiving$p.value, test_christmas$p.value))
  holiday_window_tests

  holiday_window_tables <- as.data.frame(cluster_by_window_pct) %>%
    rename(travel_cluster = Var1, holiday_window = Var2, percent = Freq) %>%
    left_join(as.data.frame(table(wave1$holiday_window)) %>%
                rename(holiday_window = Var1, n_respondents = Freq),
              by = "holiday_window") %>%
    mutate(p_value_vs_neither = case_when(
      holiday_window == "Thanksgiving window" ~ test_thanksgiving$p.value,
      holiday_window == "Christmas/New Year window" ~ test_christmas$p.value))

  # 2) Overall contacts by Thanksgiving group, for everyone and within each
  # travel cluster (unadjusted). The reference group is respondents with no
  # holiday in their two-week travel window.
  model_data <- filter(wave1, !is.na(thanksgiving_group))
  groups_to_model <- c("All respondents", levels(model_data$travel_cluster_merged))

  for (g in groups_to_model) {
    if (g == "All respondents") {
      g_data <- model_data
    } else {
      g_data <- filter(model_data, travel_cluster_merged == g)
    }
    g_design <- svydesign(ids = ~1, strata = ~state_residence, weights = ~weight, data = g_data)
    fit <- svyglm.nb(overall_total ~ thanksgiving_group, design = g_design)

    # one row for each Thanksgiving group compared with the reference group
    for (level in levels(g_data$thanksgiving_group)[-1]) {
      beta <- coef(fit)[[paste0("eta.thanksgiving_group", level)]]
      se   <- sqrt(diag(vcov(fit)))[[paste0("eta.thanksgiving_group", level)]]
      thanksgiving_models[[paste(g, level)]] <- tibble(
        travel_cluster = g,
        thanksgiving_group = level,
        n_group = sum(g_data$thanksgiving_group == level),
        n_reference = sum(g_data$thanksgiving_group == "No holiday in past two weeks"),
        crr = exp(beta),
        lower = exp(beta - qnorm(0.975) * se),
        upper = exp(beta + qnorm(0.975) * se),
        p_value = 2 * pnorm(abs(beta / se), lower.tail = FALSE)
      )
    }
  }
}
thanksgiving_models <- bind_rows(thanksgiving_models)
thanksgiving_models

model_results %>%
  filter(model == "Adjusted", predictor == "travel_cluster_merged") %>%
  select(outcome, level, crr, lower, upper, n)


#---- Save #----
saveRDS(wave1_2, file.path(derived_dir, "analysis_data.rds"))
saveRDS(list(fits = model_fits, travel_scaled = travel_scaled, lowmob_scaled = lowmob_scaled,
             k4 = k4, km_lowmob = km_lowmob),
        file.path(derived_dir, "model_fits.rds"))

write_csv(sample_flow, file.path(out_dir, "sample_flow.csv"))
write_csv(any_travel_summary, file.path(out_dir, "any_travel_summary.csv"))
write_csv(weight_summary, file.path(out_dir, "weight_summary.csv"))
write_csv(cluster_summary, file.path(out_dir, "cluster_summary.csv"))
write_csv(as_tibble(k4$centers, rownames = "cluster") %>% mutate(size = k4$size),
          file.path(out_dir, "cluster_centroids_main.csv"))
write_csv(as_tibble(km_lowmob$centers, rownames = "cluster") %>% mutate(size = km_lowmob$size),
          file.path(out_dir, "cluster_centroids_low_mobility.csv"))
if (apply_exclusions) {
  write_csv(elbow, file.path(out_dir, "elbow_wss.csv"))
  write_csv(cluster_tests, file.path(out_dir, "cluster_tests.csv"))
  write_csv(holiday_models, file.path(out_dir, "holiday_models.csv"))
  write_csv(holiday_window_tables, file.path(out_dir, "travel_cluster_by_holiday_window.csv"))
  write_csv(holiday_window_tests, file.path(out_dir, "holiday_window_tests.csv"))
  write_csv(thanksgiving_models, file.path(out_dir, "thanksgiving_window_models.csv"))
}
write_csv(model_results, file.path(out_dir, "regression_results.csv"))
writeLines(capture.output(sessionInfo()), file.path(out_dir, "session_info.txt"))
