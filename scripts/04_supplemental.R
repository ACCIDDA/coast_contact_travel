#---- 04_supplemental.R #----
# Supplemental Materials tables and figures from the saved analysis
# (01_analysis.R). Run after 01_analysis.R.
#
# Run from the repository root:
#   Rscript scripts/04_supplemental.R
#
# Outputs (outputs/supplement)
#   Table S1     contacts by type, setting and demographics (weighted)
#   Figure S1    contacts by setting among those reporting any contact
#   Table S2     travel by state (weighted), among those who answered both
#                travel questions
#   Figure S3    demographics by travel cluster
#   Figure S4    histograms of contacts by travel cluster
#   Figure S5    boxplots of overall contacts by travel cluster
#   Table S3     overall contacts by travel cluster
#   Table S4     unadjusted and adjusted models for travel cluster
#   Figure S6    Pearson residuals for the adjusted model of overall contacts
#   Figure S7, Table S5   sensitivity analysis with the full sample (reruns
#                01_analysis.R without exclusions, which takes a while)
#   Figures S8-S9  models with distance travelled instead of travel cluster
#   Figure S10   sample compared with the ACS margins used for weighting
#   Table S7     survey weights by time period and state
#   Table S8     contacts by weekday/weekend and on holidays, with holiday
#                models
#   Figures S13-S14  elbow plots for the travel clusters
#
# Not made here: Figure S2 (cluster diagram; cluster sizes are in Table S3),
# Table S6 (sampling targets set before data collection), and Figures S11-S12
# (unweighted contact matrices, made in 03_contact_matrices.R).

#---- Set up #----
library(tidyverse)
library(Hmisc)   # wtd.quantile
library(survey)  # svyquantile, svydesign
library(sjstats) # svyglm.nb()
library(patchwork)
library(writexl)

analysis_dir <- file.path("outputs", "analysis")
supp_dir <- file.path("outputs", "supplement")
dir.create(supp_dir, recursive = TRUE, showWarnings = FALSE)

wave1_2 <- readRDS(file.path("data", "derived", "analysis_data.rds"))
model_results <- read_csv(file.path(analysis_dir, "regression_results.csv"), show_col_types = FALSE)

cluster_levels <- levels(wave1_2$travel_cluster_merged)

# Respondents who answered both travel questions (those with a travel cluster)
clustered <- filter(wave1_2, travel_answered)


#---- Weighted summary helpers #----
# Weighted mean: sum(w * x) / sum(w)
wtd_mean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0  # TRUE for usable respondents
  sum(w[ok] * x[ok]) / sum(w[ok])
}

# Weighted standard deviation: the weights are rescaled to sum to 1, then the
# SD is the square root of the weighted average squared distance from the
# weighted mean
wtd_sd <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok] / sum(w[ok])
  sqrt(sum(w * (x - sum(w * x))^2))
}

# Weighted quantiles (probs = 0.5 for the median, c(0.25, 0.75) for the IQR).
# Uses svyquantile() from the survey package on a simple weighted design. If
# there are no usable respondents it returns NA, if there is only one it
# returns that value, and if svyquantile() fails it uses Hmisc's
# wtd.quantile() instead.
svy_quant_safe <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(rep(NA_real_, length(probs)))
  x <- x[ok]; w <- w[ok]
  if (length(x) == 1) return(rep(x, length(probs)))
  d <- data.frame(x = x, w = w)
  tryCatch({
    des <- svydesign(ids = ~1, weights = ~w, data = d)
    as.numeric(svyquantile(~x, design = des, quantiles = probs, na.rm = TRUE, ci = FALSE)[[1]])
  }, error = function(e) {
    as.numeric(wtd.quantile(d$x, weights = d$w, probs = probs, type = "quantile"))
  })
}

fmt_iqr <- function(q25, q75, digits = 0) {
  paste0("(", round(q25, digits), ", ", round(q75, digits), ")")
}

# "n (%)" cell: n is the unweighted number with condition TRUE and % is the
# weighted percent among respondents with a non-missing condition
fmt_w_n_pct <- function(w, condition) {
  ok <- !is.na(condition)
  sprintf("%s (%.1f%%)", format(sum(condition[ok]), big.mark = ","),
          100 * sum(w[ok & condition]) / sum(w[ok]))
}

# "mean (SD)" cell, weighted, among respondents with a value
fmt_w_mean_sd <- function(x, w) {
  sprintf("%s (%s)", format(round(wtd_mean(x, w), 2), nsmall = 2, big.mark = ","),
          format(round(wtd_sd(x, w), 2), nsmall = 2, big.mark = ","))
}

# "median (Q1-Q3)" cell, weighted, among respondents with a value
fmt_w_median_iqr <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_character_)
  qs <- wtd.quantile(x[ok], weights = w[ok], probs = c(0.25, 0.5, 0.75), type = "quantile")
  sprintf("%.1f (%.1f-%.1f)", qs[2], qs[1], qs[3])
}

fmt_crr <- function(crr, lower, upper, reference) {
  if_else(reference, "REF", sprintf("%.2f (%.2f-%.2f)", crr, lower, upper))
}


#---- Table S1: contacts by type, setting, and demographics (weighted) #----
# 1) Physical vs non-physical and by setting
summary_vars <- list(
  "Total contacts" = c("overall_total"),
  "Nature of contact" = c("physical_total",
                          "non_physical_total"),
  "Physical contact setting" = c("physical_home",
                                 "physical_work",
                                 "physical_school",
                                 "physical_other"),
  "Non-physical contact setting" = c("non_physical_home",
                                     "non_physical_work",
                                     "non_physical_school",
                                     "non_physical_other"),
  "Total contacts by setting" = c("overall_home",
                                  "overall_work",
                                  "overall_school",
                                  "overall_other")
)

# one row per respondent per contact variable
wave1_2_long2 <- imap_dfr(summary_vars, function(cols, char_name) {
  wave1_2 %>%
    select(weight, all_of(cols)) %>%
    pivot_longer(
      cols = all_of(cols),
      names_to = "group",
      values_to = "value"
    ) %>%
    mutate(
      section = char_name,
      group = case_when(
        group == "overall_total" ~ "Overall",
        group == "physical_total" ~ "Physical",
        group == "non_physical_total" ~ "Non-physical",
        group %in% c("physical_home", "non_physical_home", "overall_home") ~ "Home",
        group %in% c("physical_work", "non_physical_work", "overall_work") ~ "Work",
        group %in% c("physical_school", "non_physical_school", "overall_school") ~ "School",
        group %in% c("physical_other", "non_physical_other", "overall_other") ~ "Other"
      )
    )
}) %>%
  mutate(
    section = factor(section, levels = names(summary_vars)),
    group = factor(group, levels = c("Overall", "Physical", "Non-physical",
                                     "Home", "Work", "School", "Other"))
  )

s1_contacts <- wave1_2_long2 %>%
  group_by(section, group) %>%
  summarise(
    mean = round(wtd_mean(value, weight), 1),
    sd = round(wtd_sd(value, weight), 1),

    median = {
      q <- svy_quant_safe(value, weight, probs = 0.50)
      round(q[1], 0)
    },

    IQR = {
      qs <- svy_quant_safe(value, weight, probs = c(0.25, 0.75))
      fmt_iqr(qs[1], qs[2], digits = 0)
    },

    n = n(),
    weighted_n = sum(weight),
    .groups = "drop"
  )

# 2) Overall contacts by demographic group
char_order <- c("Age group" = "age_group", "Household size" = "hh_size_cat",
                "Gender" = "gender", "Race/ethnicity" = "race_eth_clean",
                "Education level" = "educ_level_binned", "Household income" = "hh_income_binned",
                "Work outside household" = "work_outside_hh", "Occupation" = "occ")

# one row per respondent per demographic variable
wave1_2_long <- wave1_2 %>%
  mutate(across(all_of(unname(char_order)), as.character)) %>%
  pivot_longer(
    cols = all_of(unname(char_order)),
    names_to = "variable",
    values_to = "group"
  ) %>%
  filter(!is.na(group)) %>%
  mutate(section = factor(names(char_order)[match(variable, char_order)], levels = names(char_order)))

s1_demographics <- wave1_2_long %>%
  group_by(section, variable, group) %>%
  summarise(
    mean = round(wtd_mean(overall_total, weight), 1),
    sd = round(wtd_sd(overall_total, weight), 1),

    median = {
      q <- svy_quant_safe(overall_total, weight, probs = 0.50)
      round(q[1], 0)
    },

    IQR = {
      qs <- svy_quant_safe(overall_total, weight, probs = c(0.25, 0.75))
      fmt_iqr(qs[1], qs[2], digits = 0)
    },

    n = n(),
    weighted_n = sum(weight),
    .groups = "drop"
  ) %>%
  # list groups in the order of each variable's factor levels
  mutate(level_order = map2_int(variable, group, ~ match(.y, levels(wave1_2[[.x]])))) %>%
  arrange(section, level_order)

# combine; weighted percent is each group's share of the total survey weight
table_s1 <- bind_rows(s1_contacts, s1_demographics) %>%
  mutate(weighted_percent = sprintf("%.1f%%", 100 * weighted_n / sum(wave1_2$weight))) %>%
  select(section, group, mean, sd, median, IQR, n, weighted_percent)
table_s1
write_csv(table_s1, file.path(supp_dir, "table_s1_contacts_weighted.csv"))


#---- Figure S1: contacts by setting among those reporting any #----
contacts_long <- wave1_2 %>%
  select(overall_home, overall_school, overall_work, overall_other) %>%
  pivot_longer(
    cols = everything(),
    names_to = "setting",
    values_to = "n_contacts"
  ) %>%
  mutate(
    setting = case_when(
      setting == "overall_home"   ~ "Home",
      setting == "overall_school" ~ "School",
      setting == "overall_work"   ~ "Work",
      setting == "overall_other"  ~ "Other"
    ),
    setting = factor(setting, levels = c("Home", "School", "Work", "Other"))
  )

# percent of all respondents (unweighted) reporting any contact in each setting
pct_any <- contacts_long %>%
  group_by(setting) %>%
  summarise(pct = round(100 * mean(n_contacts > 0)), .groups = "drop")
pct_any

contacts_long_nonzero <- contacts_long %>%
  filter(n_contacts > 0) %>%
  left_join(pct_any, by = "setting") %>%
  mutate(setting_label = fct_reorder(paste0(setting, "\n", pct, "% reported contact"),
                                     as.integer(setting)))

figure_s1 <- ggplot(contacts_long_nonzero, aes(x = setting_label, y = n_contacts, fill = setting)) +
  geom_boxplot(outlier.shape = 1, outlier.alpha = 0.4, width = 0.5) +
  labs(
    title = "Contacts by setting (among those reporting 1+ contacts)",
    x = NULL,
    y = "No. contacts"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 11)
  )
ggsave(file.path(supp_dir, "figure_s1_contacts_by_setting.png"), figure_s1,
       width = 7, height = 5, dpi = 300, bg = "white")


#---- Table S2: travel by state (weighted) #----
# Only respondents who answered both travel questions (those with a travel
# cluster), so N in each column is the denominator for the percent travelling;
# means and medians are among those who reported travel for that purpose
# (non-travelers are NA in the travel variables)
travel_state_summary <- function(d) {
  tibble(
    section = rep(c("Work/School", "Vacation/Other", "Work/School", "Vacation/Other"),
                  times = c(5, 5, 5, 5)),
    variable = c("Work/School Travel Reported",
                 "Mean (SD) number of locations travelled to",
                 "Mean (SD) of sum of distances for all trips from home (km)",
                 "Mean (SD) of distance from home for all trips (km)",
                 "Mean (SD) days per week travelling",
                 "Vacation/Other Travel Reported",
                 "Mean (SD) number of locations travelled to",
                 "Mean (SD) of sum of distances for all trips from home (km)",
                 "Mean (SD) of distance from home for all trips (km)",
                 "Mean (SD) nights spent away",
                 "Work/School Travel Reported",
                 "Median (IQR) number of locations travelled to",
                 "Median (IQR) of sum of distances for all trips from home (km)",
                 "Median (IQR) of distance from home for all trips (km)",
                 "Median (IQR) days per week travelling",
                 "Vacation/Other Travel Reported",
                 "Median (IQR) number of locations travelled to",
                 "Median (IQR) of sum of distances for all trips from home (km)",
                 "Median (IQR) of distance from home for all trips (km)",
                 "Median (IQR) nights spent away"),
    value = c(fmt_w_n_pct(d$weight, d$work_school_travel_bin == 1),
              fmt_w_mean_sd(d$work_school_trips_2wk, d$weight),
              fmt_w_mean_sd(d$reg_trip_sum_km_test, d$weight),
              fmt_w_mean_sd(d$reg_trip_mean_km_test, d$weight),
              fmt_w_mean_sd(d$work_school_travel_sum7, d$weight),
              fmt_w_n_pct(d$weight, d$other_travel_bin == 1),
              fmt_w_mean_sd(d$other_trips_2wk, d$weight),
              fmt_w_mean_sd(d$other_trip_sum_km_test, d$weight),
              fmt_w_mean_sd(d$other_trip_mean_km_test, d$weight),
              fmt_w_mean_sd(d$other_travel_nights_sum14, d$weight),
              fmt_w_n_pct(d$weight, d$work_school_travel_bin == 1),
              fmt_w_median_iqr(d$work_school_trips_2wk, d$weight),
              fmt_w_median_iqr(d$reg_trip_sum_km_test, d$weight),
              fmt_w_median_iqr(d$reg_trip_mean_km_test, d$weight),
              fmt_w_median_iqr(d$work_school_travel_sum7, d$weight),
              fmt_w_n_pct(d$weight, d$other_travel_bin == 1),
              fmt_w_median_iqr(d$other_trips_2wk, d$weight),
              fmt_w_median_iqr(d$other_trip_sum_km_test, d$weight),
              fmt_w_median_iqr(d$other_trip_mean_km_test, d$weight),
              fmt_w_median_iqr(d$other_travel_nights_sum14, d$weight)),
    statistic = rep(c("Mean (SD)", "Median (IQR)"), each = 10)
  )
}

travel_answered_data <- filter(wave1_2, travel_answered)
table_s2 <- bind_rows(
  map_dfr(levels(travel_answered_data$state_residence),
          ~ travel_state_summary(filter(travel_answered_data, state_residence == .x)) %>% mutate(group = .x)),
  travel_state_summary(travel_answered_data) %>% mutate(group = "All States")
) %>%
  mutate(row = rep(1:20, times = 5)) %>%
  pivot_wider(names_from = group, values_from = value) %>%
  select(-row)

# unweighted column Ns
state_n <- c(table(travel_answered_data$state_residence), "All States" = nrow(travel_answered_data))
names(table_s2)[-(1:3)] <- paste0(names(table_s2)[-(1:3)], " (N=",
                                  format(state_n[names(table_s2)[-(1:3)]], big.mark = ",", trim = TRUE), ")")
table_s2
write_csv(table_s2, file.path(supp_dir, "table_s2_travel_by_state.csv"))


#---- Figure S3: demographics by travel cluster (unweighted) #----
plot_demographic_stack_total <- function(df, demo_var, demo_label) {

  dat <- df %>%
    mutate(
      category = fct_na_value_to_level({{ demo_var }}, level = "Missing"),
      cluster  = as.character(travel_cluster_merged)
    )

  # Within-cluster distribution
  by_cluster <- dat %>%
    count(cluster, category) %>%
    group_by(cluster) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()

  # Overall ("Total") distribution for the same respondents
  overall <- dat %>%
    count(category) %>%
    mutate(
      pct = n / sum(n),
      cluster = "Total"
    )

  plotdf <- bind_rows(
    by_cluster %>% select(cluster, category, pct),
    overall   %>% select(cluster, category, pct)
  ) %>%
    mutate(cluster = factor(cluster, levels = c(cluster_levels, "Total")))

  ggplot(plotdf, aes(x = cluster, y = pct, fill = category)) +
    geom_col(width = 0.7) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = "Travel cluster",
      y = "Percent",
      fill = demo_label,
      title = demo_label
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

p_gender <- plot_demographic_stack_total(clustered, gender_binned, "Gender")
p_race   <- plot_demographic_stack_total(clustered, race_eth_clean, "Race / ethnicity")
p_age    <- plot_demographic_stack_total(clustered, age_group, "Age group")
p_hh     <- plot_demographic_stack_total(clustered, hh_size_cat, "Household size")
p_educ   <- plot_demographic_stack_total(clustered, educ_level_binned, "Education level")
p_income <- plot_demographic_stack_total(clustered, hh_income_binned, "Household income")
p_work   <- plot_demographic_stack_total(clustered, work_outside_hh, "Work outside household")
p_occ    <- plot_demographic_stack_total(clustered, occ, "Occupation")
p_state  <- plot_demographic_stack_total(clustered, state_residence, "State of residence")
p_party  <- plot_demographic_stack_total(clustered, political_party, "Political party")
p_neighborhood <- plot_demographic_stack_total(clustered, neighborhood, "Neighborhood")

panel_1 <- (p_gender | p_race) / (p_age | p_hh)
panel_2 <- (p_educ | p_income) / (p_work | p_occ)
panel_3 <- (p_state | p_party) / (p_neighborhood | plot_spacer())
ggsave(file.path(supp_dir, "figure_s3a_demographics_by_cluster.png"), panel_1,
       width = 13, height = 10, dpi = 300, bg = "white")
ggsave(file.path(supp_dir, "figure_s3b_demographics_by_cluster.png"), panel_2,
       width = 13, height = 10, dpi = 300, bg = "white")
ggsave(file.path(supp_dir, "figure_s3c_demographics_by_cluster.png"), panel_3,
       width = 13, height = 10, dpi = 300, bg = "white")


#---- Figure S4: histograms of contacts by travel cluster #----
plotframe <- data.frame(overall_total = c(clustered$overall_total,
                                          clustered$physical_total,
                                          clustered$non_physical_total),
                        travel_cluster_merged = rep(clustered$travel_cluster_merged, times = 3),
                        label = c(rep("Total", times = nrow(clustered)),
                                  rep("Physical", times = nrow(clustered)),
                                  rep("Non-physical", times = nrow(clustered))))

figure_s4 <- ggplot(plotframe) +
  geom_histogram(
    aes(x = overall_total, fill = travel_cluster_merged),
    show.legend = FALSE,
    bins = 35
  ) +
  facet_grid(
    travel_cluster_merged ~ label,
    scales = "free",
    labeller = labeller(
      travel_cluster_merged = label_wrap_gen(width = 20)
    )
  ) +
  labs(
    x = "Total number of overall contacts",
    y = "Frequency"
  ) +
  theme_classic() +
  theme(
    strip.text.y = element_text(size = 8),
    panel.border = element_rect(
      color = "grey45",
      fill = NA,
      linewidth = 0.7
    ),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_brewer(palette = "Set2")
ggsave(file.path(supp_dir, "figure_s4_contact_histograms_by_cluster.png"), figure_s4,
       width = 9, height = 9, dpi = 300, bg = "white")


#---- Figure S5: boxplots of overall contacts by travel cluster #----
figure_s5 <- ggplot(clustered, aes(x = travel_cluster_merged, y = overall_total)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(
    x = "Travel Cluster",
    y = "Total Daily Contacts",
    title = "Distribution of Total Contacts by Travel Cluster"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(file.path(supp_dir, "figure_s5_contacts_boxplot_by_cluster.png"), figure_s5,
       width = 9, height = 7.5, dpi = 300, bg = "white")


#---- Table S3: overall contacts by travel cluster #----
table_s3 <- clustered %>%
  group_by(travel_cluster_merged) %>%
  summarise(n = n(), mean = mean(overall_total), sd = sd(overall_total),
            median = median(overall_total), min = min(overall_total), max = max(overall_total),
            q25 = quantile(overall_total, 0.25), q75 = quantile(overall_total, 0.75),
            weighted_mean = wtd_mean(overall_total, weight),
            .groups = "drop")
table_s3
write_csv(table_s3, file.path(supp_dir, "table_s3_contacts_by_cluster.csv"))


#---- Table S4: travel cluster models #----
fmt_p <- function(p, reference) {
  case_when(reference ~ "", p < 0.001 ~ "<0.001", TRUE ~ sprintf("%.3f", p))
}

table_s4 <- model_results %>%
  filter(predictor == "travel_cluster_merged") %>%
  mutate(outcome = factor(outcome, levels = c("Overall", "Physical", "Non-physical")),
         level = factor(level, levels = cluster_levels),
         beta = if_else(reference, "", sprintf("%.3f", beta)),
         crr_ci = fmt_crr(crr, lower, upper, reference),
         p = fmt_p(p_value, reference)) %>%
  select(outcome, level, model, beta, crr_ci, p, n) %>%
  pivot_wider(names_from = model, values_from = c(beta, crr_ci, p, n)) %>%
  arrange(outcome, level) %>%
  select(outcome, level,
         beta_Unadjusted, crr_ci_Unadjusted, p_Unadjusted,
         beta_Adjusted, crr_ci_Adjusted, p_Adjusted, n_Unadjusted, n_Adjusted)
table_s4
write_csv(table_s4, file.path(supp_dir, "table_s4_travel_cluster_models.csv"))


#---- Figure S6: Pearson residuals, adjusted model of overall contacts #----
model_fits <- readRDS(file.path("data", "derived", "model_fits.rds"))
overall_nb <- model_fits$fits[["Overall adjusted"]]

resid_df <- data.frame(fitted = as.numeric(predict(overall_nb, type = "response")),
                       pearson = as.numeric(residuals(overall_nb, type = "pearson")))
summary(resid_df)

figure_s6 <- ggplot(resid_df, aes(x = fitted, y = pearson)) +
  geom_point(alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Pearson residuals")
ggsave(file.path(supp_dir, "figure_s6_pearson_residuals.png"), figure_s6,
       width = 8, height = 5.5, dpi = 300, bg = "white")
rm(model_fits, overall_nb)


#---- Figure S7 and Table S5: full sample without exclusions #----
# Reruns 01_analysis.R without the two sample exclusions (survey weights and
# travel clusters are recomputed on the full sample; each cluster is labelled
# by the study cluster it is closest to). Results are saved in
# outputs/analysis/full_sample.
full_sample <- new.env()
full_sample$apply_exclusions <- FALSE
source(file.path("scripts", "01_analysis.R"), local = full_sample)

sensitivity <- bind_rows(
  model_results %>% mutate(sample = "Analytic sample"),
  full_sample$model_results %>% mutate(sample = "Full sample")
) %>%
  filter(model == "Adjusted", predictor == "travel_cluster_merged")

sample_labels <- c(
  "Analytic sample" = paste0("Sample from paper (N = ", format(nrow(wave1_2), big.mark = ","), ")"),
  "Full sample" = paste0("Full sample (N = ", format(nrow(full_sample$wave1_2), big.mark = ","), ")")
)

cluster_n_by_sample <- bind_rows(
  count(clustered, level = travel_cluster_merged) %>% mutate(sample = "Analytic sample"),
  count(filter(full_sample$wave1_2, travel_answered), level = travel_cluster_merged) %>%
    mutate(sample = "Full sample")
) %>%
  mutate(level = as.character(level))

table_s5 <- sensitivity %>%
  left_join(cluster_n_by_sample, by = c("sample", "level"), suffix = c("_model", "_cluster")) %>%
  mutate(outcome = factor(outcome, levels = c("Overall", "Physical", "Non-physical")),
         level = factor(level, levels = cluster_levels),
         crr_ci = fmt_crr(crr, lower, upper, reference),
         sample = recode(sample, "Analytic sample" = "paper", "Full sample" = "full")) %>%
  select(outcome, level, sample, n_cluster, crr_ci, n_model) %>%
  pivot_wider(names_from = sample, values_from = c(n_cluster, crr_ci, n_model)) %>%
  arrange(outcome, level) %>%
  select(outcome, level, n_cluster_paper, crr_ci_paper, n_cluster_full, crr_ci_full,
         n_model_paper, n_model_full)
table_s5
write_csv(table_s5, file.path(supp_dir, "table_s5_full_sample_sensitivity.csv"))

figure_s7 <- sensitivity %>%
  filter(!reference) %>%
  mutate(Outcome = factor(outcome, levels = c("Overall", "Non-physical", "Physical")),
         Level = factor(level, levels = cluster_levels),
         Sample = factor(sample_labels[sample], levels = sample_labels)) %>%
  ggplot(aes(x = crr, y = fct_rev(Level), color = Sample, shape = Sample)) +
  geom_point(position = position_dodge(width = 0.5), size = 1.8) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  facet_wrap(~ Outcome) +
  scale_color_manual(values = c("grey20", "#2166AC"), name = NULL) +
  scale_shape_manual(values = c(16, 17), name = NULL) +
  guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
  labs(x = "Contact Rate Ratio", y = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "grey96"),
        strip.background = element_blank())
ggsave(file.path(supp_dir, "figure_s7_full_sample_sensitivity.png"), figure_s7,
       width = 11, height = 4.5, dpi = 300, bg = "white")


#---- Figures S8 and S9: distance travelled instead of travel cluster #----
# Sum of distance travelled for work/school and vacation/other trips in the
# past two weeks (km). People who reported no travel for a purpose contribute
# 0 km for it; people who did not answer a travel question are missing.
# Figure S8: no travel is the reference group and travellers are split into
# quintiles of distance. Figure S9: distance as a z-score.
dist_data <- wave1_2 %>%
  mutate(
    work_km = case_when(work_school_travel_bin == 0 ~ 0, TRUE ~ reg_trip_sum_km_test),
    other_km = case_when(other_travel_bin == 0 ~ 0, TRUE ~ other_trip_sum_km_test),
    sum_distance_travel = work_km + other_km,
    sum_distance_travel_scaled = as.numeric(scale(sum_distance_travel)),
    sum_distance_travel_q = case_when(
      sum_distance_travel == 0 ~ "No travel",
      sum_distance_travel > 0 ~ paste0("Q", ntile(if_else(sum_distance_travel > 0, sum_distance_travel, NA), 5))),
    sum_distance_travel_q = factor(sum_distance_travel_q,
                                   levels = c("No travel", paste0("Q", 1:5)),
                                   labels = c("No travel", "Q1 (Lowest distance)", "Q2", "Q3",
                                              "Q4", "Q5 (Highest distance)"))
  )

# quintile cut points (km) among travellers
dist_data %>%
  filter(!is.na(sum_distance_travel_q)) %>%
  group_by(sum_distance_travel_q) %>%
  summarise(n = n(), min_km = min(sum_distance_travel), max_km = max(sum_distance_travel))

dist_design <- svydesign(ids = ~1, strata = ~state_residence, weights = ~weight, data = dist_data)

dist_outcomes <- c(Overall = "overall_total", Physical = "physical_total",
                   `Non-physical` = "non_physical_total")
covariates <- c("age_group", "hh_size_cat", "gender_model", "race_eth_collapsed",
                "educ_level_model", "day_of_week")

# CRRs and 95% CIs for every term of an adjusted model
tidy_dist <- function(fit, outcome, terms) {
  b  <- coef(fit)
  se <- sqrt(diag(vcov(fit)))
  n_model <- sum(complete.cases(dist_data[c(dist_outcomes[[outcome]], terms)]))
  map_dfr(terms, function(p) {
    levs <- if (is.factor(dist_data[[p]])) levels(dist_data[[p]]) else p
    coef_names <- if (is.factor(dist_data[[p]])) paste0("eta.", p, levs[-1]) else paste0("eta.", p)
    tibble(outcome = outcome, predictor = p, level = levs,
           reference = if (is.factor(dist_data[[p]])) seq_along(levs) == 1 else FALSE,
           beta = if (is.factor(dist_data[[p]])) c(0, unname(b[coef_names])) else unname(b[coef_names]),
           se = if (is.factor(dist_data[[p]])) c(0, unname(se[coef_names])) else unname(se[coef_names]))
  }) %>%
    mutate(crr = exp(beta),
           lower = exp(beta - qnorm(0.975) * se),
           upper = exp(beta + qnorm(0.975) * se),
           n = n_model)
}

dist_results <- list()
for (main_predictor in c("sum_distance_travel_q", "sum_distance_travel_scaled")) {
  terms <- c(main_predictor, covariates)
  for (o in names(dist_outcomes)) {
    fit <- svyglm.nb(reformulate(terms, response = dist_outcomes[[o]]), design = dist_design)
    dist_results[[paste(main_predictor, o)]] <- tidy_dist(fit, o, terms) %>%
      mutate(model = main_predictor)
  }
}
dist_results <- bind_rows(dist_results)
write_csv(dist_results, file.path(supp_dir, "distance_models.csv"))

predictor_labels <- c(sum_distance_travel_q = "Distance Quintile",
                      sum_distance_travel_scaled = "Distance (z-score)",
                      age_group = "Age Group", gender_model = "Gender",
                      race_eth_collapsed = "Race and Ethnicity", educ_level_model = "Education",
                      hh_size_cat = "Household Size", day_of_week = "Week")

plot_dist_model <- function(results) {
  plot_dat <- results %>%
    mutate(
      Level = if_else(reference, paste0(level, " (REF)"), level),
      Level = if_else(predictor == "sum_distance_travel_scaled", "Per SD increase in distance", Level),
      Level = fct_inorder(Level),
      Predictor = factor(predictor_labels[predictor], levels = predictor_labels),
      Outcome = factor(outcome, levels = c("Overall", "Physical", "Non-physical")),
      # no point or CI for the reference rows, their labels are kept
      across(c(crr, lower, upper), ~ if_else(reference, NA_real_, .x))
    )

  ggplot(plot_dat, aes(x = crr, y = fct_rev(Level), color = Outcome, shape = Outcome)) +
    geom_point(position = position_dodge(width = 0.4), size = 1.4, na.rm = TRUE) +
    geom_errorbarh(aes(xmin = lower, xmax = upper),
                   position = position_dodge(width = 0.4),
                   height = 0.2, linewidth = 0.3, na.rm = TRUE) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_x_continuous(trans = "log10",
                       name = "Contact Rate Ratio (log scale)",
                       breaks = c(0.5, 0.75, 1, 1.5, 2, 3)) +
    scale_color_manual(name = "Contact Type",
                       values = c("Overall" = "black",
                                  "Physical" = "darkgoldenrod3",
                                  "Non-physical" = "darkcyan")) +
    scale_shape_manual(name = "Contact Type",
                       values = c("Overall" = 16,
                                  "Physical" = 17,
                                  "Non-physical" = 15)) +
    ylab("") +
    facet_grid(Predictor ~ ., scales = "free_y", space = "free_y", switch = "y",
               labeller = labeller(Predictor = label_wrap_gen(width = 12))) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "top",
      legend.justification = "right",
      panel.spacing.y = unit(1, "lines"),
      panel.background = element_rect(fill = "grey96", color = NA),
      strip.placement = "outside",
      strip.background = element_rect(fill = "grey90", color = "grey80", linewidth = 0.6),
      strip.text.y.left = element_text(angle = 0, hjust = 0, face = "bold", size = 8, color = "grey20"),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.6),
      axis.text.y = element_text(size = 8, margin = margin(r = 6)),
      axis.ticks.y = element_line(color = "grey20", linewidth = 0.4),
      axis.ticks.length.y = unit(0.05, "cm")
    )
}

figure_s8 <- plot_dist_model(filter(dist_results, model == "sum_distance_travel_q"))
ggsave(file.path(supp_dir, "figure_s8_distance_quintile_models.png"), figure_s8,
       width = 7.5, height = 7, dpi = 300, bg = "white")
figure_s9 <- plot_dist_model(filter(dist_results, model == "sum_distance_travel_scaled"))
ggsave(file.path(supp_dir, "figure_s9_distance_zscore_models.png"), figure_s9,
       width = 7.5, height = 6.5, dpi = 300, bg = "white")


#---- Figure S10: sample compared with ACS margins #----
# Unweighted sample percentages (both waves, analytic sample) minus the 2023
# ACS percentages used for raking, in the raking categories. Respondents
# missing a category (e.g. prefer not to say) are left out of that variable.
acs_dir <- file.path("data", "2023 ACS 1 year")
state_codes <- c(florida = "FL", georgia = "GA", maryland = "MD", "north carolina" = "NC",
                 fl = "FL", ga = "GA", md = "MD", nc = "NC")

read_acs <- function(file, category_col, variable) {
  read.csv(file.path(acs_dir, file)) %>%
    rename(group = all_of(category_col)) %>%
    filter(!str_detect(tolower(group), "^total")) %>%  # drop total population rows
    transmute(variable = variable, state_residence = state_codes[tolower(State)], group,
              estimate = as.numeric(gsub(",", "", estimate))) %>%
    group_by(state_residence) %>%
    mutate(acs_pct = 100 * estimate / sum(estimate)) %>%
    ungroup()
}

acs_pct <- bind_rows(
  read_acs("ACS_2023_age.csv", "age_group", "Age") %>%
    mutate(group = recode(group, "18 to 34 years" = "18-34", "35 to 54 years" = "35-54",
                          "55 to 64 years" = "55-64", "65 years and over" = "65+")),
  read_acs("ACS_2023_sex.csv", "group", "Sex"),
  read_acs("ACS_2023_education.csv", "educ_level", "Education"),
  read_acs("ACS_2023_race.csv", "group", "Race/Ethnicity") %>%
    mutate(group = recode(group,
                          "Hispanic or Latino (of any race)" = "Hispanic",
                          "NH White or Caucasian" = "Non-Hispanic White",
                          "NH Black or African American" = "Non-Hispanic Black",
                          "NH American Indian or Alaska Native" = "Non-Hispanic AI/AN",
                          "NH Asian or Pacific Islander" = "Non-Hispanic Asian/PI",
                          "NH Other" = "Non-Hispanic Other Race",
                          "NH Multiple Races" = "Non-Hispanic Multiple Race"))
)

# raking categories, as in 01_analysis.R
sample_vars <- wave1_2 %>%
  transmute(
    state_residence = as.character(state_residence),
    Age = as.character(age_group),
    Sex = case_when(gender == "Woman" ~ "female", gender == "Man" ~ "male"),
    Education = if_else(educ_level_binned == "Prefer not to say", NA_character_,
                        as.character(educ_level_binned)),
    total_selected = (race_white == 1) + (race_black == 1) + (race_hispanic == 1) +
      (race_native == 1) + (race_asian == 1) + (race_other == 1) + (race_nr == 1),
    `Race/Ethnicity` = case_when(
      race_hispanic == 1 ~ "Hispanic",
      race_hispanic == 0 & total_selected > 1 ~ "Non-Hispanic Multiple Race",
      race_hispanic == 0 & race_asian == 1 ~ "Non-Hispanic Asian/PI",
      race_hispanic == 0 & race_white == 1 ~ "Non-Hispanic White",
      race_hispanic == 0 & race_black == 1 ~ "Non-Hispanic Black",
      race_hispanic == 0 & race_native == 1 ~ "Non-Hispanic AI/AN",
      race_hispanic == 0 & race_other == 1 ~ "Non-Hispanic Other Race")
  ) %>%
  select(-total_selected) %>%
  pivot_longer(-state_residence, names_to = "variable", values_to = "group") %>%
  filter(!is.na(group)) %>%
  count(variable, state_residence, group) %>%
  group_by(variable, state_residence) %>%
  mutate(sample_pct = 100 * n / sum(n)) %>%
  ungroup()

level_list <- list(
  Age       = c("18-34", "35-54", "55-64", "65+"),
  Sex       = c("male", "female"),
  Education = c("Less than high school graduate",
                "High school graduate (includes equivalency)",
                "Some college or associate's degree",
                "Bachelor's degree",
                "Graduate or professional degree"),
  `Race/Ethnicity` = c("Hispanic", "Non-Hispanic Multiple Race", "Non-Hispanic Asian/PI",
                       "Non-Hispanic White", "Non-Hispanic Black",
                       "Non-Hispanic AI/AN", "Non-Hispanic Other Race")
)

representation <- acs_pct %>%
  left_join(sample_vars, by = c("variable", "state_residence", "group")) %>%
  mutate(n = replace_na(n, 0L), sample_pct = replace_na(sample_pct, 0),
         diff = sample_pct - acs_pct,
         group = factor(group, levels = unlist(level_list, use.names = FALSE)),
         variable = factor(variable, levels = names(level_list))) %>%
  arrange(variable, state_residence, group)
stopifnot(!anyNA(representation$group))
write_csv(representation, file.path(supp_dir, "figure_s10_sample_vs_acs.csv"))

figure_s10 <- ggplot(representation, aes(x = group, y = diff, fill = diff > 0)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
  scale_fill_manual(
    values = c("TRUE" = "#378ADD", "FALSE" = "#D85A30"),
    labels = c("TRUE" = "Over-represented", "FALSE" = "Under-represented"),
    name = NULL
  ) +
  facet_grid(state_residence ~ variable, scales = "free_x", space = "free_x") +
  labs(
    x = NULL,
    y = "Sample % minus ACS %",
    caption = "Positive = over-represented in sample; negative = under-represented"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    strip.text = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )
ggsave(file.path(supp_dir, "figure_s10_sample_vs_acs.png"), figure_s10,
       width = 12, height = 8, dpi = 300, bg = "white")


#---- Table S7: survey weights by time period and state #----
table_s7 <- read_csv(file.path(analysis_dir, "weight_summary.csv"), show_col_types = FALSE) %>%
  transmute(`Time Period` = wave, State = state_residence, n,
            `Mean Weight` = round(mean, 2), `SD Weight` = round(sd, 2),
            `Min Weight` = round(min, 2), `5% Weight` = round(p05, 2),
            `Median Weight` = round(median, 2), `95% Weight` = round(p95, 2),
            `Max Weight` = round(max, 2), `Imputed n` = n_imputed)
write_csv(table_s7, file.path(supp_dir, "table_s7_weights.csv"))


#---- Table S8: contacts by day of week and on holidays #----
# Contacts on weekday vs weekend contact days and on each holiday, summarised
# the same way as Table S1, with the holiday and weekday/weekend models from
# 01_analysis.R.
day_outcomes <- c(Overall = "overall_total", Physical = "physical_total",
                  `Non-physical` = "non_physical_total")

summarise_contacts <- function(data, group_label) {
  map_dfr(names(day_outcomes), function(o) {
    x <- data[[day_outcomes[[o]]]]
    qs <- svy_quant_safe(x, data$weight, probs = c(0.25, 0.5, 0.75))
    tibble(group = group_label, outcome = o, n = nrow(data),
           mean = round(wtd_mean(x, data$weight), 1),
           sd = round(wtd_sd(x, data$weight), 1),
           median = round(qs[2], 0), IQR = fmt_iqr(qs[1], qs[3], digits = 0))
  })
}

holiday_names <- setdiff(levels(wave1_2$holiday), "Non-holiday")
day_groups <- list(Weekday = filter(wave1_2, day_of_week == "Weekday"),
                   Weekend = filter(wave1_2, day_of_week == "Weekend"),
                   `Non-holiday` = filter(wave1_2, holiday == "Non-holiday"))
for (h in holiday_names) day_groups[[h]] <- filter(wave1_2, holiday == h)
day_summary <- imap_dfr(day_groups, function(data, g) {
  if (nrow(data) == 0) return(tibble(group = g, outcome = names(day_outcomes), n = 0L))
  summarise_contacts(data, g)
})

# holiday models from 01_analysis.R
holiday_models <- read_csv(file.path(analysis_dir, "holiday_models.csv"), show_col_types = FALSE)

weekend_models <- model_results %>%
  filter(predictor == "day_of_week", level == "Weekend") %>%
  transmute(group = "Weekend", outcome, model, crr, lower, upper, p_value, n)

table_s8 <- day_summary %>%
  left_join(holiday_models %>%
              transmute(group = holiday, outcome,
                        crr_ci_vs_non_holiday = sprintf("%.2f (%.2f-%.2f)", crr, lower, upper),
                        p_vs_non_holiday = p_value),
            by = c("group", "outcome")) %>%
  left_join(weekend_models %>%
              filter(model == "Unadjusted") %>%
              transmute(group, outcome,
                        crr_ci_weekend_unadjusted = sprintf("%.2f (%.2f-%.2f)", crr, lower, upper)),
            by = c("group", "outcome")) %>%
  left_join(weekend_models %>%
              filter(model == "Adjusted") %>%
              transmute(group, outcome,
                        crr_ci_weekend_adjusted = sprintf("%.2f (%.2f-%.2f)", crr, lower, upper)),
            by = c("group", "outcome"))
table_s8
write_csv(table_s8, file.path(supp_dir, "table_s8_day_of_week_holidays.csv"))


#---- Figures S13 and S14: elbow plots #----
elbow <- read_csv(file.path(analysis_dir, "elbow_wss.csv"), show_col_types = FALSE)
plot_elbow <- function(stage_name, title) {
  ggplot(filter(elbow, stage == stage_name), aes(k, wss)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = 1:10) +
    labs(x = "Number of clusters k", y = "Total within-cluster sum of squares", title = title) +
    theme_minimal()
}
ggsave(file.path(supp_dir, "figure_s13_elbow_full_sample.png"),
       plot_elbow("Full sample", "Elbow plot (all respondents)"),
       width = 6, height = 4, dpi = 300, bg = "white")
ggsave(file.path(supp_dir, "figure_s14_elbow_low_mobility.png"),
       plot_elbow("Low mobility cluster", "Elbow plot (low mobility cluster)"),
       width = 6, height = 4, dpi = 300, bg = "white")


#---- Save all tables to one workbook #----
write_xlsx(list(Table_S1 = table_s1, Table_S2 = table_s2, Table_S3 = table_s3,
                Table_S4 = table_s4, Table_S5 = table_s5, Table_S7 = table_s7,
                Table_S8 = table_s8, Holiday_models = holiday_models,
                Distance_models = dist_results, Sample_vs_ACS = representation),
           file.path(supp_dir, "supplement_tables.xlsx"))
writeLines(capture.output(sessionInfo()), file.path(supp_dir, "session_info.txt"))
