#---- 02_tables_figures.R #----
# Manuscript tables and figures from the saved analysis (01_analysis.R).
# Contact matrices, Q-indices and Figures 1-3 are made in 03_contact_matrices.R.
#
# Run from the repository root:
#   Rscript scripts/02_tables_figures.R
#
# Outputs (outputs/tables, outputs/figures)
#   Table 1   sample demographics by state (unweighted)
#   Table 2   travel overall and by travel cluster (weighted)
#   Table 3   demographics by travel cluster (unweighted) and cluster tests
#   Figure 4  adjusted contact rate ratios by travel cluster
# Supplemental tables and figures are made in 04_supplemental.R.

#---- Set up #----
library(tidyverse)
library(Hmisc)   # wtd.quantile
library(table1)
library(writexl)

analysis_dir <- file.path("outputs", "analysis")
table_dir <- file.path("outputs", "tables")
figure_dir <- file.path("outputs", "figures")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

wave1_2 <- readRDS(file.path("data", "derived", "analysis_data.rds"))
model_results <- read_csv(file.path(analysis_dir, "regression_results.csv"), show_col_types = FALSE)
cluster_tests <- read_csv(file.path(analysis_dir, "cluster_tests.csv"), show_col_types = FALSE)

cluster_levels <- levels(wave1_2$travel_cluster_merged)

save_table1 <- function(tab, name) {
  write.csv(as.data.frame(tab), file.path(table_dir, paste0(name, ".csv")), row.names = FALSE)
  htmltools::save_html(htmltools::HTML(as.character(tab)), file.path(table_dir, paste0(name, ".html")))
}


#---- Table 1: demographics by state #----
t1 <- wave1_2
label(t1$age) <- "Age"
label(t1$age_group) <- "Age Group"
label(t1$numb_hh_memb) <- "Household Size"
label(t1$hh_size_cat) <- "Household Size"
label(t1$gender) <- "Gender"
label(t1$race_eth_clean) <- "Race/Ethnicity"
label(t1$educ_level_binned) <- "Education Level"
label(t1$hh_income_binned) <- "Household Income"
label(t1$occ) <- "Occupation"
label(t1$work_outside_hh) <- "Work Outside Household"
label(t1$state_residence) <- "State of Residence"
label(t1$gender_binned) <- "Gender"

table_1 <- table1(
  ~ age + age_group + numb_hh_memb + hh_size_cat + gender + race_eth_clean +
    educ_level_binned + hh_income_binned + occ + work_outside_hh | state_residence,
  data = t1, overall = "All States", big.mark = ","
)
save_table1(table_1, "table_1_demographics_by_state")


#---- Table 2: travel overall and by cluster (weighted) #----
# "n (%)" cell: condition is TRUE/FALSE for each respondent (e.g. reported
# work/school travel). n is the unweighted number with TRUE and % is the
# weighted percent, sum of weights with TRUE / sum of all weights
fmt_w_n_pct <- function(w, condition) {
  sprintf("%s (%.1f%%)", format(sum(condition), big.mark = ","),
          100 * sum(w[condition]) / sum(w))
}
# "median (Q1-Q3)" cell: weighted 25th, 50th and 75th percentiles of x (e.g.
# km travelled), using only respondents with a value; non-travelers are NA so
# they are not included
fmt_w_median_iqr <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_character_)
  qs <- wtd.quantile(x[ok], weights = w[ok], probs = c(0.25, 0.5, 0.75), type = "quantile")
  sprintf("%.1f (%.1f-%.1f)", qs[2], qs[1], qs[3])
}

work_vars <- c("work_school_trips_2wk", "reg_trip_sum_km_test", "reg_trip_mean_km_test",
               "work_school_travel_sum7")
other_vars <- c("other_trips_2wk", "other_trip_sum_km_test", "other_trip_mean_km_test",
                "other_travel_nights_sum14")
# Tables 2 and 3 include only respondents who answered both travel
# questions (those with a travel cluster)
clustered <- filter(wave1_2, travel_answered)

# medians are among people who reported travel for that purpose
travel_data <- clustered %>%
  mutate(across(all_of(work_vars), ~ if_else(work_school_travel_bin == 1, .x, NA)),
         across(all_of(other_vars), ~ if_else(other_travel_bin == 1, .x, NA)))

travel_summary <- function(d) {
  tibble(
    section = rep(c("Work/School", "Vacation/Other"), each = 5),
    variable = c("Travel reported, n (%)",
                 "Median (IQR) number of locations travelled to",
                 "Median (IQR) sum distance all trips from home (km)",
                 "Median (IQR) distance from home (km)",
                 "Median (IQR) days per week travelling",
                 "Travel reported, n (%)",
                 "Median (IQR) number of locations travelled to",
                 "Median (IQR) sum distance all trips from home (km)",
                 "Median (IQR) distance from home (km)",
                 "Median (IQR) nights spent away"),
    value = c(fmt_w_n_pct(d$weight, d$work_school_travel_bin == 1),
              fmt_w_median_iqr(d$work_school_trips_2wk, d$weight),
              fmt_w_median_iqr(d$reg_trip_sum_km_test, d$weight),
              fmt_w_median_iqr(d$reg_trip_mean_km_test, d$weight),
              fmt_w_median_iqr(d$work_school_travel_sum7, d$weight),
              fmt_w_n_pct(d$weight, d$other_travel_bin == 1),
              fmt_w_median_iqr(d$other_trips_2wk, d$weight),
              fmt_w_median_iqr(d$other_trip_sum_km_test, d$weight),
              fmt_w_median_iqr(d$other_trip_mean_km_test, d$weight),
              fmt_w_median_iqr(d$other_travel_nights_sum14, d$weight))
  )
}

table_2 <- bind_rows(
  travel_summary(travel_data) %>% mutate(group = "Overall"),
  map_dfr(cluster_levels, ~ travel_summary(filter(travel_data, travel_cluster_merged == .x)) %>%
            mutate(group = .x))
) %>%
  mutate(group = factor(group, levels = c("Overall", cluster_levels))) %>%
  pivot_wider(names_from = group, values_from = value)

# unweighted column Ns
cluster_n <- c(Overall = nrow(clustered), table(clustered$travel_cluster_merged))
names(table_2)[-(1:2)] <- paste0(names(table_2)[-(1:2)], " (N=",
                                 format(cluster_n[names(table_2)[-(1:2)]], big.mark = ",", trim = TRUE), ")")
table_2
write_csv(table_2, file.path(table_dir, "table_2_travel_by_cluster.csv"))


#---- Table 3: demographics by travel cluster (unweighted) #----
table_3 <- table1(
  ~ age + age_group + numb_hh_memb + hh_size_cat + gender_binned + race_eth_clean +
    educ_level_binned + hh_income_binned + occ + work_outside_hh + state_residence |
    travel_cluster_merged,
  data = filter(t1, travel_answered), overall = "Overall", big.mark = ","
)
save_table1(table_3, "table_3_demographics_by_cluster")

# Bonferroni-adjusted p-values for each cluster versus the rest of the sample;
# bold these cells in the manuscript table when p < 0.05. NA = not assessed.
table_3_tests <- cluster_tests %>%
  mutate(cell = case_when(is.na(p_value) ~ "Not assessed",
                          bonferroni_p < 0.001 ~ "<0.001",
                          TRUE ~ sprintf("%.3f", bonferroni_p))) %>%
  select(variable, cluster, cell) %>%
  pivot_wider(names_from = cluster, values_from = cell)
write_csv(table_3_tests, file.path(table_dir, "table_3_cluster_tests.csv"))


#---- Model results, all predictors #----
fmt_crr <- function(crr, lower, upper, reference) {
  if_else(reference, "REF", sprintf("%.2f (%.2f-%.2f)", crr, lower, upper))
}

# all predictors, for reference
regression_all <- model_results %>%
  mutate(crr_ci = fmt_crr(crr, lower, upper, reference),
         p = case_when(reference ~ "", p_value < 0.001 ~ "<0.001", TRUE ~ sprintf("%.3f", p_value))) %>%
  select(outcome, model, predictor, level, crr_ci, p, n)
write_csv(regression_all, file.path(table_dir, "regression_all_predictors.csv"))


#---- Figure 4: adjusted contact rate ratios by travel cluster #----
forest_labels <- c(
  "Subcluster stay at home" = "Stay at home (REF)",
  "Subcluster occasional vacation travel" = "Occasional non-work traveler",
  "Subcluster low-frequency work commuter" = "Low-frequency commuter",
  "Routine local commuters" = "Routine local commuter",
  "Long-duration non-work travelers" = "Long-duration traveler",
  "High-mobility long-distance travelers" = "High-mobility traveler"
)

mv_travel <- model_results %>%
  filter(model == "Adjusted", predictor == "travel_cluster_merged") %>%
  mutate(Level = factor(forest_labels[level], levels = forest_labels),
         Outcome = factor(outcome, levels = c("Overall", "Physical", "Non-physical"))) %>%
  filter(!reference)  # no point for the reference group, its row label is kept below

# sized for a portrait page (6.5 in text width), like the manuscript version
figure_4 <- ggplot(mv_travel,
                   aes(x = crr, y = fct_rev(Level), color = Outcome, shape = Outcome)) +
  geom_point(position = position_dodge(width = 0.4), size = 1.4) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.4), height = 0.2, linewidth = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_continuous(name = "Contact Rate Ratio", breaks = seq(0.75, 2.75, 0.25)) +
  scale_y_discrete(drop = FALSE) +
  expand_limits(x = 0.75) +
  scale_color_manual(name = "Contact Type",
                     values = c("Overall" = "black", "Physical" = "darkgoldenrod3",
                                "Non-physical" = "darkcyan")) +
  scale_shape_manual(name = "Contact Type",
                     values = c("Overall" = 16, "Physical" = 17, "Non-physical" = 15)) +
  ylab("") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(0, 0, 0, 0),
    panel.background = element_rect(fill = "grey96", color = NA),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.6),
    axis.text.y = element_text(size = 10, margin = margin(r = 4)),
    axis.ticks.y = element_line(color = "grey20", linewidth = 0.4),
    plot.margin = margin(5, 10, 5, 5)
  )
ggsave(file.path(figure_dir, "figure_4_forest_plot.png"), figure_4, width = 6.5, height = 7, dpi = 300, bg = "white")
ggsave(file.path(figure_dir, "figure_4_forest_plot.pdf"), figure_4, width = 6.5, height = 7)


#---- Save all tables to one workbook #----
write_xlsx(list(Table_1 = as.data.frame(table_1), Table_2 = table_2,
                Table_3 = as.data.frame(table_3), Table_3_tests = table_3_tests,
                Regression_all = regression_all),
           file.path(table_dir, "manuscript_tables.xlsx"))
