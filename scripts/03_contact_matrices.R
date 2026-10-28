#---- 03_contact_matrices.R #----
# Contact matrix data, age-structured contact matrices, and Q-indices.
#
# Run from the repository root:
#   Rscript scripts/03_contact_matrices.R
#
# Part 1 builds the contact matrix data from data/derived/analysis_data.rds
# (made by 01_analysis.R) in the socialmixr participants/contacts format.
# The exported files contain no survey IDs, exact ages or other demographics,
# so Part 2 can be rerun from data/analysis_data/ alone.
#
# Outputs
#   data/analysis_data/participants.csv  one row per respondent (age group, weight, travel cluster)
#   data/analysis_data/contacts.csv      one row per reported contact (age range, type, setting)
#   outputs/contact_matrices/            matrices (csv, rds) and Q-indices
#   outputs/figures/                     Figures 1-3 and supplementary matrix figures

#---- Set up #----
library(tidyverse)
library(socialmixr)
library(patchwork)
library(viridis)

analysis_file <- file.path("data", "derived", "analysis_data.rds")
contact_dir <- file.path("data", "analysis_data")
out_dir <- file.path("outputs", "contact_matrices")
figure_dir <- file.path("outputs", "figures")
dir.create(contact_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

age_limits <- c(0, 1, 5, 10, 18, 30, 40, 50, 60)

# Contact age bins in the survey and the age range each one covers
age_lookup <- tibble(
  age_bin = c("0_1", "1_4", "5_9", "10_17", "18_29", "30_39", "40_49", "50_59", "60_plus"),
  cnt_age_est_min = c(0, 1, 5, 10, 18, 30, 40, 50, 60),
  cnt_age_est_max = c(1, 4, 9, 17, 29, 39, 49, 59, 99)
)

cluster_levels <- c(
  "Subcluster stay at home",
  "Subcluster occasional vacation travel",
  "Subcluster low-frequency work commuter",
  "Routine local commuters",
  "Long-duration non-work travelers",
  "High-mobility long-distance travelers"
)


#---- Part 1: create contact matrix data #----
# Skipped when the analysis data is not available (e.g. when rerunning from
# the shared contact data only).
if (file.exists(analysis_file)) {
  wave1_2 <- readRDS(analysis_file)

  contact_cols <- wave1_2 %>%
    select(matches("^(any_physical|non_physical)_.+_(home|work|school|other)$")) %>%
    colnames()
  stopifnot(length(contact_cols) == 72)

  # Participants: part_id replaces survey_id; participants are kept at the
  # lower bound of their age group, which is all the matrices use
  wave1_2 <- wave1_2 %>%
    arrange(survey_id) %>%
    mutate(part_id = row_number())

  participants_out <- wave1_2 %>%
    transmute(
      part_id,
      part_age_group = cut(age, breaks = c(age_limits, Inf), right = FALSE),
      part_age = age_limits[as.integer(part_age_group)],
      accidda_wt = weight,
      travel_cluster_merged = as.character(travel_cluster_merged)
    )
  # travel cluster is blank for people who did not answer both travel questions
  stopifnot(!anyNA(select(participants_out, -travel_cluster_merged)))

  # Contacts: one row per reported contact. Missing counts are treated as no
  # contacts, as in the dissertation.
  contacts_out <- wave1_2 %>%
    select(part_id, all_of(contact_cols)) %>%
    pivot_longer(
      cols = all_of(contact_cols),
      names_to = c("phys_type", "age_bin", "setting"),
      names_pattern = "^(any_physical|non_physical)_(0_1|1_4|5_9|10_17|18_29|30_39|40_49|50_59|60_plus)_(home|work|school|other)$",
      values_to = "n_contacts"
    ) %>%
    mutate(n_contacts = as.integer(replace_na(n_contacts, 0))) %>%
    filter(n_contacts > 0) %>%
    left_join(age_lookup, by = "age_bin") %>%
    uncount(weights = n_contacts) %>% # one row per contact
    transmute(
      cont_id = row_number(),
      part_id,
      cnt_age_exact = NA_integer_,
      cnt_age_est_min,
      cnt_age_est_max,
      phys_contact = as.integer(phys_type == "any_physical"),
      cnt_home = as.integer(setting == "home"),
      cnt_work = as.integer(setting == "work"),
      cnt_school = as.integer(setting == "school"),
      cnt_other = as.integer(setting == "other")
    )

  # Check the number of contacts matches the survey totals
  stopifnot(nrow(contacts_out) == sum(select(wave1_2, all_of(contact_cols)), na.rm = TRUE))

  write_csv(participants_out, file.path(contact_dir, "participants.csv"))
  write_csv(contacts_out, file.path(contact_dir, "contacts.csv"))
}


#---- Part 2: contact matrices #----
participants <- read_csv(file.path(contact_dir, "participants.csv"), show_col_types = FALSE) %>%
  mutate(country = "USA")
contacts <- read_csv(file.path(contact_dir, "contacts.csv"), show_col_types = FALSE)
stopifnot(all(contacts$part_id %in% participants$part_id))

accidda <- survey(participants, contacts, reference = NULL)

# socialmixr renamed age.limits to age_limits in newer versions
age_arg <- if ("age_limits" %in% names(formals(contact_matrix))) "age_limits" else "age.limits"

# Mean contacts per participant by participant and contact age group.
# Labels the oldest group "60+" whatever the socialmixr version.
make_cm <- function(svy, weighted = TRUE, filter = NULL) {
  args <- list(svy, filter = filter)
  args[[age_arg]] <- age_limits
  if (weighted) args$weights <- "accidda_wt"
  m <- do.call(contact_matrix, args)$matrix
  dimnames(m) <- lapply(dimnames(m), function(x) sub("\\[60,Inf\\)", "60\\+", x))
  m
}

# Weighted, by contact type (Figure 1)
cm_type <- list(
  "Physical" = make_cm(accidda, filter = list(phys_contact = 1)),
  "Non-physical" = make_cm(accidda, filter = list(phys_contact = 0)),
  "Overall" = make_cm(accidda)
)

# Weighted, by setting (Figure 2, bottom row)
cm_setting <- list(
  "Home" = make_cm(accidda, filter = list(cnt_home = 1)),
  "Work" = make_cm(accidda, filter = list(cnt_work = 1)),
  "School" = make_cm(accidda, filter = list(cnt_school = 1)),
  "Other" = make_cm(accidda, filter = list(cnt_other = 1))
)

# Weighted, by contact type within setting (Figure 2, top two rows)
cm_type_setting <- list()
for (s in c("home", "work", "school", "other")) {
  for (p in c(1, 0)) {
    name <- paste0(str_to_title(s), if (p == 1) " (Physical)" else " (Non-physical)")
    cm_type_setting[[name]] <- make_cm(accidda, filter = setNames(list(p, 1), c("phys_contact", paste0("cnt_", s))))
  }
}

# Unweighted, by contact type, by setting, and by contact type within setting (supplement)
cm_type_unwtd <- list(
  "Physical" = make_cm(accidda, weighted = FALSE, filter = list(phys_contact = 1)),
  "Non-physical" = make_cm(accidda, weighted = FALSE, filter = list(phys_contact = 0)),
  "Overall" = make_cm(accidda, weighted = FALSE)
)
cm_setting_unwtd <- list(
  "Home" = make_cm(accidda, weighted = FALSE, filter = list(cnt_home = 1)),
  "Work" = make_cm(accidda, weighted = FALSE, filter = list(cnt_work = 1)),
  "School" = make_cm(accidda, weighted = FALSE, filter = list(cnt_school = 1)),
  "Other" = make_cm(accidda, weighted = FALSE, filter = list(cnt_other = 1))
)
cm_type_setting_unwtd <- list()
for (s in c("home", "work", "school", "other")) {
  for (p in c(1, 0)) {
    name <- paste0(str_to_title(s), if (p == 1) " (Physical)" else " (Non-physical)")
    cm_type_setting_unwtd[[name]] <- make_cm(accidda, weighted = FALSE,
                                             filter = setNames(list(p, 1), c("phys_contact", paste0("cnt_", s))))
  }
}

# Weighted, by travel cluster (Figure 3). Each cluster is its own survey so
# the denominator is that cluster's participants only.
cm_cluster <- list()
for (cl in cluster_levels) {
  part_sub <- filter(participants, travel_cluster_merged == cl)
  cont_sub <- filter(contacts, part_id %in% part_sub$part_id)
  cm_cluster[[cl]] <- make_cm(survey(part_sub, cont_sub, reference = NULL))
}
cluster_n <- count(participants, travel_cluster_merged)


#---- Q-index #----
# Iozzi Q-index: (trace(P) - 1) / (n - 1), where P is the row-normalised
# contact matrix. Participant age groups with no contacts (all participants
# are adults, so the child rows) are dropped from both axes.
q_index_iozzi_square <- function(M) {
  M <- as.matrix(M)
  M[is.na(M)] <- 0

  common <- intersect(rownames(M), colnames(M))
  M_sq <- M[common, common, drop = FALSE]

  # drop rows with zero totals (can happen in school/work filtered matrices)
  keep <- rowSums(M_sq) > 0
  M_sq <- M_sq[keep, keep, drop = FALSE]
  n <- nrow(M_sq)
  if (n < 2) return(NA_real_)

  P <- sweep(M_sq, 1, rowSums(M_sq), "/")
  P[is.na(P)] <- 0
  (sum(diag(P)) - 1) / (n - 1)
}

q_index <- bind_rows(
  tibble(group = "Contact type", matrix = names(cm_type), q_index = map_dbl(cm_type, q_index_iozzi_square)),
  tibble(group = "Setting", matrix = names(cm_setting), q_index = map_dbl(cm_setting, q_index_iozzi_square)),
  tibble(group = "Travel cluster", matrix = names(cm_cluster), q_index = map_dbl(cm_cluster, q_index_iozzi_square))
)
write_csv(q_index, file.path(out_dir, "q_index.csv"))


#---- Save matrices #----
matrices <- list(
  type = cm_type, setting = cm_setting, type_setting = cm_type_setting,
  type_unweighted = cm_type_unwtd, setting_unweighted = cm_setting_unwtd,
  type_setting_unweighted = cm_type_setting_unwtd,
  cluster = cm_cluster
)
saveRDS(matrices, file.path(out_dir, "contact_matrices.rds"))

matrices_long <- imap_dfr(matrices, function(group, group_name) {
  imap_dfr(group, function(m, name) {
    as_tibble(as.table(m), .name_repair = ~ c("participant_age_group", "contact_age_group", "mean_contacts")) %>%
      mutate(group = group_name, matrix = name, .before = 1)
  })
})
write_csv(matrices_long, file.path(out_dir, "contact_matrices.csv"))


#---- Plots #----
# Heatmap of one matrix with a shared fill scale
plot_cm <- function(m, title, fill_limits, label_size = 3) {
  df <- reshape2::melt(m, varnames = c("age.group", "age.group.contact"), value.name = "contacts")
  df$age.group <- factor(df$age.group, levels = rownames(m))
  df$age.group.contact <- factor(df$age.group.contact, levels = colnames(m))

  ggplot(df, aes(x = age.group, y = age.group.contact, fill = contacts)) +
    geom_tile() +
    geom_text(aes(label = ifelse(is.na(contacts), "", sprintf("%.2f", contacts))),
              color = "black", size = label_size) +
    scale_fill_viridis(limits = fill_limits, oob = scales::squish, option = "viridis",
                       direction = -1, na.value = "grey95") +
    labs(x = "Participant age group", y = "Contact age group",
         fill = "Mean contacts", title = title) +
    theme(legend.position = "bottom")
}

# All panels in a figure share one colour scale
plot_cm_grid <- function(cms, ncol, titles = names(cms), label_size = 3) {
  fill_limits <- c(0, max(unlist(cms), na.rm = TRUE))
  plots <- map2(cms, titles, plot_cm, fill_limits = fill_limits, label_size = label_size)
  # wider colour bar so the legend labels don't overlap
  wrap_plots(plots, ncol = ncol, guides = "collect") &
    theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))
}

figure_1 <- plot_cm_grid(cm_type, ncol = 3,
  titles = c("Physical Contact Rates", "Non-physical Contact Rates", "Overall Contact Rates"))
ggsave(file.path(figure_dir, "figure_1_matrices_by_type.png"), figure_1, width = 16, height = 6, dpi = 300, bg = "white")

# Figure 2: one column per setting; rows are physical, non-physical, and
# overall (physical + non-physical) contacts.
# Sized to fill a landscape letter page with 1 inch margins (9 x 6.5 in), so
# text is set smaller and the axis titles are shared across panels.
figure_2 <- plot_cm_grid(
  c(cm_type_setting[str_detect(names(cm_type_setting), "\\(Physical\\)")],
    cm_type_setting[str_detect(names(cm_type_setting), "Non-physical")],
    setNames(cm_setting, paste(names(cm_setting), "(Overall)"))),
  ncol = 4, label_size = 1.6) +
  plot_layout(axis_titles = "collect") &
  theme(plot.title = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 5.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.25, "cm"),
        plot.margin = margin(2, 2, 2, 2)) &
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
ggsave(file.path(figure_dir, "figure_2_matrices_by_type_and_setting.png"), figure_2,
       width = 9, height = 6.5, dpi = 300, bg = "white")

cluster_titles <- paste0(cluster_levels, " (n = ",
                         cluster_n$n[match(cluster_levels, cluster_n$travel_cluster_merged)], ")")
figure_3 <- plot_cm_grid(cm_cluster, ncol = 2, titles = cluster_titles) &
  theme_minimal(base_size = 11) &
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(figure_dir, "figure_3_matrices_by_cluster.png"), figure_3, width = 11, height = 15, dpi = 300, bg = "white")

# Supplement: unweighted matrices
figure_type_unwtd <- plot_cm_grid(cm_type_unwtd, ncol = 3,
  titles = c("Physical Unweighted Contacts", "Non-physical Unweighted Contacts", "Overall Unweighted Contacts"))
ggsave(file.path(figure_dir, "figure_s_unweighted_matrices_by_type.png"), figure_type_unwtd,
       width = 16, height = 6, dpi = 300, bg = "white")

# Same layout as Figure 2
figure_type_setting_unwtd <- plot_cm_grid(
  c(cm_type_setting_unwtd[str_detect(names(cm_type_setting_unwtd), "\\(Physical\\)")],
    cm_type_setting_unwtd[str_detect(names(cm_type_setting_unwtd), "Non-physical")],
    setNames(cm_setting_unwtd, paste(names(cm_setting_unwtd), "(Overall)"))),
  ncol = 4)
ggsave(file.path(figure_dir, "figure_s_unweighted_matrices_by_type_and_setting.png"), figure_type_setting_unwtd,
       width = 20, height = 16, dpi = 300, bg = "white")

writeLines(capture.output(sessionInfo()), file.path(out_dir, "session_info.txt"))
