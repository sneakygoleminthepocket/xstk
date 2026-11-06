# Internet Advertisements - Probability and Statistics project
# Base R only. Run this script from the project root with:
#   Rscript R/analysis.R
# Optional arguments:
#   Rscript R/analysis.R path/to/data.csv path/to/project-root

args <- commandArgs(trailingOnly = TRUE)
input_file <- if (length(args) >= 1) args[1] else file.path("data", "add.csv")
project_root <- if (length(args) >= 2) args[2] else "."
out_dir <- file.path(project_root, "outputs")
fig_dir <- file.path(project_root, "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop(paste0("Cannot find CSV file: ", input_file,
              "\nRun this script from the project root or pass the data path as the first argument."))
}

# Read the CSV as text so that missing-value markers and the export header
# can be checked before converting the analysis variables to numbers.
raw <- read.csv(input_file, header = FALSE, colClasses = "character",
                na.strings = character(0), check.names = FALSE,
                strip.white = FALSE, blank.lines.skip = FALSE, fill = TRUE)

# The supplied Kaggle export has a numeric pseudo-header and a row index.
# Remove them only when their sequences match the expected patterns.
if (nrow(raw) > 1 && ncol(raw) > 1000) {
  first_row <- suppressWarnings(as.numeric(trimws(as.character(unlist(raw[1, ], use.names = FALSE)))))
  if (length(first_row) == ncol(raw) && all(!is.na(first_row[-1])) &&
      all(first_row[-1] == seq.int(0, ncol(raw) - 2))) {
    raw <- raw[-1, , drop = FALSE]
  }
}

first_col <- suppressWarnings(as.numeric(trimws(as.character(raw[[1]]))))
if (length(first_col) == nrow(raw) && all(!is.na(first_col)) &&
    all(first_col == seq.int(0, nrow(raw) - 1))) {
  raw <- raw[, -1, drop = FALSE]
}

raw[] <- lapply(raw, function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("?", "")] <- NA_character_
  x
})
if (ncol(raw) < 5) stop("The cleaned CSV has fewer than five columns; check its format.")

class_raw <- tolower(trimws(raw[[ncol(raw)]]))
raw_label_counts <- as.data.frame(table(class_raw, useNA = "ifany"), stringsAsFactors = FALSE)
names(raw_label_counts) <- c("raw_label", "count")
write.csv(raw_label_counts, file.path(out_dir, "raw_class_label_counts.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

class_clean <- ifelse(class_raw %in% c("ad", "ad."), "ad",
                      ifelse(class_raw %in% c("nonad", "nonad."), "nonad", NA_character_))
valid_class <- !is.na(class_clean)
raw <- raw[valid_class, , drop = FALSE]
class_clean <- class_clean[valid_class]

num <- function(x) suppressWarnings(as.numeric(x))
analysis <- data.frame(
  height = num(raw[[1]]),
  width = num(raw[[2]]),
  aspect_ratio = num(raw[[3]]),
  local = num(raw[[4]]),
  class = factor(class_clean, levels = c("nonad", "ad")),
  stringsAsFactors = FALSE
)

log_positive <- function(x) {
  result <- rep(NA_real_, length(x))
  valid <- !is.na(x) & is.finite(x) & x > 0
  result[valid] <- log(x[valid])
  result
}
analysis$log_height <- log_positive(analysis$height)
analysis$log_width <- log_positive(analysis$width)

fmt_num <- function(x, digits = 3) {
  if (length(x) == 0) return(character(0))
  result <- rep("NA", length(x))
  valid <- !is.na(x) & is.finite(x)
  if (any(valid)) {
    result[valid] <- formatC(x[valid], format = "f", digits = digits, big.mark = ",")
  }
  result
}

fmt_p <- function(p) {
  if (length(p) == 0 || is.na(p)) return("NA")
  if (p < 0.001) return("$<0.001$")
  formatC(p, format = "f", digits = 3)
}

write_tabular <- function(path, header, rows, align) {
  lines <- c(paste0("\\begin{tabular}{", align, "}"), "\\toprule",
             paste(header, collapse = " & "), "\\\\", "\\midrule")
  if (length(rows)) {
    for (row in rows) lines <- c(lines, paste(row, collapse = " & "), "\\\\")
  }
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  writeLines(lines, path, useBytes = TRUE)
}

# Preserve all supplied rows for the primary analysis. Exact repeated feature
# rows are examined separately as a sensitivity check in the report.
duplicate_frame <- raw
duplicate_frame[[ncol(duplicate_frame)]] <- as.character(analysis$class)
duplicate_rows <- sum(duplicated(duplicate_frame))
keep_unique <- !duplicated(duplicate_frame)
unique_analysis <- analysis[keep_unique, , drop = FALSE]

# 1) Class counts (the report uses this table, not a second class-count plot).
class_counts <- as.data.frame(table(analysis$class), stringsAsFactors = FALSE)
names(class_counts) <- c("class", "count")
class_counts$percentage <- 100 * class_counts$count / sum(class_counts$count)
write.csv(class_counts, file.path(out_dir, "class_counts.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
class_rows <- lapply(seq_len(nrow(class_counts)), function(i) {
  c(as.character(class_counts$class[i]), as.character(class_counts$count[i]),
    paste0(fmt_num(class_counts$percentage[i], 2), "\\%"))
})
write_tabular(file.path(out_dir, "class_counts_table.tex"),
              c("Class", "Count", "Percentage"), class_rows, "lrr")

# 2) Descriptive summaries for height and width only.
selected_vars <- c("height", "width")
pretty_vars <- c(height = "Height (pixels)", width = "Width (pixels)")
desc_records <- list()
desc_rows <- list()
for (v in selected_vars) {
  for (cl in c("ad", "nonad")) {
    x <- analysis[[v]][analysis$class == cl]
    x <- x[!is.na(x) & is.finite(x)]
    q <- if (length(x)) as.numeric(quantile(x, c(0.25, 0.75), names = FALSE)) else c(NA_real_, NA_real_)
    rec <- data.frame(variable = v, class = cl, n = length(x),
                      mean = if (length(x)) mean(x) else NA_real_,
                      sd = if (length(x) > 1) sd(x) else NA_real_,
                      median = if (length(x)) median(x) else NA_real_,
                      q1 = q[1], q3 = q[2])
    desc_records[[length(desc_records) + 1]] <- rec
    desc_rows[[length(desc_rows) + 1]] <- c(
      pretty_vars[[v]], cl, as.character(length(x)), fmt_num(rec$mean, 2),
      fmt_num(rec$sd, 2), fmt_num(rec$median, 2),
      paste0(fmt_num(q[1], 2), "--", fmt_num(q[2], 2))
    )
  }
}
desc_table <- do.call(rbind, desc_records)
write.csv(desc_table, file.path(out_dir, "descriptive_summary.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write_tabular(file.path(out_dir, "descriptive_table.tex"),
              c("Variable", "Class", "$n$", "Mean", "SD", "Median", "IQR ($Q_1$--$Q_3$)"),
              desc_rows, "llrrrrr")

# 3) Height missingness by class and its Pearson chi-square test.
height_missing_rows <- list()
for (cl in c("ad", "nonad")) {
  values <- analysis$height[analysis$class == cl]
  n <- length(values)
  missing_n <- sum(is.na(values))
  height_missing_rows[[length(height_missing_rows) + 1]] <- data.frame(
    class = cl, total_n = n, missing_n = missing_n,
    missing_percent = 100 * missing_n / n
  )
}
height_missing <- do.call(rbind, height_missing_rows)
write.csv(height_missing, file.path(out_dir, "height_missingness_by_class.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

missing_table <- table(analysis$class, is.na(analysis$height))
# Use the uncorrected Pearson statistic; correct = FALSE disables R's default Yates correction.
missing_chi <- chisq.test(missing_table, correct = FALSE)
missing_chi_result <- data.frame(
  test = "Class by height missingness",
  statistic = unname(missing_chi$statistic),
  df = unname(missing_chi$parameter),
  p_value = missing_chi$p.value,
  ad_missing_percent = height_missing$missing_percent[height_missing$class == "ad"],
  nonad_missing_percent = height_missing$missing_percent[height_missing$class == "nonad"],
  difference_percentage_points = height_missing$missing_percent[height_missing$class == "ad"] -
    height_missing$missing_percent[height_missing$class == "nonad"]
)
write.csv(missing_chi_result, file.path(out_dir, "chi_square_results.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# Observed 2x2 table for the report (missing and observed height by class).
missingness_table_rows <- list(
  c("Advertisement", as.character(missing_table["ad", "TRUE"]),
    as.character(missing_table["ad", "FALSE"]),
    as.character(sum(missing_table["ad", ]))),
  c("Non-advertisement", as.character(missing_table["nonad", "TRUE"]),
    as.character(missing_table["nonad", "FALSE"]),
    as.character(sum(missing_table["nonad", ])))
)
write_tabular(file.path(out_dir, "height_missingness_table.tex"),
              c("Class", "Height missing", "Height observed", "Total"),
              missingness_table_rows, "lrrr")
expected_counts_all_above_100 <- all(missing_chi$expected > 100)

# 4) Welch tests for mean log-height and mean log-width.
welch_records <- list()
welch_rows <- list()
for (v in c("log_height", "log_width")) {
  x <- analysis[[v]][analysis$class == "ad"]
  y <- analysis[[v]][analysis$class == "nonad"]
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  test <- t.test(x, y, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
  difference <- unname(test$estimate[1] - test$estimate[2])
  ratio <- exp(difference)
  ratio_low <- exp(unname(test$conf.int[1]))
  ratio_high <- exp(unname(test$conf.int[2]))
  label <- if (v == "log_height") "Log height" else "Log width"
  welch_records[[length(welch_records) + 1]] <- data.frame(
    variable = v, n_ad = length(x), n_nonad = length(y),
    t = unname(test$statistic), df = unname(test$parameter),
    p_value = test$p.value, log_mean_difference = difference,
    ci_lower = unname(test$conf.int[1]), ci_upper = unname(test$conf.int[2]),
    geometric_mean_ratio = ratio, ratio_ci_lower = ratio_low,
    ratio_ci_upper = ratio_high
  )
  welch_rows[[length(welch_rows) + 1]] <- c(
    label, paste0(length(x), " / ", length(y)),
    fmt_num(unname(test$statistic), 3), fmt_num(unname(test$parameter), 1),
    paste0(fmt_num(ratio, 3), " [", fmt_num(ratio_low, 3), ", ",
           fmt_num(ratio_high, 3), "]"), fmt_p(test$p.value)
  )
}
welch_results <- do.call(rbind, welch_records)
write.csv(welch_results, file.path(out_dir, "welch_results.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write_tabular(file.path(out_dir, "welch_test_table.tex"),
              c("Variable", "$n$ (ad / non-ad)", "$t$", "$df$",
                "Geometric-mean ratio (95\\% CI)", "$p$"),
              welch_rows, "lrrrll")

# 5) The single variance comparison is the classical F-test for log-height.
ad_log_height <- analysis$log_height[analysis$class == "ad"]
nonad_log_height <- analysis$log_height[analysis$class == "nonad"]
ad_log_height <- ad_log_height[is.finite(ad_log_height)]
nonad_log_height <- nonad_log_height[is.finite(nonad_log_height)]
height_f_test <- var.test(ad_log_height, nonad_log_height,
                          alternative = "two.sided", conf.level = 0.95)
height_f_result <- data.frame(
  variable = "log_height",
  n_ad = length(ad_log_height), n_nonad = length(nonad_log_height),
  F_ratio = unname(height_f_test$statistic),
  df1 = unname(height_f_test$parameter[1]),
  df2 = unname(height_f_test$parameter[2]),
  sd_ad = sd(ad_log_height), sd_nonad = sd(nonad_log_height),
  p_value = height_f_test$p.value
)
write.csv(height_f_result, file.path(out_dir, "height_f_test.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
f_row <- list(c(
  "Log height", paste0(length(ad_log_height), " / ", length(nonad_log_height)),
  paste0(unname(height_f_test$parameter[1]), " / ", unname(height_f_test$parameter[2])),
  fmt_num(unname(height_f_test$statistic), 3),
  paste0(fmt_num(sd(ad_log_height), 3), " / ", fmt_num(sd(nonad_log_height), 3)),
  fmt_p(height_f_test$p.value)
))
write_tabular(file.path(out_dir, "f_test_table.tex"),
              c("Variable", "$n$ (ad / non-ad)", "$df$ (ad / non-ad)",
                "$F=s^2_{ad}/s^2_{nonad}$", "SD (ad / non-ad)", "$p$"),
              f_row, "lrrrll")

# 6) Additional checks for the log-height variance comparison.
# The Fligner-Killeen test is rank-based and more resistant to non-normality.
height_fligner_test <- fligner.test(list(ad_log_height, nonad_log_height))

# Bootstrap the variance ratio by resampling within each class. The fixed seed
# makes the percentile interval reproducible on the same R version.
bootstrap_reps <- 10000L
bootstrap_seed <- 20261105L
set.seed(bootstrap_seed)
bootstrap_variance_ratios <- numeric(bootstrap_reps)
for (b in seq_len(bootstrap_reps)) {
  ad_boot <- sample(ad_log_height, size = length(ad_log_height), replace = TRUE)
  nonad_boot <- sample(nonad_log_height, size = length(nonad_log_height), replace = TRUE)
  bootstrap_variance_ratios[b] <- var(ad_boot) / var(nonad_boot)
}
bootstrap_variance_ci <- quantile(bootstrap_variance_ratios,
                                  probs = c(0.025, 0.975),
                                  na.rm = TRUE, names = FALSE, type = 7)

variance_robustness_result <- data.frame(
  F_ratio = unname(height_f_test$statistic),
  F_p_value = height_f_test$p.value,
  fligner_statistic = unname(height_fligner_test$statistic),
  fligner_df = unname(height_fligner_test$parameter),
  fligner_p_value = height_fligner_test$p.value,
  bootstrap_lower = unname(bootstrap_variance_ci[1]),
  bootstrap_upper = unname(bootstrap_variance_ci[2]),
  bootstrap_repetitions = bootstrap_reps,
  bootstrap_seed = bootstrap_seed
)
write.csv(variance_robustness_result,
          file.path(out_dir, "variance_robustness_results.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

variance_robustness_rows <- list(
  c("Classical F-test", paste0("$F = ", fmt_num(height_f_result$F_ratio, 3), "$"),
    fmt_p(height_f_result$p_value)),
  c("Fligner--Killeen", paste0("$\\chi^2 = ",
    fmt_num(unname(height_fligner_test$statistic), 2), "$"),
    fmt_p(height_fligner_test$p.value)),
  c("Bootstrap percentile CI",
    paste0("[", fmt_num(bootstrap_variance_ci[1], 2), ", ",
           fmt_num(bootstrap_variance_ci[2], 2), "]"),
    paste0(bootstrap_reps, " resamples"))
)
write_tabular(file.path(out_dir, "variance_robustness_table.tex"),
              c("Procedure", "Result", "$p$-value / details"),
              variance_robustness_rows, "lll")

# 7) Exact repeated-row sensitivity for the same height comparisons.
unique_ad_height <- unique_analysis$log_height[unique_analysis$class == "ad"]
unique_nonad_height <- unique_analysis$log_height[unique_analysis$class == "nonad"]
unique_ad_width <- unique_analysis$log_width[unique_analysis$class == "ad"]
unique_nonad_width <- unique_analysis$log_width[unique_analysis$class == "nonad"]
unique_ad_height <- unique_ad_height[is.finite(unique_ad_height)]
unique_nonad_height <- unique_nonad_height[is.finite(unique_nonad_height)]
unique_ad_width <- unique_ad_width[is.finite(unique_ad_width)]
unique_nonad_width <- unique_nonad_width[is.finite(unique_nonad_width)]
unique_height_t <- t.test(unique_ad_height, unique_nonad_height,
                          alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
unique_width_t <- t.test(unique_ad_width, unique_nonad_width,
                         alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
unique_height_f <- var.test(unique_ad_height, unique_nonad_height,
                            alternative = "two.sided", conf.level = 0.95)
all_height_difference <- unname(welch_results$log_mean_difference[welch_results$variable == "log_height"])
all_width_difference <- unname(welch_results$log_mean_difference[welch_results$variable == "log_width"])
all_height_ratio <- exp(all_height_difference)
all_width_ratio <- exp(all_width_difference)
unique_height_ratio <- exp(unname(unique_height_t$estimate[1] - unique_height_t$estimate[2]))
unique_width_ratio <- exp(unname(unique_width_t$estimate[1] - unique_width_t$estimate[2]))
unique_height_ratio_ci <- exp(unname(unique_height_t$conf.int))
unique_width_ratio_ci <- exp(unname(unique_width_t$conf.int))
all_height_ratio_ci <- as.numeric(unlist(welch_results[welch_results$variable == "log_height",
                                                        c("ratio_ci_lower", "ratio_ci_upper")],
                                                 use.names = FALSE))
all_width_ratio_ci <- as.numeric(unlist(welch_results[welch_results$variable == "log_width",
                                                       c("ratio_ci_lower", "ratio_ci_upper")],
                                                use.names = FALSE))
duplicate_sensitivity <- data.frame(
  total_rows = nrow(analysis), repeated_rows_beyond_first = duplicate_rows,
  repeated_row_percent = 100 * duplicate_rows / nrow(analysis),
  unique_full_rows = nrow(unique_analysis),
  height_geometric_mean_ratio_all = all_height_ratio,
  height_geometric_mean_ratio_unique = unique_height_ratio,
  width_geometric_mean_ratio_all = all_width_ratio,
  width_geometric_mean_ratio_all_ci_lower = all_width_ratio_ci[1],
  width_geometric_mean_ratio_all_ci_upper = all_width_ratio_ci[2],
  width_geometric_mean_ratio_unique = unique_width_ratio,
  width_geometric_mean_ratio_unique_ci_lower = unique_width_ratio_ci[1],
  width_geometric_mean_ratio_unique_ci_upper = unique_width_ratio_ci[2],
  height_geometric_mean_ratio_all_ci_lower = all_height_ratio_ci[1],
  height_geometric_mean_ratio_all_ci_upper = all_height_ratio_ci[2],
  height_geometric_mean_ratio_unique_ci_lower = unique_height_ratio_ci[1],
  height_geometric_mean_ratio_unique_ci_upper = unique_height_ratio_ci[2],
  height_F_ratio_all = unname(height_f_test$statistic),
  height_F_ratio_unique = unname(unique_height_f$statistic),
  unique_height_welch_p = unique_height_t$p.value,
  unique_width_welch_p = unique_width_t$p.value,
  unique_height_F_p = unique_height_f$p.value
)
write.csv(duplicate_sensitivity, file.path(out_dir, "duplicate_sensitivity.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

ratio_ci_text <- function(estimate, lower, upper, digits = 2) {
  paste0(fmt_num(estimate, digits), " [", fmt_num(lower, digits), ", ",
         fmt_num(upper, digits), "]")
}
duplicate_sensitivity_rows <- list(
  c("Height geometric-mean ratio",
    ratio_ci_text(all_height_ratio, all_height_ratio_ci[1], all_height_ratio_ci[2]),
    ratio_ci_text(unique_height_ratio, unique_height_ratio_ci[1], unique_height_ratio_ci[2])),
  c("Width geometric-mean ratio",
    ratio_ci_text(all_width_ratio, all_width_ratio_ci[1], all_width_ratio_ci[2]),
    ratio_ci_text(unique_width_ratio, unique_width_ratio_ci[1], unique_width_ratio_ci[2])),
  c("Log-height F ratio",
    fmt_num(unname(height_f_test$statistic), 3),
    fmt_num(unname(unique_height_f$statistic), 3))
)
write_tabular(file.path(out_dir, "duplicate_sensitivity_table.tex"),
              c("Estimate", "All supplied rows", "One copy per exact row"),
              duplicate_sensitivity_rows, "lll")

# 8) Most common complete width-height pairs. The report summarizes the
# leading advertisement size in prose instead of reproducing a long table.
size_records <- list()
for (cl in c("ad", "nonad")) {
  sub <- analysis[analysis$class == cl & !is.na(analysis$width) &
                    !is.na(analysis$height), , drop = FALSE]
  counts <- aggregate(rep(1, nrow(sub)),
                      by = list(width = sub$width, height = sub$height), FUN = sum)
  names(counts)[3] <- "count"
  counts <- counts[order(-counts$count, counts$width, counts$height), , drop = FALSE]
  counts$percentage <- 100 * counts$count / nrow(sub)
  counts$class <- cl
  counts <- counts[seq_len(min(3, nrow(counts))),
                   c("class", "width", "height", "count", "percentage")]
  size_records[[length(size_records) + 1]] <- counts
}
top_sizes <- do.call(rbind, size_records)
write.csv(top_sizes, file.path(out_dir, "top_image_sizes.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# Values used in the report's banner-size description.
ad_heights <- analysis$height[analysis$class == "ad"]
ad_complete <- analysis[analysis$class == "ad" &
                          !is.na(analysis$height) & !is.na(analysis$width), , drop = FALSE]
ad_height_60_count <- sum(ad_heights == 60, na.rm = TRUE)
ad_banner_468_60_count <- sum(ad_complete$width == 468 & ad_complete$height == 60)

# 9) Figures. Each legend is given its own space; titles and labels do not overlap.

# Height missingness by class. The single series needs no legend.
missing_percent <- c(
  100 * mean(is.na(analysis$height[analysis$class == "ad"])),
  100 * mean(is.na(analysis$height[analysis$class == "nonad"]))
)
png(file.path(fig_dir, "missingness_by_class.png"), width = 1500, height = 900, res = 150)
par(mar = c(5.2, 5.1, 4.2, 1.5), mgp = c(2.6, 0.8, 0),
    cex.axis = 0.9, cex.lab = 0.95)
missing_midpoints <- barplot(
  missing_percent,
  names.arg = c("Advertisement", "Non-advertisement"),
  ylim = c(0, max(missing_percent) + 8),
  main = "Missing height by class",
  xlab = "Class", ylab = "Missing height (%)",
  col = c("tomato3", "steelblue3"), border = NA,
  cex.names = 0.9
)
text(missing_midpoints, missing_percent + 1.4,
     labels = paste0(fmt_num(missing_percent, 1), "%"), cex = 1)
dev.off()

# Log-scale boxplots. The ad log-height box can collapse to a line because
# more than half of the recorded advertisement heights equal 60 pixels.
ad_height_quartiles <- as.numeric(quantile(ad_log_height,
                                           probs = c(0.25, 0.50, 0.75),
                                           names = FALSE))
boxplot_note <- paste0(
  "Ad log-height: Q1 = median = Q3 = ",
  fmt_num(ad_height_quartiles[1], 3),
  " (IQR = 0); the box therefore appears as a line."
)
png(file.path(fig_dir, "log_geometry_boxplots.png"), width = 1800, height = 1000, res = 150)
par(mfrow = c(1, 2), mar = c(5, 4.5, 3.8, 1), oma = c(2.8, 0, 0, 0),
    mgp = c(2.5, 0.75, 0), cex.axis = 0.9, cex.lab = 0.95)
box_colors <- c("nonad" = "steelblue2", "ad" = "tomato2")
boxplot(log_height ~ class, data = analysis, main = "Log(height)",
        xlab = "Class", ylab = "Natural log",
        col = unname(box_colors[levels(analysis$class)]), outline = TRUE,
        outpch = 16, outcex = 0.42,
        outcol = adjustcolor("gray20", alpha.f = 0.6),
        boxlwd = 1.2, medlwd = 2.4, whisklwd = 1.1)
boxplot(log_width ~ class, data = analysis, main = "Log(width)",
        xlab = "Class", ylab = "Natural log",
        col = unname(box_colors[levels(analysis$class)]), outline = TRUE,
        outpch = 16, outcex = 0.42,
        outcol = adjustcolor("gray20", alpha.f = 0.6),
        boxlwd = 1.2, medlwd = 2.4, whisklwd = 1.1)
mtext(boxplot_note, side = 1, outer = TRUE, line = 0.7, cex = 0.78)
par(mfrow = c(1, 1))
dev.off()

# Raw-scale histograms emphasize repeated dimensions such as 60-pixel heights
# and 468-pixel widths. The shared legend occupies a separate panel.
png(file.path(fig_dir, "geometry_histograms.png"), width = 1900, height = 900, res = 150)
layout(matrix(c(1, 2, 3), nrow = 1), widths = c(1, 1, 0.22))
plot_histogram_pair <- function(variable, title, bin_width) {
  values <- analysis[[variable]]
  ad_values <- values[analysis$class == "ad"]
  nonad_values <- values[analysis$class == "nonad"]
  ad_values <- ad_values[is.finite(ad_values)]
  nonad_values <- nonad_values[is.finite(nonad_values)]
  max_value <- ceiling(max(values, na.rm = TRUE) / bin_width) * bin_width + bin_width
  breaks <- seq(0, max_value, by = bin_width)
  ad_hist <- hist(ad_values, breaks = breaks, plot = FALSE, probability = TRUE)
  nonad_hist <- hist(nonad_values, breaks = breaks, plot = FALSE, probability = TRUE)
  y_max <- max(c(ad_hist$density, nonad_hist$density), na.rm = TRUE) * 1.12
  hist(nonad_values, breaks = breaks, probability = TRUE,
       col = adjustcolor("steelblue3", 0.48), border = "white",
       ylim = c(0, y_max), main = title, xlab = "Pixels",
       ylab = "Density", cex.main = 1.24)
  hist(ad_values, breaks = breaks, probability = TRUE,
       col = adjustcolor("tomato3", 0.48), border = "white", add = TRUE)
}
par(mar = c(5, 4.7, 4, 1), mgp = c(2.5, 0.75, 0),
    cex.axis = 1.22, cex.lab = 1.20)
plot_histogram_pair("height", "Recorded image height", 10)
plot_histogram_pair("width", "Recorded image width", 20)
par(mar = rep(0, 4))
plot.new()
legend("center", legend = c("Non-advertisement", "Advertisement"),
       fill = c("steelblue3", "tomato3"), bty = "n", cex = 0.88)
layout(1)
dev.off()

# Optional descriptive scatter plot. No correlation coefficient or test is reported.
scatter_data <- analysis[is.finite(analysis$log_height) &
                           is.finite(analysis$log_width), , drop = FALSE]
scatter_colors <- ifelse(scatter_data$class == "ad",
                         adjustcolor("tomato3", 0.55),
                         adjustcolor("steelblue3", 0.45))
png(file.path(fig_dir, "log_width_height_scatter.png"), width = 1700, height = 1100, res = 150)
layout(matrix(c(1, 2), nrow = 1), widths = c(0.84, 0.16))
par(mar = c(5, 5, 4, 1), mgp = c(2.6, 0.8, 0),
    cex.axis = 1.08, cex.lab = 1.12)
plot(scatter_data$log_width, scatter_data$log_height,
     pch = 16, cex = 0.6, col = scatter_colors,
     xlab = "Log width", ylab = "Log height",
     main = "Log-height versus log-width", cex.main = 1.18)
par(mar = rep(0, 4))
plot.new()
legend("center", legend = c("Advertisement", "Non-advertisement"),
       pch = 16, col = c("tomato3", "steelblue3"), bty = "n", cex = 0.9)
layout(1)
dev.off()

# 10) Audit notes for the report and for reproducibility.
height_missing_ad <- sum(is.na(analysis$height) & analysis$class == "ad")
height_missing_nonad <- sum(is.na(analysis$height) & analysis$class == "nonad")
audit_lines <- c(
  "Internet Advertisements - analysis audit",
  paste0("Input file: ", input_file),
  paste0("Rows after cleaning: ", nrow(analysis)),
  paste0("Class counts: ad ", sum(analysis$class == "ad"),
         "; nonad ", sum(analysis$class == "nonad")),
  paste0("Height observed: ad ", length(ad_log_height),
         "; nonad ", length(nonad_log_height)),
  paste0("Height missing: ad ", height_missing_ad, " (",
         fmt_num(missing_percent[1], 2), "%); nonad ", height_missing_nonad,
         " (", fmt_num(missing_percent[2], 2), "%)"),
  paste0("Height-missing chi-square: ", fmt_num(missing_chi_result$statistic, 3),
         "; df = ", missing_chi_result$df,
         "; p = ", format(missing_chi_result$p_value, scientific = TRUE, digits = 4)),
  paste0("Chi-square continuity correction: disabled (correct = FALSE); all expected counts > 100: ",
         expected_counts_all_above_100),
  paste0("Log-height Welch geometric-mean ratio: ", fmt_num(welch_results$geometric_mean_ratio[welch_results$variable == "log_height"], 3)),
  paste0("Log-width Welch geometric-mean ratio: ", fmt_num(welch_results$geometric_mean_ratio[welch_results$variable == "log_width"], 3)),
  paste0("Log-height F ratio (ad variance / non-ad variance): ", fmt_num(height_f_result$F_ratio, 3)),
  paste0("Fligner-Killeen statistic: ",
         fmt_num(unname(height_fligner_test$statistic), 2),
         "; df = ", unname(height_fligner_test$parameter),
         "; p = ", format(height_fligner_test$p.value, scientific = TRUE, digits = 4)),
  paste0("Bootstrap log-height variance-ratio 95% CI: [",
         fmt_num(bootstrap_variance_ci[1], 2), ", ",
         fmt_num(bootstrap_variance_ci[2], 2), "] from ", bootstrap_reps,
         " resamples (seed ", bootstrap_seed, ")"),
  paste0("Exact repeated full rows beyond first copies: ", duplicate_rows,
         " (", fmt_num(100 * duplicate_rows / nrow(analysis), 2), "%)"),
  paste0("Unique full feature rows: ", nrow(unique_analysis)),
  paste0("Ad height exactly 60 pixels: ", ad_height_60_count,
         " of ", sum(is.finite(ad_heights)), " observed (",
         fmt_num(100 * ad_height_60_count / sum(is.finite(ad_heights)), 1), "%)"),
  paste0("Ad dimensions exactly 468 by 60 pixels: ", ad_banner_468_60_count,
         " of ", nrow(ad_complete), " complete ad records (",
         fmt_num(100 * ad_banner_468_60_count / nrow(ad_complete), 1), "%)"),
  paste0("Height geometric-mean ratio after exact-repeat removal: ",
         fmt_num(unique_height_ratio, 3)),
  paste0("Width geometric-mean ratio after exact-repeat removal: ",
         fmt_num(unique_width_ratio, 3)),
  paste0("Width geometric-mean ratio after exact-repeat removal 95% CI: [",
         fmt_num(unique_width_ratio_ci[1], 2), ", ",
         fmt_num(unique_width_ratio_ci[2], 2), "]"),
  paste0("Log-height F ratio after exact-repeat removal: ",
         fmt_num(unname(unique_height_f$statistic), 3)),
  "Primary analyses use all supplied records and available observations for each variable.",
  "Aspect ratio is treated as width divided by height and is not tested separately.",
  "The local indicator is described in the dataset section but is not tested.",
  "The F-test assumes independent normal populations; histograms show concentrated, non-bell-shaped distributions.",
  "Fligner-Killeen is used as a rank-based variance check; the bootstrap interval resamples rows independently within class.",
  "Exact repeated feature rows do not prove repeated physical images because no unique image identifier is supplied."
)
writeLines(audit_lines, file.path(out_dir, "analysis_audit.txt"), useBytes = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "session_info.txt"), useBytes = TRUE)

cat("Analysis complete. Outputs saved to:", normalizePath(out_dir), "\n")
cat("Figures saved to:", normalizePath(fig_dir), "\n")
cat("Rows:", nrow(analysis), " | ad:", sum(analysis$class == "ad"),
    " | nonad:", sum(analysis$class == "nonad"), "\n")
cat("Height missingness:", fmt_num(missing_percent[1], 1), "% ad;",
    fmt_num(missing_percent[2], 1), "% nonad\n")
cat("Fligner-Killeen p-value:", format(height_fligner_test$p.value, scientific = TRUE, digits = 3), "\n")
cat("Bootstrap 95% CI for log-height variance ratio: [",
    fmt_num(bootstrap_variance_ci[1], 2), ", ",
    fmt_num(bootstrap_variance_ci[2], 2), "]\n", sep = "")
