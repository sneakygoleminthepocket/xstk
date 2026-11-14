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
data_dir <- file.path(project_root, "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

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

log_positive <- function(x) {
  result <- rep(NA_real_, length(x))
  valid <- !is.na(x) & is.finite(x) & x > 0
  result[valid] <- log(x[valid])
  result
}

# Convert and export the complete preprocessed dataset before any analysis-
# specific filtering. Keep every predictor and every valid-class record;
# missing predictor values remain NA. The first four attributes have known
# meanings, while the remaining numbered attributes retain their positions.
predictor_count <- ncol(raw) - 1L
if (predictor_count < 4L) {
  stop("The cleaned dataset has fewer than four expected measurement columns.")
}
converted_predictors <- lapply(raw[seq_len(predictor_count)], num)
non_numeric_columns <- which(vapply(seq_len(predictor_count), function(j) {
  original <- raw[[j]]
  converted <- converted_predictors[[j]]
  any(!is.na(original) & is.na(converted))
}, logical(1)))
if (length(non_numeric_columns)) {
  stop(paste0("Non-numeric values remain in predictor columns: ",
              paste(non_numeric_columns - 1L, collapse = ", ")))
}
preprocessed_names <- c(
  "height", "width", "aspect_ratio", "local",
  if (predictor_count > 4L) {
    sprintf("feature_%04d", seq.int(4L, predictor_count - 1L))
  }
)
preprocessed_predictors <- as.data.frame(converted_predictors,
                                           check.names = FALSE,
                                           stringsAsFactors = FALSE)
names(preprocessed_predictors) <- preprocessed_names
preprocessed_dataset <- data.frame(
  record_id = seq_len(nrow(raw)),
  preprocessed_predictors,
  log_height = log_positive(preprocessed_predictors$height),
  log_width = log_positive(preprocessed_predictors$width),
  class = class_clean,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
preprocessed_file <- file.path(data_dir, "preprocessed_dataset.csv")
write.csv(preprocessed_dataset, preprocessed_file, row.names = FALSE,
          na = "", fileEncoding = "UTF-8")

preprocessing_audit <- c(
  "Preprocessing output",
  paste0("Rows retained: ", nrow(preprocessed_dataset)),
  paste0("Original numeric predictors retained: ", predictor_count),
  paste0("Derived log variables: log_height, log_width"),
  paste0("Class labels: ", paste(sort(unique(class_clean)), collapse = ", ")),
  paste0("Rows with missing height: ", sum(is.na(preprocessed_dataset$height))),
  paste0("Rows with missing width: ", sum(is.na(preprocessed_dataset$width))),
  "Missing values are stored as empty CSV fields.",
  "No records were removed because a predictor value was missing.",
  "Exact duplicate records are retained in the primary preprocessed dataset.",
  paste0("Output file: ", preprocessed_file)
)
writeLines(preprocessing_audit,
           file.path(out_dir, "preprocessing_audit.txt"), useBytes = TRUE)

analysis <- data.frame(
  row_id = seq_len(nrow(raw)),
  height = preprocessed_predictors$height,
  width = preprocessed_predictors$width,
  aspect_ratio = preprocessed_predictors$aspect_ratio,
  local = preprocessed_predictors$local,
  class = factor(class_clean, levels = c("nonad", "ad")),
  stringsAsFactors = FALSE
)
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
writeLines(capture.output(print(missing_chi)),
           file.path(out_dir, "r_output_chisq_test.txt"), useBytes = TRUE)
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
writeLines(capture.output(print(height_f_test)),
           file.path(out_dir, "r_output_var_test.txt"), useBytes = TRUE)
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
writeLines(capture.output(print(height_fligner_test)),
           file.path(out_dir, "r_output_fligner_test.txt"), useBytes = TRUE)

variance_robustness_result <- data.frame(
  F_ratio = unname(height_f_test$statistic),
  F_p_value = height_f_test$p.value,
  fligner_statistic = unname(height_fligner_test$statistic),
  fligner_df = unname(height_fligner_test$parameter),
  fligner_p_value = height_fligner_test$p.value
)
write.csv(variance_robustness_result,
          file.path(out_dir, "variance_robustness_results.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

variance_robustness_rows <- list(
  c("Classical F-test", paste0("$F = ", fmt_num(height_f_result$F_ratio, 3), "$"),
    fmt_p(height_f_result$p_value)),
  c("Fligner--Killeen", paste0("$\\chi^2 = ",
    fmt_num(unname(height_fligner_test$statistic), 2), "$"),
    fmt_p(height_fligner_test$p.value))
)
write_tabular(file.path(out_dir, "variance_robustness_table.tex"),
              c("Procedure", "Result", "$p$-value"),
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

# 8) Binary logistic regression using the two logged image dimensions.
# The response is 1 for advertisements and 0 for non-advertisements.
# Only records with both positive dimensions are eligible for this model.
model_data <- analysis[is.finite(analysis$log_height) &
                         is.finite(analysis$log_width), , drop = FALSE]
model_data$ad <- as.integer(model_data$class == "ad")

# The cleaned-data row identifier is retained after complete-case selection.
# Rows with row_id modulo 10 equal to 0, 1, or 2 form the held-out test set.
test_flag <- model_data$row_id %% 10L %in% c(0L, 1L, 2L)
train_data <- model_data[!test_flag, , drop = FALSE]
test_data <- model_data[test_flag, , drop = FALSE]

if (nrow(model_data) != 2369L || nrow(train_data) != 1657L || nrow(test_data) != 712L) {
  warning("The logistic-regression sample or deterministic split differs from the documented CSV version.")
}

logistic_model <- glm(
  ad ~ log_width + log_height,
  data = train_data,
  family = binomial(link = "logit")
)

logistic_summary_object <- summary(logistic_model)
logistic_summary <- logistic_summary_object$coefficients
logistic_console_excerpt <- capture.output(print(summary(logistic_model)$coefficients))
writeLines(logistic_console_excerpt,
           file.path(out_dir, "r_output_logistic_summary.txt"), useBytes = TRUE)
z_975 <- qnorm(0.975)
coefficient_names <- c("(Intercept)", "log_width", "log_height")
coefficient_records <- lapply(coefficient_names, function(term) {
  estimate <- logistic_summary[term, "Estimate"]
  standard_error <- logistic_summary[term, "Std. Error"]
  z_value <- logistic_summary[term, "z value"]
  p_value <- logistic_summary[term, "Pr(>|z|)"]
  lower <- estimate - z_975 * standard_error
  upper <- estimate + z_975 * standard_error
  data.frame(
    predictor = term,
    estimate = estimate,
    standard_error = standard_error,
    z = z_value,
    p_value = p_value,
    odds_ratio_per_doubling = if (term == "(Intercept)") NA_real_ else exp(estimate * log(2)),
    odds_ratio_lower = if (term == "(Intercept)") NA_real_ else exp(lower * log(2)),
    odds_ratio_upper = if (term == "(Intercept)") NA_real_ else exp(upper * log(2))
  )
})
logistic_coefficients <- do.call(rbind, coefficient_records)
write.csv(logistic_coefficients,
          file.path(out_dir, "logistic_coefficients.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

coefficient_rows <- lapply(seq_len(nrow(logistic_coefficients)), function(i) {
  rec <- logistic_coefficients[i, ]
  label <- if (rec$predictor == "(Intercept)") "Intercept" else
    if (rec$predictor == "log_width") "Log-width" else "Log-height"
  odds_text <- if (rec$predictor == "(Intercept)") "--" else
    paste0(fmt_num(rec$odds_ratio_per_doubling, 2), " [",
           fmt_num(rec$odds_ratio_lower, 2), ", ",
           fmt_num(rec$odds_ratio_upper, 2), "]")
  c(
    label,
    fmt_num(rec$estimate, 3),
    fmt_num(rec$standard_error, 3),
    fmt_num(rec$z, 2),
    fmt_p(rec$p_value),
    odds_text
  )
})
write_tabular(file.path(out_dir, "logistic_coefficients_table.tex"),
              c("Predictor", "Estimate", "SE", "$z$", "$p$",
                "Odds ratio per doubling (95\\% CI)"),
              coefficient_rows, "lrrrrl")

# Rank-based AUC handles tied prediction scores by assigning average ranks.
auc_rank <- function(actual, score) {
  actual <- as.integer(actual)
  n1 <- sum(actual == 1L); n0 <- sum(actual == 0L)
  if (n1 == 0L || n0 == 0L) return(NA_real_)
  ranks <- rank(score, ties.method = "average")
  (sum(ranks[actual == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

safe_divide <- function(numerator, denominator) {
  if (denominator == 0) return(NA_real_)
  numerator / denominator
}

logistic_probabilities <- predict(logistic_model, newdata = test_data,
                                  type = "response")
main_cutoff <- 0.50
lower_cutoff <- mean(train_data$ad)
predicted_class <- as.integer(logistic_probabilities >= main_cutoff)

classification_metrics <- function(actual, probability, cutoff) {
  predicted <- as.integer(probability >= cutoff)
  counts <- table(
    factor(actual, levels = c(0, 1)),
    factor(predicted, levels = c(0, 1))
  )
  tn <- unname(counts[1, 1])
  fp <- unname(counts[1, 2])
  fn <- unname(counts[2, 1])
  tp <- unname(counts[2, 2])
  data.frame(
    TN = tn, FP = fp, FN = fn, TP = tp,
    accuracy = safe_divide(tp + tn, length(actual)),
    sensitivity = safe_divide(tp, tp + fn),
    specificity = safe_divide(tn, tn + fp)
  )
}

# Values used in the report's banner-size description.
ad_heights <- analysis$height[analysis$class == "ad"]
ad_complete <- analysis[analysis$class == "ad" &
                          !is.na(analysis$height) & !is.na(analysis$width), , drop = FALSE]
ad_height_60_count <- sum(ad_heights == 60, na.rm = TRUE)
ad_banner_468_60_count <- sum(ad_complete$width == 468 & ad_complete$height == 60)
complete_data <- analysis[is.finite(analysis$log_height) &
                            is.finite(analysis$log_width), , drop = FALSE]
rule_actual <- as.integer(complete_data$class == "ad")
rule_predicted <- as.integer(complete_data$width == 468 & complete_data$height == 60)
banner_rule_counts <- table(factor(rule_actual, levels = c(0, 1)),
                            factor(rule_predicted, levels = c(0, 1)))
banner_rule_tn <- unname(banner_rule_counts[1, 1])
banner_rule_fp <- unname(banner_rule_counts[1, 2])
banner_rule_fn <- unname(banner_rule_counts[2, 1])
banner_rule_tp <- unname(banner_rule_counts[2, 2])
banner_rule_result <- data.frame(
  complete_cases = length(rule_actual), TN = banner_rule_tn, FP = banner_rule_fp,
  FN = banner_rule_fn, TP = banner_rule_tp,
  accuracy = (banner_rule_tn + banner_rule_tp) / length(rule_actual),
  sensitivity = safe_divide(banner_rule_tp, banner_rule_tp + banner_rule_fn),
  specificity = safe_divide(banner_rule_tn, banner_rule_tn + banner_rule_fp)
)
write.csv(banner_rule_result, file.path(out_dir, "banner_rule_descriptive.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

main_metrics <- classification_metrics(test_data$ad, logistic_probabilities,
                                       main_cutoff)
lower_metrics <- classification_metrics(test_data$ad, logistic_probabilities,
                                        lower_cutoff)

# Majority-class baseline: every test observation is classified as non-ad.
baseline_metrics <- data.frame(
  TN = sum(test_data$ad == 0L),
  FP = 0L,
  FN = sum(test_data$ad == 1L),
  TP = 0L,
  accuracy = mean(test_data$ad == 0L),
  sensitivity = 0,
  specificity = 1
)

holdout_auc <- auc_rank(test_data$ad, logistic_probabilities)
performance_records <- rbind(
  data.frame(rule = "Majority baseline", cutoff = "Always non-ad",
             baseline_metrics, auc = NA_real_),
  data.frame(rule = "Logistic regression", cutoff = "0.500",
             main_metrics, auc = holdout_auc),
  data.frame(rule = "Logistic regression", cutoff = fmt_num(lower_cutoff, 3),
             lower_metrics, auc = holdout_auc)
)
write.csv(performance_records,
          file.path(out_dir, "logistic_performance.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

performance_rows <- lapply(seq_len(nrow(performance_records)), function(i) {
  rec <- performance_records[i, ]
  c(
    rec$rule,
    rec$cutoff,
    as.character(rec$TN), as.character(rec$FP),
    as.character(rec$FN), as.character(rec$TP),
    paste0(fmt_num(100 * rec$accuracy, 1), "\\%"),
    paste0(fmt_num(100 * rec$sensitivity, 1), "\\%"),
    paste0(fmt_num(100 * rec$specificity, 1), "\\%"),
    if (is.na(rec$auc)) "--" else fmt_num(rec$auc, 3)
  )
})
write_tabular(file.path(out_dir, "logistic_performance_table.tex"),
              c("Rule", "Cutoff", "TN", "FP", "FN", "TP",
                "Accuracy", "Sensitivity", "Specificity", "AUC"),
              performance_rows, "llrrrrrrrr")

# Compare the contribution of each predictor using nested logistic models.
width_only_model <- glm(ad ~ log_width, data = train_data,
                        family = binomial(link = "logit"))
height_only_model <- glm(ad ~ log_height, data = train_data,
                         family = binomial(link = "logit"))
height_added_test <- anova(width_only_model, logistic_model, test = "Chisq")
width_added_test <- anova(height_only_model, logistic_model, test = "Chisq")
width_only_prob <- predict(width_only_model, newdata = test_data, type = "response")
height_only_prob <- predict(height_only_model, newdata = test_data, type = "response")
predictor_comparison <- data.frame(
  width_only_test_auc = auc_rank(test_data$ad, width_only_prob),
  height_only_test_auc = auc_rank(test_data$ad, height_only_prob),
  both_predictors_test_auc = holdout_auc,
  p_value_for_adding_height_to_width = height_added_test$`Pr(>Chi)`[2],
  p_value_for_adding_width_to_height = width_added_test$`Pr(>Chi)`[2]
)
write.csv(predictor_comparison,
          file.path(out_dir, "logistic_predictor_comparison.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# Deterministic five-fold cross-validation, used as a brief stability check.
# Fold assignment cycles through the ordered complete cases: 1, 2, 3, 4, 5, 1, ...
cv_fold <- ((seq_len(nrow(model_data)) - 1L) %% 5L) + 1L
cv_probabilities <- rep(NA_real_, nrow(model_data))
for (fold in 1:5) {
  validation_flag <- cv_fold == fold
  fold_train <- model_data[!validation_flag, , drop = FALSE]
  fold_validation <- model_data[validation_flag, , drop = FALSE]
  fold_model <- glm(ad ~ log_width + log_height, data = fold_train,
                    family = binomial(link = "logit"))
  cv_probabilities[validation_flag] <- predict(
    fold_model, newdata = fold_validation, type = "response"
  )
}
cv_auc <- auc_rank(model_data$ad, cv_probabilities)

# De-duplicated classification sensitivity analysis. The original cleaned row
# identifier is retained so the same deterministic split rule is applied.
unique_model_data <- unique_analysis[
  is.finite(unique_analysis$log_height) &
    is.finite(unique_analysis$log_width), , drop = FALSE
]
unique_model_data$ad <- as.integer(unique_model_data$class == "ad")
unique_test_flag <- unique_model_data$row_id %% 10L %in% c(0L, 1L, 2L)
unique_train <- unique_model_data[!unique_test_flag, , drop = FALSE]
unique_test <- unique_model_data[unique_test_flag, , drop = FALSE]
unique_logistic_model <- glm(
  ad ~ log_width + log_height,
  data = unique_train,
  family = binomial(link = "logit")
)
unique_probabilities <- predict(unique_logistic_model, newdata = unique_test,
                                type = "response")
unique_metrics <- classification_metrics(unique_test$ad, unique_probabilities,
                                          main_cutoff)
unique_auc <- auc_rank(unique_test$ad, unique_probabilities)
unique_baseline_accuracy <- mean(unique_test$ad == 0L)
unique_width_coefficient <- summary(unique_logistic_model)$coefficients["log_width", ]
unique_height_coefficient <- summary(unique_logistic_model)$coefficients["log_height", ]
unique_width_or_doubling <- exp(unname(unique_width_coefficient["Estimate"]) * log(2))

classification_audit <- data.frame(
  complete_cases = nrow(model_data),
  complete_ad = sum(model_data$ad == 1L),
  complete_nonad = sum(model_data$ad == 0L),
  excluded_for_missing_dimensions = nrow(analysis) - nrow(model_data),
  train_n = nrow(train_data), test_n = nrow(test_data),
  train_ad = sum(train_data$ad == 1L), train_nonad = sum(train_data$ad == 0L),
  test_ad = sum(test_data$ad == 1L), test_nonad = sum(test_data$ad == 0L),
  test_cutoff = main_cutoff,
  lower_cutoff = lower_cutoff,
  test_auc = holdout_auc,
  five_fold_cv_auc = cv_auc,
  deduplicated_n = nrow(unique_model_data),
  deduplicated_test_n = nrow(unique_test),
  deduplicated_test_auc = unique_auc,
  deduplicated_test_accuracy = unique_metrics$accuracy,
  deduplicated_baseline_accuracy = unique_baseline_accuracy,
  deduplicated_sensitivity = unique_metrics$sensitivity,
  deduplicated_specificity = unique_metrics$specificity,
  deduplicated_width_odds_ratio_per_doubling = unique_width_or_doubling,
  deduplicated_height_coefficient = unname(unique_height_coefficient["Estimate"]),
  deduplicated_height_p_value = unname(unique_height_coefficient["Pr(>|z|)"])
)
write.csv(classification_audit,
          file.path(out_dir, "logistic_classification_audit.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

part_b_audit_lines <- c(
  "Part B - Logistic regression audit",
  paste0("Complete cases: ", nrow(model_data), " (ads = ", sum(model_data$ad == 1L),
         "; non-ads = ", sum(model_data$ad == 0L), ")"),
  paste0("Training set: ", nrow(train_data), " (ads = ", sum(train_data$ad == 1L),
         "; non-ads = ", sum(train_data$ad == 0L), ")"),
  paste0("Testing set: ", nrow(test_data), " (ads = ", sum(test_data$ad == 1L),
         "; non-ads = ", sum(test_data$ad == 0L), ")"),
  "Model formula: ad ~ log_width + log_height",
  logistic_console_excerpt,
  paste0("Hold-out AUC: ", fmt_num(holdout_auc, 3)),
  paste0("Five-fold cross-validation AUC: ", fmt_num(cv_auc, 3)),
  paste0("Accuracy at 0.5: ", fmt_num(100 * main_metrics$accuracy, 1), "%"),
  paste0("Sensitivity at 0.5: ", fmt_num(100 * main_metrics$sensitivity, 1), "%"),
  paste0("Specificity at 0.5: ", fmt_num(100 * main_metrics$specificity, 1), "%"),
  paste0("Post-hoc 468x60 rule accuracy: ", fmt_num(100 * banner_rule_result$accuracy, 1), "%"),
  paste0("Post-hoc 468x60 rule sensitivity: ", fmt_num(100 * banner_rule_result$sensitivity, 1), "%"),
  paste0("Post-hoc 468x60 rule specificity: ", fmt_num(100 * banner_rule_result$specificity, 1), "%")
)
writeLines(part_b_audit_lines, file.path(out_dir, "partB_audit.txt"), useBytes = TRUE)

# ROC points and the two operating points reported in the performance table.
roc_thresholds <- c(Inf, sort(unique(logistic_probabilities), decreasing = TRUE), -Inf)
roc_records <- lapply(roc_thresholds, function(cutoff) {
  metric <- classification_metrics(test_data$ad, logistic_probabilities, cutoff)
  data.frame(
    cutoff = cutoff,
    false_positive_rate = 1 - metric$specificity,
    true_positive_rate = metric$sensitivity
  )
})
roc_points <- do.call(rbind, roc_records)
write.csv(roc_points, file.path(out_dir, "logistic_roc_points.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

roc_location <- function(cutoff) {
  metric <- classification_metrics(test_data$ad, logistic_probabilities, cutoff)
  c(x = 1 - metric$specificity, y = metric$sensitivity)
}
roc_main <- roc_location(main_cutoff)
roc_lower <- roc_location(lower_cutoff)
png(file.path(fig_dir, "logistic_roc.png"), width = 1500, height = 1250, res = 150)
par(mar = c(5.5, 5.5, 4.4, 1.5), mgp = c(2.9, 0.95, 0), pty = "s",
    xaxs = "i", yaxs = "i", cex.axis = 1.32, cex.lab = 1.30)
plot(roc_points$false_positive_rate, roc_points$true_positive_rate,
     type = "l", lwd = 2.8, col = "steelblue4",
     xlim = c(0, 1), ylim = c(0, 1), asp = 1, xaxs = "i", yaxs = "i",
     xlab = "False positive rate (1 - specificity)",
     ylab = "True positive rate (sensitivity)",
     main = paste0("ROC curve for logistic regression (AUC = ",
                   fmt_num(holdout_auc, 3), ")"), cex.main = 1.30)
abline(0, 1, lty = 2, col = "gray55", lwd = 1.3)
points(roc_main[1], roc_main[2], pch = 19, cex = 1.35, col = "tomato3")
points(roc_lower[1], roc_lower[2], pch = 17, cex = 1.45, col = "darkgreen")
text(roc_main[1], roc_main[2], labels = "Cutoff 0.50", pos = 4,
     offset = 0.7, cex = 1.05, col = "tomato3")
text(roc_lower[1], roc_lower[2], labels = paste0("Cutoff ", fmt_num(lower_cutoff, 3)),
     pos = 4, offset = 0.7, cex = 1.05, col = "darkgreen")
legend("bottomright",
       legend = c("ROC curve", "No-discrimination line", "Cutoff 0.50", "Lower cutoff"),
       lty = c(1, 2, NA, NA), pch = c(NA, NA, 19, 17),
       col = c("steelblue4", "gray55", "tomato3", "darkgreen"),
       bty = "n", cex = 1.02)
dev.off()

# 9) Most common complete width-height pairs. The report summarizes the
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

# 10) Figures. Each legend is given its own space; titles and labels do not overlap.

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
png(file.path(fig_dir, "log_geometry_boxplots.png"), width = 2400, height = 1450, res = 200)
par(mfrow = c(1, 2), mar = c(5.5, 5.1, 4.2, 1.4), oma = c(3.0, 0, 0, 0),
    mgp = c(2.8, 0.9, 0), cex.axis = 1.55, cex.lab = 1.60, cex.main = 1.58)
box_colors <- c("nonad" = "steelblue2", "ad" = "tomato2")
boxplot(log_height ~ class, data = analysis, main = "Log(height)",
        xlab = "Class", ylab = "Natural log",
        col = unname(box_colors[levels(analysis$class)]), outline = TRUE,
        outpch = 16, outcex = 0.62,
        outcol = adjustcolor("gray20", alpha.f = 0.6),
        boxlwd = 1.2, medlwd = 2.4, whisklwd = 1.1)
boxplot(log_width ~ class, data = analysis, main = "Log(width)",
        xlab = "Class", ylab = "Natural log",
        col = unname(box_colors[levels(analysis$class)]), outline = TRUE,
        outpch = 16, outcex = 0.62,
        outcol = adjustcolor("gray20", alpha.f = 0.6),
        boxlwd = 1.2, medlwd = 2.4, whisklwd = 1.1)
mtext(boxplot_note, side = 1, outer = TRUE, line = 0.85, cex = 1.02)
par(mfrow = c(1, 1))
dev.off()

# Raw-scale histograms emphasize repeated dimensions such as 60-pixel heights
# and 468-pixel widths. The shared legend occupies a separate panel.
png(file.path(fig_dir, "geometry_histograms.png"), width = 2400, height = 1250, res = 200)
layout(matrix(c(1, 2, 3), nrow = 1), widths = c(1, 1, 0.25))
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
       ylab = "Density", cex.main = 1.34)
  hist(ad_values, breaks = breaks, probability = TRUE,
       col = adjustcolor("tomato3", 0.48), border = "white", add = TRUE)
}
par(mar = c(5.2, 4.8, 4.1, 1.2), mgp = c(2.7, 0.9, 0),
    cex.axis = 2.30, cex.lab = 1.85)
plot_histogram_pair("height", "Recorded image height", 10)
plot_histogram_pair("width", "Recorded image width", 20)
par(mar = rep(0, 4))
plot.new()
legend("center", legend = c("Non-advertisement", "Advertisement"),
       fill = c("steelblue3", "tomato3"), bty = "n", cex = 1.28)
layout(1)
dev.off()

# Descriptive scatter plot. A single plotting panel keeps the observations large and readable.
scatter_data <- analysis[is.finite(analysis$log_height) &
                           is.finite(analysis$log_width), , drop = FALSE]
scatter_colors <- ifelse(scatter_data$class == "ad",
                         adjustcolor("tomato3", 0.55),
                         adjustcolor("steelblue3", 0.45))
png(file.path(fig_dir, "log_width_height_scatter.png"), width = 2300, height = 1550, res = 200)
par(mar = c(5.6, 5.8, 4.4, 2.0), mgp = c(2.9, 0.95, 0),
    cex.axis = 1.55, cex.lab = 1.68, cex.main = 1.62)
plot(scatter_data$log_width, scatter_data$log_height,
     pch = 16, cex = 0.78, col = scatter_colors,
     xlim = c(0, 6.7), ylim = c(0, 6.7),
     xlab = "Log width", ylab = "Log height",
     main = "Log-height versus log-width")
# Circle the shared coordinates of the 181 advertisement records at 468 by 60.
points(log(468), log(60), pch = 1, cex = 2.35, lwd = 2.6, col = "darkred")
# Place the legend and cluster note in the sparse upper-left region of the plot.
legend(x = 0.25, y = 6.15,
       legend = c("Non-advertisement", "Advertisement"),
       pch = 16, col = c("steelblue3", "tomato3"),
       bty = "n", cex = 1.20, pt.cex = 1.12)
text(0.25, 4.85, labels = "Circled cluster:\n181 ads at 468 x 60",
     adj = c(0, 1), cex = 1.20, col = "darkred")
dev.off()

# 11) Audit notes for the report and for reproducibility.
height_missing_ad <- sum(is.na(analysis$height) & analysis$class == "ad")
height_missing_nonad <- sum(is.na(analysis$height) & analysis$class == "nonad")
predictor_frame <- raw[, -ncol(raw), drop = FALSE]
predictor_keys <- apply(predictor_frame, 1, function(row) {
  paste(ifelse(is.na(row), "<NA>", row), collapse = "\034")
})
profile_classes <- split(as.character(analysis$class), predictor_keys)
mixed_profile_count <- sum(vapply(profile_classes, function(labels) length(unique(labels)) > 1L, logical(1)))
side1_nonad_count <- sum(analysis$class == "nonad" &
                           (analysis$height == 1 | analysis$width == 1), na.rm = TRUE)
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
  paste0("Distinct full 1,558-predictor profiles represented under both labels: ", mixed_profile_count),
  paste0("468x60 rule on all complete cases: accuracy ",
         fmt_num(100 * banner_rule_result$accuracy, 1), "% ; sensitivity ",
         fmt_num(100 * banner_rule_result$sensitivity, 1), "% ; specificity ",
         fmt_num(100 * banner_rule_result$specificity, 1), "%"),
  paste0("Non-advertisement records with a one-pixel side: ", side1_nonad_count),
  paste0("Logistic model complete cases: ", nrow(model_data),
         " (ad ", sum(model_data$ad == 1L), "; nonad ", sum(model_data$ad == 0L), ")"),
  paste0("Logistic train/test sizes: ", nrow(train_data), " / ", nrow(test_data)),
  paste0("Logistic hold-out confusion matrix at 0.50: TN ", main_metrics$TN,
         "; FP ", main_metrics$FP, "; FN ", main_metrics$FN,
         "; TP ", main_metrics$TP),
  paste0("Logistic test accuracy / baseline: ", fmt_num(100 * main_metrics$accuracy, 1),
         "% / ", fmt_num(100 * baseline_metrics$accuracy, 1), "%"),
  paste0("Logistic test sensitivity / specificity: ",
         fmt_num(100 * main_metrics$sensitivity, 1), "% / ",
         fmt_num(100 * main_metrics$specificity, 1), "%"),
  paste0("Logistic test AUC: ", fmt_num(holdout_auc, 3)),
  paste0("Deterministic five-fold cross-validation AUC: ", fmt_num(cv_auc, 3)),
  "Cross-validation folds cycle by complete-case position: ((position - 1) mod 5) + 1.",
  paste0("Logistic lower cutoff (training ad prevalence): ", fmt_num(lower_cutoff, 3)),
  paste0("De-duplicated logistic AUC: ", fmt_num(unique_auc, 3)),
  "Primary analyses use all supplied records and available observations for each variable.",
  "Aspect ratio is treated as width divided by height and is not tested separately.",
  "The local indicator is described in the dataset section but is not tested.",
  "The F-test assumes independent normal populations; histograms show concentrated, non-bell-shaped distributions.",
  "Fligner-Killeen is used as a rank-based variance check that is less sensitive to non-normality.",
  "Exact repeated feature rows do not prove repeated physical images because no unique image identifier is supplied."
)
writeLines(audit_lines, file.path(out_dir, "analysis_audit.txt"), useBytes = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "session_info.txt"), useBytes = TRUE)

cat("Analysis complete. Outputs saved to:", normalizePath(out_dir), "\n")
cat("Figures saved to:", normalizePath(fig_dir), "\n")
cat("Preprocessed dataset saved to:", normalizePath(preprocessed_file), "\n")
cat("Rows:", nrow(analysis), " | ad:", sum(analysis$class == "ad"),
    " | nonad:", sum(analysis$class == "nonad"), "\n")
cat("Height missingness:", fmt_num(missing_percent[1], 1), "% ad;",
    fmt_num(missing_percent[2], 1), "% nonad\n")
cat("Fligner-Killeen p-value:", format(height_fligner_test$p.value, scientific = TRUE, digits = 3), "\n")
cat("Logistic hold-out AUC:", fmt_num(holdout_auc, 3), "\n")
