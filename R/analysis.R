# Statistics and Probability project: Internet Advertisements
# Dependencies: base R only (stats, graphics, grDevices, utils are included with R)
# Run from the project root:
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
              "\nRun this script from the project root or pass the file path as argument 1."))
}

# Read every field as text first so that '?' and whitespace are handled explicitly.
raw <- read.csv(input_file, header = FALSE, colClasses = "character",
                na.strings = character(0), check.names = FALSE,
                strip.white = FALSE, blank.lines.skip = FALSE, fill = TRUE)

# The supplied Kaggle export contains a first row of column numbers (0,...,1558).
# Remove it only when the row matches that pattern; original UCI ad.data files
# without this extra row are also accepted.
if (nrow(raw) > 1 && ncol(raw) > 1000) {
  first_row_values <- suppressWarnings(as.numeric(trimws(as.character(unlist(raw[1, ], use.names = FALSE)))))
  if (length(first_row_values) == ncol(raw) &&
      all(!is.na(first_row_values[-1])) &&
      all(first_row_values[-1] == seq.int(0, ncol(raw) - 2))) {
    raw <- raw[-1, , drop = FALSE]
  }
}

# The supplied export also has an extra leading row-index column 0,...,n-1.
# Drop it only if it is demonstrably a sequential index.
first_col_values <- suppressWarnings(as.numeric(trimws(as.character(raw[[1]]))))
if (length(first_col_values) == nrow(raw) && all(!is.na(first_col_values)) &&
    all(first_col_values == seq.int(0, nrow(raw) - 1))) {
  raw <- raw[, -1, drop = FALSE]
}

# Trim whitespace and translate '?' / blank strings to NA.
raw[] <- lapply(raw, function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("?", "")] <- NA_character_
  x
})

if (ncol(raw) < 5) stop("The cleaned CSV has fewer than five columns; check the file format.")

# UCI feature order begins with height, width, aspect ratio, and local indicator.
# The final field is the response label. We intentionally focus on these four
# interpretable predictors instead of the remaining 1,554 sparse binary features.
class_raw <- tolower(trimws(raw[[ncol(raw)]]))
class_clean <- ifelse(class_raw %in% c("ad", "ad."), "ad",
                      ifelse(class_raw %in% c("nonad", "nonad."), "nonad", NA_character_))

num <- function(x) suppressWarnings(as.numeric(x))
analysis <- data.frame(
  height = num(raw[[1]]),
  width = num(raw[[2]]),
  aspect_ratio = num(raw[[3]]),
  local = num(raw[[4]]),
  class = factor(class_clean, levels = c("nonad", "ad")),
  stringsAsFactors = FALSE
)
analysis <- analysis[!is.na(analysis$class), , drop = FALSE]
log_positive <- function(x) {
  result <- rep(NA_real_, length(x))
  valid <- !is.na(x) & is.finite(x) & x > 0
  result[valid] <- log(x[valid])
  result
}
analysis$log_height <- log_positive(analysis$height)
analysis$log_width <- log_positive(analysis$width)
analysis$log_aspect_ratio <- log_positive(analysis$aspect_ratio)

# Helpers for concise and consistent output.
fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) return("NA")
  formatC(x, format = "f", digits = digits, big.mark = ",")
}
fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return(format(p, scientific = TRUE, digits = 3))
  formatC(p, format = "f", digits = 3)
}
tex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
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

# 1) Class distribution.
class_counts <- as.data.frame(table(analysis$class), stringsAsFactors = FALSE)
names(class_counts) <- c("class", "count")
class_counts$percentage <- 100 * class_counts$count / sum(class_counts$count)
write.csv(class_counts, file.path(out_dir, "class_counts.csv"), row.names = FALSE, fileEncoding = "UTF-8")
class_rows <- lapply(seq_len(nrow(class_counts)), function(i) {
  c(as.character(class_counts$class[i]), as.character(class_counts$count[i]),
    paste0(fmt_num(class_counts$percentage[i], 2), "\\%"))
})
write_tabular(file.path(out_dir, "class_counts_table.tex"),
              c("Class", "Count", "Percentage"), class_rows, "lrr")

# 2) Descriptive statistics for selected variables by class.
selected_vars <- c("height", "width", "aspect_ratio", "local")
pretty_vars <- c(height = "Height (pixels)", width = "Width (pixels)",
                 aspect_ratio = "Aspect ratio", local = "Local indicator")
desc_rows <- list()
desc_records <- list()
for (v in selected_vars) {
  for (cl in c("ad", "nonad")) {
    x <- analysis[[v]][analysis$class == cl]
    x <- x[!is.na(x)]
    q <- if (length(x)) as.numeric(quantile(x, c(0.25, 0.75), names = FALSE)) else c(NA, NA)
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
write.csv(desc_table, file.path(out_dir, "descriptive_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_tabular(file.path(out_dir, "descriptive_table.tex"),
              c("Variable", "Class", "$n$", "Mean", "SD", "Median", "IQR (Q1--Q3)"),
              desc_rows, "llrrrrr")

# 3) Missingness overall and within each class.
missing_vars <- c("height", "width", "aspect_ratio", "local")
missing_records <- list()
missing_rows <- list()
for (v in missing_vars) {
  all_missing <- sum(is.na(analysis[[v]]))
  ad_missing <- sum(is.na(analysis[[v]]) & analysis$class == "ad")
  nonad_missing <- sum(is.na(analysis[[v]]) & analysis$class == "nonad")
  ad_n <- sum(analysis$class == "ad")
  nonad_n <- sum(analysis$class == "nonad")
  rec <- data.frame(variable = v, total_n = nrow(analysis), missing_n = all_missing,
                    missing_percent = 100 * all_missing / nrow(analysis),
                    ad_missing_n = ad_missing, ad_missing_percent = 100 * ad_missing / ad_n,
                    nonad_missing_n = nonad_missing, nonad_missing_percent = 100 * nonad_missing / nonad_n)
  missing_records[[length(missing_records) + 1]] <- rec
  missing_rows[[length(missing_rows) + 1]] <- c(
    pretty_vars[[v]], paste0(all_missing, " (", fmt_num(rec$missing_percent, 2), "\\%)"),
    paste0(ad_missing, " (", fmt_num(rec$ad_missing_percent, 2), "\\%)"),
    paste0(nonad_missing, " (", fmt_num(rec$nonad_missing_percent, 2), "\\%)")
  )
}
missing_table <- do.call(rbind, missing_records)
write.csv(missing_table, file.path(out_dir, "missingness_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_tabular(file.path(out_dir, "missingness_table.tex"),
              c("Variable", "Overall missing", "Ad missing", "Non-ad missing"),
              missing_rows, "lrrr")

# 4) Inferential analysis: F-tests of log-scale variance and Welch t-tests of log-scale means.
log_vars <- c("log_height", "log_width", "log_aspect_ratio")
log_labels <- c(log_height = "Log height", log_width = "Log width", log_aspect_ratio = "Log aspect ratio")
f_results <- list()
t_results <- list()
for (v in log_vars) {
  x <- analysis[[v]][analysis$class == "ad"]
  y <- analysis[[v]][analysis$class == "nonad"]
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  ftest <- var.test(x, y, alternative = "two.sided", conf.level = 0.95)
  ttest <- t.test(x, y, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
  f_results[[length(f_results) + 1]] <- data.frame(
    method = "F-test of equal variances", variable = v,
    n_ad = length(x), n_nonad = length(y),
    statistic = unname(ftest$statistic), df1 = unname(ftest$parameter[1]),
    df2 = unname(ftest$parameter[2]), p_value = ftest$p.value,
    estimate_difference = NA_real_, ci_lower = unname(ftest$conf.int[1]),
    ci_upper = unname(ftest$conf.int[2]), exp_difference = NA_real_,
    exp_ci_lower = NA_real_, exp_ci_upper = NA_real_
  )
  tdiff <- unname(ttest$estimate[1] - ttest$estimate[2])
  t_results[[length(t_results) + 1]] <- data.frame(
    method = "Welch two-sample t-test", variable = v,
    n_ad = length(x), n_nonad = length(y),
    statistic = unname(ttest$statistic), df1 = unname(ttest$parameter),
    df2 = NA_real_, p_value = ttest$p.value,
    estimate_difference = tdiff, ci_lower = unname(ttest$conf.int[1]),
    ci_upper = unname(ttest$conf.int[2]), exp_difference = exp(tdiff),
    exp_ci_lower = exp(unname(ttest$conf.int[1])),
    exp_ci_upper = exp(unname(ttest$conf.int[2]))
  )
}
f_df <- do.call(rbind, f_results)
t_df <- do.call(rbind, t_results)
f_df$p_adjusted <- p.adjust(f_df$p_value, method = "holm")
t_df$p_adjusted <- p.adjust(t_df$p_value, method = "holm")

# 5) Chi-square tests: local indicator vs class; missing height vs class.
local_data <- analysis[!is.na(analysis$local), , drop = FALSE]
local_data$local_factor <- factor(local_data$local, levels = c(0, 1))
local_tab <- table(local_data$class, local_data$local_factor)
local_chi <- chisq.test(local_tab, correct = FALSE)
miss_height <- is.na(analysis$height)
miss_tab <- table(analysis$class, miss_height)
miss_chi <- chisq.test(miss_tab, correct = FALSE)
chi_df <- data.frame(
  test = c("Class by local indicator", "Class by height missingness"),
  statistic = c(unname(local_chi$statistic), unname(miss_chi$statistic)),
  df = c(unname(local_chi$parameter), unname(miss_chi$parameter)),
  p_value = c(local_chi$p.value, miss_chi$p.value),
  detail = c("Observed local=1 proportions: ad vs nonad",
             "Missing height proportions: ad vs nonad")
)
write.csv(chi_df, file.path(out_dir, "chi_square_results.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Combine test outputs in a machine-readable file.
write.csv(rbind(f_df, t_df), file.path(out_dir, "inference_results.csv"), row.names = FALSE, fileEncoding = "UTF-8")

f_rows <- lapply(seq_len(nrow(f_df)), function(i) {
  c(unname(log_labels[[f_df$variable[i]]]),
    paste0(f_df$n_ad[i], " / ", f_df$n_nonad[i]),
    fmt_num(f_df$statistic[i], 3),
    paste0(fmt_num(sqrt(var(analysis[[f_df$variable[i]]][analysis$class == "ad"], na.rm = TRUE)), 3), " / ",
           fmt_num(sqrt(var(analysis[[f_df$variable[i]]][analysis$class == "nonad"], na.rm = TRUE)), 3)),
    fmt_p(f_df$p_value[i]), fmt_p(f_df$p_adjusted[i]))
})
write_tabular(file.path(out_dir, "f_test_table.tex"),
              c("Variable", "$n$ (ad / non-ad)", "$F=s^2_{ad}/s^2_{nonad}$", "SD (ad / non-ad)", "$p$", "Holm-adjusted $p$"),
              f_rows, "llrrrr")

t_rows <- lapply(seq_len(nrow(t_df)), function(i) {
  c(unname(log_labels[[t_df$variable[i]]]), fmt_num(t_df$statistic[i], 3),
    fmt_num(t_df$df1[i], 1),
    paste0(fmt_num(t_df$estimate_difference[i], 3), " [", fmt_num(t_df$ci_lower[i], 3), ", ", fmt_num(t_df$ci_upper[i], 3), "]"),
    paste0(fmt_num(t_df$exp_difference[i], 3), " [", fmt_num(t_df$exp_ci_lower[i], 3), ", ", fmt_num(t_df$exp_ci_upper[i], 3), "]"),
    fmt_p(t_df$p_adjusted[i]))
})
write_tabular(file.path(out_dir, "welch_test_table.tex"),
              c("Variable", "$t$", "$df$", "Log-mean difference (95\\% CI)",
                "Geometric-mean ratio (95\\% CI)", "Holm-adjusted $p$"),
              t_rows, "lrrlll")
chi_rows <- lapply(seq_len(nrow(chi_df)), function(i) {
  c(chi_df$test[i], fmt_num(chi_df$statistic[i], 3), as.character(chi_df$df[i]),
    fmt_p(chi_df$p_value[i]), chi_df$detail[i])
})
write_tabular(file.path(out_dir, "chi_square_table.tex"),
              c("Test", "$\\chi^2$", "$df$", "$p$", "Interpretation target"),
              chi_rows, "lrrrl")

# 6) Figures, all made with base R to avoid extra package dependencies.
png(file.path(fig_dir, "class_distribution.png"), width = 1400, height = 850, res = 150)
barplot(class_counts$count, names.arg = as.character(class_counts$class),
        main = "Class distribution in the supplied CSV", xlab = "Class", ylab = "Number of observations")
dev.off()

miss_plot <- sapply(c("height", "width", "aspect_ratio"), function(v) {
  sapply(c("ad", "nonad"), function(cl) mean(is.na(analysis[[v]][analysis$class == cl])) * 100)
})
png(file.path(fig_dir, "missingness_by_class.png"), width = 1400, height = 850, res = 150)
barplot(miss_plot, beside = TRUE, names.arg = c("Height", "Width", "Aspect ratio"),
        legend.text = rownames(miss_plot), args.legend = list(x = "topright", bty = "n"),
        main = "Missing geometry values by class", xlab = "Variable", ylab = "Missing observations (%)")
dev.off()

png(file.path(fig_dir, "log_geometry_boxplots.png"), width = 1800, height = 700, res = 150)
par(mfrow = c(1, 3), mar = c(5, 4, 4, 1))
boxplot(log_height ~ class, data = analysis, main = "Log(height)", xlab = "Class", ylab = "Natural log")
boxplot(log_width ~ class, data = analysis, main = "Log(width)", xlab = "Class", ylab = "Natural log")
boxplot(log_aspect_ratio ~ class, data = analysis, main = "Log(aspect ratio)", xlab = "Class", ylab = "Natural log")
par(mfrow = c(1, 1))
dev.off()

# Q-Q diagnostics for each log-transformed geometry variable, by class.
# This gives the F-test assumption discussion diagnostics for all three variables.
qq_variables <- c(
  log_height = "Log height",
  log_width = "Log width",
  log_aspect_ratio = "Log aspect ratio"
)
# Remove the superseded one-variable plot so it cannot be mistaken for the new figure.
old_qq_file <- file.path(fig_dir, "qq_log_height.png")
if (file.exists(old_qq_file)) unlink(old_qq_file)
png(file.path(fig_dir, "qq_log_geometry.png"), width = 1800, height = 1500, res = 150)
par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
for (v in names(qq_variables)) {
  for (cl in c("ad", "nonad")) {
    values <- analysis[[v]][analysis$class == cl]
    values <- values[is.finite(values)]
    qqnorm(values, main = paste0("Q-Q: ", qq_variables[[v]], ", ", cl),
           xlab = "Theoretical normal quantiles", ylab = "Sample quantiles")
    qqline(values, col = "blue")
  }
}
mtext("Normal Q-Q plots for log-transformed geometry", outer = TRUE, cex = 1.1)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))
dev.off()

local_prop <- tapply(local_data$local, local_data$class, mean, na.rm = TRUE) * 100
png(file.path(fig_dir, "local_proportion.png"), width = 1200, height = 800, res = 150)
barplot(local_prop[c("ad", "nonad")], names.arg = c("Ad", "Non-ad"),
        ylim = c(0, 100), main = "Proportion with local = 1 by class",
        xlab = "Class", ylab = "Local = 1 (%)")
dev.off()

# 7) Text audit for reproducibility and interpretation.
lines <- c(
  "Internet Advertisements - analysis audit",
  paste0("Input file: ", input_file),
  paste0("Rows after cleaning: ", nrow(analysis)),
  paste0("Selected fields: height, width, aspect_ratio, local, class"),
  paste0("Class counts: ", paste(names(table(analysis$class)), as.integer(table(analysis$class)), collapse = "; ")),
  paste0("Height missing: ", sum(is.na(analysis$height)), " (", fmt_num(100 * mean(is.na(analysis$height)), 2), "%)"),
  paste0("Width missing: ", sum(is.na(analysis$width)), " (", fmt_num(100 * mean(is.na(analysis$width)), 2), "%)"),
  paste0("Aspect-ratio missing: ", sum(is.na(analysis$aspect_ratio)), " (", fmt_num(100 * mean(is.na(analysis$aspect_ratio)), 2), "%)"),
  paste0("Local missing: ", sum(is.na(analysis$local)), " (", fmt_num(100 * mean(is.na(analysis$local)), 2), "%)"),
  "",
  "Variance tests use log-transformed positive geometry variables and compare ad variance / non-ad variance.",
  "Welch tests compare mean log values; exp(mean difference) is a ratio of geometric means.",
  "F-tests are sensitive to departures from normality; see the supplied Q-Q plots.",
  "Missingness differs by class; complete-case findings should therefore be interpreted cautiously."
)
writeLines(lines, file.path(out_dir, "analysis_audit.txt"), useBytes = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "session_info.txt"), useBytes = TRUE)

cat("Analysis complete. Outputs saved to:", normalizePath(out_dir), "\n")
cat("Figures saved to:", normalizePath(fig_dir), "\n")
cat("Rows:", nrow(analysis), " | ad:", sum(analysis$class == "ad"),
    " | nonad:", sum(analysis$class == "nonad"), "\n")
