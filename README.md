# Internet Advertisements — Statistical and Predictive Analysis

This project examines the relationship between image dimensions and the advertisement label in the supplied Internet Advertisements dataset. Part A presents descriptive and inferential analyses of image height, width, and height missingness. Part B evaluates a binary logistic regression model that uses log-height and log-width to distinguish advertisements from non-advertisements.

## Project structure

- `R/analysis.R` — complete analysis pipeline, written in base R.
- `data/add.csv` — supplied dataset copy.
- `figures/` — figures produced by the R pipeline.
- `outputs/` — descriptive tables, inferential results, classification results, and audit files.

## Analysis overview

### Part A: Statistical investigation

1. Descriptive statistics and graphical summaries of recorded height and width.
2. Welch two-sample tests for differences in mean log-height and mean log-width, reported as geometric-mean ratios with 95% confidence intervals.
3. A classical F-test comparing the two log-height variances.
4. A Fligner–Killeen rank-based variance test as a check less sensitive to non-normality.
5. An uncorrected Pearson chi-square test (`correct = FALSE`) for the association between class and missing height.
6. A sensitivity analysis comparing the main dimension ratios and log-height variance ratio before and after retaining one copy of each exact full feature-and-class row.

### Part B: Predictive investigation

A binary logistic regression model estimates the probability that an image is an advertisement from `log_width` and `log_height`. The predictive sample contains records with both positive dimensions observed. The deterministic hold-out split assigns cleaned row numbers with remainders 0, 1, or 2 modulo 10 to the test set. The main classification threshold is 0.5; a second threshold equal to the training-set advertisement proportion is included for comparison.

The report evaluates the held-out predictions using a confusion matrix, accuracy, sensitivity, specificity, and the area under the receiver operating characteristic curve (AUC). Accuracy is compared with a majority-class baseline that always predicts non-advertisement. The contribution of each dimension is examined with nested likelihood-ratio tests and single-predictor AUC values. A deterministic five-fold cross-validation AUC is reported as a supplementary stability check, and the classifier is re-evaluated after exact repeated full rows are removed.

## Execution

The pipeline requires base R and does not require additional R packages. From the project root, the analysis can be generated with:

```bat
Rscript R/analysis.R
```

On Windows, `run_project.bat` locates `Rscript`, executes the analysis pipeline. RStudio can also run the script after the project root is set as the working directory.

## Generated outputs

The R pipeline writes descriptive and inferential CSV results, LaTeX table fragments, the logistic regression coefficient and performance tables, predictor-comparison results, classification audit files, R console-output excerpts (`r_output_*.txt`), the `partB_audit.txt` summary, the 468-by-60 descriptive-rule summary, ROC coordinates, and PNG figures. The figures include the missingness bar chart, log-scale boxplots, raw-scale histograms, the log-width/log-height scatter plot, and the Part B ROC curve.

## Data sources and references

- Diez, D. M., Çetinkaya-Rundel, M., and Barr, C. D. (2019). *OpenIntro Statistics* (4th ed.). OpenIntro. <https://www.openintro.org/book/os/>.
- Kaggle. *Internet Advertisements Data Set*. <https://www.kaggle.com/datasets/uciml/internet-advertisements-data-set/data>. Accessed November 8, 2026.
- NIST/SEMATECH. *e-Handbook of Statistical Methods*, Section 1.3.5.9, F-Test for Equality of Two Variances. <https://www.itl.nist.gov/div898/handbook/eda/section3/eda359.htm>.
- R Core Team. *R: A Language and Environment for Statistical Computing*. <https://www.R-project.org/>.
- Kushmerick, N. (1999). *Internet Advertisements* [Data set]. UCI Machine Learning Repository. <https://doi.org/10.24432/C5V011>.
- James, G., Witten, D., Hastie, T., and Tibshirani, R. (2021). *An Introduction to Statistical Learning: With Applications in R* (2nd ed.). Springer. <https://www.statlearning.com/>.
- Fligner, M. A., and Killeen, T. J. (1976). Distribution-free two-sample tests for scale. *Journal of the American Statistical Association*, 71(353), 210–213. <https://doi.org/10.1080/01621459.1976.10481517>.

## Generated classification outputs

The outputs include `partB_audit.txt`, console excerpts for the logistic model and the three Part A tests, the 468-by-60 descriptive rule summary, held-out metrics, cross-validation AUC, and ROC coordinates.
