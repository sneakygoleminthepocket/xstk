
# Internet Advertisements — Statistics and Probability Project

This repository contains the R source code, dataset, and generated results for our HCMUT Statistics and Probability group project.

The analysis investigates differences between advertisement and non-advertisement images using introductory statistical methods, including descriptive statistics, Welch two-sample t-tests, F-tests for equality of variances, and chi-square tests of independence.

## Repository structure

```text
.
├── README.md
├── R/
│   └── analysis.R
├── data/
│   └── add.csv
├── figures/
└── outputs/
```

## Requirements

- R installed on your computer.
- No additional R packages are required; the analysis uses base R.

## How to run the analysis

Download or clone the repository and open a terminal in the repository root directory.

Run the following command:

```bash
Rscript R/analysis.R
```

The script reads `data/add.csv`, performs the data cleaning and statistical analysis, and regenerates the result files.

### Alternative: Run using RStudio

1. Open the downloaded repository folder.
2. Open RStudio.
3. Set the working directory to the repository root.
4. Run the following command in the R console:

```r
source("R/analysis.R")
```

## Generated results

The script automatically regenerates the following files:

- `figures/`: PNG plots used to visualise the analysis.
- `outputs/`: Statistical summaries, hypothesis-test results, LaTeX table fragments, and analysis audit files.

Existing generated files with matching names are overwritten when the script runs again.

## Demonstration guide

For the project presentation:

1. Open `R/analysis.R` and explain the data-cleaning procedure.
2. Run the script from the repository root.
3. Show the generated plots in `figures/`.
4. Open selected result tables in `outputs/` and explain the statistical findings.

The analysis can be run independently of the LaTeX report. No PDF compilation is required for the demonstration.

## Dataset source

The supplied CSV is included in `data/add.csv`.

- Kaggle: https://www.kaggle.com/datasets/uciml/internet-advertisements-data-set/data
- UCI Machine Learning Repository: https://archive.ics.uci.edu/dataset/51/internet+advertisements

The analysis preserves the class labels in the supplied CSV and documents the one-observation discrepancy from the class counts listed in the official UCI documentation.

