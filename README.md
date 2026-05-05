# Live Entertainment Disruptions and National Stock Market Reactions

This repository contains the replication materials for the Master's thesis:

**Live Entertainment Disruptions and National Stock Market Reactions: An Event Study**  
Wendy Olivia Bazua Corrales  
Charles University, Faculty of Social Sciences, Institute of Economic Studies  
2026

## Repository contents

- `full_code.R`: reproducibility script used to run the event study, robustness checks, cross-sectional regressions, placebo tests, and sectoral analysis.
- `events_final.csv`: final cleaned event dataset used in the thesis.
- `Data Requirements.txt`: description of the external market data files required to reproduce the empirical analysis.

## Data availability

The event dataset is included in this repository as `events_final.csv`.

The financial market data used in the thesis are not redistributed because they were obtained from Refinitiv Workspace Lite. Users who reproduce the analysis must provide equivalent daily market data from an authorized or reliable source.

The required external files are:

- `indices_daily.csv`
- `acwi_daily.csv`
- `vix_daily.csv`
- `msci_regional_daily.csv`
- `sectoral_daily.csv`

All CSV files must be placed in the same folder as `full_code.R`.

## How to run the replication script

1. Download or clone this repository.
2. Add the required external market data CSV files to the same folder as `full_code.R`.
3. Open `full_code.R` in RStudio.
4. Set the working directory to the folder containing the script and CSV files.
5. Run the full script.

The script prints the main tables and robustness results to the console. Figures are displayed in the R graphics window. No external folders or files are created by the script.

## Expected sample counts

- `events_final.csv`: 267 events.
- Analytical sample: 266 events after one event is excluded due to insufficient stock index coverage.

Additional reductions in sample size may occur across specifications because of overlapping event windows or missing financial data.

## Notes on reproducibility

The script expects the event dataset to be already cleaned. Users do not need to reconstruct the event dataset or manually recode event categories.

Small numerical differences may occur if users rely on different data providers, price adjustments, trading calendars, return construction procedures, or data vintages.

Detailed data requirements are provided in `Data Requirements.txt`.

## Citation

If using this repository, please cite the thesis:

Bazua Corrales, Wendy Olivia. 2026. *Live Entertainment Disruptions and National Stock Market Reactions: An Event Study*. Master's thesis, Charles University, Faculty of Social Sciences, Institute of Economic Studies.
