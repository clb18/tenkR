# tenkR

An R package for downloading SEC EDGAR 10-K financial statements and exporting them to a formatted Excel workbook.

## Installation

```r
# Install from GitHub (once published)
# install.packages("devtools")
devtools::install_github("yourusername/tenkR")

# Or install locally from source
devtools::install("path/to/tenkR")
```

## Quick Start

```r
library(tenkR)

# One line to get a full Excel workbook of financial statements
get_financials("AAPL", user_agent = "Your Name your@email.com")

# Works with company names too
get_financials("Microsoft", user_agent = "Your Name your@email.com")

# Control how many years and where to save
get_financials("TSLA",
               user_agent  = "Your Name your@email.com",
               n_years     = 5,
               output_dir  = "~/Documents/Finance")
```

This saves an Excel file named `AAPL_Financial_Statements_2026-03-24.xlsx` to your working directory with three formatted tabs:

- **Income Statement** — Revenue through Net Income and EPS
- **Balance Sheet** — Assets, Liabilities, and Equity
- **Cash Flow Statement** — Operating, Investing, and Financing activities

## Working with the Data in R

`get_financials()` also returns the data as a list for use directly in R:

```r
results <- get_financials("AAPL", "Your Name your@email.com")

# Access each statement as a data frame
results$income_statement
results$balance_sheet
results$cash_flow
```

## Looking Up Companies

```r
# Search by ticker or name
lookup_company("AAPL",      "Your Name your@email.com")
lookup_company("Apple",     "Your Name your@email.com")
lookup_company("Microsoft", "Your Name your@email.com")
```

## SEC Fair Access Policy

The SEC requires all API users to identify themselves. Always provide your name and email in the `user_agent` argument. Do not make more than 10 requests per second. See [SEC EDGAR access policy](https://www.sec.gov/os/accessing-edgar-data) for details.

## Notes

- Data comes directly from the SEC EDGAR Company Facts API
- Only US domestic filers using US-GAAP are fully supported
- Values are in USD as reported (not normalized to millions)
- Not all companies report every line item — missing values appear as `NA`
- Some companies use non-standard XBRL concept names; the package tries multiple aliases per line item

## Dependencies

- `httr` — HTTP requests to the SEC API
- `jsonlite` — JSON parsing
- `dplyr` / `tidyr` — Data manipulation
- `openxlsx` — Excel file creation
- `stringr` — String matching for company name lookup
