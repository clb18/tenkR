\name{get_financials}
\alias{get_financials}
\title{Download SEC 10-K financial statements and export to Excel}
\usage{
get_financials(identifier, user_agent, n_years = 10, output_dir = ".")
}
\arguments{
\item{identifier}{A character string. Either a ticker symbol (e.g., \code{"AAPL"})
or a company name (e.g., \code{"Apple"}). Ticker symbols are matched exactly;
company names are matched partially.}

\item{user_agent}{A character string identifying your application to the SEC.
\strong{Required by SEC policy.} Format: \code{"Your Name your@email.com"}.
Example: \code{"Jane Smith jane@university.edu"}}

\item{n_years}{Integer. Number of fiscal years to include (default: \code{10}).}

\item{output_dir}{Character. Directory to save the Excel file
(default: current working directory \code{"."}).}
}
\value{
A named list with elements: \code{income_statement}, \code{balance_sheet},
\code{cash_flow}, \code{company_name}, \code{ticker}, \code{cik}, and
\code{filepath}. The Excel workbook is saved as a side effect.
}
\description{
The main function of the secExtract package. Given a company ticker or name,
this function looks up the company CIK, downloads all reported XBRL financial
data, builds formatted financial statements, and exports a clean Excel workbook.
}
\examples{
\dontrun{
# Download Apple's financials by ticker
results <- get_financials("AAPL", "Jane Smith jane@university.edu")

# Download by company name
results <- get_financials("Microsoft", "Jane Smith jane@university.edu")

# Get 5 years of data, save to a specific folder
results <- get_financials("TSLA",
                          user_agent = "Jane Smith jane@university.edu",
                          n_years = 5,
                          output_dir = "~/Documents/Finance")

# Access the data in R
results$income_statement
results$balance_sheet
results$cash_flow
}
}
