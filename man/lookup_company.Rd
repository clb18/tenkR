\name{lookup_company}
\alias{lookup_company}
\title{Look up a company's CIK number from a ticker symbol or company name}
\usage{
lookup_company(identifier, user_agent)
}
\arguments{
\item{identifier}{A character string containing either a ticker symbol or
company name.}

\item{user_agent}{A character string identifying your application to the SEC.
Required by SEC policy. Format: "Your Name your@email.com"}
}
\value{
A data frame with columns: cik, ticker, company_name
}
\description{
Searches the SEC EDGAR company database to find the Central Index Key (CIK)
number for a given company. Accepts either a ticker symbol (e.g., "AAPL")
or a partial/full company name (e.g., "Apple").
}
\examples{
\dontrun{
# Look up by ticker
lookup_company("AAPL", "Jane Smith jane@university.edu")

# Look up by company name
lookup_company("Apple", "Jane Smith jane@university.edu")
}
}
