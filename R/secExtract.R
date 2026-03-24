#' secExtract: Download SEC EDGAR Financial Statements into Excel
#'
#' The secExtract package provides a simple interface for downloading
#' 10-K financial statements from the SEC EDGAR database and exporting
#' them to a formatted Excel workbook.
#'
#' @section Main function:
#' The primary function is \code{\link{get_financials}}, which takes a
#' company ticker or name and produces a formatted Excel workbook with
#' three tabs: Income Statement, Balance Sheet, and Cash Flow Statement.
#'
#' @section Quick start:
#' \preformatted{
#' library(secExtract)
#'
#' # Set your user agent (required by SEC policy)
#' my_agent <- "Your Name your@email.com"
#'
#' # Download financials for Apple
#' get_financials("AAPL", my_agent)
#'
#' # Download financials by company name
#' get_financials("Microsoft", my_agent)
#' }
#'
#' @section SEC fair access policy:
#' The SEC requires all API users to identify themselves via a user agent
#' string. Please provide your name and email in the \code{user_agent}
#' argument of each function. Do not make more than 10 requests per second.
#' See \url{https://www.sec.gov/os/accessing-edgar-data} for details.
#'
#' @docType package
#' @name secExtract
"_PACKAGE"

#' @import dplyr
#' @import tidyr
#' @importFrom httr GET user_agent content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle
#'   addStyle setColWidths freezePane saveWorkbook
#' @importFrom stringr str_detect fixed
NULL
