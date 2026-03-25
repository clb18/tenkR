#' Look up a company's CIK number from a ticker symbol or company name
#'
#' Searches the SEC EDGAR company database to find the Central Index Key (CIK)
#' number for a given company. Accepts either a ticker symbol (e.g., "AAPL")
#' or a partial/full company name (e.g., "Apple").
#'
#' @param identifier A character string containing either a ticker symbol or
#'   company name.
#' @param user_agent A character string identifying your application to the SEC.
#'   Required by SEC policy. Format: "Your Name your@email.com"
#'
#' @return A data frame with columns: cik, ticker, company_name
#' @export
#'
#' @examples
#' \dontrun{
#' # Look up by ticker
#' lookup_company("AAPL", "Jane Smith jane@university.edu")
#'
#' # Look up by name
#' lookup_company("Apple", "Jane Smith jane@university.edu")
#' }
lookup_company <- function(identifier, user_agent) {
  .check_user_agent(user_agent)

  url <- "https://www.sec.gov/files/company_tickers.json"
  res <- httr::GET(url, httr::user_agent(user_agent))
  .check_response(res, url)

  raw <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  df <- dplyr::bind_rows(raw) %>%
    dplyr::mutate(cik = sprintf("%010d", cik_str)) %>%
    dplyr::select(cik, ticker, title) %>%
    dplyr::rename(company_name = title)

  # Try exact ticker match first
  result <- df %>% dplyr::filter(toupper(ticker) == toupper(identifier))

  # Fall back to partial company name match
  if (nrow(result) == 0) {
    result <- df %>%
      dplyr::filter(stringr::str_detect(
        toupper(company_name), toupper(stringr::fixed(identifier))
      ))
  }

  if (nrow(result) == 0) {
    stop(paste0(
      "No company found for: '", identifier, "'.\n",
      "Try a ticker symbol (e.g., 'AAPL') or a more specific company name."
    ))
  }

  if (nrow(result) > 1) {
    message("Multiple matches found. Returning all matches - use the CIK to be specific:")
    print(result)
    message("Tip: Call get_financials() with the exact ticker for the company you want.")
  }

  return(result)
}


#' Retrieve company facts from the SEC EDGAR API
#'
#' Internal function that fetches all reported XBRL facts for a company.
#'
#' @param cik A 10-digit CIK string (e.g., "0000320193")
#' @param user_agent A character string identifying your application to the SEC.
#'
#' @return A named list of company facts from the SEC API
#' @keywords internal
.get_company_facts <- function(cik, user_agent) {
  url <- paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK", cik, ".json")
  res <- httr::GET(url, httr::user_agent(user_agent))
  .check_response(res, url)
  jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
}


#' Extract a single financial concept from company facts
#'
#' @param facts The company facts list returned by .get_company_facts()
#' @param concept A US-GAAP XBRL concept name (e.g., "Revenues")
#'
#' @return A data frame with columns: date, <concept>, or NULL if not found
#' @keywords internal
.get_concept <- function(facts, concept) {
  tryCatch({
    df <- facts$facts$`us-gaap`[[concept]]$units$USD
    if (is.null(df) || nrow(df) == 0) return(NULL)

    df %>%
      dplyr::filter(form == "10-K", fp == "FY") %>%
      dplyr::arrange(dplyr::desc(filed)) %>%
      dplyr::distinct(end, .keep_all = TRUE) %>%
      dplyr::select(end, val) %>%
      dplyr::rename(date = end, !!concept := val)
  }, error = function(e) NULL)
}


#' Validate that a user agent string has been provided
#' @keywords internal
.check_user_agent <- function(user_agent) {
  if (missing(user_agent) || is.null(user_agent) || nchar(trimws(user_agent)) == 0) {
    stop(
      "A user_agent string is required by SEC policy.\n",
      "Format: \"Your Name your@email.com\"\n",
      "Example: \"Jane Smith jane@university.edu\""
    )
  }
}


#' Validate an HTTP response
#' @keywords internal
.check_response <- function(res, url) {
  if (httr::status_code(res) != 200) {
    stop(paste0(
      "SEC EDGAR request failed (HTTP ", httr::status_code(res), ").\n",
      "URL: ", url, "\n",
      "Please check your internet connection and try again."
    ))
  }
}
