#' Download SEC 10-K financial statements and export to Excel
#'
#' The main function of the secExtract package. Given a company ticker or name,
#' this function:
#' \enumerate{
#'   \item Looks up the company's CIK number from SEC EDGAR
#'   \item Downloads all reported XBRL financial data from the SEC API
#'   \item Builds formatted income statement, balance sheet, and cash flow statement
#'   \item Exports a clean, professionally formatted Excel workbook
#' }
#'
#' @param identifier A character string. Either a ticker symbol (e.g., \code{"AAPL"})
#'   or a company name (e.g., \code{"Apple"}). Ticker symbols are matched exactly;
#'   company names are matched partially.
#' @param user_agent A character string identifying your application to the SEC.
#'   \strong{Required by SEC policy.} Format: \code{"Your Name your@email.com"}.
#'   Example: \code{"Jane Smith jane@university.edu"}
#' @param n_years Integer. Number of fiscal years to include (default: \code{10}).
#' @param output_dir Character. Directory to save the Excel file
#'   (default: current working directory \code{"."}).
#'
#' @return A named list with three elements: \code{income_statement},
#'   \code{balance_sheet}, and \code{cash_flow} — each a data frame.
#'   The Excel workbook is saved as a side effect.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Download Apple's financials (by ticker)
#' results <- get_financials("AAPL", "Jane Smith jane@university.edu")
#'
#' # Download by company name
#' results <- get_financials("Microsoft", "Jane Smith jane@university.edu")
#'
#' # Get 5 years of data and save to a specific folder
#' results <- get_financials("TSLA",
#'                           user_agent = "Jane Smith jane@university.edu",
#'                           n_years = 5,
#'                           output_dir = "~/Documents/Finance")
#'
#' # Access the data directly in R
#' results$income_statement
#' results$balance_sheet
#' results$cash_flow
#' }
get_financials <- function(identifier,
                           user_agent,
                           n_years = 10,
                           output_dir = ".") {

  # ── Input validation ───────────────────────────────────────────────────────
  .check_user_agent(user_agent)

  if (!is.character(identifier) || nchar(trimws(identifier)) == 0) {
    stop("'identifier' must be a non-empty ticker symbol or company name.")
  }
  if (!is.numeric(n_years) || n_years < 1) {
    stop("'n_years' must be a positive integer.")
  }
  if (!dir.exists(output_dir)) {
    stop(paste0("Output directory does not exist: '", output_dir, "'"))
  }

  # ── Step 1: Look up company ────────────────────────────────────────────────
  message("Step 1/4: Looking up company...")
  company_info <- lookup_company(identifier, user_agent)

  # If multiple matches, use the first and warn
  if (nrow(company_info) > 1) {
    message("Using first match: ", company_info$company_name[1],
            " (", company_info$ticker[1], ")")
    company_info <- company_info[1, ]
  }

  cik          <- company_info$cik
  ticker       <- company_info$ticker
  company_name <- company_info$company_name

  message("  Found: ", company_name, " | Ticker: ", ticker, " | CIK: ", cik)

  # ── Step 2: Download facts ─────────────────────────────────────────────────
  message("Step 2/4: Downloading financial data from SEC EDGAR...")
  facts <- .get_company_facts(cik, user_agent)
  message("  Data retrieved successfully.")

  # ── Step 3: Build statements ───────────────────────────────────────────────
  message("Step 3/4: Building financial statements...")

  income_stmt   <- .build_statement(facts, .income_concepts,  n_years)
  balance_sheet <- .build_statement(facts, .balance_concepts, n_years)
  cash_flow     <- .build_statement(facts, .cashflow_concepts, n_years)

  # If Net Change in Cash is all NA, calculate it from the three subtotals
  if (!is.null(cash_flow)) {
    net_change_row <- which(cash_flow$`Line Item` == "Net Change in Cash")
    if (length(net_change_row) > 0) {
      ops_row <- which(cash_flow$`Line Item` == "Cash from Operations")
      inv_row <- which(cash_flow$`Line Item` == "Cash from Investing")
      fin_row <- which(cash_flow$`Line Item` == "Cash from Financing")
      if (length(ops_row) > 0 & length(inv_row) > 0 & length(fin_row) > 0) {
        cash_flow[net_change_row, 2:ncol(cash_flow)] <-
          cash_flow[ops_row, 2:ncol(cash_flow)] +
          cash_flow[inv_row, 2:ncol(cash_flow)] +
          cash_flow[fin_row, 2:ncol(cash_flow)]
      }
    }
  }
  # Report which statements had data
  stmts_found <- c(
    if (!is.null(income_stmt))   "Income Statement",
    if (!is.null(balance_sheet)) "Balance Sheet",
    if (!is.null(cash_flow))     "Cash Flow"
  )
  message("  Statements built: ", paste(stmts_found, collapse = ", "))

  # ── Step 4: Export to Excel ────────────────────────────────────────────────
  message("Step 4/4: Writing Excel workbook...")

  filepath <- .write_excel(
    income_stmt   = income_stmt,
    balance_sheet = balance_sheet,
    cash_flow     = cash_flow,
    company_name  = company_name,
    ticker        = ticker,
    output_dir    = output_dir
  )

  message("Done! Open the file at: ", filepath)

  # ── Return data invisibly for use in R ────────────────────────────────────
  invisible(list(
    income_statement = income_stmt,
    balance_sheet    = balance_sheet,
    cash_flow        = cash_flow,
    company_name     = company_name,
    ticker           = ticker,
    cik              = cik,
    filepath         = filepath
  ))
}
