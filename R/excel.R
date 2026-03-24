#' Write financial statements to a formatted Excel workbook
#'
#' Internal function that takes three financial statement data frames and
#' writes them to a professionally formatted Excel workbook with one sheet
#' per statement.
#'
#' @param income_stmt Data frame for the income statement
#' @param balance_sheet Data frame for the balance sheet
#' @param cash_flow Data frame for the cash flow statement
#' @param company_name The company name string (used in headers)
#' @param ticker The ticker symbol (used in the file name)
#' @param output_dir Directory to save the file (default: working directory)
#'
#' @return The file path of the saved workbook (invisibly)
#' @keywords internal
.write_excel <- function(income_stmt, balance_sheet, cash_flow,
                         company_name, ticker, output_dir = ".") {

  wb <- openxlsx::createWorkbook()

  # ── Style definitions ──────────────────────────────────────────────────────

  title_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 14, textDecoration = "bold"
  )
  subtitle_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 10, fontColour = "#555555"
  )
  header_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 10, textDecoration = "bold",
    fontColour = "white", fgFill = "#1F3864",
    halign = "center", border = "Bottom", borderColour = "#FFFFFF"
  )
  section_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 10, textDecoration = "bold",
    fgFill = "#D6E4F0"
  )
  label_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 10
  )
  number_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 10,
    numFmt = "#,##0", halign = "right"
  )
  decimal_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 10,
    numFmt = "0.00", halign = "right"
  )
  total_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 10, textDecoration = "bold",
    numFmt = "#,##0", halign = "right",
    border = "Top", borderColour = "#1F3864"
  )
  total_label_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 10, textDecoration = "bold",
    border = "Top", borderColour = "#1F3864"
  )
  source_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 8, fontColour = "#888888"
  )

  # Keywords that trigger bold "total" row formatting
  total_keywords <- c(
    "gross profit", "operating income", "net income", "total assets",
    "total liabilities", "total current assets", "total current liabilities",
    "total stockholders equity", "cash from operations",
    "cash from investing", "cash from financing", "net change in cash",
    "pre-tax income", "ebitda"
  )

  # Keywords that trigger section header formatting
  section_keywords <- c(
    "revenue", "cash & equivalents", "accounts payable", "net income",
    "capital expenditures", "debt issuance", "property, plant"
  )

  # ── Sheet writer ───────────────────────────────────────────────────────────

  write_sheet <- function(wb, sheet_name, df, title, units_note) {

    if (is.null(df) || nrow(df) == 0) {
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet_name,
                          x = paste(company_name, "-", title),
                          startRow = 1, startCol = 1)
      openxlsx::writeData(wb, sheet_name,
                          x = "No data available for this statement.",
                          startRow = 3, startCol = 1)
      return()
    }

    openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)

    n_rows <- nrow(df)
    n_cols <- ncol(df)
    data_start_row <- 5

    # Title and metadata
    openxlsx::writeData(wb, sheet_name,
                        x = paste(company_name, "\u2013", title),
                        startRow = 1, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, title_style, rows = 1, cols = 1)

    openxlsx::writeData(wb, sheet_name,
                        x = paste0(units_note, "  |  Source: SEC EDGAR (data.sec.gov)  |  Annual 10-K filings"),
                        startRow = 2, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, subtitle_style, rows = 2, cols = 1)

    # Data table
    openxlsx::writeData(wb, sheet_name, x = df,
                        startRow = data_start_row, startCol = 1,
                        headerStyle = header_style)

    # Row-by-row formatting
    for (i in seq_len(n_rows)) {
      row_num  <- data_start_row + i
      label    <- tolower(df$`Line Item`[i])
      is_total <- any(stringr::str_detect(label, total_keywords))
      is_eps   <- stringr::str_detect(label, "eps")

      # Label column
      openxlsx::addStyle(wb, sheet_name,
                         if (is_total) total_label_style else label_style,
                         rows = row_num, cols = 1)

      # Number columns
      for (j in 2:n_cols) {
        openxlsx::addStyle(wb, sheet_name,
                           if (is_total) total_style
                           else if (is_eps) decimal_style
                           else number_style,
                           rows = row_num, cols = j)
      }
    }

    # Column widths
    openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = 35)
    openxlsx::setColWidths(wb, sheet_name, cols = 2:n_cols, widths = 14)

    # Freeze panes: keep line item column and header visible while scrolling
    openxlsx::freezePane(wb, sheet_name,
                         firstActiveRow = data_start_row + 1,
                         firstActiveCol = 2)

    # Source note at the bottom
    source_row <- data_start_row + n_rows + 2
    openxlsx::writeData(wb, sheet_name,
                        x = paste0("Downloaded: ", Sys.Date(),
                                   "  |  Values as reported in USD. Totals row may differ ",
                                   "from sum of components due to rounding or unreported sub-items."),
                        startRow = source_row, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, source_style,
                       rows = source_row, cols = 1)
  }

  # ── Write each sheet ───────────────────────────────────────────────────────

  write_sheet(wb, "Income Statement",  income_stmt,
              paste(company_name, "\u2013 Income Statement"),
              "Values in USD (as reported)")

  write_sheet(wb, "Balance Sheet",     balance_sheet,
              paste(company_name, "\u2013 Balance Sheet"),
              "Values in USD (as reported)")

  write_sheet(wb, "Cash Flow",         cash_flow,
              paste(company_name, "\u2013 Cash Flow Statement"),
              "Values in USD (as reported)")

  # ── Save ──────────────────────────────────────────────────────────────────

  filename <- file.path(
    output_dir,
    paste0(toupper(ticker), "_Financial_Statements_", Sys.Date(), ".xlsx")
  )

  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  message("Saved: ", filename)
  invisible(filename)
}
