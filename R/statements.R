#' Build a financial statement from company facts
#'
#' Internal function that pulls a list of XBRL concepts, merges them by date,
#' filters to annual 10-K data, and returns a wide-format data frame with
#' fiscal years as columns and line items as rows.
#'
#' @param facts The company facts list from .get_company_facts()
#' @param concepts A named character vector where names are display labels
#'   and values are XBRL concept names. If multiple concepts are provided
#'   for one label, the first non-NULL result is used.
#' @param n_years Number of fiscal years to include (default: 10)
#'
#' @return A data frame with line items as rows and fiscal years as columns,
#'   or NULL if no data could be retrieved.
#' @keywords internal
.build_statement <- function(facts, concepts, n_years = 10) {

  results <- list()

  for (label in names(concepts)) {
    candidates <- concepts[[label]]
    for (concept in candidates) {
      df <- .get_concept(facts, concept)
      if (!is.null(df)) {
        # Rename the value column to the display label
        names(df)[2] <- label
        results[[label]] <- df
        break
      }
    }
  }

  if (length(results) == 0) return(NULL)

  # Merge all concepts by date
  merged <- Reduce(function(a, b) dplyr::full_join(a, b, by = "date"), results)

  # Keep most recent n_years, sort newest to oldest (left to right)
  merged <- merged %>%
    dplyr::arrange(dplyr::desc(date)) %>%
    dplyr::slice_head(n = n_years)

  # Transpose: line items as rows, years as columns
  t_df <- merged %>%
    tidyr::pivot_longer(-date, names_to = "Line Item", values_to = "value") %>%
    tidyr::pivot_wider(names_from = date, values_from = value) %>%
    dplyr::mutate(`Line Item` = factor(`Line Item`, levels = names(concepts))) %>%
    dplyr::arrange(`Line Item`) %>%
    dplyr::mutate(`Line Item` = as.character(`Line Item`))

  return(t_df)
}


# ── Concept definitions ────────────────────────────────────────────────────────
# Each entry is a named list: display label -> vector of possible XBRL concepts
# (in priority order, first match wins). This handles the fact that companies
# use different concept names for the same line item.

.income_concepts <- list(
  "Revenue" = c(
    "RevenueFromContractWithCustomerExcludingAssessedTax",
    "Revenues",
    "SalesRevenueNet",
    "SalesRevenueGoodsNet"
  ),
  "Cost of Revenue" = c(
    "CostOfGoodsSold",
    "CostOfRevenue",
    "CostOfGoodsAndServicesSold"
  ),
  "Gross Profit" = c(
    "GrossProfit"
  ),
  "R&D Expense" = c(
    "ResearchAndDevelopmentExpense"
  ),
  "SG&A Expense" = c(
    "SellingGeneralAndAdministrativeExpense",
    "GeneralAndAdministrativeExpense"
  ),
  "Operating Income" = c(
    "OperatingIncomeLoss"
  ),
  "Interest Expense" = c(
    "InterestExpense",
    "InterestAndDebtExpense"
  ),
  "Pre-Tax Income" = c(
    "IncomeLossFromContinuingOperationsBeforeIncomeTaxesExtraordinaryItemsNoncontrollingInterest",
    "IncomeLossFromContinuingOperationsBeforeIncomeTaxesMinorityInterestAndIncomeLossFromEquityMethodInvestments"
  ),
  "Income Tax Expense" = c(
    "IncomeTaxExpenseBenefit"
  ),
  "Net Income" = c(
    "NetIncomeLoss",
    "NetIncome"
  ),
  "EPS (Basic)" = c(
    "EarningsPerShareBasic"
  ),
  "EPS (Diluted)" = c(
    "EarningsPerShareDiluted"
  ),
  "Shares Outstanding (Basic)" = c(
    "CommonStockSharesOutstanding",
    "WeightedAverageNumberOfSharesOutstandingBasic"
  ),
  "Shares Outstanding (Diluted)" = c(
    "WeightedAverageNumberOfDilutedSharesOutstanding"
  )
)

.balance_concepts <- list(
  # Current Assets
  "Cash & Equivalents" = c(
    "CashAndCashEquivalentsAtCarryingValue",
    "CashAndCashEquivalentsFairValueDisclosure"
  ),
  "Short-Term Investments" = c(
    "MarketableSecuritiesCurrent",
    "ShortTermInvestments",
    "AvailableForSaleSecuritiesCurrent"
  ),
  "Accounts Receivable" = c(
    "AccountsReceivableNetCurrent",
    "ReceivablesNetCurrent"
  ),
  "Inventory" = c(
    "InventoryNet"
  ),
  "Other Current Assets" = c(
    "OtherAssetsCurrent"
  ),
  "Total Current Assets" = c(
    "AssetsCurrent"
  ),
  # Non-Current Assets
  "Property, Plant & Equipment (net)" = c(
    "PropertyPlantAndEquipmentNet"
  ),
  "Goodwill" = c(
    "Goodwill"
  ),
  "Intangible Assets" = c(
    "FiniteLivedIntangibleAssetsNet",
    "IntangibleAssetsNetExcludingGoodwill"
  ),
  "Long-Term Investments" = c(
    "MarketableSecuritiesNoncurrent",
    "LongTermInvestments"
  ),
  "Other Non-Current Assets" = c(
    "OtherAssetsNoncurrent"
  ),
  "Total Assets" = c(
    "Assets"
  ),
  # Current Liabilities
  "Accounts Payable" = c(
    "AccountsPayableCurrent"
  ),
  "Short-Term Debt" = c(
    "ShortTermBorrowings",
    "CommercialPaper"
  ),
  "Other Current Liabilities" = c(
    "OtherLiabilitiesCurrent"
  ),
  "Total Current Liabilities" = c(
    "LiabilitiesCurrent"
  ),
  # Non-Current Liabilities
  "Long-Term Debt" = c(
    "LongTermDebtNoncurrent",
    "LongTermDebt"
  ),
  "Other Non-Current Liabilities" = c(
    "OtherLiabilitiesNoncurrent"
  ),
  "Total Liabilities" = c(
    "Liabilities"
  ),
  # Equity
  "Common Stock & APIC" = c(
    "CommonStockholdersEquity",
    "AdditionalPaidInCapital"
  ),
  "Retained Earnings" = c(
    "RetainedEarningsAccumulatedDeficit"
  ),
  "Total Stockholders Equity" = c(
    "StockholdersEquity",
    "StockholdersEquityIncludingPortionAttributableToNoncontrollingInterest"
  )
)

.cashflow_concepts <- list(
  # Operating
  "Net Income" = c(
    "NetIncomeLoss"
  ),
  "Depreciation & Amortization" = c(
    "DepreciationDepletionAndAmortization",
    "Depreciation"
  ),
  "Stock-Based Compensation" = c(
    "ShareBasedCompensation"
  ),
  "Changes in Working Capital" = c(
    "IncreaseDecreaseInOperatingCapital"
  ),
  "Cash from Operations" = c(
    "NetCashProvidedByUsedInOperatingActivities"
  ),
  # Investing
  "Capital Expenditures" = c(
    "PaymentsToAcquirePropertyPlantAndEquipment"
  ),
  "Acquisitions" = c(
    "PaymentsToAcquireBusinessesNetOfCashAcquired",
    "PaymentsToAcquireBusinessesGross"
  ),
  "Purchases of Investments" = c(
    "PaymentsToAcquireInvestments",
    "PaymentsToAcquireAvailableForSaleSecurities"
  ),
  "Sales of Investments" = c(
    "ProceedsFromSaleOfAvailableForSaleSecurities",
    "ProceedsFromMaturitiesPrepaymentsAndCallsOfAvailableForSaleSecurities"
  ),
  "Cash from Investing" = c(
    "NetCashProvidedByUsedInInvestingActivities"
  ),
  # Financing
  "Debt Issuance" = c(
    "ProceedsFromIssuanceOfLongTermDebt",
    "ProceedsFromIssuanceOfDebt"
  ),
  "Debt Repayment" = c(
    "RepaymentsOfLongTermDebt",
    "RepaymentsOfDebt"
  ),
  "Share Repurchases" = c(
    "PaymentsForRepurchaseOfCommonStock"
  ),
  "Dividends Paid" = c(
    "PaymentsOfDividends",
    "PaymentsOfDividendsCommonStock"
  ),
  "Cash from Financing" = c(
    "NetCashProvidedByUsedInFinancingActivities"
  ),
  # Net change
  "Net Change in Cash" = c(
    "CashAndCashEquivalentsPeriodIncreaseDecrease",
    "EffectOfExchangeRateOnCashAndCashEquivalents"
  )
)
