test_that("lookup_company requires user_agent", {
  expect_error(lookup_company("AAPL"), "user_agent")
  expect_error(lookup_company("AAPL", ""), "user_agent")
})

test_that("get_financials requires user_agent", {
  expect_error(get_financials("AAPL"), "user_agent")
})

test_that("get_financials validates n_years", {
  expect_error(
    get_financials("AAPL", "Test test@test.com", n_years = -1),
    "n_years"
  )
})

test_that("get_financials validates output_dir", {
  expect_error(
    get_financials("AAPL", "Test test@test.com", output_dir = "/nonexistent/path"),
    "Output directory"
  )
})

test_that("get_financials validates identifier", {
  expect_error(
    get_financials("", "Test test@test.com"),
    "identifier"
  )
})
