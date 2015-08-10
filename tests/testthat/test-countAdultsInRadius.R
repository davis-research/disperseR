context("countAdultsInRadius")

test_that("countAdultsInRadius functions correctly", {
  expect_equal(countAdultsInRadius(c(1,2), data.frame(x=c(1:5), y=c(1:5)), 6), 5)
})
