context("cleanResponse")

test_that("cleanResponse works correctly", {
  expect_equal(cleanResponse(data.frame(col1=c(NA, 1), col2=c(NA,2)), 1),
               data.frame(col1=1, col2=2))
  expect_equal(cleanResponse(data.frame(col1=c(1, 2), col2=c(2, NA)), 2),
               data.frame(col1=1, col2=2))
  expect_equal(cleanResponse(data.frame(col1=c(NA, 1, 2), col2=c(NA,2, 3)), c(1,2)),
               data.frame(col1=2, col2=3))
})

test_that("cleanResponse throws errors", {
  expect_error(cleanResponse(data.frame(col1=NA, col2=NA), 1))
  expect_error(cleanResponse(data.frame()))

  })

test_that("cleanResponse functions correctly", {
  expect_equal(cleanResponse(data.frame(x=c(1,2), y=c(1,2))),
               cleanResponse(data.frame(x=c(1,2), y=c(1,2))))
  expect_equal(cleanResponse(data.frame(x=c(1,2), y=c(1,2)), rm.rows=1),
               cleanResponse(data.frame(x=c(2), y=c(2))))
})
