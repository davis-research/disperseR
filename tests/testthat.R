library(testthat)
library(disperseR)

test_check("disperseR")

context("cart2polar")

test_that("responses align in correct group", {
  expect_equal(cart2polar(0,0,2,2)$theta, 45)
  expect_equal(cart2polar(0,0,-2,2)$theta, 135)
  expect_equal(cart2polar(0,0,-2,-2)$theta, 225)
  expect_equal(cart2polar(0,0,2,-2)$theta, 315)
})
