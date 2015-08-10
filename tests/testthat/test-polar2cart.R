
context("polar2cart")

test_that("Correct function", {
  expect_equal(polar2cart(1, 1, 2, 90), data.frame(x=3, y=1, stringsAsFactors=F))
  expect_equal(polar2cart(1, 1, 0, 90), data.frame(x=1, y=1, stringsAsFactors=F))
  expect_equal(polar2cart(1, 1, 1, 0), data.frame(x=1, y=2, stringsAsFactors=F))
  expect_equal(polar2cart(0,0,5,36.87, rnd=0), data.frame(x=3, y=4, stringsAsFactors=F))
})
