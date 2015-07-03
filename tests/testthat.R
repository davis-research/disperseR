library(testthat)
library(disperseR)

test_check("disperseR")

context("cart2polar")

test_that("cart2polar responses align in correct orientation", {
  expect_equal(cart2polar(0,0,2,2)$theta, 45)
  expect_equal(cart2polar(0,0,-2,2)$theta, 135)
  expect_equal(cart2polar(0,0,-2,-2)$theta, 225)
  expect_equal(cart2polar(0,0,2,-2)$theta, 315)
})

context("findRotation")

test_that("Results are expected", {
  expect_equal(findRotation(0,0,1,-1), 45)
  expect_equal(findRotation(0,2,0,0), 90)
  expect_error(findRotation(0,0,0,2))
})


context("rotatePlot")

test_that("Bad x/y values", {
  expect_warning(rotatePlot(data.frame(x=c(1,1:5), y=c(0,0:-4), stringsAsFactors=FALSE)), "You have repeat")
  expect_warning(rotatePlot(data.frame(x=c(1,1:5), y=c(0:-5), stringsAsFactors=FALSE)), "You have repeat")
  expect_error(rotatePlot(data.frame(x=c(1:6), y=c(-4,0:-4), stringsAsFactors=FALSE)), "Sorry")
  expect_error(rotatePlot(data.frame(x=c("a",1:5), y=0:-5, stringsAsFactors=FALSE)), "Sorry")
})

test_that("Correct function", {
  expect_equal(rotatePlot(data.frame(x=c(0,4), y=c(0,-3), stringsAsFactors=FALSE)), data.frame(x=c(5,0), y=c(1,3)))
})

context("polar2cart")

test_that("Correct function", {
  expect_equal(polar2cart(1, 1, 2, 90), data.frame(x=3, y=1, stringsAsFactors=F))
  expect_equal(polar2cart(1, 1, 0, 90), data.frame(x=1, y=1, stringsAsFactors=F))
  expect_equal(polar2cart(1, 1, 1, 0), data.frame(x=1, y=2, stringsAsFactors=F))
  expect_equal(polar2cart(0,0,5,36.87, rnd=0), data.frame(x=3, y=4, stringsAsFactors=F))
})

