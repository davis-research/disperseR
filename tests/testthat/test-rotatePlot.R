context("rotatePlot")

test_that("Bad x/y values", {
  expect_error(rotatePlot(data.frame(x=c("a",1:5), y=0:-5, stringsAsFactors=FALSE)), "Sorry")
})


test_that("Correct function", {
  expect_equal(rotatePlot(data.frame(x=c(0,4), y=c(0,-3), stringsAsFactors=FALSE)), data.frame(x=c(0, 5), y=c(0,0)))
})
