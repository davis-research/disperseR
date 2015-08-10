context("buildBoxes")

test_that("buildBoxes formulates correctly", {
  expect_equal(buildBoxes(getSubplotCoords())[1,1], 0)
  expect_equal(buildBoxes(getSubplotCoords())[64,3], 16)
})
