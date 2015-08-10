context("findRotation")

test_that("Results are expected", {
  expect_equal(findRotation(0,0,1,-1), 45)
  expect_equal(findRotation(0,2,0,0), 90)
  expect_equal(findRotation(0,0,0,2), 270)
})
