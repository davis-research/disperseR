context("cart2polar")

test_that("cart2polar functions properly", {
  expect_equal(cart2polar(1,1,c(-2,2,1),c(11,1,2))[3,4], 1)
  expect_equal(cart2polar(1,1,c(-2,2,1), c(11,1,2))[3,1], 0)
})

test_that("cart2polar responses align in correct orientation", {
  expect_equal(cart2polar(0,0,2,2)$theta, 45)
  expect_equal(cart2polar(0,0,-2,2)$theta, 135)
  expect_equal(cart2polar(0,0,-2,-2)$theta, 225)
  expect_equal(cart2polar(0,0,2,-2)$theta, 315)
})
