A <- c(0, 0, 0, 0, 1, 1, 1, 1)
B <- c(1, 1, 1, 1, 0, 0, 0, 0)
C <- rep(0, 8)
D <- 0:7

test_that("adjusted Rand difference works", {

  expect_equal(
    d_adjRand(A, B),
    0
  )

  expect_equal(
    d_adjRand(C, D),
    1
  )

  expect_gt(
    d_adjRand(A, C),
    d_adjRand(A, D)
  )
})

L <- c(1, 1, -1, -1, NA, NA, NA, NA)
M <- c(-1, -1, 1, 1, NA, NA, NA, NA)
N <- c(NA, NA, NA, NA, 1, 1, -1, -1)
O <- rep(NA, 8)

test_that("adjusted Rand difference ignores NAs", {

  expect_equal(
    d_adjRand(L, M),
    0
  )

  expect_equal(
    d_adjRand(C, O),
    1
  )

  expect_gt(
    d_adjRand(M, N),
    d_adjRand(M, O)
  )
})
