df <- data.frame(
  "A" = c(0, 0, 0, 0, 1, 1, 1, 1),
  "B" = c(1, 1, 1, 1, 0, 0, 0, 0),
  "C" = rep(0, 8),
  "D" = 0:7
)
MAT <- papply(df, MARGIN = 2, FUN = d_adjRand)

test_that("papply works", {

  expect_equal(
    d_adjRand(df$A, df$B),
    MAT["A", "B"]
  )

  expect_equal(
    d_adjRand(df$B, df$C),
    MAT["B", "C"]
  )

  expect_equal(
    d_adjRand(df$C, df$D),
    MAT["C", "D"]
  )
})
