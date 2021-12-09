df <- data.frame(
  "A" = c(0, 0, 0, 0, 1, 1, 1, 1),
  "B" = c(1, 1, 1, 1, 0, 0, 0, 0),
  "C" = rep(0, 8),
  "D" = 0:7
)
df <- t(df)
label <- c("A", "B", "C", "D")

df2 <- data.frame(
  "L" = c(0, 0, 0, 0, 0, 1, 1, 1),
  "M" = c(0, 1, 1, 1, 0, 0, 0, 0),
  "N" = c(0, 0, 0, 0, 0, 0, 0, 1),
  "O" = rep(9, 8)
)
df2 <- t(df2)

m <- cluster_model(df, label, d_jaccard)
k <- predict(m, df2)
names(k) <- NULL

test_that("cluster_model works", {

  expect_equal(
    k,
    c("A", "B", "C", NA)
  )
})
