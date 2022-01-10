d_jaccard_circular <- circularize(d_jaccard, max_shift = 4, return_min = FALSE)
a <- sample(c(1, 2, 3), size = 20, replace = TRUE)
b <- sample(c(1, 2, 3), size = 20, replace = TRUE)

ref <- c(
  d_jaccard(a, b),
  d_jaccard(a, b[c(2:20, 1)]),
  d_jaccard(a, b[c(3:20, 1:2)]),
  d_jaccard(a, b[c(4:20, 1:3)])
)

new <- d_jaccard_circular(a, b)

test_that("circularize works with d_jaccard", {
  expect_equal(ref, new)
})
