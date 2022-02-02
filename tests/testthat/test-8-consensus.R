a <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4)
b <- c(3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1)
b_should_be <- c(1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4)
b_match1 <- match_labels(a, b)
b_match2 <- match_labels(a, b, strict = TRUE)

test_that("match_labels works properly", {

  expect_equal(
    b_match1,
    as.character(b_should_be)
  )

  expect_equal(
    b_match2,
    as.character(b_should_be)
  )
})

mat <- data.frame(a, b, a, b)
mat_ref1 <- as_consensus_matrix(mat, ref_col = 1, strict = FALSE)
mat_ref2 <- as_consensus_matrix(mat, ref_col = 2, strict = FALSE)

test_that("as_consensus_matrix works properly", {

  expect_equal(
    mat_ref1$b, # when a is reference
    factor(b_should_be, levels = 1:4)
  )

  expect_equal(
    mat_ref2$b, # when b is reference
    factor(b, levels = 1:3)
  )
})
