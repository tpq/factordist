a <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4)
b <- c(3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4, 1, 1, 1, 1)
b_should_be <- a
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
mat_ref1 <- as_consensus_matrix(mat, ref_col = 1, strict = TRUE)
mat_ref2 <- as_consensus_matrix(mat, ref_col = 2, strict = TRUE)

test_that("as_consensus_matrix works properly", {

  expect_equal(
    as.numeric(mat_ref1$b), # when a is reference
    b_should_be
  )

  expect_equal(
    as.numeric(mat_ref2$b), # when b is reference
    b
  )
})

mat[,1] <- c(rep(1, 7), rep(3, 5), rep(4, 4))
final_labels <- as_consensus_cluster(mat, ref_col = 2, wildcard_cutoff = 0.8, strict = FALSE)
final_labels_to_be <- c(rep("3", 4), rep(NA, 4), rep("4", 4), rep("1", 4))

test_that("as_consensus_cluster works properly", {

  expect_equal(
    final_labels,
    final_labels_to_be
  )
})
