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
