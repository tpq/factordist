M <- matrix(sample(1:25), 5, 5)
M_sy <- sym(M)
M_mi <- sym_min(t(M))
M_ma <- sym_max(M)

test_that("sym functions work", {

  expect_equal(
    M_sy[1,2],
    M[2,1]
  )

  expect_equal(
    M_mi[1,2],
    min(M[1,2], M[2,1])
  )

  expect_equal(
    M_ma[1,2],
    max(M[1,2], M[2,1])
  )
})
