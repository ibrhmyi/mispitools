# cpt_missing_person() is P(observed sex, age group, hair colour | H1):
# the sex/age errors times the hair error-matrix row of the MP's colour.
# It is soft-deprecated, so the one-time notice is silenced here.

cpt_mp <- function(...) suppressMessages(cpt_missing_person(...))

# simple hair error matrix: 0.8 on the diagonal, 0.05 elsewhere
epc_simple <- matrix(0.05, 5, 5) + diag(0.75, 5)

test_that("cpt_missing_person matches the hand calculation for a female MP", {
  out <- cpt_mp(MPs = "F", MPc = 2, eps = 0.1, epa = 0.2, epc = epc_simple)
  sex_age <- c(`F-T1` = 0.9 * 0.8, `F-T0` = 0.9 * 0.2,
               `M-T1` = 0.1 * 0.8, `M-T0` = 0.1 * 0.2)
  hair <- c(0.05, 0.8, 0.05, 0.05, 0.05)
  expected <- outer(sex_age, hair)
  colnames(expected) <- as.character(1:5)

  expect_equal(out, expected)
  expect_equal(out["F-T1", "2"], 0.576)
  expect_equal(out["M-T0", "1"], 0.001)
})

test_that("cpt_missing_person for a male MP swaps the sex rows", {
  f <- cpt_mp(MPs = "F", MPc = 3, eps = 0.1, epa = 0.2, epc = epc_simple)
  m <- cpt_mp(MPs = "M", MPc = 3, eps = 0.1, epa = 0.2, epc = epc_simple)

  expect_equal(unname(m[c("M-T1", "M-T0"), ]), unname(f[c("F-T1", "F-T0"), ]))
  expect_equal(unname(m[c("F-T1", "F-T0"), ]), unname(f[c("M-T1", "M-T0"), ]))
  expect_equal(m["M-T1", "3"], 0.9 * 0.8 * 0.8)
})

test_that("cpt_missing_person default uses error_matrix_hair()", {
  out <- cpt_mp()
  epc <- error_matrix_hair()

  expect_equal(dim(out), c(4, 5))
  expect_equal(rownames(out), c("F-T1", "F-T0", "M-T1", "M-T0"))
  expect_equal(sum(out), 1)
  # hair margin is row 1 (black) of the error matrix
  expect_equal(unname(colSums(out)), unname(epc[1, ]))
  expect_equal(unname(rowSums(out)),
               c(0.95 * 0.95, 0.95 * 0.05, 0.05 * 0.95, 0.05 * 0.05))
})

test_that("cpt_missing_person accepts numeric sex codes", {
  expect_equal(cpt_mp(MPs = 1), cpt_mp(MPs = "F"))
  expect_equal(cpt_mp(MPs = 2), cpt_mp(MPs = "M"))
})

test_that("cpt_missing_person with no errors puts all mass on one cell", {
  out <- cpt_mp(MPs = "M", MPc = 4, eps = 0, epa = 0, epc = diag(5))
  expect_equal(out["M-T1", "4"], 1)
  expect_equal(sum(out), 1)
})

test_that("cpt_missing_person normalises an error matrix whose rows don't sum to 1", {
  expect_warning(out <- cpt_mp(MPc = 2, epc = epc_simple * 2),
                 "rows do not sum to 1")
  expect_equal(out, cpt_mp(MPc = 2, epc = epc_simple))
})

test_that("cpt_missing_person rejects bad input", {
  expect_error(cpt_mp(MPs = "X"), "'F'")
  expect_error(cpt_mp(MPs = 3), "1 \\(Female\\) or 2")
  expect_error(cpt_mp(MPc = 6), "1 to 5")
  expect_error(cpt_mp(MPc = c(1, 2)), "1 to 5")
  expect_error(cpt_mp(eps = 1.5), "eps must be")
  expect_error(cpt_mp(epa = -0.1), "epa must be")
  expect_error(cpt_mp(epc = diag(4)), "5x5")
})
