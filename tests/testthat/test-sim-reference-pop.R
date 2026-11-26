# sim_reference_pop() samples hair, then skin | hair, then eye | hair, skin.
# The tables are fixed inside the function, so with a large n the sample
# frequencies should be close to them.

test_that("sim_reference_pop returns the documented shape", {
  pop <- sim_reference_pop(n = 50, seed = 1)

  expect_s3_class(pop, "data.frame")
  expect_named(pop, c("hair_colour", "skin_colour", "eye_colour"))
  expect_equal(nrow(pop), 50)
  for (col in names(pop)) {
    expect_true(all(pop[[col]] %in% 1:5))
  }
})

test_that("sim_reference_pop is reproducible through seed", {
  expect_identical(sim_reference_pop(n = 200, seed = 7),
                   sim_reference_pop(n = 200, seed = 7))
  expect_false(identical(sim_reference_pop(n = 200, seed = 7),
                         sim_reference_pop(n = 200, seed = 8)))
})

test_that("sim_reference_pop frequencies follow its probability tables", {
  n <- 20000
  pop <- sim_reference_pop(n = n, seed = 42)
  # 5 standard errors, so this only fails if a table is actually wrong
  close <- function(x, p, m) {
    expect_lt(abs(mean(x) - p), 5 * sqrt(p * (1 - p) / m))
  }

  # P(hair) = 0.4, 0.3, 0.15, 0.1, 0.05
  hair_p <- c(0.4, 0.3, 0.15, 0.1, 0.05)
  for (h in 1:5) close(pop$hair_colour == h, hair_p[h], n)

  # P(skin = 1 | hair = 3) = 0.60, P(skin = 3 | hair = 1) = 0.40
  h3 <- pop$hair_colour == 3
  close(pop$skin_colour[h3] == 1, 0.60, sum(h3))
  h1 <- pop$hair_colour == 1
  close(pop$skin_colour[h1] == 3, 0.40, sum(h1))

  # P(eye = 1 | hair = 1, skin = 3) = 0.70
  h1s3 <- h1 & pop$skin_colour == 3
  close(pop$eye_colour[h1s3] == 1, 0.70, sum(h1s3))
})

test_that("sim_reference_pop rejects bad n", {
  expect_error(sim_reference_pop(n = 0), "positive integer")
  expect_error(sim_reference_pop(n = "10"), "positive integer")
})
