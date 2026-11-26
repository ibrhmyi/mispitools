# compute_conditioned_prop() gives P(observed hair, skin, eye | H1) for each
# combination present in the data: (1 - e) for a trait that matches the MP,
# e for one that doesn't, multiplied over the three traits and normalised.

test_that("compute_conditioned_prop matches the hand calculation", {
  # 5 people, 4 distinct combinations; MP is (1, 1, 1), all errors 0.1
  d <- data.frame(hair_colour = c(1, 1, 1, 2, 3),
                  skin_colour = c(1, 1, 2, 2, 1),
                  eye_colour  = c(1, 1, 1, 2, 1))
  out <- compute_conditioned_prop(d, h = 1, s = 1, y = 1,
                                  eh = 0.1, es = 0.1, ey = 0.1)

  expect_named(out, c("hair_colour", "skin_colour", "eye_colour", "numerators"))
  # duplicates are dropped: one row per combination
  expect_equal(nrow(out), 4)

  # unnormalised: (1,1,1) 0.729, (1,2,1) 0.081, (2,2,2) 0.001, (3,1,1) 0.081
  total <- 0.729 + 0.081 + 0.001 + 0.081
  key <- paste(out$hair_colour, out$skin_colour, out$eye_colour)
  p <- setNames(out$numerators, key)
  expect_equal(p[["1 1 1"]], 0.729 / total)
  expect_equal(p[["1 2 1"]], 0.081 / total)
  expect_equal(p[["2 2 2"]], 0.001 / total)
  expect_equal(p[["3 1 1"]], 0.081 / total)
  expect_equal(sum(out$numerators), 1)
})

test_that("compute_conditioned_prop uses each trait's own error rate", {
  d <- data.frame(hair_colour = c(1, 2, 1, 1),
                  skin_colour = c(1, 1, 2, 1),
                  eye_colour  = c(1, 1, 1, 2))
  out <- compute_conditioned_prop(d, h = 1, s = 1, y = 1,
                                  eh = 0.1, es = 0.2, ey = 0.3)
  key <- paste(out$hair_colour, out$skin_colour, out$eye_colour)
  p <- setNames(out$numerators, key)

  raw <- c(`1 1 1` = 0.9 * 0.8 * 0.7,  # all match
           `2 1 1` = 0.1 * 0.8 * 0.7,  # hair wrong
           `1 2 1` = 0.9 * 0.2 * 0.7,  # skin wrong
           `1 1 2` = 0.9 * 0.8 * 0.3)  # eye wrong
  expect_equal(p[names(raw)], raw / sum(raw))
})

test_that("compute_conditioned_prop with no errors keeps only the exact match", {
  d <- data.frame(hair_colour = c(1, 2, 3),
                  skin_colour = c(4, 4, 4),
                  eye_colour  = c(5, 5, 5))
  out <- compute_conditioned_prop(d, h = 2, s = 4, y = 5,
                                  eh = 0, es = 0, ey = 0)
  expect_equal(out$numerators[out$hair_colour == 2], 1)
  expect_equal(sum(out$numerators[out$hair_colour != 2]), 0)
})

test_that("compute_conditioned_prop warns when nothing can match", {
  d <- data.frame(hair_colour = 1, skin_colour = 1, eye_colour = 1)
  expect_warning(out <- compute_conditioned_prop(d, h = 2, s = 1, y = 1,
                                                 eh = 0, es = 0, ey = 0),
                 "All numerators are zero")
  expect_equal(out$numerators, 0)
})

test_that("compute_conditioned_prop works on sim_reference_pop output", {
  pop <- sim_reference_pop(n = 300, seed = 1)
  out <- compute_conditioned_prop(pop, h = 1, s = 1, y = 1,
                                  eh = 0.01, es = 0.01, ey = 0.01)

  expect_equal(nrow(out), nrow(unique(pop)))
  expect_equal(sum(out$numerators), 1)
  # the MP's own combination is the most likely one, if it is in the data
  if (any(out$hair_colour == 1 & out$skin_colour == 1 & out$eye_colour == 1)) {
    i <- which.max(out$numerators)
    expect_equal(c(out$hair_colour[i], out$skin_colour[i], out$eye_colour[i]),
                 c(1, 1, 1))
  }
})

test_that("compute_conditioned_prop rejects bad input", {
  d <- data.frame(hair_colour = 1, skin_colour = 1, eye_colour = 1)
  args <- list(h = 1, s = 1, y = 1, eh = 0.1, es = 0.1, ey = 0.1)
  call_with <- function(data = d, ...) {
    a <- modifyList(args, list(...))
    do.call(compute_conditioned_prop, c(list(data = data), a))
  }

  expect_error(call_with(data = list(a = 1)), "data.frame")
  expect_error(call_with(data = data.frame(hair_colour = 1)), "skin_colour")
  expect_error(call_with(h = 0), "h \\(hair color\\)")
  expect_error(call_with(s = 6), "s \\(skin color\\)")
  expect_error(call_with(y = "1"), "y \\(eye color\\)")
  expect_error(call_with(eh = -0.1), "eh \\(hair error rate\\)")
  expect_error(call_with(es = 2), "es \\(skin error rate\\)")
  expect_error(call_with(ey = 1.1), "ey \\(eye error rate\\)")
})
