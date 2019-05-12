context("test-gcf_grid")

test_that("gcf_grid works", {
  x <- y <- seq(-4*pi, 4*pi, len = 27)
  r <- sqrt(outer(x^2, y^2, "+"))
  expect_error(p <- gcf_grid(cos(r^2)*exp(-r/(2*pi))), NA)
  expect_is(p, "gg")
  expect_is(p, "ggplot")
})
