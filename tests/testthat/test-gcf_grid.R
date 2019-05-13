context("test-gcf_grid")

test_that("gcf_grid works", {
  x <- y <- seq(-4*pi, 4*pi, len = 27)
  r <- sqrt(outer(x^2, y^2, "+"))
  expect_error(p <- gcf_grid(cos(r^2)*exp(-r/(2*pi))), NA)
  expect_is(p, "gg")
  expect_is(p, "ggplot")
  
  # x,y,z in one list
  expect_error(gcf_grid(list(x=x,y=y,z=cos(r^2)*exp(-r/(2*pi)))), NA)
  # x,y in one list, z separate
  expect_error(gcf_grid(list(x=x,y=y),z=cos(r^2)*exp(-r/(2*pi))), NA)
  # bad x/y values
  expect_error(gcf_grid(rev(x), y, cos(r^2)*exp(-r/(2*pi))))
  expect_error(gcf_grid(x, rev(y), cos(r^2)*exp(-r/(2*pi))))
  # Bar
  expect_error(gcf_grid(cos(r^2)*exp(-r/(2*pi)), bar=T), NA)
  # pts
  expect_error(gcf_grid(cos(r^2)*exp(-r/(2*pi)), pts=matrix(c(.1,.2,.3,.4),2,2)), NA)
  # Can't reset.par
  expect_error(gcf_grid(cos(r^2)*exp(-r/(2*pi)), reset.par = F))
  # afterplotfunc, not useful, maybe should remove
  expect_error(gcf_grid(cos(r^2)*exp(-r/(2*pi)), afterplotfunc = function(){print("after")}), NA)
})
