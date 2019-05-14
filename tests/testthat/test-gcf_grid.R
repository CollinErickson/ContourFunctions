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
  expect_error(gcf_grid(cos(r^2)*exp(-r/(2*pi)), pts=c(.1,.2,.3,.4)), NA)
  # Can't reset.par
  expect_error(gcf_grid(cos(r^2)*exp(-r/(2*pi)), reset.par = F))
  # afterplotfunc, not useful, maybe should remove
  expect_error(gcf_grid(cos(r^2)*exp(-r/(2*pi)), afterplotfunc = function(){("after")}), NA)
  
  # Lines
  expect_error(gcf_grid(cos(r^2)*exp(-r/(2*pi)), lines_only = T), NA)
  expect_error(gcf_grid(cos(r^2)*exp(-r/(2*pi)), with_lines = T), NA)
  expect_error(gcf_grid(cos(r^2)*exp(-r/(2*pi)), with_lines = T, lines_only = T), NA)
  
  # Errors
  expect_error(gcf_grid()) # No z given
  expect_error(gcf_grid(x)) # No z given
  expect_error(gcf_grid(rev(x), y, cos(r^2)*exp(-r/(2*pi))))
  expect_error(gcf_grid(x, rev(y), cos(r^2)*exp(-r/(2*pi))))
  
})

test_that("gcf_func", {
  p1 <- gcf_func(function(xx){exp(-sum((xx-.5)^2/.1))})
  p2 <- cf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, gg=T)
  expect_is(p1, "gg")
  expect_is(p2, "gg")
})

test_that("gcf_data", {
  x <- runif(20)
  y <- runif(20)
  z <- exp(-(x-.5)^2-5*(y-.5)^2)# + rnorm(20,0,.05)
  p1 <- gcf_data(x,y,z)
  p2 <- cf_data(x,y,z, gg=T)
  expect_is(p1, "gg")
  expect_is(p2, "gg")
})

test_that("gcf", {
  # Func
  p1 <- gcf(function(xx){exp(-sum((xx-.5)^2/.1))})
  p2 <- cf(function(xx){exp(-sum((xx-.5)^2/.1))}, gg=T)
  expect_is(p1, "gg")
  expect_is(p2, "gg")
  
  # Data
  x <- runif(20)
  y <- runif(20)
  z <- exp(-(x-.5)^2-5*(y-.5)^2)# + rnorm(20,0,.05)
  p3 <- gcf_data(x,y,z)
  p4 <- cf_data(x,y,z, gg=T)
  expect_is(p3, "gg")
  expect_is(p4, "gg")
})