context("non-ggplot tests")

test_that("cf_grid", {
  a <- b <- seq(-4*pi, 4*pi, len = 27)
  r <- sqrt(outer(a^2, b^2, "+"))
  cf_grid(cos(r^2)*exp(-r/(2*pi)))
  cf_grid(cos(r^2)*exp(-r/(2*pi)), bar=T)
  cf_grid(cos(r^2)*exp(-r/(2*pi)), bar=T, mainminmax = T)
  
})

test_that("cf_data", {
  x <- runif(20)
  y <- runif(20)
  z <- exp(-(x-.5)^2-5*(y-.5)^2)# + rnorm(20,0,.05)
  cf_data(x,y,z)
  cf_data(x,y,z, bar=T)
  cf(x,y,z)
})

test_that("cf_func", {
  cf_func(function(xx){exp(-sum((xx-.5)^2/.1))})
  cf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, bar=T)
  cf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, main="Title check")
  cf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, bar=T, main="Title check")
  cf_func(function(xx){exp(-sum((xx-.5)^2/.1))},pts=matrix(c(.2,.2,.2,.8,.8,.2,.8,.8),byrow=T,ncol=2))
  cf(function(xx){exp(-sum((xx-.5)^2/.1))})
  })