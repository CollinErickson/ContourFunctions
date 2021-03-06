context("non-ggplot tests")

test_that("cf_grid", {
  a <- b <- seq(-4*pi, 4*pi, len = 27)
  r <- sqrt(outer(a^2, b^2, "+"))
  cf_grid(cos(r^2)*exp(-r/(2*pi)))
  cf_grid(cos(r^2)*exp(-r/(2*pi)), bar=T)
  cf_grid(cos(r^2)*exp(-r/(2*pi)), bar=T, mainminmax = T)
  # Give in list x,y,z
  cf_grid(list(x=a,y=b,z=cos(r^2)*exp(-r/(2*pi))))
  # Give in nothing, get error
  expect_error(cf_grid())
  cf_grid(x=list(x=a,y=b), z=cos(r^2)*exp(-r/(2*pi)))
  expect_error(cf_grid(rev(x), y, cos(r^2)*exp(-r/(2*pi))))
  expect_error(cf_grid(x, rev(y), cos(r^2)*exp(-r/(2*pi))))
  expect_error(cf_grid(cos(r^2)*exp(-r/(2*pi)), xaxis=T, yaxis=F), NA)
  expect_error(cf_grid(cos(r^2)*exp(-r/(2*pi)), xaxis=F, yaxis=T), NA)
  expect_error(cf_grid(a,b,z=c(cos(r^2)*exp(-r/(2*pi)))))
  cf_grid(matrix(as.integer(3*cos(r^2)*exp(-r/(2*pi))),27,27))
  cf_grid(cos(r^2)*exp(-r/(2*pi)), pts=c(.1,.2,.3,.4,.5,.6))
  cf_grid(cos(r^2)*exp(-r/(2*pi)), afterplotfunc=function(){text(.4,.6,labels="Text added!")})
  expect_true(is.function(cf_grid(cos(r^2)*exp(-r/(2*pi)), reset.par = F)))
  
  # csa
  expect_error(csa(), NA)
  expect_error(csa(silent=T), NA)
})

test_that("cf_data", {
  x <- runif(20)
  y <- runif(20)
  z <- exp(-(x-.5)^2-5*(y-.5)^2)# + rnorm(20,0,.05)
  x2 <- runif(100)
  y2 <- runif(100)
  z2 <- exp(-(x2-.5)^2-5*(y2-.5)^2)# + rnorm(20,0,.05)
  cf_data(x,y,z)
  cf_data(x,y,z, bar=T)
  cf(x,y,z)
  cf_data(x,y,z, fit="mlegp")
  cf_data(x,y,z, xylim=c(-1,2))
  cf_data(cbind(x,y),y=z)
  cf_data(cbind(x,y),z=z)
  cf_data(cbind(x,y,z))
  # Fit with other packages
  expect_error(cf_data(x,y,z, fit="locfit"), NA)
  # gam needs more data to avoid error
  expect_error(cf_data(x2,y2,z2, fit="gam"), NA)
  # Fit binomial
  expect_error(cf_data(x2, y2, round(pmax(0, pmin(1, z2+rnorm(100,0,.3)))), family="binomial", fit="locfit"), NA)
  expect_error(cf_data(x2, y2, round(pmax(0, pmin(1, z2+rnorm(100,0,.3)))), family=binomial(), fit="gam"), NA)
  
  # Errors
  expect_error(cf_data(cbind(x,y,y),y=z))
  expect_error(cf_data(cbind(x,y,y),z=z))
  expect_error(cf_data(cbind(x,y)))
  expect_error(cf_data(cbind(x,y,z,z)))
})

test_that("cf_func", {
  cf_func(function(xx){exp(-sum((xx-.5)^2/.1))})
  cf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, bar=T)
  cf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, main="Title check")
  cf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, bar=T, main="Title check")
  cf_func(function(xx){exp(-sum((xx-.5)^2/.1))},pts=matrix(c(.2,.2,.2,.8,.8,.2,.8,.8),byrow=T,ncol=2))
  cf(function(xx){exp(-sum((xx-.5)^2/.1))})
  
  # xylim
  cf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, xylim = c(-1,2))
  
  # use lines
  cf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, use_lines = T)
  
  # out.col.name
  expect_error(cf_func(function(xx){data.frame(y1=exp(-sum((xx-.5)^2/.1)), y2=1)}))
  expect_error(cf_func(function(xx){data.frame(y1=exp(-sum((xx-.5)^2/.1)), y2=1)}, out.col.name = "y1"), NA)
  # out.name from list
  expect_error(cf_func(function(xx){list(y1=exp(-sum((xx-.5)^2/.1)), y2=1)}, out.name = "y1"), NA)
})

test_that("cf_4dim", {
  
  expect_error(cf_4dim(function(x) {x[1] + x[2]^2 + sin(2*pi*x[3])}), NA)
  
  # Add pts
  expect_error(cf_4dim(function(x) {x[1] + x[2]^2 + sin(2*pi*x[3])}, pts=matrix(c(.1,.2,.3,.4,.5,.6,.7,.8),2,4)), NA)
  expect_error(cf_4dim(function(x) {x[1] + x[2]^2 + sin(2*pi*x[3])}, pts=matrix(c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1),2,5)))
  
  # Not same scale, this is a lot slower
  expect_error(cf_4dim(function(x) {x[1] + x[2]^2 + sin(2*pi*x[3])}, same_scale = F), NA)
  
  expect_error(
    cf_4dim(function(x) x[1]*x[3] + sin(x[2]*x[4]), color.palette=heat.colors,
         nover1=3, nover2=8, cex.var_names = .5), NA)
         
  expect_error(
    cf_4dim(function(x) x[1]*x[3] + sin(x[2]*x[4]), color.palette=topo.colors,
         nover1=3, nover2=8, cex.var_names = 1, over_srt = c(90,0),
         edge_width=c(.1, .2), nlevels = 5),
    NA)
})

test_that("cf highdim", {
  
  expect_error(cf_highdim(function(x) {x[1]^2 + exp(x[2])}, D=3), NA)
  
  friedman <- function(x) {
   10*sin(pi*x[1]*x[2]) + 20*(x[3]-.5)^2 + 10*x[4] + 5*x[5]
  }
  expect_error(cf_highdim(friedman, 5, color.palette=topo.colors), NA)
  expect_error(cf_highdim(friedman, 5, 
            color.palette=function(x) {gray((1:x)/x)},
            nlevels=10),
            NA)

  expect_error({
    # Average over background dimensions, use higher reps to reduce noise.
    f1 <- function(x) {x[1] + x[2]^2 + x[3]^3}
    cf_highdim(f1, 4, average=TRUE, average_reps=1e2, n=10)
    f1b <- function(x) {x[,1] + x[,2]^2 + x[,3]^3}
    cf_highdim(f1b, 4, average=TRUE, average_reps=1e2, n=10, batchmax=Inf)
    cf_highdim(f1b, 4, average_reps=1e2, n=10, batchmax=Inf,
              color.palette = topo.colors, nlevels=3)
    }, NA)
  
  # Error when pts has wrong num of columns
  expect_error(cf_highdim(function(x) {x[1]^2 + exp(x[2])}, D=3, pts=matrix(1:4,2,2)))
  
  # Batch max between 1 and nrows
  expect_error(cf_highdim(function(x) {if (is.matrix(x)){x[,1]^2+exp(x[,2])} else {x[1]^2 + exp(x[2])}}, D=3, batchmax=10), NA)
  # Batch max between 1 and nrows AND average
  expect_error(cf_highdim(function(x) {if (is.matrix(x)){x[,1]^2+exp(x[,2])} else {x[1]^2 + exp(x[2])}}, D=3, batchmax=10, average=T), NA)
  
  # Not same scale
  expect_error(cf_highdim(function(x) {x[1]^2 + exp(x[2])}, D=3, same_scale = F), NA)
  
  # From cf
  expect_error(cf(function(x) {x[1]^2 + exp(x[2])}, D=3), NA)
})

test_that("cf error", {
  expect_error(cf("you shouldn't give in text here!"))
})