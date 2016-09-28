
a <- b <- seq(-4*pi, 4*pi, len = 27)
r <- sqrt(outer(a^2, b^2, "+"))
cf_grid3(cos(r^2)*exp(-r/(2*pi)))
cf_grid3(cos(r^2)*exp(-r/(2*pi)), bar=T)


x <- runif(20)
y <- runif(20)
z <- exp(-(x-.5)^2-5*(y-.5)^2)# + rnorm(20,0,.05)
cf_data(x,y,z)
cf_data(x,y,z, bar=T)

cf_func(function(xx){exp(-sum((xx-.5)^2/.1))})
cf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, bar=T)


split.screen(c(2,2))
screen(1)
cf_grid3(cos(r^2)*exp(-r/(2*pi)), bar=T)
close.screen(1)
screen(2)
cf_func(function(xx){exp(-sum((xx-.5)^2/.1))}, bar=T)
close.screen(2)
