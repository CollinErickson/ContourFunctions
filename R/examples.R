
x <- y <- seq(-4*pi, 4*pi, len = 27)
r <- sqrt(outer(x^2, y^2, "+"))
cf_grid(cos(r^2)*exp(-r/(2*pi)))


x <- runif(20)
y <- runif(20)
z <- exp(-(x-.5)^2-5*(y-.5)^2)
cf_data(x,y,z)

cf_func(function(xx){exp(-sum((xx-.5)^2))})
