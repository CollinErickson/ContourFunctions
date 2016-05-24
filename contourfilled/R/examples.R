
x <- y <- seq(-4*pi, 4*pi, len = 27)
r <- sqrt(outer(x^2, y^2, "+"))
contourfilled(cos(r^2)*exp(-r/(2*pi)))


x <- runif(20)
y <- runif(20)
z <- exp(-(x-.5)^2-5*(y-.5)^2)
contourfilled.data(x,y,z)
