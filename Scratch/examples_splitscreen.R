split.screen(c(1,2))
screen(1)
cf::cf(TestFunctions::banana,bar=T)
plot(1:10)
screen(2)
cf::cf(TestFunctions::branin, bar=F, mainminmax=F)
close.screen(2)
hist(runif(10))
close.screen(1)
hist(rnorm(70))
split.screen()
close.screen(all=T)



split.screen(c(1,3))
screen(2)
cf::cf(TestFunctions::banana,bar=T)
screen()

split.screen()
close.screen(all=T)
