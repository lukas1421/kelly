dfCum = data.frame(seq(0, 1, 0.01),rep(0,101))
colnames(dfCum) <- c('f','cumprod')

b = 1
p = 0.7
ruinProb = 1 - p
x <- rbinom(n = 500, size = 1, prob = ruinProb)
#print(c('mean is', mean(x)))
#test
runningmax = 0
optimalF = 0
print(c(
  'kelly',
  'ruinprob',
  ruinProb,
  'b',
  b,
  'fraction',
  (1 - ruinProb) - ruinProb / b
))
for (f in seq(0, 1, 0.01)) {
  y <- x
  y[which(y == 0)] <- 1 + f * b
  y[which(y == 1)] <- 1 - f
  result = tail(cumprod(y), 1)
  dfCum[which(dfCum$f==f), ]['cumprod'] <- result
  
  #print(c(i,result))
  if (result > runningmax) {
    runningmax = result
    optimalF = f
  }
}
print(dfCum)

ggplot(data=dfCum, aes(x=f,y=cumprod)) + geom_line()

