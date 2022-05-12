getOptimal <- function() {
  
  b = 1
  p = 0.6
  ruinProb = 1 - p
  x <- rbinom(n = 200, size = 1, prob = ruinProb)
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
    #print(c(i,result))
    if (result > runningmax) {
      runningmax = result
      optimalF = f
    }
  }
  #print(c(optimalF,runningmax))
  return(optimalF)
}
#test etst

sum = 0
trials = 1000
df = data.frame(rep(0,trials))
colnames(df) <- 'res'
for (i in seq(1, trials)) {
  result = getOptimal()
  print(result)
  sum = sum + result
  df[i,] <- result
}
print(c('average', sum / trials))
print(df)
print(mean(df$res))

p <- ggplot(df, aes(x=res)) + geom_histogram(binwidth=0.01)
p
