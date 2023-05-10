## trying out rolling average/proportion for plotting % nights with detections ... didn't end up using this

library(runner)

x <- cumsum(rnorm(40))
y <- 3 * x + rnorm(40)
date <- Sys.Date() + cumsum(sample(1:3, 40, replace = TRUE)) # unequaly spaced time series
group <-  rep(c("a", "b"), 20)

df <- data.frame(date, group, y, x)
df
head(df)


slope <- runner(
  df,
  function(x) {
    coefficients(lm(y ~ x, data = x))[2]
  }
)

plot(slope)

colnames(df)[3:4] <- c('b','a')
test <- runner(df[,-1], 
               function(x) {
                 as.numeric(x$a)-as.numeric(x$b)
                 }
               )
test
plot(test)

##

# summarizing - sum of 4-elements
runner(
  1:15,
  k = 4,
  f = sum
)

# summarizing - slope from lm
df <- data.frame(
  a = 1:15,
  b = 3 * 1:15 + rnorm(15)
)
df

runner(
  x = df,
  k = 5,
  f = function(x) {
    model <- lm(b ~ a, data = x)
    coefficients(model)["a"]
  }
)

weekProp <- runner(
  x = df,
  k = 7,
  f = function(x) {
    x$a[1] / sum(x$b)
  }
)

df
weekProp[[1]]


