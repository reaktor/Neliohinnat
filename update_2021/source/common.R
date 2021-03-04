# Center & scale for modeling 
YEAR_SCALE = 10
year2yr <- function (year) (year-2010)/YEAR_SCALE

# Prefixes for postal codes
prefix.factor <- function (pnro, n) as.factor(substr(pnro, 1, n))
l3 <- { . %>% prefix.factor(., 1) }
l2 <- { . %>% prefix.factor(., 2) }
l1 <- { . %>% prefix.factor(., 3) }

first.nna2 <- function (c1, c2)  ifelse(!is.na(c1), c1, c2)
first.nna <- function (...) Reduce(first.nna2, list(...))

sum.0na2 <- function (c1, c2)  ifelse(is.na(c1), 0, c1) + ifelse(is.na(c2), 0, c2)
sum.0na <- function (...) Reduce(sum.0na2, list(...))


std <- function (x) (x - mean(x, na.rm=T)) / sd(x, na.rm=T)
nlogit <- function (n, N, prior=20) { 
  n = replace_na(n, 0)
  N = replace_na(N, 0)
  p <- (n + prior*mean(n, na.rm=T)/mean(N, na.rm=T))/(N + prior)
  std(log(p / (1-p)))
}
regshare <- function(x, y, prior=20) {
  x = replace_na(x, 0)
  y = replace_na(y, 0)
  p <- (x + prior*mean(x, na.rm=T)/mean(y, na.rm=T))/(y + prior)
  std(p)
}
stdna <- function (x) {
  x = replace_na(x, 0)
  (x - mean(x, na.rm=T)) / sd(x, na.rm=T)
}