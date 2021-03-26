# Center & scale for modeling 
# The thing below is for data starting from year 2010.
# The thing in comments is for the long-term data.
# If you modify this, you need to rerun your model, and ALSO MODIFY
# post-processing code, especially replace the denominator '7' 
# with something else, maybe '10', in several places. (Be careful!)
year2yr <- function (year) (year-2012.964)/7
# year2yr <- function (year) (year-2010)/10

# Prefixes for postal codes
prefix.factor <- function (pnro, n) as.factor(substr(pnro, 1, n))
l3 <- { . %>% prefix.factor(., 1) }
l2 <- { . %>% prefix.factor(., 2) }
l1 <- { . %>% prefix.factor(., 3) }

first.nna2 <- function (c1, c2)  ifelse(!is.na(c1), c1, c2)
first.nna <- function (...) Reduce(first.nna2, list(...))

sum.0na2 <- function (c1, c2)  ifelse(is.na(c1), 0, c1) + ifelse(is.na(c2), 0, c2)
sum.0na <- function (...) Reduce(sum.0na2, list(...))
