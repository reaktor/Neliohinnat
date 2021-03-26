# Script for validating 2016 data

library("dplyr")

# this one
load("data_2015/pnro_data_20150318.RData")
pnro.ashi.dat.2015 <- pnro.ashi.dat
load("data_2016/pnro_data_20160215.RData")

temp_2015 <- pnro.ashi.dat.2015 %>%
  select(pnro:n)
temp_2016 <- pnro.ashi.dat %>%
  select(pnro:n)%>%
  filter(year != 2015)
temp_2016 %>% anti_join(temp_2015, by=c("pnro", "year")) %>% View

temp_join <- temp_2015 %>%
  inner_join(temp_2016, by=c("pnro", "year"))

plot(temp_join$price.x, temp_join$price.y)
plot(temp_join$n.x, temp_join$n.y)


# then d
d_2015 <- readRDS("data_2015/d.rds")
d_2016 <- readRDS("data_2016/d.rds")

n_distinct(d_2015$pnro)

n_distinct(d_2016$pnro)
