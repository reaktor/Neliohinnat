# Script for visualising apartment price data and model

## ANIMATE DATA #########

library("dplyr")
library("tidyr")
source("source_2016/common.R")
library("rgdal")
library("maptools")
library("ggplot2")
library("viridis")

# devtools::install_github("dgrtwo/gganimate")
library("gganimate")

# Create almost empty theme
new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$axis.text <- element_blank()
# new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")
theme_set(new_theme_empty)


# Load raw data
raw_df <- readRDS("data_2016/d.rds")

# # Add trend
# raw_trend_df <- raw_df %>%
#   group_by(pnro) %>% 
#   do(data_frame(lprice=mean(.$lprice),
#                 trend=coef(lm(lprice ~ year + 1, 
#                               weights = .$n,
#                               data=.))[["year"]])) %>%
#   data.frame()

# Load model results
res.long <- readRDS("data_2016/pnro-results_long_2016.rds")

# Predict prices for years 2005-2015
pred_df <- lapply(2005:2015, function(year)
  res.long %>%
    mutate(price_pred = exp(6 + lprice + trend*year2yr(year) + quad*year2yr(year)**2),
           trend_pred = (trend + 2*quad*year2yr(year))/10) %>%
    group_by(pnro, log.density) %>% 
    summarise(price_pred_mean = mean(price_pred),
              trend_pred_mean = mean(trend_pred)) %>%
    mutate(year = year)
) %>% bind_rows()

# Load spatial polygons for pnro areas
load("data_2016/pnro_data_20160215.RData")
# Transform coordinates
pnro.sp.alt <- spTransform(pnro.sp, CRS("+init=epsg:2393"))
# Transform to dataframe wiht ggplot2::fortify
pnro_alt_df <- ggplot2::fortify(pnro.sp.alt, region="pnro")

# Combine everything together
combined_df <- raw_df %>%
  select(pnro, year, price) %>%
  mutate(source = "Raw") %>%
  bind_rows(pred_df %>%
              select(pnro, year, price = price_pred_mean) %>%
              mutate(source = "Model")) %>%
  mutate(source = factor(source, levels=c("Raw", "Model"))) %>%
  left_join(pnro_alt_df %>% 
              rename(pnro = id))

saveRDS(combined_df, file="data_2016/model_vs_raw_plotdata_temp.rds")
# # Merge raw data with spatial data
# raw_spatial_df <- raw_df %>%
#   select(pnro, year, price) %>%
#   left_join(pnro_alt_df %>% 
#               rename(pnro = id))
# # Merge predicted data
# pred_spatial_df <- pred_df %>%
#   select(pnro, year, price = price_pred_mean) %>%
#   left_join(pnro_alt_df %>% 
#               rename(pnro = id))
# combined_df <- raw_spatial_df %>%
#   mutate(source = "Raw data") %>%
#   bind_rows(pred_spatial_df %>%
#               mutate(source = "Modelled data")) %>%
#   mutate(source = factor(source, levels=c("Raw data", "Modelled data")))

# Test plot

combined_df %>%
  filter(year == 2015) %>%
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=price), colour=NA) +
  scale_fill_viridis() +
  theme(legend.position="bottom") +
  facet_wrap(~ source, nrow=1)

# # Facet
# combined_df %>%
#   filter(source=="Modelled data") %>%
#   ggplot(aes(x=long, y=lat)) + 
#   geom_polygon(aes(group=group, fill=price), color=NA) +
#   scale_fill_viridis(trans="log") +
#   theme(legend.position="top") +
#   facet_wrap(~ year)

## Animate


p <- combined_df %>%
  ggplot(aes(x=long, y=lat, frame=year)) + 
  geom_polygon(aes(group=group, fill=price), color=NA) +
  scale_fill_viridis(trans = "log") +
  theme(legend.position="none") +
  facet_wrap(~ source, nrow=1) +
  ggtitle("Mean apartment prices:") +
  theme(plot.title = element_text(size=20),
        strip.text.x = element_text(size=14))

gg_animate(p = p, filename="figs_2016/model_vs_raw_log_temp.gif")

# Another one on Souther Finland
p2 <- combined_df %>%
  filter(lat < 7000000) %>%
  ggplot(aes(x=long, y=lat, frame=year)) + 
  geom_polygon(aes(group=group, fill=price), color=NA) +
  scale_fill_viridis(trans = "log") +
  theme(legend.position="none") +
  facet_wrap(~ source, nrow=2) +
  ggtitle("Mean apartment prices:") +
  theme(plot.title = element_text(size=20),
        strip.text.x = element_text(size=14))

gg_animate(p = p2, filename="figs_2016/model_vs_raw_log_southern_temp.gif")

# TODO: Remove borders!
# Maybe focus on Southern finland

# http://stackoverflow.com/questions/25875877/remove-border-lines-in-ggplot-map-choropleth
