# Purpose: to plot U.S deposits by county
# Author: Saani Rawat, University of Cincinnati
# Last changed: 18th Sep, 2021

library(tidyverse)
library(purrr)
library(here)
library(janitor)
library(readr)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(tpmaptools)
library(sf)
library(usmap)
library(gganimate)
library(ggthemes)
library(gifski)
library(png)
library(transformr)


# loading aggregated us deposits data
load(glue::glue("{here::here('data','data_clean')}/sod_merge_by_county.RData"))

# merge sod_merge_by_county with countypop
df_sod_county <- sod_merge_by_county %>%
    mutate(deposits = deposits/1000000) #in $Bns



plot_usmap(data = df_sod_county %>% filter(year == 2021),
           regions = "counties",
           values = "deposits", 
           labels = FALSE) +
    scale_fill_gradient(low = "white",
                        high = "blue",
                        name = "deposits (in $Bns)") +
    ggtitle("Total NC Deposits: County-level") +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          legend.position = c(0.26, 0.01),
          legend.direction = 'horizontal',
          legend.key.width = unit(0.7, "in"),
          legend.key.height = unit(0.3, "in")) +
    scale_color_brewer(palette = "Set2")

# plotting U.S deposits by state & year (animation)
p <- plot_usmap(data = df_sod_county %>% filter(!is.na(year)), 
                regions = "counties",
                values = "deposits",
                labels = FALSE) +
    scale_fill_gradient(low = "white",
                        high = "blue",
                        name = "deposits (in $Bns)") +
    ggtitle("Total U.S Deposits: State-level") +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          legend.position = c(0.26, 0.01),
          legend.direction = 'horizontal',
          legend.key.width = unit(0.7, "in"),
          legend.key.height = unit(0.3, "in")) +
    scale_color_brewer(palette = "Set2")

p.animation <- p +
    transition_time(year) +
    labs(subtitle = "Year: {frame_time}")


