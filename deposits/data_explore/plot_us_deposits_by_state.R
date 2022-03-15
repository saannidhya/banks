# Purpose: to plot U.S deposits by state
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
load(glue::glue("{here::here('data','data_clean')}/sod_merge_by_state.RData"))

# merging with statepop df to get state list with merge
st_list <- as_tibble(unique(sod_merge_by_state$stnamebr)) %>%
                rename(state = value) %>%
                mutate(state = tolower(state)) %>%
                left_join(statepop %>% rename(state = full) %>% select(-c(pop_2015)) %>% mutate(state = tolower(state)), 
                          by = "state")

# merge sod_merge_by_state with statepop
df_sod_state <- sod_merge_by_state %>%
                rename(state = stnamebr) %>%
                inner_join(statepop %>% rename(state = full) %>% select(-c(pop_2015)) %>% mutate(state = tolower(state)), 
                           by = "state") %>%
                mutate(deposits = deposits/1000000) #converting into billions (data already in 1000s)

# plotting U.S deposits (year 2021)
plot_usmap(data = df_sod_state %>% filter(year == 2021), 
           values = "deposits",
           labels = TRUE) +
    scale_fill_gradient(low = "white",
                        high = "blue",
                        name = "deposits (in $Bns)") +
    ggtitle("Total U.S Deposits (State-level, Year: 2021)") +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          legend.position = c(0.4, 0.01),
          legend.direction = 'horizontal',
          legend.key.width = unit(0.7, "in"),
          legend.key.height = unit(0.3, "in"))

plot_usmap(data = df_sod_state %>% filter(year == 2021), 
           values = "deposits",
           labels = TRUE) +
    scale_fill_gradient(low = "#f1eef6",
                        high = "#0570b0",
                        na.value = "black",
                        name = "branches") +
    ggtitle("U.S Bank Branches (State-level, Year: 2021)") +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          legend.position = c(0.4, 0.01),
          legend.direction = 'horizontal',
          legend.key.width = unit(0.7, "in"),
          legend.key.height = unit(0.3, "in"))

# plotting U.S deposits by state & year (animation)
p1 <- plot_usmap(data = df_sod_state, 
           values = "deposits",
           labels = TRUE) +
        scale_fill_gradient(low = "#f1eef6",
                        high = "#0570b0",
                        na.value = "black",
                        name = "deposits (in $Bns)") +
        ggtitle("Total U.S Deposits: State-level") +
        theme(plot.title = element_text(size = 16, hjust = 0.5),
              legend.position = c(0.26, 0.01),
              legend.direction = 'horizontal',
              legend.key.width = unit(0.7, "in"),
              legend.key.height = unit(0.3, "in")) +
        scale_color_brewer(palette = "Set2")
p1.animation <- p1 +
    transition_time(year) +
    labs(subtitle = "Year: {as.integer(frame_time)}")
# save animation 
anim_save(glue::glue("{here::here('data','data_explore')}/plot_us_deposits_by_state.gif"))


# plotting U.S deposits by state & year since 2008 (animation)
p2 <- plot_usmap(data = df_sod_state %>% filter(year > 2007) , 
                values = "num_branches",
                labels = TRUE) +
    scale_fill_gradient(low = "#f1eef6",
                        high = "#0570b0",
                        na.value = "black",
                        name = "branches") +
    ggtitle("Numer of Bank branches in U.S: State-level") +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          legend.position = c(0.26, 0.01),
          legend.direction = 'horizontal',
          legend.key.width = unit(0.7, "in"),
          legend.key.height = unit(0.3, "in")) +
    scale_color_brewer(palette = "Set2")
p2.animation <- p2 +
    transition_time(year) +
    labs(subtitle = "Year: {as.integer(frame_time)}")
# save animation 
anim_save(glue::glue("{here::here('data','data_explore')}/plot_us_dep_branches_by_state.gif"))

## Linking to GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
# nc <- st_read(system.file("shape/nc.shp", package="sf"))
# geocode_OSM()
