library(leaflet)
library(sp)
library(scales)
library(htmltools)
library(tidyverse)
library(lubridate)
load('data/processed/bottle.RData')
# bottle <- bottle %>% filter(year(date) >= 2010)

## -----------------------------
## SPATIAL PAGE

# map data for a year
get_map_data <- function(yr, qr){
  ## note: for each date & station, usually only one set of coordinates recorded
  ## i.e. no lat/long variation across depths, with rare but extant exceptions
  
  # station locations
  station_locations <- bottle %>%
    filter(year(date) == yr) %>%
    # select location info
    select(lat, 
           lon, 
           line, 
           station,
           date) %>%
    # unique sampling locations
    distinct(date, line, station, lat, lon) %>%
    # find average location and variation for each station
    group_by(line, station) %>%
    summarize(across(.cols = c(lat, lon), 
                     .fns = list(ctr = mean, 
                                 var = var)),
              .groups = 'drop') %>%
    mutate(loc_se = sqrt(lat_var + lon_var))
  
  out <- bottle %>%
    # filter to specified year
    filter(year(date) == yr,
           quarter == qr) %>%
    # select spatiotemporal info
    select(depth, 
           line,
           station,
           date) %>%
    # find maximum depth measured that year and number of visits
    group_by(line, station) %>%
    summarize(maxdepth = max(depth), .groups = 'drop') %>%
    ungroup() %>%
    # merge station info (center lat and long)
    full_join(station_locations, by = c('line', 'station')) %>%
    # define indicator for whether a station was sampled
    mutate(sampled_ix = is.na(maxdepth)) %>%
    # create labels to display on hover
    mutate(label_line1 = paste('Line ID', line, sep = ' '),
           label_line2 = paste('Station ID', station, sep = ' '),
           label_line3 = paste('Max depth', maxdepth, sep = ' ')) %>%
    unite(label,
          contains('label'),
          sep = ' <br/> ') %>%
    select(-contains('label_line'))
  
  return(out)
}

# leaflet-specific
point_color_fn <- colorFactor(c('#B73407', '#393939'), 
                              c(T, F))

lines <- bottle %>% pull(line) %>% unique()

# generate base map layer
make_basemap <- function(){
  leaflet() %>% 
    setView(lng = -121.33940, 
            lat = 33.94975, 
            zoom = 5) %>%
    addProviderTiles(providers$Esri.OceanBasemap)
}



update_basemap <- function(basemap, filtered_data){
  basemap %>%
    clearMarkers() %>%
    addCircleMarkers(lat = ~lat_ctr, 
                     lng = ~lon_ctr, 
                     popup = ~label, 
                     color = ~point_color_fn(sampled_ix),
                     #once bottom_d added change radius = bottom depth/ or hypoxia 
                     radius = ~ -log(loc_se),
                     data = filtered_data)
}

# custom transformation for depth profiles
rev_sqrt <- trans_new('revsqrt', 
                      function(x) -sqrt(x),
                      function(x) x^2,
                      breaks = breaks_log(n = 5, base = 10))

# depth profiles - Og function
# make_profile <- function(yr, qr){
#   bottle %>% 
#     filter(year(date) == yr,
#            quarter == qr) %>%
#     select(oxygen, 
#            salinity, 
#            temperature, 
#            depth, 
#            cast) %>%
#     pivot_longer(1:3, 
#                  names_to = "measurement", 
#                  values_to = "value") %>%
#     ggplot(aes(x = value, y = depth,
#                group = interaction(cast, measurement))) +
#     geom_path(alpha = 0.1) +
#     scale_y_continuous(trans = rev_sqrt) +
#     facet_wrap(~ measurement,
#                nrow = 1,
#                scales = 'free_x') +
#     geom_hline(yintercept = 0) +
#     labs(x = '') +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 90, 
#                                      size = 8, 
#                                      vjust = 0.5),
#           axis.text.y = element_text(size = 8))
# }

make_profile <- function(yr, lin){
  stations_in_line <- bottle %>%
    filter(year(date) == yr,
           depth <= 1000,
           line == lin) %>%
    mutate(quarter = replace(quarter, quarter == 1, "Q1 - Winter"), 
           quarter = replace(quarter, quarter == 2, "Q2 - Spring"), 
           quarter = replace(quarter, quarter == 3, "Q3 - Summer"), 
           quarter = replace(quarter, quarter == 4, "Q4 - Fall")) %>%
    subset(line == lin)
  bottle %>% 
    filter(year(date) == yr,
           depth <= 1000,
           line != lin) %>%
    mutate(quarter = replace(quarter, quarter == 1, "Q1 - Winter"), 
           quarter = replace(quarter, quarter == 2, "Q2 - Spring"), 
           quarter = replace(quarter, quarter == 3, "Q3 - Summer"), 
           quarter = replace(quarter, quarter == 4, "Q4 - Fall")) %>%
    ggplot(aes(x = oxygen, y = depth,
               group = interaction(cast, quarter))) +
    geom_path(color = "black", alpha = 0.1) +
    geom_path(data = stations_in_line,
              color = "aquamarine3",
              size = 2,
              alpha = 0.1) +
    scale_y_continuous(trans = rev_sqrt) +
    facet_wrap(~ quarter,
               nrow = 1) +
    geom_hline(yintercept = 0) +
    labs(x = 'Oxygen (ml of O_2/L of seawater)') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, 
                                     size = 8, 
                                     vjust = 0.5),
          axis.text.y = element_text(size = 8))
}



make_station_line <- function(yr, lin){
  bottle %>%
    # filter to year, quarter, and station of interest
    filter(year == yr, 
           # quarter == qr,
           line == lin) %>%
    # bin depths into roughly even numbers of observations
    mutate(depth_interval = cut_number(depth, 10)) %>%
    # aggregate within depth bins
    mutate(quarter = replace(quarter, quarter == 1, "Q1 - Winter"), 
           quarter = replace(quarter, quarter == 2, "Q2 - Spring"), 
           quarter = replace(quarter, quarter == 3, "Q3 - Summer"), 
           quarter = replace(quarter, quarter == 4, "Q4 - Fall")) %>%
    group_by(depth_interval,
             quarter,
             distance) %>%
    summarize(oxygen = median(oxygen, na.rm = T)) %>% # tinker with summary stat
    ggplot(aes(x = distance, y = fct_rev(depth_interval))) +
    facet_wrap(~ quarter, 
               # scales = "free_x",
               nrow = 4) +
    # using geom_tile instead of raster in order to create boxes of different widths
    geom_tile(aes(fill = oxygen, width = distance)) +
    # adjust color scale
    scale_fill_gradient2(low = '#E74C3C',
                         mid = '#F1C40F',
                         high = '#3498DB',
                         midpoint = log10(1.4),
                         trans = 'log10') +
    # aesthetics
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, 
                                     size = 8,
                                     vjust = 0.5),
          panel.grid = element_blank()) +
    labs(x = 'Distance from shore (Nautical Miles)', y = 'Depth (m)',
         fill='Oxygen (mL O2/L seawater)') 
}


  
  
bottle %>% 
  filter(depth == 50) %>% 
  mean(oxygen, na.rm = TRUE)

mean(filter(bottle, depth == 50)$oxygen, na.rm = TRUE)

  
(subset(bottle, oxygen < 5.26)  / bottle)*100
  
  
  