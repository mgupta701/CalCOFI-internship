library(tidyverse)

###### Time series plot for oxygen

# Modifying original ts plot to show percentile
oxy_ts_plot_percentile <- function(n_ranges, percentile, date_min, date_max){
  if (n_ranges > 1){
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac = cut(depth, n_ranges))
  }
  else {
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac = rep("[0,500]", length(year)))
  }

  z <- bottle_sub %>%
    group_by(year, quarter, depth_fac) %>%
    summarise(oxygen_perc = quantile(oxygen, probs = percentile/100, na.rm = TRUE),
              date = median(date, na.rm = T))
  
  z %>% 
    ggplot(aes(x = date, 
               y = oxygen_perc, 
               group = depth_fac, 
               color = depth_fac, 
               shape = as.factor(quarter))) +
    geom_point(na.rm=T) +
    geom_line(linetype='dashed') +
    labs(title = paste(percentile,
                       "th Percentile Oxygen Across All On-Shelf Stations Over Time"), 
         x = "Date", 
         y = "Oxygen (mL/L)", 
         color = "Depth Range (m)", 
         shape = "Quarter") +
    scale_shape_discrete(name="Quarter",
                         breaks=c("1", "2", "3","4"),
                         labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit = c(as.Date(date_min), as.Date(date_max)), 
                 date_labels = "%Y %b %d", 
                 breaks = scales::breaks_pretty(7)) +
    scale_y_continuous(limits=c(NA,NA), 
                       expand = c(0.1, 0.1)) +
    theme_bw() 
  
}

oxy_ts_plot_percentile(5, 95, '1949-02-28', '2020-01-26')

# Time series plot of the standard deviation in oxygen values
oxy_ts_plot_sd <- function(n_ranges, date_min, date_max){
  if (n_ranges > 1){
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac = cut(depth, n_ranges))
  }
  else {
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac = rep("[0,500]", length(year)))
  }
  
  z <- bottle_sub %>%
    group_by(year, quarter, depth_fac) %>%
    summarise(oxygen_sd = sd(oxygen, na.rm = TRUE),
              date = median(date, na.rm = T))
  
  z %>% 
    ggplot(aes(x = date, 
               y = oxygen_sd, 
               group = depth_fac, 
               color = depth_fac, 
               shape = as.factor(quarter))) +
    geom_point(na.rm=T) +
    geom_line(linetype='dashed') +
    labs(title = "Standard Deviation in Oxygen Across All On-Shelf Stations Over Time", 
         x = "Date", 
         y = "Oxygen (mL/L)", 
         color = "Depth Range (m)", 
         shape = "Quarter") +
    scale_shape_discrete(name="Quarter",
                         breaks=c("1", "2", "3","4"),
                         labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit = c(as.Date(date_min), as.Date(date_max)), 
                 date_labels = "%Y %b %d", 
                 breaks = scales::breaks_pretty(7)) +
    scale_y_continuous(limits=c(NA,NA), 
                       expand = c(0.1, 0.1)) +
    theme_bw() 
  
}

oxy_ts_plot_sd(5, '1970-06-14', '2020-01-26')


##### Making one oxygen plot per quarter

oxy_ts_plot_percentile_quarters <- function(n_ranges, percentile, date_min, date_max){
  if (n_ranges > 1){
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac = cut(depth, n_ranges))
  }
  else {
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac = rep("[0,500]", length(year)))
  }
  
  z <- bottle_sub %>%
    group_by(year, quarter, depth_fac) %>%
    summarise(oxygen_perc = quantile(oxygen, probs = percentile/100, na.rm = TRUE),
              date = median(date, na.rm = T))
  
  z %>% 
    ggplot(aes(x = date, 
               y = oxygen_perc, 
               group = depth_fac, 
               color = depth_fac, 
               # shape = as.factor(quarter)
               )) +
    geom_point(na.rm = T) +
    facet_wrap(~ quarter,
               nrow = 2) +
    geom_line(linetype='dashed') +
    labs(title = paste(percentile,
                       "th Percentile Oxygen Across All On-Shelf Stations Over Time"), 
         subtitle = "Q1 is Winter, Q2 is Spring, Q3 is Summer, Q4 is Fall",
         x = "Date", 
         y = "Oxygen (mL/L)", 
         color = "Depth Range (m)", 
         # shape = "Quarter"
         ) +
    # scale_shape_discrete(name="Quarter",
                         # breaks=c("1", "2", "3","4"),
                         # labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit = c(as.Date(date_min), as.Date(date_max)), 
                 date_labels = "%Y %b %d", 
                 breaks = scales::breaks_pretty(7)) +
    scale_y_continuous(limits=c(NA,NA), 
                       expand = c(0.1, 0.1)) +
    theme_bw() 
  
}

oxy_ts_plot_percentile_quarters(5, 5, '1949-02-28', '2020-01-26')


###### Time Series plot for temperature

temp_ts_plot_percentile_quarters <- function(n_ranges, percentile, date_min, date_max){
  if (n_ranges > 1){
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac = cut(depth, n_ranges))
  }
  else {
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac= rep("[0,500]",length(year)))
  }
  
  z <- bottle_sub %>%
    group_by(year, quarter, depth_fac) %>%
    summarise(temperature_perc = quantile(temperature, probs = percentile/100, na.rm = TRUE),
              date = median(date, na.rm = T))
  
  z %>% 
    ggplot(aes(x = date, 
               y = temperature_perc, 
               group = depth_fac, 
               color = depth_fac, 
               # shape=as.factor(quarter)
               )) +
    geom_point(na.rm=T) +
    facet_wrap(~ quarter,
               nrow = 2) +
    geom_line(linetype='dashed') +
    labs(title = paste(percentile,
                       "th Percentile Temperature Across All On-Shelf Stations Over Time"), 
         x = "Date", 
         y = "Temperature (°C)", 
         color = "Depth Range (m)", 
         # shape = "Quarter"
         ) +
    # scale_shape_discrete(name="Quarter",
    #                      breaks=c("1", "2", "3","4"),
    #                      labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit=c(as.Date(date_min),as.Date(date_max)), date_labels = "%Y %b %d", breaks = scales::breaks_pretty(7)) +
    scale_y_continuous(limits=c(NA,NA),
                       expand = c(0.1, 0.1)) +
    theme_bw() 
}

temp_ts_plot_percentile_quarters(5, 95, '1949-02-28', '2020-01-26')
