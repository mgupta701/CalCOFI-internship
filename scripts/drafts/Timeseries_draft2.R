#load packages
install.packages("plyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggExtra")
install.packages("heatmaply")


library(plyr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggExtra)
library(heatmaply)




# changing to numeric for filtering purposes
bottle$line <- as.numeric(bottle$line)
bottle$station <- as.numeric(bottle$station)

###### Time series plot for oxygen

# Modifying original ts plot to show percentile
oxy_ts_plot_percentile <- function(n_ranges, percentile, date_min, date_max){
  if (n_ranges > 1){
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500,
             line >= 76.7 & line <= 93.3) %>%
      mutate(depth_fac = cut(depth, n_ranges))
  }
  else {
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500,
             line >= 76.7 & line <= 93.3) %>%
      mutate(depth_fac = rep("[0,500]", length(year)))
  }

x <- bottle_sub %>%
    group_by(year, quarter, depth_fac) %>%
    summarise(oxygen_perc = quantile(oxygen, probs = percentile/100, na.rm = TRUE),
              date = median(date, na.rm = T))
  
  x %>% 
    ggplot(aes(x = date, 
               y = oxygen_perc, 
               group = depth_fac, 
               color = depth_fac, 
               shape = as.factor(quarter))) +
    geom_point(na.rm=T) +
    geom_line(linetype='dashed') +
    labs(title = paste(percentile,
                       "th Percentile Oxygen Over Time"), 
         subtitle = "Data is from stations on the shelf within the core sampling region",
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

oxy_ts_plot_percentile(3, 5, '1949-02-28', '2020-01-26')



## Faceting by  quarter

oxy_ts_plot_percentile_quarters <- function(n_ranges, percentile, date_min, date_max){
  if (n_ranges > 1){
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500,
             line >= 76.7 & line <= 93.3) %>%
      mutate(depth_fac = cut(depth, n_ranges))
  }
  else {
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500,
             line >= 76.7 & line <= 93.3) %>%
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

oxy_ts_plot_percentile_quarters(3, 95, '1949-02-28', '2020-01-26')



#heat map visual ###
## Counting unique stations for each quarter
bottle_station_count <- bottle_filter %>%
  unite('station_id', line:station, remove = FALSE)

bottle_station_count <- bottle_station_count %>%
  unite('time', c(year, quarter), remove = FALSE)

bottle_station_count<-bottle_station_count%>% select(c("station_id",time,oxygen))
bottle_station_count<-na.omit(bottle_station_count)


station_number_check<-bottle_station_count %>%select(c("station_id","time"))
station_number_check<-station_number_check %>%unique()%>% count(time)
station_number_check

## Counting unique stations for each year
bottle_station_count <- bottle_filter %>%
  unite('station_id', line:station, remove = FALSE)

year_check_pre<-bottle_station_count%>% select(c("station_id","year","oxygen"))
year_check_pre<-na.omit(bottle_station_count)
year_check<-bottle_station_count %>%select(c("station_id","year"))
year_check<-year_check %>%unique()%>% count(year)
year_check


# filter bottle data set 

bottle$line<-as.numeric(bottle$line)
bottle_filter <- bottle %>%
  subset(station <= 60) %>%
  filter(depth >= 0 & depth <= 300,
         line >= 76.7 & line <= 93.3)

# return years that have less than n stations
n <- 5
yrs_under_threshold <- data.frame(year = double())

for (row in 1:nrow(year_check)) {
  stations <- year_check[row, "n"]
  year <- year_check[row, "year"]
  
  if (stations < n){
    yrs_under_threshold <- rbind(yrs_under_threshold, year)
  }
}
yrs_under_threshold

# return quarters that have less than n stations

qts_under_threshold <- data.frame(time = character())

for (row in 1:nrow(station_number_check)) {
  stations <- station_number_check[row, "n"]
  quarter <- station_number_check[row, "time"]
  
  if (stations < n){
    qts_under_threshold <- rbind(qts_under_threshold, quarter)
  }
}
qts_under_threshold
#### heat map visual 


# function to get list of oxygen for heat map
get_oxy_percent_quarters <- function(percentile, date_min, date_max){
 
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(line >= 76.7 & line <= 93.3, depth <= 150) 
  
  h <- bottle_sub %>%
    group_by(year, quarter) %>%
    summarise(oxygen_perc = quantile(oxygen, probs = percentile/100, na.rm = TRUE),
              date = median(date, na.rm = T))
  
  return(h)
}

hm_median_quarters <- get_oxy_percent_quarters(50, '1949-02-28', '2020-01-26')
hm_median_quarters <- hm_median_quarters %>% group_by(quarter,year)
hm_median_quarters$quarter <- as.character(hm_median_quarters$quarter)
hm_median_quarters <- hm_median_quarters %>%
  mutate(quarter = recode(quarter,'1' = 'Winter','2' = 'Spring','3' =  'Summer','4'='Fall' ))
yform <- list(categoryorder = "array",
              categoryarray = c( "Fall", 
                                "Summer",
                                "Spring",
                                "Winter"))

hm_median_quarters


## Interactive heatmap with year and quarter

median_heatmap_quarterly <- plot_ly(x = hm_median_quarters$year,
                                    y = hm_median_quarters$quarter,
                                    z = hm_median_quarters$oxygen_perc,
                                    type = "heatmap",
                                    colors = "magma",  # previously used "Blues"
                                    reversescale = T, 
                                    hovertemplate = "Year:%{x} <br> Quarter: %{y} <br> Median Oxygen Level: %{z}<extra></extra>") %>%
  layout(title="Median Oxygen Levels in Core CalCOFI stations up to 300 meters, over quarters",
         yaxis=yform,
         plot_bgcolor = 'gray')

median_heatmap_quarterly



## heatmap without quarters
get_oxygen_perc_years <- function(percentile, date_min, date_max){

  h <- bottle_filter %>%
    group_by(year) %>%
    summarise(oxygen_perc = quantile(oxygen, probs = percentile/100, na.rm = TRUE),
              date = median(date, na.rm = T))
  
  return(h)
}

hm_median_years <- get_oxygen_perc_years(50, '1949-02-28', '2020-01-26')
hm_median_years <- hm_median_years %>% group_by(year)

hm_median_years$y <- rep(0,71)
ax <- list(showticklabels = FALSE)

median_heatmap_yearly <- plot_ly(x = hm_median_years$year, 
                                 y = hm_median_years$y, 
                                 z = hm_median_years$oxygen_perc, 
                                 type="heatmap", 
                                 colors ="magma",
                                 reversescale=T, 
                                 hovertemplate= "Year:%{x} <br> Median Oxygen Level: %{z}<extra></extra>") %>%
  layout(title = "Median Oxygen Levels in Core CalCOFI stations up to 300 meters", 
         yaxis = ax, 
         plot_bgcolor = 'grey')
median_heatmap_yearly

## heatmap with mean levels of oxygen yearly

hm_mean_years <- bottle_filter %>%
  group_by(year) %>%
  summarise(mean_oxy = mean(oxygen,na.rm = TRUE))


hm_mean_years$y <- rep(0,71)
ax <- list(showticklabels = FALSE)

mean_heatmap_yearly <- plot_ly(x = hm_mean_years$year, 
                               y = hm_mean_years$y, 
                               z = hm_mean_years$mean_oxy, 
                               type = "heatmap", 
                               colors = "magma",
                               reversescale = T, 
                               hovertemplate = "Year:%{x} <br> Mean Oxygen Level: %{z}<extra></extra>") %>%
  layout(title = "Mean Oxygen Levels in Core CalCOFI stations up to 300 meters", 
         yaxis = ax, 
         plot_bgcolor = 'grey')
mean_heatmap_yearly


#heatmap with mean levels of oxygen showed quarterly 

mean_data_quarterly <- bottle_filter %>%
  group_by(year,quarter) %>%
  summarise(mean_oxy = mean(oxygen,na.rm = TRUE))
mean_data_quarterly


mean_heatmap_quarterly <- plot_ly(x = mean_data_quarterly$year, 
                                  y = mean_data_quarterly$quarter,
                                  z = mean_data_quarterly$mean_temp, 
                                  type = "heatmap",
                                  colors = "magma",
                                  reversescale = T, 
                                  hovertemplate = "Year:%{x} <br> Quarter: %{y} <br> Mean Oxygen Level: %{z}<extra></extra>") %>%
  layout(title = "Mean Oxygen Levels in Core CalCOFI stations up to 300 meters, over quarters",
         yaxis = yform, 
         plot_bgcolor = 'grey')
mean_heatmap_quarterly


add_missing_data<-data.frame("year"=c(1956,1956,1965,1967,1967,1968,1968,1970,1970,1970,1971,1971,1971,1973,1973,
                                      1973,1974,1974,1974,1976,1977,1977,1977,1978,1980,1980,1980,1981,1982,1982,
                                      1982,1991,2009,2018,2020,2020,2020),
                             "quarter"=c(3,4,4,1,4,3,4,1,2,4,2,3,4,1,2,3,1,2,3,3,1,2,3,4,1,2,3,4,2,3,4,2,2,3,2,3,4),
                             "mean_oxy"=c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN
                                           ,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,
                                           NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN))
add_missing_data

total_mean_quarterly <- rbind(add_missing_data, mean_data_quarterly) # append empty values with mean data

# filter out quarters with under 5 stations sampled
total_mean_quarterly <- total_mean_quarterly %>%
  unite('time', c(year, quarter), remove = FALSE)  # add time column
# change mean_oxy to NaN for selected times
for (row in 1:nrow(total_mean_quarterly)){
  if (total_mean_quarterly[row, "time"] %in% qts_under_threshold$time){
    total_mean_quarterly[row, "mean_oxy"] <- NaN
  }
}
total_mean_quarterly <- total_mean_quarterly[c("year", "quarter", "mean_oxy")]

total_mean_quarterly <- arrange(total_mean_quarterly, year) %>%
  mutate(quarter = recode(quarter,'1' = 'Winter','2' = 'Spring','3' =  'Summer','4'='Fall' ))


mean_heatmap_quarterly2 <- plot_ly(x = total_mean_quarterly$year, 
                                   y = total_mean_quarterly$quarter,
                                   z = total_mean_quarterly$mean_oxy, 
                                   type = "heatmap",
                                   colors = "magma",
                                   reversescale = T, 
                                   hovertemplate= "Year:%{x} <br> Quarter: %{y} <br> Mean Oxygen Level: %{z}<extra></extra>") %>%
  layout(title = "Mean Oxygen Levels in Core CalCOFI stations up to 300 meters, over quarters",
         yaxis = yform, 
         plot_bgcolor = 'grey')
mean_heatmap_quarterly2


## same as above but median

median_data_quarterly <- get_oxy_percent_quarters(50, '1949-02-28', '2020-01-26')
median_data_quarterly <- median_data_quarterly[c("year", "quarter", "oxygen_perc")]


add_missing_data_med <- data.frame("year"=c(1956,1956,1965,1967,1967,1968,1968,1970,1970,1970,1971,1971,1971,1973,1973,
                                      1973,1974,1974,1974,1976,1977,1977,1977,1978,1980,1980,1980,1981,1982,1982,
                                      1982,1991,2009,2018,2020,2020,2020),
                             "quarter"=c(3,4,4,1,4,3,4,1,2,4,2,3,4,1,2,3,1,2,3,3,1,2,3,4,1,2,3,4,2,3,4,2,2,3,2,3,4),
                             "oxygen_perc"=c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN
                                          ,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,
                                          NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN))

total_median_quarterly <- rbind(add_missing_data_med, median_data_quarterly) # append empty values with mean data

# filter out quarters with under 5 stations sampled
total_median_quarterly <- total_median_quarterly %>%
  unite('time', c(year, quarter), remove = FALSE)  # add time column
# change mean_oxy to NaN for selected times
for (row in 1:nrow(total_median_quarterly)){
  if (total_median_quarterly[row, "time"] %in% qts_under_threshold$time){
    total_median_quarterly[row, "mean_oxy"] <- NaN
  }
}
total_median_quarterly <- total_median_quarterly[c("year", "quarter", "oxygen_perc")]

total_median_quarterly <- arrange(total_mean_quarterly, year) %>%
  mutate(quarter = recode(quarter,'1' = 'Winter','2' = 'Spring','3' =  'Summer','4'='Fall' ))


median_heatmap_quarterly2 <- plot_ly(x = total_median_quarterly$year, 
                                   y = total_median_quarterly$quarter,
                                   z = total_median_quarterly$mean_oxy, 
                                   type = "heatmap",
                                   colors = "magma",
                                   reversescale = T, 
                                   hovertemplate= "Year:%{x} <br> Quarter: %{y} <br> Mean Oxygen Level: %{z}<extra></extra>") %>%
  layout(title = "Median Oxygen Levels in Core CalCOFI stations up to 300 meters, over quarters",
         yaxis = yform, 
         plot_bgcolor = 'grey')
median_heatmap_quarterly2

# test to make sure values displayed are correct
test_ox_data<- bottle_filter %>% filter(year== 2000) %>% select(c("oxygen"))%>%na.omit()
ox_sum<-sum(test_ox_data$oxygen,na.rm = TRUE)
ox_sum/nrow(test_ox_data)


f<-bottle_filter%>%group_by(year)%>%count()
f

# write.csv(f,"/Users/annalieseadams/Desktop/n.csv",row.names=FALSE)


# write.csv(year_check,"/Users/annalieseadams/Desktop/year_check.csv",row.names=FALSE)
# write.csv(station_number_check,"/Users/annalieseadams/Desktop/station_check.csv",row.names=FALSE)
