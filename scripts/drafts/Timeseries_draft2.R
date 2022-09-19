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



## filter bottle data set 

bottle$line<-as.numeric(bottle$line)
bottle_filter<- bottle %>%
  subset(station <= 60) %>%
  filter(depth>=0 & depth<=300,
         line >= 76.7 & line <= 93.3)


bottle_heat<-bottle_filter %>% select(c("quarter","year","oxygen"))
quarter<-bottle_heat$quarter
oxygen<-bottle_heat$oxygen
year<-bottle_heat$year

#Assign color variables
col1 = "#d8e1cf" 
col2 = "#438484"

#original heatmap 

heat_map<-ggplot(bottle_heat, aes(year,quarter)) + geom_tile(aes(fill = oxygen),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Oxygen Levels")) +
  theme_bw() + theme_minimal() + 
  labs(title="Oxygen Levels From 1949-2020 over 4 quarters",x = "Year", y = "Quarter")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
heat_map



#histogram of oxygen levels to help decide how to break up colors on heatmap
hist(bottle_heat$oxygen)


# function to get list of oxygen for heat map
# Modifying original ts plot to show percentile
oxy_hm <- function( percentile, date_min, date_max){
 
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(line >= 76.7 & line <= 93.3, depth<=300) 
  
  h <- bottle_sub %>%
    group_by(year, quarter) %>%
    summarise(oxygen_perc = quantile(oxygen, probs = percentile/100, na.rm = TRUE),
              date = median(date, na.rm = T))
  
  return(h)
}

hm_data<- oxy_hm(50, '1949-02-28', '2020-01-26')
hm_data<- hm_data %>% group_by(quarter,year)
hm_data$quarter<- as.character(hm_data$quarter)
hm_data <- hm_data %>%
  mutate(quarter = recode(quarter,'1' = 'Winter','2' = 'Spring','3' =  'Summer','4'='Fall' ))
yform <- list(categoryorder = "array",
              categoryarray = c( "Fall", 
                                "Summer",
                                "Spring",
                                "Winter"))
# stationary heat map with original color scale and without breaks 
heat_map<-ggplot(hm_data, aes(year,quarter)) + geom_tile(aes(fill = oxygen_perc),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Oxygen Levels")) +
  theme_bw() + theme_minimal() + 
  labs(title="Oxygen Levels From 1949-2020 over 4 quarters",x = "Year", y = "Quarter")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
heat_map

#stationary heatmap with different oxygen scale,  different colors, and added units to legend

heat_map2<-ggplot(hm_data, aes(year,quarter)) + geom_tile(aes(fill = oxygen_perc),colour = "white", na.rm = TRUE) +
     scale_fill_gradientn(colours = c("red", "black", "blue"), breaks= c(3,3.5,4),labels=c("3 mg/L ","3.5 mg/L","4 mg/L")) +  
     guides(fill=guide_legend(title="Oxygen Levels"))+
     theme_bw() + theme_minimal() + 
     labs(title = "Oxygen Levels over quarters from 1949-2020 ",
                   x = "Year", y = "Quarter") +
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
heat_map2


# interactive heatmap with year and quarter

median_heatmap_quarterly<-plot_ly(x=hm_data$year, y=hm_data$quarter,z=hm_data$oxygen_perc, type="heatmap",colors="magma",reversescale=T, hovertemplate= "Year:%{x} <br> Quarter: %{y} <br> Median Oxygen Level: %{z}<extra></extra>")%>%
  layout(title="Median Oxygen Levels in Core CalCOFI stations up to 300 meters, over quarters",yaxis=yform, plot_bgcolor='grey')
median_heatmap_quarterly



# heatmap without median value of oxygen yearly
oxy_hm_wo_quarter <- function( percentile, date_min, date_max){
  
  bottle_sub <- bottle %>% 
    subset(station <= 60) %>%
    filter(line >= 76.7 & line <= 93.3, depth<=300) 
  
  h <- bottle_sub %>%
    group_by(year) %>%
    summarise(oxygen_perc = quantile(oxygen, probs = percentile/100, na.rm = TRUE),
              date = median(date, na.rm = T))
  
  return(h)
}

hm2_data<- oxy_hm_wo_quarter(50, '1949-02-28', '2020-01-26')
hm2_data<- hm2_data %>% group_by(year)

hm2_data$y<-rep(0,71)
ax <- list(showticklabels = FALSE)

median_heatmap_yearly<-plot_ly(x=hm2_data$year, y=hm2_data$y, z=hm2_data$oxygen_perc, type="heatmap", colors ="magma",reversescale=T, hovertemplate= "Year:%{x} <br> Median Oxygen Level: %{z}<extra></extra>")%>%
  layout(title="Median Oxygen Levels in Core CalCOFI stations up to 300 meters", yaxis=ax, plot_bgcolor='grey')
median_heatmap_yearly

#heatmap with mean levels of oxygen yearly
bottle_filter<- bottle %>% subset(station <= 60) %>%
  filter(line >= 76.7 & line <= 93.3, depth<=300) 

mean_data<-bottle_filter %>%
  group_by(year) %>%
  summarise(mean_temp = mean(oxygen,na.rm = TRUE))

b<-bottle_filter %>% group_by(year) %>% filter(year==1973)
b


mean_data$y<-rep(0,71)
ax <- list(showticklabels = FALSE)



mean_heatmap_yearly<-plot_ly(x=mean_data$year, y=mean_data$y, z=mean_data$mean_temp, type="heatmap", colors ="magma",reversescale=T, hovertemplate= "Year:%{x} <br> Mean Oxygen Level: %{z}<extra></extra>")%>%
  layout(title="Mean Oxygen Levels in Core CalCOFI stations up to 300 meters", yaxis=ax, plot_bgcolor='grey')
mean_heatmap_yearly



#heatmap with mean levels of oxygen showed quarterly 

bottle_filter<- bottle %>% subset(station <= 60) %>%
  filter(line >= 76.7 & line <= 93.3, depth<=300) 

mean_data_quarterly<-bottle_filter %>%
  group_by(year,quarter) %>%
  summarise(mean_temp = mean(oxygen,na.rm = TRUE))
mean_data_quarterly



total_mean_quarterly%>% group_by(year,quarter)
mean_heatmap_quarterly<-plot_ly(x=mean_data_quarterly$year, y=mean_data_quarterly$quarter,z=mean_data_quarterly$mean_temp, type="heatmap",colors="magma",reversescale=T, hovertemplate= "Year:%{x} <br> Quarter: %{y} <br> Mean Oxygen Level: %{z}<extra></extra>")%>%
  layout(title="Mean Oxygen Levels in Core CalCOFI stations up to 300 meters, over quarters",yaxis=yform, plot_bgcolor='grey')
mean_heatmap_quarterly



add_missing_data<-data.frame("year"=c(1956,1956,1965,1967,1967,1968,1968,1970,1970,1970,1971,1971,1971,1973,1973,
                                      1973,1974,1974,1974,1976,1977,1977,1977,1978,1980,1980,1980,1981,1982,1982,
                                      1982,1991,2009,2018,2020,2020,2020),
                             "quarter"=c(3,4,4,1,4,3,4,1,2,4,2,3,4,1,2,3,1,2,3,3,1,2,3,4,1,2,3,4,2,3,4,2,2,3,2,3,4),
                             "mean_temp"=c("NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN"
                                           ,"NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN",
                                           "NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN"))
add_missing_data
total_mean_quarterly<-rbind(add_missing_data,mean_data_quarterly)
total_mean_quarterly<- arrange(total_mean_quarterly,year)
mean_heatmap_quarterly2<-plot_ly(x=total_mean_quarterly$year, y=total_mean_quarterly$quarter,z=total_mean_quarterly$mean_temp, type="heatmap",colors="magma",reversescale=T, hovertemplate= "Year:%{x} <br> Quarter: %{y} <br> Mean Oxygen Level: %{z}<extra></extra>")%>%
  layout(title="Mean Oxygen Levels in Core CalCOFI stations up to 300 meters, over quarters",yaxis=yform, plot_bgcolor='grey')
mean_heatmap_quarterly2


# test to make sure values displayed are correct
test_ox_data<- bottle_filter %>% filter(year== 2000) %>% select(c("oxygen"))%>%na.omit()
ox_sum<-sum(test_ox_data$oxygen,na.rm = TRUE)
ox_sum/nrow(test_ox_data)


bottle_station_count <- bottle_filter %>%
  unite('station_id', line:station, remove = FALSE)

bottle_station_count <- bottle_station_count %>%
  unite('time', c(year, quarter), remove = FALSE)

bottle_station_count<-bottle_station_count%>% select(c("station_id",time,oxygen))
bottle_station_count<-na.omit(bottle_station_count)


station_number_check<-bottle_station_count %>%select(c("station_id","time"))
station_number_check<-station_number_check %>%unique()%>% count(time)
station_number_check

bottle_station_count <- bottle_filter %>%
  unite('station_id', line:station, remove = FALSE)

year_check_pre<-bottle_station_count%>% select(c("station_id","year","oxygen"))
year_check_pre<-na.omit(bottle_station_count)
year_check<-bottle_station_count %>%select(c("station_id","year"))
year_check<-year_check %>%unique()%>% count(year)
year_check

write.csv(year_check,"/Users/annalieseadams/Desktop/year_check.csv",row.names=FALSE)
write.csv(station_number_check,"/Users/annalieseadams/Desktop/station_check.csv",row.names=FALSE)

write.csv(f,"/Users/annalieseadams/Desktop/n.csv",row.names=FALSE)

f<-bottle_filter%>%group_by(year)%>%count()
f