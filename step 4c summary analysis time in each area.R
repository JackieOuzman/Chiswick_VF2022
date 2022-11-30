

### Summary analysis how much time in VF and non VF parts of the paddock

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


step1_2_3 <- read_csv("W:/VF/Sheep_Lameroo_2022/animal_logs/jax_working/animal_GPS_data_step1_2_3.csv")

str(step1_2_3)
total_time <- step1_2_3 %>% 
  dplyr::select(local_time,
                date,
                DOY,
                Sheep_ID,
                training_period)
str(total_time)
total_time_trial <- total_time %>% 
  filter(training_period == "non_training")


#### ---------------- what is the max and min time for the whole trial ----------------#### 

start <- min(total_time_trial$local_time, na.rm = TRUE) #"2022-10-17 13:11:15 ACDT"
end <-   max(total_time_trial$local_time, na.rm = TRUE) # "2022-10-21 11:41:23 ACDT"
#Since we’re dealing with elapsed time between two dates, let’s start with Intervals. We can define an Interval using the %--% operator.

time.interval <- start %--% end
time.interval
#To create a Duration between these two dates, we can use the as.duration function.

time.duration <- as.duration(time.interval)
time.duration # "340208s (~3.94 days)"



#### ---------------- how many seconds were the animals in the yard not part of the trial? ----------------#### 



yard_start_18 <- ymd_hms("2022-10-18 09:40:00", tz = "Australia/Adelaide")
yard_end_18 <-   ymd_hms("2022-10-18 10:30:00", tz = "Australia/Adelaide")

yarded_interval_18 <- yard_start_18 %--% yard_end_18
yarded_duration_18 <- as.duration(yarded_interval_18)
yarded_duration_18 #"3000s (~50 minutes)"


yard_start_19 <- ymd_hms("2022-10-19 09:10:00", tz = "Australia/Adelaide")
yard_end_19 <-   ymd_hms("2022-10-19 10:18:00", tz = "Australia/Adelaide")

yarded_interval_19 <- yard_start_19 %--% yard_end_19
yarded_duration_19 <- as.duration(yarded_interval_19)
yarded_duration_19 #"4080s (~1.13 hours)"


yard_start_20 <- ymd_hms("2022-10-20 08:58:00", tz = "Australia/Adelaide")
yard_end_20 <-   ymd_hms("2022-10-20 10:19:00", tz = "Australia/Adelaide")

yarded_interval_20 <- yard_start_20 %--% yard_end_20
yarded_duration_20 <- as.duration(yarded_interval_20)
yarded_duration_20 #"4860s (~1.35 hours)"

rm(yard_start_18, yard_end_18, yard_start_19, yard_end_19,yard_start_20, yard_end_20)

### sum of yarded time 

sum_yard_time <- sum(yarded_duration_18, yarded_duration_19, yarded_duration_20)
sum_yard_time_duration <- as.duration(sum_yard_time)
sum_yard_time_duration #"11940s (~3.32 hours)"


#### ---------------- remove the yarded time from the lenght of the trail  ----------------#### 

lenght_of_trail <- time.duration - sum_yard_time_duration
lenght_of_trail #"328268s (~3.8 days)"
#### don't forget to times this by all the animals
lenght_of_trail <- lenght_of_trail*10

lenght_of_trail
##
rm(total_time, total_time_trial)
rm(end, start, time.duration, time.interval, 
   yarded_duration_18, yarded_duration_19, yarded_duration_20, 
   yarded_interval_18, yarded_interval_19, yarded_interval_20)










#### ---------------- for each sheep how much time did they spend outside the VF  ----------------#### 
### maybe I can make a new variable in exclusion zone.




############################################################################################
############                  bring in boundaries             ##############################
############################################################################################



Lameroo_Vf_area_hard_fence_bound <- st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_working/HF_Lameroo_rough_proj.shp")  # this is the hard fences
Lameroo_Vf_area_hard_fence_bound_buff <- st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/HF_Lameroo_rough_10_proj.shp")  # this is the 

Lameroo_Vf_area <-                  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_proj.shp")
Lameroo_Vf_area_buffer_10 <-                  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_Buffer10_proj.shp")
water_pt <-  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/water_pts.shp")

Exclusion_zone  <- st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/Exclusion_zone.shp")



Lameroo_Vf_area_hard_fence_bound <-
  st_transform(Lameroo_Vf_area_hard_fence_bound, crs = 28354)

Lameroo_Vf_area_buffer_10 <-
  st_transform(Lameroo_Vf_area_buffer_10, crs = 28354)
Lameroo_Vf_area <-
  st_transform(Lameroo_Vf_area, crs = 28354)
water_pt <-
  st_transform(water_pt, crs = 28354)

Lameroo_Vf_area_hard_fence_bound_buff<-
  st_transform(Lameroo_Vf_area_hard_fence_bound_buff, crs = 28354)

Exclusion_zone<-  st_transform(Exclusion_zone, crs = 28354)

################################################################
### Clip to the VF hard fences  with 10 meter buffer   #########
################################################################



step1_2_3 <- read_csv("W:/VF/Sheep_Lameroo_2022/animal_logs/jax_working/animal_GPS_data_step1_2_3.csv")
step1_2_3_trial <- step1_2_3 %>%  filter(training_period == "non_training")
str(step1_2_3_trial)
step1_2_3_trial <- step1_2_3_trial %>% 
  mutate( hour = hour(local_time),
          minute = minute(local_time))

step1_2_3_trial <- step1_2_3_trial %>%  dplyr::select(Sheep_ID, date, training_period, local_time,hour,  minute, X, Y )
#turn into spatial data
step1_2_3_trial_sf <-   st_as_sf(step1_2_3_trial,
                         coords = c("X", "Y"),
                         crs = 28354,
                         agr = "constant")



#To the VF area
step1_2_3_trial_sf_clip_Excl <-
  st_intersection(step1_2_3_trial_sf, Exclusion_zone)

str(step1_2_3_trial_sf_clip_Excl)

plotVF <- ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA, linetype = "dashed", size = 0.5) +
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = step1_2_3_trial_sf_clip_Excl ,alpha = 0.2) +
  facet_wrap(.~ date                )+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())#+
# labs(title = "Animal logs",
#              subtitle = "log when animals were yarded removed, and clipped to 10 meter buffer")
plotVF


### ----- when did an animal cross over to the exlusion zone and how long did it last?
step1_2_3_trial_sf_clip_Excl %>%  distinct(date)
str(step1_2_3_trial_sf_clip_Excl)

### ----- I can assume every unique time entry is about 10 min block of time----#####


count_entries_per_sheep <- step1_2_3_trial_sf_clip_Excl %>%  
  group_by(Sheep_ID, date) %>% 
  distinct(local_time, .keep_all = TRUE ) %>% 
  tally()
  
count_entries_per_sheep <- count_entries_per_sheep %>% 
  mutate(Time_in_exclusion_zone_19 = 10*n,
         D_Time_in_exclusion_zone_19 = duration(num = Time_in_exclusion_zone_19, units = "minutes"))

count_entries_per_sheep <- ungroup(count_entries_per_sheep)
count_entries_per_sheep <- count_entries_per_sheep %>%  
  dplyr::select(Sheep_ID, date, n, Time_in_exclusion_zone_19, D_Time_in_exclusion_zone_19)  
count_entries_per_sheep
##############################################################
sum_clm <- count_entries_per_sheep %>% summarize_if(is.numeric, sum, na.rm=TRUE)
sum_clm


Time_in_exclusion_zone <- count_entries_per_sheep_sum_clm
# Time_in_exclusion_zone <- count_entries_per_sheep_sum_clm$Time_in_exclusion_zone_19
Time_in_exclusion_zone


lenght_of_trail
exclusion <- sum_clm$D_Time_in_exclusion_zone_19
exclusion

(255000/3282680)*100
