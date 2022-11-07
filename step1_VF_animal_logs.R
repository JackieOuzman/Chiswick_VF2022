library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)



### bring in animal logs for VF all

animal_GPS_data <- read_csv("W:/VF/Sheep_Chiswick_2022/raw_data/trial_csiro_armidale_chiswick_mob_259_mob1_filtered.csv")
#format time and date clm from character to time
animal_GPS_data <-
  animal_GPS_data %>%
  mutate(timeOfEvent = as.POSIXct(timeOfEvent, tz = "GMT", format = "%d/%m/%Y %H:%M"))


animal_GPS_data <- animal_GPS_data %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"))

animal_GPS_data <- animal_GPS_data %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Sydney"))

## Add a clm for ID_jaxs
animal_GPS_data <- animal_GPS_data %>% 
  dplyr::mutate( ID_jaxs = row_number())


### what are the fences callled in this dataset?
unique(animal_GPS_data$fencesID) # we only have 3: "1dd82" "1f1eb" "1766d"and NULL


## reorder the clms
animal_GPS_data <- animal_GPS_data %>% 
  dplyr::select(ID_jaxs,deviceUIDHex:local_time)



animal_GPS_data <- animal_GPS_data %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Sydney"),
         DOY = yday(date))


#############################################################################################
####    Assign collar to sheep names #####
unique(animal_GPS_data$deviceName)
#Sue suggests that these were the ones used via email Thu 3/11/2022 10:35 AM
# 9380422
# 9380674
# 9380743
# 9380451
# 9380265

animal_GPS_data <- animal_GPS_data %>% 
  mutate(Sheep_ID = case_when(
    #deviceName == 9380142 ~ "xx",
    # deviceName == 9380268  ~ "2", 
      #after 18/10 I assume at 10:30
    # deviceName == 9380470  ~ "4",
    # deviceName == 9380479  ~ "5",
    # deviceName == 9380787 ~ "8",
    
    deviceName == 9380422  ~ "1",
    deviceName == 9380674  ~ "2",
    deviceName == 9380743  ~ "3",
    deviceName == 9380451  ~ "4",
    deviceName == 9380265  ~ "5",
  
    TRUE                      ~ "other"
    
  ))

#only keep the collar that sue said:)
animal_GPS_data <- animal_GPS_data %>%
  filter(Sheep_ID != "other")

############################################################################################
############                  Turn into spatial data          ##############################
############################################################################################
#str(animal_GPS_data)

## remove null values in coodinates
animal_GPS_data <- animal_GPS_data %>% 
  filter(!is.na(gpsData.lng))

#turn into spatial data
animal_GPS_data_sf <-
  st_as_sf(animal_GPS_data,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

animal_GPS_data_sf_trans <-
  st_transform(animal_GPS_data_sf, crs = 28355)


rm(animal_GPS_data,animal_GPS_data_sf )





############################################################################################
############                  bring in boundaries             ##############################
############################################################################################




Chiswick_hard_fence_bound <- st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/Chiswick_paddock_boundary.shp")  # this is the hard fences

Chiswick_hard_fence_bound <-
  st_transform(Chiswick_hard_fence_bound, crs = 28355)

###------------------------------ UP TO HERE ----------------------------------------#######

Lameroo_Vf_area_hard_fence_bound_buff <- st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/HF_Lameroo_rough_10_proj.shp")  # this is the 

Lameroo_Vf_area <-                  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_proj.shp")
Lameroo_Vf_area_buffer_10 <-                  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_Buffer10_proj.shp")
water_pt <-  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/water_pts.shp")







Lameroo_Vf_area_buffer_10 <-
  st_transform(Lameroo_Vf_area_buffer_10, crs = 28355)
Lameroo_Vf_area <-
  st_transform(Lameroo_Vf_area, crs = 28355)
water_pt <-
  st_transform(water_pt, crs = 28355)

Lameroo_Vf_area_hard_fence_bound_buff<-
  st_transform(Lameroo_Vf_area_hard_fence_bound_buff, crs = 28355)


ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = animal_GPS_data_sf_trans ,alpha = 0.01) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "all animal logs with a buffer of 10m")



ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = animal_GPS_data_sf_trans ,alpha = 0.01) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
facet_wrap(. ~ date)+
  labs(title = "all animal logs, dates as facet")









# --------------------------------------------------------------------------------------------------------------------- #
###### Need to work out what the start time 
#Rick said there was a time the animals moved back in the VF area after they were in the south of the paddock and this is the satrt time

GPS_data_sf_trans_17 <- animal_GPS_data_sf_trans %>% filter(date=="2022-10-17")

GPS_data_sf_trans_17 <- GPS_data_sf_trans_17 %>% 
  mutate(hour = hour(local_time),
         minute = minute(local_time))

ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = GPS_data_sf_trans_17 ,alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  facet_wrap(.~ hour)+
  labs(title = "Day 1, hours as facet")

#so 11am to 14 looks like the window make 3 df for each of these hrs

GPS_data_sf_trans_17_Hr_11 <- GPS_data_sf_trans_17 %>% 
  filter(local_time >= ymd_hms("2022-10-17 11:00:00", tz= "Australia/Adelaide"), #yyy-mm-dd hh:mm:ss
         local_time <=  ymd_hms("2022-10-17 12:00:00", tz= "Australia/Adelaide"))

GPS_data_sf_trans_17_Hr_12 <- GPS_data_sf_trans_17 %>% 
  filter(local_time >= ymd_hms("2022-10-17 12:00:00", tz= "Australia/Adelaide"), #yyy-mm-dd hh:mm:ss
         local_time <=  ymd_hms("2022-10-17 13:00:00", tz= "Australia/Adelaide")) 

GPS_data_sf_trans_17_Hr_13 <- GPS_data_sf_trans_17 %>% 
  filter(local_time >= ymd_hms("2022-10-17 13:00:00", tz= "Australia/Adelaide"), #yyy-mm-dd hh:mm:ss
         local_time <=  ymd_hms("2022-10-17 14:00:00", tz= "Australia/Adelaide"))  

ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = GPS_data_sf_trans_17_Hr_11 ,alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "Day 1 at 11am minute as facet")+
  facet_wrap(.~ minute)
  
ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = GPS_data_sf_trans_17_Hr_12 ,alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "Day 1 at 12am minute as facet")+
  facet_wrap(.~ minute)

ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = GPS_data_sf_trans_17_Hr_12 ,alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "Day 1 at 12am minute as facet")+
  facet_wrap(.~ minute)


ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = GPS_data_sf_trans_17_Hr_13 ,alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "Day 1 at 1pm minute as facet")+
  facet_wrap(.~ minute)

### so it look like the VF started at 13:10 prior to this the animals were training
## the data will need a new clm for training and VF trial

rm( GPS_data_sf_trans_17_Hr_12, GPS_data_sf_trans_17_Hr_13)




### ok I also need to know when the training period started

ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = GPS_data_sf_trans_17 ,alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  facet_wrap(.~ hour)+
  labs(title = "Day 1, hours as facet")

## looks like they entered the paddock at 11am - but what was the exact time? look like it was 11:40
ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = GPS_data_sf_trans_17_Hr_11 ,alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "Day 1 at 11am minute as facet")+
  facet_wrap(.~ minute)

rm( GPS_data_sf_trans_17, GPS_data_sf_trans_17_Hr_11)


# --------------------------------------------------------------------------------------------------------------------- #










################################################################################
#### filtering out data based on times start of training period to the end of the trial ####

# start of trial and training period (according to sue) - keep everything after  17th 11:35 or 11:40 s above

animal_GPS_data_sf_trans <- animal_GPS_data_sf_trans %>% 
  filter(
  local_time >=  ymd_hms("2022-10-17 11:40:00", tz= "Australia/Adelaide"))

animal_GPS_data_sf_trans <- animal_GPS_data_sf_trans %>% 
  filter(
    local_time <=  ymd_hms("2022-10-21 11:50:00", tz= "Australia/Adelaide"))





### define a training period with new clm

animal_GPS_data_sf_trans <- animal_GPS_data_sf_trans %>% 
  mutate(training_period = case_when(
    local_time <= ymd_hms("2022-10-17 13:10:00", tz= "Australia/Adelaide")~ "training",
    TRUE                      ~ "non_training"
    
  ))





#### each day the animals were yarded so i need to remove this data

# let divide the data per day
day_17 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-10-17")
day_18 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-10-18")
day_19 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-10-19")
day_20 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-10-20")
day_21 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-10-21")

# keep everything after before yarding and after yarding

day_18_before_yarding <- day_18 %>%
  filter(local_time <=  ymd_hms("2022-10-18 09:40:00", tz = "Australia/Adelaide"))
day_18_after_yarding <- day_18 %>%
  filter(local_time >=  ymd_hms("2022-10-18 10:30:00", tz = "Australia/Adelaide"))
                  
day_18_clean <- rbind(day_18_before_yarding, day_18_after_yarding)
rm(day_18_before_yarding, day_18_after_yarding, day_18)


day_19_before_yarding <- day_19 %>%
  filter(local_time <=  ymd_hms("2022-10-19 09:10:00", tz = "Australia/Adelaide"))
day_19_after_yarding <- day_19 %>%
  filter(local_time >=  ymd_hms("2022-10-19 10:18:00", tz = "Australia/Adelaide"))

day_19_clean <- rbind(day_19_before_yarding, day_19_after_yarding)
rm(day_19_before_yarding, day_19_after_yarding, day_19)



day_20_before_yarding <- day_20 %>%
  filter(local_time <=  ymd_hms("2022-10-20 08:58:00", tz = "Australia/Adelaide"))
day_20_after_yarding <- day_20 %>%
  filter(local_time >=  ymd_hms("2022-10-20 10:19:00", tz = "Australia/Adelaide"))

day_20_clean <- rbind(day_20_before_yarding, day_20_after_yarding)
rm(day_20_before_yarding, day_20_after_yarding, day_20)


### put it back togther 

animals_GPS_trim_time <- rbind(day_17, day_18_clean, day_19_clean, day_19_clean, day_20_clean, day_21)

rm(day_17, day_18_clean, day_19_clean, day_21, day_20_clean, animal_GPS_data_sf_trans)

########################################################################################





### remove the water and other animals logs

unique(animals_GPS_trim_time_non_train$Sheep_ID)

animals_GPS_trim_time <- animals_GPS_trim_time %>% 
  filter(Sheep_ID !=  "other") %>% 
  filter(Sheep_ID !=  "water_pt")


ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  geom_sf(data = water_pt ,color ="Blue") +
  geom_sf(data = animals_GPS_trim_time ,alpha = 0.05) +
  facet_wrap(.~ date)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "All animal logs between 17th at 11:40 and 21st at 11:50",
  subtitle = "log when animals were yarded removed")



# -------------------------------------------------------------------------------------------------- ###


#I think this should be the end of step 1.







########################################################################################################



output_path <- "W:/VF/Sheep_Lameroo_2022/animal_logs/jax_working"  #animals_GPS_trim_time


############################################################################################################################
### format the aniaml log data so I output the clm with local time and keep time difference cals and have clm for x and y

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(animals_GPS_trim_time))
animals_GPS_trim_time_df <- as.data.frame(animals_GPS_trim_time)

animals_GPS_trim_time_df <- animals_GPS_trim_time_df %>% 
  dplyr::select(-"geometry")


animals_GPS_trim_time <-   cbind(animals_GPS_trim_time_df,coordinates )
## ensure the date and time clms are outputting and outputting in the correct format.


animals_GPS_trim_time$local_time <-   format(animals_GPS_trim_time$local_time, usetz=TRUE)
animals_GPS_trim_time$GMT        <-   format(animals_GPS_trim_time$GMT, usetz=TRUE)
animals_GPS_trim_time$start_fence <-  format(animals_GPS_trim_time$start_fence, usetz=TRUE)
animals_GPS_trim_time$end_fence    <- format(animals_GPS_trim_time$end_fence, usetz=TRUE)
animals_GPS_trim_time$start_trial    <- format(animals_GPS_trim_time$start_trial, usetz=TRUE)

write.csv(animals_GPS_trim_time, 
          paste0(output_path,"/animals_GPS_trim_time_step1.csv"), 
          row.names=FALSE)
#############################################################



