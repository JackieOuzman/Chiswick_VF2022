library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)



### bring in animal logs for VF all

animal_GPS_data <- read_csv("W:/VF/Sheep_Chiswick_2022/raw_data/db_trial_csiro_armidale_chiswick_mob_259_filtered.csv")
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


## I am having a spot of trouble with collar 9380422

 collar_9380422 <- animal_GPS_data %>% 
   filter(deviceName== "9380422")
 max(collar_9380422$local_time)
 min(collar_9380422$local_time)

# ### what are the fences callled in this dataset?
# unique(animal_GPS_data$fencesID) # we only have 3: "1dd82" "1f1eb" "1766d"and NULL
# 
# ### when were these 3 fences active?
# fence_1dd82 <- animal_GPS_data %>% 
#   filter(fencesID== "1dd82")
# 
# max(fence_1dd82$local_time)
# min(fence_1dd82$local_time)
# 
# fence_1f1eb <- animal_GPS_data %>% 
#   filter(fencesID== "1f1eb")
# 
# max(fence_1f1eb$local_time)
# min(fence_1f1eb$local_time)
# 
# fence_1766d <- animal_GPS_data %>% 
#   filter(fencesID== "1766d")
# 
# max(fence_1766d$local_time)
# min(fence_1766d$local_time)
#rm(fence_1766d, fence_1dd82, fence_1f1eb)

#looks like I will use 1dd82 so only keep these records - not super confident with this 
 #if I keep only these records I loose a sheep collar 9380422
 # I think more than one fence was used, but perhaps it wasnt recorded.

# animal_GPS_data <- animal_GPS_data %>% 
#      filter(fencesID== "1dd82")
#    

## ok lets just remove the Nulls
  animal_GPS_data <- animal_GPS_data %>% 
       filter(fencesID!= "NULL")
     
str(animal_GPS_data)


## reorder the clms
animal_GPS_data <- animal_GPS_data %>% 
  dplyr::select(ID_jaxs,Sheep_ID, deviceUIDHex:local_time)



animal_GPS_data <- animal_GPS_data %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Sydney"),
         DOY = yday(date))



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




Chiswick_hard_fence_bound <- st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/Chiswick_paddock_boundary_final.shp")  # this is the hard fences

Chiswick_hard_fence_bound <-
  st_transform(Chiswick_hard_fence_bound, crs = 28355)


Chiswick_hard_fence_bound_buff <- st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/Chiswick_paddock_boundary_final_buff10.shp")  # this is the 

Chiswick_hard_fence_bound_buff <-
  st_transform(Chiswick_hard_fence_bound_buff, crs = 28355)


VF_paddock <-   st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/VF_paddock.shp")

VF_paddock <-  st_transform(VF_paddock, crs = 28355)

#water_pt <-  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/water_pts.shp")





ggplot() +
  geom_sf(data = Chiswick_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_paddock, color = "black", fill = NA) +
  geom_sf(data = Chiswick_hard_fence_bound_buff, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = animal_GPS_data_sf_trans ,alpha = 0.03) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "all animal logs with a buffer of 10m")



ggplot() +
  geom_sf(data = Chiswick_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_paddock, color = "black", fill = NA) +
  geom_sf(data = Chiswick_hard_fence_bound_buff, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = animal_GPS_data_sf_trans ,alpha = 0.05) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
facet_wrap(. ~ date)+
  labs(title = "all animal logs, dates as facet")




str(animal_GPS_data_sf_trans)




# --------------------------------------------------------------------------------------------------------------------- #




################################################################################
#### filtering out data based on times trail 28/6- 9:50 and end 2/7- at 10:10 – this is based on the times Dana gave me

# start of trial (according to sue) - keep everything after  17th 11:35 or 11:40 s above

animal_GPS_data_sf_trans <- animal_GPS_data_sf_trans %>% 
  filter(
  local_time >=  ymd_hms("2022-06-28 09:50:00", tz= "Australia/Sydney"))

animal_GPS_data_sf_trans <- animal_GPS_data_sf_trans %>% 
  filter(
    local_time <=  ymd_hms("2022-07-02 10:10:00", tz= "Australia/Sydney"))








# Times sheep were brought in each day for the VF Chiswick trial;
# 28/6- sheep out 9:50
# 29/6 11:21- 12:21
# 30/6 10:34- 11:36
# 1/7- 10:37- 11:20
# 2/7- Brought in at 10:10
#### each day the animals were yarded so i need to remove this data

# let divide the data per day
day_28 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-06-28")
day_29 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-06-29")
day_30 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-06-30")
day_1 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-07-01")
day_2 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-07-02")

# keep everything after before yarding and after yarding

day_29_before_yarding <- day_29 %>%
  filter(local_time <=  ymd_hms("2022-06-29 11:21:00", tz = "Australia/Sydney"))
day_29_after_yarding <- day_29 %>%
  filter(local_time >=  ymd_hms("2022-06-29 12:21:00", tz = "Australia/Sydney"))
                  
day_29_clean <- rbind(day_29_before_yarding, day_29_after_yarding)
rm(day_29_before_yarding, day_29_after_yarding, day_29)


day_30_before_yarding <- day_30 %>%
  filter(local_time <=  ymd_hms("2022-06-30 10:34:00", tz = "Australia/Sydney"))
day_30_after_yarding <- day_30 %>%
  filter(local_time >=  ymd_hms("2022-06-30 11:36:00", tz = "Australia/Sydney"))

day_30_clean <- rbind(day_30_before_yarding, day_30_after_yarding)
rm(day_30_before_yarding, day_30_after_yarding, day_30)

day_1_before_yarding <- day_1 %>%
  filter(local_time <=  ymd_hms("2022-07-01 10:37:00", tz = "Australia/Sydney"))
day_1_after_yarding <- day_1 %>%
  filter(local_time >=  ymd_hms("2022-07-01 11:20:00", tz = "Australia/Sydney"))

day_1_clean <- rbind(day_1_before_yarding, day_1_after_yarding)
rm(day_1_before_yarding, day_1_after_yarding, day_1)


### put it back togther 

animals_GPS_trim_time <- rbind(day_28, day_29_clean, day_30_clean, day_1_clean, day_2)

rm(day_28, day_29_clean, day_30_clean, day_1_clean, day_2)

########################################################################################





### remove the water and other animals logs


animals_GPS_trim_time <- animals_GPS_trim_time %>% 
  filter(Sheep_ID !=  "other") %>% 
  filter(Sheep_ID !=  "water_pt")


ggplot() +
  geom_sf(data = Chiswick_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_paddock, color = "black", fill = NA) +
  geom_sf(data = Chiswick_hard_fence_bound_buff, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = animal_GPS_data_sf_trans ,alpha = 0.05) +
  facet_wrap(.~ date)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "All animal logs between 28/06 at 09:50 and 02/07 at 10:10",
  subtitle = "log when animals were yarded removed")



# -------------------------------------------------------------------------------------------------- ###


#I think this should be the end of step 1.







########################################################################################################



output_path <- "W:/VF/Sheep_Chiswick_2022/animal_logs/jax_working"  #animals_GPS_trim_time


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



