library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


## this code takes the cleaned data and splits it into animals and converts the cumm value into counts

path_output_files <- "W:/VF/Sheep_Chiswick_2022/animal_logs/jax_working/"
VF_animals_logs <- read_csv(paste0(path_output_files,"animals_GPS_trim_time_step1.csv"))
names(VF_animals_logs)


### check on records

check_records <- VF_animals_logs %>% 
  dplyr::group_by(date,deviceName, Sheep_ID) %>% 
  dplyr::summarise(n_AudioCount = sum(cumulativeAudioCount),
                   n_ShockCount = sum(cumulativeShockCount)
                   )


check_records <- check_records %>% 
  arrange( Sheep_ID,date,deviceName, n_AudioCount )

### check on values
check_min_max <- VF_animals_logs %>% 
  dplyr::group_by(date,deviceName, Sheep_ID) %>% 
  dplyr::summarise(min_AudioCount = min(cumulativeAudioCount,na.rm = TRUE),
            max_AudioCount = max(cumulativeAudioCount,na.rm = TRUE),
            min_ShockCount = sum(cumulativeShockCount,na.rm = TRUE),
            max_ShockCount = max(cumulativeShockCount,na.rm = TRUE)
  )

check_min_max <- check_min_max %>% 
  arrange(Sheep_ID,deviceName,date, min_AudioCount )




#make a list###
str(VF_animals_logs)

#list_animals <- "1_1390038"
list_animals <- VF_animals_logs %>% 
  dplyr::select(deviceName) %>% 
  distinct(deviceName)
list_animals <-ungroup(list_animals)






list_animals$deviceName[1:34]
list_animals <- c(
  9380265,
  9380451,
  9380470 ,
  9380674,
  9380743,
  9380142 
  )



for (list_animals in list_animals){
  
  
  ##################################################################################################################
  
  
  deviceName_x <- sub("*._", "", list_animals)
  
  df <- VF_animals_logs %>% 
    filter(deviceName == deviceName_x)
    #filter(deviceName == 490705)
  
  df <- df %>% arrange(local_time)
  names(df)
  df <- df %>% 
    mutate(Audio_values = cumulativeAudioCount - lag(cumulativeAudioCount))
  #test <- df %>%  select(deviceName, local_time, cumulativeAudioCount, Audio_values)
  df <- df %>% 
    mutate(Shock_values = cumulativeShockCount - lag(cumulativeShockCount))
  
  df <- df %>%  dplyr::select(ID_jaxs:fencesID ,Audio_values, Shock_values, resting_percentage:Y   )
  
  name <- paste0("temp_",deviceName_x)
  assign(name,df)
}

## merge into one file and remove the renaming records



Fence_all <- rbind(
  temp_9380265,
  temp_9380451,
  temp_9380470 ,
  temp_9380674,
  temp_9380743,
  temp_9380142 
  
)
rm( 
  temp_9380265,
  temp_9380451,
  temp_9380470 ,
  temp_9380674,
  temp_9380743,
  temp_9380142
  )






path_output_files
write.csv(Fence_all, 
          paste0(path_output_files,"/animal_GPS_data_step1_2.csv"), 
          row.names=FALSE)

