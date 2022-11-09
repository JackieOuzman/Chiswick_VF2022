## step 3 clipping the data - This has not been run yet

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


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


############################################################################################



################################################################
### Clip to the VF hard fences  with 10 meter buffer   #########
################################################################



step1_2 <- read_csv("W:/VF/Sheep_Chiswick_2022/animal_logs/jax_working/animal_GPS_data_step1_2.csv")

#turn into spatial data
step1_2_sf <-   st_as_sf(step1_2,
                       coords = c("X", "Y"),
                       crs = 28355,
                       agr = "constant")







#To the large block boundary
step1_2_sf_clip <-
  st_intersection(step1_2_sf, Chiswick_hard_fence_bound_buff)


## remove all the rows that don't have fence ID
#unique(animal_GPS_data_sf_trans_clip$fencesID)
step1_2_sf_clip <-step1_2_sf_clip %>%
  filter(!is.na(fencesID) ) %>%
  filter(fencesID !=  "NULL")


## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(step1_2_sf_clip))
step1_2_sf_clip_df <- as.data.frame(step1_2_sf_clip)

step1_2_sf_clip_df <- step1_2_sf_clip_df %>% 
  dplyr::select(-"geometry")


step1_2_sf_clip_df <-   cbind(step1_2_sf_clip_df,coordinates )






path_output_files <- "W:/VF/Sheep_Chiswick_2022/animal_logs/jax_working/"
path_output_files
write.csv(step1_2_sf_clip_df, 
          paste0(path_output_files,"/animal_GPS_data_step1_2_3.csv"), 
          row.names=FALSE)
