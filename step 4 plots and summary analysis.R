### plots and summary analysis

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

water_pt <-  st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/water_pt.shp")


############################################################################################


###########################################################################################
############                  bring in step 1 2 and 3 df             ##############################
############################################################################################

step1_2_3 <- read_csv("W:/VF/Sheep_Chiswick_2022/animal_logs/jax_working/animal_GPS_data_step1_2_3.csv")

#turn into spatial data
step1_2_3_sf <-   st_as_sf(step1_2_3,
                       coords = c("X", "Y"),
                       crs = 28355,
                       agr = "constant")





names(step1_2_3_sf)

check <-
  step1_2_3_sf %>% dplyr::select( "deviceName"  ,
                           "Audio_values"    ,
                           "Shock_values" , 
                           "local_time"          ,  
                           "Sheep_ID" )





## TASK Mean audio/pulse ratio AIM: Across all animals
names(step1_2_3_sf)
# step 1 summaries audio and pulse per animal per day also training period 
summary_audio_ratio <- step1_2_3_sf %>% 
  dplyr::group_by(Sheep_ID, date) %>% 
  dplyr::summarise(audio_sum = sum(Audio_values, na.rm = TRUE),
            pulse_sum = sum(Shock_values, na.rm = TRUE),
            ratio_sum1 = audio_sum/ (pulse_sum+audio_sum )*100,
            ratio_sum2 = pulse_sum/ (audio_sum )*100)
  
summary_audio_ratio <- ungroup(summary_audio_ratio)

summary_audio_ratio$ratio_sum1 [is.nan(summary_audio_ratio$ratio_sum1 )]<-NA
summary_audio_ratio$ratio_sum2 [is.nan(summary_audio_ratio$ratio_sum2 )]<-NA

names(summary_audio_ratio)
summary_audio_ratio %>%
  ggplot(aes(x = date , y = ratio_sum1)) +
  geom_col()+
  theme_classic() +
  facet_wrap(.~Sheep_ID)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    x = "Date",
    y = "ratio (Audio / pulse+audio)*100 ",
    title = "Animals fitted with VF collars",
    subtitle = "audio and pulse counts summed per day and animal and then ratio calulated")
  
summary_audio_ratio

# step 2 summaries audio and pulse per animal per day also training period 

summary_audio_ratio_1 <- summary_audio_ratio %>% 
  dplyr::summarise(audio_av = mean(audio_sum, na.rm = TRUE),
            pulse_av = mean(pulse_sum, na.rm = TRUE),
            
            std_dev_Av_Audio = sd(audio_sum, na.rm = TRUE),
            SE_Av_Audio = std_dev_Av_Audio / sqrt(n()),
            
            std_dev_Av_Pulse = sd(pulse_sum, na.rm = TRUE),
            SE_Av_Pulse = std_dev_Av_Pulse / sqrt(n()),
            
            ratio_1_mean = mean(ratio_sum1, na.rm= TRUE),
            ratio_2_mean = mean(ratio_sum2, na.rm= TRUE),
            
            std_dev_Av_Ratio_1 = sd(ratio_sum1, na.rm = TRUE),
            SE_Av_std_dev_Av_Ratio_1 = std_dev_Av_Ratio_1 / sqrt(n()),
            
            std_dev_Av_Ratio_2 = sd(ratio_sum2, na.rm = TRUE),
            SE_Av_std_dev_Av_Ratio_2 = std_dev_Av_Ratio_2 / sqrt(n())
            
            )
  




summary_audio_ratio_1 <- as.data.frame(summary_audio_ratio_1)

summary_audio_ratio_1 <- summary_audio_ratio_1 %>%
  dplyr::select(
                audio_av ,  
                pulse_av  ,   
                std_dev_Av_Audio ,
                SE_Av_Audio , 
                std_dev_Av_Pulse ,
                SE_Av_Pulse,  
                ratio_1_mean  ,
                ratio_2_mean , 
                std_dev_Av_Ratio_1  ,    
                SE_Av_std_dev_Av_Ratio_1 , 
                std_dev_Av_Ratio_2,
                SE_Av_std_dev_Av_Ratio_2)



summary_audio_ratio_1





## GPS Plots

#AIM: Daily plots are probably sufficient. 

#But more frequent plots during the training period.
#And there was that instance where they broke through the fence overnight (we think something spooked them), 
#so a more detailed breakdown of that could also be interesting too. 


#### ------------------ PLOTS similar to what I did FOR DANA AND PAPER 10/11/2022-----------------------------#####

plot1 <- ggplot() +
  geom_sf(data = Chiswick_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_paddock, color = "black", fill = NA) +
  geom_sf(data = Chiswick_hard_fence_bound_buff, color = "black", fill = NA) +
  geom_sf(data = water_pt ,color ="Blue") +
  geom_sf(data = step1_2_3_sf ,alpha = 0.05) +
  facet_wrap(.~ date)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())#+
  # labs(title = "Animal logs in during trial",
  #      subtitle = "log when animals were yarded removed, and clipped to 10 meter buffer")
plot1


ggsave(plot1,
       device = "png",
       filename = paste0("Chiswick_sheep_all_days_noTitle_Buffer_bound.png"),
       path= "W:/VF/Sheep_Chiswick_2022/plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)






