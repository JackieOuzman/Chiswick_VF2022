### plots and summary analysis

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)

###########################################################################################
############                  bring in boundaries             ##############################
############################################################################################



Lameroo_Vf_area_hard_fence_bound <- st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_working/HF_Lameroo_rough_proj.shp")  # this is the hard fences
Lameroo_Vf_area_hard_fence_bound_buff <- st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/HF_Lameroo_rough_10_proj.shp")  # this is the 

Lameroo_Vf_area <-                  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_proj.shp")
Lameroo_Vf_area_buffer_10 <-                  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_Buffer10_proj.shp")
water_pt <-  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/water_pts.shp")





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

###########################################################################################
############                  bring in step 1 2 and 3 df             ##############################
############################################################################################

step1_2_3 <- read_csv("W:/VF/Sheep_Lameroo_2022/animal_logs/jax_working/animal_GPS_data_step1_2_3.csv")

#turn into spatial data
step1_2_3_sf <-   st_as_sf(step1_2_3,
                       coords = c("X", "Y"),
                       crs = 28354,
                       agr = "constant")





names(step1_2_3_sf)

check <-
  step1_2_3_sf %>% dplyr::select( "deviceName"  ,
                           "Audio_values"    ,
                           "Shock_values" , 
                           "local_time"          ,  
                           "Sheep_ID"  ,
                           training_period)





## TASK Mean audio/pulse ratio AIM: Across all animals
names(step1_2_3_sf)
# step 1 summaries audio and pulse per animal per day also training period 
summary_audio_ratio <- step1_2_3_sf %>% 
  dplyr::group_by(Sheep_ID, date, training_period) %>% 
  dplyr::summarise(audio_sum = sum(Audio_values, na.rm = TRUE),
            pulse_sum = sum(Shock_values, na.rm = TRUE),
            ratio_sum1 = audio_sum/ (pulse_sum+audio_sum )*100,
            ratio_sum2 = pulse_sum/ (audio_sum )*100)
  
summary_audio_ratio <- ungroup(summary_audio_ratio)

summary_audio_ratio$ratio_sum1 [is.nan(summary_audio_ratio$ratio_sum1 )]<-NA
summary_audio_ratio$ratio_sum2 [is.nan(summary_audio_ratio$ratio_sum2 )]<-NA

names(summary_audio_ratio)
summary_audio_ratio %>%
  filter(training_period == "non_training") %>% 
  ggplot(aes(x = date , y = ratio_sum1)) +
  geom_col()+
  theme_classic() +
  facet_wrap(.~Sheep_ID)+
  theme(axis.text.x = element_text(angle = 90))+
  #geom_vline(xintercept = as.Date(vertical_lines), col = "blue")+
  labs(
    x = "Date",
    y = "ratio (Audio / pulse+audio)*100 ",
    title = "Animals fitted with VF collars in non training period",
    subtitle = "audio and pulse counts summed per day and animal and then ratio calulated")
  
summary_audio_ratio

# step 2 summaries audio and pulse per animal per day also training period 

summary_audio_ratio_from_training <- summary_audio_ratio %>% 
  dplyr::group_by(training_period) %>% 
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
  

summary_audio_ratio_from_training$training_period <- factor(summary_audio_ratio_from_training$training_period,
                                                            levels = c("training", "non_training"))    

names(summary_audio_ratio_from_training)


summary_audio_ratio_from_training <- as.data.frame(summary_audio_ratio_from_training)

summary_audio_ratio_from_training <- summary_audio_ratio_from_training %>%dplyr::select(training_period ,
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


DT::datatable(summary_audio_ratio_from_training ,
              rownames = FALSE,  
              options = list(columnDefs =
                               list(list(className = 'dt-center',
                                         targets = "_all")))) %>%
  formatRound(c(2:13), 2) #this round clm number  to 2 decimal places
  






## GPS Plots

#AIM: Daily plots are probably sufficient. 

#But more frequent plots during the training period.
#And there was that instance where they broke through the fence overnight (we think something spooked them), 
#so a more detailed breakdown of that could also be interesting too. 
names(step1_2_3_sf)
step1_2_3_sf_trial_only <- step1_2_3_sf %>% 
  filter(training_period == "non_training")

#### ------------------ PLOTS FOR DANA AND PAPER 10/11/2022-----------------------------#####

plot1 <- ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "black",linetype = "dotted", size = 0.5, fill = NA) +
  
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = water_pt ,color ="Blue") +
  geom_sf(data = step1_2_3_sf_trial_only ,alpha = 0.05) +
  facet_wrap(.~ date)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())#+
  # labs(title = "Animal logs in during trial",
  #      subtitle = "log when animals were yarded removed, and clipped to 10 meter buffer")
plot1


ggsave(plot1,
       device = "png",
       filename = paste0("Lameroo_sheep_all_days_noTitle_Buffer_bound.png"),
       path= "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)


### DAY 1 only 


step1_2_3_sf_day1 <- step1_2_3_sf %>% filter(date=="2022-10-17")

step1_2_3_sf_day1 <- step1_2_3_sf_day1 %>% 
  mutate(hour = hour(local_time),
         minute = minute(local_time))

step1_2_3_sf_day1$training_period <- factor(step1_2_3_sf_day1$training_period,
                                                            levels = c("training", "non training"))    

step1_2_3_sf_day1_training <-step1_2_3_sf_day1 %>%  filter(training_period == "training")
step1_2_3_sf_day1_Non_training <-step1_2_3_sf_day1 %>%  filter(training_period == "non_training")

day1 <- ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "black",linetype = "dotted", size = 0.5, fill = NA) +
  
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = step1_2_3_sf_day1_training ,alpha = 0.08, color = "red") +
  geom_sf(data = step1_2_3_sf_day1_Non_training ,alpha = 0.08, color = "black") +
  
    theme_bw()+
  facet_wrap(. ~ hour)+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())#+
  #labs(title = "Animal logs on first day")
day1


ggsave(day1,
       device = "png",
       filename = paste0("Lameroo_sheep_day1_diff_colour_buff_bound_no_title.png"),
       path= "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)



step1_2_3_sf_day4 <- step1_2_3_sf %>% filter(date=="2022-10-20")

step1_2_3_sf_day4 <- step1_2_3_sf_day4 %>% 
  mutate(hour = hour(local_time),
         minute = minute(local_time))

step1_2_3_sf_day4$training_period <- factor(step1_2_3_sf_day4$training_period,
                                            levels = c("training", "non_training"))    


day4 <-ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "black",linetype = "dotted", size = 0.5, fill = NA) +
  
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = step1_2_3_sf_day4 ,alpha = 0.08) +
  theme_bw()+
  facet_wrap(.~ hour,  nrow = 3)+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())#+
  #labs(title = "Animal logs on 4th day")
day4

ggsave(day4,
       device = "png",
       filename = paste0("Lameroo_sheep_day4_no_title_buff_bound.png"),
       path= "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)






