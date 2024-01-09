#Writing this to parallel
#microclim-static-v-dynam/microclim-static-v-dynam/scripts/demo/extract-summarize-exposure.R
#install.packages("Hmisc")#for weighted variance
library(Hmisc)
source(here("scripts","read-wrangle-trajectories.R"))
source(here("scripts","read-lst.R"))
#Begin with just using the one LST date

#Extract the LST data within the GPS trajectory.

#Test one one study ID
all_gpx_buff_2= all_gpx_buff %>% 
  filter(study_id==2)

all_gpx_buff_2
study_id_2=all_gpx_buff_2 %>% 
  st_set_geometry(NULL) %>% 
  distinct(study_id) %>% 
  pull()
study_id_2

all_gpx_buff
#check to ensure that these overlap
mv_lst_20230602=lst_20230602 %>% mapview()
mv_gpx_buff_2=all_gpx_buff_2 %>% mapview()
mv_gpx_buff_2+mv_lst_20230602

# Test on one-----
#use the cropped version instead
nrow(lst_20230602_crop)*ncol(lst_20230602_crop)
traj_extract_e_test=lst_20230602_crop %>% 
  terra::extract(
    all_gpx_buff_2, 
    na.rm=TRUE, 
    weights = TRUE 
  ) %>% 
  as_tibble() %>% #uses tidyterra to convert to tibble
  rename(
    line_id = ID,#rename this to line id for that person
    e=2#second column is the exposure.
  ) 

#note the IDs are the level of the raster image, not the GPX file
nrow(traj_extract_e_test)
nrow(all_gpx_buff_2)
n_distinct(traj_extract_e_test$line_id)
traj_extract_e_test %>% 
  print(n=1000)

traj_extract_e_test

#simpler than before. Use the weighted.mean function instead of
#calculating the weighted average yourself
traj_extract_by_line_id_test=traj_extract_e_test %>% 
  group_by(line_id) %>%
  summarise(
    e=weighted.mean(
      x=e,
      w=weight,
      na.rm=T),
    #These weights are based on the areal overlap, not time
    sum_of_weights_area=sum(weight,na.rm=T),
    n_pixel = n() # number of observations corresponds to number of pixels per line segment
  ) %>% 
  ungroup() %>% 
  mutate(study_id=study_id_2) %>% #add this so it can be linked
  #now link in the time weight
  left_join(lookup_duration_study_id_line_id,by=c("study_id","line_id")) %>% 
  mutate(area_time_weight=sum_of_weights_area*t_elapsed_to_next_s)

#yes, this is much simpler.
#Now link in the time component and weight by time
traj_extract_by_line_id_test
summary(traj_extract_by_line_id_test$area_time_weight)

#Summarize by study_id
#Write a function as I use this a lot
summarise_traj_by_id=function(df){
  df %>% 
    summarise(
      e_m=weighted.mean(
        x=e,
        w=area_time_weight,
        na.rm=T),
      #generate a weighted standard deviation using Hmisc package
      #https://stackoverflow.com/questions/10049402
      e_var=Hmisc::wtd.var(x=e,weights=area_time_weight,normwt = TRUE),
      e_min=min(e,na.rm=T),
      e_max=max(e,na.rm=T),
      e_med=median(e,na.rm=T),
      n_line = n(), #keep track of how many line segments
    ) %>% 
      ungroup() %>% 
      mutate(e_sd=sqrt(e_var)) %>% 
    dplyr::select(study_id, 
                  starts_with("e_m"),
                  starts_with("e_var"),
                  starts_with("e_sd"),
                  starts_with("e_min"),
                  starts_with("e_max"),
                  starts_with("e_med"),
                  starts_with("n"),
                  everything()
    )
}

traj_summary_by_id_test=traj_extract_by_line_id_test %>% 
  filter(is.na(area_time_weight)==F) %>% 
  group_by(study_id) %>% 
  summarise_traj_by_id()


traj_summary_by_id_test


# Extract from all of them------
##Write a function
lookup_duration_study_id_line_id
summary(lst_20230602_crop)
## Write function to extract from all study ids----
traj_extract_e=function(study_id_val){
  #filter the trajectory buffer to the corresponding study id
  traj_buff_id = all_gpx_buff %>% #excluding high speed
    filter(study_id==study_id_val)
  
  #this is the LST image that has the best quality
  #doesn't align in time necessarily with each GPS
  traj_extract_e_obj=lst_20230602_crop %>% 
    terra::extract(
      traj_buff_id, #object just created above
      na.rm=TRUE, 
      weights = TRUE 
    ) %>% 
    as_tibble() %>%  
    rename(
      line_id = ID,#rename this to line id for that person
      e=2#second column is the exposure.
    ) 
  
  traj_extract_e_obj_by_line_id=traj_extract_e_obj %>% 
    group_by(line_id) %>%
    summarise(
      #Jan 9, 2024 use R's built-in weighted.mean() function
      #instead of calculating weighted average manually
      e=weighted.mean(
        x=e,
        w=weight,
        na.rm=T),
      #These weights are based on the areal overlap, not time
      sum_of_weights_area=sum(weight,na.rm=T),
      n_pixel = n() # number of observations corresponds to number of pixels per line segment
    ) %>% 
    ungroup() %>% 
    mutate(study_id=study_id_val) %>% #add this so it can be linked
    #now link in the time weight
    left_join(lookup_duration_study_id_line_id,by=c("study_id","line_id")) %>% 
    mutate(
      area_time_weight=sum_of_weights_area*t_elapsed_to_next_s,
      e_name = "lst", #this could be dynamic in the function. the name of the exposure
      #converting land-surface temperature from kelvin to celsius
      e = case_when(
        e_name== "lst" ~ e-273.15,
        TRUE ~ e
      ))
  
}
#test on one
traj_extract_wrangle_test_fun=traj_extract_e(2)

#summarize
traj_extract_wrangle_test_fun %>% 
  filter(is.na(area_time_weight)==F) %>% 
  group_by(study_id) %>% 
  summarise_traj_by_id()


# Run function on all of them----
study_id_list = all_gpx_buff %>% 
  st_set_geometry(NULL) %>% 
  group_by(study_id) %>% 
  summarise(n=n()) %>% 
  pull(study_id)

study_id_list
traj_extract_df  = study_id_list %>% 
  map_dfr(traj_extract_e)

#summarize information by research participant
traj_summary_by_id=traj_extract_df %>% 
  filter(is.na(area_time_weight)==F) %>% 
  group_by(study_id) %>% 
  summarise_traj_by_id()

traj_summary_by_id
#save this for Rmarkdown
setwd(here("data-processed"))
save(traj_summary_by_id,file="traj_summary_by_id.RData")

#between participant sd of means
traj_summary_by_id %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(
    e_m_of_m=mean(e_m,na.rm=T),
    e_sd_of_m=sd(e_m, na.rm=T),
    e_m_of_sd=mean(e_sd,na.rm=T))
