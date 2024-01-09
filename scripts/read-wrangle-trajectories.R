#Read trajectory data
#December 19, 2023
#seems to work best reading in .kml files rather than the gpx
library(here)
library(tidyverse)
library(sf)
library(mapview)
#install.packages("gpx")
library(gpx)
library(lubridate)
#read kml files------
#Try to simply read in as sf 
setwd(here("data-input","trajectories","kml"))

#Try it twice
rp6=st_read("20230616-101717 - RP6.kml") %>% 
  st_zm(drop=TRUE,what="ZM") %>% 
  #The track has two observations, one which appears
  #to be the starting point and a second which is the track.
  #Remove the first observation so that I can convert it into
  #a simpler linestring
  #filter to the second observation which contains
  #the track
  slice(2) %>% 
  st_cast("LINESTRING")

rp6 %>% mapview()
rp7=st_read("20230616-104508 - RP7.kml")%>% 
  st_zm(drop=TRUE,what="ZM") %>% 
  #The track has two observations, one which appears
  #to be the starting point and a second which is the track.
  #Remove the first observation so that I can convert it into
  #a simpler linestring
  #filter to the second observation which contains
  #the track
  slice(2) %>% 
  st_cast("LINESTRING")
rp7
plot(rp7)
rp7 %>% mapview()


#Now repeat over all files in the folder
#So that I can repeat this over all files in this folder
file_names_kml=list.files(path=here("data-input",
                           "trajectories","kml"))

read_kml_cast_to_linestring=function(num){
  out=st_read(file_names_kml[num]) %>% 
    st_zm(drop=TRUE,what="ZM") %>% 
    slice(2) %>% 
    st_cast("LINESTRING") %>% 
    mutate(study_id=num)#some id
  
  return(out)
}

#Test function
hi=read_kml_cast_to_linestring(4)
hi %>% mapview()


#Okay, now repeat the function over all files
#I'm starting with 2 because one doesn't seem to be a participant
all_kmls=2:length(file_names_kml) %>% 
  map_dfr(read_kml_cast_to_linestring)

all_kmls %>% mapview(zcol="study_id")

#buffer around the kmls
all_kmls_buff=all_kmls %>% 
  st_buffer(100) %>% 
  dplyr::select(study_id)

#save for rmarkdown
setwd(here("data-processed"))
save(all_kmls_buff,file="all_kmls_buff.RData")
all_kmls_buff %>% mapview(zcol="study_id")
    
all_kmls_buff_centroid=all_kmls_buff %>% 
  st_centroid()

save(all_kmls_buff_centroid,file="all_kmls_buff_centroid.RData")
all_kmls_buff_centroid %>% mapview()


# read gpx files------
#that does work, but it's not super easy to extract specific information.
#what about as GPX files?


setwd(here("data-input",
           "trajectories","gpx"
))

## Test one----
gpx_test_tracks=read_gpx("20230616-101717 - RP6.gpx") %>% 
#  as_tibble() %>% 
  pluck("tracks") %>% #just grab the tracks part of the list
  as_tibble() %>% 
  #interesting. this works. Have never used "unpack" before
  #https://stackoverflow.com/questions/62328384/unnest-longer-gives-dollar-sign-instead-of-normal-tibble
  unpack(cols=everything()) %>% 
  st_as_sf(coords=c("Longitude","Latitude"),crs=4326) %>% 
  rename(date_time=Time) %>% #better name
  mutate(
    #time to the next point
    t_elapsed_to_next = lead(date_time)-date_time,
    t_elapsed_to_next_units = units(t_elapsed_to_next),#seconds here
    t_elapsed_to_next_s = as.numeric(t_elapsed_to_next)#number of seconds to next point
  )

nrow(gpx_test_tracks)
gpx_test_tracks %>% mapview()

#Total duration of the GPS track.
gpx_test_tracks %>% 
  st_set_geometry(NULL) %>% 
  mutate(dummy=1) %>% 
  summarise(
    date_time_min=min(date_time),
    date_time_max=max(date_time)
    )

#Bring them all in
file_names_gpx=list.files(path=here("data-input",
                                    "trajectories","gpx"))
file_names_gpx

read_gpx_cast_to_sf=function(num){
  out=read_gpx(file_names_gpx[num]) %>% 
    #  as_tibble() %>% 
    pluck("tracks") %>% #just grab the tracks part of the list
    as_tibble() %>% 
    #interesting. this works. Have never used "unpack" before
    #https://stackoverflow.com/questions/62328384
    unpack(cols=everything()) %>% 
    st_as_sf(coords=c("Longitude","Latitude"),crs=4326) %>%  
    rename(date_time=Time) %>% #better name
    mutate(
      study_id=num,#add an identifier for the research participant
      line_id=row_number(),#add an identifier for the line segment within study_id
      #time to the next point
      t_elapsed_to_next = lead(date_time)-date_time,
      t_elapsed_to_next_units = units(t_elapsed_to_next),#seconds here
      t_elapsed_to_next_s = as.numeric(t_elapsed_to_next)#number of seconds to next point
        )
  
  return(out)
}

setwd(here("data-input","trajectories","gpx"))
hi=read_gpx_cast_to_sf(4)
hi %>% mapview()


#Load and wrangle them all
all_gpx=2:length(file_names_gpx) %>% 
  map_dfr(read_gpx_cast_to_sf) %>% 
  #drop Segment Id and elevation and extensions
  dplyr::select(-contains("Segment"),-contains("Elevation"),-contains("extens"))

all_gpx %>% mapview(zcol="study_id")
mv_all_gpx=all_gpx %>% mapview(zcol="study_id")

#save for rmarkdown
setwd(here("data-processed"))
save(all_gpx,file="all_gpx.RData")

#What's the distribution of dates in these?
summary(all_gpx$date_time)

#create a lookup for time elapsed to next
lookup_duration_study_id_line_id= all_gpx %>%
  st_set_geometry(NULL) %>% 
  distinct(study_id, line_id,t_elapsed_to_next_s)

lookup_duration_study_id_line_id

# Create buffer around GPX-based trajectories------
st_crs(all_gpx)
#Doesn't take too long. Just do it around all of them.
all_gpx_buff_2=all_gpx %>% 
  filter(study_id==2) %>% 
  st_buffer(200)

all_gpx_buff_2 %>% mapview()
#Doesn't take too long. Do it this simple way without a function:
all_gpx_buff=all_gpx %>% 
  st_buffer(200)

all_gpx_buff

# Create a circular buffer to crop the terra images to be smaller-----
mv_all_gpx
buff_circular_for_crop=all_gpx %>% 
  slice(1) %>% 
  st_buffer(10000)

mv_buff_circular_for_crop =buff_circular_for_crop%>% 
  mapview()
mv_buff_circular_for_crop+mv_all_gpx

# # Create a unary union of the buffer for a vis
# all_gpx_union=all_gpx %>% 
#   group_by(study_id) %>% 
#   summarise(n=n())
# 
# all_gpx_union %>% mapview(zcol="study_id")
# 
# all_gpx_buff_union=all_gpx_buff %>% 
#   group_by(study_id) %>% 
#   summarise(n=n())
# 
# all_gpx_buff_union %>% 
#   mapview()
