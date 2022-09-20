# grand mesa plot for npp prop
# sept 19th, 2022

library(terra)
library(dplyr)
library(tidyr)
library(ggplot2):theme_set(theme_void(12))

setwd("/Users/jacktarricone/g_mesa/data/")
dir_list <-list.files(pattern = '_grd')
unw_list <-list.files(dir_list, pattern = 'unw.grd.tiff', full.names = TRUE)
unw_names <-list.files(dir_list, pattern = 'unw.grd.tiff', full.names = FALSE)
unw_list # list tiff files

# read in as rast list
unw_rast_list <-lapply(unw_list, rast)

# crop
crop_ext <-ext(-108.3, -107.93, 38.97, 39.12)
crop_list <-lapply(unw_rast_list, function(x) crop(x, crop_ext))
plot(crop_list[[6]]) # test plot
crop_list

# stack and crop down to 'HPs corner'
unw_stack <-rast(crop_list)
hp_ext <-ext(-108.25, -108.17, 39.04, 39.08)
hp_unw <-crop(unw_stack, hp_ext)
plot(hp_unw)

# convert to df for ggplot
unw_df <-as.data.frame(hp_unw, xy = TRUE)

# change col names
colnames(unw_df)[3:8] <-c("p1","p2","p3","p4","p5","p6")
head(unw_df)

# date info for renaming

# p1
# "startTime": "2021-01-27T19:47:02Z"
# "stopTime": "2021-02-03T23:50:19Z"

# p2
# "startTime": "2021-02-03T23:50:36Z"
# "stopTime": "2021-02-10T19:12:27Z"

# p3
# "startTime": "2021-02-10T19:12:13Z"
# "stopTime": "2021-03-03T22:03:01Z"

# p4
# "startTime": "2021-03-03T22:03:00Z"
# "stopTime": "2021-03-10T16:36:09Z"

# p5
# "startTime": "2021-03-10T16:36:24Z"
# "stopTime": "2021-03-16T16:39:43Z"

# p6
# "startTime": "2021-03-16T16:39:43Z"
# "stopTime": "2021-03-22T15:30:53Z"

# normalize so just phase variation
normalit <-function(m){
  if(mean(m) > 0)
  (m - mean(m, na.rm = TRUE))
  else(
    m + abs(mean(m, na.rm= TRUE))
  )
}

# test
scaled <-normalit(unw_df$'3/03-3/10 raw')
hist(scaled)

# mutate
unw_norm <-unw_df %>%
            mutate(p1_n = normalit(p1),
                   p2_n = normalit(p2),
                   p3_n = normalit(p3),
                   p4_n = normalit(p4),
                   p5_n = normalit(p5),
                   p6_n = normalit(p6))


# add lat long back on
unw_norm <-cbind(unw_df$x, unw_df$y, unw_norm[,9:14])
colnames(unw_norm)[1:2] <-c('x','y')
colnames(unw_norm)[3:8] <-c("1/27-2/03",
                          "2/03-2/10",
                          "2/10-3/03",
                          "3/03-3/10",
                          "3/10-3/16",
                          "3/16-3/22")
head(unw_norm)

# reformat for plotting
plotting_df <- pivot_longer(unw_norm, 3:8)
colnames(plotting_df)[4] <-'unw'

# test plot
ggplot(plotting_df) +
  geom_raster(aes(x,y, fill = as.numeric(unw))) +
  labs(x="Latitude (deg)",
       y="Longitude (deg)",
       title = "phase")+
  facet_wrap(~ name, ncol = 3) +
  theme_light() +
  coord_equal() +
  labs(fill = "unw", title = "Grand Mesa Unwrapped Phase 2021 UAVSAR Time Series") +
  scale_fill_gradientn('UNW (rad)', limits = c(-3,3),
                      colours = colorRampPalette(c("darkred", "white", "darkblue"))(20)) 


setwd("/Volumes/JT/projects/uavsar/gmesa/")
ggsave(file = "gmesa_unw_ts_2021.png",
       width = 10, 
       height = 5,
       dpi = 400)
