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

# stack
unw_stack <-rast(crop_list)
hp_ext <-ext(-108.25, -108.17, 39.04, 39.08)
hp_unw <-crop(unw_stack, hp_ext)
plot(hp_unw)

# convert to df for ggplot
unw_df <-as.data.frame(hp_unw, xy = TRUE)

# change col names
colnames(unw_df)[3:8] <-c("1/27-2/03",
                          "2/03-2/10",
                          "2/10-3/03",
                          "3/03-3/10",
                          "3/10-3/16",
                          "")
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
normalit<-function(m){
  if(mean(m) > 0)
  (m - mean(m))
  else(
    m + abs(mean(m))
  )
}

# test
scaled <-normalit(unw_df$p6)
hist(scaled)

# mutate
unw_norm <-unw_df %>%
            mutate(p1_norm = normalit(p1), 
                   p2_norm = normalit(p2),
                   p3_norm = normalit(p3),
                   p4_norm = normalit(p4),
                   p5_norm = normalit(p5),
                   p6_norm = normalit(p6))

# add lat long back on
unw_norm <-cbind(unw_df$x, unw_df$y, unw_norm[,9:14])
colnames(unw_norm)[1:2] <-c('x','y')
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
  scale_fill_gradient('UNW (rad)', limits = c(-2,2)) 









feb1_depth <-raster("/Volumes/JT/projects/uavsar/lidar/ASO_GrandMesa_mosaic_2020Feb1-2_AllData_and_Reports/ASO_GrandMesa_2020Feb1-2_snowdepth_3m.tif")
feb13_depth <-raster("/Volumes/JT/projects/uavsar/lidar/ASO_GrandMesa_mosaic_2020Feb13_AllData_and_Reports/ASO_GrandMesa_2020Feb13_snowdepth_3m.tif")
feb1_depth
feb13_depth
depth_change <-feb13_depth - feb1_depth
plot(depth_change)
writeRaster(depth_change, "/Volumes/JT/projects/uavsar/lidar/gmesa_depth_change_02-01_02-13.tif")

unw <-raster("/Volumes/JT/projects/uavsar/gmesa/data/2021/grmesa_27416_20003-028_20005-007_0011d_s01_L090HH_01.unw.grd.tiff")

# load in 2021 .unw data
rast_list <-list.files("/Volumes/JT/projects/uavsar/gmesa/data/2021/", pattern =".tiff", full.names = TRUE)
unw_list <-lapply(rast_list, raster)

unw3 <-unw_list[[3]]
plot(unw3)

# crop so can stack rasters
crop_exnt <-extent(-108.3, -107.93, 38.97, 39.12)
crop_list <-lapply(unw_list, function(x) crop(x, crop_exnt))
plot(crop_list[[6]]) # test plot
crop_list

# crop to the corner where HPs box is but slightly larger
hp_exnt <-extent(-108.25, -108.17, 39.04, 39.08)
hp_list <-lapply(unw_list, function(x) crop(x, hp_exnt))
hp_stack <-stack(hp_list)
values(hp_stack)[values(hp_stack) == 0] = NA
plot(hp_stack)

# stack
unw_stack <-stack(crop_list)
values(unw_stack)[values(unw_stack) == 0] = NA
#writeRaster(unw_stack, "/Volumes/JT/projects/uavsar/gmesa/data/2021/unw_stack.tiff")
plot(unw_stack)

plot(unw)
values(unw)[values(unw) == 0] = NA

png("gmesa_2021.png", height = 1024 * 0.707, width = 1024)
plot(hp_stack,
     xlab = "longitude (deg)",
     ylab = "latitude (deg)",
     #main = "grand mesa 2/1-2/13 unwrapped phase change",
     col = hcl.colors(255, palette = "RdBu"))

setwd("/Volumes/JT/projects/uavsar/gmesa/")
png("gmesa_2021.png", height = 924 * 0.707, width = 924, units = "mm" , res = 500, pointsize = 60)
plot(hp_stack,
     xlab = "longitude (deg)",
     ylab = "latitude (deg)",
     #main = "grand mesa 2/1-2/13 unwrapped phase change",
     col = hcl.colors(255, palette = "RdBu"))
dev.off()

?png
colr <- colorRampPalette(brewer.pal(11, 'RdBu'))
levelplot(hp_stack,
          maxpixels = 1e5,
          margin=FALSE,                       # suppress marginal graphics
          colorkey=list(
            space='bottom',                   # plot legend at bottom
            labels=list(at=-7:14, font=4)      # legend ticks and labels 
          ),    
          par.settings=list(
            axis.line=list(col='transparent') # suppress axes and legend outline
          ),
          scales=list(draw=FALSE),            # suppress axis labels
          col.regions=colr,                   # colour ramp
          at=seq(-7, 14, len=101))             # colour ramp breaks

setwd("/Volumes/JT/projects/uavsar/gmesa/")
png("plt4.png", height = 1024 * 0.707, width = 1024)  #0.707 is a convenient aspect.ratio
plt4
dev.off()