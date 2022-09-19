# grand mesa plot for npp prop
# sept 19th, 2022

library(terra)
library(ggplot2):theme_set(theme_void(12))

setwd("/Users/jacktarricone/g_mesa/data/")
dir_list <-list.files(pattern = '_grd')
unw_list <-list.files(dir_list, pattern = 'unw.grd.tiff', full.names = TRUE)
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
unw_df <-as.data.frame(unw_stack, xy = TRUE)
head(unw_df)

# test plot
ggplot(unw_df) +
  geom_tile(aes(x,y, fill = 'grmesa_27416_21019-017_21021-005_0006d_s01_L090VV_01.unw.grd')) +
  labs(x="Latitude (deg)",
       y="Longitude (deg)",
       title = "phase")




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