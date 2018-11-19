library(raster)
library(rgdal)

setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany")

dgm <- raster("germany_dem_srtm_laea30.tif")
samples <- read.csv(header=TRUE, sep=",", file="spectral/data_clear.csv")
coords <- readOGR(dsn="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany",layer="samples")
stations <- readOGR(dsn ="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd", layer="stat_dwd_5km_300px")


# extract elevation for samples 
elevation <- as.data.frame(extract(dgm, coords,sp=TRUE))
elevation$coords.x1 = NULL
elevation$coords.x2 = NULL
colnames(elevation) <- c("ID", "ID_s", "X", "Y", "elevation_sample")
elevation$ID_s <- sprintf("%02d", as.numeric(elevation$ID_s))         # add zero 

elevation$plotid <- paste(elevation$ID, elevation$ID_s, sep="")       # add plot id column

######################################################################
results_v4 <- merge(results_v4, elevation[, c("plotid","elevation_sample", "X", "Y")], by = "plotid", all.x=TRUE)
######################################################################

samples <- merge(samples, elevation[, c("plotid","elevation_sample", "X", "Y")], by= "plotid", all.x=TRUE)
names(stations)[names(stations) == 'Stations_i'] <- 'dwd_stat'

# elevation difference to station 
samples <- merge(samples, stations[, c("DEM", "dwd_stat")], by ="dwd_stat", all.x=TRUE)
samples$elevation_diff <- samples$elevation_sample - samples$DEM
write.csv(samples, file = "spectral/data_clear.csv", row.names = FALSE)

# mean difference/ dwd station 
elevation_mean_diff <- samples %>% group_by(dwd_stat) %>% summarise(elevation_diff_mean = mean(elevation_diff))
stations_df <- as.data.frame(merge(stations, elevation_mean_diff[, c("elevation_diff_mean", "dwd_stat")], by ="dwd_stat", all.x=TRUE))
stations_df$coords.x1 = NULL
stations_df$coords.x2 = NULL
write.csv(stations_df, file = "stations.csv", row.names = FALSE)

