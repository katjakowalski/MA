
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk/metadata/station")
stations <- read.csv(file="station.txt", header=TRUE, sep=";", fileEncoding="UTF-8")

dwd_stations <- merge(mean_evi[c("stat_id")], stations, by.x="stat_id", by.y="Stations_id", all.x=TRUE)

write.csv(dwd_stations, file="20190126_stations_dwd.csv", row.names=FALSE)

refl_data <- readOGR("dwd_reflectance_samples.shp")


sample_check <- merge(data, refl_data, by.x="plotid",by.y="POINT_ID", all.y=TRUE)
sample_missing <- subset(sample_check, is.na(tile))

write.csv(sample_missing, file="20190126_samples_missing.csv", row.names=FALSE)
