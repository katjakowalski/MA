

library(mgcv)
library(tidyverse)

setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/spectral")

# data <- read.csv(header=TRUE, sep=",", file= "dwd_reflectance_samples.csv") 
# data_x0059_y0056 <- read.csv(header=TRUE, sep=",", file="dwd_reflectance_samples_X0059_Y0056_20170917_LEVEL2_SEN2A.csv" )
# 
# data <- data[!(data$blue == 0 & data$green== 0 & data$red== 0 & data$nir==0 & data$swir1==0 & data$swir2 == 0 ),]
# data <- rbind(data, data_x0059_y0056)
# 
# write.csv(data, file = "data_20181024.csv", row.names = FALSE)

data <- read.csv(header=TRUE, sep=",", file="data_20181024.csv")

############################################# QAI extraction
no_data <- function(x) { bitwAnd(x, 1)}
data$qa_nodata <- no_data(data$QAI)

qa_water <- function(x){ bitwAnd(bitwShiftR(x, 5), 1)}
data$qa_water <- qa_water(data$QAI)

qa_aerosol1 <- function(x){ bitwAnd(bitwShiftR(x, 6), 1) } # 6  --> 01, 1 interpolated
qa_aerosol1 <- qa_aerosol1(data$QAI)

qa_aerosol2 <- function(x){ bitwAnd(bitwShiftR(x, 7), 1) } # 7  --> 10, 2 high
qa_aerosol2 <- qa_aerosol2(data$QAI)

data$qa_aerosol <- 0
data$qa_aerosol[qa_aerosol1 == TRUE & qa_aerosol2 == TRUE] <- 3
data$qa_aerosol[qa_aerosol1 == TRUE & qa_aerosol2 == FALSE] <- 2
data$qa_aerosol[qa_aerosol1 == FALSE & qa_aerosol2 == TRUE] <- 1

qa_subzero <-  function(x){ bitwAnd(bitwShiftR(x, 8), 1) }
data$qa_subzero <- qa_subzero(data$QAI)

qa_saturation <-  function(x){ bitwAnd(bitwShiftR(x, 9), 1) }
data$qa_saturation <- qa_saturation(data$QAI)


qa_zenith <-  function(x){ bitwAnd(bitwShiftR(x, 10), 1) }
data$qa_zenith <- qa_zenith(data$QAI)

qa_illumination11 <- function(x){ bitwAnd(bitwShiftR(x,11), 1) }
qa_illumination11 <- qa_illumination11(data$QAI)
qa_illumination12 <- function(x){ bitwAnd(bitwShiftR(x,12), 1) }
qa_illumination12 <- qa_illumination12(data$QAI)

data$qa_illumination <- 0
data$qa_illumination[qa_illumination11 == TRUE & qa_illumination12 == TRUE] <- 3
data$qa_illumination[qa_illumination11 == TRUE & qa_illumination12 == FALSE] <- 2
data$qa_illumination[qa_illumination11 == FALSE & qa_illumination12 == TRUE] <- 1

qa_slope <- function(x){ bitwAnd(bitwShiftR(x,13), 1) }
data$qa_slope <- qa_slope(data$QAI)

qa_vapor <- function(x){ bitwAnd(bitwShiftR(x,14), 1) }
data$qa_vapor <- qa_vapor(data$QAI)

data$dwd_stat <- as.integer(substr(data$plotid, nchar(data$plotid)- 7 +1, nchar(data$plotid)-2))


data <- data %>% arrange(blue, green, red, nir, swir1, swir2, tile)



# set all negative values 0
data[,c("red","green","blue","nir","swir1","swir2")][data[,c("red","green","blue","nir","swir1","swir2")] < 0] <- 0


#############################################################indices


data$ndvi <- (data$nir - data$red)/ (data$nir+ data$red)
data$evi <- 2.5 * (((data$nir/10000)-(data$red/10000)) / (((data$nir/10000) + 6 * (data$red/10000) - 7.5 * (data$blue/10000)) + 1))


############################################################quality subset

data$quality <- 0 
data$quality[data$qa_cloud != 0 | data$qa_snow != 0 | data$qa_shadow != 0 |  data$qa_nodata != 0] <- 1
data_clear <- data[data$quality == 0,]



write.csv(data_clear, file = "data_clear.csv", row.names = FALSE)


