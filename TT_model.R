
library(lubridate)
library(ggplot2)
library(zoo)
library(dplyr)
library(tidyr)

setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk")
tmk <- read.csv(file="TMK_MN004.txt", header=TRUE, sep=";")

tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))

tmk <- tmk %>%
  group_by(STATION_ID)%>%
  mutate(datum = as.Date(datum)) %>%
  complete(datum = seq.Date(min(datum), max(datum), by="day"))

tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)

tmk <- subset(tmk, tmk$year == 2017 & tmk$doy <= 196)

# rolling mean 
tmk$gap_fill <- rollapply(
  data    = tmk$WERT,
  width   = 4,
  FUN     = function(x) mean(x, na.rm=TRUE), 
  fill    = NA,
  align   = "center",
  partial=TRUE
)

# fills gaps 
tmk <- tmk %>%
  mutate(WERT = coalesce(WERT, gap_fill))

# remove plots with data gaps and shorter time series 
tmk <- tmk %>%
  group_by(STATION_ID) %>%
  filter(!any(is.nan(WERT))) %>%
  filter(length(STATION_ID) == 196)


#################################################################################
tt_model <- function(statid,
                     t_day,
                     year,
                     doy,
                     t_base = 5,
                     t_crit = 250) {
  data = data.frame(statid,
                    t_day,
                    year,
                    doy)
  
  SOS_TT <- NULL
  k <- 0
  
  for (i in unique(data$statid)) {
    d = subset(data, data$statid == i & data$year == 2017)
    
    forcing <- 0
    k <- k + 1
    
    for (x in d$doy) {
      out <- NULL
      dat <- subset(d, d$doy == x)
      doy_sos <- dat$doy
      t <- dat$t_day
      
      if (t > t_base) {
        val = t - t_base
        forcing = forcing + val
      }
      
      if (forcing > t_crit) {
        out <- data.frame("stat_id" = i,
                          "TT" = doy_sos)
        SOS_TT <- rbind(SOS_TT, out)
        
        break
      }
    }
    if (is.null(out)) {
      print(paste(i))
    }
  }
  return(SOS_TT)
}

# SOS_TT <- tt_model(
#   statid = tmk$STATION_ID,
#   t_day = tmk$WERT,
#   year = tmk$year,
#   doy = tmk$doy
# )
# 
# ############################################################################
# 
# # load and merge SOS estimates from RS
# mean_evi <- read.csv(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/20190201_results_stat_evi.csv")
# mean_ndvi <- read.csv(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/20190201_results_stat_ndvi.csv")
# 
# SOS_TT <- merge(mean_evi[, c("sp", "b4","stat_id", "X","Y")], SOS_TT, by="stat_id", all.x=TRUE)
# colnames(SOS_TT) <- c("stat_id", "GAM_EVI","LOG_EVI","X","Y","TT" )
# 
# SOS_TT <- merge(mean_ndvi[, c("sp", "b4","stat_id")], SOS_TT, by="stat_id", all.y=TRUE)
# colnames(SOS_TT)[c(2:3)] <- c("GAM_NDVI","LOG_NDVI")
# 

