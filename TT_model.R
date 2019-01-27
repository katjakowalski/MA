
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

tmk$gap_fill <- rollapply(
  data    = tmk$WERT,
  width   = 4,
  FUN     = function(x) mean(x, na.rm=TRUE), 
  fill    = NA,
  align   = "center",
  partial=TRUE
)

tmk <- tmk %>%
  mutate(WERT = coalesce(WERT, gap_fill))

tmk <- tmk %>%
  group_by(STATION_ID) %>%
  filter(!any(is.nan(WERT)))



tt_model <- function(statid, 
                     t_day, 
                     year,
                     doy,
                     t_base=5,
                     t_crit = 250) {
  
  data = data.frame(statid, 
                    t_day,
                    year, 
                    doy)
  
  SOS_TT <- NULL
  k <- 0
  
  for( i in unique(data$statid)){

    d = subset(data, data$statid == i & data$year == 2017)

      forcing <- 0
      k <- k +1
      
      for(x in d$doy){
        out <- NULL
        dat <- subset(d, d$doy == x)
        doy_sos <- dat$doy
        t <- dat$t_day
        
        #print(paste(doy_sos))
        if (t > t_base){
          val = t-t_base
          forcing = forcing + val 
        }
        
        if (forcing > t_crit){
          #print(paste(i, doy_sos))
          out <- data.frame("stat_id" = i, 
                            "TT" = doy_sos)
          SOS_TT <- rbind(SOS_TT, out)
          
          break 
        }
      }
      if (is.null(out)){
        print(paste(i))
        #out2 <- data.frame(i, 0)
        #SOS_TT <- rbind(SOS_TT, out2)
      }
    #}
    #else{print(paste("obs", i))}
  }
    return(SOS_TT)
}



SOS_TT <- tt_model(statid=tmk$STATION_ID, 
                   t_day=tmk$WERT,
                   year = tmk$year,
                   doy= tmk$doy)

quantile(SOS_TT$TT,na.rm=TRUE, c(0.05,0.5,0.95))

############################################################################
# load and merge SOS estimates from RS 

#setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results")
# results_evi <- read.csv(file="20190117_results_stat_evi", header=TRUE, sep=",")
# results_ndvi <- read.csv(file="20190117_results_stat_ndvi", header=TRUE, sep=",")

SOS_TT <- merge(mean_evi[, c("sp", "b4","stat_id", "X","Y")], SOS_TT, by="stat_id", all.x=TRUE)
colnames(SOS_TT) <- c("stat_id", "GAM_EVI","LOG_EVI","X","Y","TT" )

SOS_TT <- merge(mean_ndvi[, c("sp", "b4","stat_id")], SOS_TT, by="stat_id", all.y=TRUE)
colnames(SOS_TT)[c(2:3)] <- c("GAM_NDVI","LOG_NDVI")

# correlation 
cor.test(SOS_TT$GAM_NDVI, SOS_TT$TT, use="complete.obs")
cor.test(SOS_TT$LOG_NDVI, SOS_TT$TT, use="complete.obs")

cor.test(SOS_TT$GAM_EVI, SOS_TT$TT, use="complete.obs")
cor.test(SOS_TT$LOG_EVI, SOS_TT$TT, use="complete.obs")

# difference (residuals)
SOS_TT$diff_GAM_EVI_TT <- SOS_TT$TT - SOS_TT$GAM_EVI
SOS_TT$diff_LOG_EVI_TT <- SOS_TT$TT - SOS_TT$LOG_EVI

SOS_TT$diff_GAM_NDVI_TT <- SOS_TT$TT - SOS_TT$GAM_NDVI 
SOS_TT$diff_LOG_NDVI_TT <- SOS_TT$TT - SOS_TT$LOG_NDVI 

# mean difference 
mean(SOS_TT$diff_GAM_EVI_TT, na.rm = TRUE)
mean(SOS_TT$diff_LOG_EVI_TT, na.rm = TRUE)

mean(SOS_TT$diff_GAM_NDVI_TT, na.rm = TRUE)
mean(SOS_TT$diff_LOG_NDVI_TT, na.rm = TRUE)

# plot difference histograms

# plot_diff <- melt(SOS_TT[, c("diff_GAM_NDVI_TT", "diff_LOG_NDVI_TT")])
# 
# png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190122_Differences_TT_NDVI.png",
#     width= 1200, height=1000, res=200 )
# ggplot(data=plot_diff)+
#   geom_histogram(aes(x=value, fill=variable),
#                  binwidth=1,
#                  alpha=1/2,
#                  position="identity")+
#   scale_fill_manual(values=c("black","blue"))+
#   labs(x="GDD", title="Differences SOS TT Model vs. NDVI")
# dev.off()




# correlation difference & east-west
cor.test(SOS_TT$diff_GAM_EVI_TT, SOS_TT$X, use="complete.obs")
cor.test(SOS_TT$diff_LOG_EVI_TT, SOS_TT$X, use="complete.obs")
cor.test(SOS_TT$diff_LOG_NDVI_TT, SOS_TT$X, use="complete.obs")
cor.test(SOS_TT$diff_GAM_NDVI_TT, SOS_TT$X, use="complete.obs")


ggplot(SOS_TT)+
  geom_point(aes(x=diff_GAM_EVI_TT, y=X))

# correlation with elevation (DEM), 
SOS_TT <- merge(SOS_TT, dwd_stations[, c("DEM","proxartifi", "prox_undis", "LC", "Stations_i")], by.x="stat_id", by.y="Stations_i", all.x=TRUE)

# difference SOS - TT
cor.test(SOS_TT$diff_GAM_EVI_TT, SOS_TT$DEM, use="complete.obs")
cor.test(SOS_TT$diff_LOG_EVI_TT, SOS_TT$DEM, use="complete.obs")
cor.test(SOS_TT$diff_GAM_NDVI_TT, SOS_TT$DEM, use="complete.obs")
cor.test(SOS_TT$diff_LOG_NDVI_TT, SOS_TT$DEM, use="complete.obs")

# SOS 
cor.test(SOS_TT$GAM_EVI, SOS_TT$DEM, use="complete.obs")
cor.test(SOS_TT$GAM_NDVI, SOS_TT$DEM, use="complete.obs")
cor.test(SOS_TT$LOG_NDVI, SOS_TT$DEM, use="complete.obs")
cor.test(SOS_TT$LOG_EVI, SOS_TT$DEM, use="complete.obs")


#correlation difference distance to urabn areas (> 2000)
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/river_cities")
cities <- read.csv(file="20190122_stat_dwd_dist_cities.csv", header=TRUE, sep=",")
SOS_TT <- merge(SOS_TT, cities[, c("Distance", "Stations_i")], by.x="stat_id", by.y="Stations_i")

pheno_urban <- subset(SOS_TT, SOS_TT$Distance <= 5000)

cor.test(pheno_urban$GAM_EVI, pheno_urban$Distance, use="complete.obs")

ggplot(data=SOS_TT)+
  geom_point(aes(x=LOG_EVI, y=Distance))




