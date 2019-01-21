library(lubridate)
library(ggplot2)

###########################################################################################
# load DWD meteorological data 
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

tmk <- tmk %>%
  group_by(STATION_ID) %>%
  filter(length(STATION_ID) == 196)

colnames(tmk)[1] <- "stat_id"

# load SOS data 
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/")

LSP_evi <- read.csv(file="20181211_mean_evi.csv", header=TRUE)
LSP_ndvi <- read.csv(file="20181211_mean_ndvi.csv", header=TRUE)

LSP <- merge(LSP_evi[, c("b4", "sp", "stat_id")], LSP_ndvi[, c("b4", "sp", "stat_id")], by="stat_id")
colnames(LSP) <- c("stat_id", "LOG_EVI", "GAM_EVI", "LOG_NDVI", "GAM_NDVI")

data <- merge(tmk, LSP[, c("LOG_EVI", "GAM_EVI","LOG_NDVI","GAM_NDVI", "stat_id")], by="stat_id", all.x=TRUE)
data <- data[!is.na(data$LOG_EVI), ]
data <- data[!is.na(data$GAM_EVI), ]
data <- data[!is.na(data$GAM_NDVI), ]
data <- data[!is.na(data$LOG_NDVI), ]
data_PEP <- merge(tmk, PEP_SOS, by.x="stat_id", by.y="DWD_ID")

###########################################################################################

tt_GDD <- function(statid, 
                     t_day, 
                     year,
                     doy,
                     t_base=5,
                     doy_crit) {
  
  data = data.frame(statid, 
                    t_day,
                    year, 
                    doy,
                    doy_crit)
  
  SOS_LSP <- NULL
  
  for( i in unique(data$statid)){   # loop through plots 
    
    d = subset(data, data$statid == i)
    doy_crit <- d$doy_crit[1]
    d = subset(d, d$doy <= doy_crit)
    forcing <- 0
      
    for(x in d$doy){              # loop through days until SOS
        
      out <- NULL
      dat <- subset(d, d$doy == x)
      doy_sos <- dat$doy
      t <- dat$t_day
      
      if (t > t_base){
        val = t-t_base
        forcing = forcing + val 
      }
    }
    out <- data.frame("stat_id" = i, 
                      "GDD_PEP" = forcing)
    SOS_LSP <- rbind(SOS_LSP, out)
  }
  return(SOS_LSP)
}

###########################################################################################

GDD_GAM_EVI <- tt_GDD(statid=data$stat_id, 
                   t_day=data$WERT,
                   year = data$year,
                   doy= data$doy,
                   doy_crit = data$GAM_EVI)

GDD_LOG_EVI <- tt_GDD(statid=data$stat_id, 
                      t_day=data$WERT,
                      year = data$year,
                      doy= data$doy,
                      doy_crit = data$LOG_EVI)

GDD_LOG_NDVI <- tt_GDD(statid=data$stat_id, 
                      t_day=data$WERT,
                      year = data$year,
                      doy= data$doy,
                      doy_crit = data$LOG_NDVI)

GDD_GAM_NDVI <- tt_GDD(statid=data$stat_id, 
                      t_day=data$WERT,
                      year = data$year,
                      doy= data$doy,
                      doy_crit = data$GAM_NDVI)

GDD_PEP <- tt_GDD(statid=data_PEP$stat_id, 
                      t_day=data_PEP$WERT,
                      year = data_PEP$year,
                      doy= data_PEP$doy,
                      doy_crit = data_PEP$day)
###########################################################################################

GDD_SOS <- merge(GDD_GAM_EVI, GDD_LOG_EVI, by="stat_id")
GDD_SOS <- merge(GDD_SOS, GDD_GAM_NDVI, by="stat_id")
GDD_SOS <- merge(GDD_SOS, GDD_LOG_NDVI, by="stat_id")
GDD_SOS <- merge(GDD_SOS, GDD_PEP, by="stat_id", all.x=TRUE)

LSP_evi <- rename(LSP_evi, c("b4"="LOG_EVI","sp"="GAM_EVI"))
LSP_ndvi <- rename(LSP_ndvi, c("b4"="LOG_NDVI","sp"="GAM_NDVI"))

GDD_SOS <- merge(GDD_SOS, LSP_evi[, c("LOG_EVI", "GAM_EVI", "stat_id","X","Y")], by="stat_id", all.x=TRUE)
GDD_SOS <- merge(GDD_SOS, LSP_ndvi[, c("LOG_NDVI", "GAM_NDVI", "stat_id")], by="stat_id", all.x=TRUE)

write.csv(GDD_SOS, file="20190119_TT_GDD_results.csv",row.names = FALSE )

# mean GDD 
mean(GDD_SOS$GDD_GAM_EVI)
mean(GDD_SOS$GDD_LOG_EVI)
mean(GDD_SOS$GDD_GAM_NDVI)
mean(GDD_SOS$GDD_LOG_NDVI)

# correlation of models, same index 
cor.test(GDD_SOS$GDD_GAM_EVI, GDD_SOS$GDD_LOG_EVI, use="complete.obs")
cor.test(GDD_SOS$GDD_GAM_NDVI, GDD_SOS$GDD_LOG_NDVI, use="complete.obs")

# correlation of indices, same model
cor.test(GDD_SOS$GDD_GAM_EVI, GDD_SOS$GDD_GAM_NDVI, use="complete.obs")
cor.test(GDD_SOS$GDD_LOG_EVI, GDD_SOS$GDD_LOG_NDVI, use="complete.obs")

# plot histogram of all GDD estimates
pl_GDD_EVI <- melt(GDD_SOS[, c("GDD_GAM_EVI", "GDD_LOG_EVI", "GDD_PEP")])
pl_GDD_NDVI <- melt(GDD_SOS[, c("GDD_GAM_NDVI", "GDD_LOG_NDVI", "GDD_PEP")])
pl_GDD <- rbind(pl_GDD_EVI, pl_GDD_NDVI)


png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190119_GDD_EVI_histogram.png", 
    width= 1200, height=1000, res=200 )
ggplot(data=pl_GDD_EVI)+
  geom_histogram(aes(x=value, fill=variable), 
                 binwidth=5, 
                 alpha=1/2,
                 position="identity")+
  labs(x="GDD", title="TT Model EVI")
dev.off()


png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190119_GDD_NDVI_histogram.png", 
    width= 1200, height=1000, res=200 )
ggplot(data=pl_GDD_NDVI)+
  geom_histogram(aes(x=value, fill=variable), 
                 binwidth=5, 
                 alpha=1/2,
                 position="identity")+
  labs(x="GDD", title="TT Model NDVI")
dev.off()

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190120_GDD_boxplot.png", 
    width= 1400, height=1000, res=200 )
ggplot(data=pl_GDD)+
  geom_boxplot(aes(y=value, x=variable, fill=variable))+
  scale_x_discrete(labels=c("GAM_EVI","LOG_EVI","PEP","GAM_NDVI","LOG_NDVI"))+
  theme(legend.position="none")+
  labs(x=" ", y="GDD")
dev.off()
