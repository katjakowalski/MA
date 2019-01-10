library(lubridate)
library(ggplot2)


# load DWD meteorological data 
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk")
tmk <- read.csv(file="TMK_MN004.txt", header=TRUE, sep=";")
tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))
tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)
colnames(tmk)[2] <- "stat_id"

# load LSP data 
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/")
LSP <- read.csv(file="20181211_mean_evi.csv", header=TRUE)

data <- merge(tmk, LSP[, c("b4", "sp", "stat_id")], by="stat_id", all.x=TRUE)
data <- data[!is.na(data$sp), ]
data <- data[!is.na(data$b4), ]

data_PEP <- merge(tmk, PEP_SOS, by.x="stat_id", by.y="DWD_ID")

tt_LSP <- function(statid, 
                     t_day, 
                     year,
                     doy,
                     t_base=5,
                     t_crit) {
  
  data = data.frame(statid, 
                    t_day,
                    year, 
                    doy,
                    t_crit)
  
  SOS_LSP <- NULL
  
  for( i in unique(data$statid)){   # loop through plots 
    
    d = subset(data, data$statid == i & data$year == 2017)
    
    if(length(d$doy) >= 330){       # minimum observations in yr
      F_crit <- d$t_crit[1]
      forcing <- 0
      
      for(x in d$doy){              # loop through days 
        
        out <- NULL
        dat <- subset(d, d$doy == x)
        doy_sos <- dat$doy
        t <- dat$t_day
  
        if (t > t_base){
          val = t-t_base
          forcing = forcing + val 
        }

        
        if (forcing >= F_crit){
          out <- data.frame("stat_id" = i, 
                            "GDD_LOG" = doy_sos)
          SOS_LSP <- rbind(SOS_LSP, out)
          break 
        }
      }
      if (is.null(out)){
        print(paste(i))
      }
    }
    else{print(paste("obs", i))}
  }
  return(SOS_LSP)
}



GDD_LSP_GAM <- tt_LSP(statid=data$stat_id, 
                   t_day=data$WERT,
                   year = data$year,
                   doy= data$doy,
                   t_crit = data$sp)

GDD_LSP_LOG <- tt_LSP(statid=data$stat_id, 
                      t_day=data$WERT,
                      year = data$year,
                      doy= data$doy,
                      t_crit = data$b4)

GDD_PEP <- tt_LSP(statid=data_PEP$stat_id, 
                      t_day=data_PEP$WERT,
                      year = data_PEP$year,
                      doy= data_PEP$doy,
                      t_crit = data_PEP$day)

GDD_LSP <- merge(GDD_LSP_GAM, GDD_LSP_LOG, by="stat_id")
GDD_LSP <- merge(GDD_LSP, GDD_PEP, by="stat_id", all.x=TRUE)
GDD_LSP$diff_LOG_GAM <- abs(GDD_LSP$GDD_LOG - GDD_LSP$GDD_GAM)


ggplot(data=SOS_LSP)+
  geom_point(aes(x=SOS_LSP_GAM, y=SOS_PEP))

cor.test(GDD_LSP$GDD_GAM, GDD_LSP$GDD_LOG, use="complete.obs")
cor.test(GDD_LSP$GDD_LOG, GDD_LSP$GDD_PEP, use="complete.obs")



mean(SOS_LSP$diff_LOG_GAM, na.rm = TRUE)

LSP_plot <- melt(SOS_LSP[, c("SOS_LSP_GAM", "SOS_LSP_LOG")])

quantile(SOS_LSP$SOS_LSP_GAM, na.rm=TRUE, c(.05, .50, .95))
quantile(SOS_LSP$SOS_LSP_LOG, na.rm=TRUE, c(.05, .50, .95))
quantile(SOS_LSP$diff_LOG_GAM, na.rm=TRUE, c(.05, .50, .95))

ggplot(data=LSP_plot)+
  geom_histogram(aes(x=value, fill=variable), 
                 binwidth=5, 
                 alpha=1/2,
                 position="identity")

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190110_GDD_density.png", 
    width= 1200, height=1000, res=200 )
ggplot(LSP_plot) +
  geom_density(aes(x = value,y=..scaled..,fill=variable), 
               alpha=1/3,
               adjust=1/3)
dev.off()

ggplot(data=LSP_plot)+
  geom_boxplot(aes(y=value, fill=variable))


