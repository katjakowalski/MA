
library(lubridate)
library(ggplot2)
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk")
tmk <- read.csv(file="TMK_MN004.txt", header=TRUE, sep=";")




tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))
tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)

tt_model <- function(statid, 
                     t_day, 
                     year,
                     doy,
                     t_base=5,
                     t_crit = 300) {
 
   data = data.frame(statid, 
                    t_day,
                    year, 
                    doy)
   
   SOS_TT <- NULL
   
   k <- 0
   
   for( i in unique(data$statid)){
     #print(paste(i))
     
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
         forcing = forcing + t
       }
       
       if (forcing > t_crit){
         #print(paste(i, doy_sos))
         out <- data.frame("stat_id" = i, 
                           "SOS" = doy_sos)
         SOS_TT <- rbind(SOS_TT, out)
  
         break 
       }
     }
     if (is.null(out)){
       print(paste(i))
       #out2 <- data.frame(i, 0)
       #SOS_TT <- rbind(SOS_TT, out2)
     }
   }
   return(SOS_TT)
}

SOS_TT <- tt_model(statid=tmk$STATION_ID, 
                   t_day=tmk$WERT,
                   year = tmk$year,
                   doy= tmk$doy)

setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results")

pheno_rs <- read.csv(file="20181211_mean_evi.csv", header=TRUE, sep=",")


pheno_rs <- merge(pheno_rs, SOS_TT, by="stat_id", all.x=TRUE)

ggplot(data=pheno_rs)+
  geom_point(aes(x=SOS, y=b4))

ggplot(data=pheno_rs)+
  geom_histogram(aes(x=SOS), alpha=1/2, binwidth=1)+
  geom_histogram(aes(x=b4), binwidth=1)
