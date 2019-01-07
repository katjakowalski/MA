
library(lubridate)
library(ggplot2)
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk")
tmk <- read.csv(file="TMK_MN004.txt", header=TRUE, sep=";")

tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))
tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)



# fill gaps
for(i in unique(tmk$STATION_ID)){
  data <- subset(tmk, tmk$STATION_ID==i & tmk$year==2017)
  if(diff(data$doy) != 1){
    doy_vec <- c(1:365)
    
  }
}


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
    #print(paste(i))
    d = subset(data, data$statid == i & data$year == 2017)
    if(length(d$doy) >= 330){
  
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
    }
    else{print(paste("obs", i))}
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

cor.test(pheno_rs$b4, pheno_rs$TT, use="complete.obs")
cor.test(pheno_rs$sp, pheno_rs$TT, use="complete.obs")

pheno_rs$diff_GAM_TT <- abs(pheno_rs$sp - pheno_rs$TT)
pheno_rs$diff_LOG_TT <- abs(pheno_rs$b4 - pheno_rs$TT)

mean(pheno_rs$diff_GAM_TT, na.rm = TRUE)
mean(pheno_rs$diff_LOG_TT, na.rm = TRUE)

quantile(pheno_rs$TT, na.rm=TRUE, c(.05, .50,  .75, .95))








