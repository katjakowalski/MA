
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
    #print(paste(i))
    d = subset(data, data$statid == i & data$year == 2017)
    #if(length(d$doy) >= 330){
  
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

############################################################################
# load and merge SOS estimates from RS 
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results")
results_evi <- read.csv(file="20181211_mean_evi.csv", header=TRUE, sep=",")
results_ndvi <- read.csv(file="20181211_mean_ndvi.csv", header=TRUE, sep=",")
pheno_rs <- merge(results_evi[, c("sp", "b4","stat_id", "X", "Y")], SOS_TT, by="stat_id", all.x=TRUE)
colnames(pheno_rs) <- c("stat_id", "GAM_EVI","LOG_EVI","X","Y","TT" )
pheno_rs <- merge(results_ndvi[, c("sp", "b4","stat_id")], pheno_rs, by="stat_id", all.x=TRUE)
colnames(pheno_rs)[c(2:3)] <- c("GAM_NDVI","LOG_NDVI")

# correlation 
cor.test(pheno_rs$GAM_NDVI, pheno_rs$TT, use="complete.obs")
cor.test(pheno_rs$LOG_NDVI, pheno_rs$TT, use="complete.obs")

cor.test(pheno_rs$GAM_EVI, pheno_rs$TT, use="complete.obs")
cor.test(pheno_rs$LOG_EVI, pheno_rs$TT, use="complete.obs")

# difference (residuals)
pheno_rs$diff_GAM_EVI_TT <- pheno_rs$TT - pheno_rs$GAM_EVI
pheno_rs$diff_LOG_EVI_TT <- pheno_rs$TT - pheno_rs$LOG_EVI

pheno_rs$diff_GAM_NDVI_TT <- pheno_rs$TT - pheno_rs$GAM_NDVI 
pheno_rs$diff_LOG_NDVI_TT <- pheno_rs$TT - pheno_rs$LOG_NDVI 

# mean difference 
mean(pheno_rs$diff_GAM_EVI_TT, na.rm = TRUE)
mean(pheno_rs$diff_LOG_EVI_TT, na.rm = TRUE)

mean(pheno_rs$diff_GAM_NDVI_TT, na.rm = TRUE)
mean(pheno_rs$diff_LOG_NDVI_TT, na.rm = TRUE)

# Percentiles
quantile(pheno_rs$TT, na.rm=TRUE, c(.05, .50,  .75, .95))


pheno_rs <- pheno_rs[!is.na(pheno_rs$TT),]

write.csv(pheno_rs, file="20190119_TT_LSP_results.csv",row.names = FALSE )

max(pheno_rs$TT)
# correlation difference with elevation ? 


