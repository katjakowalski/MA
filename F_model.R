setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk")
tmk <- read.csv(file="TMK_MN004.txt", header=TRUE, sep=";")

tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))

tmk <- tmk %>%
  group_by(STATION_ID)%>%
  mutate(datum = as.Date(datum)) %>%
  complete(datum = seq.Date(min(datum), max(datum), by="day"))

tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)

tmk <- subset(tmk, tmk$datum >= "2016-09-01"  & tmk$datum <= "2017-07-15" )    

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

# remove all plots with NA
tmk <- tmk %>%
  group_by(STATION_ID) %>%
  filter(!any(is.nan(WERT))) %>%
  filter(length(STATION_ID) == 318)

###################################################################################
sq_model <- function(statid, 
                     t_day, 
                     year,
                     doy,
                     date,
                     f_base= 5,
                     c_base = 5,
                     f_crit = 250,
                     c_crit = 64) {
  
  data = data.frame(statid, 
                    t_day,
                    year, 
                    doy,
                    date)

  SOS_CD <- NULL

  for( i in unique(data$statid)){

    d_f = subset(data, data$statid == i & data$year == 2017)
    d_c = subset(data, data$statid == i & data$date >= "2016-09-01" & data$date <= "2017-05-01" )

      forcing <- 0
      chilling <- 0 
 
      k <- 0
      t1 <- 0
      # chilling 
      for(e in d_c$doy){
        k = k+1
        dat_c = subset(d_c, d_c$doy == e)
        doy_chill <- dat_c$doy
        t_c <- dat_c$t_day
        
        if( t_c <= c_base){
          chilling = chilling + 1
        }
        
        if(chilling > c_crit){
          t1 = doy_chill
          break
        }
      }
      if (t1 == 0){
        print(paste("chilling insufficient for station", i))
        out <- NULL 
        out <- data.frame("stat_id" = i, 
                          "SQ" = NA,
                          "t_CD" = NA)
        SOS_CD <- rbind(SOS_CD, out)
        next
      }
    
      
      for(f in d_f$doy){
        if(f >= t1 ){
          out <- NULL
          dat <- subset(d_f, d_f$doy == f)
          doy_sos <- dat$doy
          t <- dat$t_day

        
          if (t >= c_base){
            val = t-c_base
            forcing = forcing + val 
          }
          
          if (forcing >= f_crit){
           
            out <- data.frame("stat_id" = i, 
                              "SQ" = doy_sos,
                              "t_CD" = t1)
            SOS_CD <- rbind(SOS_CD, out)
            break
          }
        }
        else{next}
      }
  }
  return(SOS_CD)
}


SOS_SQ <- sq_model(statid=tmk$STATION_ID, 
                   t_day=tmk$WERT,
                   year = tmk$year,
                   doy= tmk$doy,
                   date = tmk$datum)

mean(is.na(SOS_SQ$SQ))


# merge with RS SOS estimates 

GDD_SOS <- merge(GDD_SOS, SOS_SQ[,c("SQ", "stat_id")], by="stat_id", all.x=TRUE)


# correlation
cor.test(GDD_SOS$GAM_EVI, GDD_SOS$SQ, use="complete.obs")
cor.test(GDD_SOS$LOG_EVI, GDD_SOS$SQ, use="complete.obs")

cor.test(GDD_SOS$GAM_NDVI, GDD_SOS$SQ, use="complete.obs")
cor.test(GDD_SOS$LOG_NDVI, GDD_SOS$SQ, use="complete.obs")

cor.test(GDD_SOS$PEP_SOS, GDD_SOS$SQ, use="complete.obs")

# difference CD - SOS
GDD_SOS$diff_GAM_EVI_SQ <- GDD_SOS$SQ - GDD_SOS$GAM_EVI
GDD_SOS$diff_LOG_EVI_SQ <- GDD_SOS$SQ - GDD_SOS$LOG_EVI 

GDD_SOS$diff_GAM_NDVI_SQ <- GDD_SOS$SQ - GDD_SOS$GAM_NDVI
GDD_SOS$diff_LOG_NDVI_SQ <- GDD_SOS$SQ - GDD_SOS$LOG_NDVI 


setwd("O:/Student_Data/Kowalski/MA")
write.csv(GDD_SOS, file="20190202_GDD_SOS.csv",row.names = FALSE )

cor.test(GDD_SOS$CD_GAM_EVI, GDD_SOS$diff_GAM_EVI_SQ)
cor.test(GDD_SOS$CD_LOG_EVI, GDD_SOS$diff_LOG_EVI_SQ)
cor.test(GDD_SOS$CD_GAM_NDVI, GDD_SOS$diff_GAM_NDVI_SQ)
cor.test(GDD_SOS$CD_LOG_NDVI, GDD_SOS$diff_LOG_NDVI_SQ)

mean(GDD_SOS$diff_GAM_EVI_SQ, na.rm = TRUE)
mean(GDD_SOS$diff_LOG_EVI_SQ, na.rm = TRUE)
mean(GDD_SOS$diff_LOG_NDVI_SQ, na.rm = TRUE)
mean(GDD_SOS$diff_GAM_NDVI_SQ, na.rm = TRUE)


# difference TT - CD
pheno_rs$diff_TT_CD <- pheno_rs$TT - pheno_rs$CD 

ggplot(data=pheno_rs)+
  geom_histogram(aes(x=diff_GAM_EVI_CD))

quantile(pheno_rs_cd$CD, na.rm=TRUE, c(.05, .50,  .95))

write.csv(GDD_SOS, file="20190201_GDD_SOS.csv",row.names = FALSE )

ggplot(data=pheno_rs)+
  geom_point(aes(x=TT, y=CD))

mean(pheno_rs$diff_TT_CD, na.rm=TRUE)
