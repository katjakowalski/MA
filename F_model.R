setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk")
tmk <- read.csv(file="TMK_MN004.txt", header=TRUE, sep=";")

tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))

tmk <- tmk %>%
  group_by(STATION_ID)%>%
  mutate(datum = as.Date(datum)) %>%
  complete(datum = seq.Date(min(datum), max(datum), by="day"))

tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)

tmk <- subset(tmk,  tmk$datum >= "2016-09-01" )

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


cd_model <- function(statid, 
                     t_day, 
                     year,
                     doy,
                     date,
                     f_base= 5,
                     c_base = 5,
                     f_crit = 250,
                     c_crit = 80) {
  
  data = data.frame(statid, 
                    t_day,
                    year, 
                    doy,
                    date)
  
  SOS_CD <- NULL
  #k <- 0
  
  for( i in unique(data$statid)){

    d_f = subset(data, data$statid == i & data$year == 2017)
    d_c = subset(data, data$statid == i & data$date >= "2016-09-01" & data$date <= "2017-05-01" )
    print(i)
    print(paste("df", length(d_f$doy)))
    print(paste("dc", length(d_c$doy)))
    
    #if(length(d_f$doy) >= 330 & length(d_c$doy) >= 190 ){
      
      forcing <- 0
      chilling <- 0 
      #k <- k + 1
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
          #print(paste(t1))
          print(paste(k))
          break
        }
      }
      if (t1 == 0){
        print(paste("chilling insufficient for station", i))
        out <- NULL 
        out <- data.frame("stat_id" = i, 
                          "CD" = NA,
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
                              "CD" = doy_sos,
                              "t_CD" = t1)
            SOS_CD <- rbind(SOS_CD, out)
            break
          }
        }
        else{next}
      }
      #if (is.null(out)){
      #  print(paste(i))

      #}
    #}
    #else{print(paste("obs", i))}
  }
  return(SOS_CD)
}


SOS_CD <- cd_model(statid=tmk$STATION_ID, 
                   t_day=tmk$WERT,
                   year = tmk$year,
                   doy= tmk$doy,
                   date = tmk$datum)

mean(is.na(SOS_CD$CD))

# merge with RS SOS estimates 
#setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results")
#pheno_rs_cd <- read.csv(file="20181211_mean_evi.csv", header=TRUE, sep=",")
#pheno_rs_cd <- merge(pheno_rs_cd, SOS_CD, by="stat_id", all.x=TRUE)

pheno_rs <- merge(pheno_rs, SOS_CD, by="stat_id", all.x=TRUE)

# correlation
cor.test(pheno_rs$GAM_EVI, pheno_rs_cd$CD, use="complete.obs")
cor.test(pheno_rs$LOG_EVI, pheno_rs_cd$CD, use="complete.obs")

cor.test(pheno_rs$GAM_NDVI, pheno_rs_cd$CD, use="complete.obs")
cor.test(pheno_rs$LOG_NDVI, pheno_rs_cd$CD, use="complete.obs")

# difference CD - SOS
pheno_rs$diff_GAM_EVI_CD <- pheno_rs$CD - pheno_rs$GAM_EVI
pheno_rs$diff_LOG_EVI_CD <- pheno_rs$CD - pheno_rs$LOG_EVI 

pheno_rs$diff_GAM_NDVI_CD <- pheno_rs$CD - pheno_rs$GAM_NDVI
pheno_rs$diff_LOG_NDVI_CD <- pheno_rs$CD - pheno_rs$LOG_NDVI 

mean(pheno_rs$diff_GAM_EVI_CD, na.rm = TRUE)
mean(pheno_rs$diff_LOG_EVI_CD, na.rm = TRUE)

# difference TT - CD
pheno_rs$diff_TT_CD <- pheno_rs$TT - pheno_rs$CD 

ggplot(data=pheno_rs)+
  geom_histogram(aes(x=diff_GAM_EVI_CD))

quantile(pheno_rs_cd$CD, na.rm=TRUE, c(.05, .50,  .95))

write.csv(pheno_rs, file="20190121_PBM_results.csv",row.names = FALSE )

ggplot(data=pheno_rs)+
  geom_point(aes(x=TT, y=CD))

mean(pheno_rs$diff_TT_CD, na.rm=TRUE)
