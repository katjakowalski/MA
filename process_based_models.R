library(lubridate)
library(ggplot2)


root <- "C:/Users/kowalskk/MA/MA"
###############################################################################################################################
#### TT Model ####

source(file.path(root, "TT_Model.R"))

SOS_TT <- tt_model(
  statid = tmk$STATION_ID,
  t_day = tmk$WERT,
  year = tmk$year,
  doy = tmk$doy
)

# load and merge SOS estimates from RS
mean_evi <- read.csv(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/20190201_results_stat_evi.csv")
mean_ndvi <- read.csv(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/20190201_results_stat_ndvi.csv")

SOS_TT <- merge(mean_evi[, c("sp", "b4","stat_id", "X","Y")], SOS_TT, by="stat_id", all.x=TRUE)
colnames(SOS_TT) <- c("stat_id", "GAM_EVI","LOG_EVI","X","Y","TT" )

SOS_TT <- merge(mean_ndvi[, c("sp", "b4","stat_id")], SOS_TT, by="stat_id", all.y=TRUE)
colnames(SOS_TT)[c(2:3)] <- c("GAM_NDVI","LOG_NDVI")
#### end ####

###############################################################################################################################
#### TT-model - calibrated with SOS ####

source(file.path(root, "TT_GDD.R"))

GDD_GAM_EVI <- tt_GDD(
  statid = data_input$stat_id,
  t_day = data_input$WERT,
  year = data_input$year,
  doy = data_input$doy,
  datum = data_input$datum,
  doy_crit = data_input$GAM_EVI
)
colnames(GDD_GAM_EVI) <- c("stat_id", "GDD_GAM_EVI")

GDD_LOG_EVI <- tt_GDD(
  statid = data_input$stat_id,
  t_day = data_input$WERT,
  year = data_input$year,
  doy = data_input$doy,
  datum = data_input$datum,
  doy_crit = data_input$LOG_EVI
)
colnames(GDD_LOG_EVI) <- c("stat_id", "GDD_LOG_EVI")

GDD_LOG_NDVI <- tt_GDD(
  statid = data_LOG_NDVI$stat_id,
  t_day = data_LOG_NDVI$WERT,
  year = data_LOG_NDVI$year,
  doy = data_LOG_NDVI$doy,
  datum = data_LOG_NDVI$datum,
  doy_crit = data_LOG_NDVI$LOG_NDVI
)
colnames(GDD_LOG_NDVI) <- c("stat_id", "GDD_LOG_NDVI")

GDD_GAM_NDVI <- tt_GDD(
  statid = data_input$stat_id,
  t_day = data_input$WERT,
  year = data_input$year,
  doy = data_input$doy,
  datum = data_input$datum,
  doy_crit = data_input$GAM_NDVI
)
colnames(GDD_GAM_NDVI) <- c("stat_id", "GDD_GAM_NDVI")

GDD_PEP <- tt_GDD(
  statid = data_PEP$stat_id,
  t_day = data_PEP$WERT,
  year = data_PEP$year,
  doy = data_PEP$doy,
  datum = data_PEP$datum,
  doy_crit = data_PEP$day
)
colnames(GDD_PEP) <- c("stat_id", "GDD_PEP")

GDD_SOS <- merge(GDD_GAM_EVI, GDD_LOG_EVI, by="stat_id", all.x=TRUE)
GDD_SOS <- merge(GDD_SOS, GDD_GAM_NDVI, by="stat_id", all.x=TRUE)
GDD_SOS <- merge(GDD_SOS, GDD_LOG_NDVI, by="stat_id", all.x=TRUE)
GDD_SOS <- merge(GDD_SOS, GDD_PEP, by="stat_id", all.x=TRUE)

# add SOS estimates 
GDD_SOS <- merge(GDD_SOS, SOS_TT[, c("LOG_EVI", "GAM_EVI", "LOG_NDVI","GAM_NDVI", "stat_id","X","Y")], by="stat_id", all.x=TRUE)
GDD_SOS <- merge(GDD_SOS, PEP_SOS[, c("DWD_ID","day")], by.x="stat_id",by.y="DWD_ID", all.x=TRUE)
names(GDD_SOS)[names(GDD_SOS) == 'day'] <- 'PEP_SOS'

#add differences 
GDD_SOS <- merge(GDD_SOS, SOS_TT[, c("diff_GAM_EVI_TT","diff_GAM_NDVI_TT","diff_LOG_EVI_TT","diff_LOG_NDVI_TT","TT", "stat_id")], by="stat_id")

#### end ####

###############################################################################################################################
#### SQ-model ####
source(file.path(root, "SQ_model.R"))

SOS_SQ <- sq_model(statid=tmk$STATION_ID, 
                   t_day=tmk$WERT,
                   year = tmk$year,
                   doy= tmk$doy,
                   date = tmk$datum)

# merge with RS SOS estimates 
GDD_SOS <- merge(GDD_SOS, SOS_SQ[,c("SQ", "stat_id")], by="stat_id", all.x=TRUE)

#### end ####

###############################################################################################################################
#### SQ model - calibrated with SOS to count chilling days ####

source(file.path(root, "SQ_model_CD.R"))

CD_GAM_EVI <- tt_CD_count(statid=data_input$stat_id, 
                          t_day=data_input$WERT,
                          year = data_input$year,
                          doy= data_input$doy,
                          datum = data_input$datum,
                          doy_crit = data_input$GAM_EVI)
colnames(CD_GAM_EVI) <- c("stat_id","CD_GAM_EVI")

CD_LOG_EVI <- tt_CD_count(statid=data_input$stat_id, 
                          t_day=data_input$WERT,
                          year = data_input$year,
                          doy= data_input$doy,
                          datum = data_input$datum,
                          doy_crit = data_input$LOG_EVI)
colnames(CD_LOG_EVI) <- c("stat_id", "CD_LOG_EVI")

CD_LOG_NDVI <- tt_CD_count(statid=data_LOG_NDVI$stat_id, 
                           t_day=data_LOG_NDVI$WERT,
                           year = data_LOG_NDVI$year,
                           doy= data_LOG_NDVI$doy,
                           datum = data_LOG_NDVI$datum,
                           doy_crit = data_LOG_NDVI$LOG_NDVI)
colnames(CD_LOG_NDVI) <- c("stat_id", "CD_LOG_NDVI")

CD_GAM_NDVI <- tt_CD_count(statid=data_input$stat_id, 
                           t_day=data_input$WERT,
                           year = data_input$year,
                           doy= data_input$doy,
                           datum = data_input$datum,
                           doy_crit = data_input$GAM_NDVI)
colnames(CD_GAM_NDVI) <- c("stat_id", "CD_GAM_NDVI")


CD_PEP <- tt_CD_count(statid=data_PEP$stat_id, 
                      t_day=data_PEP$WERT,
                      year = data_PEP$year,
                      doy= data_PEP$doy,
                      datum = data_PEP$datum,
                      doy_crit = data_PEP$day)
colnames(CD_PEP) <- c("stat_id",  "CD_PEP")


CD_SOS <- merge(CD_GAM_EVI, CD_LOG_EVI, by="stat_id", all.x=TRUE)
CD_SOS <- merge(CD_SOS, CD_GAM_NDVI, by="stat_id", all.x=TRUE)
CD_SOS <- merge(CD_SOS, CD_LOG_NDVI, by="stat_id", all.x=TRUE)
CD_SOS <- merge(CD_SOS, CD_PEP, by="stat_id", all.x=TRUE)

# add SOS estimates 
GDD_SOS <- merge(GDD_SOS, CD_SOS , by="stat_id", all.x=TRUE)

# difference CD - SOS
GDD_SOS$diff_GAM_EVI_SQ <- GDD_SOS$SQ - GDD_SOS$GAM_EVI
GDD_SOS$diff_LOG_EVI_SQ <- GDD_SOS$SQ - GDD_SOS$LOG_EVI 

GDD_SOS$diff_GAM_NDVI_SQ <- GDD_SOS$SQ - GDD_SOS$GAM_NDVI
GDD_SOS$diff_LOG_NDVI_SQ <- GDD_SOS$SQ - GDD_SOS$LOG_NDVI 

# write results
write.csv(GDD_SOS, file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/20190204_GDD_SOS.csv",row.names = FALSE )

GDD_PEP <- GDD_SOS[!is.na(GDD_SOS$GDD_PEP),]
write.csv(GDD_PEP, file="20190204_GDD_PEP.csv",row.names = FALSE )

#### end ####