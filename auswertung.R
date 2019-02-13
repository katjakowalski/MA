
#### RS models ####

## Tab. X 
# model differences (sample)
mean(results_ndvi$diff_px, na.rm=TRUE)
mean(results_evi$diff_px, na.rm=TRUE)

mean(results_ndvi$observations)
mean(results_evi$observations)

# Convergence (sample)
map(results_evi[c(2,9)], function(x) mean(is.na(x)))
map(results_ndvi[c(2,9)], function(x) mean(is.na(x)))

# Correlation (sample)
cor(results_evi$b4, results_evi$sp, use="complete.obs")
cor(results_ndvi$b4, results_ndvi$sp, use="complete.obs")

# Percentiles (sample)
map(results_evi[c(2,9)], function(x) quantile(x, na.rm=TRUE, c(.05, .50, .95)))
map(results_ndvi[c(2,9)], function(x) quantile(x, na.rm=TRUE, c(.05, .50, .95)))

# differences between indices (sample)
results_px <- merge(results_ndvi[, c("plotid","b4","sp","observations", "stat_id")], 
                    results_evi[, c("plotid","b4","sp")], by="plotid")
colnames(results_px) <- c("plotid","LOG_NDVI", "GAM_NDVI","observations", "stat_id", "LOG_EVI","GAM_EVI")
results_px$GAM_diff <- results_px$GAM_NDVI- results_px$GAM_EVI
results_px$LOG_diff <- results_px$LOG_NDVI - results_px$LOG_EVI

results_px$NDVI_diff <- results_px$GAM_NDVI- results_px$LOG_NDVI
results_px$EVI_diff <- results_px$GAM_EVI - results_px$LOG_EVI

cor(results_px$LOG_NDVI, results_px$LOG_EVI, use="complete.obs")
cor(results_px$GAM_NDVI, results_px$GAM_EVI, use="complete.obs")

map(results_px[8:11], mean, na.rm=TRUE)

# no. of sample fits for each station
log_evi <- results_evi %>%
  filter(!is.na(b4)) %>%
  group_by(stat_id) %>%
  summarise(n = n()) %>%
  map(., mean)

log_ndvi <- results_ndvi %>%
  filter(!is.na(b4)) %>%
  group_by(stat_id) %>%
  summarise(ct = n()) %>%
  map(., mean)


sample_log_evi <- subset(results_evi, !is.na(b4))
sample_log_evi <- as.data.frame(table(sample_log_evi$stat_id))

sample_log_ndvi <- subset(results_ndvi, !is.na(b4))
sample_log_ndvi <- as.data.frame(table(sample_log_ndvi$stat_id))

sample_gam_ndvi <- subset(results_ndvi, !is.na(sp))
sample_gam_ndvi <- as.data.frame(table(sample_gam_ndvi$stat_id))

sample_gam_evi <- subset(results_evi, !is.na(sp))
sample_gam_evi <- as.data.frame(table(sample_gam_evi$stat_id))

sum(mean(sample_log_evi$Freq),
    mean(sample_log_ndvi$Freq),
    mean(sample_gam_ndvi$Freq),
    mean(sample_gam_evi$Freq))/4

# mean difference (plot)
mean(mean_ndvi$diff_station, na.rm=TRUE)
sd(mean_ndvi$diff_station, na.rm=TRUE)

mean(mean_evi$diff_station)
sd(mean_evi$diff_station)

# mean MSE (station)
mean(mean_evi$MSE_gam)
mean(mean_evi$MSE_log)

mean(mean_ndvi$MSE_gam)
mean(mean_ndvi$MSE_log, na.rm=TRUE)

# Correlation (station)
cor.test(mean_ndvi$b4, mean_ndvi$sp, use="complete.obs")
cor.test(mean_evi$b4, mean_evi$sp, use="complete.obs")

# Percentiles (station)
data.frame("LOG_NDVI" = c(quantile(mean_ndvi$b4, na.rm=TRUE, c(.05, .50, .95))),
           "LOG_EVI"=c(quantile(mean_evi$b4, na.rm=TRUE, c(.05, .50, .95))),
           "GAM_NDVI" = c(quantile(mean_ndvi$sp, na.rm=TRUE, c(.05, .50, .95))),
           "GAM_EVI" = c(quantile(mean_evi$sp, na.rm=TRUE, c(.05, .50, .95)))
)

# mean SOS and sd

data.frame(c(mean(mean_ndvi$b4, na.rm=TRUE),
             mean(mean_evi$b4),
             mean(mean_ndvi$sp),
             mean(mean_evi$sp)))


# differences between indices (station)
mean_results <- merge(mean_evi[, c("stat_id","b4","sp","observations")], 
                      mean_ndvi[, c("stat_id","b4","sp")], by="stat_id")
colnames(mean_results) <- c("stat_id","LOG_EVI", "GAM_EVI","observations","LOG_NDVI","GAM_NDVI")
mean_results$GAM_diff <- mean_results$GAM_NDVI - mean_results$GAM_EVI
mean_results$LOG_diff <- mean_results$LOG_NDVI - mean_results$LOG_EVI

cor.test(mean_results$LOG_NDVI, mean_results$LOG_EVI, use="complete.obs")
cor.test(mean_results$GAM_NDVI, mean_results$GAM_EVI, use="complete.obs")

mean(mean_results$LOG_diff, na.rm=TRUE)
mean(mean_results$GAM_diff, na.rm=TRUE)

#### end ####

data.frame("LOG_NDVI" = ,
           "LOG_EVI" = ,
           "GAM_NDVI" = ,
           "GAM_EVI" = )

##### phenological gradients #####

## SOS ##

# east-west gradient SOS 
data.frame("LOG_NDVI" = cor(GDD_SOS$LOG_NDVI, GDD_SOS$X, use="complete.obs") ,
           "LOG_EVI" = cor(GDD_SOS$LOG_EVI, GDD_SOS$X),
           "GAM_NDVI" = cor(GDD_SOS$GAM_NDVI, GDD_SOS$X),
           "GAM_EVI" = cor(GDD_SOS$GAM_EVI, GDD_SOS$X))


# elevation gradient SOS
data.frame("LOG_NDVI" = cor(GDD_SOS$DEM, GDD_SOS$LOG_NDVI, use="complete.obs"),
           "LOG_EVI" = cor(GDD_SOS$DEM, GDD_SOS$LOG_EVI, use="complete.obs"),
           "GAM_NDVI" = cor(GDD_SOS$DEM, GDD_SOS$GAM_NDVI, use="complete.obs"),
           "GAM_EVI" = cor(GDD_SOS$DEM, GDD_SOS$GAM_EVI, use="complete.obs"),
           "PEP" = cor(GDD_SOS$DEM, GDD_SOS$PEP_SOS, use="complete.obs"),
           "TT" = cor(GDD_SOS$DEM, GDD_SOS$TT, use="complete.obs"),
           "SQ" = cor(GDD_SOS$DEM, GDD_SOS$SQ, use="complete.obs"))

data.frame("LOG_NDVI" = cor(GDD_PEP$LOG_NDVI, GDD_PEP$DEM),
           "LOG_EVI" = cor(GDD_PEP$LOG_EVI, GDD_PEP$DEM),
           "GAM_NDVI" = cor(GDD_PEP$GAM_NDVI, GDD_PEP$DEM),
           "GAM_EVI" = cor(GDD_PEP$GAM_EVI, GDD_PEP$DEM) )


# urban LC SOS
data.frame("LOG_NDVI" = cor(GDD_SOS$X_sum, GDD_SOS$LOG_NDVI, use="complete.obs"),
           "LOG_EVI" = cor(GDD_SOS$X_sum, GDD_SOS$LOG_EVI, use="complete.obs"),
           "GAM_NDVI" = cor(GDD_SOS$X_sum, GDD_SOS$GAM_NDVI, use="complete.obs"),
           "GAM_EVI" = cor(GDD_SOS$X_sum, GDD_SOS$GAM_EVI, use="complete.obs"),
           "PEP" = cor(GDD_SOS$X_sum, GDD_SOS$PEP_SOS, use="complete.obs"),
           "TT" = cor(GDD_SOS$X_sum, GDD_SOS$TT, use="complete.obs"),
           "SQ" = cor(GDD_SOS$X_sum, GDD_SOS$SQ, use="complete.obs"))

## GDD ##

# east-west gradient GDD
data.frame("LOG_NDVI" = cor(GDD_SOS$GDD_LOG_NDVI, GDD_SOS$X,use="complete.obs"),
           "LOG_EVI" = cor(GDD_SOS$GDD_LOG_EVI, GDD_SOS$X),
           "GAM_NDVI" = cor(GDD_SOS$GDD_GAM_NDVI, GDD_SOS$X),
           "GAM_EVI" = cor(GDD_SOS$GDD_GAM_EVI, GDD_SOS$X),
           "PEP" = cor(GDD_SOS$GDD_PEP, GDD_SOS$X,use="complete.obs"))

# elevation LC GDD
data.frame("LOG_NDVI" = cor(GDD_SOS$DEM, GDD_SOS$GDD_LOG_NDVI, use="complete.obs"),
           "LOG_EVI" = cor(GDD_SOS$DEM, GDD_SOS$GDD_LOG_EVI, use="complete.obs"),
           "GAM_NDVI" = cor(GDD_SOS$DEM, GDD_SOS$GDD_GAM_NDVI, use="complete.obs"),
           "GAM_EVI" = cor(GDD_SOS$DEM, GDD_SOS$GDD_GAM_EVI, use="complete.obs"),
           "PEP" = cor(GDD_SOS$DEM, GDD_SOS$GDD_PEP, use="complete.obs"))

# urban LC GDD
data.frame("LOG_NDVI" = cor(GDD_PEP$X_sum, GDD_PEP$GDD_LOG_NDVI, use="complete.obs"),
           "LOG_EVI" = cor(GDD_PEP$X_sum, GDD_PEP$GDD_LOG_EVI, use="complete.obs"),
           "GAM_NDVI" = cor(GDD_PEP$X_sum, GDD_PEP$GDD_GAM_NDVI, use="complete.obs"),
           "GAM_EVI" = cor(GDD_PEP$X_sum, GDD_PEP$GDD_GAM_EVI, use="complete.obs"),
           "PEP" = cor(GDD_PEP$X_sum, GDD_PEP$GDD_PEP, use="complete.obs"))



## Residuals TT ##

# elevation residuals TT
data.frame("LOG_NDVI" = cor(GDD_SOS$DEM, GDD_SOS$diff_LOG_NDVI_TT, use="complete.obs"),
           "LOG_EVI" = cor(GDD_SOS$DEM, GDD_SOS$diff_LOG_EVI_TT, use="complete.obs"),
           "GAM_NDVI" = cor(GDD_SOS$DEM, GDD_SOS$diff_GAM_NDVI_TT, use="complete.obs"),
           "GAM_EVI" = cor(GDD_SOS$DEM, GDD_SOS$diff_GAM_EVI_TT, use="complete.obs"),
           "PEP" = cor(GDD_SOS$DEM, GDD_SOS$diff_PEP_TT, use="complete.obs"))

# urban LC residuals TT
data.frame("LOG_NDVI" = cor(GDD_SOS$X_sum, GDD_SOS$diff_LOG_NDVI_TT, use="complete.obs"),
           "LOG_EVI" = cor(GDD_SOS$X_sum, GDD_SOS$diff_LOG_EVI_TT, use="complete.obs"),
           "GAM_NDVI" = cor(GDD_SOS$X_sum, GDD_SOS$diff_GAM_NDVI_TT, use="complete.obs"),
           "GAM_EVI" = cor(GDD_SOS$X_sum, GDD_SOS$diff_GAM_EVI_TT, use="complete.obs"),
           "PEP" = cor(GDD_SOS$X_sum, GDD_SOS$diff_PEP_TT, use="complete.obs"))

ggplot(GDD_SOS)+
  geom_point(aes(x=diff_LOG_EVI_TT, y=CD_GAM_EVI))

cor(GDD_SOS$X_sum, GDD_SOS$CD_LOG_EVI,use="complete.obs")

ggplot(GDD_SOS)+
  geom_point(aes(x=Y, y=CD_GAM_NDVI))

## Residuals SQ ##

# east west gradient residuals SQ
data.frame("LOG_NDVI" = cor(GDD_SOS$X, GDD_SOS$diff_LOG_NDVI_SQ, use="complete.obs"),
           "LOG_EVI" = cor(GDD_SOS$X, GDD_SOS$diff_LOG_EVI_SQ, use="complete.obs"),
           "GAM_NDVI" = cor(GDD_SOS$X, GDD_SOS$diff_GAM_NDVI_SQ, use="complete.obs"),
           "GAM_EVI" = cor(GDD_SOS$X, GDD_SOS$diff_GAM_EVI_SQ, use="complete.obs"),
           "PEP" = cor(GDD_SOS$X, GDD_SOS$diff_PEP_SQ, use="complete.obs"))

# elevation residuals SQ
data.frame("LOG_NDVI" = cor(GDD_SOS$DEM, GDD_SOS$diff_LOG_NDVI_SQ, use="complete.obs"),
           "LOG_EVI" = cor(GDD_SOS$DEM, GDD_SOS$diff_LOG_EVI_SQ, use="complete.obs"),
           "GAM_NDVI" = cor(GDD_SOS$DEM, GDD_SOS$diff_GAM_NDVI_SQ, use="complete.obs"),
           "GAM_EVI" = cor(GDD_SOS$DEM, GDD_SOS$diff_GAM_EVI_SQ, use="complete.obs"),
           "PEP" = cor(GDD_SOS$DEM, GDD_SOS$diff_PEP_SQ, use="complete.obs"))

# urban LC residuals SQ
data.frame("LOG_NDVI" = cor(GDD_SOS$X_sum, GDD_SOS$diff_LOG_NDVI_SQ, use="complete.obs"),
           "LOG_EVI" = cor(GDD_SOS$X_sum, GDD_SOS$diff_LOG_EVI_SQ, use="complete.obs"),
           "GAM_NDVI" = cor(GDD_SOS$X_sum, GDD_SOS$diff_GAM_NDVI_SQ, use="complete.obs"),
           "GAM_EVI" = cor(GDD_SOS$X_sum, GDD_SOS$diff_GAM_EVI_SQ, use="complete.obs"),
           "PEP" = cor(GDD_SOS$X_sum, GDD_SOS$diff_PEP_SQ, use="complete.obs"))


#### end ####


#### TT Model ####

quantile(GDD_SOS$TT,na.rm=TRUE, c(0.05,0.5,0.95))

mean(GDD_SOS$TT)

# correlation SOS & TT
cor.test(GDD_SOS$GAM_NDVI, GDD_SOS$TT, use="complete.obs")
cor.test(GDD_SOS$LOG_NDVI, GDD_SOS$TT, use="complete.obs")

cor.test(GDD_SOS$GAM_EVI, GDD_SOS$TT, use="complete.obs")
cor.test(GDD_SOS$LOG_EVI, GDD_SOS$TT, use="complete.obs")

cor.test(GDD_SOS$PEP_SOS, GDD_SOS$TT, use="complete.obs")

# mean difference 
mean(GDD_SOS$diff_GAM_EVI_TT, na.rm = TRUE)
mean(GDD_SOS$diff_LOG_EVI_TT, na.rm = TRUE)

mean(GDD_SOS$diff_GAM_NDVI_TT, na.rm = TRUE)
mean(GDD_SOS$diff_LOG_NDVI_TT, na.rm = TRUE)

mean(GDD_SOS$diff_PEP_TT, na.rm=TRUE)

# correlation difference & east-west
cor.test(GDD_SOS$diff_GAM_EVI_TT, GDD_SOS$X, use="complete.obs")
cor.test(GDD_SOS$diff_LOG_EVI_TT, GDD_SOS$X, use="complete.obs")
cor.test(GDD_SOS$diff_LOG_NDVI_TT, GDD_SOS$X, use="complete.obs")
cor.test(GDD_SOS$diff_GAM_NDVI_TT, GDD_SOS$X, use="complete.obs")
cor.test(GDD_SOS$diff_PEP_TT, GDD_SOS$X, use="complete.obs")


# difference SOS - TT & DEM
cor.test(SOS_TT$diff_GAM_EVI_TT, SOS_TT$DEM, use="complete.obs")
cor.test(SOS_TT$diff_LOG_EVI_TT, SOS_TT$DEM, use="complete.obs")
cor.test(SOS_TT$diff_GAM_NDVI_TT, SOS_TT$DEM, use="complete.obs")
cor.test(SOS_TT$diff_LOG_NDVI_TT, SOS_TT$DEM, use="complete.obs")

# SOS 
cor.test(SOS_TT$GAM_EVI, SOS_TT$DEM, use="complete.obs")
cor.test(SOS_TT$GAM_NDVI, SOS_TT$DEM, use="complete.obs")
cor.test(SOS_TT$LOG_NDVI, SOS_TT$DEM, use="complete.obs")
cor.test(SOS_TT$LOG_EVI, SOS_TT$DEM, use="complete.obs")

cor.test(GDD_SOS$PEP_SOS, GDD_SOS$DEM)

#### end ####

#### TT_GDD ####

# mean GDD 
data.frame(map(GDD_SOS[c("GDD_LOG_NDVI","GDD_LOG_EVI","GDD_GAM_NDVI", 
              "GDD_GAM_EVI", "GDD_PEP")], mean, na.rm=TRUE))


data.frame("LOG_NDVI" =quantile(GDD_SOS$GDD_LOG_NDVI, c(0.05, 0.5, 0.95), na.rm=TRUE),
           "LOG_EVI" = quantile(GDD_SOS$GDD_LOG_EVI, c(0.05, 0.5, 0.95)),
           "GAM_NDVI" = quantile(GDD_SOS$GDD_GAM_NDVI, c(0.05, 0.5, 0.95)),
           "GAM_EVI" = quantile(GDD_SOS$GDD_GAM_EVI, c(0.05, 0.5, 0.95)),
           "PEP" = quantile(GDD_SOS$GDD_PEP, c(0.05, 0.5, 0.95)))



#mean SOS PEP, quantiles
mean(GDD_SOS$PEP_SOS, na.rm=TRUE)
quantile(GDD_SOS$PEP_SOS, c(0.05,0.5,0.95), na.rm=TRUE)

# correlation SOS & PEP
cor.test(GDD_SOS$GAM_EVI, GDD_SOS$PEP_SOS)
cor.test(GDD_SOS$LOG_EVI, GDD_SOS$PEP_SOS)
cor.test(GDD_SOS$GAM_NDVI, GDD_SOS$PEP_SOS)
cor.test(GDD_SOS$LOG_NDVI, GDD_SOS$PEP_SOS)

cor.test(GDD_SOS$TT, GDD_SOS$PEP_SOS, use="complete.obs")
cor.test(GDD_SOS$SQ, GDD_SOS$PEP_SOS, use="complete.obs")



# correlation of models, same index 
cor.test(GDD_SOS$GDD_GAM_EVI, GDD_SOS$GDD_LOG_EVI, use="complete.obs")
cor.test(GDD_SOS$GDD_GAM_NDVI, GDD_SOS$GDD_LOG_NDVI, use="complete.obs")

# correlation of indices, same model
cor.test(GDD_SOS$GDD_GAM_EVI, GDD_SOS$GDD_GAM_NDVI, use="complete.obs")
cor.test(GDD_SOS$GDD_LOG_EVI, GDD_SOS$GDD_LOG_NDVI, use="complete.obs")

# correlation GDD PEP & GDD SOS 
cor.test(GDD_SOS$GDD_GAM_EVI, GDD_SOS$GDD_PEP, use="complete.obs")
cor.test(GDD_SOS$GDD_LOG_NDVI, GDD_SOS$GDD_PEP, use="complete.obs")
cor.test(GDD_SOS$GDD_GAM_NDVI, GDD_SOS$GDD_PEP, use="complete.obs")
cor.test(GDD_SOS$GDD_LOG_EVI, GDD_SOS$GDD_PEP, use="complete.obs")


# correlation GDD & CD
cor.test(GDD_SOS$GDD_LOG_NDVI, GDD_SOS$CD_LOG_NDVI, use="complete.obs")
cor.test(GDD_SOS$GDD_LOG_EVI, GDD_SOS$CD_LOG_EVI, use="complete.obs")
cor.test(GDD_SOS$GDD_GAM_NDVI, GDD_SOS$CD_GAM_NDVI, use="complete.obs")
cor.test(GDD_SOS$GDD_GAM_EVI, GDD_SOS$CD_GAM_EVI, use="complete.obs")

cor.test(GDD_SOS$GDD_PEP, GDD_SOS$CD_PEP)

# regression GDD SOS dependent on CD

GAM_EVI <- lm(diff_LOG_EVI_TT ~ CD_LOG_EVI, data=GDD_SOS)

ggplot(data=GDD_SOS)+
  geom_point(aes(x=diff_GAM_EVI_TT, CD_GAM_EVI))

summary(GAM_EVI)

GDD_SOS$CD_GAM_EVI

# mean SOS and standard deviation
mean(GDD_SOS$GAM_EVI)
sd(GDD_SOS$GAM_EVI)
mean(GDD_SOS$LOG_EVI)
sd(GDD_SOS$LOG_EVI)

mean(GDD_SOS$GAM_NDVI)
sd(GDD_SOS$GAM_NDVI)
mean(GDD_SOS$LOG_NDVI)
sd(GDD_SOS$LOG_NDVI)

mean(GDD_SOS$PEP_SOS, na.rm=TRUE)
mean(GDD_PEP$GAM_EVI)

cor.test(GDD_SOS$GAM_EVI, GDD_SOS$PEP_SOS)
cor.test(GDD_SOS$LOG_EVI, GDD_SOS$PEP_SOS)

cor.test(GDD_SOS$GAM_NDVI, GDD_SOS$PEP_SOS)
cor.test(GDD_SOS$LOG_NDVI, GDD_SOS$PEP_SOS)


#### end ####

#### SQ Model ####


quantile(GDD_SOS$SQ,na.rm=TRUE, c(0.05,0.5,0.95))
mean(GDD_SOS$SQ, na.rm=TRUE)

# correlation
data.frame("LOG_NDVI" = ,
           "LOG_EVI" = ,
           "GAM_NDVI" = ,
           "GAM_EVI" = )
cor(GDD_SOS$GAM_EVI, GDD_SOS$SQ, use="complete.obs")
cor(GDD_SOS$LOG_EVI, GDD_SOS$SQ, use="complete.obs")

cor(GDD_SOS$GAM_NDVI, GDD_SOS$SQ, use="complete.obs")
cor(GDD_SOS$LOG_NDVI, GDD_SOS$SQ, use="complete.obs")

cor(GDD_SOS$PEP_SOS, GDD_SOS$SQ, use="complete.obs")

cor(GDD_SOS$TT, GDD_SOS$SQ, use="complete.obs")

mean(GDD_SOS$diff_GAM_EVI_SQ, na.rm = TRUE)
mean(GDD_SOS$diff_LOG_EVI_SQ, na.rm = TRUE)
mean(GDD_SOS$diff_LOG_NDVI_SQ, na.rm = TRUE)
mean(GDD_SOS$diff_GAM_NDVI_SQ, na.rm = TRUE)
mean(GDD_SOS$diff_PEP_SQ, na.rm=TRUE)

quantile(pheno_rs_cd$CD, na.rm=TRUE, c(.05, .50,  .95))
mean(pheno_rs$diff_TT_CD, na.rm=TRUE)



#### end ####


#### TT LSP cal ####
cor.test(GDD_SOS$CD_GAM_EVI, GDD_SOS$diff_GAM_EVI_SQ, use="complete.obs")
cor.test(GDD_SOS$CD_LOG_EVI, GDD_SOS$diff_LOG_EVI_SQ)
cor.test(GDD_SOS$CD_GAM_NDVI, GDD_SOS$diff_GAM_NDVI_SQ)
cor.test(GDD_SOS$CD_LOG_NDVI, GDD_SOS$diff_LOG_NDVI_SQ)

cor.test(GDD_SOS$CD_PEP, GDD_SOS$diff_PEP_SQ)


# correlation CD and differences TT/SOS
cor.test(GDD_SOS$CD_LOG_EVI, GDD_SOS$diff_LOG_EVI_TT)
cor.test(GDD_SOS$CD_GAM_EVI, GDD_SOS$diff_GAM_EVI_TT)

cor.test(GDD_SOS$CD_LOG_NDVI, GDD_SOS$diff_LOG_NDVI_TT)
cor.test(GDD_SOS$CD_GAM_NDVI, GDD_SOS$diff_GAM_NDVI_TT)


# correlation GDD & CD
cor.test(GDD_SOS$CD_GAM_EVI, GDD_SOS$GDD_GAM_EVI)
cor.test(GDD_SOS$CD_LOG_EVI, GDD_SOS$GDD_LOG_EVI)
cor.test(GDD_SOS$CD_GAM_NDVI, GDD_SOS$GDD_GAM_NDVI)
cor.test(GDD_SOS$CD_LOG_NDVI, GDD_SOS$GDD_LOG_NDVI)


# correlation SOS & SQ
cor.test(GDD_SOS$GAM_EVI, GDD_SOS$SQ)

# mean CD
sum(mean(GDD_SOS$CD_GAM_EVI, na.rm=TRUE),
    mean(GDD_SOS$CD_LOG_EVI, na.rm=TRUE),
    mean(GDD_SOS$CD_GAM_NDVI, na.rm=TRUE),
    mean(GDD_SOS$CD_LOG_NDVI, na.rm=TRUE))/4

#### end ####

#### mean sprint temperature ####
cor.test(GDD_SOS$GAM_EVI, GDD_SOS$spring_mean_temp)
cor.test(GDD_SOS$LOG_EVI, GDD_SOS$spring_mean_temp)
cor.test(GDD_SOS$GAM_NDVI, GDD_SOS$spring_mean_temp)
cor.test(GDD_SOS$LOG_NDVI, GDD_SOS$spring_mean_temp)
cor.test(GDD_SOS$PEP_SOS, GDD_SOS$spring_mean_temp)

cor.test(GDD_SOS$TT, GDD_SOS$spring_mean_temp)
cor.test(GDD_SOS$SQ, GDD_SOS$spring_mean_temp)


#### end ####

