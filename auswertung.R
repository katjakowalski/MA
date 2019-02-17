

#### Tab. 2 #### 
# Correlations and mean differences for four different model and vegetation index combinations. 
tab_2 <- data.frame(
  "r_sample" = c(
    cor(results_px$LOG_EVI, results_px$GAM_EVI, use = "complete.obs"),
    cor(results_px$LOG_NDVI, results_px$GAM_NDVI, use = "complete.obs"),
    cor(results_px$LOG_NDVI, results_px$LOG_EVI, use = "complete.obs"),
    cor(results_px$GAM_NDVI, results_px$GAM_EVI, use = "complete.obs")
  ),
  "r_plot" = c(
    cor(GDD_SOS$LOG_EVI, GDD_SOS$GAM_EVI),
    cor(GDD_SOS$LOG_NDVI, GDD_SOS$GAM_NDVI, use = "complete.obs"),
    cor(GDD_SOS$LOG_NDVI, GDD_SOS$LOG_EVI, use = "complete.obs"),
    cor(GDD_SOS$GAM_EVI, GDD_SOS$GAM_NDVI)
  ),
  "mean_diff_sample" = c(
    mean(results_evi$diff_px, na.rm = TRUE),
    mean(results_ndvi$diff_px, na.rm = TRUE),
    mean(results_px$LOG_diff, na.rm = TRUE),
    mean(results_px$GAM_diff, na.rm = TRUE)
    ),
  "mean_diff_plot" = c(
    mean(mean_evi$diff_station),
    mean(mean_ndvi$diff_station, na.rm = TRUE),
    mean(mean_results$LOG_diff, na.rm = TRUE),
    mean(mean_results$GAM_diff, na.rm = TRUE)
  )
)
tab_2 <- as.data.frame(t(tab_2))
colnames(tab_2) <- c("LOG_EVI_GAM_EVI", "LOG_NDVI_GAM_NDVI","LOG_NDVI_LOG_EVI", "GAM_NDVI_GAM_EVI")     
#### end ####

#### Tab. 3 #### 
# Summary statistics for four remote-sensing models, ground observations and two process-based models. 

tab_3 <- as.data.frame(t(data.frame(
  "convergence" = c(apply(results_px[c("LOG_NDVI", "LOG_EVI", "GAM_NDVI", "GAM_EVI")], 2,
                        function(x) mean(!is.na(x))), NA,NA,NA),
  "Q0.05" = apply(GDD_SOS[c("LOG_NDVI", "LOG_EVI", "GAM_NDVI", "GAM_EVI","PEP_SOS","TT","SQ")], 2,
                     function(x) quantile(x, na.rm=TRUE, .05)),
  "Q0.5" = apply(GDD_SOS[c("LOG_NDVI", "LOG_EVI", "GAM_NDVI", "GAM_EVI","PEP_SOS","TT","SQ")], 2,
                 function(x) quantile(x, na.rm=TRUE, .5)),
  "Q0.95" = apply(GDD_SOS[c("LOG_NDVI", "LOG_EVI", "GAM_NDVI", "GAM_EVI","PEP_SOS","TT","SQ")], 2,
                 function(x) quantile(x, na.rm=TRUE, .5)),
  "MSE" = c(apply(MSE_px[c("LOG_NDVI", "LOG_EVI", "GAM_NDVI", "GAM_EVI")], 2, 
          mean, na.rm=TRUE), NA,NA,NA),
  "mean_SOS" = apply(GDD_SOS[c("LOG_NDVI", "LOG_EVI", "GAM_NDVI", "GAM_EVI","PEP_SOS","TT","SQ")], 2,
                     mean, na.rm=TRUE),
  "r_SQ" = apply(GDD_SOS[c("LOG_NDVI", "LOG_EVI", "GAM_NDVI", "GAM_EVI", "PEP_SOS", "TT", "SQ")], 2, 
                   function(x) cor(x, GDD_SOS$SQ,use="complete.obs")),
  "r_TT" = apply(GDD_SOS[c("LOG_NDVI", "LOG_EVI", "GAM_NDVI", "GAM_EVI", "PEP_SOS", "TT", "SQ")], 2, 
                   function(x) cor(x, GDD_SOS$TT,use="complete.obs")),
  "r_GO" = apply(GDD_SOS[c("LOG_NDVI", "LOG_EVI", "GAM_NDVI", "GAM_EVI", "PEP_SOS", "TT", "SQ")], 2, 
                   function(x) cor(x, GDD_SOS$TT,use="complete.obs")),
  "r_Tmean" = apply(GDD_SOS[c("LOG_NDVI", "LOG_EVI", "GAM_NDVI", "GAM_EVI", "PEP_SOS", "TT", "SQ")], 2, 
                 function(x) cor(x, GDD_SOS$spring_mean_temp,use="complete.obs")),
  "r_elevation" = apply(GDD_SOS[c("LOG_NDVI", "LOG_EVI", "GAM_NDVI", "GAM_EVI", "PEP_SOS", "TT", "SQ")], 2, 
                    function(x) cor(x, GDD_SOS$DEM,use="complete.obs")),
  "r_east_west" = apply(GDD_SOS[c("LOG_NDVI", "LOG_EVI", "GAM_NDVI", "GAM_EVI", "PEP_SOS", "TT", "SQ")], 2, 
                       function(x) cor(x, GDD_SOS$X,use="complete.obs")),
  "r_urban" = apply(GDD_SOS[c("LOG_NDVI", "LOG_EVI", "GAM_NDVI", "GAM_EVI", "PEP_SOS", "TT", "SQ")], 2, 
                        function(x) cor(x, GDD_SOS$X_sum,use="complete.obs"))
)))

#### end ####

#### Tab. 4 Residuals TT model ####
tab_4_1 <- as.data.frame(t(data.frame(
  "Mean" = apply(GDD_SOS[c("diff_LOG_NDVI_TT", "diff_LOG_EVI_TT", "diff_GAM_NDVI_TT", "diff_GAM_EVI_TT", "diff_PEP_TT")], 2, mean, na.rm=TRUE),
  "r2_forcing" = c("diff_LOG_NDVI_TT" = summary(lm(diff_LOG_NDVI_TT~GDD_LOG_NDVI, GDD_SOS))$r.squared,
                  "diff_LOG_EVI_TT" = summary(lm(diff_LOG_EVI_TT~GDD_LOG_EVI, GDD_SOS))$r.squared,
                  "diff_GAM_NDVI_TT" = summary(lm(diff_GAM_NDVI_TT~GDD_GAM_NDVI, GDD_SOS))$r.squared,
                  "diff_GAM_EVI_TT" = summary(lm(diff_GAM_EVI_TT~GDD_GAM_EVI, GDD_SOS))$r.squared,
                  "diff_PEP_TT" = summary(lm(diff_PEP_TT~GDD_PEP, GDD_SOS))$r.squared),
  "r_forcing" = c("dif_LOG_NDVI_TT" = cor(GDD_SOS$GDD_LOG_NDVI, GDD_SOS$diff_LOG_NDVI_TT, use="complete.obs"),
                  "diff_LOG_EVI_TT" = cor(GDD_SOS$GDD_LOG_EVI, GDD_SOS$diff_LOG_EVI_TT, use="complete.obs"),
                  "diff_GAM_NDVI_TT" = cor(GDD_SOS$GDD_GAM_NDVI, GDD_SOS$diff_GAM_NDVI_TT, use="complete.obs"),
                  "diff_GAM_EVI_TT" = cor(GDD_SOS$GDD_GAM_EVI, GDD_SOS$diff_GAM_EVI_TT, use="complete.obs"),
                  "diff_PEP_TT" = cor(GDD_SOS$GDD_PEP, GDD_SOS$diff_PEP_TT, use="complete.obs")),
  "r2_chilling" = c("diff_LOG_NDVI_TT" = summary(lm(diff_LOG_NDVI_TT~CD_LOG_NDVI, GDD_SOS))$r.squared,
                   "diff_LOG_EVI_TT" = summary(lm(diff_LOG_EVI_TT~CD_LOG_EVI, GDD_SOS))$r.squared,
                   "diff_GAM_NDVI_TT" = summary(lm(diff_GAM_NDVI_TT~CD_GAM_NDVI, GDD_SOS))$r.squared,
                   "diff_GAM_EVI_TT" = summary(lm(diff_GAM_EVI_TT~CD_GAM_EVI, GDD_SOS))$r.squared,
                   "diff_PEP_TT" = summary(lm(diff_PEP_TT~CD_PEP, GDD_SOS))$r.squared),
  "r_chilling" = c("dif_LOG_NDVI_TT" = cor(GDD_SOS$CD_LOG_NDVI, GDD_SOS$diff_LOG_NDVI_TT, use="complete.obs"),
                   "diff_LOG_EVI_TT" = cor(GDD_SOS$CD_LOG_EVI, GDD_SOS$diff_LOG_EVI_TT, use="complete.obs"),
                   "diff_GAM_NDVI_TT" = cor(GDD_SOS$CD_GAM_NDVI, GDD_SOS$diff_GAM_NDVI_TT, use="complete.obs"),
                   "diff_GAM_EVI_TT" = cor(GDD_SOS$CD_GAM_EVI, GDD_SOS$diff_GAM_EVI_TT, use="complete.obs"),
                   "diff_PEP_TT" = cor(GDD_SOS$CD_PEP, GDD_SOS$diff_PEP_TT, use="complete.obs")),
  "r2_elevation" = apply(GDD_SOS[c("diff_LOG_NDVI_TT", "diff_LOG_EVI_TT", "diff_GAM_NDVI_TT", "diff_GAM_EVI_TT", "diff_PEP_TT")], 2, 
                        function(x) summary(lm(x ~ DEM, GDD_SOS))$r.squared),
  "r_elevation" = apply(GDD_SOS[c("diff_LOG_NDVI_TT", "diff_LOG_EVI_TT", "diff_GAM_NDVI_TT", "diff_GAM_EVI_TT", "diff_PEP_TT")], 2, 
                      function(x) cor(x, GDD_SOS$DEM,use="complete.obs")),
  "r2_east_west" = apply(GDD_SOS[c("diff_LOG_NDVI_TT", "diff_LOG_EVI_TT", "diff_GAM_NDVI_TT", "diff_GAM_EVI_TT", "diff_PEP_TT")], 2, 
                         function(x) summary(lm(x ~ X, GDD_SOS))$r.squared),
  "r_east_west" = apply(GDD_SOS[c("diff_LOG_NDVI_TT", "diff_LOG_EVI_TT", "diff_GAM_NDVI_TT", "diff_GAM_EVI_TT", "diff_PEP_TT")], 2, 
                        function(x) cor(x, GDD_SOS$X,use="complete.obs")),
  "r2_urban" = apply(GDD_SOS[c("diff_LOG_NDVI_TT", "diff_LOG_EVI_TT", "diff_GAM_NDVI_TT", "diff_GAM_EVI_TT", "diff_PEP_TT")], 2, 
                         function(x) summary(lm(x ~ X_sum, GDD_SOS))$r.squared),
  "r_urban" = apply(GDD_SOS[c("diff_LOG_NDVI_TT", "diff_LOG_EVI_TT", "diff_GAM_NDVI_TT", "diff_GAM_EVI_TT", "diff_PEP_TT")], 2, 
                        function(x) cor(x, GDD_SOS$X_sum,use="complete.obs"))
)))

tab_4_2 <- as.data.frame(t(data.frame(
  "Mean" = apply(GDD_SOS[c("diff_LOG_NDVI_SQ", "diff_LOG_EVI_SQ", "diff_GAM_NDVI_SQ", "diff_GAM_EVI_SQ", "diff_PEP_SQ")], 2, mean, na.rm=TRUE),
  "r2_forcing" = c("diff_LOG_NDVI_SQ" = summary(lm(diff_LOG_NDVI_SQ~GDD_LOG_NDVI, GDD_SOS))$r.squared,
                   "diff_LOG_EVI_SQ" = summary(lm(diff_LOG_EVI_SQ~GDD_LOG_EVI, GDD_SOS))$r.squared,
                   "diff_GAM_NDVI_SQ" = summary(lm(diff_GAM_NDVI_SQ~GDD_GAM_NDVI, GDD_SOS))$r.squared,
                   "diff_GAM_EVI_SQ" = summary(lm(diff_GAM_EVI_SQ~GDD_GAM_EVI, GDD_SOS))$r.squared,
                   "diff_PEP_SQ" = summary(lm(diff_PEP_SQ~GDD_PEP, GDD_SOS))$r.squared),
  "r_forcing" = c("dif_LOG_NDVI_SQ" = cor(GDD_SOS$GDD_LOG_NDVI, GDD_SOS$diff_LOG_NDVI_SQ, use="complete.obs"),
                  "diff_LOG_EVI_SQ" = cor(GDD_SOS$GDD_LOG_EVI, GDD_SOS$diff_LOG_EVI_SQ, use="complete.obs"),
                  "diff_GAM_NDVI_SQ" = cor(GDD_SOS$GDD_GAM_NDVI, GDD_SOS$diff_GAM_NDVI_SQ, use="complete.obs"),
                  "diff_GAM_EVI_SQ" = cor(GDD_SOS$GDD_GAM_EVI, GDD_SOS$diff_GAM_EVI_SQ, use="complete.obs"),
                  "diff_PEP_SQ" = cor(GDD_SOS$GDD_PEP, GDD_SOS$diff_PEP_SQ, use="complete.obs")),
  "r2_chilling" = c("diff_LOG_NDVI_SQ" = summary(lm(diff_LOG_NDVI_SQ~CD_LOG_NDVI, GDD_SOS))$r.squared,
                    "diff_LOG_EVI_SQ" = summary(lm(diff_LOG_EVI_SQ~CD_LOG_EVI, GDD_SOS))$r.squared,
                    "diff_GAM_NDVI_SQ" = summary(lm(diff_GAM_NDVI_SQ~CD_GAM_NDVI, GDD_SOS))$r.squared,
                    "diff_GAM_EVI_SQ" = summary(lm(diff_GAM_EVI_SQ~CD_GAM_EVI, GDD_SOS))$r.squared,
                    "diff_PEP_SQ" = summary(lm(diff_PEP_SQ~CD_PEP, GDD_SOS))$r.squared),
  "r_chilling" = c("dif_LOG_NDVI_SQ" = cor(GDD_SOS$CD_LOG_NDVI, GDD_SOS$diff_LOG_NDVI_SQ, use="complete.obs"),
                   "diff_LOG_EVI_SQ" = cor(GDD_SOS$CD_LOG_EVI, GDD_SOS$diff_LOG_EVI_SQ, use="complete.obs"),
                   "diff_GAM_NDVI_SQ" = cor(GDD_SOS$CD_GAM_NDVI, GDD_SOS$diff_GAM_NDVI_SQ, use="complete.obs"),
                   "diff_GAM_EVI_SQ" = cor(GDD_SOS$CD_GAM_EVI, GDD_SOS$diff_GAM_EVI_SQ, use="complete.obs"),
                   "diff_PEP_SQ" = cor(GDD_SOS$CD_PEP, GDD_SOS$diff_PEP_SQ, use="complete.obs")),
  "r2_elevation" = apply(GDD_SOS[c("diff_LOG_NDVI_SQ", "diff_LOG_EVI_SQ", "diff_GAM_NDVI_SQ", "diff_GAM_EVI_SQ", "diff_PEP_SQ")], 2, 
                         function(x) summary(lm(x ~ DEM, GDD_SOS))$r.squared),
  "r_elevation" = apply(GDD_SOS[c("diff_LOG_NDVI_SQ", "diff_LOG_EVI_SQ", "diff_GAM_NDVI_SQ", "diff_GAM_EVI_SQ", "diff_PEP_SQ")], 2, 
                        function(x) cor(x, GDD_SOS$DEM,use="complete.obs")),
  "r2_east_west" = apply(GDD_SOS[c("diff_LOG_NDVI_SQ", "diff_LOG_EVI_SQ", "diff_GAM_NDVI_SQ", "diff_GAM_EVI_SQ", "diff_PEP_SQ")], 2, 
                         function(x) summary(lm(x ~ X, GDD_SOS))$r.squared),
  "r_east_west" = apply(GDD_SOS[c("diff_LOG_NDVI_SQ", "diff_LOG_EVI_SQ", "diff_GAM_NDVI_SQ", "diff_GAM_EVI_SQ", "diff_PEP_SQ")], 2, 
                        function(x) cor(x, GDD_SOS$X,use="complete.obs")),
  "r2_urban" = apply(GDD_SOS[c("diff_LOG_NDVI_SQ", "diff_LOG_EVI_SQ", "diff_GAM_NDVI_SQ", "diff_GAM_EVI_SQ", "diff_PEP_SQ")], 2, 
                     function(x) summary(lm(x ~ X_sum, GDD_SOS))$r.squared),
  "r_urban" = apply(GDD_SOS[c("diff_LOG_NDVI_SQ", "diff_LOG_EVI_SQ", "diff_GAM_NDVI_SQ", "diff_GAM_EVI_SQ", "diff_PEP_SQ")], 2, 
                    function(x) cor(x, GDD_SOS$X_sum,use="complete.obs"))
)))

as.data.frame(t(data.frame(
  "Mean" = apply(GDD_SOS[c("GDD_LOG_NDVI", "GDD_LOG_EVI", "GDD_GAM_NDVI", "GDD_GAM_EVI", "GDD_PEP")], 2, mean, na.rm=TRUE),
  "Q0.05" = apply(GDD_SOS[c("GDD_LOG_NDVI", "GDD_LOG_EVI", "GDD_GAM_NDVI", "GDD_GAM_EVI","GDD_PEP")], 2,
                  function(x) quantile(x, na.rm=TRUE, .05)),
  "Q0.5" = apply(GDD_SOS[c("GDD_LOG_NDVI", "GDD_LOG_EVI", "GDD_GAM_NDVI", "GDD_GAM_EVI","GDD_PEP")], 2,
                 function(x) quantile(x, na.rm=TRUE, .5)),
  "Q0.95" = apply(GDD_SOS[c("GDD_LOG_NDVI", "GDD_LOG_EVI", "GDD_GAM_NDVI", "GDD_GAM_EVI","GDD_PEP")], 2,
                 function(x) quantile(x, na.rm=TRUE, .95)),
  "r_GDD_GO" = c(apply(GDD_SOS[c("GDD_LOG_NDVI", "GDD_LOG_EVI", "GDD_GAM_NDVI", "GDD_GAM_EVI")], 2, 
                    function(x) cor(x, GDD_SOS$GDD_PEP,use="complete.obs")),NA),
  "r_chilling" = c("GDD_LOG_NDVI" = cor(GDD_SOS$CD_LOG_NDVI, GDD_SOS$GDD_LOG_NDVI, use="complete.obs"),
                   "GDD_LOG_EVI" = cor(GDD_SOS$CD_LOG_EVI, GDD_SOS$GDD_LOG_EVI, use="complete.obs"),
                   "GDD_GAM_NDVI" = cor(GDD_SOS$CD_GAM_NDVI, GDD_SOS$GDD_GAM_NDVI, use="complete.obs"),
                   "GDD_GAM_EVI" = cor(GDD_SOS$CD_GAM_EVI, GDD_SOS$GDD_GAM_EVI, use="complete.obs"),
                   "GDD_PEP" = cor(GDD_SOS$CD_PEP, GDD_SOS$GDD_PEP, use="complete.obs")),
  "r_urban" = apply(GDD_SOS[c("GDD_LOG_NDVI", "GDD_LOG_EVI", "GDD_GAM_NDVI", "GDD_GAM_EVI", "GDD_PEP")], 2, 
                    function(x) cor(x, GDD_SOS$X_sum,use="complete.obs")),
  "r_east_west" = apply(GDD_SOS[c("GDD_LOG_NDVI", "GDD_LOG_EVI", "GDD_GAM_NDVI", "GDD_GAM_EVI", "GDD_PEP")], 2, 
                        function(x) cor(x, GDD_SOS$X,use="complete.obs")),
  "r_elevation" = apply(GDD_SOS[c("GDD_LOG_NDVI", "GDD_LOG_EVI", "GDD_GAM_NDVI", "GDD_GAM_EVI", "GDD_PEP")], 2, 
                        function(x) cor(x, GDD_SOS$DEM,use="complete.obs")) )))


# no. of sample fits for each station
sample_log_evi <- subset(results_evi, !is.na(LOG_EVI))
sample_log_evi <- as.data.frame(table(sample_log_evi$stat_id))

sample_log_ndvi <- subset(results_ndvi, !is.na(LOG_NDVI))
sample_log_ndvi <- as.data.frame(table(sample_log_ndvi$stat_id))

sample_gam_ndvi <- subset(results_ndvi, !is.na(GAM_NDVI))
sample_gam_ndvi <- as.data.frame(table(sample_gam_ndvi$stat_id))

sample_gam_evi <- subset(results_evi, !is.na(GAM_EVI))
sample_gam_evi <- as.data.frame(table(sample_gam_evi$stat_id))

sum(mean(sample_log_evi$Freq),
    mean(sample_log_ndvi$Freq),
    mean(sample_gam_ndvi$Freq),
    mean(sample_gam_evi$Freq))/4

# differences between indices (station)
mean_results <- merge(mean_evi[, c("stat_id","b4","sp","observations")], 
                      mean_ndvi[, c("stat_id","b4","sp")], by="stat_id")
colnames(mean_results) <- c("stat_id","LOG_EVI", "GAM_EVI","observations","LOG_NDVI","GAM_NDVI")
mean_results$GAM_diff <- mean_results$GAM_NDVI - mean_results$GAM_EVI
mean_results$LOG_diff <- mean_results$LOG_NDVI - mean_results$LOG_EVI

cor.test(mean_results$LOG_NDVI, mean_results$LOG_EVI, use="complete.obs")
cor.test(mean_results$GAM_NDVI, mean_results$GAM_EVI, use="complete.obs")











