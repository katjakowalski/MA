
## TT Model

quantile(GDD_SOS$TT,na.rm=TRUE, c(0.05,0.5,0.95))
# correlation 
cor.test(GDD_SOS$GAM_NDVI, GDD_SOS$TT, use="complete.obs")
cor.test(GDD_SOS$LOG_NDVI, GDD_SOS$TT, use="complete.obs")

cor.test(GDD_SOS$GAM_EVI, GDD_SOS$TT, use="complete.obs")
cor.test(GDD_SOS$LOG_EVI, GDD_SOS$TT, use="complete.obs")


# mean difference 
mean(GDD_SOS$diff_GAM_EVI_TT, na.rm = TRUE)
mean(GDD_SOS$diff_LOG_EVI_TT, na.rm = TRUE)

mean(GDD_SOS$diff_GAM_NDVI_TT, na.rm = TRUE)
mean(GDD_SOS$diff_LOG_NDVI_TT, na.rm = TRUE)


# correlation difference & east-west
cor.test(GDD_SOS$diff_GAM_EVI_TT, GDD_SOS$X, use="complete.obs")
cor.test(GDD_SOS$diff_LOG_EVI_TT, GDD_SOS$X, use="complete.obs")
cor.test(GDD_SOS$diff_LOG_NDVI_TT, GDD_SOS$X, use="complete.obs")
cor.test(GDD_SOS$diff_GAM_NDVI_TT, GDD_SOS$X, use="complete.obs")


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

#####################################################################################
# TT_GDD



#mean SOS PEP, quantiles
mean(GDD_SOS$PEP_SOS, na.rm=TRUE)
quantile(GDD_SOS$PEP_SOS, c(0.05,0.5,0.95), na.rm=TRUE)

# correlation SOS & PEP
cor.test(GDD_SOS$GAM_EVI, GDD_SOS$PEP_SOS)
cor.test(GDD_SOS$LOG_EVI, GDD_SOS$PEP_SOS)
cor.test(GDD_SOS$GAM_NDVI, GDD_SOS$PEP_SOS)
cor.test(GDD_SOS$LOG_NDVI, GDD_SOS$PEP_SOS)

cor.test(GDD_SOS$TT, GDD_SOS$PEP_SOS, use="complete.obs")

# mean GDD 
mean(GDD_SOS$GDD_GAM_EVI, na.rm=TRUE)
sd(GDD_SOS$GDD_GAM_EVI)
mean(GDD_SOS$GDD_LOG_EVI,na.rm=TRUE)
sd(GDD_SOS$GDD_LOG_EVI)
mean(GDD_SOS$GDD_GAM_NDVI,na.rm=TRUE)
sd(GDD_SOS$GDD_GAM_NDVI)
mean(GDD_SOS$GDD_LOG_NDVI,na.rm=TRUE)
sd(GDD_SOS$GDD_LOG_NDVI, na.rm=TRUE)
mean(GDD_PEP$GDD_PEP, na.rm=TRUE)


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



###############################################################################################################################
# F Model

# correlation
cor.test(GDD_SOS$GAM_EVI, GDD_SOS$SQ, use="complete.obs")
cor.test(GDD_SOS$LOG_EVI, GDD_SOS$SQ, use="complete.obs")

cor.test(GDD_SOS$GAM_NDVI, GDD_SOS$SQ, use="complete.obs")
cor.test(GDD_SOS$LOG_NDVI, GDD_SOS$SQ, use="complete.obs")

cor.test(GDD_SOS$PEP_SOS, GDD_SOS$SQ, use="complete.obs")


mean(GDD_SOS$diff_GAM_EVI_SQ, na.rm = TRUE)
mean(GDD_SOS$diff_LOG_EVI_SQ, na.rm = TRUE)
mean(GDD_SOS$diff_LOG_NDVI_SQ, na.rm = TRUE)
mean(GDD_SOS$diff_GAM_NDVI_SQ, na.rm = TRUE)

quantile(pheno_rs_cd$CD, na.rm=TRUE, c(.05, .50,  .95))
mean(pheno_rs$diff_TT_CD, na.rm=TRUE)

#####################################################################################################

#TT LSP cal 
cor.test(GDD_SOS$CD_GAM_EVI, GDD_SOS$diff_GAM_EVI_SQ, use="complete.obs")
cor.test(GDD_SOS$CD_LOG_EVI, GDD_SOS$diff_LOG_EVI_SQ)
cor.test(GDD_SOS$CD_GAM_NDVI, GDD_SOS$diff_GAM_NDVI_SQ)
cor.test(GDD_SOS$CD_LOG_NDVI, GDD_SOS$diff_LOG_NDVI_SQ)


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



# regression GDD SOS dependent on CD

GAM_EVI <- lm(diff_LOG_EVI_TT ~ CD_LOG_EVI, data=GDD_SOS)

ggplot(data=GDD_SOS)+
  geom_point(aes(x=diff_GAM_EVI_TT, CD_GAM_EVI))

summary(GAM_EVI)

GDD_SOS$CD_GAM_EVI
