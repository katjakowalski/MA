

library(mgcv)
library(tidyverse)
library(ggplot2)
library(reshape2)


setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/spectral")

data <- read.csv(header=TRUE, sep=",", file= "data_clear.csv") 

############################################################

plotid <- 554102
  #554102
 # 243724
  #567618
  
  #567612
  
  #4427

  # high NDVI late doy: 154407
  # 120004, NDVI problem

  # 39012 not enough data
  
  #327119 problem with slope /1st value after base
  
  #36821

  #169424
  #337606

data_sub <- data_evi[data_evi$plotid== plotid & data_evi$year == 2017,]
base_index_evi <- mean(subset(data_sub, data_sub$doy <= 50)$evi)
base_index_ndvi <- mean(subset(data_sub, data_sub$doy <= 50)$ndvi)

##########################################################################
png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/ts_169424.png", 
    width= 1200, height=1000, res=200 )

ggplot(data_sub, aes(x = doy, y = evi)) +
  geom_point(aes(shape=sensor))+
  #geom_line(aes(x=doy, y=predict))+
  labs(x="DOY", y="EVI")+
  scale_x_continuous(labels=seq(0,350, 100), breaks= seq(0,350, 100))+
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20),
        legend.text=element_text(size=18)) 


dev.off()


##########################################################################


d_tr <- subset(data_sub, data_sub$doy >= 75 & data_sub$doy <=225)
transition <- with(d_tr, doy[evi == max(evi)]) + 20

data_sub <- subset(data_sub, data_sub$doy <= transition)

#b4_start <- round(mean(data_sub[which(data_sub$evi > median(data_sub$evi)), "doy"]), 0)

#if (is.nan(base_index)){
#  base_index <- min(data_sub$evi)
#}


data_sub <- data_sub[!data_sub$doy <= 50,]                              # delete all rows < doy 50
df_base <- data_sub[0,]                                   # create empty df with column names
df_base[c(1:50), ] <- rep(NA, ncol(df_base))       # fill 50 rows with NA 
df_base$evi <- base_index_evi
df_base$ndvi <- base_index_ndvi
df_base$doy <- seq(1,50,1)
data_sub <- rbind(data_sub, df_base)



# fit_ols <- nls(evi ~ b1 + (b2 / (1 + exp(- b3 * (doy - b4)))),
#            start = list(b1 = min(data_sub$evi), 
#                         b2 = max(data_sub$evi), 
#                         b3 = 0.1, 
#                         b4 = b4_start),
#            data = data_sub)


fit_spl_evi <- gam(evi~ s(doy, bs="cs"), data = data_sub)

fit_spl_ndvi <- gam(evi~ s(doy, bs="tp"), data = data_sub)

data_sub$predict_ndvi <- as.numeric(predict(fit_spl_ndvi))
data_sub$predict_evi <- as.numeric(predict(fit_spl_evi))




newDF <- with(data_sub, data.frame(doy = seq(0, transition, 1)))  
B <- predict(fit_spl_evi,  newDF, type = "response", se.fit = TRUE)
eps <- 1e-7
X0 <- predict(fit_spl_evi, newDF, type = 'lpmatrix')
newDFeps_p <- newDF + eps
X1 <- predict(fit_spl_evi, newDFeps_p, type = 'lpmatrix')
Xp <- (X0 - X1) / eps
fd_d1 <- Xp %*% coef(fit_spl_evi)
which.min(fd_d1) 

data_sub_pl <- data_sub[, c("predict_evi","predict_ndvi")]
data_sub_pl$predict_evi <- data_sub_pl$predict_evi
data_sub_pl <- melt(data_sub_pl)
data_sub_pl$doy <- rep(data_sub$doy,2)


png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/gam_log_554102.png", 
    width= 1200, height=1000, res=200 )
ggplot(data= data_sub_pl) +
  #geom_point(data=data_sub, aes(x = doy, y = evi)) +
  geom_line( aes(x=doy, y= value, color=variable), size=0.8) +
  #geom_line(data = data.frame(doy = 0:190, deriv1 = fd_d1),
            #aes(x = doy, y = deriv1  *10 +0.2), color="blue")+
  #geom_point(aes(x=116, y=0.66), size=3, color="black")+
  #geom_point(aes(x=129, y=0.59), size=3, color="royalblue2")+
  #geom_vline(xintercept=116, linetype="dotted")+
  #geom_vline(xintercept=129, linetype="dotted")+
  scale_x_continuous(labels=seq(0,350, 50), breaks= seq(0,350, 50))+
  scale_color_manual(values=c("royalblue2","black"),
                     name="Index",
                     labels=c("EVI", "NDVI"))+
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20),
        legend.text=element_text(size=18))+
  labs(x="DOY", y="VI")
dev.off()

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/gam_169424.png", 
    width= 1200, height=1000, res=200 )
ggplot(data= data_sub) +
  geom_point(aes(x = doy, y = ndvi)) +
  geom_line( aes(x=doy, y= predict_ndvi), color="red", size=0.8) +
  geom_line(data = data.frame(doy = 0:190, deriv1 = fd_d1),
            aes(x = doy, y = (deriv1  *10 *-1)+0.3), color="black")+
  geom_point(aes(x=133, y=0.425), size=3, color="red")+
  geom_vline(xintercept=133, linetype="dotted")+
  scale_x_continuous(labels=seq(0,350, 50), breaks= seq(0,350, 50))+
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20),
        legend.text=element_text(size=18)) +
  labs(x="DOY", y="EVI")
dev.off()

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/gam_169424.png", 
    width= 1200, height=1000, res=200 )
ggplot(data= data_sub_pl) +
  geom_point(data=data_sub, aes(x = doy, y = evi)) +
  #geom_line( aes(x=doy, y= value, color=variable), size= 0.7) +
  geom_line(data = data.frame(doy = 0:245, deriv1 = fd_d1),
    aes(x = doy, y = deriv1  *10 +0.2), color="blue")+
  #geom_vline(xintercept=133, linetype="dotted")+
  #geom_vline(xintercept=131, linetype="dotted")+
  scale_x_continuous(labels=seq(0,350, 50), breaks= seq(0,350, 50))+
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20),
        legend.text=element_text(size=14)) +
  labs(x="DOY", y="EVI")+
  scale_color_manual(values=c("red","blue"),
                     labels=c("GAM","LOG"),
                     name="Model")
dev.off()


ggplot(data=results_px)+
  geom_point(aes(x=GAM_EVI, y=GAM_NDVI))

ggplot(data=results_px)+
  geom_point(aes(x=LOG_EVI, y=LOG_NDVI))
