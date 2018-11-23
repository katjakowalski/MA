

library(mgcv)
library(tidyverse)
library(ggplot2)


setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/spectral")

data <- read.csv(header=TRUE, sep=",", file= "data_clear.csv") 

############################################################

plotid <- 75706

  # 39012 not enough data
  
  #327119 problem with slope /1st value after base
  
  #36821

  #169424
  #337606

data_sub <- data[data$plotid== plotid & data$year == 2017,]
base_index <- mean(subset(data_sub, data_sub$doy <= 50)$evi)

##########################################################################
png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/ts_169424.png", 
    width= 1200, height=1000, res=200 )

ggplot(data_sub, aes(x = doy, y = evi)) +
  geom_point(aes(shape=sensor))+
  #geom_line(aes(x=doy, y=predict))+
  labs(x="DOY", y="EVI")+
  scale_x_continuous(labels=seq(0,350, 50), breaks= seq(0,350, 50))+
  theme(axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        text = element_text(size=16),
        legend.text=element_text(size=14)) 

#geom_line(aes(x = doy, y = predict))

dev.off()
##########################################################################



d_tr <- subset(data_sub, data_sub$doy >= 75 & data_sub$doy <=225)
transition <- with(d_tr, doy[evi == max(evi)]) + 20
data_sub <- subset(data_sub, data_sub$doy <= transition)

data_sub <- data_sub[data_sub$doy <= transition, ]

b4_start <- round(mean(data_sub[which(data_sub$evi > median(data_sub$evi)), "doy"]), 0)


if (is.nan(base_index)){
  base_index <- min(data_sub$evi)
}

data_sub <- data_sub[!data_sub$doy <= 50,]                              # delete all rows < doy 50
df_base <- data_sub[0,]                                   # create empty df with column names
df_base[c(1:50), ] <- rep(NA, ncol(df_base))       # fill 50 rows with NA 
df_base$evi <- base_index
df_base$doy <- seq(1,50,1)
data_sub <- rbind(data_sub, df_base)




## splines

fit <- gam(evi ~ s(doy), data = data_sub)


data_sub$predict <- predict(fit)

summary(fit)





newDF <- with(data_sub, data.frame(doy = seq(0, transition, 1)))  

B <- predict(fit,  newDF, type = "response", se.fit = TRUE)


eps <- 1e-7
X0 <- predict(fit, newDF, type = 'lpmatrix')

newDFeps_p <- newDF + eps

X1 <- predict(fit, newDFeps_p, type = 'lpmatrix')

Xp <- (X0 - X1) / eps

fd_d1 <- Xp %*% coef(fit)

which.min(fd_d1) 




png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/gam_169424.png", 
    width= 1200, height=1000, res=200 )
ggplot(data_sub) +
  geom_point(aes(x = doy, y = evi)) +
  geom_line(aes(x = doy, y = predict), size=0.8, color="red") +
  geom_line(data = data.frame(doy = 0:190, deriv1 = fd_d1),
            aes(x = doy, y = deriv1  *10 +0.2), color="blue")+
  #geom_point(aes(x=133, y=0.415), size=6, shape="x", color="blue")+
  geom_vline(xintercept=133, linetype="dotted")+
  scale_x_continuous(labels=seq(0,350, 50), breaks= seq(0,350, 50))+
  theme(axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        text = element_text(size=16),
        legend.text=element_text(size=14)) +
  labs(x="DOY", y="EVI")
dev.off()

## logistic

fit <- nls(evi ~ b1 + (b2 / (1 + exp(- b3 * (doy - b4)))),
           start = list(b1 = min(data_sub$evi), 
                        b2 = max(data_sub$evi), 
                        b3 = 0.1, 
                        b4 = b4_start),
           data = data_sub)

round(coef(fit), 2)
sum(resid(fit)^2)
data_sub$predict <- predict(fit)

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/log_169424.png", 
    width= 1200, height=1000, res=200 )
ggplot(data_sub) +
  geom_point(aes(x = doy, y = evi)) +
  geom_line(aes(x = doy, y = predict), color="red", size=0.8) +
  geom_point(aes(x=131.27, y=0.415), size=6, shape="x", color="blue")+
  geom_vline(xintercept=131.27, linetype="dotted")+
  #geom_line(data = data.frame(doy = 0:190, deriv1 = fd_d1),
  #aes(x = doy, y = deriv1 * 10), col = "red")+
  scale_x_continuous(labels=seq(0,350, 25), breaks= seq(0,350, 25))+
  theme(axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        text = element_text(size=16),
        legend.text=element_text(size=14)) +
  labs(x="DOY", y="EVI")
dev.off()


