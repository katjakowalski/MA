

library(mgcv)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(mgcViz)

setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/spectral")

data <- read.csv(header=TRUE, sep=",", file= "data_clear.csv") 

data_evi <- subset(data, data$evi < 1.1 & data$evi >= 0 & data$year == 2017)
data_ndvi <- subset(data, data$ndvi < 1.1 & data$ndvi >= 0 & data$year == 2017)

############################################################

plotid <-98312


  #76620
  #290507
  #155005
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
  #scale_x_continuous(labels=seq(0,350, 100), breaks= seq(0,350, 100))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) 


dev.off()


##########################################################################


d_tr <- subset(data_sub, data_sub$doy >= 75 & data_sub$doy <=225)
transition <- with(d_tr, doy[evi == max(evi)]) + 20

data_sub <- subset(data_sub, data_sub$doy <= transition)

b4_start <- round(mean(data_sub[which(data_sub$evi > median(data_sub$evi)), "doy"]), 0)

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



fit_ols <- nls(evi ~ b1 + (b2 / (1 + exp(- b3 * (doy - b4)))),
           start = list(b1 = min(data_sub$evi),
                        b2 = max(data_sub$evi),
                        b3 = 0.2,
                        b4 = b4_start),
           data = data_sub)


fit_spl_evi <- gam(evi~ s(doy, sp= 0.005), data = data_sub)
fit_spl_evi$coefficients
summary(fit_spl_evi)
fit_spl_evi$coefficients
####################################################################################################

k <- 8
df <- with(data_sub, data.frame(doy = seq(min(doy), max(doy))))
knots <- with(data_sub, list(doy = seq(min(doy), max(doy), length = k)))
sm <- smoothCon(s(doy, k = k, bs = "cr"), data = df, knots = knots)[[1]]$X
colnames(sm) <- levs <- paste0("F", seq_len(k))
basis <- gather(cbind(sm, df), Fun, Value, -doy)
basis <- transform(basis, Fun = factor(Fun, levels = levs))

sm2 <- smoothCon(s(doy, k = k, bs = "cr"), data = data_sub, knots = knots)[[1]]$X
beta <- coef(lm(evi ~ sm2 - 1, data = data_sub))
scbasis <- sweep(sm, 2L, beta, FUN = "*")
colnames(scbasis) <- levs <- paste0("F", seq_len(k))
fit <- cbind(df, fitted = rowSums(scbasis))
scbasis <- gather(cbind(scbasis, df), Fun, Value, -doy)
scbasis <- transform(scbasis, Fun = factor(Fun, levels = levs))

ylims <- range(basis$Value, scbasis$Value, data_sub$evi)

ggplot(basis, aes(x = doy, y = Value, group = Fun, colour = Fun)) +
  geom_path() +
  scale_x_continuous(breaks = knots$doy, labels = NULL, minor_breaks = NULL) +
  scale_y_continuous(limits = ylims) +
  scale_colour_discrete(name = "Basis Function") +
  theme(legend.position = "none") +
  geom_point(data = data_sub, mapping = aes(x = doy, y = evi), inherit.aes = FALSE, size = 2, colour = "grey70") +
  labs(y = "d15n_label", x = "Year CE (Knots)")

ggplot(scbasis, aes(x = doy, y = Value, group = Fun, colour = Fun)) +
  geom_path() +
  scale_x_continuous(breaks = knots$doy, labels = NULL, minor_breaks = NULL) +
  scale_y_continuous(limits = ylims) +
  scale_colour_discrete(name = "Basis Function") +
  theme(legend.position = "none") +
  geom_point(data = data_sub, mapping = aes(x = doy, y = evi), inherit.aes = FALSE, size = 2, colour = "grey70") +
  geom_line(data = fit, mapping = aes(x = doy, y = fitted), inherit.aes = FALSE,
            size = 0.75, colour = "black") +
  labs(y = "d15n_label", x = "Year CE (Knots)")

tp <- smoothCon(s(doy, k = k, bs = "tp"), data = df)[[1]]$X
colnames(tp) <- levs <- paste0("F", seq_len(k))
tpbasis <- gather(cbind(tp, df), Fun, Value, -doy)
tpbasis <- transform(tpbasis, Fun = factor(Fun, levels = levs))

ggplot(tpbasis, aes(x = doy, y = Value, group = Fun, colour = Fun)) +
  geom_path() +
  scale_colour_discrete(name = "Basis Function") +
  theme(legend.position = "none") +
  labs(y = "d15n_label", x = "Year CE")

####################################################################################################

data_sub$predict_gam <- as.numeric(predict(fit_spl_evi))
data_sub$predict_log <- as.numeric(predict(fit_ols))


newDF <- with(data_sub, data.frame(doy = seq(0, transition, 1)))  
B <- predict(fit_spl_evi,  newDF, type = "response", se.fit = TRUE)
eps <- 1e-7
X0 <- predict(fit_spl_evi, newDF, type = 'lpmatrix')
newDFeps_p <- newDF + eps
X1 <- predict(fit_spl_evi, newDFeps_p, type = 'lpmatrix')
Xp <- (X0 - X1) / eps
fd_d1 <- Xp %*% coef(fit_spl_evi)
which.min(fd_d1) 

data_sub_pl <- data_sub[, c("predict_gam", "predict_log")]
data_sub_pl <- melt(data_sub_pl)
data_sub_pl$doy <- rep(data_sub$doy,2)

df_deriv <- data.frame(doy = 0:189,value = (fd_d1*10*-1)+0.3, variable="deriv")

data_sub_pl <- rbind(data_sub_pl, df_deriv)





png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/gam_169424.png", 
    width= 1200, height=1000, res=200 )
ggplot(data= data_sub) +
  geom_point(aes(x = doy, y = evi, shape=sensor)) +
  geom_line( aes(x=doy, y= predict_evi), color="red", size=0.7)+
  geom_line(data = data.frame(doy = 0:207, deriv1 = fd_d1),
            aes(x = doy, y = (deriv1 *10 *-1)+0.3), color="black")+
  #geom_point(aes(x=133, y=0.425), size=3, color="red")+
  #geom_vline(xintercept=133, linetype="dotted")+
  theme_bw()+
  scale_x_continuous(labels=seq(0,350, 50), breaks= seq(0,350, 50))+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(x="DOY", y="EVI")

dev.off()

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190208_GAM_LOG.png", 
    width= 16, height=10,unit="cm", res=200 )
ggplot(data= data_sub_pl) +
  geom_point(data=data_sub, aes(x = doy, y = evi)) +
  geom_line( aes(x=doy, y= value, color=variable), size= 0.8) +
  #geom_line(data = data.frame(doy = 0:210, deriv1 = fd_d1),
    #aes(x = doy, y = (deriv1  *10 * -1) +0.3))+
  #geom_vline(xintercept=128, linetype="dashed")+
  #geom_vline(xintercept=125.0416, linetype="dotted")+
  scale_x_continuous(labels=seq(0,350, 50), breaks= seq(0,350, 50))+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  labs(x="DOY", y="EVI")+
  scale_color_manual(values=c("#1f78b4", "#d95f02","gray30"),
                     labels=c("GAM","LOG","GAM slope"),
                     name=" ")
dev.off()



