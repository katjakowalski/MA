

lm_eqn <- function(x,y,df){
  m <- lm(y ~ x, df);
  eq <- substitute(y == b * x + a,
                   list(a = format(round(coef(m)[1],2)), 
                        b = format(round(coef(m)[2],2))))
  as.character(as.expression(eq));
}

rmse_eqn <- function(x,y,df){
  m <- lm(y ~ x, df);
  eq <- substitute("RMSE"~"="~RMSE,
                   list(RMSE = format(round(sqrt(mean((resid(m))^2, na.rm=TRUE)),2))))
  as.character(as.expression(eq));
}

lm1 <- lm(GDD_PEP~GDD_GAM_NDVI, data=GDD_SOS)

p1 <- ggplot(GDD_SOS,aes(x=GDD_GAM_NDVI, y=GDD_PEP))+
  geom_point(alpha=1/2)+
  geom_abline(intercept = 0, slope = 1, linetype="dashed",color="#383838", size=0.9)+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE, size=0.6, color="black")+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12))+
  scale_x_continuous(limits=c(25,550), breaks=seq(0,550,100))+
  scale_y_continuous(limits=c(25,350),breaks=seq(0,350,100))+
  labs( x= expression(paste("GDD ",GAM[NDVI])), y= "GDD ground observation")+
  annotate("text", x=25, y=280, 
           label= paste0("r = ",round(cor(GDD_SOS$GDD_GAM_NDVI,GDD_SOS$GDD_PEP, use="complete.obs"),2)), 
           size=4, hjust=0)+
  annotate("text", x=25, y=340, hjust=0, 
           label = lm_eqn(x=GDD_SOS$GDD_GAM_NDVI, y=GDD_SOS$GDD_PEP, df=GDD_SOS), 
           parse = TRUE, size=4)+
  annotate("text", x= 25, y=310, hjust=0, size=4,
           label= paste0("RMSE ==",round(sqrt(mean((resid(lm1))^2, na.rm=TRUE)),2)),
           parse=TRUE)

lm1 <- lm(GDD_PEP~GDD_GAM_EVI, data=GDD_SOS)

p2 <- ggplot(GDD_SOS, aes(x=GDD_GAM_EVI, y=GDD_PEP))+
  geom_point(alpha=1/2)+
  geom_abline(intercept = 0, slope = 1, linetype="dashed",color="#383838", size=0.9)+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE, size=0.6, color="black")+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_continuous(limits=c(25,550), breaks= seq(0,550,100))+
  scale_y_continuous(limits=c(25,350),breaks=seq(0,350,100))+
  labs( x= expression(paste("SOS ",GAM[EVI])), y= "GDD ground observation")+
  annotate("text", x=25, y=280, 
           label= paste0("r = ",round(cor(GDD_SOS$GDD_GAM_EVI,GDD_SOS$GDD_PEP, use="complete.obs"),2)), 
           size=4, hjust=0)+
  annotate("text", x=25, y=340, hjust=0, 
           label = lm_eqn(x=GDD_SOS$GDD_GAM_EVI, y=GDD_SOS$GDD_PEP, df=GDD_SOS), 
           parse = TRUE, size=4)+
  annotate("text", x= 25, y=310, hjust=0, size=4,
           label= paste0("RMSE ==",round(sqrt(mean((resid(lm1))^2, na.rm=TRUE)),2)),
           parse=TRUE)

lm1 <- lm(GDD_PEP~GDD_LOG_NDVI, data=GDD_SOS)

p3 <- ggplot(GDD_SOS, aes(x=GDD_LOG_NDVI, y=GDD_PEP))+
  geom_point(alpha=1/2)+
  geom_abline(intercept = 0, slope = 1, linetype="dashed",color="#383838", size=0.9)+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE, size=0.6, color="black")+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_continuous(limits=c(25,550), breaks= seq(0,550,100))+
  scale_y_continuous(limits=c(25,350),breaks=seq(0,350,100))+
  labs( x= expression(paste("GDD ",LOG[NDVI])), y= "GDD ground observation")+
  annotate("text", x=25, y=280, 
           label= paste0("r = ",round(cor(GDD_SOS$GDD_LOG_NDVI,GDD_SOS$GDD_PEP, use="complete.obs"),2)), 
           size=4, hjust=0)+
  annotate("text", x=25, y=340, hjust=0, 
           label = lm_eqn(x=GDD_SOS$GDD_LOG_NDVI, y=GDD_SOS$GDD_PEP, df=GDD_SOS), 
           parse = TRUE, size=4)+
  annotate("text", x= 25, y=310, hjust=0, size=4,
           label= paste0("RMSE ==",round(sqrt(mean((resid(lm1))^2, na.rm=TRUE)),2)),
           parse=TRUE)

lm1 <- lm(GDD_PEP~GDD_LOG_EVI, data=GDD_SOS)

p4 <- ggplot(GDD_SOS, aes(x=GDD_LOG_EVI, y=GDD_PEP))+
  geom_point(alpha=1/2)+
  geom_abline(intercept = 0, slope = 1, linetype="dashed",color="#383838", size=0.9)+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE, size=0.6, color="black")+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_continuous(limits=c(25,550), breaks= seq(0,550,100))+
  scale_y_continuous(limits=c(25,350),breaks=seq(0,350,100))+
  labs( x= expression(paste("GDD ",LOG[EVI])), y= "GDD ground observation")+
  annotate("text", x=25, y=280, 
           label= paste0("r = ",round(cor(GDD_SOS$GDD_LOG_EVI,GDD_SOS$GDD_PEP, use="complete.obs"),2)), 
           size=4, hjust=0)+
  annotate("text", x=25, y=340, hjust=0, 
           label = lm_eqn(x=GDD_SOS$GDD_LOG_EVI, y=GDD_SOS$GDD_PEP, df=GDD_SOS), 
           parse = TRUE, size=4)+
  annotate("text", x= 25, y=310, hjust=0, size=4,
           label= paste0("RMSE ==",round(sqrt(mean((resid(lm1))^2, na.rm=TRUE)),2)),
           parse=TRUE)


png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190204_PEP_GDD_x4.png", 
    width= 2480, height=1504, res=300 )
grid.arrange(p2, p4, p1, p3, nrow = 2)
dev.off()

#### SOS mean spring temp ####
lm1 <- lm(GAM_EVI ~ spring_mean_temp, data=GDD_SOS )

p1 <- ggplot(GDD_SOS, aes(y=GAM_EVI, x=spring_mean_temp))+
  geom_point(alpha=1/2)+
  #geom_abline(intercept = 0, slope = 1, linetype="dashed",color="#383838", size=0.9)+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE, size=0.6, color="black")+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  #scale_x_continuous(limits=c(2.5,12.5),breaks=seq(2.5,12.5,2.5))+
  #scale_y_continuous(limits=c(80,150),breaks= seq(0,150,20))+
  labs( x= "mean spring temperature [°C]", y= expression(paste("SOS ",GAM[EVI])))+
  annotate("text", y=85, x=2.5, 
           label= paste0("r = ",round(cor(GDD_SOS$GAM_EVI,GDD_SOS$spring_mean_temp, use="complete.obs"),2)), 
           size=4, hjust=0)+
  annotate("text", y=105, x=2.5, hjust=0, 
           label = lm_eqn(y=GDD_SOS$GAM_EVI, x=GDD_SOS$spring_mean_temp, df=GDD_SOS), 
           parse = TRUE, size=4)+
  annotate("text", y=95, x=2.5, hjust=0, size=4,
           label= paste0("RMSE ==",round(sqrt(mean((resid(lm1))^2, na.rm=TRUE)),2)),
           parse=TRUE)

lm1 <- lm(LOG_EVI ~ spring_mean_temp, data=GDD_SOS )

p2 <- ggplot(GDD_SOS, aes(y=LOG_EVI, x=spring_mean_temp))+
  geom_point(alpha=1/2)+
  geom_abline(intercept = 0, slope = 1, linetype="dashed",color="#383838", size=0.9)+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE, size=0.6, color="black")+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_continuous(limits=c(2.5,12.5),breaks=seq(2.5,12.5,2.5))+
  scale_y_continuous(limits=c(80,150),breaks= seq(0,150,20))+
  labs( x= "mean spring temperature [°C]", y= expression(paste("SOS ",LOG[EVI])))+
  annotate("text", y=85, x=2.5, 
           label= paste0("r = ",round(cor(GDD_SOS$LOG_EVI,GDD_SOS$spring_mean_temp, use="complete.obs"),2)), 
           size=4, hjust=0)+
  annotate("text", y=105, x=2.5, hjust=0, 
           label = lm_eqn(y=GDD_SOS$LOG_EVI, x=GDD_SOS$spring_mean_temp, df=GDD_SOS), 
           parse = TRUE, size=4)+
  annotate("text", y=95, x=2.5, hjust=0, size=4,
           label= paste0("RMSE ==",round(sqrt(mean((resid(lm1))^2, na.rm=TRUE)),2)),
           parse=TRUE)

lm1 <- lm(GAM_NDVI ~ spring_mean_temp, data=GDD_SOS )

p3 <- ggplot(GDD_SOS, aes(y=GAM_NDVI, x=spring_mean_temp))+
  geom_point(alpha=1/2)+
  geom_abline(intercept = 0, slope = 1, linetype="dashed",color="#383838", size=0.9)+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE, size=0.6, color="black")+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_continuous(limits=c(2.5,12.5),breaks=seq(2.5,12.5,2.5))+
  scale_y_continuous(limits=c(80,150),breaks= seq(0,150,20))+
  labs( x= "mean spring temperature [°C]", y= expression(paste("SOS ",GAM[NDVI])))+
  annotate("text", y=85, x=2.5, 
           label= paste0("r = ",round(cor(GDD_SOS$GAM_NDVI,GDD_SOS$spring_mean_temp, use="complete.obs"),2)), 
           size=4, hjust=0)+
  annotate("text", y=105, x=2.5, hjust=0, 
           label = lm_eqn(y=GDD_SOS$GAM_NDVI, x=GDD_SOS$spring_mean_temp, df=GDD_SOS), 
           parse = TRUE, size=4)+
  annotate("text", y=95, x=2.5, hjust=0, size=4,
           label= paste0("RMSE ==",round(sqrt(mean((resid(lm1))^2, na.rm=TRUE)),2)),
           parse=TRUE)


lm1 <- lm(LOG_NDVI ~ spring_mean_temp, data=GDD_SOS )

p4 <- ggplot(GDD_SOS, aes(y=LOG_NDVI, x=spring_mean_temp))+
  geom_point(alpha=1/2)+
  geom_abline(intercept = 0, slope = 1, linetype="dashed",color="#383838", size=0.9)+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE, size=0.6, color="black")+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_continuous(limits=c(2.5,12.5),breaks=seq(2.5,12.5,2.5))+
  scale_y_continuous(limits=c(80,150),breaks= seq(0,150,20))+
  labs( x= "mean spring temperature [°C]", y= expression(paste("SOS ",LOG[NDVI])))+
  annotate("text", y=85, x=2.5, 
           label= paste0("r = ",round(cor(GDD_SOS$LOG_NDVI,GDD_SOS$spring_mean_temp, use="complete.obs"),2)), 
           size=4, hjust=0)+
  annotate("text", y=105, x=2.5, hjust=0, 
           label = lm_eqn(y=GDD_SOS$LOG_NDVI, x=GDD_SOS$spring_mean_temp, df=GDD_SOS), 
           parse = TRUE, size=4)+
  annotate("text", y=95, x=2.5, hjust=0, size=4,
           label= paste0("RMSE ==",round(sqrt(mean((resid(lm1))^2, na.rm=TRUE)),2)),
           parse=TRUE)

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190208_T_spring_SOS_4x.png", 
    width= 2480, height=1504, res=300 )
grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()

