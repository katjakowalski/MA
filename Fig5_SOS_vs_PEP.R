

# scatterplot GDD SOS vs. GDD PEP

lm_eqn <- function(x,y,df){
  m <- lm(y ~ x, df);
  eq <- substitute(y == b * x + a * ","* ~~"RMSE"~"="~RMSE ,#*","~~italic(r)^2~"="~r2,
                   list(a = format(coef(m)[1], digits = 5),
                        b = format(coef(m)[2], digits = 2),
                        RMSE = format(sqrt(mean((y - x)^2, na.rm=TRUE))/100, digits = 2)))
  as.character(as.expression(eq));
}

p1 <- ggplot(GDD_SOS,aes(x=GDD_GAM_NDVI, y=GDD_PEP))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1.1, color="darkgrey")+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE, color="black")+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12))+
  scale_x_continuous(limits=c(25,550), breaks=seq(0,550,100))+
  scale_y_continuous(limits=c(25,350),breaks=seq(0,350,50))+
  labs( x=expression(GAM[NDVI]), y= "PEP")+
  annotate("text", x=300, y=40, label= "r = 0.48", size=3.5, hjust=0)+
  annotate("text", x = 300, y = 55, hjust=0,
           label = lm_eqn(x=GDD_SOS$GDD_GAM_NDVI, y=GDD_SOS$GDD_PEP, df=GDD_SOS),
           parse = TRUE, size=3.5)



p2 <- ggplot(GDD_SOS)+
  geom_point(aes(x=GDD_GAM_EVI, y=GDD_PEP))+
  geom_abline(intercept = 0, slope = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_continuous(limits=c(0,550), breaks= seq(0,550,100))+
  scale_y_continuous(limits=c(25,350),breaks=seq(0,350,50))+
  labs( x=expression(GAM[EVI]), y= "PEP")+
  annotate("text", x=300, y=40, label= "r = 0.48", size=3.5, hjust=0)+
  annotate("text", x = 300, y = 55, hjust=0,
           label = lm_eqn(x=GDD_SOS$GDD_GAM_EVI, y=GDD_SOS$GDD_PEP, df=GDD_SOS),
           parse = TRUE, size=3.5)


p3 <- ggplot(GDD_SOS)+
  geom_point(aes(x=GDD_LOG_NDVI, y=GDD_PEP))+
  geom_abline(intercept = 0, slope = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_continuous(limits=c(0,550), breaks= seq(0,550,100))+
  scale_y_continuous(limits=c(25,350),breaks=seq(0,350,50))+
  labs( x=expression(LOG[NDVI]), y= "PEP")+
  annotate("text", x=300, y=40, label= "r = 0.48", size=3.5, hjust=0)+
  annotate("text", x = 300, y = 55, hjust=0,
           label = lm_eqn(x=GDD_SOS$GDD_LOG_NDVI, y=GDD_SOS$GDD_PEP, df=GDD_SOS),
           parse = TRUE, size=3.5)


p4 <- ggplot(GDD_SOS)+
  geom_point(aes(x=GDD_LOG_EVI, y=GDD_PEP))+
  geom_abline(intercept = 0, slope = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_continuous(limits=c(0,550), breaks= seq(0,550,100))+
  scale_y_continuous(limits=c(25,350),breaks=seq(0,350,50))+
  labs( x=expression(LOG[EVI]), y= "PEP")+
  annotate("text", x=300, y=40, label= "r = 0.48", size=3.5, hjust=0)+
  annotate("text", x = 300, y = 55, hjust=0,
           label = lm_eqn(x=GDD_SOS$GDD_LOG_EVI, y=GDD_SOS$GDD_PEP, df=GDD_SOS),
           parse = TRUE, size=3.5)


png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190202_PEP_GDD_x4.png",
    width= 1200, height=800, res=200 )
grid.arrange(p2, p1, p4, p3, nrow = 2)
dev.off()
