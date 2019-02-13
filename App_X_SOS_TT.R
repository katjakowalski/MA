
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

lm1 <- lm(TT~GAM_EVI, data=GDD_SOS)

p1 <- ggplot(GDD_SOS,aes(x=GAM_EVI, y=TT))+
  geom_point(alpha=1/2)+
  geom_abline(intercept = 0, slope = 1, linetype="dashed",color="#383838", size=0.9)+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE, size=0.6, color="black")+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12))+
  scale_x_continuous(limits=c(95,160), breaks=seq(90,160,10))+
  scale_y_continuous(limits=c(95,160),breaks=seq(90,160,10))+
  labs( x= expression(paste("SOS ",GAM[EVI])), y= "SOS TT")+
  annotate("text", x=95, y=150, 
           label= paste0("r = ",round(cor(GDD_SOS$GAM_EVI,GDD_SOS$TT, use="complete.obs"),2)), 
           size=4, hjust=0)+
  annotate("text", x=95, y=160, hjust=0, 
           label = lm_eqn(x=GDD_SOS$GAM_EVI, y=GDD_SOS$TT, df=GDD_SOS), 
           parse = TRUE, size=4)+
  annotate("text", x= 95, y=155, hjust=0, size=4,
           label= paste0("RMSE ==",round(sqrt(mean((resid(lm1))^2, na.rm=TRUE)),2)),
           parse=TRUE)
