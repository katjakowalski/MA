
library(ggplot2)
library(reshape2)

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190107_EVI_TT_LOG.png", 
    width= 1200, height=1000, res=200 )
ggplot(data=pheno_rs)+
  geom_point(aes(x=TT, y=b4), alpha=1/4, color="darkblue")+
  scale_x_continuous(labels=seq(0,350, 20), 
                     breaks= seq(0,350, 20),
                     limits= c(90,160))+
  scale_y_continuous(labels=seq(0,350,20), 
                     breaks=seq(0,350,20),
                     limits= c(90,160))+
  labs(x="SOS (TT)", y="SOS (LOG)")+
  coord_equal()+
  geom_abline(intercept = 0, slope = 1)+
  theme(axis.text.x = element_text(size=16, color="black"),
        axis.text.y = element_text(size=16, color="black"),
        text = element_text(size=18),
        legend.text=element_text(size=16)) +
  ggtitle("EVI")
dev.off()

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190107_EVI_TT_GAM.png", 
    width= 1200, height=1000, res=200 )
ggplot(data=pheno_rs)+
  geom_point(aes(x=TT, y=sp), alpha=1/4, color="darkblue")+
  scale_x_continuous(labels=seq(0,350, 20), 
                     breaks= seq(0,350, 20),
                     limits= c(90,160))+
  scale_y_continuous(labels=seq(0,350,20), 
                     breaks=seq(0,350,20),
                     limits= c(90,160))+
  labs(x="SOS (TT)", y="SOS (GAM)")+
  coord_equal()+
  geom_abline(intercept = 0, slope = 1)+
  theme(axis.text.x = element_text(size=16, color="black"),
        axis.text.y = element_text(size=16, color="black"),
        text = element_text(size=18),
        legend.text=element_text(size=16)) +
  ggtitle("EVI")
dev.off()

# boxplots

f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

model_res <- pheno_rs[, c("b4","sp", "TT","CD")]
model_res <- melt(model_res)

model_res$variable <- as.character(model_res$variable)
model_res$variable[model_res$variable == "b4"] <- "LOG"
model_res$variable[model_res$variable == "sp"] <- "GAM"

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190107_quantiles.png", 
    width= 1200, height=1000, res=200 )
ggplot(model_res, aes(y=value, x=variable)) +
  stat_summary(fun.data = f, geom="boxplot", position=position_dodge())+
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20))+
  labs(x="Model", y="SOS")
dev.off()



png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190107_EVI_CD_GAM.png", 
    width= 1200, height=1000, res=200 )
ggplot(data=pheno_rs)+
  geom_point(aes(x=CD, y=sp), alpha=1/4, color="darkblue")+
  scale_x_continuous(labels=seq(0,350, 20), 
                     breaks= seq(0,350, 20),
                     limits= c(90,180))+
  scale_y_continuous(labels=seq(0,350,20), 
                     breaks=seq(0,350,20),
                     limits= c(90,180))+
  labs(x="SOS (CD)", y="SOS (GAM)")+
  coord_equal()+
  geom_abline(intercept = 0, slope = 1)+
  theme(axis.text.x = element_text(size=16, color="black"),
        axis.text.y = element_text(size=16, color="black"),
        text = element_text(size=18),
        legend.text=element_text(size=16)) +
  ggtitle("EVI")
dev.off()

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190107_EVI_CD_LOG.png", 
    width= 1200, height=1000, res=200 )
ggplot(data=pheno_rs)+
  geom_point(aes(x=CD, y=b4), alpha=1/4, color="darkblue")+
  scale_x_continuous(labels=seq(0,350, 20), 
                     breaks= seq(0,350, 20),
                     limits= c(90,170))+
  scale_y_continuous(labels=seq(0,350,20), 
                     breaks=seq(0,350,20),
                     limits= c(90,170))+
  labs(x="SOS (CD)", y="SOS (GAM)")+
  coord_equal()+
  geom_abline(intercept = 0, slope = 1)+
  theme(axis.text.x = element_text(size=16, color="black"),
        axis.text.y = element_text(size=16, color="black"),
        text = element_text(size=18),
        legend.text=element_text(size=16)) +
  ggtitle("EVI")
dev.off()
