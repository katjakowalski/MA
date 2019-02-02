library(lubridate)
library(ggplot2)

###########################################################################################
# load DWD meteorological data 
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk")
tmk <- read.csv(file="TMK_MN004.txt", header=TRUE, sep=";")

tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))

tmk <- tmk %>%
  group_by(STATION_ID)%>%
  mutate(datum = as.Date(datum)) %>%
  complete(datum = seq.Date(min(datum), max(datum), by="day"))

tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)

tmk <- subset(tmk, tmk$datum >= "2016-09-01"  & tmk$datum <= "2017-07-15" )    

tmk$gap_fill <- rollapply(
  data    = tmk$WERT,
  width   = 4,
  FUN     = function(x) mean(x, na.rm=TRUE), 
  fill    = NA,
  align   = "center",
  partial=TRUE
)

tmk <- tmk %>%
  mutate(WERT = coalesce(WERT, gap_fill))

# remove all plots with NA
tmk <- tmk %>%
  group_by(STATION_ID) %>%
  filter(!any(is.nan(WERT))) %>%
  filter(length(STATION_ID) == 318)

colnames(tmk)[1] <- "stat_id"

# load PEP data
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/PEP/PEP725_Kowalski")
data_PEP <- read.csv(file="PEP725_Kowalski.csv", header=TRUE, sep=";")
PEP_stats <- read.csv(file="PEP_DWD_stat.csv", header=TRUE, sep=",")
PEP_SOS <- subset(data_PEP, data_PEP$phase_id==11)
colnames(PEP_SOS)[1] <- "PEP_ID"

PEP <- merge(PEP_stats, PEP_SOS[, c("day", "PEP_ID", "species")], by="PEP_ID", all=TRUE)
PEP <- PEP[!is.na(PEP$day), ]
PEP <- PEP[!is.na(PEP$DWD_ID), ]
PEP_SOS <- aggregate(day ~ DWD_ID, data=PEP, mean)

###########################################################################################

data_input <- merge(tmk, SOS_TT[, c("LOG_EVI", "GAM_EVI","LOG_NDVI","GAM_NDVI", "stat_id")], by="stat_id", all.y=TRUE)
data_LOG_NDVI <- data_input[!is.na(data_input$LOG_NDVI), ]

data_PEP <- merge(tmk, PEP_SOS, by.x="stat_id", by.y="DWD_ID", all.y=TRUE)


###########################################################################################

tt_GDD <- function(statid, 
                   t_day, 
                   year,
                   doy,
                   datum,
                   t_base=5,
                   c_min = 0,
                   c_max = 5,
                   doy_crit) {
  
  data = data.frame(statid, 
                    t_day,
                    year, 
                    datum,
                    doy,
                    doy_crit)
  
  SOS_LSP <- NULL
  
  for( i in unique(data$statid)){   # loop through plots 
   
    
    
    # subset forcing
    d = subset(data, data$statid == i)
    

    # SOS 
    doy_crit <- d$doy_crit[1]
    d = subset(d, d$doy <= doy_crit)
    
    # subset chilling
    date_crit <- as.Date(doy_crit, origin ="2017-01-01") -1
    d_c = subset(data, data$statid == i & data$datum >= "2016-09-01" & data$datum <= date_crit )
    
    
    forcing <- 0
    chilling <- 0 
    out <- NULL
    
    if(length(d$t_day) < 10 ){
    out <- data.frame("stat_id" = i, 
                      "GDD" = NA,
                      "CD" = NA)
    SOS_LSP <- rbind(SOS_LSP, out)
    next
    }
    # forcing
    for (x in d$doy) {                  # loop through days until SOS
      
      dat <- subset(d, d$doy == x)
      t <- dat$t_day
      
      if (t > t_base) {
        val = t - t_base
        forcing = forcing + val
      }
    }

    # count chilling 
    for(c in d_c$doy){
      
      dat_c <- subset(d_c, d_c$doy == c)
      t <- dat_c$t_day

      if (t >= c_min &
          t <= c_max) {
        chilling = chilling + 1
      }
    }
   
    out <- data.frame("stat_id" = i, 
                      "GDD" = forcing,
                      "CD" = chilling)
    SOS_LSP <- rbind(SOS_LSP, out)
  }
  return(SOS_LSP)
}

###########################################################################################

GDD_GAM_EVI <- tt_GDD(statid=data_input$stat_id, 
                   t_day=data_input$WERT,
                   year = data_input$year,
                   doy= data_input$doy,
                   datum = data_input$datum,
                   doy_crit = data_input$GAM_EVI)
colnames(GDD_GAM_EVI) <- c("stat_id", "GDD_GAM_EVI", "CD_GAM_EVI")

GDD_LOG_EVI <- tt_GDD(statid=data_input$stat_id, 
                      t_day=data_input$WERT,
                      year = data_input$year,
                      doy= data_input$doy,
                      datum = data_input$datum,
                      doy_crit = data_input$LOG_EVI)
colnames(GDD_LOG_EVI) <- c("stat_id", "GDD_LOG_EVI", "CD_LOG_EVI")

GDD_LOG_NDVI <- tt_GDD(statid=data_LOG_NDVI$stat_id, 
                      t_day=data_LOG_NDVI$WERT,
                      year = data_LOG_NDVI$year,
                      doy= data_LOG_NDVI$doy,
                      datum = data_LOG_NDVI$datum,
                      doy_crit = data_LOG_NDVI$LOG_NDVI)
colnames(GDD_LOG_NDVI) <- c("stat_id", "GDD_LOG_NDVI", "CD_LOG_NDVI")


GDD_GAM_NDVI <- tt_GDD(statid=data_input$stat_id, 
                      t_day=data_input$WERT,
                      year = data_input$year,
                      doy= data_input$doy,
                      datum = data_input$datum,
                      doy_crit = data_input$GAM_NDVI)
colnames(GDD_GAM_NDVI) <- c("stat_id", "GDD_GAM_NDVI", "CD_GAM_NDVI")


GDD_PEP <- tt_GDD(statid=data_PEP$stat_id, 
                      t_day=data_PEP$WERT,
                      year = data_PEP$year,
                      doy= data_PEP$doy,
                      datum = data_PEP$datum,
                      doy_crit = data_PEP$day)
colnames(GDD_PEP) <- c("stat_id", "GDD_PEP", "CD_PEP")

###########################################################################################

GDD_SOS <- merge(GDD_GAM_EVI, GDD_LOG_EVI, by="stat_id", all.x=TRUE)
GDD_SOS <- merge(GDD_SOS, GDD_GAM_NDVI, by="stat_id", all.x=TRUE)
GDD_SOS <- merge(GDD_SOS, GDD_LOG_NDVI, by="stat_id", all.x=TRUE)
GDD_SOS <- merge(GDD_SOS, GDD_PEP, by="stat_id", all.x=TRUE)

# add SOS estimates 
GDD_SOS <- merge(GDD_SOS, SOS_TT[, c("LOG_EVI", "GAM_EVI", "LOG_NDVI","GAM_NDVI", "stat_id","X","Y")], by="stat_id", all.x=TRUE)
GDD_SOS <- merge(GDD_SOS, PEP_SOS[, c("DWD_ID","day")], by.x="stat_id",by.y="DWD_ID", all.x=TRUE)
names(GDD_SOS)[names(GDD_SOS) == 'day'] <- 'PEP_SOS'

#add differences 
GDD_SOS <- merge(GDD_SOS, SOS_TT[, c("diff_GAM_EVI_TT","diff_GAM_NDVI_TT","diff_LOG_EVI_TT","diff_LOG_NDVI_TT","TT", "stat_id")], by="stat_id")

write.csv(GDD_SOS, file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/20190201_GDD_SOS.csv",row.names = FALSE )

GDD_PEP <- GDD_SOS[!is.na(GDD_SOS$GDD_PEP),]
write.csv(GDD_PEP, file="20190201_results_PEP.csv",row.names = FALSE )

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

# correlation SOS & PEP
cor.test(GDD_SOS$GAM_EVI, GDD_SOS$PEP_SOS)
cor.test(GDD_SOS$LOG_EVI, GDD_SOS$PEP_SOS)
cor.test(GDD_SOS$GAM_NDVI, GDD_SOS$PEP_SOS)
cor.test(GDD_SOS$LOG_NDVI, GDD_SOS$PEP_SOS)

cor.test(GDD_SOS$PEP_SOS, GDD_SOS$TT)

# correlation SOS & SQ
cor.test(GDD_SOS$GAM_EVI, GDD_SOS$SQ)

# mean CD
sum(mean(GDD_SOS$CD_GAM_EVI, na.rm=TRUE),
    mean(GDD_SOS$CD_LOG_EVI, na.rm=TRUE),
    mean(GDD_SOS$CD_GAM_NDVI, na.rm=TRUE),
    mean(GDD_SOS$CD_LOG_NDVI, na.rm=TRUE))/4

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

# plot histogram of all GDD estimates
pl_GDD_EVI <- melt(GDD_SOS[, c("GDD_GAM_EVI", "GDD_LOG_EVI", "GDD_PEP")])
pl_GDD_NDVI <- melt(GDD_SOS[, c("GDD_GAM_NDVI", "GDD_LOG_NDVI", "GDD_PEP")])
pl_GDD <- rbind(pl_GDD_EVI, pl_GDD_NDVI)

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190119_GDD_EVI_density.png", 
    width= 900, height=700, res=200 )
ggplot(data=pl_GDD_EVI)+
  geom_density(aes(x=value, fill=variable), adjust=0.6, alpha=0.8, position="identity")+
  labs(x="GDD")+
  scale_fill_manual(values=c("#1f78b4","grey30","grey70"),labels=c("GAM","LOG","PEP"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title = element_blank())
dev.off()

# SOS vs. PEP SOS 

p1 <- ggplot(GDD_SOS)+
  geom_point(aes(x=GAM_EVI, y=PEP_SOS))+
  geom_abline(aes(intercept=0, slope=1))+
  coord_equal()+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title = element_blank())+
  scale_x_continuous(limits=c(65,160))+
  scale_y_continuous(limits= c(80,140),breaks=seq(80,140,20))+
  labs(x=expression(GAM[EVI]), y="PEP")+
  annotate("text", x=150, y=83, label= "r=0.53", size=4.3) 

p2 <- ggplot(GDD_SOS)+
  geom_point(aes(x=LOG_EVI, y=PEP_SOS))+
  geom_abline(aes(intercept=0, slope=1))+
  coord_equal()+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title = element_blank())+
  scale_x_continuous(limits=c(65,160))+
  scale_y_continuous(limits= c(80,140),breaks=seq(80,140,20))+
  labs(x=expression(LOG[EVI]), y="PEP")+
  annotate("text", x=150, y=83, label= "r=0.45", size=4.3) 

p3 <- ggplot(GDD_SOS)+
  geom_point(aes(x=GAM_NDVI, y=PEP_SOS))+
  geom_abline(aes(intercept=0, slope=1))+
  coord_equal()+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title = element_blank())+
  scale_x_continuous(limits=c(65,160))+
  scale_y_continuous(limits= c(80,140),breaks=seq(80,140,20))+
  labs(x=expression(GAM[NDVI]), y="PEP")+
  annotate("text", x=150, y=83, label= "r=0.36", size=4.3) 

p4 <- ggplot(GDD_SOS)+
  geom_point(aes(x=LOG_NDVI, y=PEP_SOS))+
  geom_abline(aes(intercept=0, slope=1))+
  coord_equal()+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title = element_blank())+
  scale_x_continuous(limits=c(65,160))+
  scale_y_continuous(limits= c(80,140),breaks=seq(80,140,20))+
  labs(x=expression(LOG[NDVI]), y="PEP")+
  annotate("text", x=150, y=83, label= "r=0.32", size=4.3) 

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190126_PEP_LSP_4x.png", 
    width= 1200, height=800, res=200 )
grid.arrange(p1,p2,p3,p4, nrow=2)
dev.off()



plot_GDD <- GDD_SOS[, c("GDD_GAM_EVI","GDD_GAM_NDVI", "GDD_LOG_EVI", "GDD_LOG_NDVI","GDD_PEP")]
plot_GDD <- melt(plot_GDD)
plot_GDD$index[plot_GDD$variable == "GDD_LOG_EVI" |  plot_GDD$variable== "GDD_GAM_EVI"] <- "EVI"
plot_GDD$index[plot_GDD$variable == "GDD_LOG_NDVI" |  plot_GDD$variable== "GDD_GAM_NDVI"] <- "NDVI"
plot_GDD$variable <- as.character(plot_GDD$variable)
plot_GDD$variable[plot_GDD$variable == "GDD_LOG_EVI" |  plot_GDD$variable== "GDD_LOG_NDVI"] <- "LOG"
plot_GDD$variable[plot_GDD$variable == "GDD_GAM_EVI" |  plot_GDD$variable== "GDD_GAM_NDVI"] <- "GAM"
plot_GDD$variable[plot_GDD$variable == "GDD_PEP"] <- "PEP"


png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190127_boxplot_GDD.png", 
    width= 900, height=700, res=200 )
ggplot(plot_GDD, aes(y=value, x=variable, fill=index )) +
  geom_boxplot(varwidth=TRUE)+
  scale_fill_manual(values=c( "royalblue2", "grey45"),
                    labels = c("EVI", "NDVI", " "))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.title=element_blank(),
        panel.grid.major.x = element_blank())+
  labs(x="Model", y="GDD")
dev.off()

cor.test(GDD_SOS$GAM_EVI, GDD_SOS$PEP_SOS)
cor.test(GDD_SOS$LOG_EVI, GDD_SOS$PEP_SOS)

cor.test(GDD_SOS$GAM_NDVI, GDD_SOS$PEP_SOS)
cor.test(GDD_SOS$LOG_NDVI, GDD_SOS$PEP_SOS)

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

