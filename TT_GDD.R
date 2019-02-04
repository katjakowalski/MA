
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk")
tmk <- read.csv(file="TMK_MN004.txt", header=TRUE, sep=";")

tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))

tmk <- tmk %>%
  group_by(STATION_ID)%>%
  mutate(datum = as.Date(datum)) %>%
  complete(datum = seq.Date(min(datum), max(datum), by="day"))

tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)

tmk <- subset(tmk, tmk$year == 2017 & tmk$doy <= 196)

# rolling mean 
tmk$gap_fill <- rollapply(
  data    = tmk$WERT,
  width   = 4,
  FUN     = function(x) mean(x, na.rm=TRUE), 
  fill    = NA,
  align   = "center",
  partial=TRUE
)

# fills gaps 
tmk <- tmk %>%
  mutate(WERT = coalesce(WERT, gap_fill))

# remove plots with data gaps and shorter time series 
tmk <- tmk %>%
  group_by(STATION_ID) %>%
  filter(!any(is.nan(WERT))) %>%
  filter(length(STATION_ID) == 196)

colnames(tmk)[1] <- "stat_id"

# load PEP data
setwd( "\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/PEP/PEP725_Kowalski")
data_PEP <- read.csv(file = "PEP725_Kowalski.csv", header = TRUE, sep = ";")
PEP_stats <- read.csv(file = "PEP_DWD_stat.csv", header = TRUE, sep = ",")
PEP_SOS <- subset(data_PEP, data_PEP$phase_id == 11)
colnames(PEP_SOS)[1] <- "PEP_ID"

PEP <- merge(PEP_stats, PEP_SOS[, c("day", "PEP_ID", "species")], by = "PEP_ID", all =TRUE)
PEP <- PEP[!is.na(PEP$day),]
PEP <- PEP[!is.na(PEP$DWD_ID),]
PEP_SOS <- aggregate(day ~ DWD_ID, data = PEP, mean)

data_input <- merge(tmk, SOS_TT[, c("LOG_EVI", "GAM_EVI","LOG_NDVI","GAM_NDVI", "stat_id")], by="stat_id", all.y=TRUE)
data_LOG_NDVI <- data_input[!is.na(data_input$LOG_NDVI), ]

data_PEP <- merge(tmk, PEP_SOS, by.x="stat_id", by.y="DWD_ID", all.y=TRUE)


tt_GDD <- function(statid, 
                   t_day, 
                   year,
                   doy,
                   datum,
                   t_base=5,
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
    
    forcing <- 0
    out <- NULL
    
    if(length(d$t_day) < 10 ){
      out <- data.frame("stat_id" = i, 
                        "GDD" = NA)
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
    
    out <- data.frame("stat_id" = i, 
                      "GDD" = forcing)
    SOS_LSP <- rbind(SOS_LSP, out)
  }
  return(SOS_LSP)
}




# # scatterplot GDD SOS vs. GDD PEP
# 
# lm_eqn <- function(x,y,df){
#   m <- lm(y ~ x, df);
#   eq <- substitute(y == b * x + a * ","* ~~"RMSE"~"="~RMSE ,#*","~~italic(r)^2~"="~r2, 
#                    list(a = format(coef(m)[1], digits = 5), 
#                         b = format(coef(m)[2], digits = 2), 
#                         RMSE = format(sqrt(mean((y - x)^2, na.rm=TRUE))/100, digits = 2)))
#   as.character(as.expression(eq));                 
# }
# 
# p1 <- ggplot(GDD_SOS,aes(x=GDD_GAM_NDVI, y=GDD_PEP))+
#   geom_point()+
#   geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1.1, color="darkgrey")+
#   theme_bw()+
#   geom_smooth(method="lm", se=FALSE, color="black")+
#   theme(axis.text.x = element_text(size=12, color="black"),
#         axis.text.y = element_text(size=12, color="black"),
#         text = element_text(size=12),
#         legend.text=element_text(size=12))+
#   scale_x_continuous(limits=c(25,550), breaks=seq(0,550,100))+
#   scale_y_continuous(limits=c(25,350),breaks=seq(0,350,50))+
#   labs( x=expression(GAM[NDVI]), y= "PEP")+
#   annotate("text", x=300, y=40, label= "r = 0.48", size=3.5, hjust=0)+
#   annotate("text", x = 300, y = 55, hjust=0, 
#            label = lm_eqn(x=GDD_SOS$GDD_GAM_NDVI, y=GDD_SOS$GDD_PEP, df=GDD_SOS), 
#            parse = TRUE, size=3.5)
# 
# 
# 
# p2 <- ggplot(GDD_SOS)+
#   geom_point(aes(x=GDD_GAM_EVI, y=GDD_PEP))+
#   geom_abline(intercept = 0, slope = 1)+
#   theme_bw()+
#   theme(axis.text.x = element_text(size=12, color="black"),
#         axis.text.y = element_text(size=12, color="black"),
#         text = element_text(size=12),
#         legend.text=element_text(size=12)) +
#   scale_x_continuous(limits=c(0,550), breaks= seq(0,550,100))+
#   scale_y_continuous(limits=c(25,350),breaks=seq(0,350,50))+
#   labs( x=expression(GAM[EVI]), y= "PEP")+
#   annotate("text", x=300, y=40, label= "r = 0.48", size=3.5, hjust=0)+
#   annotate("text", x = 300, y = 55, hjust=0, 
#            label = lm_eqn(x=GDD_SOS$GDD_GAM_EVI, y=GDD_SOS$GDD_PEP, df=GDD_SOS), 
#            parse = TRUE, size=3.5)
# 
# 
# p3 <- ggplot(GDD_SOS)+
#   geom_point(aes(x=GDD_LOG_NDVI, y=GDD_PEP))+
#   geom_abline(intercept = 0, slope = 1)+
#   theme_bw()+
#   theme(axis.text.x = element_text(size=12, color="black"),
#         axis.text.y = element_text(size=12, color="black"),
#         text = element_text(size=12),
#         legend.text=element_text(size=12)) +
#   scale_x_continuous(limits=c(0,550), breaks= seq(0,550,100))+
#   scale_y_continuous(limits=c(25,350),breaks=seq(0,350,50))+
#   labs( x=expression(LOG[NDVI]), y= "PEP")+
#   annotate("text", x=300, y=40, label= "r = 0.48", size=3.5, hjust=0)+
#   annotate("text", x = 300, y = 55, hjust=0, 
#            label = lm_eqn(x=GDD_SOS$GDD_LOG_NDVI, y=GDD_SOS$GDD_PEP, df=GDD_SOS), 
#            parse = TRUE, size=3.5)
# 
# 
# p4 <- ggplot(GDD_SOS)+
#   geom_point(aes(x=GDD_LOG_EVI, y=GDD_PEP))+
#   geom_abline(intercept = 0, slope = 1)+
#   theme_bw()+
#   theme(axis.text.x = element_text(size=12, color="black"),
#         axis.text.y = element_text(size=12, color="black"),
#         text = element_text(size=12),
#         legend.text=element_text(size=12)) +
#   scale_x_continuous(limits=c(0,550), breaks= seq(0,550,100))+
#   scale_y_continuous(limits=c(25,350),breaks=seq(0,350,50))+
#   labs( x=expression(LOG[EVI]), y= "PEP")+
#   annotate("text", x=300, y=40, label= "r = 0.48", size=3.5, hjust=0)+
#   annotate("text", x = 300, y = 55, hjust=0, 
#            label = lm_eqn(x=GDD_SOS$GDD_LOG_EVI, y=GDD_SOS$GDD_PEP, df=GDD_SOS), 
#            parse = TRUE, size=3.5)
# 
# 
# png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190202_PEP_GDD_x4.png", 
#     width= 1200, height=800, res=200 )
# grid.arrange(p2, p1, p4, p3, nrow = 2)
# dev.off()
# 
# # plot histogram of all GDD estimates
# pl_GDD_EVI <- melt(GDD_SOS[, c("GDD_GAM_EVI", "GDD_LOG_EVI", "GDD_PEP")])
# pl_GDD_NDVI <- melt(GDD_SOS[, c("GDD_GAM_NDVI", "GDD_LOG_NDVI", "GDD_PEP")])
# pl_GDD <- rbind(pl_GDD_EVI, pl_GDD_NDVI)
# 
# png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190119_GDD_EVI_density.png", 
#     width= 900, height=700, res=200 )
# ggplot(data=pl_GDD_EVI)+
#   geom_density(aes(x=value, fill=variable), adjust=0.6, alpha=0.8, position="identity")+
#   labs(x="GDD")+
#   scale_fill_manual(values=c("#1f78b4","grey30","grey70"),labels=c("GAM","LOG","PEP"))+
#   theme_bw()+
#   theme(axis.text.x = element_text(size=12, color="black"),
#         axis.text.y = element_text(size=12, color="black"),
#         text = element_text(size=12),
#         legend.text=element_text(size=12),
#         legend.title = element_blank())
# dev.off()
# 
# # SOS vs. PEP SOS 
# 
# p1 <- ggplot(GDD_SOS)+
#   geom_point(aes(x=GAM_EVI, y=PEP_SOS))+
#   geom_abline(aes(intercept=0, slope=1))+
#   coord_equal()+
#   theme_bw()+
#   theme(axis.text.x = element_text(size=12, color="black"),
#         axis.text.y = element_text(size=12, color="black"),
#         text = element_text(size=12),
#         legend.text=element_text(size=12),
#         legend.title = element_blank())+
#   scale_x_continuous(limits=c(65,160))+
#   scale_y_continuous(limits= c(80,140),breaks=seq(80,140,20))+
#   labs(x=expression(GAM[EVI]), y="PEP")+
#   annotate("text", x=150, y=83, label= "r=0.53", size=4.3) 
# 
# p2 <- ggplot(GDD_SOS)+
#   geom_point(aes(x=LOG_EVI, y=PEP_SOS))+
#   geom_abline(aes(intercept=0, slope=1))+
#   coord_equal()+
#   theme_bw()+
#   theme(axis.text.x = element_text(size=12, color="black"),
#         axis.text.y = element_text(size=12, color="black"),
#         text = element_text(size=12),
#         legend.text=element_text(size=12),
#         legend.title = element_blank())+
#   scale_x_continuous(limits=c(65,160))+
#   scale_y_continuous(limits= c(80,140),breaks=seq(80,140,20))+
#   labs(x=expression(LOG[EVI]), y="PEP")+
#   annotate("text", x=150, y=83, label= "r=0.45", size=4.3) 
# 
# p3 <- ggplot(GDD_SOS)+
#   geom_point(aes(x=GAM_NDVI, y=PEP_SOS))+
#   geom_abline(aes(intercept=0, slope=1))+
#   coord_equal()+
#   theme_bw()+
#   theme(axis.text.x = element_text(size=12, color="black"),
#         axis.text.y = element_text(size=12, color="black"),
#         text = element_text(size=12),
#         legend.text=element_text(size=12),
#         legend.title = element_blank())+
#   scale_x_continuous(limits=c(65,160))+
#   scale_y_continuous(limits= c(80,140),breaks=seq(80,140,20))+
#   labs(x=expression(GAM[NDVI]), y="PEP")+
#   annotate("text", x=150, y=83, label= "r=0.36", size=4.3) 
# 
# p4 <- ggplot(GDD_SOS)+
#   geom_point(aes(x=LOG_NDVI, y=PEP_SOS))+
#   geom_abline(aes(intercept=0, slope=1))+
#   coord_equal()+
#   theme_bw()+
#   theme(axis.text.x = element_text(size=12, color="black"),
#         axis.text.y = element_text(size=12, color="black"),
#         text = element_text(size=12),
#         legend.text=element_text(size=12),
#         legend.title = element_blank())+
#   scale_x_continuous(limits=c(65,160))+
#   scale_y_continuous(limits= c(80,140),breaks=seq(80,140,20))+
#   labs(x=expression(LOG[NDVI]), y="PEP")+
#   annotate("text", x=150, y=83, label= "r=0.32", size=4.3) 
# 
# png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190126_PEP_LSP_4x.png", 
#     width= 1200, height=800, res=200 )
# grid.arrange(p1,p2,p3,p4, nrow=2)
# dev.off()
# 
# 
# 
# plot_GDD <- GDD_SOS[, c("GDD_GAM_EVI","GDD_GAM_NDVI", "GDD_LOG_EVI", "GDD_LOG_NDVI","GDD_PEP")]
# plot_GDD <- melt(plot_GDD)
# plot_GDD$index[plot_GDD$variable == "GDD_LOG_EVI" |  plot_GDD$variable== "GDD_GAM_EVI"] <- "EVI"
# plot_GDD$index[plot_GDD$variable == "GDD_LOG_NDVI" |  plot_GDD$variable== "GDD_GAM_NDVI"] <- "NDVI"
# plot_GDD$variable <- as.character(plot_GDD$variable)
# plot_GDD$variable[plot_GDD$variable == "GDD_LOG_EVI" |  plot_GDD$variable== "GDD_LOG_NDVI"] <- "LOG"
# plot_GDD$variable[plot_GDD$variable == "GDD_GAM_EVI" |  plot_GDD$variable== "GDD_GAM_NDVI"] <- "GAM"
# plot_GDD$variable[plot_GDD$variable == "GDD_PEP"] <- "PEP"
# 
# 
# png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190127_boxplot_GDD.png", 
#     width= 900, height=700, res=200 )
# ggplot(plot_GDD, aes(y=value, x=variable, fill=index )) +
#   geom_boxplot(varwidth=TRUE)+
#   scale_fill_manual(values=c( "royalblue2", "grey45"),
#                     labels = c("EVI", "NDVI", " "))+
#   theme_bw()+
#   theme(axis.text.x = element_text(size=12, color="black"),
#         axis.text.y = element_text(size=12, color="black"),
#         text = element_text(size=12),
#         legend.title=element_blank(),
#         panel.grid.major.x = element_blank())+
#   labs(x="Model", y="GDD")
# dev.off()
# 


