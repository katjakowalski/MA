setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/PEP/PEP725_Kowalski")

data_PEP <- read.csv(file="PEP725_Kowalski.csv", header=TRUE, sep=";")
PEP_stats <- read.csv(file="PEP_DWD_stat.csv", header=TRUE, sep=",")

PEP_SOS <- subset(data_PEP, data_PEP$phase_id==11)
#PEP_SOS <- PEP_SOS[ PEP_SOS$genus %in% c("Fagus","Quercus"), ]

colnames(PEP_SOS)[1] <- "PEP_ID"

PEP <- merge(PEP_stats, PEP_SOS[, c("day", "PEP_ID", "species")], by="PEP_ID", all=TRUE)
PEP <- PEP[!is.na(PEP$day), ]
PEP <- PEP[!is.na(PEP$DWD_ID), ]

PEP_SOS <- aggregate(. ~ DWD_ID, data=PEP, mean)


setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results")
pheno_rs <- read.csv(file="20181211_mean_evi.csv", header=TRUE, sep=",")
pheno_rs <- read.csv(file="20181211_mean_ndvi.csv", header=TRUE, sep=",")

pheno_rs <- merge(pheno_rs, PEP_SOS[, c("day", "DWD_ID")], by.x="stat_id",by.y="DWD_ID")

cor.test(pheno_rs$b4, 
         pheno_rs$day, use="complete.obs")

ggplot(data=pheno_rs)+
  geom_point(aes(x=day, y=sp))+
  geom_abline(intercept = 0, slope = 1)+
  coord_equal()

ggplot(data=pheno_rs)+
  geom_histogram(aes(x=day), alpha=1/2, binwidth=1)+
  geom_histogram(aes(x=sp), binwidth=1, fill="red", alpha=1/3)

