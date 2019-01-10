

setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/pheno")
rotbuche <- read.csv(file="PH_Sofortmelder_Wildwachsende_Pflanze_Rotbuche_akt.txt",
         sep=";", header=TRUE)
rotbuche <- subset(rotbuche, rotbuche$Referenzjahr==2017 & rotbuche$Phase_id==4)
rotbuche <- select(rotbuche, Stations_id, Jultag)
colnames(rotbuche) <- c("stat_id", "DOY_dwd")
mean_evi <- merge(mean_evi, rotbuche, by="stat_id", all.x=TRUE)
plot(mean_evi$sp, mean_evi$DOY_dwd)
