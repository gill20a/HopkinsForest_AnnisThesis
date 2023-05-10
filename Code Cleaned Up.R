setwd("C:/Users/maddi/Dropbox/Gill Lab/Aggregates/")
agg<-read.csv("Aggregate Fraction Data.csv")
soils<-read.csv("20220411HMF_Soil_Sampling_062821.csv")

setwd("C:/Users/maddi/Dropbox/Gill Lab/Aggregates/Roots/")
roots<-read.csv("Root Biomass Data 2.csv")

setwd("C:/Users/maddi/Dropbox/Gill Lab/Maddie Thesis/All 2022 CN Data")
bulkCN<-read.csv("June2022_BulkSoilCoreCN.csv")
fracCN<-read.csv("2022 CN Data MacroAggregate and Density Fraction organized.csv")
density<-read.csv("Density Frac Data.csv")

agg$Site<-c(rep("Lower", 24), rep("Upper", 24))
agg$color<-c(rep("forest green", 24), rep("purple", 24))

roots$Site<-c(rep("Lower", 24), rep("Upper", 24))
roots$ScaledFineRootMass_mg_gsoil<-(roots$FineRootMass*(roots$TotalMass/roots$PickedMass)*1000)/roots$TotalMass

density$Site<-c(rep("Lower", 24), rep("Upper", 24))
density$LightFraction<-density$LF.Mass/density$Total.Soil.Mass
density$DenseFraction<-density$DF.Mass/density$Total.Soil.Mass
density$MineralFraction<-density$MF.Mass/density$Total.Soil.Mass

bulkCN$Site<-c(rep("Lower", 24), rep("Upper", 24))
bulkCN$CNratio<-bulkCN$PercentC/bulkCN$PercentN

fracCN$Site<-c(rep("Lower", 24), rep("Upper", 24))
fracCN$Macro.CNratio<-fracCN$Macro.PercentC/fracCN$Macro.Percent.N
fracCN$Micro.CNratio<-fracCN$Micro.PercentC/fracCN$Micro.Percent.N
fracCN$SC.CNratio<-fracCN$S.C.PercentC/fracCN$S.C.Percent.N
fracCN$LF.CNratio<-fracCN$LF.Percent.C/fracCN$LF.Percent.N
fracCN$DF.CNratio<-fracCN$DF.Percent.C/fracCN$DF.Percent.N
fracCN$MF.CNratio<-fracCN$MF.Percent.C/fracCN$MF.Percent.N

# Agg Pool C (g agg C/ g dry soil)
fracCN$MacroPoolC<-(fracCN$Macro.PercentC/100)*agg$MacroFraction
fracCN$MicroPoolC<-(fracCN$Micro.PercentC/100)*agg$MicroFraction
fracCN$S.CPoolC<-(fracCN$S.C.PercentC/100)*agg$S.CFraction


# Density Pool C (g den C/ g macro soil)
fracCN$LFPoolC<-(fracCN$LF.Percent.C/100)*density$LightFraction
fracCN$DFPoolC<-(fracCN$DF.Percent.C/100)*density$DenseFraction
fracCN$MFPoolC<-(fracCN$MF.Percent.C/100)*density$MineralFraction

diff<-(fracCN$LFPoolC+fracCN$DFPoolC+fracCN$MFPoolC)/(fracCN$Macro.PercentC/100)
diff2<-(fracCN$MacroPoolC+fracCN$MicroPoolC+fracCN$S.CPoolC)/bulkCN$PoolCTotal

fracCN$MacroPoolPercentC<-fracCN$MacroPoolC/(bulkCN$PercentC/100)
fracCN$MicroPoolPercentC<-fracCN$MicroPoolC/(bulkCN$PercentC/100)
fracCN$SCPoolPercentC<-fracCN$S.CPoolC/(bulkCN$PercentC/100)

fracCN$LFPoolPercentC<-fracCN$LFPoolC/(fracCN$Macro.PercentC/100)
fracCN$DFPoolPercentC<-fracCN$DFPoolC/(fracCN$Macro.PercentC/100)
fracCN$MFPoolPercentC<-fracCN$MFPoolC/(fracCN$Macro.PercentC/100)

model<-lm(fracCN$LFPoolPercentC~fracCN$Site)
summary(model)


#Tables 1,2 - various means and standard errors
# table means and standard errors
library(Matrix)
macrofrac_mean<-round(tapply(agg$MacroFraction, agg$Site, mean, na.rm=T), 3)
macrofrac_se<-round(tapply(agg$MacroFraction, agg$Site, sd, na.rm=T)/
                      tapply(agg$MacroFraction, agg$Site, nnzero, na.counted=F), 3)
microfrac_mean<-round(tapply(agg$MicroFraction, agg$Site, mean, na.rm=T), 3)
microfrac_se<-round(tapply(agg$MicroFraction, agg$Site, sd, na.rm=T)/
                      tapply(agg$MicroFraction, agg$Site, nnzero, na.counted=F), 3)
S.Cfrac_mean<-round(tapply(agg$S.CFraction, agg$Site, mean, na.rm=T), 3)
S.Cfrac_se<- round(tapply(agg$S.CFraction, agg$Site, sd, na.rm=T)/
                     tapply(agg$S.CFraction, agg$Site, nnzero, na.counted=F), 3)

macropercentC_mean<-round(tapply(fracCN$Macro.PercentC, fracCN$Site, mean, na.rm=T), 3)
macropercentC_se<-round(tapply(fracCN$Macro.PercentC, fracCN$Site, sd, na.rm=T)/
                          tapply(fracCN$Macro.PercentC, fracCN$Site, nnzero, na.counted=F), 3)
micropercentC_mean<-round(tapply(fracCN$Micro.PercentC, fracCN$Site, mean, na.rm=T), 3)
micropercentC_se<-round(tapply(fracCN$Micro.PercentC, fracCN$Site, sd, na.rm=T)/
                          tapply(fracCN$Micro.PercentC, fracCN$Site, nnzero, na.counted=F), 3)
S.CpercentC_mean<-round(tapply(fracCN$S.C.PercentC, fracCN$Site, mean, na.rm=T), 3)
S.CpercentC_se<-round(tapply(fracCN$S.C.PercentC, fracCN$Site, sd, na.rm=T)/
                        tapply(fracCN$S.C.PercentC, fracCN$Site, nnzero, na.counted=F), 3)

macroCN_mean<-round(tapply(fracCN$Macro.CNratio, fracCN$Site, mean, na.rm=T), 3)
macroCN_se<-round(tapply(fracCN$Macro.CNratio, fracCN$Site, sd, na.rm=T)/
                    tapply(fracCN$Macro.CNratio, fracCN$Site, nnzero, na.counted=F), 3)
microCN_mean<-round(tapply(fracCN$Micro.CNratio, fracCN$Site, mean, na.rm=T), 3)
microCN_se<-round(tapply(fracCN$Micro.CNratio, fracCN$Site, sd, na.rm=T)/
                    tapply(fracCN$Micro.CNratio, fracCN$Site, nnzero, na.counted=F), 3)
SC.CN_mean<-round(tapply(fracCN$SC.CNratio, fracCN$Site, mean, na.rm=T), 3)
SC.CN_se<-round(tapply(fracCN$SC.CNratio, fracCN$Site, sd, na.rm=T)/
                  tapply(fracCN$SC.CNratio, fracCN$Site, nnzero, na.counted=F), 3)

macropoolC_mean<-round(tapply(fracCN$MacroPoolC, fracCN$Site, mean, na.rm=T), 3)
macropoolC_se<-round(tapply(fracCN$MacroPoolC, fracCN$Site, sd, na.rm=T)/
                       tapply(fracCN$MacroPoolC, fracCN$Site, nnzero, na.counted=F), 3)
micropoolC_mean<-round(tapply(fracCN$MicroPoolC, fracCN$Site, mean, na.rm=T), 3)
micropoolC_se<-round(tapply(fracCN$MicroPoolC, fracCN$Site, sd, na.rm=T)/
                       tapply(fracCN$MicroPoolC, fracCN$Site, nnzero, na.counted=F), 3)
S.CpoolC_mean<-round(tapply(fracCN$S.CPoolC, fracCN$Site, mean, na.rm=T), 3)
S.CpoolC_se<- round(tapply(fracCN$S.CPoolC, fracCN$Site, sd, na.rm=T)/
                      tapply(fracCN$S.CPoolC, fracCN$Site, nnzero, na.counted=F), 3)

roots_mean<-round(tapply(roots$ScaledFineRootMass_mg_gsoil, agg$Site, mean, na.rm=T), 3)
roots_se<-round(tapply(roots$ScaledFineRootMass_mg_gsoil, agg$Site, sd, na.rm=T)/
                  tapply(roots$ScaledFineRootMass_mg_gsoil, agg$Site, nnzero, na.counted=F), 3)

aggtable<-rbind(macrofrac_mean, microfrac_mean, S.Cfrac_mean, macropercentC_mean, micropercentC_mean, S.CpercentC_mean,
                macroCN_mean, microCN_mean, SC.CN_mean, roots_mean, macrofrac_se, microfrac_se, S.Cfrac_se, macropercentC_se, 
                micropercentC_se, S.CpercentC_se, macroCN_se, microCN_se, SC.CN_se, roots_se)
write.csv(aggtable, "aggtable.csv")


LFfrac_mean<-round(tapply(density$LightFraction, density$Site, mean, na.rm=T), 3)
LFfrac_se<-round(tapply(density$LightFraction, density$Site, sd, na.rm=T)/
                   tapply(density$LightFraction, density$Site, nnzero, na.counted=F), 3)
DFfrac_mean<-round(tapply(density$DenseFraction, density$Site, mean, na.rm=T), 3)
DFfrac_se<-round(tapply(density$DenseFraction, density$Site, sd, na.rm=T)/
                   tapply(density$DenseFraction, density$Site, nnzero, na.counted=F), 3)
MFfrac_mean<-round(tapply(density$MineralFraction, density$Site, mean, na.rm=T), 3)
MFfrac_se<-round(tapply(density$MineralFraction, density$Site, sd, na.rm=T)/
                   tapply(density$MineralFraction, density$Site, nnzero, na.counted=F), 3)

LFpercentC_mean<-round(tapply(fracCN$LF.Percent.C, fracCN$Site, mean, na.rm=T), 3)
LFpercentC_se<-round(tapply(fracCN$LF.Percent.C, fracCN$Site, sd, na.rm=T)/
                       tapply(fracCN$LF.Percent.C, fracCN$Site, nnzero, na.counted=F), 3)
DFpercentC_mean<-round(tapply(fracCN$DF.Percent.C, fracCN$Site, mean, na.rm=T), 3)
DFpercentC_se<-round(tapply(fracCN$DF.Percent.C, fracCN$Site, sd, na.rm=T)/
                       tapply(fracCN$DF.Percent.C, fracCN$Site, nnzero, na.counted=F), 3)
MFpercentC_mean<-round(tapply(fracCN$MF.Percent.C, fracCN$Site, mean, na.rm=T), 3)
MFpercentC_se<-round(tapply(fracCN$MF.Percent.C, fracCN$Site, sd, na.rm=T)/
                       tapply(fracCN$MF.Percent.C, fracCN$Site, nnzero, na.counted=F), 3)

LF.CN_mean<-round(tapply(fracCN$LF.CNratio, fracCN$Site, mean, na.rm=T), 3)
LF.CN_se<-round(tapply(fracCN$LF.CNratio, fracCN$Site, sd, na.rm=T)/
                  tapply(fracCN$LF.CNratio, fracCN$Site, nnzero, na.counted=F), 3)
DF.CN_mean<-round(tapply(fracCN$DF.CNratio, fracCN$Site, mean, na.rm=T), 3)
DF.CN_se<-round(tapply(fracCN$DF.CNratio, fracCN$Site, sd, na.rm=T)/
                  tapply(fracCN$DF.CNratio, fracCN$Site, nnzero, na.counted=F), 3)
MF.CN_mean<-round(tapply(fracCN$MF.CNratio, fracCN$Site, mean, na.rm=T), 3)
MF.CN_se<-round(tapply(fracCN$MF.CNratio, fracCN$Site, sd, na.rm=T)/
                  tapply(fracCN$MF.CNratio, fracCN$Site, nnzero, na.counted=F), 3)

densitytable<-rbind(LFfrac_mean, DFfrac_mean, MFfrac_mean, LFpercentC_mean, DFpercentC_mean,
                    MFpercentC_mean, LF.CN_mean, DF.CN_mean, MF.CN_mean, LFfrac_se, DFfrac_se,
                    MFfrac_se, LFpercentC_se, MFpercentC_se, DFpercentC_se, LF.CN_se, DF.CN_se, MF.CN_se)
write.csv(densitytable, "densitytable.csv")

BulkpercentC_mean<-round(tapply(bulkCN$PercentC, bulkCN$Site, mean, na.rm=T), 3)
BulkpercentC_se<-round(tapply(bulkCN$PercentC, bulkCN$Site, sd, na.rm=T)/
                         tapply(bulkCN$PercentC, bulkCN$Site, nnzero, na.counted=F), 3)
Bulk.CN_mean<-round(tapply(bulkCN$CNratio, bulkCN$Site, mean, na.rm=T), 3)
Bulk.CN_se<-round(tapply(bulkCN$CNratio, bulkCN$Site, sd, na.rm=T)/
                    tapply(bulkCN$CNratio, bulkCN$Site, nnzero, na.counted=F), 3)

macropoolC_mean<-round(tapply(fracCN$MacroPoolC, fracCN$Site, mean, na.rm=T), 3)
macropoolC_se<-round(tapply(fracCN$MacroPoolC, fracCN$Site, sd, na.rm=T)/
                       tapply(fracCN$MacroPoolC, fracCN$Site, nnzero, na.counted=F), 3)
micropoolC_mean<-round(tapply(fracCN$MicroPoolC, fracCN$Site, mean, na.rm=T), 3)
micropoolC_se<-round(tapply(fracCN$MicroPoolC, fracCN$Site, sd, na.rm=T)/
                       tapply(fracCN$MicroPoolC, fracCN$Site, nnzero, na.counted=F), 4)
SCpoolC_mean<-round(tapply(fracCN$S.CPoolC, fracCN$Site, mean, na.rm=T), 3)
SCpoolC_se<-round(tapply(fracCN$S.CPoolC, fracCN$Site, sd, na.rm=T)/
                    tapply(fracCN$S.CPoolC, fracCN$Site, nnzero, na.counted=F), 4)

LFpoolC_mean<-round(tapply(fracCN$LFPoolC, fracCN$Site, mean, na.rm=T), 3)
LFpoolC_se<-round(tapply(fracCN$LFPoolC, fracCN$Site, sd, na.rm=T)/
                    tapply(fracCN$LFPoolC, fracCN$Site, nnzero, na.counted=F), 3)
DFpoolC_mean<-round(tapply(fracCN$DFPoolC, fracCN$Site, mean, na.rm=T), 3)
DFpoolC_se<-round(tapply(fracCN$DFPoolC, fracCN$Site, sd, na.rm=T)/
                    tapply(fracCN$DFPoolC, fracCN$Site, nnzero, na.counted=F), 3)
MFpoolC_mean<-round(tapply(fracCN$MFPoolC, fracCN$Site, mean, na.rm=T), 3)
MFpoolC_se<-round(tapply(fracCN$MFPoolC, fracCN$Site, sd, na.rm=T)/
                    tapply(fracCN$MFPoolC, fracCN$Site, nnzero, na.counted=F), 3)

#Figure 3, agg frac stacked bar
library(Hmisc)
library(Matrix)
MacroPoolpercent_mean<-tapply(fracCN$MacroPoolPercentC, fracCN$Site, mean, na.rm=T)
MicroPoolpercent_mean<-tapply(fracCN$MicroPoolPercentC, fracCN$Site, mean, na.rm=T)
SCpoolpercent_mean<-tapply(fracCN$SCPoolPercentC, fracCN$Site, mean, na.rm=T)
AllFrac_mean4<-rbind(MacroPoolpercent_mean, MicroPoolpercent_mean, SCpoolpercent_mean)

micro_mean_sum<-MacroPoolpercent_mean+MicroPoolpercent_mean
SC_mean_sum<-MacroPoolpercent_mean+MicroPoolpercent_mean+SCpoolpercent_mean

MacroPoolpercent_se<-tapply(fracCN$MacroPoolPercentC, fracCN$Site, sd, na.rm=T)/
  tapply(fracCN$MacroPoolPercentC, fracCN$Site, nnzero, na.counted=F)
MicroPoolpercent_se<-tapply(fracCN$MicroPoolPercentC, fracCN$Site, sd, na.rm=T)/
  tapply(fracCN$MicroPoolPercentC, fracCN$Site, nnzero, na.counted=F)
SCpoolpercent_se<-tapply(fracCN$SCPoolPercentC, fracCN$Site, sd, na.rm=T)/
  tapply(fracCN$SCPoolPercentC, fracCN$Site, nnzero, na.counted=F)

upper_macro<-MacroPoolpercent_mean+MacroPoolpercent_se
lower_macro<-MacroPoolpercent_mean-MacroPoolpercent_se
upper_micro<-micro_mean_sum+MicroPoolpercent_se
lower_micro<-micro_mean_sum-MicroPoolpercent_se
upper_SC<-SC_mean_sum+SCpoolpercent_se
lower_SC<-SC_mean_sum-SCpoolpercent_se

#tiff(file = "bulkpool_stackedbar.tiff", width = 14, height = 15, units = "cm", res = 300)
par(mgp=c(2,0.8,0))
barx<-barplot(AllFrac_mean4, ylim=c(0, 1.19), xlab="Site", ylab="Proportion of Bulk Soil C Pool",
              col=c("forest green", "purple", "orange"))
arrows(barx, MacroPoolpercent_mean, barx, upper_macro, lwd=1.5, angle=90, length=0.1)
arrows(barx, MacroPoolpercent_mean, barx, lower_macro, lwd=1.5, angle=90, length=0.1)
arrows(barx, micro_mean_sum, barx, upper_micro, lwd=1.5, angle=90, length=0.1)
arrows(barx, micro_mean_sum, barx, lower_micro, lwd=1.5, angle=90, length=0.1)
arrows(barx, SC_mean_sum, barx, upper_SC, lwd=1.5, angle=90, length=0.1)
arrows(barx, SC_mean_sum, barx, lower_SC, lwd=1.5, angle=90, length=0.1)
box()
legend("topright", c("Macroaggregates", "Microaggregates", "Silt/Clay"), pch=16, col=c("forest green", "purple", "orange"), bty="n")


#Figure 4, macroagg %C and frac size vs. bulk %C
#tiff(file = "Macro_sidebyside.tiff", width = 17, height = 20, units = "cm", res = 300)
par(mfrow=c(2,2), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
plot(agg$MacroFraction, bulkCN$PercentC, pch=16, col=agg$color, xlab="Macroaggregate Fraction", ylab="Bulk Soil Percent C")
mtext("a", side=1, adj=0.95,line=-1.5, font=2)
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
model<-lm(bulkCN$PercentC~agg$MacroFraction)
summary(model)
abline(model, col="dark grey", lwd=2)
lower_agg = agg$Site == "Lower"
lower_bulk = bulkCN$Site == "Lower"
model_lower<-lm(bulkCN[lower_bulk, "PercentC"]~agg[lower_agg, "MacroFraction"])
upper_agg = agg$Site == "Upper"
upper_bulk = bulkCN$Site == "Upper"
model_upper<-lm(bulkCN[upper_bulk, "PercentC"]~agg[upper_agg, "MacroFraction"])
abline(model_lower, col="forest green", lty=2)
abline(model_upper, col="purple", lty=2)

plot(fracCN$Macro.PercentC, bulkCN$PercentC, pch=16, col=agg$color, xlab="Macroaggregate Percent C", ylab="Bulk Soil Percent C")
mtext("b", side=1, adj=0.95,line=-1.5, font=2)
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
model<-lm(bulkCN$PercentC~fracCN$Macro.PercentC)
summary(model)
abline(model, col="dark grey")

#Figure 5, macro frac vs macro pool C
#tiff(file = "Macro_mapoolC.tiff", width = 14, height = 15, units = "cm", res = 300)
par(mgp=c(2,0.8,0))
plot(agg$MacroFraction, fracCN$MacroPoolC, pch=16, col=agg$color, xlab="Macroaggregate Fraction", ylab="Macroaggregate C Pool")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
model<-lm(fracCN$MacroPoolC~agg$MacroFraction)
summary(model)
abline(model, col="dark grey")
lower_frac = fracCN$Site == "Lower"
lower_agg = agg$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "MacroPoolC"]~agg[lower_agg, "MacroFraction"])
upper_frac = fracCN$Site == "Upper"
upper_agg = agg$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "MacroPoolC"]~agg[upper_agg, "MacroFraction"])
abline(model_lower, col="forest green", lty=2)
abline(model_upper, col="purple", lty=2)
summary(model_lower)
summary(model_upper)

#Figure 6, earthworm rel abundance vs. macro %C, earthworm boxplot
#tiff(file = "earthworm_sidebyside.tiff", width = 17, height = 20, units = "cm", res = 300)
par(mfrow=c(2,2), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
boxplot(fracCN[lower_frac, "Macro.PercentC"]~fracCN[lower_frac, "Earthworm"], ylab="Macroaggregate Percent C", xlab="Presence of Earthworms within Lower Site",col= c("forestgreen", "green3"))
mtext("a", side=1, adj=0.95,line=-15, font=2)
model<-lm(fracCN[lower_frac, "Macro.PercentC"]~fracCN[lower_frac, "Earthworm"])
summary(model)

lower_frac = fracCN$Site == "Lower"
lower_agg = agg$Site == "Lower"
plot(fracCN[lower_frac, "Earthworm_RelAbundance"], fracCN[lower_frac, "Macro.PercentC"], pch=16, col=agg$color, xlab="Lower Site Earthworm Relative Abundance", ylab="Lower Site Macroaggregate Percent C")
mtext("b", side=1, adj=0.95,line=-15, font=2)
model_lower<-lm(fracCN[lower_frac, "Macro.PercentC"]~fracCN[lower_frac, "Earthworm_RelAbundance"])
abline(model_lower, col="dark grey")
summary(model_lower)

#Figure 7, fine root biomass vs. macroagg frac, %C
roots$ScaledFineRootMass_mg_gsoil<-(roots$FineRootMass*(roots$TotalMass/roots$PickedMass)*1000)/roots$TotalMass

#tiff(file = "Roots_sidebyside.tiff", width = 17, height = 20, units = "cm", res = 300)
par(mfrow=c(2,2), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
plot(roots$ScaledFineRootMass_mg_gsoil, agg$MacroFraction, pch=16, col=agg$color, xlab="", ylab="Macroaggregate Fraction")
mtext("a", side=1, adj=0.95,line=-15, font=2)
legend("bottomright", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("Fine Root Biomass", side=1, line=1.9, adj=0.5, cex=0.82)
mtext(expression(~(mg~fine~roots~g~dry~soil^-1)), side=1, line=2.92, adj=0.5, cex=0.78)
model<-lm(agg$MacroFraction~roots$ScaledFineRootMass_mg_gsoil)
summary(model)
abline(model, col="dark grey")

plot(roots$ScaledFineRootMass_mg_gsoil, fracCN$Macro.PercentC, pch=16, col=agg$color, xlab="", ylab="Macroaggregate Percent C")
mtext("b", side=1, adj=0.95,line=-15, font=2)
legend("bottomright", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("Fine Root Biomass", side=1, line=1.9, adj=0.5, cex=0.82)
mtext(expression(~(mg~fine~roots~g~dry~soil^-1)), side=1, line=2.92, adj=0.5, cex=0.78)
model<-lm(fracCN$Macro.PercentC~roots$ScaledFineRootMass_mg_gsoil)
summary(model)
abline(model, col="dark grey")

#Figure 8, microagg frac and %C vs bulk %C
#tiff(file = "micro_sidebyside.tiff", width = 17, height = 20, units = "cm", res = 300)
par(mfrow=c(2,2), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
plot(agg$MicroFraction, bulkCN$PercentC,pch=16, col=agg$color, xlab="Microaggregate Fraction", ylab="Bulk Soil Percent C")
mtext("a", side=1, adj=0.95,line=-1.5, font=2)
legend("topright", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
model<-lm(bulkCN$PercentC~agg$MicroFraction)
summary(model)
abline(model, col="dark grey")
lower_agg = agg$Site == "Lower"
lower_bulk = bulkCN$Site == "Lower"
model_lower<-lm(bulkCN[lower_bulk, "PercentC"]~agg[lower_agg, "MicroFraction"])
upper_agg = agg$Site == "Upper"
upper_bulk = bulkCN$Site == "Upper"
model_upper<-lm(bulkCN[upper_bulk, "PercentC"]~agg[upper_agg, "MicroFraction"])
abline(model_lower, col="forest green", lty=2)
abline(model_upper, col="purple", lty=2)
summary(model_lower)
summary(model_upper)

plot(fracCN$Micro.PercentC, bulkCN$PercentC, pch=16, col=agg$color, xlab="Microaggregate Percent C", ylab="Bulk Soil Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("b", side=1, adj=0.95,line=-1.5, font=2)
model<-lm(bulkCN$PercentC~fracCN$Micro.PercentC)
summary(model)
abline(model, col="dark grey")

#Figure 9, silt/clay size and %C vs bulk %C
#tiff(file = "SC_sidebyside.tiff", width = 17, height = 20, units = "cm", res = 300)
par(mfrow=c(2,2), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
plot(agg$S.CFraction, bulkCN$PercentC, pch=16, col=agg$color, xlab="Silt/Clay Fraction", ylab="Bulk Soil Percent C")
legend("topright", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("a", side=1, adj=0.05,line=-1.5, font=2)
model<-lm(bulkCN$PercentC~agg$S.CFraction)
summary(model)
abline(model, col="dark grey")
lower_agg = agg$Site == "Lower"
lower_bulk = bulkCN$Site == "Lower"
model_lower<-lm(bulkCN[lower_bulk, "PercentC"]~agg[lower_agg, "S.CFraction"])
upper_agg = agg$Site == "Upper"
upper_bulk = bulkCN$Site == "Upper"
model_upper<-lm(bulkCN[upper_bulk, "PercentC"]~agg[upper_agg, "S.CFraction"])
abline(model_lower, col="forest green", lty=2)
abline(model_upper, col="purple", lty=2)
summary(model_lower)
summary(model_upper)

plot(fracCN$S.C.PercentC, bulkCN$PercentC, pch=16, col=agg$color, xlab="Silt/Clay Percent C", ylab="Bulk Soil Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("b", side=1, adj=0.95,line=-1.5, font=2)
model<-lm(bulkCN$PercentC~fracCN$S.C.PercentC)
summary(model)
abline(model, col="dark grey")

#Figure 10, macro vs silt/clay
#tiff(file = "MapercentC_SCpercentC.tiff", width = 15, height = 15, units = "cm", res = 300)
plot(fracCN$Macro.PercentC, fracCN$S.C.PercentC, pch=16, col=agg$color, xlab="Macroaggregate Percent C", ylab="Silt/Clay Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "S.C.PercentC"]~fracCN[lower_frac, "Macro.PercentC"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "S.C.PercentC"]~fracCN[upper_frac, "Macro.PercentC"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

#Figure 11, agg C:N vs bulk %C
#tiff(file = "aggCN_bulkpercentC_sidebyside.tiff", width = 17, height = 17, units = "cm", res = 300)
par(mfrow=c(2,3), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
plot(fracCN$Macro.CNratio, bulkCN$PercentC, pch=16, cex.lab=1.2, col=agg$color, xlab="Macroaggregate C to N Ratio", ylab="Bulk Soil Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("a", side=1, adj=0.95,line=-1.5, font=2)
lower_bulk = bulkCN$Site == "Lower"
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(bulkCN[lower_bulk, "PercentC"]~fracCN[lower_frac, "Macro.CNratio"])
upper_bulk = bulkCN$Site == "Upper"
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(bulkCN[upper_bulk, "PercentC"]~fracCN[upper_frac, "Macro.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

plot(fracCN$Micro.CNratio, bulkCN$PercentC, pch=16, cex.lab=1.2, col=agg$color, xlab="Microaggregate C to N Ratio", ylab="Bulk Soil Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("b", side=1, adj=0.95,line=-1.5, font=2)
lower_bulk = bulkCN$Site == "Lower"
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(bulkCN[lower_bulk, "PercentC"]~fracCN[lower_frac, "Micro.CNratio"])
upper_bulk = bulkCN$Site == "Upper"
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(bulkCN[upper_bulk, "PercentC"]~fracCN[upper_frac, "Micro.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

plot(fracCN$SC.CNratio, bulkCN$PercentC, pch=16, cex.lab=1.2, col=agg$color, xlab="Silt/Clay C to N Ratio", ylab="Bulk Soil Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("c", side=1, adj=0.95,line=-1.5, font=2)
lower_bulk = bulkCN$Site == "Lower"
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(bulkCN[lower_bulk, "PercentC"]~fracCN[lower_frac, "SC.CNratio"])
upper_bulk = bulkCN$Site == "Upper"
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(bulkCN[upper_bulk, "PercentC"]~fracCN[upper_frac, "SC.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

#Figure 12, agg CN vs agg %C
#tiff(file = "aggCN_aggpercentC_sidebyside.tiff", width = 17, height = 17, units = "cm", res = 300)
par(mfrow=c(2,3), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
plot(fracCN$Macro.CNratio, fracCN$Macro.PercentC, pch=16, cex.lab=1.2, col=agg$color, xlab="Macroaggregate C to N Ratio", ylab="Macroaggregate Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("a", side=1, adj=0.95,line=-17, font=2)
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "Macro.PercentC"]~fracCN[lower_frac, "Macro.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "Macro.PercentC"]~fracCN[upper_frac, "Macro.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

plot(fracCN$Micro.CNratio, fracCN$Micro.PercentC, pch=16, cex.lab=1.2, col=agg$color, xlab="Microaggregate C to N Ratio", ylab="Microaggregate Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("b", side=1, adj=0.95,line=-17, font=2)
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "Micro.PercentC"]~fracCN[lower_frac, "Micro.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "Micro.PercentC"]~fracCN[upper_frac, "Micro.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

plot(fracCN$SC.CNratio, fracCN$S.C.PercentC, pch=16, cex.lab=1.2, col=agg$color, ylab="Silt/Clay Percent C", xlab="Silt/Clay C to N Ratio")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("c", side=1, adj=0.95,line=-17, font=2)
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "S.C.PercentC"]~fracCN[lower_frac, "SC.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "S.C.PercentC"]~fracCN[upper_frac, "SC.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

#Figure 13, CN vs nitrification
tiff(file = "CN_nitrification_combined.tiff", width = 17, height = 17, units = "cm", res = 300)
par(mfrow=c(2,2), mgp=c(2,0.8,0), mar=c(3,3,0.5,0.5))
plot(bulkCN$CNratio, fracCN$Nitrification, pch=16, col=agg$color, xlab="Bulk Soil C to N Ratio", ylab="Nitrification")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("a", side=1, adj=0.95,line=-16, font=2)
lower_bulk = bulkCN$Site == "Lower"
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "Nitrification"]~bulkCN[lower_bulk, "CNratio"])
upper_bulk = bulkCN$Site == "Upper"
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "Nitrification"]~bulkCN[upper_bulk, "CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

plot(fracCN$Macro.CNratio, fracCN$Nitrification, pch=16, col=agg$color, xlab="Macroaggregate C to N Ratio", ylab="Nitrification")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("b", side=1, adj=0.95,line=-16, font=2)
lower_bulk = bulkCN$Site == "Lower"
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "Nitrification"]~fracCN[lower_frac, "Macro.CNratio"])
upper_bulk = bulkCN$Site == "Upper"
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "Nitrification"]~fracCN[upper_frac, "Macro.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

plot(fracCN$Micro.CNratio, fracCN$Nitrification, pch=16, col=agg$color, xlab="Microaggregate C to N Ratio", ylab="Nitrification")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("c", side=1, adj=0.95,line=-16, font=2)
lower_bulk = bulkCN$Site == "Lower"
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "Nitrification"]~fracCN[lower_frac, "Micro.CNratio"])
upper_bulk = bulkCN$Site == "Upper"
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "Nitrification"]~fracCN[upper_frac, "Micro.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

plot(fracCN$SC.CNratio, fracCN$Nitrification, pch=16, col=agg$color, xlab="Silt/Clay C to N Ratio", ylab="Nitrification")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("d", side=1, adj=0.95,line=-16, font=2)
lower_bulk = bulkCN$Site == "Lower"
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "Nitrification"]~fracCN[lower_frac, "SC.CNratio"])
upper_bulk = bulkCN$Site == "Upper"
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "Nitrification"]~fracCN[upper_frac, "SC.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

#Figure 14, CN vs N mineralization
tiff(file = "CN_Nmineral.tiff", width = 17, height = 17, units = "cm", res = 300)
par(mfrow=c(2,2), mgp=c(2,0.8,0), mar=c(3,3,0.5,0.5))
plot(bulkCN$CNratio, fracCN$N.Mineralization, pch=16, col=agg$color, xlab="Bulk Soil C to N Ratio", ylab="Nitrogen Mineralization")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("a", side=1, adj=0.95,line=-16, font=2)
lower_bulk = bulkCN$Site == "Lower"
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "N.Mineralization"]~bulkCN[lower_bulk, "CNratio"])
upper_bulk = bulkCN$Site == "Upper"
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "N.Mineralization"]~bulkCN[upper_bulk, "CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

plot(fracCN$Macro.CNratio, fracCN$N.Mineralization, pch=16, col=agg$color, xlab="Macroaggregate C to N Ratio", ylab="Nitrogen Mineralization")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("b", side=1, adj=0.95,line=-16, font=2)
lower_bulk = bulkCN$Site == "Lower"
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "N.Mineralization"]~fracCN[lower_frac, "Macro.CNratio"])
upper_bulk = bulkCN$Site == "Upper"
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "N.Mineralization"]~fracCN[upper_frac, "Macro.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

plot(fracCN$Micro.CNratio, fracCN$N.Mineralization, pch=16, col=agg$color, xlab="Microaggregate C to N Ratio", ylab="Nitrogen Mineralization")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("c", side=1, adj=0.95,line=-16, font=2)
lower_bulk = bulkCN$Site == "Lower"
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "N.Mineralization"]~fracCN[lower_frac, "Micro.CNratio"])
upper_bulk = bulkCN$Site == "Upper"
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "N.Mineralization"]~fracCN[upper_frac, "Micro.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

plot(fracCN$SC.CNratio, fracCN$N.Mineralization, pch=16, col=agg$color, xlab="Silt/Clay C to N Ratio", ylab="Nitrogen Mineratlization")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("d", side=1, adj=0.95,line=-16, font=2)
lower_bulk = bulkCN$Site == "Lower"
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "N.Mineralization"]~fracCN[lower_frac, "SC.CNratio"])
upper_bulk = bulkCN$Site == "Upper"
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "N.Mineralization"]~fracCN[upper_frac, "SC.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

#Figure 15, ammonium
#tiff(file = "percentC_NH4.tiff", width = 14, height = 15, units = "cm", res = 300)
par(mgp=c(2,0.8,0))
ylabel<-expression(Initial~Ammonium~(µg~NH[4]^"+"~g~dry~soil^-1))
plot(bulkCN$PercentC, fracCN$NH4_Initial_g, pch=16, col=agg$color, xlab="Bulk Soil Percent C", ylab=ylabel)
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
model<-lm(fracCN$NH4_Initial_g~bulkCN$PercentC)
summary(model)
abline(model, col="dark grey")

#Figure 16, dense pool C stacked bar
library(Hmisc)
library(Matrix)
LFpoolpercent_mean<-tapply(fracCN$LFPoolPercentC, fracCN$Site, mean, na.rm=T)
DFpoolpercent_mean<-tapply(fracCN$DFPoolPercentC, fracCN$Site, mean, na.rm=T)
MFpoolpercent_mean<-tapply(fracCN$MFPoolPercentC, fracCN$Site, mean, na.rm=T)
AllFrac_mean3<-rbind(LFpoolpercent_mean, DFpoolpercent_mean, MFpoolpercent_mean)

DF_mean_sum<-LFpoolpercent_mean+DFpoolpercent_mean
MF_mean_sum<-LFpoolpercent_mean+DFpoolpercent_mean+MFpoolpercent_mean

LFpoolpercent_se<-tapply(fracCN$LFPoolPercentC, fracCN$Site, sd, na.rm=T)/
  tapply(fracCN$LFPoolPercentC, fracCN$Site, nnzero, na.counted=F)
DFpoolpercent_se<-tapply(fracCN$DFPoolPercentC, fracCN$Site, sd, na.rm=T)/
  tapply(fracCN$DFPoolPercentC, fracCN$Site, nnzero, na.counted=F)
MFpoolpercent_se<-tapply(fracCN$MFPoolPercentC, fracCN$Site, sd, na.rm=T)/
  tapply(fracCN$MFPoolPercentC, fracCN$Site, nnzero, na.counted=F)

upper_LF<-LFpoolpercent_mean+LFpoolpercent_se
lower_LF<-LFpoolpercent_mean-LFpoolpercent_se
upper_DF<-DF_mean_sum+DFpoolpercent_se
lower_DF<-DF_mean_sum-DFpoolpercent_se
upper_MF<-MF_mean_sum+MFpoolpercent_se
lower_MF<-MF_mean_sum-MFpoolpercent_se

#tiff(file = "densepool_stackedbar.tiff", width = 14, height = 15, units = "cm", res = 300)
par(mgp=c(2,0.8,0)) 
barx<-barplot(AllFrac_mean3, ylim=c(0, 1.15), xlab="Site", ylab="Proportion of Macroaggregate C Pool",
              col=c("forest green", "purple", "orange"), width=1)
arrows(barx, LFpoolpercent_mean, barx, upper_LF, lwd=1.5, angle=90, length=0.1)
arrows(barx, LFpoolpercent_mean, barx, lower_LF, lwd=1.5, angle=90, length=0.1)
arrows(barx, DF_mean_sum, barx, upper_DF, lwd=1.5, angle=90, length=0.1)
arrows(barx, DF_mean_sum, barx, lower_DF, lwd=1.5, angle=90, length=0.1)
arrows(barx, MF_mean_sum, barx, upper_MF, lwd=1.5, angle=90, length=0.1)
arrows(barx, MF_mean_sum, barx, lower_MF, lwd=1.5, angle=90, length=0.1)
box()
legend("topleft", c("Light Fraction", "Dense Fraction", "Mineral Fraction"), pch=16, col=c("forest green", "purple", "orange"), bty="n")


#Figure 17, dense boxplots
library(Hmisc)
library(Matrix)
LF_mean<-tapply(fracCN$LF.Percent.C, fracCN$Site, mean, na.rm=T)
DF_mean<-tapply(fracCN$DF.Percent.C, fracCN$Site, mean, na.rm=T)
MF_mean<-tapply(fracCN$MF.Percent.C, fracCN$Site, mean, na.rm=T)
AllFrac_mean<-rbind(LF_mean, DF_mean, MF_mean)

AllFrac_mean

LF_se<-tapply(fracCN$LF.Percent.C, fracCN$Site, sd, na.rm=T)/
  tapply(fracCN$LF.Percent.C, fracCN$Site, nnzero, na.counted=F)
DF_se<-tapply(fracCN$DF.Percent.C, fracCN$Site, sd, na.rm=T)/
  tapply(fracCN$DF.Percent.C, fracCN$Site, nnzero, na.counted=F)
MF_se<-tapply(fracCN$MF.Percent.C, fracCN$Site, sd, na.rm=T)/
  tapply(fracCN$MF.Percent.C, fracCN$Site, nnzero, na.counted=F)
AllFrac_se<-rbind(LF_se, DF_se, LF_se)
upperarrow<-AllFrac_mean+AllFrac_se
lowerarrow<-AllFrac_mean-AllFrac_se

LF_mean2<-tapply(density$LightFraction, density$Site, mean, na.rm=T)
DF_mean2<-tapply(density$DenseFraction, density$Site, mean, na.rm=T)
MF_mean2<-tapply(density$MineralFraction, density$Site, mean, na.rm=T)
AllFrac_mean2<-rbind(LF_mean2, DF_mean2, MF_mean2)

LF_se2<-tapply(density$LightFraction, density$Site, sd, na.rm=T)/
  tapply(density$LightFraction, density$Site, nnzero, na.counted=F)
DF_se2<-tapply(density$DenseFraction, density$Site, sd, na.rm=T)/
  tapply(density$DenseFraction, density$Site, nnzero, na.counted=F)
MF_se2<-tapply(density$MineralFraction, density$Site, sd, na.rm=T)/
  tapply(density$MineralFraction, density$Site, nnzero, na.counted=F)
AllFrac_se2<-rbind(LF_se2, DF_se2, LF_se2)
upperarrow2<-AllFrac_mean2+AllFrac_se2
lowerarrow2<-AllFrac_mean2-AllFrac_se2

#tiff(file = "densebar_sidebyside.tiff", width = 17, height = 20, units = "cm", res = 300)
par(mfrow=c(2,2), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
barx<-barplot(t(AllFrac_mean), beside=T, ylim=c(0, max(upperarrow+1)), xlab="Macroaggregate Density Fraction", ylab="Fraction Percent C",
              col=c("forest green", "purple"), xaxt="n")
arrows(barx, t(AllFrac_mean), barx,t(lowerarrow), lwd=1.5, angle=90, length=0.1)
arrows(barx, t(AllFrac_mean), barx,t(upperarrow), lwd=1.5, angle=90, length=0.1)
box()
axis(1, at=c(2,5,8),labels=c("Light", "Dense", "Mineral"), cex.axis=0.85)

legend("topright", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("a", side=1, adj=0.2,line=-15, font=2)
letters=c("", "*", "**")
xvals<-c(2,5,8)
text(xvals, c(0.72,4,5.9), pos=3, label=letters, cex=2.5)

barx<-barplot(t(AllFrac_mean2), beside=T, ylim=c(0, max(upperarrow2+0.08)), xlab="Macroaggregate Density Fraction", ylab="Fraction Size",
              col=c("forest green", "purple"), xaxt="n")
arrows(barx, t(AllFrac_mean2), barx,t(lowerarrow2), lwd=1.5, angle=90, length=0.1)
arrows(barx, t(AllFrac_mean2), barx,t(upperarrow2), lwd=1.5, angle=90, length=0.1)
box()
axis(1, at=c(2,5,8),labels=c("Light", "Dense", "Mineral"), cex.axis=0.85)

legend("topright", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("b", side=1, adj=0.05,line=-15, font=2)
letters=c("**", "", "")
xvals<-c(2,5,8)
text(xvals, c(0.24,4,5.9), pos=3, label=letters, cex=2.5)

#Figure 18, light frac size and %C vs macro %C
#tiff(file = "LF_sidebyside.tiff", width = 17, height = 20, units = "cm", res = 300)
par(mfrow=c(2,2), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
plot(density$LightFraction, fracCN$Macro.PercentC, pch=16, col=agg$color, xlab="Macroaggregate Light Fraction", ylab="Macroaggregate Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("a", side=1, adj=0.95,line=-15, font=2)
model<-lm(fracCN$Macro.PercentC~density$LightFraction)
summary(model)
abline(model, col="dark grey")

plot(fracCN$LF.Percent.C, fracCN$Macro.PercentC, pch=16, col=agg$color, xlab="Macroaggregate Light Fraction Percent C", ylab="Macroaggregate Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("b", side=1, adj=0.95,line=-15, font=2)
model<-lm(fracCN$Macro.PercentC~fracCN$LF.Percent.C)
summary(model)
abline(model, col="dark grey")

#Figure 19, macro frac vs. light frac
#tiff(file = "Macro_LF.tiff", width = 14, height = 15, units = "cm", res = 300)
par(mgp=c(2,0.8,0))
plot(agg$MacroFraction, density$LightFraction, pch=16, col=agg$color, xlab="Macroaggregate Fraction Size", ylab="Macroaggregate Light Fraction Size")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
lower_density = density$Site == "Lower"
lower_agg = agg$Site == "Lower"
model_lower<-lm(density[lower_density, "LightFraction"]~agg[lower_agg, "MacroFraction"])
upper_density = density$Site == "Upper"
upper_agg = agg$Site == "Upper"
model_upper<-lm(density[upper_density, "LightFraction"]~agg[upper_agg, "MacroFraction"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

#Figure 20, mineral frac size and %C vs macro %C
#tiff(file = "MF_sidebyside.tiff", width = 17, height = 20, units = "cm", res = 300)
par(mfrow=c(2,2), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
plot(density$MineralFraction, fracCN$Macro.PercentC, pch=16, col=agg$color, xlab="Macroaggregate Mineral Fraction Size", ylab="Macroaggregate Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("a", side=1, adj=0.95,line=-15, font=2)
model<-lm(fracCN$Macro.PercentC~density$MineralFraction)
summary(model)
abline(model, col="dark grey")

plot(fracCN$MF.Percent.C, fracCN$Macro.PercentC, pch=16, col=agg$color, xlab="Macroaggregate Mineral Fraction Percent C", ylab="Macroaggregate Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("b", side=1, adj=0.95,line=-15, font=2)
model<-lm(fracCN$Macro.PercentC~fracCN$MF.Percent.C)
summary(model)
abline(model, col="dark grey")

#Figure 21, light frac vs mineral frac %C
#tiff(file = "LF_MFpercentC.tiff", width = 14, height = 15, units = "cm", res = 300)
par(mgp=c(2,0.8,0))
plot(density$LightFraction, fracCN$MF.Percent.C, pch=16, col=agg$color, xlab="Macroaggregate Light Fraction Size", ylab="Macroaggregate Mineral Fraction Percent C")
legend("bottomright", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
model<-lm(fracCN$MF.Percent.C~density$LightFraction)
summary(model)
abline(model, col="dark grey")

#Figure 22, dense CN vs dense %C
#tiff(file = "densityCN_densitypercentC_combined.tiff", width = 17, height = 17, units = "cm", res = 300)
par(mfrow=c(2,3), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
plot(fracCN$LF.CNratio, fracCN$LF.Percent.C, pch=16, col=agg$color, xlab="Light Fraction C to N Ratio", ylab="Macroaggregate Light Fraction Percent C")
legend("topright", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("a", side=1, adj=0.95,line=-1.5, font=2)
model<-lm(fracCN$LF.Percent.C~fracCN$LF.CNratio)
summary(model)
abline(model, col="dark grey")
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "LF.Percent.C"]~fracCN[lower_frac, "LF.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "LF.Percent.C"]~fracCN[upper_frac, "LF.CNratio"])
abline(model_lower, col="forest green", lty=2)
abline(model_upper, col="purple", lty=2)
summary(model_lower)
summary(model_upper)

plot(fracCN$DF.CNratio, fracCN$DF.Percent.C, pch=16, col=agg$color, xlab="Dense Fraction C to N Ratio", ylab="Macroaggregate Dense Fraction Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("b", side=1, adj=0.95,line=-1.5, font=2)
model<-lm(fracCN$DF.Percent.C~fracCN$DF.CNratio)
summary(model)
abline(model, col="dark grey")
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "DF.Percent.C"]~fracCN[lower_frac, "DF.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "DF.Percent.C"]~fracCN[upper_frac, "DF.CNratio"])
abline(model_lower, col="forest green", lty=2)
abline(model_upper, col="purple", lty=2)
summary(model_lower)
summary(model_upper)

plot(fracCN$MF.CNratio, fracCN$MF.Percent.C, pch=16, col=agg$color, xlab="Mineral Fraction C to N Ratio", ylab="Macroaggregate Mineral Fraction Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("c", side=1, adj=0.95,line=-1.5, font=2)
model<-lm(fracCN$MF.Percent.C~fracCN$MF.CNratio)
summary(model)
abline(model, col="dark grey")
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "MF.Percent.C"]~fracCN[lower_frac, "MF.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "MF.Percent.C"]~fracCN[upper_frac, "MF.CNratio"])
abline(model_lower, col="forest green", lty=2)
abline(model_upper, col="purple", lty=2)
summary(model_lower)
summary(model_upper)

#Figure 23, dense CN vs macro %C
tiff(file = "densityCN_macropercentC_combined.tiff", width = 17, height = 17, units = "cm", res = 300)
par(mfrow=c(2,3), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
plot(fracCN$LF.CNratio, fracCN$Macro.PercentC, pch=16, col=agg$color, xlab="Light Fraction C to N Ratio", ylab="Macroaggregate Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("a", side=1, adj=0.95,line=-16.5, font=2)
model<-lm(fracCN$Macro.PercentC~fracCN$LF.CNratio)
summary(model)
abline(model, col="dark grey")
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "Macro.PercentC"]~fracCN[lower_frac, "LF.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "Macro.PercentC"]~fracCN[upper_frac, "LF.CNratio"])
abline(model_lower, col="forest green", lty=2)
abline(model_upper, col="purple", lty=2)
summary(model_lower)
summary(model_upper)

plot(fracCN$DF.CNratio, fracCN$Macro.PercentC, pch=16, col=agg$color, xlab="Dense Fraction C to N Ratio", ylab="Macroaggregate Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("b", side=1, adj=0.95,line=-16.5, font=2)
model<-lm(fracCN$Macro.PercentC~fracCN$DF.CNratio)
summary(model)
abline(model, col="dark grey")
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "Macro.PercentC"]~fracCN[lower_frac, "DF.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "Macro.PercentC"]~fracCN[upper_frac, "DF.CNratio"])
abline(model_lower, col="forest green", lty=2)
abline(model_upper, col="purple", lty=2)
summary(model_lower)
summary(model_upper)

plot(fracCN$MF.CNratio, fracCN$Macro.PercentC, pch=16, col=agg$color, xlab="Mineral Fraction C to N Ratio", ylab="Macroaggregate Percent C")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("c", side=1, adj=0.95,line=-16.5, font=2)
model<-lm(fracCN$Macro.PercentC~fracCN$MF.CNratio)
summary(model)
abline(model, col="dark grey")
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "Macro.PercentC"]~fracCN[lower_frac, "MF.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "Macro.PercentC"]~fracCN[upper_frac, "MF.CNratio"])
abline(model_lower, col="forest green", lty=2)
abline(model_upper, col="purple", lty=2)
summary(model_lower)
summary(model_upper)

#Figure 24, macro CN vs dense %C

tiff(file = "MacroCN_densitypercentC_combined.tiff", width = 17, height = 17, units = "cm", res = 300)
par(mfrow=c(2,3), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
plot(fracCN$Macro.CNratio, fracCN$LF.Percent.C, pch=16, col=agg$color, xlab="Macroaggregate C to N Ratio", ylab="Light Fraction Percent C")
legend("topright", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("a", side=1, adj=0.95,line=-1.5, font=2)
model<-lm(fracCN$LF.Percent.C~fracCN$Macro.CNratio)
summary(model)
abline(model, col="dark grey")
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "LF.Percent.C"]~fracCN[lower_frac, "Macro.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "LF.Percent.C"]~fracCN[upper_frac, "Macro.CNratio"])
abline(model_lower, col="forest green", lty=2)
abline(model_upper, col="purple", lty=2)
summary(model_lower)
summary(model_upper)

#tiff(file = "MacroCN_DFpercentC.tiff", width = 15, height = 15, units = "cm", res = 300)
plot(fracCN$Macro.CNratio, fracCN$DF.Percent.C, pch=16, col=agg$color, xlab="Macroaggregate C to N Ratio", ylab="Dense Fraction Percent C")
legend("topright", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("b", side=1, adj=0.93,line=-1.5, font=2)
model<-lm(fracCN$DF.Percent.C~fracCN$Macro.CNratio)
summary(model)
abline(model, col="dark grey")
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "DF.Percent.C"]~fracCN[lower_frac, "Macro.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "DF.Percent.C"]~fracCN[upper_frac, "Macro.CNratio"])
abline(model_lower, col="forest green", lty=2)
abline(model_upper, col="purple", lty=2)
summary(model_lower)
summary(model_upper)


#tiff(file = "MacroCN_MFpercentC.tiff", width = 15, height = 15, units = "cm", res = 300)
plot(fracCN$Macro.CNratio, fracCN$MF.Percent.C, pch=16, col=agg$color, xlab="Macroaggregate C to N Ratio", ylab="Mineral Fraction Percent C")
legend("topright", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("c", side=1, adj=0.95,line=-1.5, font=2)
model<-lm(fracCN$MF.Percent.C~fracCN$Macro.CNratio)
summary(model)
abline(model, col="dark grey")
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "MF.Percent.C"]~fracCN[lower_frac, "Macro.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "MF.Percent.C"]~fracCN[upper_frac, "Macro.CNratio"])
abline(model_lower, col="forest green", lty=2)
abline(model_upper, col="purple", lty=2)
summary(model_lower)
summary(model_upper)

#Figure 25, dense CN vs. nitrification
#tiff(file = "denseCN_nitrification_combined.tiff", width = 17, height = 17, units = "cm", res = 300)
par(mfrow=c(2,3), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
plot(fracCN$LF.CNratio, fracCN$Nitrification, pch=16, col=agg$color, xlab="Light Fraction C to N Ratio", ylab="Nitrification")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("a", side=1, adj=0.95,line=-16.5, font=2)
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "Nitrification"]~fracCN[lower_frac, "LF.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "Nitrification"]~fracCN[upper_frac, "LF.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

plot(fracCN$DF.CNratio, fracCN$Nitrification, pch=16, col=agg$color, xlab="Dense Fraction C to N Ratio", ylab="Nitrification")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("b", side=1, adj=0.95,line=-16.5, font=2)
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "Nitrification"]~fracCN[lower_frac, "DF.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "Nitrification"]~fracCN[upper_frac, "DF.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

plot(fracCN$MF.CNratio, fracCN$Nitrification, pch=16, col=agg$color, xlab="Mineral Fraction C to N Ratio", ylab="Nitrification")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("c", side=1, adj=0.95,line=-16.5, font=2)
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "Nitrification"]~fracCN[lower_frac, "MF.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "Nitrification"]~fracCN[upper_frac, "MF.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)


#Figure 26, dense Cn vs N mineralization
tiff(file = "denseCN_Nmineral_combined.tiff", width = 17, height = 17, units = "cm", res = 300)
par(mfrow=c(2,3), mgp=c(2,0.8,0), mar=c(4,3,4,0.5))
plot(fracCN$LF.CNratio, fracCN$N.Mineralization, pch=16, col=agg$color, xlab="Light Fraction C to N Ratio", ylab="Nitrogen Mineralization")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("a", side=1, adj=0.95,line=-16.5, font=2)
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "N.Mineralization"]~fracCN[lower_frac, "LF.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "N.Mineralization"]~fracCN[upper_frac, "LF.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

plot(fracCN$DF.CNratio, fracCN$N.Mineralization, pch=16, col=agg$color, xlab="Dense Fraction C to N Ratio", ylab="Nitrogen Mineralization")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("b", side=1, adj=0.95,line=-16.5, font=2)
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "N.Mineralization"]~fracCN[lower_frac, "DF.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "N.Mineralization"]~fracCN[upper_frac, "DF.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

plot(fracCN$MF.CNratio, fracCN$N.Mineralization, pch=16, col=agg$color, xlab="Mineral Fraction C to N Ratio", ylab="Nitrogen Mineralization")
legend("topleft", c("Lower", "Upper"), pch=16, col=c("forest green", "purple"), bty="n")
mtext("c", side=1, adj=0.95,line=-16.3, font=2)
lower_frac = fracCN$Site == "Lower"
model_lower<-lm(fracCN[lower_frac, "N.Mineralization"]~fracCN[lower_frac, "MF.CNratio"])
upper_frac = fracCN$Site == "Upper"
model_upper<-lm(fracCN[upper_frac, "N.Mineralization"]~fracCN[upper_frac, "MF.CNratio"])
abline(model_lower, col="forest green")
abline(model_upper, col="purple")
summary(model_lower)
summary(model_upper)

