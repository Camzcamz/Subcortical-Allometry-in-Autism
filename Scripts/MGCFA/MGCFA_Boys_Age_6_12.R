library(lavaan)
library(semTools)
library(dplyr)
#### Function for Parameters ####
Parameters_loading <- function (x) {parameterestimates(x, standardize = T) %>% filter(op == "=~")}
Parameters_intercept <- function (x) {parameterestimates(x, standardize = T) %>% filter(op == "~1")}

#####################################
Abide_LMEM_log_Clean$DX_GROUP <- factor(Abide_LMEM_log_Clean$DX_GROUP, levels = c("Control", "ASD"))

Boys_Age_6_12 = Abide_LMEM_log_Clean%>%
  filter(AGE_AT_SCAN < 12) %>%
  filter(SEX == "Male") 
save(Boys_Age_6_12, file = "data/Boys_Age_6_12.Rdata")

ASD_Boys_Age_6_12 = Boys_Age_6_12%>%
  filter(DX_GROUP == "ASD")
Control_Boys_Age_6_12 = Boys_Age_6_12%>%
  filter(DX_GROUP == "Control")

TBV0.model <- 'TBV =~ NA*CorticalWhiteMatterVol_log 
+ Brain_Stem_log 
+ Left_Ventral_DC_log + Right_Ventral_DC_log 
+ lhCortexVol_log + rhCortexVol_log
+ Left_Accumbens_Area_log + Right_Accumbens_Area_log
+ Left_Cerebellum_Cortex_log + Right_Cerebellum_Cortex_log 
+ Left_Caudate_log + Right_Caudate_log 
+ Left_Hippocampus_log + Right_Hippocampus_log
+ Left_Pallidum_log + Right_Pallidum_log 
+ Left_Putamen_log + Right_Putamen_log 
+ Right_Thalamus_Proper_log + Left_Thalamus_Proper_log
+ Left_Amygdala_log + Right_Amygdala_log

TBV ~ DX_GROUP'

fit_TBV<- cfa(TBV0.model, data=Boys_Age_6_12, estimator = "MLR",check.gradient = FALSE)
summary (fit_TBV, standardize = T)

# -0.161    0.151   -1.063    0.288   -0.168   -0.084
#
TBV.model <- 'TBV =~ NA*CorticalWhiteMatterVol_log 
+ Brain_Stem_log 
+ Left_Ventral_DC_log + Right_Ventral_DC_log 
+ lhCortexVol_log + rhCortexVol_log
+ Left_Accumbens_Area_log + Right_Accumbens_Area_log
+ Left_Cerebellum_Cortex_log + Right_Cerebellum_Cortex_log 
+ Left_Caudate_log + Right_Caudate_log 
+ Left_Hippocampus_log + Right_Hippocampus_log
+ Left_Pallidum_log + Right_Pallidum_log 
+ Left_Putamen_log + Right_Putamen_log 
+ Right_Thalamus_Proper_log + Left_Thalamus_Proper_log
+ Left_Amygdala_log + Right_Amygdala_log
TBV ~ c(0.084, 0)* 1 
TBV ~~ 1*TBV
lhCortexVol_log	~~	rhCortexVol_log
Left_Caudate_log	~~	Right_Caudate_log
Left_Putamen_log	~~	Right_Putamen_log
Left_Cerebellum_Cortex_log	~~	Right_Cerebellum_Cortex_log
Right_Thalamus_Proper_log	~~	Left_Thalamus_Proper_log
Left_Accumbens_Area_log	~~	Right_Accumbens_Area_log
Left_Ventral_DC_log	~~	Right_Ventral_DC_log
Left_Amygdala_log	~~	Right_Amygdala_log
Left_Hippocampus_log	~~	Right_Hippocampus_log
Left_Pallidum_log ~~ Right_Pallidum_log 

CorticalWhiteMatterVol_log	~~	Brain_Stem_log

CorticalWhiteMatterVol_log	~~	Left_Ventral_DC_log
CorticalWhiteMatterVol_log	~~	Right_Ventral_DC_log

CorticalWhiteMatterVol_log ~~ Left_Amygdala_log
CorticalWhiteMatterVol_log	~~	Right_Amygdala_log

CorticalWhiteMatterVol_log~~Left_Putamen_log
CorticalWhiteMatterVol_log~~Right_Putamen_log

CorticalWhiteMatterVol_log~~Left_Pallidum_log
CorticalWhiteMatterVol_log	~~	Right_Pallidum_log

CorticalWhiteMatterVol_log~~rhCortexVol_log
CorticalWhiteMatterVol_log~~lhCortexVol_log

CorticalWhiteMatterVol_log~~Right_Hippocampus_log
CorticalWhiteMatterVol_log~~Left_Hippocampus_log

CorticalWhiteMatterVol_log~~Right_Cerebellum_Cortex_log
CorticalWhiteMatterVol_log~~Left_Cerebellum_Cortex_log

CorticalWhiteMatterVol_log~~Left_Accumbens_Area_log
CorticalWhiteMatterVol_log	~~	Right_Accumbens_Area_log

CorticalWhiteMatterVol_log~~Left_Caudate_log
CorticalWhiteMatterVol_log~~Right_Caudate_log

Left_Caudate_log	~~	Right_Putamen_log
Left_Caudate_log	~~	Left_Putamen_log
Right_Caudate_log	~~	Left_Putamen_log

Left_Caudate_log	~~	Left_Pallidum_log
Right_Caudate_log	~~	Left_Pallidum_log
Right_Caudate_log	~~	Right_Putamen_log
Right_Caudate_log	~~	Right_Pallidum_log
Left_Caudate_log	~~	Right_Pallidum_log

Left_Pallidum_log	~~	Left_Putamen_log
Left_Pallidum_log	~~	Right_Putamen_log
Right_Pallidum_log	~~	Right_Putamen_log
Right_Pallidum_log	~~	Left_Putamen_log

Right_Putamen_log	~~	Right_Amygdala_log
Left_Putamen_log	~~	Right_Amygdala_log
Left_Putamen_log	~~	Left_Amygdala_log
Right_Putamen_log	~~	Left_Amygdala_log

Right_Hippocampus_log	~~	Right_Putamen_log
Left_Hippocampus_log	~~	Left_Amygdala_log
Right_Hippocampus_log	~~	Left_Amygdala_log
Right_Hippocampus_log	~~	Right_Amygdala_log
Left_Hippocampus_log	~~	Right_Amygdala_log
Left_Hippocampus_log	~~	Left_Thalamus_Proper_log
Left_Hippocampus_log	~~	Right_Putamen_log
Right_Hippocampus_log	~~	Left_Putamen_log
Left_Hippocampus_log ~~ Right_Thalamus_Proper_log

lhCortexVol_log	~~	Right_Thalamus_Proper_log
rhCortexVol_log	~~	Right_Thalamus_Proper_log

lhCortexVol_log	~~	Left_Pallidum_log
rhCortexVol_log	~~	Right_Pallidum_log
lhCortexVol_log	~~	Right_Pallidum_log

lhCortexVol_log	~~	Right_Caudate_log
rhCortexVol_log ~~ Right_Caudate_log
rhCortexVol_log	~~	Left_Caudate_log
lhCortexVol_log	~~	Left_Caudate_log

rhCortexVol_log	~~	Right_Putamen_log
lhCortexVol_log	~~	Right_Putamen_log

lhCortexVol_log	~~	Left_Accumbens_Area_log
rhCortexVol_log	~~	Left_Accumbens_Area_log
rhCortexVol_log	~~	Right_Accumbens_Area_log
lhCortexVol_log	~~	Right_Accumbens_Area_log

rhCortexVol_log	~~	Left_Amygdala_log
lhCortexVol_log	~~	Left_Amygdala_log

rhCortexVol_log	~~	Left_Thalamus_Proper_log
lhCortexVol_log	~~	Left_Thalamus_Proper_log

Left_Accumbens_Area_log	~~	Left_Putamen_log
Right_Accumbens_Area_log	~~	Right_Putamen_log
Right_Accumbens_Area_log	~~	Left_Putamen_log
Left_Accumbens_Area_log	~~	Right_Putamen_log

Left_Accumbens_Area_log	~~	Right_Amygdala_log
Right_Accumbens_Area_log	~~	Left_Amygdala_log
Right_Accumbens_Area_log	~~	Right_Amygdala_log
Left_Accumbens_Area_log	~~	Left_Amygdala_log

Right_Accumbens_Area_log	~~	Right_Hippocampus_log
Left_Accumbens_Area_log	~~	Right_Hippocampus_log

Right_Accumbens_Area_log	~~	Right_Caudate_log
Right_Accumbens_Area_log	~~	Left_Caudate_log
Left_Accumbens_Area_log	~~	Left_Caudate_log
Left_Accumbens_Area_log	~~	Right_Caudate_log

Right_Accumbens_Area_log	~~	Right_Pallidum_log
Left_Accumbens_Area_log	~~	Right_Pallidum_log
Left_Accumbens_Area_log	~~	Left_Pallidum_log
Right_Accumbens_Area_log	~~	Left_Pallidum_log

Right_Accumbens_Area_log	~~	Right_Thalamus_Proper_log
Right_Accumbens_Area_log	~~	Left_Thalamus_Proper_log

Brain_Stem_log	~~	Right_Pallidum_log
Brain_Stem_log	~~	Left_Pallidum_log
Brain_Stem_log	~~	Right_Ventral_DC_log
Brain_Stem_log	~~	Left_Ventral_DC_log
Brain_Stem_log	~~	Left_Cerebellum_Cortex_log
Brain_Stem_log	~~	Right_Cerebellum_Cortex_log

Brain_Stem_log	~~	lhCortexVol_log
Brain_Stem_log	~~	rhCortexVol_log


Left_Ventral_DC_log	~~	Left_Accumbens_Area_log
Left_Ventral_DC_log	~~	Left_Pallidum_log
Left_Ventral_DC_log	~~	Right_Pallidum_log
Right_Ventral_DC_log	~~	Right_Caudate_log
Left_Ventral_DC_log	~~	Left_Hippocampus_log
Left_Ventral_DC_log	~~	Right_Hippocampus_log
Right_Ventral_DC_log	~~	Right_Amygdala_log
Right_Ventral_DC_log	~~	Left_Hippocampus_log

Left_Ventral_DC_log	~~	Right_Thalamus_Proper_log
Left_Ventral_DC_log	~~	Left_Thalamus_Proper_log
Right_Ventral_DC_log~~Right_Thalamus_Proper_log
Right_Ventral_DC_log	~~	Left_Thalamus_Proper_log
Right_Ventral_DC_log	~~	Right_Hippocampus_log

Right_Thalamus_Proper_log	~~	Right_Amygdala_log
Right_Thalamus_Proper_log	~~	Left_Amygdala_log
Left_Thalamus_Proper_log	~~	Left_Amygdala_log
Left_Thalamus_Proper_log	~~	Right_Amygdala_log

Right_Cerebellum_Cortex_log	~~	Left_Putamen_log
Left_Cerebellum_Cortex_log	~~	Left_Caudate_log
Left_Cerebellum_Cortex_log	~~	Right_Hippocampus_log

Brain_Stem_log ~~ Left_Caudate_log
Left_Ventral_DC_log ~~ Right_Amygdala_log
Right_Ventral_DC_log	~~	Left_Putamen_log
Left_Ventral_DC_log	~~	Right_Caudate_log
Left_Hippocampus_log ~~ Right_Pallidum_log
Right_Ventral_DC_log	~~	Left_Pallidum_log
Right_Pallidum_log	~~	Right_Amygdala_log
Left_Accumbens_Area_log	~~	Left_Cerebellum_Cortex_log
Brain_Stem_log	~~	Right_Putamen_log

Right_Hippocampus_log	~~	Left_Pallidum_log
Left_Hippocampus_log	~~	Left_Pallidum_log
Left_Ventral_DC_log ~~ Right_Putamen_log
Right_Ventral_DC_log	~~	Right_Putamen_log
Left_Ventral_DC_log ~~ Left_Cerebellum_Cortex_log
Brain_Stem_log~~Left_Putamen_log
Right_Ventral_DC_log	~~	Right_Accumbens_Area_log
Left_Cerebellum_Cortex_log ~~ Right_Thalamus_Proper_log'


Configural_Model<- cfa(TBV.model, data=Boys_Age_6_12, 
                       check.gradient = FALSE, 
                       group = "DX_GROUP", estimator = "MLR")
print(fitMeasures(Configural_Model, c("cfi.robust", "tli.robust", "rmsea.robust")))
mi <- modindices(Configural_Model)
View(mi) 

# Remove " TBV ~ c(0.060, 0)* 1 " from 
Control_Configural_Model<- cfa(TBV.model, data=Control_Boys_Age_6_12, check.gradient = FALSE, estimator = "MLR")
summary(Control_Configural_Model, fit.measures = T, standardize = T)
print(fitMeasures(Control_Configural_Model, c("cfi.robust", "tli.robust", "rmsea.robust")))

ASD_Configural_Model<- cfa(TBV.model, data=ASD_Boys_Age_6_12,check.gradient = FALSE, estimator = "MLR")
summary(ASD_Configural_Model, fit.measures = T, standardize = T)
print(fitMeasures(ASD_Configural_Model, c("cfi.robust", "tli.robust", "rmsea.robust")))

## Metric Model 
Metric_Invariance_Model<- cfa(TBV.model, data=Boys_Age_6_12, group = "DX_GROUP", check.gradient = FALSE,check.gradient = FALSE, estimator = "MLR", group.equal = c("loadings"))
anova(Configural_Model,Metric_Invariance_Model) #

## Scalar Model 
Scalar_Invariance_Model<- cfa(TBV.model, data=Boys_Age_6_12, group = "DX_GROUP", check.gradient = FALSE, group.equal = c("loadings", "intercepts"),
                              estimator = "MLR")
anova(Scalar_Invariance_Model,Metric_Invariance_Model) 

sink("CSV/anova_tables_boys_6_12.csv")
print(fitMeasures(Configural_Model, c("cfi.robust", "rmsea.robust", "tli.robust")))
print (anova(Configural_Model,Metric_Invariance_Model))
print(fitMeasures(Metric_Invariance_Model, c("cfi.robust", "rmsea.robust", "tli.robust")))
print( anova(Scalar_Invariance_Model,Metric_Invariance_Model) )
print(fitMeasures(Scalar_Invariance_Model, c("cfi.robust", "rmsea.robust", "tli.robust")))
sink()

sink("CSV/A_Boys_6_12_Abide.csv")
print(fitMeasures(Configural_Model, c("cfi.robust", "tli.robust","rmsea.robust")))
print(fitMeasures(ASD_Configural_Model, c("cfi.robust","tli.robust", "rmsea.robust")))
print(fitMeasures(Control_Configural_Model, c("cfi.robust","tli.robust", "rmsea.robust")))
sink()

sink("CSV/parameterestimates_configural_boys_6_12.csv")
options(max.print= 200000)
print(Parameters_loading(Configural_Model))
print(Parameters_intercept(Configural_Model))
sink()