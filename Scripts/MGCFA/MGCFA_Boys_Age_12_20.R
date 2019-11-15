library(lavaan)
library(semTools)
library (dplyr)

Abide_LMEM_log_Clean$DX_GROUP <- factor(Abide_LMEM_log_Clean$DX_GROUP, levels = c("Control", "ASD"))

Boys_Age_12_19 = Abide_LMEM_log_Clean %>%
  filter(AGE_AT_SCAN >= 12 & AGE_AT_SCAN <20) %>%
  filter(SEX == "Male") 

ASD_Boys_Age_12_19 = Boys_Age_13_19%>%
  filter(DX_GROUP == "ASD")
Control_Boys_Age_12_19 = Boys_Age_13_19%>%
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

fit_TBV<- cfa(TBV0.model, data=Boys_Age_12_19, check.gradient = F, estimator = "MLR")
summary(fit_TBV, fit.measures=TRUE, standardized = T) 

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
TBV ~~ 1*TBV
TBV ~ c(0, 0.027)* 1
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

Right_Putamen_log	~~	Right_Amygdala_log

Left_Ventral_DC_log	~~	Right_Thalamus_Proper_log
Right_Ventral_DC_log	~~	Right_Pallidum_log
Left_Accumbens_Area_log	~~	Right_Putamen_log
Right_Accumbens_Area_log	~~	Right_Putamen_log
Right_Accumbens_Area_log	~~	Left_Putamen_log
Left_Accumbens_Area_log	~~	Left_Putamen_log
CorticalWhiteMatterVol_log	~~	rhCortexVol_log
rhCortexVol_log	~~	Right_Amygdala_log
rhCortexVol_log	~~	Right_Putamen_log
Right_Hippocampus_log	~~	Left_Amygdala_log
lhCortexVol_log	~~	Left_Pallidum_log
Right_Caudate_log	~~	Left_Thalamus_Proper_log
Left_Ventral_DC_log	~~	Left_Cerebellum_Cortex_log
Left_Accumbens_Area_log	~~	Left_Cerebellum_Cortex_log
Left_Accumbens_Area_log	~~	Right_Caudate_log
Right_Ventral_DC_log	~~	Right_Caudate_log
Left_Hippocampus_log	~~	Right_Pallidum_log
Right_Hippocampus_log	~~	Right_Amygdala_log
Left_Hippocampus_log	~~	Left_Pallidum_log
CorticalWhiteMatterVol_log	~~	Right_Pallidum_log
CorticalWhiteMatterVol_log	~~	Left_Pallidum_log
Brain_Stem_log	~~	Right_Cerebellum_Cortex_log
Right_Cerebellum_Cortex_log	~~	Right_Thalamus_Proper_log
Brain_Stem_log	~~	Left_Cerebellum_Cortex_log
rhCortexVol_log	~~	Right_Thalamus_Proper_log
CorticalWhiteMatterVol_log~~Right_Amygdala_log			
Left_Pallidum_log	~~	Right_Putamen_log
Right_Hippocampus_log	~~	Left_Pallidum_log
Brain_Stem_log	~~	Left_Ventral_DC_log
Brain_Stem_log	~~	Right_Ventral_DC_log
Left_Caudate_log ~~ Left_Hippocampus_log
Left_Hippocampus_log	~~	Right_Thalamus_Proper_log
Right_Cerebellum_Cortex_log	~~	Left_Putamen_log
Right_Caudate_log	~~	Left_Hippocampus_log
Left_Hippocampus_log	~~	Right_Amygdala_log
Left_Accumbens_Area_log	~~	Left_Amygdala_log
Left_Accumbens_Area_log	~~	Right_Amygdala_log
Left_Thalamus_Proper_log	~~	Left_Amygdala_log
Left_Hippocampus_log	~~	Left_Amygdala_log
Left_Caudate_log	~~	Left_Amygdala_log
Left_Caudate_log	~~	Right_Amygdala_log
Right_Caudate_log	~~	Right_Pallidum_log
Left_Caudate_log	~~	Right_Thalamus_Proper_log
CorticalWhiteMatterVol_log	~~	Right_Caudate_log
CorticalWhiteMatterVol_log	~~	Right_Accumbens_Area_log
CorticalWhiteMatterVol_log	~~	Right_Cerebellum_Cortex_log
Left_Cerebellum_Cortex_log	~~	Left_Thalamus_Proper_log
Right_Pallidum_log	~~	Right_Putamen_log
Right_Hippocampus_log	~~	Right_Pallidum_log
Right_Pallidum_log	~~	Right_Amygdala_log
Brain_Stem_log	~~	Left_Thalamus_Proper_log
Right_Ventral_DC_log	~~	Left_Putamen_log
Right_Accumbens_Area_log	~~	Right_Amygdala_log
lhCortexVol_log	~~	Left_Thalamus_Proper_log
Left_Accumbens_Area_log	~~	Right_Hippocampus_log
Right_Pallidum_log	~~	Left_Thalamus_Proper_log
Right_Pallidum_log	~~	Left_Putamen_log
Left_Pallidum_log	~~	Left_Putamen_log
Right_Accumbens_Area_log	~~	Left_Thalamus_Proper_log
CorticalWhiteMatterVol_log	~~	Left_Cerebellum_Cortex_log
Right_Ventral_DC_log	~~	Right_Accumbens_Area_log
Left_Ventral_DC_log	~~	Left_Accumbens_Area_log
Right_Ventral_DC_log	~~	Right_Putamen_log
Right_Pallidum_log	~~	Left_Amygdala_log
Left_Accumbens_Area_log	~~	Left_Pallidum_log
Left_Pallidum_log	~~	Right_Thalamus_Proper_log
Right_Hippocampus_log	~~	Right_Putamen_log
Left_Ventral_DC_log	~~	Right_Pallidum_log
Brain_Stem_log	~~	Left_Pallidum_log
Left_Ventral_DC_log	~~	Left_Pallidum_log
Brain_Stem_log	~~	Right_Pallidum_log
Left_Putamen_log	~~	Left_Thalamus_Proper_log
Brain_Stem_log	~~	Right_Hippocampus_log
Right_Caudate_log	~~	Left_Putamen_log
Left_Thalamus_Proper_log	~~	Right_Amygdala_log
Left_Ventral_DC_log	~~	Right_Putamen_log
Right_Ventral_DC_log	~~	Right_Hippocampus_log
CorticalWhiteMatterVol_log	~~	Right_Hippocampus_log
Left_Ventral_DC_log	~~	Left_Caudate_log
Brain_Stem_log	~~	Right_Amygdala_log
Left_Ventral_DC_log	~~	Left_Putamen_log
Brain_Stem_log	~~	Left_Putamen_log
Left_Caudate_log	~~	Left_Putamen_log
Brain_Stem_log	~~	lhCortexVol_log
lhCortexVol_log	~~	Right_Thalamus_Proper_log
Right_Thalamus_Proper_log	~~	Right_Amygdala_log
Left_Ventral_DC_log~~rhCortexVol_log
rhCortexVol_log	~~	Right_Pallidum_log
CorticalWhiteMatterVol_log	~~	Left_Ventral_DC_log

Right_Thalamus_Proper_log	~~	Left_Amygdala_log
Right_Cerebellum_Cortex_log	~~	Right_Amygdala_log
CorticalWhiteMatterVol_log	~~	Left_Putamen_log
Left_Putamen_log	~~	Right_Amygdala_log

Left_Cerebellum_Cortex_log	~~	Right_Amygdala_log
Right_Pallidum_log	~~	Right_Thalamus_Proper_log
Left_Caudate_log	~~	Right_Pallidum_log
Left_Accumbens_Area_log	~~	Left_Thalamus_Proper_log
Left_Cerebellum_Cortex_log	~~	Right_Pallidum_log
Brain_Stem_log	~~	rhCortexVol_log
Right_Cerebellum_Cortex_log	~~	Right_Pallidum_log
Right_Ventral_DC_log	~~	Left_Cerebellum_Cortex_log
rhCortexVol_log	~~	Left_Putamen_log
Right_Putamen_log	~~	Left_Thalamus_Proper_log
CorticalWhiteMatterVol_log	~~	Left_Accumbens_Area_log
Right_Accumbens_Area_log	~~	Right_Caudate_log
Left_Caudate_log	~~	Right_Putamen_log
Right_Caudate_log	~~	Right_Putamen_log
Left_Caudate_log	~~	Left_Pallidum_log
Right_Caudate_log	~~	Left_Pallidum_log
Right_Ventral_DC_log ~~ Right_Thalamus_Proper_log
Left_Ventral_DC_log ~~ Right_Accumbens_Area_log
Right_Ventral_DC_log ~~ Left_Caudate_log
Left_Ventral_DC_log ~~ Left_Thalamus_Proper_log
Right_Accumbens_Area_log ~~ Right_Thalamus_Proper_log
rhCortexVol_log ~~ Right_Accumbens_Area_log

Left_Accumbens_Area_log ~~ Right_Thalamus_Proper_log
Left_Pallidum_log ~~ Left_Thalamus_Proper_log
rhCortexVol_log ~~ Left_Amygdala_log

Right_Accumbens_Area_log ~~ Left_Pallidum_log
Right_Accumbens_Area_log ~~ Right_Pallidum_log
Left_Pallidum_log ~~ Right_Amygdala_log
Right_Accumbens_Area_log ~~ Left_Amygdala_log
Right_Putamen_log ~~ Left_Amygdala_log
Left_Putamen_log ~~ Left_Amygdala_log
lhCortexVol_log ~~ Right_Caudate_log
lhCortexVol_log ~~ Left_Caudate_log
'
# Configural Model 
Configural_Model<- cfa(TBV.model, data=Boys_Age_12_19, check.gradient = F, group = "DX_GROUP", estimator = "MLR")
fitMeasures(Configural_Model, c("cfi.robust" ,"rmsea.robust", "tli.robust"))
summary(Configural_Model, standardized = T, fit.measures = T)

sink ("CSV/boys_12_19_communality.csv")
lavInspect(Configural_Model, "rsquare")
sink() 

# Per group, remove "TBV ~ c(0, 0.027)* 1" in the TBV.model or the model will not run 
fit_TBV_ASD<- cfa(TBV.model, data=ASD_Boys_Age_12_19, check.gradient = F, group = "DX_GROUP", estimator = "MLR")
fitMeasures(fit_TBV_ASD, c("cfi.robust" ,"rmsea.robust", "tli.robust"))

fit_TBV_Control<- cfa(TBV.model, data=Control_Boys_Age_12_19, check.gradient = F, group = "DX_GROUP", estimator = "MLR")
fitMeasures(fit_TBV_Control, c("cfi.robust" ,"rmsea.robust", "tli.robust"))

# Metric Invariance 
Metric_Invariance_Model<- cfa(TBV.model, data=Boys_Age_12_19, check.gradient = F, group = "DX_GROUP", group.equal = c("loadings"), estimator = "MLR")
anova(Metric_Invariance_Model, Configural_Model)
fitMeasures(Metric_Invariance_Model, c("cfi.robust" ,"rmsea.robust", "tli.robust"))

#Get difference in slope effect size between groups by calculating Local Vol std.all Group 1 - Local Vol std.all Group 2 for each volume 
Parameters_loading <- function (x) {parameterestimates(x, standardize = T) %>% filter(op == "=~")}
sink ("CSV/Parameters_loading_Configural_Model.csv")
print(Parameters_loading(Configural_Model))
sink() 

# Scalar Invariance 
Scalar_Invariance_Model<- cfa(TBV.model, data=Boys_Age_12_19, check.gradient = F, group = "DX_GROUP", group.equal = c("loadings", "intercepts"), estimator = "MLR")
anova(Metric_Invariance_Model, Scalar_Invariance_Model)
fitMeasures(Scalar_Invariance_Model, c("cfi.robust" ,"rmsea.robust", "tli.robust"))

sink("CSV/anova_tables_Boys_Age_13_19.csv")
print(fitMeasures(Configural_Model, c("cfi.robust", "rmsea.robust", "tli.robust")))
print (anova(Configural_Model,Metric_Invariance_Model))
print(fitMeasures(Metric_Invariance_Model, c("cfi.robust", "rmsea.robust", "tli.robust")))
print( anova(Scalar_Invariance_Model,Metric_Invariance_Model) )
print(fitMeasures(Scalar_Invariance_Model, c("cfi.robust", "rmsea.robust", "tli.robust")))
sink()

sink("CSV/Boys_12_19_Abide.csv")
print(fitMeasures(Configural_Model, c("cfi.robust", "tli.robust","rmsea.robust")))
print(fitMeasures(fit_TBV_ASD, c("cfi.robust","tli.robust", "rmsea.robust")))
print(fitMeasures(fit_TBV_Control, c("cfi.robust","tli.robust", "rmsea.robust")))
sink()

sink("CSV/parameterestimates_configural_Boys_Age_13_19.csv")
options(max.print= 200000)
print(Parameters_loading(Configural_Model))
print(Parameters_intercept(Configural_Model))
sink()


