library(lavaan)
library(semTools)
library (dplyr)

Abide_LMEM_log_Clean$DX_GROUP <- factor(Abide_LMEM_log_Clean$DX_GROUP, levels = c("Control", "ASD"))

Boys_Under_107_Abide = Abide_LMEM_log_Clean%>%
  filter(FIQ2<=107.8 ) %>%
  filter(SEX == "Male") 

# Control ASD order as in sample
ASD_Abide_LMEM_log_Clean = Boys_Under_107_Abide%>%
  filter(DX_GROUP == "ASD") 
Control_Abide_LMEM_log_Clean = Boys_Under_107_Abide%>%
  filter(DX_GROUP == "Control") 

####### BETA ########
TBV1.model <- 'TBV =~ NA*CorticalWhiteMatterVol_log 
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

fit_TBV<- cfa(TBV1.model, data=Boys_Under_107_Abide, 
              check.gradient = FALSE, estimator = "MLR", fixed.x = T)
summary(fit_TBV, standardize = T) # 0.015    0.007    2.276    0.023    0.267    0.130
parameterestimates(fit_TBV)

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
TBV ~ c(0.130, 0)* 1

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

Left_Hippocampus_log	~~	Left_Amygdala_log
Left_Accumbens_Area_log	~~	Right_Amygdala_log
rhCortexVol_log	~~	Right_Amygdala_log
Brain_Stem_log	~~	Right_Ventral_DC_log
Brain_Stem_log	~~	Left_Ventral_DC_log
Right_Accumbens_Area_log	~~	Left_Pallidum_log
lhCortexVol_log	~~	Left_Pallidum_log
CorticalWhiteMatterVol_log	~~	Right_Caudate_log
Left_Accumbens_Area_log	~~	Left_Amygdala_log
Left_Caudate_log	~~	Left_Pallidum_log
Right_Accumbens_Area_log	~~	Right_Thalamus_Proper_log
Brain_Stem_log	~~	Left_Accumbens_Area_log
Brain_Stem_log	~~	Left_Pallidum_log
Brain_Stem_log	~~	Right_Putamen_log
Right_Ventral_DC_log	~~	Left_Putamen_log
Right_Ventral_DC_log	~~	Right_Pallidum_log
Left_Ventral_DC_log	~~	Right_Hippocampus_log
Left_Accumbens_Area_log	~~	Right_Hippocampus_log
Right_Accumbens_Area_log	~~	Right_Cerebellum_Cortex_log
Right_Accumbens_Area_log	~~	Right_Hippocampus_log	
Right_Cerebellum_Cortex_log	~~	Left_Caudate_log
Right_Accumbens_Area_log	~~	Left_Cerebellum_Cortex_log
Right_Cerebellum_Cortex_log	~~	Right_Pallidum_log
Right_Putamen_log	~~	Right_Amygdala_log
Right_Accumbens_Area_log	~~	Left_Hippocampus_log
Left_Caudate_log	~~	Right_Amygdala_log
Left_Cerebellum_Cortex_log	~~	Left_Amygdala_log
Left_Ventral_DC_log	~~	Left_Caudate_log
Right_Ventral_DC_log	~~	Right_Accumbens_Area_log
Left_Accumbens_Area_log	~~	Right_Cerebellum_Cortex_log
Left_Accumbens_Area_log	~~	Left_Hippocampus_log
rhCortexVol_log	~~	Right_Putamen_log
Left_Accumbens_Area_log	~~	Left_Pallidum_log
Left_Ventral_DC_log	~~	Left_Hippocampus_log
Right_Caudate_log	~~	Left_Hippocampus_log
Right_Cerebellum_Cortex_log	~~	Left_Pallidum_log
CorticalWhiteMatterVol_log	~~	Left_Caudate_log
CorticalWhiteMatterVol_log	~~	Right_Hippocampus_log
Left_Accumbens_Area_log	~~	Left_Cerebellum_Cortex_log
CorticalWhiteMatterVol_log	~~	Left_Ventral_DC_log
Right_Accumbens_Area_log	~~	Left_Thalamus_Proper_log
Left_Cerebellum_Cortex_log	~~	Right_Caudate_log
Left_Cerebellum_Cortex_log	~~	Right_Pallidum_log
Right_Hippocampus_log	~~	Right_Thalamus_Proper_log
Right_Hippocampus_log	~~	Left_Thalamus_Proper_log
CorticalWhiteMatterVol_log	~~	Right_Ventral_DC_log
Brain_Stem_log	~~	Right_Cerebellum_Cortex_log
Left_Hippocampus_log	~~	Right_Putamen_log
Right_Ventral_DC_log	~~	Right_Amygdala_log
Brain_Stem_log	~~	Left_Cerebellum_Cortex_log
Left_Accumbens_Area_log	~~	Left_Caudate_log
Right_Ventral_DC_log	~~	Left_Hippocampus_log
Left_Accumbens_Area_log	~~	Right_Caudate_log
Right_Ventral_DC_log	~~	Right_Putamen_log
Right_Putamen_log	~~	Left_Amygdala_log
Right_Ventral_DC_log	~~	Left_Accumbens_Area_log
rhCortexVol_log	~~	Left_Putamen_log
CorticalWhiteMatterVol_log	~~	lhCortexVol_log
Right_Accumbens_Area_log	~~	Right_Putamen_log
Left_Pallidum_log	~~	Left_Putamen_log
Right_Pallidum_log	~~	Right_Amygdala_log
CorticalWhiteMatterVol_log~~Left_Cerebellum_Cortex_log
CorticalWhiteMatterVol_log	~~	rhCortexVol_log
Right_Putamen_log	~~	Left_Thalamus_Proper_log
CorticalWhiteMatterVol_log	~~	Right_Cerebellum_Cortex_log
lhCortexVol_log	~~	Left_Hippocampus_log
rhCortexVol_log	~~	Right_Accumbens_Area_log
CorticalWhiteMatterVol_log	~~	Brain_Stem_log
rhCortexVol_log	~~	Left_Accumbens_Area_log
CorticalWhiteMatterVol_log	~~	Right_Amygdala_log
Left_Putamen_log	~~	Left_Amygdala_log
rhCortexVol_log	~~	Left_Thalamus_Proper_log
Left_Pallidum_log	~~	Right_Thalamus_Proper_log
Left_Pallidum_log	~~	Left_Amygdala_log
Left_Caudate_log	~~	Right_Thalamus_Proper_log
Left_Accumbens_Area_log	~~	Right_Putamen_log
Left_Pallidum_log	~~	Right_Putamen_log
CorticalWhiteMatterVol_log	~~	Right_Pallidum_log
Left_Putamen_log	~~	Right_Amygdala_log
Right_Accumbens_Area_log	~~	Right_Caudate_log
Right_Hippocampus_log	~~	Left_Amygdala_log
Right_Hippocampus_log	~~	Left_Putamen_log
Right_Hippocampus_log	~~	Right_Pallidum_log
Right_Ventral_DC_log	~~	Right_Cerebellum_Cortex_log
Brain_Stem_log	~~	Left_Caudate_log
Left_Ventral_DC_log	~~	Left_Pallidum_log

Right_Hippocampus_log	~~	Right_Amygdala_log

Left_Accumbens_Area_log	~~	Left_Putamen_log
Brain_Stem_log	~~	Right_Caudate_log
Right_Hippocampus_log	~~	Right_Putamen_log
Left_Hippocampus_log	~~	Right_Amygdala_log
Right_Accumbens_Area_log	~~	Right_Amygdala_log
Right_Accumbens_Area_log	~~	Left_Amygdala_log
CorticalWhiteMatterVol_log	~~	Left_Thalamus_Proper_log
Left_Ventral_DC_log	~~	Left_Cerebellum_Cortex_log

Left_Caudate_log	~~	Left_Amygdala_log
Left_Hippocampus_log	~~	Left_Putamen_log
Left_Putamen_log	~~	Right_Thalamus_Proper_log
Left_Pallidum_log	~~	Right_Amygdala_log

Right_Accumbens_Area_log	~~	Left_Putamen_log
CorticalWhiteMatterVol_log	~~	Left_Accumbens_Area_log
Right_Cerebellum_Cortex_log	~~	Right_Putamen_log
CorticalWhiteMatterVol_log	~~	Left_Amygdala_log
CorticalWhiteMatterVol_log	~~	Left_Pallidum_log
Left_Caudate_log	~~	Left_Thalamus_Proper_log	
CorticalWhiteMatterVol_log	~~	Left_Hippocampus_log
Left_Caudate_log	~~	Left_Hippocampus_log	
Right_Caudate_log	~~	Left_Pallidum_log	
Right_Caudate_log	~~	Right_Pallidum_log
Brain_Stem_log	~~	Left_Amygdala_log
Left_Ventral_DC_log	~~	Left_Amygdala_log
Right_Pallidum_log	~~	Right_Putamen_log
Left_Cerebellum_Cortex_log	~~	Right_Putamen_log
Left_Ventral_DC_log	~~	Right_Accumbens_Area_log
Right_Pallidum_log	~~	Left_Putamen_log
lhCortexVol_log	~~	Right_Pallidum_log

Right_Pallidum_log	~~	Left_Amygdala_log
Right_Putamen_log	~~	Right_Thalamus_Proper_log	
Left_Cerebellum_Cortex_log	~~	Right_Amygdala_log
Right_Cerebellum_Cortex_log	~~	Left_Hippocampus_log
Right_Cerebellum_Cortex_log	~~	Right_Amygdala_log
Right_Cerebellum_Cortex_log	~~	Left_Putamen_log
Left_Putamen_log	~~	Left_Thalamus_Proper_log
Left_Cerebellum_Cortex_log	~~	Left_Putamen_log
Left_Cerebellum_Cortex_log	~~	Right_Hippocampus_log
Right_Accumbens_Area_log	~~	Left_Caudate_log
Brain_Stem_log	~~	Left_Hippocampus_log

Left_Thalamus_Proper_log	~~	Left_Amygdala_log
Left_Ventral_DC_log	~~	Left_Putamen_log
Left_Ventral_DC_log ~~ Right_Pallidum_log
Right_Thalamus_Proper_log ~~ Left_Amygdala_log
Right_Ventral_DC_log ~~ Right_Hippocampus_log
Right_Ventral_DC_log ~~ Left_Amygdala_log

Left_Ventral_DC_log ~~ Right_Cerebellum_Cortex_log
Brain_Stem_log ~~ Right_Amygdala_log
Brain_Stem_log ~~ Right_Hippocampus_log
Left_Hippocampus_log~~ Right_Pallidum_log
Left_Thalamus_Proper_log ~~ Right_Amygdala_log
Right_Thalamus_Proper_log ~~ Right_Amygdala_log
lhCortexVol_log ~~ Left_Amygdala_log
Left_Cerebellum_Cortex_log ~~ Left_Hippocampus_log
Left_Ventral_DC_log ~~ Right_Putamen_log
Left_Ventral_DC_log ~~ rhCortexVol_log
rhCortexVol_log ~~ Left_Cerebellum_Cortex_log

Left_Ventral_DC_log ~~ lhCortexVol_log
rhCortexVol_log ~~ Left_Amygdala_log
rhCortexVol_log ~~ Right_Thalamus_Proper_log
'

# Configural Model
fit_TBV<- cfa(TBV.model, data=Boys_Under_107_Abide, 
              check.gradient = F, group = "DX_GROUP", estimator = "MLR")
fitMeasures(fit_TBV, c("cfi.robust", "tli.robust", "rmsea.robust"))
summary (fit_TBV, standardized = T , fit.measures = T )

sink ("CSV/boys_under_median_communality.csv")
lavInspect(fit_TBV, "rsquare")
sink() 

## Per Group remove "TBV ~ c(0.130, 0)* 1" from the TBV.model or model will not run 
fit_TBV_ASD<- cfa(TBV.model, data=ASD_Boys_Under_107_Abide, check.gradient = F,group = "DX_GROUP", estimator = "MLR")
fitMeasures(fit_TBV_ASD, c("cfi.robust", "rmsea.robust"))
summary (fit_TBV_ASD, standardized = T , fit.measures = T )

fit_TBV_Control<- cfa(TBV.model, data=Control_Boys_Under_107_Abide, check.gradient = F,group = "DX_GROUP", estimator = "MLR")
fitMeasures(fit_TBV_Control, c("cfi.robust", "rmsea.robust", "tli.robust"))
summary (fit_TBV_Control, standardized = T,fit.measures = T )

# Metric Invariance 
Metric_Invariance_Model<- cfa(TBV.model, data=Boys_Under_107_Abide, check.gradient = F,
                              group = "DX_GROUP", group.equal = c("loadings"), estimator = "MLR")
anova(Metric_Invariance_Model, fit_TBV)

# Scalar Invariance 
Scalar_Invariance_Model<- cfa(TBV.model, data=Boys_Under_107_Abide, check.gradient = F,group = "DX_GROUP", group.equal = c("loadings", "intercepts"), estimator = "MLR")
anova(Metric_Invariance_Model, Scalar_Invariance_Model)

sink("CSV/anova_tables_boys_under_107.csv")
print(fitMeasures(fit_TBV, c("cfi.robust", "rmsea.robust", "tli.robust")))
print (anova(fit_TBV,Metric_Invariance_Model))
print(fitMeasures(Metric_Invariance_Model, c("cfi.robust", "rmsea.robust", "tli.robust")))
print( anova(Scalar_Invariance_Model,Metric_Invariance_Model) )
print(fitMeasures(Scalar_Invariance_Model, c("cfi.robust", "rmsea.robust", "tli.robust")))
sink()

sink("CSV/Boys_Under_107_Abide.csv")
print(fitMeasures(fit_TBV, c("cfi.robust", "tli.robust","rmsea.robust")))
print(fitMeasures(fit_TBV_ASD, c("cfi.robust","tli.robust", "rmsea.robust")))
print(fitMeasures(fit_TBV_Control, c("cfi.robust","tli.robust", "rmsea.robust")))
sink()

sink("CSV/parameterestimates_configural_boys_under_107.csv")
options(max.print= 200000)
print(Parameters_loading(fit_TBV))
print(Parameters_intercept(fit_TBV))
sink()
