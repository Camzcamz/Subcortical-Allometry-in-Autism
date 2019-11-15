library(lavaan)
library(semTools)
library (dplyr)

Abide_LMEM_log_Clean$DX_GROUP <- factor(Abide_LMEM_log_Clean$DX_GROUP, levels = c("Control", "ASD"))

Boys_Over_107_Abide = Abide_LMEM_log_Clean%>%
  filter(FIQ2>107.8 ) %>%
  filter(SEX == "Male") 

ASD_Boys_Over_107_Abide = Boys_Over_107_Abide %>%
  filter(DX_GROUP == "ASD")
Control_Boys_Over_107_Abide = Boys_Over_107_Abide %>%
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

fit_TBV<- cfa(TBV0.model, data=Boys_Over_107_Abide, check.gradient = F, estimator = "MLR")
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
TBV ~ c(0.044, 0.00)* 1
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
Brain_Stem_log	~~	Left_Pallidum_log
Left_Ventral_DC_log	~~	Right_Caudate_log
rhCortexVol_log	~~	Right_Putamen_log
CorticalWhiteMatterVol_log	~~	Brain_Stem_log
Right_Ventral_DC_log ~~ Right_Thalamus_Proper_log
CorticalWhiteMatterVol_log ~~ Right_Amygdala_log
Left_Cerebellum_Cortex_log ~~ Left_Putamen_log
lhCortexVol_log ~~ Right_Thalamus_Proper_log
Right_Ventral_DC_log ~~ Left_Pallidum_log
Right_Accumbens_Area_log ~~ Right_Putamen_log
Left_Hippocampus_log ~~ Left_Amygdala_log
Right_Caudate_log~~Left_Thalamus_Proper_log
Right_Caudate_log ~~ Right_Thalamus_Proper_log
CorticalWhiteMatterVol_log ~~ Right_Thalamus_Proper_log
Left_Pallidum_log ~~ Left_Putamen_log
Left_Pallidum_log ~~ Right_Putamen_log
Brain_Stem_log ~~ Right_Cerebellum_Cortex_log

Brain_Stem_log ~~ Left_Cerebellum_Cortex_log
CorticalWhiteMatterVol_log ~~ Right_Pallidum_log

Right_Pallidum_log ~~ Right_Putamen_log
Right_Pallidum_log ~~ Left_Putamen_log
lhCortexVol_log ~~ Left_Putamen_log
Right_Hippocampus_log ~~ Left_Pallidum_log
Right_Hippocampus_log ~~ Right_Pallidum_log
Left_Hippocampus_log ~~ Right_Pallidum_log
Right_Pallidum_log ~~ Left_Amygdala_log

Brain_Stem_log ~~ Left_Hippocampus_log
lhCortexVol_log ~~ Right_Putamen_log
Left_Accumbens_Area_log ~~ Right_Caudate_log
Right_Ventral_DC_log ~~ Left_Cerebellum_Cortex_log
Left_Accumbens_Area_log ~~ Left_Putamen_log
Right_Accumbens_Area_log ~~ Left_Putamen_log
Left_Accumbens_Area_log ~~ Right_Putamen_log
CorticalWhiteMatterVol_log ~~ Left_Hippocampus_log
Right_Caudate_log ~~ Right_Putamen_log
Left_Caudate_log ~~ Right_Putamen_log
Left_Thalamus_Proper_log ~~ Left_Amygdala_log
Left_Pallidum_log ~~ Left_Thalamus_Proper_log
Left_Ventral_DC_log ~~ Right_Thalamus_Proper_log
Left_Ventral_DC_log ~~ Left_Thalamus_Proper_log
Right_Cerebellum_Cortex_log ~~ Left_Thalamus_Proper_log
CorticalWhiteMatterVol_log ~~ Right_Cerebellum_Cortex_log
Brain_Stem_log ~~ Left_Ventral_DC_log
Brain_Stem_log ~~ Right_Ventral_DC_log
CorticalWhiteMatterVol_log ~~ Left_Putamen_log

Right_Hippocampus_log ~~ Right_Putamen_log
Right_Ventral_DC_log ~~ Right_Caudate_log
Right_Ventral_DC_log ~~ Left_Caudate_log
Left_Pallidum_log ~~ Right_Thalamus_Proper_log
Left_Ventral_DC_log ~~ Left_Caudate_log
Right_Hippocampus_log ~~ Right_Amygdala_log
lhCortexVol_log ~~ Right_Caudate_log

lhCortexVol_log ~~ Left_Thalamus_Proper_log
rhCortexVol_log ~~ Right_Caudate_log
Left_Caudate_log ~~ Left_Pallidum_log
Right_Caudate_log ~~ Left_Pallidum_log
rhCortexVol_log ~~ Left_Thalamus_Proper_log
Left_Accumbens_Area_log ~~ Right_Cerebellum_Cortex_log

lhCortexVol_log~~ Right_Hippocampus_log
rhCortexVol_log ~~ Left_Caudate_log
Left_Ventral_DC_log ~~ Left_Accumbens_Area_log
Left_Ventral_DC_log ~~ Right_Accumbens_Area_log
Left_Ventral_DC_log ~~ Left_Hippocampus_log

rhCortexVol_log ~~ Left_Amygdala_log

Right_Accumbens_Area_log ~~ Left_Hippocampus_log
Left_Accumbens_Area_log ~~ Left_Hippocampus_log
Left_Hippocampus_log ~~ Right_Amygdala_log
Right_Hippocampus_log ~~ Left_Amygdala_log
Right_Caudate_log ~~ Right_Amygdala_log
Right_Ventral_DC_log ~~ Left_Hippocampus_log

CorticalWhiteMatterVol_log ~~ Right_Ventral_DC_log
CorticalWhiteMatterVol_log ~~ Left_Pallidum_log
CorticalWhiteMatterVol_log ~~ Right_Hippocampus_log
Brain_Stem_log ~~ Right_Thalamus_Proper_log
Left_Putamen_log ~~ Left_Thalamus_Proper_log
Right_Accumbens_Area_log ~~ Right_Caudate_log
Right_Caudate_log ~~ Left_Putamen_log
Left_Caudate_log ~~ Left_Putamen_log
Left_Accumbens_Area_log ~~ Right_Hippocampus_log
'

fit_TBV<- cfa(TBV.model, data=Boys_Over_107_Abide, check.gradient = F, group = "DX_GROUP", estimator = "MLR")
summary(fit_TBV, fit.measures=TRUE, standardized = T) 
print(fitMeasures(fit_TBV, c("cfi.robust","tli.robust", "rmsea.robust")))

sink ("CSV/boys_over_median_communality.csv")
lavInspect(fit_TBV, "rsquare")
sink() 


### PER GROUP - Remove " TBV ~ c(0.044, 0.00)* 1 " from the TBV.model or the model will not run 
ASD_fit_TBV1<- cfa(TBV.model,data=ASD_Boys_Over_107_Abide, check.gradient = F, group = "DX_GROUP", estimator = "MLR")
summary(ASD_fit_TBV1, fit.measures=TRUE, standardized = T) 
fitMeasures(ASD_fit_TBV1, c("cfi.robust","tli.robust", "rmsea.robust")) 

Control_fit_TBV1<- cfa(TBV.model, data=Control_Boys_Over_107_Abide, check.gradient = F, group = "DX_GROUP", estimator = "MLR")
summary(Control_fit_TBV1, fit.measures=TRUE, standardized = T) 
fitMeasures(Control_fit_TBV1, c("cfi.robust","tli.robust", "rmsea.robust"))

# Metric Invariance
Metric_Invariance_Model<- cfa(TBV.model, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator = "MLR", group.equal = c("loadings"))
fitMeasures(Metric_Invariance_Model, c("cfi.robust","tli.robust", "rmsea.robust"))
anova(fit_TBV,Metric_Invariance_Model) 

#Get difference in slope effect size between groups by calculating Local Vol std.all Group 1 - Local Vol std.all Group 2 for each volume 
Parameters_loading <- function (x) {parameterestimates(x, standardize = T) %>% filter(op == "=~")}
sink ("CSV/Parameters_loading_fit_TBV.csv")
print(Parameters_loading(fit_TBV))
sink() 

# Scalar Invariance
Scalar_Invariance_Model<- cfa(TBV.model, data=Boys_Over_107_Abide, check.gradient = F, group = "DX_GROUP", estimator = "MLR", group.equal = c("loadings", "intercepts"))
fitMeasures(Control_fiScalar_Invariance_Modelt_TBV1, c("cfi.robust","tli.robust", "rmsea.robust"))
anova(Metric_Invariance_Model,Scalar_Invariance_Model) 


sink("CSV/anova_tables_Boys_Over_107_Abide.csv")
print(fitMeasures(fit_TBV, c("cfi.robust", "rmsea.robust", "tli.robust")))
print (anova(fit_TBV,Metric_Invariance_Model))
print(fitMeasures(Metric_Invariance_Model, c("cfi.robust", "rmsea.robust", "tli.robust")))
print( anova(Scalar_Invariance_Model,Metric_Invariance_Model) )
print(fitMeasures(Scalar_Invariance_Model, c("cfi.robust", "rmsea.robust", "tli.robust")))
sink()


sink("CSV/A_Boys_Over_107_Abide.csv")
print(fitMeasures(fit_TBV, c("cfi.robust", "rmsea.robust", "tli.robust")))
print(fitMeasures(ASD_fit_TBV1, c("cfi.robust","tli.robust", "rmsea.robust")))
print(fitMeasures(Control_fit_TBV1, c("cfi.robust","tli.robust", "rmsea.robust")))
sink()

sink("CSV/parameterestimates_configural_Boys_Over_107_Abide.csv")
options(max.print= 200000)
print(Parameters_loading(fit_TBV))
print(Parameters_intercept(fit_TBV))
sink()



