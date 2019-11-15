library(lavaan)
library(semTools)
library (dplyr)


Abide_LMEM_log_Clean$DX_GROUP <- factor(Abide_LMEM_log_Clean$DX_GROUP, levels = c("Control", "ASD"))
# Control ASD order as in sample
ASD_Abide_LMEM_log_Clean = Abide_LMEM_log_Clean%>%
  filter(DX_GROUP == "ASD") 
Control_Abide_LMEM_log_Clean = Abide_LMEM_log_Clean%>%
  filter(DX_GROUP == "Control") 

####### BETA ########
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

TBV ~ DX_GROUP'

fit_TBV<- cfa(TBV.model, data=Abide_LMEM_log_Clean, check.gradient = FALSE, estimator = "MLR", fixed.x = T)
summary(fit_TBV, standardize = T) # 0.031 std.lv, std.all
parameterestimates(fit_TBV)

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
TBV ~~ 1*TBV 
TBV ~ c(0, 0.031)* 1
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
Left_Putamen_log	~~	Left_Amygdala_log
Right_Putamen_log ~~ Left_Amygdala_log
Left_Putamen_log	~~	Right_Amygdala_log

Right_Caudate_log	~~	Right_Putamen_log
Left_Caudate_log	~~	Left_Putamen_log
Left_Caudate_log ~~ Right_Putamen_log
Right_Caudate_log~~ Left_Putamen_log
Right_Caudate_log ~~ Right_Pallidum_log
Left_Caudate_log ~~ Left_Pallidum_log
Right_Caudate_log ~~ Left_Pallidum_log
Left_Caudate_log~~Right_Pallidum_log
Right_Caudate_log~~Right_Amygdala_log
Left_Caudate_log~~Right_Amygdala_log

Left_Caudate_log~~Left_Thalamus_Proper_log
Right_Caudate_log~~Left_Thalamus_Proper_log

Left_Pallidum_log ~~ Left_Putamen_log
Right_Pallidum_log ~~ Right_Putamen_log
Left_Pallidum_log~~Right_Putamen_log
Right_Pallidum_log~~Left_Putamen_log

Right_Pallidum_log ~~ Left_Amygdala_log
Right_Pallidum_log~~ Right_Amygdala_log

Right_Pallidum_log ~~ Right_Thalamus_Proper_log
Right_Pallidum_log~~Left_Thalamus_Proper_log
Left_Pallidum_log~~Left_Thalamus_Proper_log

Left_Accumbens_Area_log~~Right_Caudate_log
Left_Accumbens_Area_log~~Left_Caudate_log
Right_Accumbens_Area_log~~Right_Caudate_log
Right_Accumbens_Area_log~~Left_Caudate_log

Left_Hippocampus_log~~Right_Pallidum_log
Right_Hippocampus_log~~Left_Pallidum_log
Right_Hippocampus_log~~Right_Pallidum_log
Left_Hippocampus_log~~Left_Pallidum_log

Left_Hippocampus_log	~~	Left_Amygdala_log
Right_Hippocampus_log	~~	Right_Amygdala_log

Right_Hippocampus_log	~~	Left_Amygdala_log
Left_Hippocampus_log	~~	Right_Amygdala_log

Right_Hippocampus_log~~Right_Putamen_log
Left_Hippocampus_log~~Right_Thalamus_Proper_log

CorticalWhiteMatterVol_log~~Right_Ventral_DC_log
CorticalWhiteMatterVol_log~~Left_Ventral_DC_log
CorticalWhiteMatterVol_log~~Brain_Stem_log
CorticalWhiteMatterVol_log~~Right_Amygdala_log
CorticalWhiteMatterVol_log~~Right_Caudate_log
CorticalWhiteMatterVol_log~~Right_Putamen_log
CorticalWhiteMatterVol_log~~Right_Thalamus_Proper_log
CorticalWhiteMatterVol_log~~Left_Thalamus_Proper_log

Brain_Stem_log ~~ Left_Cerebellum_Cortex_log
Brain_Stem_log ~~ Right_Cerebellum_Cortex_log
Brain_Stem_log~~Left_Pallidum_log
Brain_Stem_log~~Right_Pallidum_log
Brain_Stem_log~~Left_Thalamus_Proper_log
Brain_Stem_log~~Right_Thalamus_Proper_log
Brain_Stem_log ~~ Right_Ventral_DC_log
Brain_Stem_log ~~ Left_Ventral_DC_log
Brain_Stem_log~~lhCortexVol_log
Brain_Stem_log~~rhCortexVol_log
Brain_Stem_log~~Left_Accumbens_Area_log
Brain_Stem_log~~Left_Caudate_log

CorticalWhiteMatterVol_log~~Left_Putamen_log
CorticalWhiteMatterVol_log~~Right_Pallidum_log
CorticalWhiteMatterVol_log~~Left_Pallidum_log
CorticalWhiteMatterVol_log~~rhCortexVol_log
CorticalWhiteMatterVol_log~~lhCortexVol_log
CorticalWhiteMatterVol_log~~Right_Hippocampus_log
CorticalWhiteMatterVol_log~~Left_Hippocampus_log
CorticalWhiteMatterVol_log~~Left_Amygdala_log
CorticalWhiteMatterVol_log~~Right_Cerebellum_Cortex_log
CorticalWhiteMatterVol_log~~Left_Cerebellum_Cortex_log
CorticalWhiteMatterVol_log~~Left_Accumbens_Area_log
CorticalWhiteMatterVol_log~~Right_Accumbens_Area_log

Left_Accumbens_Area_log~~Left_Putamen_log
Left_Accumbens_Area_log~~Right_Putamen_log
Right_Accumbens_Area_log~~Right_Putamen_log
Right_Accumbens_Area_log~~Left_Putamen_log

Left_Accumbens_Area_log~~Right_Amygdala_log
Left_Accumbens_Area_log~~Left_Amygdala_log
Right_Accumbens_Area_log~~Right_Amygdala_log
Right_Accumbens_Area_log~~Left_Amygdala_log

Left_Accumbens_Area_log ~~ Right_Hippocampus_log
Right_Accumbens_Area_log ~~ Right_Hippocampus_log

Right_Accumbens_Area_log~~Left_Pallidum_log
Left_Accumbens_Area_log~~Left_Pallidum_log
Right_Accumbens_Area_log~~Right_Pallidum_log
Left_Accumbens_Area_log~~Right_Pallidum_log

lhCortexVol_log~~Left_Amygdala_log
rhCortexVol_log~~Right_Amygdala_log
lhCortexVol_log~~Right_Amygdala_log

lhCortexVol_log~~Right_Putamen_log
rhCortexVol_log ~~ Right_Putamen_log
lhCortexVol_log ~~ Left_Putamen_log

lhCortexVol_log ~~ Left_Pallidum_log
rhCortexVol_log ~~ Right_Pallidum_log
rhCortexVol_log~~Left_Pallidum_log

rhCortexVol_log~~Right_Caudate_log
lhCortexVol_log~~Left_Caudate_log
lhCortexVol_log~~Right_Caudate_log
rhCortexVol_log~~Left_Caudate_log

lhCortexVol_log~~Right_Hippocampus_log
rhCortexVol_log~~Right_Hippocampus_log
lhCortexVol_log~~Left_Hippocampus_log
rhCortexVol_log~~Left_Hippocampus_log

Left_Ventral_DC_log~~Left_Pallidum_log
Right_Ventral_DC_log~~Right_Pallidum_log
Left_Ventral_DC_log~~Right_Pallidum_log

Left_Ventral_DC_log~~Left_Thalamus_Proper_log
Left_Ventral_DC_log~~Right_Thalamus_Proper_log
Right_Ventral_DC_log~~Left_Thalamus_Proper_log
Right_Ventral_DC_log~~Right_Thalamus_Proper_log

Left_Ventral_DC_log ~~ Left_Caudate_log
Left_Ventral_DC_log~~ Right_Caudate_log
Right_Ventral_DC_log ~~ Right_Caudate_log

Left_Ventral_DC_log~~Right_Amygdala_log
Right_Ventral_DC_log~~Right_Amygdala_log

Left_Accumbens_Area_log~~Left_Cerebellum_Cortex_log
Left_Accumbens_Area_log~~Right_Cerebellum_Cortex_log
Left_Ventral_DC_log~~rhCortexVol_log
Left_Ventral_DC_log~~Left_Cerebellum_Cortex_log
Left_Ventral_DC_log~~Left_Hippocampus_log
Left_Ventral_DC_log~~Left_Putamen_log

Left_Cerebellum_Cortex_log~~Left_Thalamus_Proper_log
Left_Cerebellum_Cortex_log~~Right_Thalamus_Proper_log

Left_Cerebellum_Cortex_log~~Left_Caudate_log
Right_Cerebellum_Cortex_log~~Right_Caudate_log
Right_Cerebellum_Cortex_log~~Left_Caudate_log

Left_Cerebellum_Cortex_log~~Right_Amygdala_log
Left_Cerebellum_Cortex_log~~Left_Amygdala_log
'

#### Function for Parameters ####
Parameters_loading <- function (x) {parameterestimates(x, standardize = T) %>% filter(op == "=~")}
Parameters_intercept <- function (x) {parameterestimates(x, standardize = T) %>% filter(op == "~1")}
######################### CONFIGURAL MODEL ##########################

### PER GROUP - Remove " TBV ~ c(0, 0.031)* 1 " from the TBV0.model or the model will not run 

fit_TBV_ASD_Abide_LMEM_log <- cfa(TBV0.model, data=ASD_Abide_LMEM_log_Clean, check.gradient = FALSE, estimator = "MLR", std.lv = T)
summary(fit_TBV_ASD_Abide_LMEM_log, standardize = T, fit.measures = T)
print(fitMeasures(fit_TBV_ASD_Abide_LMEM_log, c("cfi.robust", "tli.robust", "rmsea.robust")))

fit_TBV_Control_Abide_LMEM_log <- cfa(TBV0.model, data=Control_Abide_LMEM_log_Clean, check.gradient = FALSE, estimator = "MLR",std.lv = T)
summary(fit_TBV_Control_Abide_LMEM_log, standardize = T, fit.measures = T)
print(fitMeasures(fit_TBV_Control_Abide_LMEM_log, c("cfi.robust", "tli.robust", "rmsea.robust")))

##### ALL GROUPS - 
fit_TBV<- cfa(TBV0.model, data=Abide_LMEM_log_Clean, check.gradient = FALSE, group = "DX_GROUP",
              estimator = "MLR")
summary(fit_TBV, standardize = T, fit.measures = T)

moreFitIndices(fit_TBV, fit.measures = "all", nPrior = 1)


print(summary(fit_TBV, standardize = T, fit.measures = T, rsquare = T))
print(fitMeasures(fit_TBV, c("cfi.robust", "tli.robust", "rmsea.robust")))
parameterestimates(fit_TBV, standardized = T)
parameterestimates(fit_TBV_ASD_Abide_LMEM_log, standardized = T)
lavInspect(fit_TBV, "cov.all") # The model-implied variance-covariance matrix of both the observed and latent variables.
lavInspect(fit_TBV, "rsquare")

sink("CSV/parameterestimates_configural.csv")
options(max.print= 200000)
print(Parameters_loading(fit_TBV))
print(Parameters_intercept(fit_TBV))
sink()

sink("CSV/entire_sample_communality.csv")
lavInspect(fit_TBV, "rsquare")
sink()

######################### METRIC MODEL ##########################
Metric_Invariance_Model<- cfa(TBV0.model, data=Abide_LMEM_log_Clean, group = "DX_GROUP", check.gradient = FALSE, group.equal = c("loadings"),
                              estimator = "MLR")
moreFitIndices(fit_TBV, fit.measures = "all", nPrior = 1)
print(fitMeasures(Metric_Invariance_Model, c("cfi.robust", "tli.robust", "rmsea.robust")))
anova(Metric_Invariance_Model, fit_TBV)

sink("CSV/parameterestimates_metric_configural.csv")
options(max.print= 200000)
print(Parameters_loading(Metric_Invariance_Model))
print(Parameters_intercept(Metric_Invariance_Model))

sink("CSV/anova_configural_metric.csv")
print(anova(Metric_Invariance_Model, fit_TBV))
sink()

######################### SCALAR MODEL ##########################

Scalar_Invariance_Model<- cfa(TBV0.model, data=Abide_LMEM_log_Clean,
                              group = "DX_GROUP", 
                              group.equal = c("loadings", "intercepts"), check.gradient = FALSE, estimator = "MLR")
anova(Metric_Invariance_Model, Scalar_Invariance_Model)
print(fitMeasures(Scalar_Invariance_Model, c("cfi.robust", "tli.robust", "rmsea.robust")))


compareFit(scalar = Scalar_Invariance_Model, metric = Metric_Invariance_Model, config = fit_TBV,
           argsLRT = list(asymptotic = TRUE,
                          method = "satorra.bentler.2010"))

sink("CSV/parameterestimates_anova_scalar.csv")
options(max.print= 200000)
print(Parameters_loading(fit_TBV))
print(Parameters_intercept(fit_TBV))
print(fitMeasures(fit_TBV, c("cfi.robust", "rmsea.robust", "tli.robust")))
print (anova(fit_TBV,Metric_Invariance_Model))
print(fitMeasures(Metric_Invariance_Model, c("cfi.robust", "rmsea.robust", "tli.robust")))
print( anova(Scalar_Invariance_Model,Metric_Invariance_Model) )
print(fitMeasures(Scalar_Invariance_Model, c("cfi.robust", "rmsea.robust", "tli.robust")))
sink()

sink("CSV/Entire_Sample_Model_Fit_Indices.csv")
print(fitMeasures(fit_TBV, c("cfi.robust", "tli.robust","rmsea.robust")))
print(fitMeasures(fit_TBV_ASD_Abide_LMEM_log, c("cfi.robust","tli.robust", "rmsea.robust")))
print(fitMeasures(fit_TBV_Control_Abide_LMEM_log, c("cfi.robust","tli.robust", "rmsea.robust")))
sink()


