library(stringi)
library (lavaan) 
library(semTools)

# Run MGCFA_Boys_Over_FSIQ.R before running this script to obtain the Boys_Over_107 Sample 

Corr_Base <- "\nTBV ~~ 1*TBV
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
Left_Accumbens_Area_log ~~ Right_Hippocampus_log"

Boys_Over_107_Abide_constrained_local_metric_invariance <- Metric_Invariance_Model

Boys_Over_107_Abide_CorticalWhiteMatterlog.model <- paste("TBV  =~ NA*CorticalWhiteMatterVol_log
                                                          + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                          + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                          + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                          + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                          + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                          + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                          + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                          + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                          + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                          + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                          + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                          + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                          + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                          + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                          + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                          + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                          + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                          + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                          + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                          + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                          + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_CorticalWhiteMatterlog.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_CorticalWhiteMatterlog.model, " ", "")

Boys_Over_107_Abide_CorticalWhiteMatterlog_metric_invariance <- cfa(Boys_Over_107_Abide_CorticalWhiteMatterlog.model_Final, 
                                                                    data=Boys_Over_107_Abide, 
                                                                    group = "DX_GROUP", 
                                                                    check.gradient = F, 
                                                                    estimator = "MLR") 
anova(Boys_Over_107_Abide_CorticalWhiteMatterlog_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)
print(fitMeasures(Boys_Over_107_Abide_constrained_local_metric_invariance, c("cfi.robust", "rmsea.robust", "tli.robust")))
print(fitMeasures(Boys_Over_107_Abide_CorticalWhiteMatterlog_metric_invariance, c("cfi.robust", "rmsea.robust", "tli.robust")))
lavInspect(Boys_Over_107_Abide_CorticalWhiteMatterlog_metric_invariance, "theta")

Boys_Over_107_Abide_Brain_Stem_log.model <- paste("TBV  =~ NA*Brain_Stem_log
                                                  + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                  + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                  + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                  + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                  + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                  + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                  + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                  + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                  + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                  + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                  + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                  + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                  + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                  + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                  + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                  + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                  + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                  + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                  + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                  + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                  + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)*CorticalWhiteMatterVol_log", Corr_Base)
Boys_Over_107_Abide_Brain_Stem_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Brain_Stem_log.model, " ", "")
Boys_Over_107_Abide_Brain_Stem_log_metric_invariance <- cfa(Boys_Over_107_Abide_Brain_Stem_log.model_Final, data=Boys_Over_107_Abide,group = "DX_GROUP", check.gradient = F, estimator = "MLR") 
anova(Boys_Over_107_Abide_Brain_Stem_log_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance) # very sig 


Boys_Over_107_Abide_Left_Thalamus_Proper.model <- paste("TBV  =~ NA*Left_Thalamus_Proper_log
                                                        + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                        + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                        + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                        + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                        + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                        + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                        + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                        + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                        + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                        + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                        + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                        + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                        + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                        + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                        + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                        + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                        + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                        + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                        + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                        + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                        + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Left_Thalamus_Proper.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Left_Thalamus_Proper.model, " ", "")
Boys_Over_107_Abide_Left_Thalamus_Proper_metric_invariance <- cfa(Boys_Over_107_Abide_Left_Thalamus_Proper.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator = "MLR")  
anova(Boys_Over_107_Abide_Left_Thalamus_Proper_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)

Boys_Over_107_Abide_Right_Thalamus_Proper.model <- paste("TBV  =~ NA*Right_Thalamus_Proper_log
                                                         + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                         + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                         + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                         + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                         + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                         + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                         + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                         + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                         + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                         + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                         + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                         + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                         + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                         + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                         + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                         + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                         + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                         + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                         + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                         + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                         + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Right_Thalamus_Proper.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Right_Thalamus_Proper.model, " ", "")
Boys_Over_107_Abide_Right_Thalamus_Proper_metric_invariance <- cfa(Boys_Over_107_Abide_Right_Thalamus_Proper.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator = "MLR") 
anova(Boys_Over_107_Abide_Right_Thalamus_Proper_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)


# PUTAMEN 
Boys_Over_107_Abide_Left_Putamen_log.model <- paste("TBV  =~ NA*Left_Putamen_log 
                                                    + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                    + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                    + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                    + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                    + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                    + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                    + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                    + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                    + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                    + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                    + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                    + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                    + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                    + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                    + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                    + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                    + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                    + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                    + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                    + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                    + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Left_Putamen_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Left_Putamen_log.model, " ", "")

Boys_Over_107_Abide_Left_Putamen_log_metric_invariance <- cfa(Boys_Over_107_Abide_Left_Putamen_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator = "MLR") 
anova(Boys_Over_107_Abide_Left_Putamen_log_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)

Boys_Over_107_Abide_Right_Putamen_log.model <- paste("TBV  =~ NA*Right_Putamen_log 
                                                     + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                     + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                     + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                     + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                     + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                     + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                     + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                     + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                     + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                     + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log
                                                     + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                     + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                     + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                     + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                     + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                     + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                     + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                     + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                     + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                     + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                     + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Right_Putamen_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Right_Putamen_log.model, " ", "")

Boys_Over_107_Abide_Right_Putamen_log_metric_invariance <- cfa(Boys_Over_107_Abide_Right_Putamen_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator = "MLR") 
anova(Boys_Over_107_Abide_Right_Putamen_log_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)


Boys_Over_107_Abide_Left_Pallidum_log.model <- paste("TBV  =~ NA*Left_Pallidum_log 
                                                     + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                     + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                     + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                     + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                     + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                     + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                     + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                     + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                     + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                     + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                     + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                     + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                     + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                     + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                     + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                     + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                     + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                     + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                     + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                     + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                     + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Left_Pallidum_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Left_Pallidum_log.model, " ", "")
Boys_Over_107_Abide_Left_Pallidum_log_metric_invariance <- cfa(Boys_Over_107_Abide_Left_Pallidum_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator = "MLR") 
anova(Boys_Over_107_Abide_Left_Pallidum_log_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)
print(fitMeasures(Boys_Over_107_Abide_Left_Pallidum_log_metric_invariance, c("cfi.robust", "rmsea.robust", "tli.robust")))
print(fitMeasures(Boys_Over_107_Abide_constrained_local_metric_invariance, c("cfi.robust", "rmsea.robust", "tli.robust")))

Boys_Over_107_Abide_Right_Pallidum_log.model <- paste("TBV  =~ NA*Right_Pallidum_log 
                                                      + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                      + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                      + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                      + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                      + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                      + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                      + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                      + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                      + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                      + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                      + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                      + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                      + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                      + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                      + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                      + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                      + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                      + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                      + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                      + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                      + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Right_Pallidum_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Right_Pallidum_log.model, " ", "")

Boys_Over_107_Abide_Right_Pallidum_log_metric_invariance <- cfa(Boys_Over_107_Abide_Right_Pallidum_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator ="MLR") 
anova(Boys_Over_107_Abide_Right_Pallidum_log_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)

# HIPPOCAMPUS 
Boys_Over_107_Abide_Left_Hippocampus_log.model <- paste("TBV  =~ NA*Left_Hippocampus_log 
                                                        + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                        + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                        + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                        + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                        + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                        + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                        + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                        + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                        + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                        + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                        + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                        + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                        + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                        + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                        + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                        + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                        + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                        + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                        + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                        + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                        + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Left_Hippocampus_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Left_Hippocampus_log.model, " ", "")

Boys_Over_107_Abide_Left_Hippocampus_log_metric_invariance <- cfa(Boys_Over_107_Abide_Left_Hippocampus_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator ="MLR") 

anova(Boys_Over_107_Abide_Left_Hippocampus_log_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)
print(fitMeasures(Boys_Over_107_Abide_constrained_local_metric_invariance, c("cfi.robust", "rmsea.robust", "tli.robust")))
print(fitMeasures(Boys_Over_107_Abide_Left_Hippocampus_log_metric_invariance, c("cfi.robust", "rmsea.robust", "tli.robust")))


Boys_Over_107_Abide_Right_Hippocampus_log.model <- paste("TBV  =~ NA*Right_Hippocampus_log 
                                                         + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                         + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                         + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                         + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                         + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                         + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                         + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                         + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                         + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                         + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                         + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                         + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log
                                                         + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                         + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                         + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                         + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                         + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                         + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                         + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                         + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                         + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Right_Hippocampus_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Right_Hippocampus_log.model, " ", "")
Boys_Over_107_Abide_Right_Hippocampus_log_metric_invariance <- cfa(Boys_Over_107_Abide_Right_Hippocampus_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator ="MLR") 
anova(Boys_Over_107_Abide_Right_Hippocampus_log_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)
fitMeasures(Boys_Over_107_Abide_Right_Hippocampus_log_metric_invariance, c("cfi.robust", "rmsea.robust", "srmr"))

Boys_Over_107_Abide_Left_Cerebellum_Cortex_log.model <-paste("TBV  =~ NA*Left_Cerebellum_Cortex_log 
                                                             + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                             + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                             + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                             + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                             + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                             + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                             + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                             + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                             + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                             + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                             + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                             + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                             + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                             + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                             + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                             + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                             + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                             + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                             + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                             + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                             + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Left_Cerebellum_Cortex_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Left_Cerebellum_Cortex_log.model, " ", "")

Boys_Over_107_Abide_Left_Cerebellum_Cortex_log_metric_invariance <- cfa(Boys_Over_107_Abide_Left_Cerebellum_Cortex_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator ="MLR") 

anova(Boys_Over_107_Abide_Left_Cerebellum_Cortex_log_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)

Boys_Over_107_Abide_Right_Cerebellum_Cortex_log.model <- paste("TBV  =~ NA*Right_Cerebellum_Cortex_log 
                                                               + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                               + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                               + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                               + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                               + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                               + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                               + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                               + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                               + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                               + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                               + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                               + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                               + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log
                                                               + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                               + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                               + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                               + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                               + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                               + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                               + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                               + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Right_Cerebellum_Cortex_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Right_Cerebellum_Cortex_log.model, " ", "")
Boys_Over_107_Abide_Right_Cerebellum_Cortex_log_metric_invariance <- cfa(Boys_Over_107_Abide_Right_Cerebellum_Cortex_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator ="MLR") 
anova(Boys_Over_107_Abide_Right_Cerebellum_Cortex_log_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)

Boys_Over_107_Abide_Left_Caudate_log.model <- paste("TBV  =~ NA*Left_Caudate_log
                                                    + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                    + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                    + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                    + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                    + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                    + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                    + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                    + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                    + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                    + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                    + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                    + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                    + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                    + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                    + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                    + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                    + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                    + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                    + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                    + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                    + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Left_Caudate_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Left_Caudate_log.model, " ", "")

Boys_Over_107_Abide_Left_Caudate_log_Final_metric_invariance <- cfa(Boys_Over_107_Abide_Left_Caudate_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator ="MLR") 

anova(Boys_Over_107_Abide_Left_Caudate_log_Final_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)

# Results: Not significant
Boys_Over_107_Abide_Right_Caudate_log.model <- paste("TBV  =~ NA*Right_Caudate_log
                                                     + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                     + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                     + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                     + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                     + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                     + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                     + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                     + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                     + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                     + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                     + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                     + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                     + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                     + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                     + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                     + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                     + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                     + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                     + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                     + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                     + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Right_Caudate_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Right_Caudate_log.model, " ", "")

Boys_Over_107_Abide_Right_Caudate_log_Final_metric_invariance <- cfa(Boys_Over_107_Abide_Right_Caudate_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator ="MLR") 

anova(Boys_Over_107_Abide_Right_Caudate_log_Final_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)

# AMYGDALA 

Boys_Over_107_Abide_Left_Amygdala_log.model <- paste("TBV  =~ NA*Left_Amygdala_log
                                                     + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                     + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                     + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                     + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                     + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                     + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                     + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                     + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                     + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                     + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                     + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                     + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                     + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                     + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                     + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                     + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                     + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                     + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                     + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                     + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                     + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Left_Amygdala_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Left_Amygdala_log.model, " ", "")
Boys_Over_107_Abide_Left_Amygdala_log_Final_metric_invariance <- cfa(Boys_Over_107_Abide_Left_Amygdala_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator = "MLR") 
anova(Boys_Over_107_Abide_Left_Amygdala_log_Final_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)

# Results: Not significant
Boys_Over_107_Abide_Right_Amygdala_log.model <- paste("TBV  =~ NA*Right_Amygdala_log
                                                      + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                      + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                      + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                      + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                      + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                      + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                      + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                      + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                      + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                      + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                      + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                      + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                      + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                      + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                      + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                      + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                      + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                      + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                      + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                      + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                      + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Right_Amygdala_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Right_Amygdala_log.model, " ", "")
Boys_Over_107_Abide_Right_Amygdala_log_Final_metric_invariance <- cfa(Boys_Over_107_Abide_Right_Amygdala_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator ="MLR") 
anova(Boys_Over_107_Abide_Right_Amygdala_log_Final_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)


Boys_Over_107_Abide_Left_Accumbens_Area_log.model <- paste("TBV  =~ NA*Left_Accumbens_Area_log
                                                           + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                           + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                           + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                           + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                           + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                           + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                           + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                           + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                           + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                           + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                           + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                           + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                           + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                           + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                           + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                           + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                           + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                           + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                           + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                           + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                           + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Left_Accumbens_Area_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Left_Accumbens_Area_log.model, " ", "")

Boys_Over_107_Abide_Left_Accumbens_Area_log_Final_metric_invariance <- cfa(Boys_Over_107_Abide_Left_Accumbens_Area_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator ="MLR") 
anova(Boys_Over_107_Abide_Left_Accumbens_Area_log_Final_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)

# Results: Not significant
Boys_Over_107_Abide_Right_Accumbens_Area_log.model <- paste("TBV  =~ NA*Right_Accumbens_Area_log
                                                            + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                            + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                            + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                            + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                            + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                            + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                            + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                            + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                            + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                            + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                            + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                            + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                            + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                            + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                            + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                            + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                            + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                            + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                            + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                            + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                            + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Right_Accumbens_Area_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Right_Accumbens_Area_log.model, " ", "")

Boys_Over_107_Abide_Right_Accumbens_Area_log_Final_metric_invariance <- cfa(Boys_Over_107_Abide_Right_Accumbens_Area_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator = "MLR") 
anova(Boys_Over_107_Abide_Right_Accumbens_Area_log_Final_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)

# Results: Not significant

# CORTEX
Boys_Over_107_Abide_lhCortexVol_log.model <- paste("TBV  =~ NA*lhCortexVol_log
                                                   + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                   + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                   + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                   + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                   + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                   + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                   + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                   + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                   + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                   + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                   + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                   + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                   + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                   + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                   + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                   + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                   + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                   + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                   + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                   + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                   + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_lhCortexVol_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_lhCortexVol_log.model, " ", "")
Boys_Over_107_Abide_lhCortexVol_log_Final_metric_invariance <- cfa(Boys_Over_107_Abide_lhCortexVol_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator = "MLR") 
anova(Boys_Over_107_Abide_lhCortexVol_log_Final_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)

# Results: Not significant
Boys_Over_107_Abide_rhCortexVol_log.model <- paste("TBV  =~ NA*rhCortexVol_log
                                                   + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                   + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                   + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                   + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                   + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                   + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                   + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                   + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                   + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                   + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                   + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                   + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                   + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                   + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                   + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                   + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                   + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                   + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                   + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                   + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                   + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base) 
Boys_Over_107_Abide_rhCortexVol_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_rhCortexVol_log.model, " ", "")

Boys_Over_107_Abide_rhCortexVol_log_Final_metric_invariance <- cfa(Boys_Over_107_Abide_rhCortexVol_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator = "MLR") 

anova(Boys_Over_107_Abide_rhCortexVol_log_Final_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)

Boys_Over_107_Abide_Left_Ventral_DC_log.model <-paste("TBV  =~ NA*Left_Ventral_DC_log
                                                      + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                      + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                      + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                      + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                      + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                      + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                      + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                      + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                      + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                      + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                      + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                      + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                      + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                      + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                      + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                      + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                      + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                      + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                      + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                      + c(Right_VentralDC1,Right_VentralDC1)*Right_Ventral_DC_log
                                                      + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Left_Ventral_DC_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Left_Ventral_DC_log.model, " ", "")

Boys_Over_107_Abide_Left_Ventral_DC_log_Final_metric_invariance <- cfa(Boys_Over_107_Abide_Left_Ventral_DC_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator = "MLR") 
anova(Boys_Over_107_Abide_Left_Ventral_DC_log_Final_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)


Boys_Over_107_Abide_Right_Ventral_DC_log.model <- paste("TBV  =~ NA*Right_Ventral_DC_log
                                                        + c(CorticalWhiteMatterVol_log1,CorticalWhiteMatterVol_log1)* CorticalWhiteMatterVol_log
                                                        + c(Left_Thalamus_Proper1,Left_Thalamus_Proper1)*Left_Thalamus_Proper_log
                                                        + c(Left_Putamen1,Left_Putamen1)*Left_Putamen_log 
                                                        + c(Left_Pallidum1,Left_Pallidum1)*Left_Pallidum_log
                                                        + c(Left_Hippocampus1,Left_Hippocampus1)*Left_Hippocampus_log 
                                                        + c(Left_Cerebellum_Cortex1,Left_Cerebellum_Cortex1)*Left_Cerebellum_Cortex_log 
                                                        + c(Left_Caudate1,Left_Caudate1)*Left_Caudate_log
                                                        + c(Left_Amygdala1,Left_Amygdala1)*Left_Amygdala_log
                                                        + c(Left_Accumbens_area1,Left_Accumbens_area1)*Left_Accumbens_Area_log
                                                        + c(Right_Thalamus_Proper1,Right_Thalamus_Proper1)*Right_Thalamus_Proper_log
                                                        + c(Right_Putamen1,Right_Putamen1)*Right_Putamen_log
                                                        + c(Right_Pallidum1,Right_Pallidum1)*Right_Pallidum_log
                                                        + c(Right_Hippocampus1,Right_Hippocampus1)*Right_Hippocampus_log
                                                        + c(Right_Cerebellum_Cortex1,Right_Cerebellum_Cortex1)*Right_Cerebellum_Cortex_log
                                                        + c(Right_Caudate1,Right_Caudate1)*Right_Caudate_log
                                                        + c(Right_Amygdala1,Right_Amygdala1)*Right_Amygdala_log
                                                        + c(Right_Accumbens_area1,Right_Accumbens_area1)*Right_Accumbens_Area_log
                                                        + c(rhCortexVol1,rhCortexVol1)*rhCortexVol_log
                                                        + c(lhCortexVol1,lhCortexVol1)*lhCortexVol_log
                                                        + c(Left_VentralDC1,Left_VentralDC1)*Left_Ventral_DC_log
                                                        + c(Brain_Stem1,Brain_Stem1)*Brain_Stem_log", Corr_Base)
Boys_Over_107_Abide_Right_Ventral_DC_log.model_Final <- stri_replace_all_fixed(Boys_Over_107_Abide_Right_Ventral_DC_log.model, " ", "")
Boys_Over_107_Abide_Right_Ventral_DC_log_Final_metric_invariance <- cfa(Boys_Over_107_Abide_Right_Ventral_DC_log.model_Final, data=Boys_Over_107_Abide, group = "DX_GROUP", check.gradient = F, estimator = "MLR") 
anova(Boys_Over_107_Abide_Right_Ventral_DC_log_Final_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance)

sink ("CSV/results_local_metric_invariance_Boys_Over_107.csv")
print(fitMeasures(Boys_Over_107_Abide_constrained_local_metric_invariance, c("cfi.robust", "rmsea.robust", "srmr")))

print(anova(Boys_Over_107_Abide_Left_Hippocampus_log_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance))
print(fitMeasures(Boys_Over_107_Abide_Left_Hippocampus_log_metric_invariance, c("cfi.robust", "rmsea.robust", "srmr")))

print(anova(Boys_Over_107_Abide_Left_Caudate_log_Final_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance))
print(fitMeasures(Boys_Over_107_Abide_Left_Caudate_log_Final_metric_invariance, c("cfi.robust", "rmsea.robust", "srmr")))

print(anova(Boys_Over_107_Abide_Left_Accumbens_Area_log_Final_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance))
print(fitMeasures(Boys_Over_107_Abide_Left_Accumbens_Area_log_Final_metric_invariance, c("cfi.robust", "rmsea.robust", "srmr")))

print(anova(Boys_Over_107_Abide_Right_Ventral_DC_log_Final_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance))
print(fitMeasures(Boys_Over_107_Abide_Right_Ventral_DC_log_Final_metric_invariance, c("cfi.robust", "rmsea.robust", "srmr")))

print(anova(Boys_Over_107_Abide_CorticalWhiteMatterlog_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance))
print(fitMeasures(Boys_Over_107_Abide_CorticalWhiteMatterlog_metric_invariance, c("cfi.robust", "rmsea.robust", "srmr")))

print(anova(Boys_Over_107_Abide_Left_Pallidum_log_metric_invariance,Boys_Over_107_Abide_constrained_local_metric_invariance))
print(fitMeasures(Boys_Over_107_Abide_Left_Pallidum_log_metric_invariance, c("cfi.robust", "rmsea.robust", "srmr")))

sink()



