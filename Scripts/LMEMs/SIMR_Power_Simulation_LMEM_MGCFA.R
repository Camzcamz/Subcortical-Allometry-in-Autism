library(lmerTest)
library(simr)
library(dplyr)

Abide_LMEM = Abide_LMEM_log_Clean_scaled
Abide_LMEM$FSIQ <- scale(Abide_LMEM$FIQ2)
Abide_LMEM$AGE <- scale(Abide_LMEM$AGE_AT_SCAN)
Abide_LMEM$AGE2 <- scale(Abide_LMEM$Age2)
Abide_LMEM$SITE_ID2 <- as.factor(Abide_LMEM$SITE_ID2)
Abide_LMEM$DX_GROUP <- factor(Abide_LMEM$DX_GROUP, levels = c("Control", "ASD"))

## TotalGrayVol_log

TotalGrayVol_log_model <- lmer(TotalGrayVol_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                               Abide_LMEM)

fixef(TotalGrayVol_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(TotalGrayVol_log_model, alpha = 0.05, nsim = 500, test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(TotalGrayVol_log_model)["DX_GROUPASD"] <- 0.2
powerSim(TotalGrayVol_log_model, alpha = 0.05, nsim = 500, test = fixed("DX_GROUPASD", "sa"))

## Right_Thalamus_Proper_log

Right_Thalamus_Proper_log_model <- lmer(Right_Thalamus_Proper_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                                        Abide_LMEM)

fixef(Right_Thalamus_Proper_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(Right_Thalamus_Proper_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(Right_Thalamus_Proper_log_model)["DX_GROUPASD"] <- 0.2
powerSim(Right_Thalamus_Proper_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD", "sa"))

## Right_Thalamus_Proper_log

Left_Thalamus_Proper_log_model <- lmer(Left_Thalamus_Proper_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                                       Abide_LMEM)

fixef(Left_Thalamus_Proper_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(Left_Thalamus_Proper_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(Left_Thalamus_Proper_log_model)["DX_GROUPASD"] <- 0.2
powerSim(Left_Thalamus_Proper_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD", "sa"))

## Right_Putamen_log

Right_Putamen_log_model <- lmer(Right_Putamen_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                                Abide_LMEM)

fixef(Right_Putamen_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(Right_Putamen_log_model, alpha = 0.05,nsim = 500, test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(Right_Putamen_log_model)["DX_GROUPASD"] <- 0.2
powerSim(Right_Putamen_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD", "sa"))

## Right_Putamen_log

Left_Putamen_log_model <- lmer(Left_Putamen_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                                Abide_LMEM)

fixef(Left_Putamen_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(Left_Putamen_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(Left_Putamen_log_model)["DX_GROUPASD"] <- 0.2
powerSim(Left_Putamen_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD", "sa"))

## Right_Pallidum_log

Right_Pallidum_log_model <- lmer(Right_Pallidum_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                                 Abide_LMEM)

fixef(Right_Pallidum_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(Right_Pallidum_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(Right_Pallidum_log_model)["DX_GROUPASD"] <- 0.2
powerSim(Right_Pallidum_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD", "sa"))

## Right_Pallidum_log

Left_Pallidum_log_model <- lmer(Left_Pallidum_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                                 Abide_LMEM)

fixef(Left_Pallidum_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(Left_Pallidum_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(Left_Pallidum_log_model)["DX_GROUPASD"] <- 0.2
powerSim(Left_Pallidum_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD", "sa"))

## Right_Hippocampus_log

Right_Hippocampus_log_model <- lmer(Right_Hippocampus_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                                    Abide_LMEM)

fixef(Right_Hippocampus_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(Right_Hippocampus_log_model, alpha = 0.05,nsim = 500, test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(Right_Hippocampus_log_model)["DX_GROUPASD"] <- 0.2
powerSim(Right_Hippocampus_log_model, alpha = 0.05,nsim = 500, test = fixed("DX_GROUPASD", "sa"))

## Right_Hippocampus_log

Right_Hippocampus_log_model <- lmer(Right_Hippocampus_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                                    Abide_LMEM)

fixef(Right_Hippocampus_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(Right_Hippocampus_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(Right_Hippocampus_log_model)["DX_GROUPASD"] <- 0.2
powerSim(Right_Hippocampus_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD", "sa"))

## Right_Cerebellum_Cortex_log

Right_Cerebellum_Cortex_log_model <- lmer(Right_Cerebellum_Cortex_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                                          Abide_LMEM)

fixef(Right_Cerebellum_Cortex_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(Right_Cerebellum_Cortex_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(Right_Cerebellum_Cortex_log_model)["DX_GROUPASD"] <- 0.2
powerSim(Right_Cerebellum_Cortex_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD", "sa"))

## Right_Cerebellum_Cortex_log

Right_Cerebellum_Cortex_log_model <- lmer(Right_Cerebellum_Cortex_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                                          Abide_LMEM)

fixef(Right_Cerebellum_Cortex_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(Right_Cerebellum_Cortex_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(Right_Cerebellum_Cortex_log_model)["DX_GROUPASD"] <- 0.2
powerSim(Right_Cerebellum_Cortex_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD", "sa"))

## Right_Caudate_log

Right_Caudate_log_model <- lmer(Right_Caudate_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                                Abide_LMEM)

fixef(Right_Caudate_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(Right_Caudate_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(Right_Caudate_log_model)["DX_GROUPASD"] <- 0.2
powerSim(Right_Caudate_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD", "sa"))

## Right_Caudate_log

Right_Caudate_log_model <- lmer(Right_Caudate_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                                Abide_LMEM)


fixef(Right_Caudate_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(Right_Caudate_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(Right_Caudate_log_model)["DX_GROUPASD"] <- 0.2
powerSim(Right_Caudate_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD", "sa"))

## Right_Amygdala_log

Right_Amygdala_log_model <- lmer(Right_Amygdala_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                                 Abide_LMEM)


fixef(Right_Amygdala_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(Right_Amygdala_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(Right_Amygdala_log_model)["DX_GROUPASD"] <- 0.2
powerSim(Right_Amygdala_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD", "sa"))

## Right_Amygdala_log

Right_Amygdala_log_model <- lmer(Right_Amygdala_log ~ DX_GROUP*Total_Brain_Vol_log+ (1|SITE_ID2),
                                 Abide_LMEM)

fixef(Right_Amygdala_log_model)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2
powerSim(Right_Amygdala_log_model, alpha = 0.05,nsim = 500, test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))
fixef(Right_Amygdala_log_model)["DX_GROUPASD"] <- 0.2
powerSim(Right_Amygdala_log_model, alpha = 0.05, nsim = 500,test = fixed("DX_GROUPASD", "sa"))


