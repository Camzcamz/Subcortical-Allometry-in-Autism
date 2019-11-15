library(dplyr) 
library(lmerTest) 

# Functions 
coef <- function (x) { summary(x)$coefficients}
p_FDR <- function (x) { p.adjust(summary(x)$coefficients[,5], method = 'fdr')}

### Create DF  
# No log DF 
Abide_LMEM_Rep_Clean_data_long <- Abide_Zhang_DF
Abide_LMEM_Rep_Clean_data_long$DX_GROUP <- factor(Abide_LMEM_Rep_Clean_data_long$DX_GROUP, levels = c("Control", "ASD"))
Abide_LMEM_Rep_Clean_data_long$AGE_AT_SCAN_1 <- scale (Abide_LMEM_Rep_Clean_data_long$AGE_AT_SCAN, scale = F)
Abide_LMEM_Rep_Clean_data_long$Age2 <- scale((Abide_LMEM_Rep_Clean_data_long$AGE_AT_SCAN *Abide_LMEM_Rep_Clean_data_long$AGE_AT_SCAN), scale = F)

# Log DF 
Abide_MGCFA_data_long <- Abide_Zhang_log_DF
Abide_MGCFA_data_long$DX_GROUP <- factor(Abide_MGCFA_data_long$DX_GROUP, levels = c("Control", "ASD"))
Abide_MGCFA_data_long$AGE_AT_SCAN <- scale (Abide_MGCFA_data_long$AGE_AT_SCAN, scale = F)
Abide_MGCFA_data_long$Age2 <- scale (Abide_MGCFA_data_long$Age2, scale = F)

### Create Models 

# 1. Hippocampus
Hippocampus_model_Age <- lmer(Hippocampus ~ DX_GROUP * SEX* AGE_AT_SCAN_1 + Hemisphere + (1|SITE_ID2) + (1 | SUB_ID),
                           Abide_LMEM_Rep_Clean_data_long)
Hippocampus_model_Age2 <- lmer(Hippocampus ~ DX_GROUP *Age2*SEX + Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                           Abide_LMEM_Rep_Clean_data_long)
Hippocampus_model3 <- lmer(Hippocampus ~ Total_Brain_Vol + DX_GROUP *AGE_AT_SCAN*SEX + Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                           Abide_LMEM_Rep_Clean_data_long)
Hippocampus_model4 <- lmer(Hippocampus ~ Total_Brain_Vol + DX_GROUP *Age2*SEX + Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                           Abide_LMEM_Rep_Clean_data_long)
Hippocampus_model5 <- lmer(Hippocampus ~ Total_Brain_Vol_log * DX_GROUP *AGE_AT_SCAN*SEX + Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                           Abide_MGCFA_data_long)
Hippocampus_model6 <- lmer(Hippocampus ~ Total_Brain_Vol_log * DX_GROUP * Age2*SEX + Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                           Abide_MGCFA_data_long)

Hippocampus_list = c(Hippocampus_model_Age, Hippocampus_model_Age2, Hippocampus_model3, Hippocampus_model4, Hippocampus_model5, Hippocampus_model6)
lapply(Hippocampus_list, summary)
Hippocampus_list_coef<- lapply(Hippocampus_list, coef)
Hippocampus_list_FDR<- lapply(Hippocampus_list, p_FDR)
names(Hippocampus_list_coef) <- c("No_TBV_Age", "No_TBV_Age2", "TBV_Covariate_Age", "TBV_Covariate_Age2", "TBV_Allometric_Age","TBV_Allometric_Age2")
names(Hippocampus_list_FDR) <- c("No_TBV_Age", "No_TBV_Age2", "TBV_Covariate_Age", "TBV_Covariate_Age2", "TBV_Allometric_Age","TBV_Allometric_Age2")

sink("CSV/Hippocampus_list.csv") 
print(Hippocampus_list_coef)
sink()

###### For the Hippocmapus: Get Table for DX_GROUP *AGE_AT_SCAN*SEX interaction and Cohen's D 
Hippocampal_list_model_1 = c(Hippocampus_model_Age, Hippocampus_model_Age2)
Hippocampal_list_model_2 = c(Hippocampus_model3, Hippocampus_model4)
Hippocampal_list_model_3 = c(Hippocampus_model5, Hippocampus_model6)
Hippocampal_list_model_5 = c(Hippocampus_model5)
Hippocampal_list_model_6 = c(Hippocampus_model6)

Hippocampal_list_model_summary <- lapply(Hippocampal_list_model_6, function(x) {summary(x)$coefficients})
Hippocampal_list_model_summary_1_2 <- lapply(Hippocampal_list_model_1, function(x) {summary(x)$coefficients[c(9,18,45)]}) 
Hippocampal_list_model_summary_3_4 <- lapply(Hippocampal_list_model_2, function(x) {summary(x)$coefficients[c(10,20,50)]}) 
Hippocampal_list_model_summary_5 <- lapply(Hippocampal_list_model_5, function(x) {summary(x)$coefficients[c(11,22,55)]}) 
Hippocampal_list_model_summary_6 <- lapply(Hippocampal_list_model_6, function(x) {summary(x)$coefficients[c(16,33,84)]})

Hippocampal_list_model_summary_list <- c(Hippocampal_list_model_summary_1_2,Hippocampal_list_model_summary_3_4,
                                         Hippocampal_list_model_summary_5,Hippocampal_list_model_summary_6)
Hippocampal_list_model_summary_DF <- data.frame(matrix(unlist(Hippocampal_list_model_summary_list), nrow=length(Hippocampal_list_model_summary_list), byrow=T))
colnames(Hippocampal_list_model_summary_DF) <- c("Beta", "SE", "p")
rownames(Hippocampal_list_model_summary_DF) <- c("No_TBV_Age", "No_TBV_Age2", "TBV_Covariate_Age", "TBV_Covariate_Age2", "TBV_Allometric_Age","TBV_Allometric_Age2")
Hippocampal_list_model_summary_DF

# calculate Cohen's D
std <- function(x) {sqrt(654) * x}
List_SE <- as.list(Hippocampal_list_model_summary_DF$SE)
List_SD <- lapply(List_SE,std)
Hippocampal_list_model_summary_DF$SD <- unlist(List_SD)
Hippocampal_list_model_summary_DF$Cohen_D <- (Hippocampal_list_model_summary_DF$Beta/Hippocampal_list_model_summary_DF$SD)
Hippocampal_list_model_summary_DF

sink("Hippocampus_Table_Interaction.csv") 
print(Hippocampal_list_model_summary_DF)
sink() 

## Follow Up 
Follow_up <- Abide_LMEM_Rep_Clean_data_long %>% 
  filter(DX_GROUP == "ASD")%>% 
  filter(SEX == "Female")

Hippocampus_model2 <- lmer(Hippocampus ~  AGE_AT_SCAN_1 + Hemisphere+ (1|SUB_ID)+ (1|SITE_ID2) ,
                           Follow_up)

summary(Hippocampus_model2)
p_FDR(Hippocampus_model2)

Follow_up <- Abide_LMEM_Rep_Clean_data_long %>% 
  filter(AGE_AT_SCAN >= 20 & AGE_AT_SCAN < 27)%>% 
  filter(DX_GROUP == "ASD")
table(Follow_up$SEX)
Hippocampus_model2 <- lmer(Hippocampus ~  SEX + Hemisphere + (1|SITE_ID2) + (1 | SUB_ID),
                           Follow_up)

summary(Hippocampus_model2)
p_FDR(Hippocampus_model2)

####### Caudate 

Caudate_model1 <- lmer(Caudate ~ DX_GROUP + AGE_AT_SCAN  + Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                       Abide_LMEM_Rep_Clean_data_long)
Caudate_model2 <- lmer(Caudate ~ DX_GROUP * Age2 * Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                       Abide_LMEM_Rep_Clean_data_long)
Caudate_model3 <- lmer(Caudate ~ Total_Brain_Vol + DX_GROUP + AGE_AT_SCAN + Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                       Abide_LMEM_Rep_Clean_data_long)
Caudate_model4 <- lmer(Caudate ~ Total_Brain_Vol * DX_GROUP * Age2* Hemisphere+SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                       Abide_LMEM_Rep_Clean_data_long)
Caudate_model5 <- lmer(Caudate ~ Total_Brain_Vol_log * DX_GROUP + AGE_AT_SCAN+ Hemisphere+SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                       Abide_MGCFA_data_long)
Caudate_model6 <- lmer(Caudate ~ Total_Brain_Vol_log * DX_GROUP *Age2*Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                       Abide_MGCFA_data_long)

Caudate_list = c(Caudate_model1, Caudate_model2, Caudate_model3, Caudate_model4, Caudate_model5, Caudate_model6)
Caudate_list_coef<- lapply(Caudate_list, coef)
names(Caudate_list_coef) <- c("No_TBV_Age", "No_TBV_Age2", "TBV_Covariate_Age", "TBV_Covariate_Age2", "TBV_Allometric_Age","TBV_Allometric_Age2")

sink("Caudate_list.csv") 
print(Caudate_list_coef)
sink()


Caudate_list_model_1 = c(Caudate_model1, Caudate_model2)
Caudate_list_model_2 = c(Caudate_model3, Caudate_model4)
Caudate_list_model_3 = c(Caudate_model5) 
Caudate_list_model_4 = c(Caudate_model6)

A<- lapply(Caudate_list_model_3, function(x) {summary(x)$coefficients}) 
A
Caudate_list_model_summary_1_2 <- lapply(Caudate_list_model_1, function(x) {summary(x)$coefficients[c(2,7,22)]}) 
Caudate_list_model_summary_3_4 <- lapply(Caudate_list_model_2, function(x) {summary(x)$coefficients[c(3,8,23)]}) 
Caudate_list_model_summary_5 <- lapply(Caudate_list_model_3, function(x) {summary(x)$coefficients[c(3,10,31)]})
Caudate_list_model_summary_6 <- lapply(Caudate_list_model_4, function(x) {summary(x)$coefficients[c(11,22,55)]})

Caudate_list_model_summary_list <- c(Caudate_list_model_summary_1_2,Caudate_list_model_summary_3_4,Caudate_list_model_summary_5,Caudate_list_model_summary_6)
Caudate_list_model_summary_DF <- data.frame(matrix(unlist(Caudate_list_model_summary_list), nrow=length(Caudate_list_model_summary_list), byrow=T))
colnames(Caudate_list_model_summary_DF) <- c("Beta", "SE", "p")
rownames(Caudate_list_model_summary_DF) <- c("No_TBV_Age", "No_TBV_Age2", "TBV_Covariate_Age", "TBV_Covariate_Age2", "TBV_Allometric_Age","TBV_Allometric_Age2")
Caudate_list_model_summary_DF

summary(Abide_MGCFA_data_long)
# calculate Cohen's D
std <- function(x) {sqrt(654) * x}
List_SE <- as.list(Caudate_list_model_summary_DF$SE)
List_SD <- lapply(List_SE,std)
Caudate_list_model_summary_DF$SD <- unlist(List_SD)
Caudate_list_model_summary_DF$Cohen_D <- (Caudate_list_model_summary_DF$Beta/Caudate_list_model_summary_DF$SD)

sink("Caudate.csv") 
print("Group Effect")
print(Caudate_list_model_summary_DF)
sink() 

######## Putamen 


Putamen_model1 <- lmer(Putamen ~ DX_GROUP * AGE_AT_SCAN  * Hemisphere * SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                       Abide_LMEM_Rep_Clean_data_long)
Putamen_model2 <- lmer(Putamen ~ DX_GROUP + Age2 + Hemisphere+SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                       Abide_LMEM_Rep_Clean_data_long)
Putamen_model3 <- lmer(Putamen ~ Total_Brain_Vol + DX_GROUP + Age2 + Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                       Abide_LMEM_Rep_Clean_data_long)
Putamen_model4 <- lmer(Putamen ~ Total_Brain_Vol +DX_GROUP * AGE_AT_SCAN  * Hemisphere * SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                       Abide_LMEM_Rep_Clean_data_long)
Putamen_model5 <- lmer(Putamen ~ Total_Brain_Vol_log *DX_GROUP * AGE_AT_SCAN  * Hemisphere * SEX + (1|SITE_ID2) + (1 | SUB_ID),
                       Abide_MGCFA_data_long)
Putamen_model6 <- lmer(Putamen ~ Total_Brain_Vol_log * DX_GROUP + Age2 + Hemisphere+SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                       Abide_MGCFA_data_long)

Putamen_list = c(Putamen_model1, Putamen_model2, Putamen_model3, Putamen_model4, Putamen_model5, Putamen_model6)
Putamen_list_coef<- lapply(Putamen_list, coef)
names(Putamen_list_coef) <- c("No_TBV_Age", "No_TBV_Age2", "TBV_Covariate_Age2", "TBV_Covariate_Age", "TBV_Allometric_Age","TBV_Allometric_Age2")

sink("Putamen_list.csv") 
print(Putamen_list_coef)
sink()


Putamen_list_model_1 = c(Putamen_model1, Putamen_model2)
Putamen_list_model_2 = c(Putamen_model3, Putamen_model4)
Putamen_list_model_3 = c(Putamen_model5, Putamen_model6)


Putamen_list_model_summary <- lapply(Putamen_list_model, function(x) {summary(x)$coefficients})
Putamen_list_model_summary_1_2 <- lapply(Putamen_list_model_1, function(x) {summary(x)$coefficients[c(9,18,45)]}) 
Putamen_list_model_summary_3_4 <- lapply(Putamen_list_model_2, function(x) {summary(x)$coefficients[c(10,20,50)]}) 
Putamen_list_model_summary_5_6 <- lapply(Putamen_list_model_3, function(x) {summary(x)$coefficients[c(11,22,55)]})

Putamen_list_model_summary_list <- c(Putamen_list_model_summary_1_2,Putamen_list_model_summary_3_4,Putamen_list_model_summary_5_6)
Putamen_list_model_summary_DF <- data.frame(matrix(unlist(Putamen_list_model_summary_list), nrow=length(Putamen_list_model_summary_list), byrow=T))
colnames(Putamen_list_model_summary_DF) <- c("Beta", "SE", "p")
rownames(Putamen_list_model_summary_DF) <- c("No_TBV_Age", "No_TBV_Age2", "TBV_Covariate_Age", "TBV_Covariate_Age2", "TBV_Allometric_Age","TBV_Allometric_Age2")
Putamen_list_model_summary_DF

summary(Abide_MGCFA_data_long)
# calculate Cohen's D
std <- function(x) {sqrt(654) * x}
List_SE <- as.list(Putamen_list_model_summary_DF$SE)
List_SD <- lapply(List_SE,std)
Putamen_list_model_summary_DF$SD <- unlist(List_SD)
Putamen_list_model_summary_DF$Cohen_D <- (Putamen_list_model_summary_DF$Beta/Putamen_list_model_summary_DF$SD)

sink("Putamen.csv") 
print(Putamen_list_model_summary_DF)
sink() 

### TGV 
Abide_MGCFA$Age2 <- scale(Abide_MGCFA$Age2, scale = F)
Abide_MGCFA$AGE_AT_SCAN <- scale(Abide_MGCFA$AGE_AT_SCAN, scale = F)
Abide$Age2 <- scale(Abide$Age2, scale = F)
Abide$AGE_AT_SCAN <- scale(Abide$AGE_AT_SCAN, scale = F)
Abide$DX_GROUP <- factor(Abide$DX_GROUP, levels = c("Control", "ASD"))
Abide_MGCFA$DX_GROUP <- factor(Abide_MGCFA$DX_GROUP, levels = c("Control", "ASD"))

TotalGrayVol_model1 <- lmer(TotalGrayVol ~ DX_GROUP + Age2*SEX + (1|SITE_ID2),
                            Abide)
TotalGrayVol_model2 <- lmer(TotalGrayVol ~ DX_GROUP + AGE_AT_SCAN*SEX + (1|SITE_ID2),
                            Abide)
TotalGrayVol_model3 <- lmer(TotalGrayVol ~ Total_Brain_Vol + DX_GROUP + Age2*SEX + (1|SITE_ID2),
                            Abide)
TotalGrayVol_model4 <- lmer(TotalGrayVol ~ Total_Brain_Vol + DX_GROUP + AGE_AT_SCAN*SEX + (1|SITE_ID2),
                            Abide)
TotalGrayVol_model5 <- lmer(TotalGrayVol_log ~ Total_Brain_Vol_log * DX_GROUP + Age2*SEX + (1|SITE_ID2),
                            Abide_MGCFA)
TotalGrayVol_model6 <- lmer(TotalGrayVol_log ~ Total_Brain_Vol_log * DX_GROUP + AGE_AT_SCAN*SEX + (1|SITE_ID2),
                            Abide_MGCFA)
list = c(TotalGrayVol_model1, TotalGrayVol_model2, TotalGrayVol_model3, TotalGrayVol_model4, TotalGrayVol_model5, TotalGrayVol_model6)

sink("TotalGrayVol.csv") 
print(lapply(list, coef))
sink()


CorticalWhiteMatterVol_model1 <- lmer(CorticalWhiteMatterVol ~ DX_GROUP + Age2*SEX + (1|SITE_ID2),
                                      Abide)
CorticalWhiteMatterVol_model2 <- lmer(CorticalWhiteMatterVol ~ DX_GROUP *AGE_AT_SCAN*SEX + (1|SITE_ID2),
                                      Abide)
CorticalWhiteMatterVol_model3 <- lmer(CorticalWhiteMatterVol ~ Total_Brain_Vol + DX_GROUP + Age2*SEX + (1|SITE_ID2),
                                      Abide)
CorticalWhiteMatterVol_model4 <- lmer(CorticalWhiteMatterVol ~ Total_Brain_Vol + DX_GROUP * AGE_AT_SCAN*SEX + (1|SITE_ID2),
                                      Abide)
CorticalWhiteMatterVol_model5 <- lmer(CorticalWhiteMatterVol_log ~ Total_Brain_Vol_log * DX_GROUP + Age2*SEX + (1|SITE_ID2),
                                      Abide_MGCFA)
CorticalWhiteMatterVol_model6 <- lmer(CorticalWhiteMatterVol_log ~ Total_Brain_Vol_log * DX_GROUP *AGE_AT_SCAN*SEX + (1|SITE_ID2),
                                      Abide_MGCFA)
list = c(CorticalWhiteMatterVol_model1, CorticalWhiteMatterVol_model2, CorticalWhiteMatterVol_model3, CorticalWhiteMatterVol_model4, CorticalWhiteMatterVol_model5, CorticalWhiteMatterVol_model6)

sink("CorticalWhiteMatterVol.csv") 
print(lapply(list, coef))
sink()

Amygdala_model1 <- lmer(Amygdala ~ DX_GROUP +Age2+ Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_LMEM_Rep_Clean_data_long)
Amygdala_model2 <- lmer(Amygdala ~ DX_GROUP + AGE_AT_SCAN + SEX+ Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_LMEM_Rep_Clean_data_long)
Amygdala_model3 <- lmer(Amygdala ~ Total_Brain_Vol + DX_GROUP +Age2+Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_LMEM_Rep_Clean_data_long)
Amygdala_model4 <- lmer(Amygdala ~ Total_Brain_Vol + DX_GROUP + AGE_AT_SCAN + SEX+ Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_LMEM_Rep_Clean_data_long)
Amygdala_model5 <- lmer(Amygdala ~ Total_Brain_Vol_log * DX_GROUP +Age2+Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_MGCFA_data_long)
Amygdala_model6 <- lmer(Amygdala ~ Total_Brain_Vol_log * DX_GROUP + AGE_AT_SCAN + SEX+ Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_MGCFA_data_long)
list = c(Amygdala_model1, Amygdala_model2, Amygdala_model3, Amygdala_model4, Amygdala_model5, Amygdala_model6)

sink("Amygdala.csv") 
print(lapply(list, coef))
sink()

Thalamus_model1 <- lmer(Thalamus ~ DX_GROUP +Age2+ Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_LMEM_Rep_Clean_data_long)
Thalamus_model2 <- lmer(Thalamus ~ DX_GROUP + AGE_AT_SCAN + SEX+ Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_LMEM_Rep_Clean_data_long)
Thalamus_model3 <- lmer(Thalamus ~ Total_Brain_Vol + DX_GROUP +Age2+Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_LMEM_Rep_Clean_data_long)
Thalamus_model4 <- lmer(Thalamus ~ Total_Brain_Vol + DX_GROUP + AGE_AT_SCAN + SEX+ Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_LMEM_Rep_Clean_data_long)
Thalamus_model5 <- lmer(Thalamus ~ Total_Brain_Vol_log * DX_GROUP +Age2+Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_MGCFA_data_long)
Thalamus_model6 <- lmer(Thalamus ~ Total_Brain_Vol_log * DX_GROUP + AGE_AT_SCAN + SEX+ Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_MGCFA_data_long)
list = c(Thalamus_model1, Thalamus_model2, Thalamus_model3, Thalamus_model4, Thalamus_model5, Thalamus_model6)

sink("Thalamus.csv") 
print(lapply(list, coef))
sink()

Pallidum_model1 <- lmer(Pallidum ~ DX_GROUP +Age2+ Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_LMEM_Rep_Clean_data_long)
Pallidum_model2 <- lmer(Pallidum ~ DX_GROUP + AGE_AT_SCAN + SEX+ Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_LMEM_Rep_Clean_data_long)
Pallidum_model3 <- lmer(Pallidum ~ Total_Brain_Vol + DX_GROUP +Age2+Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_LMEM_Rep_Clean_data_long)
Pallidum_model4 <- lmer(Pallidum ~ Total_Brain_Vol + DX_GROUP + AGE_AT_SCAN + SEX+ Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_LMEM_Rep_Clean_data_long)
Pallidum_model5 <- lmer(Pallidum ~ Total_Brain_Vol_log * DX_GROUP +Age2+Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_MGCFA_data_long)
Pallidum_model6 <- lmer(Pallidum ~ Total_Brain_Vol_log * DX_GROUP + AGE_AT_SCAN + SEX+ Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                        Abide_MGCFA_data_long)
list = c(Pallidum_model1, Pallidum_model2, Pallidum_model3, Pallidum_model4, Pallidum_model5, Pallidum_model6)

sink("Pallidum.csv") 
print(lapply(list, coef))
sink()

Accumbens_model1 <- lmer(Accumbens ~ DX_GROUP +Age2+ Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                         Abide_LMEM_Rep_Clean_data_long)
Accumbens_model2 <- lmer(Accumbens ~ DX_GROUP + AGE_AT_SCAN + SEX+ Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                         Abide_LMEM_Rep_Clean_data_long)
Accumbens_model3 <- lmer(Accumbens ~ Total_Brain_Vol + DX_GROUP +Age2+Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                         Abide_LMEM_Rep_Clean_data_long)
Accumbens_model4 <- lmer(Accumbens ~ Total_Brain_Vol + DX_GROUP + AGE_AT_SCAN + SEX+ Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                         Abide_LMEM_Rep_Clean_data_long)
Accumbens_model5 <- lmer(Accumbens ~ Total_Brain_Vol_log * DX_GROUP +Age2+Hemisphere + SEX+ (1|SITE_ID2) + (1 | SUB_ID),
                         Abide_MGCFA_data_long)
Accumbens_model6 <- lmer(Accumbens ~ Total_Brain_Vol_log * DX_GROUP + AGE_AT_SCAN + SEX+ Hemisphere+ (1|SITE_ID2) + (1 | SUB_ID),
                         Abide_MGCFA_data_long)
list = c(Accumbens_model1, Accumbens_model2, Accumbens_model3, Accumbens_model4, Accumbens_model5, Accumbens_model6)

sink("Accumbens.csv") 
print(lapply(list, coef))
sink()
