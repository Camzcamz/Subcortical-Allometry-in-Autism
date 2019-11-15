library(lmerTest)
library(dplyr)
library(car)
library(broom)
library(sjPlot)
# Load Abide_LMEM_log_Clean Data replace [....] with your path 
load("[.....]Abide/Data/Abide_LMEM_log_Clean.RData")

# Here you have models with Age and Sex Effects and without age and sex effects. 
# Make New DF with Controls OR ASD if you replace "Control" with "ASD"
Abide_Jong_Controls<- Abide_LMEM_log_Clean %>% filter(DX_GROUP =="Control") 

## Create LMEM List 
DV_List <- grep("_log", names(Abide_Jong_Controls), fixed=T, value=T) # keep DV ending with "_log"
DV_List_Final <- DV_List [c(11, 13:30, 34:37)] # DV of interest

LMEM_Full <- as.list(paste(DV_List_Final, "~  Total_Brain_Vol_log + AGE_AT_SCAN * SEX + (1|SITE_ID2)"))

Models_LMEM_Full <- lapply(LMEM_Full, function(x) {lmer(x,data=Abide_Jong_Controls)})
names(Models_LMEM_Full) <- LMEM_Full
Get_Coeffs_Model <- lapply(Models_LMEM_Full, function(x) {summary(x)$coefficients[2]})
Get_Coeffs_Model_DF <- lapply(Get_Coeffs_Model,tidy)
View(Get_Coeffs_Model)
# From list to a DF 
New_DF_Allometric_Coef <- data.frame(matrix(unlist(Get_Coeffs_Model_DF), nrow=length(Get_Coeffs_Model_DF), byrow=T))
# Rename columns & Rows 
names(New_DF_Allometric_Coef)[1] <- "Alpha"
row.names(New_DF_Allometric_Coef) <- LMEM_Full
New_DF_Allometric_Coef$Area <- rownames(New_DF_Allometric_Coef)
New_DF_Allometric_Coef <- subset(New_DF_Allometric_Coef, select=c(2,1))

## Confidence Intervals 
CI_List <- lapply(Models_LMEM_Full, confint)
CI_List_low_high <- lapply(CI_List, '[', c(4,8))
CI_List_low_high_DF <- lapply(CI_List_low_high, tidy)

# From list to a DF 
CI_df <- data.frame(matrix(unlist(CI_List_low_high_DF), nrow=length(CI_List_low_high_DF), byrow=T))
# Rename columns & Rows 
names(CI_df)[1] <- "CILow"
names(CI_df)[2] <- "CIHigh"
row.names(CI_df) <- LMEM_Full
CI_df$Area <- rownames(CI_df)

###Linear hypothesis 
Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Isometry_Test_models <- lapply(Models_LMEM_Full, Isometry_Test)
Get_Coeffs_Isometry_Test_Model <- lapply(Isometry_Test_models, tidy)
Get_Coeffs_Isometry_Test_Model_Coef <- lapply(Get_Coeffs_Isometry_Test_Model, '[', c(2), 2:3)

#Form list to a DF
Get_Coeffs_Isometry_Test_Model_Coef_df <- data.frame(matrix(unlist(Get_Coeffs_Isometry_Test_Model_Coef), 
                                                            nrow=length(Get_Coeffs_Isometry_Test_Model_Coef), byrow=T))
names(Get_Coeffs_Isometry_Test_Model_Coef_df)[1] <- "Chi-Square"
names(Get_Coeffs_Isometry_Test_Model_Coef_df)[2] <- "p"
row.names(Get_Coeffs_Isometry_Test_Model_Coef_df) <- LMEM_Full
Get_Coeffs_Isometry_Test_Model_Coef_df$Area <- rownames(Get_Coeffs_Isometry_Test_Model_Coef_df)

No_Effect_Isometry_DF <- inner_join(New_DF_Allometric_Coef, CI_df, by = "Area")
Final_Effect_Isometry_DF <- inner_join(No_Effect_Isometry_DF, Get_Coeffs_Isometry_Test_Model_Coef_df, by = "Area")

# If significant, we  reject the null hypothesis that TBV coefficient = 1 
sink("CSV/LMEM_Scaling_List_Age_Sex_Effects.csv")
print(Final_Effect_Isometry_DF)
sink()

#### Models Without sex and age effects 
LMEM_Jong_List <- as.list(paste(DV_List_Final, "~  Total_Brain_Vol_log + (1|SITE_ID2)"))

Models_LMEM_Jong_List <- lapply(LMEM_Jong_List, function(x) {lmer(x,data=Abide_Jong_Controls)})
names(Models_LMEM_Jong_List) <- LMEM_Jong_List 
Get_Coeffs_Model <- lapply(Models_LMEM_Jong_List, function(x) {summary(x)$coefficients[2]})
Get_Coeffs_Model_DF <- lapply(Get_Coeffs_Model,tidy)

# From list to a DF 
New_DF_Allometric_Coef <- data.frame(matrix(unlist(Get_Coeffs_Model_DF), nrow=length(Get_Coeffs_Model_DF), byrow=T))
# Rename columns & Rows 
names(New_DF_Allometric_Coef)[1] <- "Alpha"
row.names(New_DF_Allometric_Coef) <- LMEM_Jong_List
New_DF_Allometric_Coef$Area <- rownames(New_DF_Allometric_Coef)
New_DF_Allometric_Coef <- subset(New_DF_Allometric_Coef, select=c(2,1))

## Confidence Intervals 
CI_List <- lapply(Models_LMEM_Jong_List, confint)
CI_List_low_high <- lapply(CI_List, '[', c(4,8))
CI_List_low_high_DF <- lapply(CI_List_low_high, tidy)

# From list to a DF 
CI_df <- data.frame(matrix(unlist(CI_List_low_high_DF), nrow=length(CI_List_low_high_DF), byrow=T))
# Rename columns & Rows 
names(CI_df)[1] <- "CILow"
names(CI_df)[2] <- "CIHigh"
row.names(CI_df) <- LMEM_Jong_List
CI_df$Area <- rownames(CI_df)

###Linear hypothesis 
Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Isometry_Test_models <- lapply(Models_LMEM_Jong_List, Isometry_Test)
Get_Coeffs_Isometry_Test_Model <- lapply(Isometry_Test_models, tidy)
Get_Coeffs_Isometry_Test_Model_Coef <- lapply(Get_Coeffs_Isometry_Test_Model, '[', c(2), 2:3)

#Form list to a DF
Get_Coeffs_Isometry_Test_Model_Coef_df <- data.frame(matrix(unlist(Get_Coeffs_Isometry_Test_Model_Coef), 
                                                            nrow=length(Get_Coeffs_Isometry_Test_Model_Coef), byrow=T))
names(Get_Coeffs_Isometry_Test_Model_Coef_df)[1] <- "Chi-Square"
names(Get_Coeffs_Isometry_Test_Model_Coef_df)[2] <- "p"
row.names(Get_Coeffs_Isometry_Test_Model_Coef_df) <- LMEM_Jong_List
Get_Coeffs_Isometry_Test_Model_Coef_df$Area <- rownames(Get_Coeffs_Isometry_Test_Model_Coef_df)

No_Effect_Isometry_DF <- inner_join(New_DF_Allometric_Coef, CI_df, by = "Area")
Final_Effect_Isometry_DF <- inner_join(No_Effect_Isometry_DF, Get_Coeffs_Isometry_Test_Model_Coef_df, by = "Area")

# If significant, we  reject the null hypothesis that TBV coefficient = 1 
sink("CSV/LMEM_Scaling_List_No_Age_Sex_Effects.csv")
print(Final_Effect_Isometry_DF)
sink()

#### Assumptions - http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/
par(mfrow = c(2, 2))
lapply(Models_LMEM_Jong_List, plot)

