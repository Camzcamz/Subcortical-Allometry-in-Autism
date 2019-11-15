library(lmerTest)
library(dplyr)
library(car)
library(broom)
library(tidyr)
library(ggplot2)
# Load Abide_LMEM_log_Clean Data replace [....] with your path 
load("[.....]Abide/Data/Abide_LMEM_log_Clean.RData")

# Make New DF with Controls OR ASD if you replace "Control" with "ASD"
Abide_Jong_Controls<- Abide_LMEM_log_Clean %>% filter(DX_GROUP =="Control") 

Abide_Jong_Controls_No_Comorbidity<-dplyr::filter(Abide_Jong_Controls,is.na(COMORBIDITY)) 
Abide_Jong_Controls_No_Comorbidity$MEDICATION_NAME 
Abide_Jong_Controls_No_Comorbidity$Medication<-if_else(is.na(Abide_Jong_Controls_No_Comorbidity$MEDICATION_NAME), 0, 1)
Abide_Jong_Controls_No_Comorbidity$Medication<- as.factor(Abide_Jong_Controls_No_Comorbidity$Medication)

##### Right_Accumbens_Area
Right_Accumbens_Area_model_Controls <- lmer(Right_Accumbens_Area_log~ Total_Brain_Vol_log + Medication + 
                                              (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Right_Accumbens_Area_model_COntrols)$coefficients 
p.adjust(summary(Right_Accumbens_Area_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Right_Accumbens_Area_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Right_Accumbens_Area)
Right_Accumbens_Area_outliers <- Right_Accumbens_Area_Boxplot$out # You can get the actual values of the outliers with this
Right_Accumbens_Area_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Right_Accumbens_Area %in% Right_Accumbens_Area_outliers),]
Right_Accumbens_Area_Boxplot <- boxplot(Right_Accumbens_Area_Outlier_DF_C$Right_Accumbens_Area)

TBV_Boxplot <- boxplot(Right_Accumbens_Area_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Right_Accumbens_Area_Outlier_DF_C <- Right_Accumbens_Area_Outlier_DF_C[-which(Right_Accumbens_Area_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Right_Accumbens_Area_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Right_Accumbens_Area_model_Controls_no_uni_out <- lmer(Right_Accumbens_Area_log~ Total_Brain_Vol_log + Medication + 
                                                         + (1|SITE_ID2), Right_Accumbens_Area_Outlier_DF_C)

summary(Right_Accumbens_Area_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Right_Accumbens_Area_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Right_Accumbens_Area_model_no_outliers_coef <- summary(Right_Accumbens_Area_model_Controls_no_uni_out)$coefficients[2]
Right_Accumbens_Area_model_no_outliers_coef_DF <- data.frame(Right_Accumbens_Area_model_no_outliers_coef)

# Rename columns & Rows 
names(Right_Accumbens_Area_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Right_Accumbens_Area_model_no_outliers_coef_DF) <- "Right Accumbens Area"
Right_Accumbens_Area_model_no_outliers_coef_DF$Area <- rownames(Right_Accumbens_Area_model_no_outliers_coef_DF)
Right_Accumbens_Area_New_DF <- subset(Right_Accumbens_Area_model_no_outliers_coef_DF, select=c(2,1))

CI_Right_Accumbens_Area <- confint(Right_Accumbens_Area_model_Controls_no_uni_out)
CI_Right_Accumbens_Area_tidy <- tidy(CI_Right_Accumbens_Area)
CI_Right_Accumbens_Area_TBV <- CI_Right_Accumbens_Area_tidy[c(4),]
CI_Right_Accumbens_Area_TBV_DF <- data.frame(CI_Right_Accumbens_Area_TBV)

names(CI_Right_Accumbens_Area_TBV)[1] <- "Area"
names(CI_Right_Accumbens_Area_TBV)[2] <- "CILow"
names(CI_Right_Accumbens_Area_TBV)[3] <- "CIHigh"
CI_Right_Accumbens_Area_TBV$Area <- "Right Accumbens Area"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Right_Accumbens_Area_Isometry_Test <- Isometry_Test(Right_Accumbens_Area_model_Controls_no_uni_out)
Right_Accumbens_Area_Isometry_Test_Tidy <- tidy(Right_Accumbens_Area_Isometry_Test)
Right_Accumbens_Area_Isometry_Test_Tidy_Coef <- Right_Accumbens_Area_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Right_Accumbens_Area_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Right_Accumbens_Area_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Right_Accumbens_Area_Isometry_Test_Tidy_Coef) <- "Right Accumbens Area"
Right_Accumbens_Area_Isometry_Test_Tidy_Coef$Area <- rownames(Right_Accumbens_Area_Isometry_Test_Tidy_Coef)

Right_Accumbens_Area_Isometry_DF <- inner_join(Right_Accumbens_Area_New_DF, CI_Right_Accumbens_Area_TBV,by = "Area")
Right_Accumbens_Area_Final_Effect_Isometry_DF <- inner_join(Right_Accumbens_Area_Isometry_DF, Right_Accumbens_Area_Isometry_Test_Tidy_Coef, by = "Area")
Right_Accumbens_Area_Final_Effect_Isometry_DF
##### Left_Accumbens_Area
# 1. Remove comorbidities

Left_Accumbens_Area_model_COntrols <- lmer(Left_Accumbens_Area_log~ Total_Brain_Vol_log + Medication + 
                                             + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Left_Accumbens_Area_model_COntrols)$coefficients 
p.adjust(summary(Left_Accumbens_Area_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Left_Accumbens_Area_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Left_Accumbens_Area)
Left_Accumbens_Area_outliers <- Left_Accumbens_Area_Boxplot$out # You can get the actual values of the outliers with this
Left_Accumbens_Area_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Left_Accumbens_Area %in% Left_Accumbens_Area_outliers),]
Left_Accumbens_Area_Boxplot <- boxplot(Left_Accumbens_Area_Outlier_DF_C$Left_Accumbens_Area)

TBV_Boxplot <- boxplot(Left_Accumbens_Area_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Left_Accumbens_Area_Outlier_DF_C <- Left_Accumbens_Area_Outlier_DF_C[-which(Left_Accumbens_Area_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Left_Accumbens_Area_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Left_Accumbens_Area_model_Controls_no_uni_out <- lmer(Left_Accumbens_Area_log~ Total_Brain_Vol_log + Medication + 
                                                        + (1|SITE_ID2), Left_Accumbens_Area_Outlier_DF_C)

summary(Left_Accumbens_Area_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Left_Accumbens_Area_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Left_Accumbens_Area_model_no_outliers_coef <- summary(Left_Accumbens_Area_model_Controls_no_uni_out)$coefficients[2]
Left_Accumbens_Area_model_no_outliers_coef_DF <- data.frame(Left_Accumbens_Area_model_no_outliers_coef)

# Rename columns & Rows 
names(Left_Accumbens_Area_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Left_Accumbens_Area_model_no_outliers_coef_DF) <- "Left Accumbens Area"
Left_Accumbens_Area_model_no_outliers_coef_DF$Area <- rownames(Left_Accumbens_Area_model_no_outliers_coef_DF)
Left_Accumbens_Area_New_DF <- subset(Left_Accumbens_Area_model_no_outliers_coef_DF, select=c(2,1))

CI_Left_Accumbens_Area <- confint(Left_Accumbens_Area_model_Controls_no_uni_out)
CI_Left_Accumbens_Area_tidy <- tidy(CI_Left_Accumbens_Area)
CI_Left_Accumbens_Area_TBV <- CI_Left_Accumbens_Area_tidy[c(4),]
CI_Left_Accumbens_Area_TBV_DF <- data.frame(CI_Left_Accumbens_Area_TBV)

names(CI_Left_Accumbens_Area_TBV)[1] <- "Area"
names(CI_Left_Accumbens_Area_TBV)[2] <- "CILow"
names(CI_Left_Accumbens_Area_TBV)[3] <- "CIHigh"
CI_Left_Accumbens_Area_TBV$Area <- "Left Accumbens Area"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Left_Accumbens_Area_Isometry_Test <- Isometry_Test(Left_Accumbens_Area_model_Controls_no_uni_out)
Left_Accumbens_Area_Isometry_Test_Tidy <- tidy(Left_Accumbens_Area_Isometry_Test)
Left_Accumbens_Area_Isometry_Test_Tidy_Coef <- Left_Accumbens_Area_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Left_Accumbens_Area_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Left_Accumbens_Area_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Left_Accumbens_Area_Isometry_Test_Tidy_Coef) <- "Left Accumbens Area"
Left_Accumbens_Area_Isometry_Test_Tidy_Coef$Area <- rownames(Left_Accumbens_Area_Isometry_Test_Tidy_Coef)

Left_Accumbens_Area_Isometry_DF <- inner_join(Left_Accumbens_Area_New_DF, CI_Left_Accumbens_Area_TBV,by = "Area")
Left_Accumbens_Area_Final_Effect_Isometry_DF <- inner_join(Left_Accumbens_Area_Isometry_DF, Left_Accumbens_Area_Isometry_Test_Tidy_Coef, by = "Area")

##### Right_Caudate
# 1. Remove comorbidities

Right_Caudate_model_COntrols <- lmer(Right_Caudate_log~ Total_Brain_Vol_log + Medication + 
                                       + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Right_Caudate_model_COntrols)$coefficients 
p.adjust(summary(Right_Caudate_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Right_Caudate_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Right_Caudate)
Right_Caudate_outliers <- Right_Caudate_Boxplot$out # You can get the actual values of the outliers with this
Right_Caudate_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Right_Caudate %in% Right_Caudate_outliers),]
Right_Caudate_Boxplot <- boxplot(Right_Caudate_Outlier_DF_C$Right_Caudate)

TBV_Boxplot <- boxplot(Right_Caudate_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Right_Caudate_Outlier_DF_C <- Right_Caudate_Outlier_DF_C[-which(Right_Caudate_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Right_Caudate_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Right_Caudate_model_Controls_no_uni_out <- lmer(Right_Caudate_log~ Total_Brain_Vol_log + Medication + 
                                                  + (1|SITE_ID2), Right_Caudate_Outlier_DF_C)

summary(Right_Caudate_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Right_Caudate_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Right_Caudate_model_no_outliers_coef <- summary(Right_Caudate_model_Controls_no_uni_out)$coefficients[2]
Right_Caudate_model_no_outliers_coef_DF <- data.frame(Right_Caudate_model_no_outliers_coef)

# Rename columns & Rows 
names(Right_Caudate_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Right_Caudate_model_no_outliers_coef_DF) <- "Right Caudate"
Right_Caudate_model_no_outliers_coef_DF$Area <- rownames(Right_Caudate_model_no_outliers_coef_DF)
Right_Caudate_New_DF <- subset(Right_Caudate_model_no_outliers_coef_DF, select=c(2,1))

CI_Right_Caudate <- confint(Right_Caudate_model_Controls_no_uni_out)
CI_Right_Caudate_tidy <- tidy(CI_Right_Caudate)
CI_Right_Caudate_TBV <- CI_Right_Caudate_tidy[c(4),]
CI_Right_Caudate_TBV_DF <- data.frame(CI_Right_Caudate_TBV)

names(CI_Right_Caudate_TBV)[1] <- "Area"
names(CI_Right_Caudate_TBV)[2] <- "CILow"
names(CI_Right_Caudate_TBV)[3] <- "CIHigh"
CI_Right_Caudate_TBV$Area <- "Right Caudate"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Right_Caudate_Isometry_Test <- Isometry_Test(Right_Caudate_model_Controls_no_uni_out)
Right_Caudate_Isometry_Test_Tidy <- tidy(Right_Caudate_Isometry_Test)
Right_Caudate_Isometry_Test_Tidy_Coef <- Right_Caudate_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Right_Caudate_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Right_Caudate_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Right_Caudate_Isometry_Test_Tidy_Coef) <- "Right Caudate"
Right_Caudate_Isometry_Test_Tidy_Coef$Area <- rownames(Right_Caudate_Isometry_Test_Tidy_Coef)

Right_Caudate_Isometry_DF <- inner_join(Right_Caudate_New_DF, CI_Right_Caudate_TBV,by = "Area")
Right_Caudate_Final_Effect_Isometry_DF <- inner_join(Right_Caudate_Isometry_DF, Right_Caudate_Isometry_Test_Tidy_Coef, by = "Area")

##### Left_Caudate
# 1. Remove comorbidities

Left_Caudate_model_COntrols <- lmer(Left_Caudate_log~ Total_Brain_Vol_log + Medication + 
                                      + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Left_Caudate_model_COntrols)$coefficients 
p.adjust(summary(Left_Caudate_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Left_Caudate_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Left_Caudate)
Left_Caudate_outliers <- Left_Caudate_Boxplot$out # You can get the actual values of the outliers with this
Left_Caudate_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Left_Caudate %in% Left_Caudate_outliers),]
Left_Caudate_Boxplot <- boxplot(Left_Caudate_Outlier_DF_C$Left_Caudate)

TBV_Boxplot <- boxplot(Left_Caudate_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Left_Caudate_Outlier_DF_C <- Left_Caudate_Outlier_DF_C[-which(Left_Caudate_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Left_Caudate_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Left_Caudate_model_Controls_no_uni_out <- lmer(Left_Caudate_log~ Total_Brain_Vol_log + Medication + 
                                                 + (1|SITE_ID2), Left_Caudate_Outlier_DF_C)

summary(Left_Caudate_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Left_Caudate_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Left_Caudate_model_no_outliers_coef <- summary(Left_Caudate_model_Controls_no_uni_out)$coefficients[2]
Left_Caudate_model_no_outliers_coef_DF <- data.frame(Left_Caudate_model_no_outliers_coef)

# Rename columns & Rows 
names(Left_Caudate_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Left_Caudate_model_no_outliers_coef_DF) <- "Left Caudate"
Left_Caudate_model_no_outliers_coef_DF$Area <- rownames(Left_Caudate_model_no_outliers_coef_DF)
Left_Caudate_New_DF <- subset(Left_Caudate_model_no_outliers_coef_DF, select=c(2,1))

CI_Left_Caudate <- confint(Left_Caudate_model_Controls_no_uni_out)
CI_Left_Caudate_tidy <- tidy(CI_Left_Caudate)
CI_Left_Caudate_TBV <- CI_Left_Caudate_tidy[c(4),]
CI_Left_Caudate_TBV_DF <- data.frame(CI_Left_Caudate_TBV)

names(CI_Left_Caudate_TBV)[1] <- "Area"
names(CI_Left_Caudate_TBV)[2] <- "CILow"
names(CI_Left_Caudate_TBV)[3] <- "CIHigh"
CI_Left_Caudate_TBV$Area <- "Left Caudate"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Left_Caudate_Isometry_Test <- Isometry_Test(Left_Caudate_model_Controls_no_uni_out)
Left_Caudate_Isometry_Test_Tidy <- tidy(Left_Caudate_Isometry_Test)
Left_Caudate_Isometry_Test_Tidy_Coef <- Left_Caudate_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Left_Caudate_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Left_Caudate_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Left_Caudate_Isometry_Test_Tidy_Coef) <- "Left Caudate"
Left_Caudate_Isometry_Test_Tidy_Coef$Area <- rownames(Left_Caudate_Isometry_Test_Tidy_Coef)

Left_Caudate_Isometry_DF <- inner_join(Left_Caudate_New_DF, CI_Left_Caudate_TBV,by = "Area")
Left_Caudate_Final_Effect_Isometry_DF <- inner_join(Left_Caudate_Isometry_DF, Left_Caudate_Isometry_Test_Tidy_Coef, by = "Area")

##### Right_Amygdala
# 1. Remove comorbidities

Right_Amygdala_model_COntrols <- lmer(Right_Amygdala_log~ Total_Brain_Vol_log + Medication + 
                                        + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Right_Amygdala_model_COntrols)$coefficients 
p.adjust(summary(Right_Amygdala_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Right_Amygdala_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Right_Amygdala)
Right_Amygdala_outliers <- Right_Amygdala_Boxplot$out # You can get the actual values of the outliers with this
Right_Amygdala_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Right_Amygdala %in% Right_Amygdala_outliers),]
Right_Amygdala_Boxplot <- boxplot(Right_Amygdala_Outlier_DF_C$Right_Amygdala)

TBV_Boxplot <- boxplot(Right_Amygdala_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Right_Amygdala_Outlier_DF_C <- Right_Amygdala_Outlier_DF_C[-which(Right_Amygdala_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Right_Amygdala_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Right_Amygdala_model_Controls_no_uni_out <- lmer(Right_Amygdala_log~ Total_Brain_Vol_log + Medication + 
                                                   + (1|SITE_ID2), Right_Amygdala_Outlier_DF_C)

summary(Right_Amygdala_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Right_Amygdala_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Right_Amygdala_model_no_outliers_coef <- summary(Right_Amygdala_model_Controls_no_uni_out)$coefficients[2]
Right_Amygdala_model_no_outliers_coef_DF <- data.frame(Right_Amygdala_model_no_outliers_coef)

# Rename columns & Rows 
names(Right_Amygdala_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Right_Amygdala_model_no_outliers_coef_DF) <- "Right Amygdala"
Right_Amygdala_model_no_outliers_coef_DF$Area <- rownames(Right_Amygdala_model_no_outliers_coef_DF)
Right_Amygdala_New_DF <- subset(Right_Amygdala_model_no_outliers_coef_DF, select=c(2,1))

CI_Right_Amygdala <- confint(Right_Amygdala_model_Controls_no_uni_out)
CI_Right_Amygdala_tidy <- tidy(CI_Right_Amygdala)
CI_Right_Amygdala_TBV <- CI_Right_Amygdala_tidy[c(4),]
CI_Right_Amygdala_TBV_DF <- data.frame(CI_Right_Amygdala_TBV)

names(CI_Right_Amygdala_TBV)[1] <- "Area"
names(CI_Right_Amygdala_TBV)[2] <- "CILow"
names(CI_Right_Amygdala_TBV)[3] <- "CIHigh"
CI_Right_Amygdala_TBV$Area <- "Right Amygdala"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Right_Amygdala_Isometry_Test <- Isometry_Test(Right_Amygdala_model_Controls_no_uni_out)
Right_Amygdala_Isometry_Test_Tidy <- tidy(Right_Amygdala_Isometry_Test)
Right_Amygdala_Isometry_Test_Tidy_Coef <- Right_Amygdala_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Right_Amygdala_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Right_Amygdala_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Right_Amygdala_Isometry_Test_Tidy_Coef) <- "Right Amygdala"
Right_Amygdala_Isometry_Test_Tidy_Coef$Area <- rownames(Right_Amygdala_Isometry_Test_Tidy_Coef)

Right_Amygdala_Isometry_DF <- inner_join(Right_Amygdala_New_DF, CI_Right_Amygdala_TBV,by = "Area")
Right_Amygdala_Final_Effect_Isometry_DF <- inner_join(Right_Amygdala_Isometry_DF, Right_Amygdala_Isometry_Test_Tidy_Coef, by = "Area")

##### Left_Amygdala
# 1. Remove comorbidities

Left_Amygdala_model_COntrols <- lmer(Left_Amygdala_log~ Total_Brain_Vol_log + Medication + 
                                       + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Left_Amygdala_model_COntrols)$coefficients 
p.adjust(summary(Left_Amygdala_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Left_Amygdala_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Left_Amygdala)
Left_Amygdala_outliers <- Left_Amygdala_Boxplot$out # You can get the actual values of the outliers with this
Left_Amygdala_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Left_Amygdala %in% Left_Amygdala_outliers),]
Left_Amygdala_Boxplot <- boxplot(Left_Amygdala_Outlier_DF_C$Left_Amygdala)

TBV_Boxplot <- boxplot(Left_Amygdala_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Left_Amygdala_Outlier_DF_C <- Left_Amygdala_Outlier_DF_C[-which(Left_Amygdala_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Left_Amygdala_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Left_Amygdala_model_Controls_no_uni_out <- lmer(Left_Amygdala_log~ Total_Brain_Vol_log + Medication + 
                                                  + (1|SITE_ID2), Left_Amygdala_Outlier_DF_C)

summary(Left_Amygdala_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Left_Amygdala_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Left_Amygdala_model_no_outliers_coef <- summary(Left_Amygdala_model_Controls_no_uni_out)$coefficients[2]
Left_Amygdala_model_no_outliers_coef_DF <- data.frame(Left_Amygdala_model_no_outliers_coef)

# Rename columns & Rows 
names(Left_Amygdala_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Left_Amygdala_model_no_outliers_coef_DF) <- "Left Amygdala"
Left_Amygdala_model_no_outliers_coef_DF$Area <- rownames(Left_Amygdala_model_no_outliers_coef_DF)
Left_Amygdala_New_DF <- subset(Left_Amygdala_model_no_outliers_coef_DF, select=c(2,1))

CI_Left_Amygdala <- confint(Left_Amygdala_model_Controls_no_uni_out)
CI_Left_Amygdala_tidy <- tidy(CI_Left_Amygdala)
CI_Left_Amygdala_TBV <- CI_Left_Amygdala_tidy[c(4),]
CI_Left_Amygdala_TBV_DF <- data.frame(CI_Left_Amygdala_TBV)

names(CI_Left_Amygdala_TBV)[1] <- "Area"
names(CI_Left_Amygdala_TBV)[2] <- "CILow"
names(CI_Left_Amygdala_TBV)[3] <- "CIHigh"
CI_Left_Amygdala_TBV$Area <- "Left Amygdala"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Left_Amygdala_Isometry_Test <- Isometry_Test(Left_Amygdala_model_Controls_no_uni_out)
Left_Amygdala_Isometry_Test_Tidy <- tidy(Left_Amygdala_Isometry_Test)
Left_Amygdala_Isometry_Test_Tidy_Coef <- Left_Amygdala_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Left_Amygdala_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Left_Amygdala_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Left_Amygdala_Isometry_Test_Tidy_Coef) <- "Left Amygdala"
Left_Amygdala_Isometry_Test_Tidy_Coef$Area <- rownames(Left_Amygdala_Isometry_Test_Tidy_Coef)

Left_Amygdala_Isometry_DF <- inner_join(Left_Amygdala_New_DF, CI_Left_Amygdala_TBV,by = "Area")
Left_Amygdala_Final_Effect_Isometry_DF <- inner_join(Left_Amygdala_Isometry_DF, Left_Amygdala_Isometry_Test_Tidy_Coef, by = "Area")
##### Right_Hippocampus
# 1. Remove comorbidities

Right_Hippocampus_model_COntrols <- lmer(Right_Hippocampus_log~ Total_Brain_Vol_log + Medication + 
                                           + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Right_Hippocampus_model_COntrols)$coefficients 
p.adjust(summary(Right_Hippocampus_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Right_Hippocampus_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Right_Hippocampus)
Right_Hippocampus_outliers <- Right_Hippocampus_Boxplot$out # You can get the actual values of the outliers with this
Right_Hippocampus_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Right_Hippocampus %in% Right_Hippocampus_outliers),]
Right_Hippocampus_Boxplot <- boxplot(Right_Hippocampus_Outlier_DF_C$Right_Hippocampus)

TBV_Boxplot <- boxplot(Right_Hippocampus_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Right_Hippocampus_Outlier_DF_C <- Right_Hippocampus_Outlier_DF_C[-which(Right_Hippocampus_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Right_Hippocampus_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Right_Hippocampus_model_Controls_no_uni_out <- lmer(Right_Hippocampus_log~ Total_Brain_Vol_log + Medication + 
                                                      + (1|SITE_ID2), Right_Hippocampus_Outlier_DF_C)

summary(Right_Hippocampus_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Right_Hippocampus_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Right_Hippocampus_model_no_outliers_coef <- summary(Right_Hippocampus_model_Controls_no_uni_out)$coefficients[2]
Right_Hippocampus_model_no_outliers_coef_DF <- data.frame(Right_Hippocampus_model_no_outliers_coef)

# Rename columns & Rows 
names(Right_Hippocampus_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Right_Hippocampus_model_no_outliers_coef_DF) <- "Right Hippocampus"
Right_Hippocampus_model_no_outliers_coef_DF$Area <- rownames(Right_Hippocampus_model_no_outliers_coef_DF)
Right_Hippocampus_New_DF <- subset(Right_Hippocampus_model_no_outliers_coef_DF, select=c(2,1))

CI_Right_Hippocampus <- confint(Right_Hippocampus_model_Controls_no_uni_out)
CI_Right_Hippocampus_tidy <- tidy(CI_Right_Hippocampus)
CI_Right_Hippocampus_TBV <- CI_Right_Hippocampus_tidy[c(4),]
CI_Right_Hippocampus_TBV_DF <- data.frame(CI_Right_Hippocampus_TBV)

names(CI_Right_Hippocampus_TBV)[1] <- "Area"
names(CI_Right_Hippocampus_TBV)[2] <- "CILow"
names(CI_Right_Hippocampus_TBV)[3] <- "CIHigh"
CI_Right_Hippocampus_TBV$Area <- "Right Hippocampus"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Right_Hippocampus_Isometry_Test <- Isometry_Test(Right_Hippocampus_model_Controls_no_uni_out)
Right_Hippocampus_Isometry_Test_Tidy <- tidy(Right_Hippocampus_Isometry_Test)
Right_Hippocampus_Isometry_Test_Tidy_Coef <- Right_Hippocampus_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Right_Hippocampus_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Right_Hippocampus_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Right_Hippocampus_Isometry_Test_Tidy_Coef) <- "Right Hippocampus"
Right_Hippocampus_Isometry_Test_Tidy_Coef$Area <- rownames(Right_Hippocampus_Isometry_Test_Tidy_Coef)

Right_Hippocampus_Isometry_DF <- inner_join(Right_Hippocampus_New_DF, CI_Right_Hippocampus_TBV,by = "Area")
Right_Hippocampus_Final_Effect_Isometry_DF <- inner_join(Right_Hippocampus_Isometry_DF, Right_Hippocampus_Isometry_Test_Tidy_Coef, by = "Area")

##### Left_Hippocampus
# 1. Remove comorbidities

Left_Hippocampus_model_COntrols <- lmer(Left_Hippocampus_log~ Total_Brain_Vol_log + Medication + 
                                          + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Left_Hippocampus_model_COntrols)$coefficients 
p.adjust(summary(Left_Hippocampus_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Left_Hippocampus_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Left_Hippocampus)
Left_Hippocampus_outliers <- Left_Hippocampus_Boxplot$out # You can get the actual values of the outliers with this
Left_Hippocampus_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Left_Hippocampus %in% Left_Hippocampus_outliers),]
Left_Hippocampus_Boxplot <- boxplot(Left_Hippocampus_Outlier_DF_C$Left_Hippocampus)

TBV_Boxplot <- boxplot(Left_Hippocampus_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Left_Hippocampus_Outlier_DF_C <- Left_Hippocampus_Outlier_DF_C[-which(Left_Hippocampus_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Left_Hippocampus_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Left_Hippocampus_model_Controls_no_uni_out <- lmer(Left_Hippocampus_log~ Total_Brain_Vol_log + Medication + 
                                                     + (1|SITE_ID2), Left_Hippocampus_Outlier_DF_C)

summary(Left_Hippocampus_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Left_Hippocampus_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Left_Hippocampus_model_no_outliers_coef <- summary(Left_Hippocampus_model_Controls_no_uni_out)$coefficients[2]
Left_Hippocampus_model_no_outliers_coef_DF <- data.frame(Left_Hippocampus_model_no_outliers_coef)

# Rename columns & Rows 
names(Left_Hippocampus_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Left_Hippocampus_model_no_outliers_coef_DF) <- "Left Hippocampus"
Left_Hippocampus_model_no_outliers_coef_DF$Area <- rownames(Left_Hippocampus_model_no_outliers_coef_DF)
Left_Hippocampus_New_DF <- subset(Left_Hippocampus_model_no_outliers_coef_DF, select=c(2,1))

CI_Left_Hippocampus <- confint(Left_Hippocampus_model_Controls_no_uni_out)
CI_Left_Hippocampus_tidy <- tidy(CI_Left_Hippocampus)
CI_Left_Hippocampus_TBV <- CI_Left_Hippocampus_tidy[c(4),]
CI_Left_Hippocampus_TBV_DF <- data.frame(CI_Left_Hippocampus_TBV)

names(CI_Left_Hippocampus_TBV)[1] <- "Area"
names(CI_Left_Hippocampus_TBV)[2] <- "CILow"
names(CI_Left_Hippocampus_TBV)[3] <- "CIHigh"
CI_Left_Hippocampus_TBV$Area <- "Left Hippocampus"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Left_Hippocampus_Isometry_Test <- Isometry_Test(Left_Hippocampus_model_Controls_no_uni_out)
Left_Hippocampus_Isometry_Test_Tidy <- tidy(Left_Hippocampus_Isometry_Test)
Left_Hippocampus_Isometry_Test_Tidy_Coef <- Left_Hippocampus_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Left_Hippocampus_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Left_Hippocampus_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Left_Hippocampus_Isometry_Test_Tidy_Coef) <- "Left Hippocampus"
Left_Hippocampus_Isometry_Test_Tidy_Coef$Area <- rownames(Left_Hippocampus_Isometry_Test_Tidy_Coef)

Left_Hippocampus_Isometry_DF <- inner_join(Left_Hippocampus_New_DF, CI_Left_Hippocampus_TBV,by = "Area")
Left_Hippocampus_Final_Effect_Isometry_DF <- inner_join(Left_Hippocampus_Isometry_DF, Left_Hippocampus_Isometry_Test_Tidy_Coef, by = "Area")
##### Right_Putamen
# 1. Remove comorbidities

Right_Putamen_model_COntrols <- lmer(Right_Putamen_log~ Total_Brain_Vol_log + Medication + 
                                       + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Right_Putamen_model_COntrols)$coefficients 
p.adjust(summary(Right_Putamen_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Right_Putamen_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Right_Putamen)
Right_Putamen_outliers <- Right_Putamen_Boxplot$out # You can get the actual values of the outliers with this
Right_Putamen_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Right_Putamen %in% Right_Putamen_outliers),]
Right_Putamen_Boxplot <- boxplot(Right_Putamen_Outlier_DF_C$Right_Putamen)

TBV_Boxplot <- boxplot(Right_Putamen_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Right_Putamen_Outlier_DF_C <- Right_Putamen_Outlier_DF_C[-which(Right_Putamen_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Right_Putamen_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Right_Putamen_model_Controls_no_uni_out <- lmer(Right_Putamen_log~ Total_Brain_Vol_log + Medication + 
                                                  + (1|SITE_ID2), Right_Putamen_Outlier_DF_C)

summary(Right_Putamen_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Right_Putamen_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Right_Putamen_model_no_outliers_coef <- summary(Right_Putamen_model_Controls_no_uni_out)$coefficients[2]
Right_Putamen_model_no_outliers_coef_DF <- data.frame(Right_Putamen_model_no_outliers_coef)

# Rename columns & Rows 
names(Right_Putamen_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Right_Putamen_model_no_outliers_coef_DF) <- "Right Putamen"
Right_Putamen_model_no_outliers_coef_DF$Area <- rownames(Right_Putamen_model_no_outliers_coef_DF)
Right_Putamen_New_DF <- subset(Right_Putamen_model_no_outliers_coef_DF, select=c(2,1))

CI_Right_Putamen <- confint(Right_Putamen_model_Controls_no_uni_out)
CI_Right_Putamen_tidy <- tidy(CI_Right_Putamen)
CI_Right_Putamen_TBV <- CI_Right_Putamen_tidy[c(4),]
CI_Right_Putamen_TBV_DF <- data.frame(CI_Right_Putamen_TBV)

names(CI_Right_Putamen_TBV)[1] <- "Area"
names(CI_Right_Putamen_TBV)[2] <- "CILow"
names(CI_Right_Putamen_TBV)[3] <- "CIHigh"
CI_Right_Putamen_TBV$Area <- "Right Putamen"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Right_Putamen_Isometry_Test <- Isometry_Test(Right_Putamen_model_Controls_no_uni_out)
Right_Putamen_Isometry_Test_Tidy <- tidy(Right_Putamen_Isometry_Test)
Right_Putamen_Isometry_Test_Tidy_Coef <- Right_Putamen_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Right_Putamen_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Right_Putamen_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Right_Putamen_Isometry_Test_Tidy_Coef) <- "Right Putamen"
Right_Putamen_Isometry_Test_Tidy_Coef$Area <- rownames(Right_Putamen_Isometry_Test_Tidy_Coef)

Right_Putamen_Isometry_DF <- inner_join(Right_Putamen_New_DF, CI_Right_Putamen_TBV,by = "Area")
Right_Putamen_Final_Effect_Isometry_DF <- inner_join(Right_Putamen_Isometry_DF, Right_Putamen_Isometry_Test_Tidy_Coef, by = "Area")

##### Left_Putamen
# 1. Remove comorbidities

Left_Putamen_model_COntrols <- lmer(Left_Putamen_log~ Total_Brain_Vol_log + Medication + 
                                      + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Left_Putamen_model_COntrols)$coefficients 
p.adjust(summary(Left_Putamen_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Left_Putamen_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Left_Putamen)
Left_Putamen_outliers <- Left_Putamen_Boxplot$out # You can get the actual values of the outliers with this
Left_Putamen_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Left_Putamen %in% Left_Putamen_outliers),]
Left_Putamen_Boxplot <- boxplot(Left_Putamen_Outlier_DF_C$Left_Putamen)

TBV_Boxplot <- boxplot(Left_Putamen_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Left_Putamen_Outlier_DF_C <- Left_Putamen_Outlier_DF_C[-which(Left_Putamen_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Left_Putamen_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Left_Putamen_model_Controls_no_uni_out <- lmer(Left_Putamen_log~ Total_Brain_Vol_log + Medication + 
                                                 + (1|SITE_ID2), Left_Putamen_Outlier_DF_C)

summary(Left_Putamen_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Left_Putamen_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Left_Putamen_model_no_outliers_coef <- summary(Left_Putamen_model_Controls_no_uni_out)$coefficients[2]
Left_Putamen_model_no_outliers_coef_DF <- data.frame(Left_Putamen_model_no_outliers_coef)

# Rename columns & Rows 
names(Left_Putamen_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Left_Putamen_model_no_outliers_coef_DF) <- "Left Putamen"
Left_Putamen_model_no_outliers_coef_DF$Area <- rownames(Left_Putamen_model_no_outliers_coef_DF)
Left_Putamen_New_DF <- subset(Left_Putamen_model_no_outliers_coef_DF, select=c(2,1))

CI_Left_Putamen <- confint(Left_Putamen_model_Controls_no_uni_out)
CI_Left_Putamen_tidy <- tidy(CI_Left_Putamen)
CI_Left_Putamen_TBV <- CI_Left_Putamen_tidy[c(4),]
CI_Left_Putamen_TBV_DF <- data.frame(CI_Left_Putamen_TBV)

names(CI_Left_Putamen_TBV)[1] <- "Area"
names(CI_Left_Putamen_TBV)[2] <- "CILow"
names(CI_Left_Putamen_TBV)[3] <- "CIHigh"
CI_Left_Putamen_TBV$Area <- "Left Putamen"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Left_Putamen_Isometry_Test <- Isometry_Test(Left_Putamen_model_Controls_no_uni_out)
Left_Putamen_Isometry_Test_Tidy <- tidy(Left_Putamen_Isometry_Test)
Left_Putamen_Isometry_Test_Tidy_Coef <- Left_Putamen_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Left_Putamen_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Left_Putamen_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Left_Putamen_Isometry_Test_Tidy_Coef) <- "Left Putamen"
Left_Putamen_Isometry_Test_Tidy_Coef$Area <- rownames(Left_Putamen_Isometry_Test_Tidy_Coef)

Left_Putamen_Isometry_DF <- inner_join(Left_Putamen_New_DF, CI_Left_Putamen_TBV,by = "Area")
Left_Putamen_Final_Effect_Isometry_DF <- inner_join(Left_Putamen_Isometry_DF, Left_Putamen_Isometry_Test_Tidy_Coef, by = "Area")
##### Right_Pallidum
# 1. Remove comorbidities

Right_Pallidum_model_COntrols <- lmer(Right_Pallidum_log~ Total_Brain_Vol_log + Medication + 
                                        + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Right_Pallidum_model_COntrols)$coefficients 
p.adjust(summary(Right_Pallidum_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Right_Pallidum_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Right_Pallidum)
Right_Pallidum_outliers <- Right_Pallidum_Boxplot$out # You can get the actual values of the outliers with this
Right_Pallidum_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Right_Pallidum %in% Right_Pallidum_outliers),]
Right_Pallidum_Boxplot <- boxplot(Right_Pallidum_Outlier_DF_C$Right_Pallidum)

TBV_Boxplot <- boxplot(Right_Pallidum_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Right_Pallidum_Outlier_DF_C <- Right_Pallidum_Outlier_DF_C[-which(Right_Pallidum_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Right_Pallidum_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Right_Pallidum_model_Controls_no_uni_out <- lmer(Right_Pallidum_log~ Total_Brain_Vol_log + Medication + 
                                                   + (1|SITE_ID2), Right_Pallidum_Outlier_DF_C)

summary(Right_Pallidum_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Right_Pallidum_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Right_Pallidum_model_no_outliers_coef <- summary(Right_Pallidum_model_Controls_no_uni_out)$coefficients[2]
Right_Pallidum_model_no_outliers_coef_DF <- data.frame(Right_Pallidum_model_no_outliers_coef)

# Rename columns & Rows 
names(Right_Pallidum_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Right_Pallidum_model_no_outliers_coef_DF) <- "Right Pallidum"
Right_Pallidum_model_no_outliers_coef_DF$Area <- rownames(Right_Pallidum_model_no_outliers_coef_DF)
Right_Pallidum_New_DF <- subset(Right_Pallidum_model_no_outliers_coef_DF, select=c(2,1))

CI_Right_Pallidum <- confint(Right_Pallidum_model_Controls_no_uni_out)
CI_Right_Pallidum_tidy <- tidy(CI_Right_Pallidum)
CI_Right_Pallidum_TBV <- CI_Right_Pallidum_tidy[c(4),]
CI_Right_Pallidum_TBV_DF <- data.frame(CI_Right_Pallidum_TBV)

names(CI_Right_Pallidum_TBV)[1] <- "Area"
names(CI_Right_Pallidum_TBV)[2] <- "CILow"
names(CI_Right_Pallidum_TBV)[3] <- "CIHigh"
CI_Right_Pallidum_TBV$Area <- "Right Pallidum"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Right_Pallidum_Isometry_Test <- Isometry_Test(Right_Pallidum_model_Controls_no_uni_out)
Right_Pallidum_Isometry_Test_Tidy <- tidy(Right_Pallidum_Isometry_Test)
Right_Pallidum_Isometry_Test_Tidy_Coef <- Right_Pallidum_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Right_Pallidum_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Right_Pallidum_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Right_Pallidum_Isometry_Test_Tidy_Coef) <- "Right Pallidum"
Right_Pallidum_Isometry_Test_Tidy_Coef$Area <- rownames(Right_Pallidum_Isometry_Test_Tidy_Coef)

Right_Pallidum_Isometry_DF <- inner_join(Right_Pallidum_New_DF, CI_Right_Pallidum_TBV,by = "Area")
Right_Pallidum_Final_Effect_Isometry_DF <- inner_join(Right_Pallidum_Isometry_DF, Right_Pallidum_Isometry_Test_Tidy_Coef, by = "Area")

##### Left_Pallidum
# 1. Remove comorbidities

Left_Pallidum_model_COntrols <- lmer(Left_Pallidum_log~ Total_Brain_Vol_log + Medication + 
                                       + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Left_Pallidum_model_COntrols)$coefficients 
p.adjust(summary(Left_Pallidum_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Left_Pallidum_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Left_Pallidum)
Left_Pallidum_outliers <- Left_Pallidum_Boxplot$out # You can get the actual values of the outliers with this
Left_Pallidum_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Left_Pallidum %in% Left_Pallidum_outliers),]
Left_Pallidum_Boxplot <- boxplot(Left_Pallidum_Outlier_DF_C$Left_Pallidum)

TBV_Boxplot <- boxplot(Left_Pallidum_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Left_Pallidum_Outlier_DF_C <- Left_Pallidum_Outlier_DF_C[-which(Left_Pallidum_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Left_Pallidum_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Left_Pallidum_model_Controls_no_uni_out <- lmer(Left_Pallidum_log~ Total_Brain_Vol_log + Medication + 
                                                  + (1|SITE_ID2), Left_Pallidum_Outlier_DF_C)

summary(Left_Pallidum_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Left_Pallidum_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Left_Pallidum_model_no_outliers_coef <- summary(Left_Pallidum_model_Controls_no_uni_out)$coefficients[2]
Left_Pallidum_model_no_outliers_coef_DF <- data.frame(Left_Pallidum_model_no_outliers_coef)

# Rename columns & Rows 
names(Left_Pallidum_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Left_Pallidum_model_no_outliers_coef_DF) <- "Left Pallidum"
Left_Pallidum_model_no_outliers_coef_DF$Area <- rownames(Left_Pallidum_model_no_outliers_coef_DF)
Left_Pallidum_New_DF <- subset(Left_Pallidum_model_no_outliers_coef_DF, select=c(2,1))

CI_Left_Pallidum <- confint(Left_Pallidum_model_Controls_no_uni_out)
CI_Left_Pallidum_tidy <- tidy(CI_Left_Pallidum)
CI_Left_Pallidum_TBV <- CI_Left_Pallidum_tidy[c(4),]
CI_Left_Pallidum_TBV_DF <- data.frame(CI_Left_Pallidum_TBV)

names(CI_Left_Putamen_TBV)[1] <- "Area"
names(CI_Left_Pallidum_TBV)[2] <- "CILow"
names(CI_Left_Pallidum_TBV)[3] <- "CIHigh"
CI_Left_Pallidum_TBV$Area <- "Left Pallidum"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Left_Pallidum_Isometry_Test <- Isometry_Test(Left_Pallidum_model_Controls_no_uni_out)
Left_Pallidum_Isometry_Test_Tidy <- tidy(Left_Pallidum_Isometry_Test)
Left_Pallidum_Isometry_Test_Tidy_Coef <- Left_Pallidum_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Left_Pallidum_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Left_Pallidum_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Left_Pallidum_Isometry_Test_Tidy_Coef) <- "Left Pallidum"
Left_Pallidum_Isometry_Test_Tidy_Coef$Area <- rownames(Left_Pallidum_Isometry_Test_Tidy_Coef)

Left_Pallidum_Isometry_DF <- inner_join(Left_Pallidum_New_DF, CI_Left_Pallidum_TBV,by = "Area")
Left_Pallidum_Final_Effect_Isometry_DF <- inner_join(Left_Pallidum_Isometry_DF, Left_Pallidum_Isometry_Test_Tidy_Coef, by = "Area")
##### Right_Thalamus_Proper
# 1. Remove comorbidities

Right_Thalamus_Proper_model_COntrols <- lmer(Right_Thalamus_Proper_log~ Total_Brain_Vol_log + Medication + 
                                               + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Right_Thalamus_Proper_model_COntrols)$coefficients 
p.adjust(summary(Right_Thalamus_Proper_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Right_Thalamus_Proper_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Right_Thalamus_Proper)
Right_Thalamus_Proper_outliers <- Right_Thalamus_Proper_Boxplot$out # You can get the actual values of the outliers with this
Right_Thalamus_Proper_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Right_Thalamus_Proper %in% Right_Thalamus_Proper_outliers),]
Right_Thalamus_Proper_Boxplot <- boxplot(Right_Thalamus_Proper_Outlier_DF_C$Right_Thalamus_Proper)

TBV_Boxplot <- boxplot(Right_Thalamus_Proper_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Right_Thalamus_Proper_Outlier_DF_C <- Right_Thalamus_Proper_Outlier_DF_C[-which(Right_Thalamus_Proper_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Right_Thalamus_Proper_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Right_Thalamus_Proper_model_Controls_no_uni_out <- lmer(Right_Thalamus_Proper_log~ Total_Brain_Vol_log + Medication + 
                                                          + (1|SITE_ID2), Right_Thalamus_Proper_Outlier_DF_C)

summary(Right_Thalamus_Proper_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Right_Thalamus_Proper_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Right_Thalamus_Proper_model_no_outliers_coef <- summary(Right_Thalamus_Proper_model_Controls_no_uni_out)$coefficients[2]
Right_Thalamus_Proper_model_no_outliers_coef_DF <- data.frame(Right_Thalamus_Proper_model_no_outliers_coef)

# Rename columns & Rows 
names(Right_Thalamus_Proper_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Right_Thalamus_Proper_model_no_outliers_coef_DF) <- "Right Thalamus_Proper"
Right_Thalamus_Proper_model_no_outliers_coef_DF$Area <- rownames(Right_Thalamus_Proper_model_no_outliers_coef_DF)
Right_Thalamus_Proper_New_DF <- subset(Right_Thalamus_Proper_model_no_outliers_coef_DF, select=c(2,1))

CI_Right_Thalamus_Proper <- confint(Right_Thalamus_Proper_model_Controls_no_uni_out)
CI_Right_Thalamus_Proper_tidy <- tidy(CI_Right_Thalamus_Proper)
CI_Right_Thalamus_Proper_TBV <- CI_Right_Thalamus_Proper_tidy[c(4),]
CI_Right_Thalamus_Proper_TBV_DF <- data.frame(CI_Right_Thalamus_Proper_TBV)

names(CI_Right_Thalamus_Proper_TBV)[1] <- "Area"
names(CI_Right_Thalamus_Proper_TBV)[2] <- "CILow"
names(CI_Right_Thalamus_Proper_TBV)[3] <- "CIHigh"
CI_Right_Thalamus_Proper_TBV$Area <- "Right Thalamus_Proper"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Right_Thalamus_Proper_Isometry_Test <- Isometry_Test(Right_Thalamus_Proper_model_Controls_no_uni_out)
Right_Thalamus_Proper_Isometry_Test_Tidy <- tidy(Right_Thalamus_Proper_Isometry_Test)
Right_Thalamus_Proper_Isometry_Test_Tidy_Coef <- Right_Thalamus_Proper_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Right_Thalamus_Proper_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Right_Thalamus_Proper_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Right_Thalamus_Proper_Isometry_Test_Tidy_Coef) <- "Right Thalamus_Proper"
Right_Thalamus_Proper_Isometry_Test_Tidy_Coef$Area <- rownames(Right_Thalamus_Proper_Isometry_Test_Tidy_Coef)

Right_Thalamus_Proper_Isometry_DF <- inner_join(Right_Thalamus_Proper_New_DF, CI_Right_Thalamus_Proper_TBV,by = "Area")
Right_Thalamus_Proper_Final_Effect_Isometry_DF <- inner_join(Right_Thalamus_Proper_Isometry_DF, Right_Thalamus_Proper_Isometry_Test_Tidy_Coef, by = "Area")

##### Left_Thalamus_Proper
# 1. Remove comorbidities

Left_Thalamus_Proper_model_COntrols <- lmer(Left_Thalamus_Proper_log~ Total_Brain_Vol_log + Medication + 
                                              + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Left_Thalamus_Proper_model_COntrols)$coefficients 
p.adjust(summary(Left_Thalamus_Proper_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Left_Thalamus_Proper_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Left_Thalamus_Proper)
Left_Thalamus_Proper_outliers <- Left_Thalamus_Proper_Boxplot$out # You can get the actual values of the outliers with this
Left_Thalamus_Proper_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Left_Thalamus_Proper %in% Left_Thalamus_Proper_outliers),]
Left_Thalamus_Proper_Boxplot <- boxplot(Left_Thalamus_Proper_Outlier_DF_C$Left_Thalamus_Proper)

TBV_Boxplot <- boxplot(Left_Thalamus_Proper_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Left_Thalamus_Proper_Outlier_DF_C <- Left_Thalamus_Proper_Outlier_DF_C[-which(Left_Thalamus_Proper_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Left_Thalamus_Proper_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Left_Thalamus_Proper_model_Controls_no_uni_out <- lmer(Left_Thalamus_Proper_log~ Total_Brain_Vol_log + Medication + 
                                                         + (1|SITE_ID2), Left_Thalamus_Proper_Outlier_DF_C)

summary(Left_Thalamus_Proper_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Left_Thalamus_Proper_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Left_Thalamus_Proper_model_no_outliers_coef <- summary(Left_Thalamus_Proper_model_Controls_no_uni_out)$coefficients[2]
Left_Thalamus_Proper_model_no_outliers_coef_DF <- data.frame(Left_Thalamus_Proper_model_no_outliers_coef)

# Rename columns & Rows 
names(Left_Thalamus_Proper_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Left_Thalamus_Proper_model_no_outliers_coef_DF) <- "Left Thalamus_Proper"
Left_Thalamus_Proper_model_no_outliers_coef_DF$Area <- rownames(Left_Thalamus_Proper_model_no_outliers_coef_DF)
Left_Thalamus_Proper_New_DF <- subset(Left_Thalamus_Proper_model_no_outliers_coef_DF, select=c(2,1))

CI_Left_Thalamus_Proper <- confint(Left_Thalamus_Proper_model_Controls_no_uni_out)
CI_Left_Thalamus_Proper_tidy <- tidy(CI_Left_Thalamus_Proper)
CI_Left_Thalamus_Proper_TBV <- CI_Left_Thalamus_Proper_tidy[c(4),]
CI_Left_Thalamus_Proper_TBV_DF <- data.frame(CI_Left_Thalamus_Proper_TBV)

names(CI_Left_Putamen_TBV)[1] <- "Area"
names(CI_Left_Thalamus_Proper_TBV)[2] <- "CILow"
names(CI_Left_Thalamus_Proper_TBV)[3] <- "CIHigh"
CI_Left_Thalamus_Proper_TBV$Area <- "Left Thalamus_Proper"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Left_Thalamus_Proper_Isometry_Test <- Isometry_Test(Left_Thalamus_Proper_model_Controls_no_uni_out)
Left_Thalamus_Proper_Isometry_Test_Tidy <- tidy(Left_Thalamus_Proper_Isometry_Test)
Left_Thalamus_Proper_Isometry_Test_Tidy_Coef <- Left_Thalamus_Proper_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Left_Thalamus_Proper_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Left_Thalamus_Proper_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Left_Thalamus_Proper_Isometry_Test_Tidy_Coef) <- "Left Thalamus_Proper"
Left_Thalamus_Proper_Isometry_Test_Tidy_Coef$Area <- rownames(Left_Thalamus_Proper_Isometry_Test_Tidy_Coef)

Left_Thalamus_Proper_Isometry_DF <- inner_join(Left_Thalamus_Proper_New_DF, CI_Left_Thalamus_Proper_TBV,by = "Area")
Left_Thalamus_Proper_Final_Effect_Isometry_DF <- inner_join(Left_Thalamus_Proper_Isometry_DF, Left_Thalamus_Proper_Isometry_Test_Tidy_Coef, by = "Area")
##### Right_Cerebellum_Cortex
# 1. Remove comorbidities

Right_Cerebellum_Cortex_model_COntrols <- lmer(Right_Cerebellum_Cortex_log~ Total_Brain_Vol_log + Medication + 
                                                 + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Right_Cerebellum_Cortex_model_COntrols)$coefficients 
p.adjust(summary(Right_Cerebellum_Cortex_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Right_Cerebellum_Cortex_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Right_Cerebellum_Cortex)
Right_Cerebellum_Cortex_outliers <- Right_Cerebellum_Cortex_Boxplot$out # You can get the actual values of the outliers with this
Right_Cerebellum_Cortex_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Right_Cerebellum_Cortex %in% Right_Cerebellum_Cortex_outliers),]
Right_Cerebellum_Cortex_Boxplot <- boxplot(Right_Cerebellum_Cortex_Outlier_DF_C$Right_Cerebellum_Cortex)

TBV_Boxplot <- boxplot(Right_Cerebellum_Cortex_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Right_Cerebellum_Cortex_Outlier_DF_C <- Right_Cerebellum_Cortex_Outlier_DF_C[-which(Right_Cerebellum_Cortex_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Right_Cerebellum_Cortex_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Right_Cerebellum_Cortex_model_Controls_no_uni_out <- lmer(Right_Cerebellum_Cortex_log~ Total_Brain_Vol_log + Medication + 
                                                            + (1|SITE_ID2), Right_Cerebellum_Cortex_Outlier_DF_C)

summary(Right_Cerebellum_Cortex_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Right_Cerebellum_Cortex_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Right_Cerebellum_Cortex_model_no_outliers_coef <- summary(Right_Cerebellum_Cortex_model_Controls_no_uni_out)$coefficients[2]
Right_Cerebellum_Cortex_model_no_outliers_coef_DF <- data.frame(Right_Cerebellum_Cortex_model_no_outliers_coef)

# Rename columns & Rows 
names(Right_Cerebellum_Cortex_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Right_Cerebellum_Cortex_model_no_outliers_coef_DF) <- "Right Cerebellum_Cortex"
Right_Cerebellum_Cortex_model_no_outliers_coef_DF$Area <- rownames(Right_Cerebellum_Cortex_model_no_outliers_coef_DF)
Right_Cerebellum_Cortex_New_DF <- subset(Right_Cerebellum_Cortex_model_no_outliers_coef_DF, select=c(2,1))

CI_Right_Cerebellum_Cortex <- confint(Right_Cerebellum_Cortex_model_Controls_no_uni_out)
CI_Right_Cerebellum_Cortex_tidy <- tidy(CI_Right_Cerebellum_Cortex)
CI_Right_Cerebellum_Cortex_TBV <- CI_Right_Cerebellum_Cortex_tidy[c(4),]
CI_Right_Cerebellum_Cortex_TBV_DF <- data.frame(CI_Right_Cerebellum_Cortex_TBV)

names(CI_Right_Cerebellum_Cortex_TBV)[1] <- "Area"
names(CI_Right_Cerebellum_Cortex_TBV)[2] <- "CILow"
names(CI_Right_Cerebellum_Cortex_TBV)[3] <- "CIHigh"
CI_Right_Cerebellum_Cortex_TBV$Area <- "Right Cerebellum_Cortex"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Right_Cerebellum_Cortex_Isometry_Test <- Isometry_Test(Right_Cerebellum_Cortex_model_Controls_no_uni_out)
Right_Cerebellum_Cortex_Isometry_Test_Tidy <- tidy(Right_Cerebellum_Cortex_Isometry_Test)
Right_Cerebellum_Cortex_Isometry_Test_Tidy_Coef <- Right_Cerebellum_Cortex_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Right_Cerebellum_Cortex_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Right_Cerebellum_Cortex_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Right_Cerebellum_Cortex_Isometry_Test_Tidy_Coef) <- "Right Cerebellum_Cortex"
Right_Cerebellum_Cortex_Isometry_Test_Tidy_Coef$Area <- rownames(Right_Cerebellum_Cortex_Isometry_Test_Tidy_Coef)

Right_Cerebellum_Cortex_Isometry_DF <- inner_join(Right_Cerebellum_Cortex_New_DF, CI_Right_Cerebellum_Cortex_TBV,by = "Area")
Right_Cerebellum_Cortex_Final_Effect_Isometry_DF <- inner_join(Right_Cerebellum_Cortex_Isometry_DF, Right_Cerebellum_Cortex_Isometry_Test_Tidy_Coef, by = "Area")

##### Left_Cerebellum_Cortex
# 1. Remove comorbidities

Left_Cerebellum_Cortex_model_COntrols <- lmer(Left_Cerebellum_Cortex_log~ Total_Brain_Vol_log + Medication + 
                                                + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Left_Cerebellum_Cortex_model_COntrols)$coefficients 
p.adjust(summary(Left_Cerebellum_Cortex_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Left_Cerebellum_Cortex_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Left_Cerebellum_Cortex)
Left_Cerebellum_Cortex_outliers <- Left_Cerebellum_Cortex_Boxplot$out # You can get the actual values of the outliers with this
Left_Cerebellum_Cortex_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Left_Cerebellum_Cortex %in% Left_Cerebellum_Cortex_outliers),]
Left_Cerebellum_Cortex_Boxplot <- boxplot(Left_Cerebellum_Cortex_Outlier_DF_C$Left_Cerebellum_Cortex)

TBV_Boxplot <- boxplot(Left_Cerebellum_Cortex_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Left_Cerebellum_Cortex_Outlier_DF_C <- Left_Cerebellum_Cortex_Outlier_DF_C[-which(Left_Cerebellum_Cortex_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Left_Cerebellum_Cortex_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Left_Cerebellum_Cortex_model_Controls_no_uni_out <- lmer(Left_Cerebellum_Cortex_log~ Total_Brain_Vol_log + Medication + 
                                                           + (1|SITE_ID2), Left_Cerebellum_Cortex_Outlier_DF_C)

summary(Left_Cerebellum_Cortex_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Left_Cerebellum_Cortex_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Left_Cerebellum_Cortex_model_no_outliers_coef <- summary(Left_Cerebellum_Cortex_model_Controls_no_uni_out)$coefficients[2]
Left_Cerebellum_Cortex_model_no_outliers_coef_DF <- data.frame(Left_Cerebellum_Cortex_model_no_outliers_coef)

# Rename columns & Rows 
names(Left_Cerebellum_Cortex_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Left_Cerebellum_Cortex_model_no_outliers_coef_DF) <- "Left Cerebellum_Cortex"
Left_Cerebellum_Cortex_model_no_outliers_coef_DF$Area <- rownames(Left_Cerebellum_Cortex_model_no_outliers_coef_DF)
Left_Cerebellum_Cortex_New_DF <- subset(Left_Cerebellum_Cortex_model_no_outliers_coef_DF, select=c(2,1))

CI_Left_Cerebellum_Cortex <- confint(Left_Cerebellum_Cortex_model_Controls_no_uni_out)
CI_Left_Cerebellum_Cortex_tidy <- tidy(CI_Left_Cerebellum_Cortex)
CI_Left_Cerebellum_Cortex_TBV <- CI_Left_Cerebellum_Cortex_tidy[c(4),]
CI_Left_Cerebellum_Cortex_TBV_DF <- data.frame(CI_Left_Cerebellum_Cortex_TBV)

names(CI_Left_Cerebellum_Cortex_TBV)[1] <- "Area"
names(CI_Left_Cerebellum_Cortex_TBV)[2] <- "CILow"
names(CI_Left_Cerebellum_Cortex_TBV)[3] <- "CIHigh"
CI_Left_Cerebellum_Cortex_TBV$Area <- "Left Cerebellum_Cortex"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Left_Cerebellum_Cortex_Isometry_Test <- Isometry_Test(Left_Cerebellum_Cortex_model_Controls_no_uni_out)
Left_Cerebellum_Cortex_Isometry_Test_Tidy <- tidy(Left_Cerebellum_Cortex_Isometry_Test)
Left_Cerebellum_Cortex_Isometry_Test_Tidy_Coef <- Left_Cerebellum_Cortex_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Left_Cerebellum_Cortex_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Left_Cerebellum_Cortex_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Left_Cerebellum_Cortex_Isometry_Test_Tidy_Coef) <- "Left Cerebellum_Cortex"
Left_Cerebellum_Cortex_Isometry_Test_Tidy_Coef$Area <- rownames(Left_Cerebellum_Cortex_Isometry_Test_Tidy_Coef)

Left_Cerebellum_Cortex_Isometry_DF <- inner_join(Left_Cerebellum_Cortex_New_DF, CI_Left_Cerebellum_Cortex_TBV,by = "Area")
Left_Cerebellum_Cortex_Final_Effect_Isometry_DF <- inner_join(Left_Cerebellum_Cortex_Isometry_DF, Left_Cerebellum_Cortex_Isometry_Test_Tidy_Coef, by = "Area")

##### Right_Ventral_DC
# 1. Remove comorbidities

Right_Ventral_DC_model_COntrols <- lmer(Right_Ventral_DC_log~ Total_Brain_Vol_log + Medication + 
                                          + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Right_Ventral_DC_model_COntrols)$coefficients 
p.adjust(summary(Right_Ventral_DC_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Right_Ventral_DC_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Right_Ventral_DC)
Right_Ventral_DC_outliers <- Right_Ventral_DC_Boxplot$out # You can get the actual values of the outliers with this
Right_Ventral_DC_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Right_Ventral_DC %in% Right_Ventral_DC_outliers),]
Right_Ventral_DC_Boxplot <- boxplot(Right_Ventral_DC_Outlier_DF_C$Right_Ventral_DC)

TBV_Boxplot <- boxplot(Right_Ventral_DC_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Right_Ventral_DC_Outlier_DF_C <- Right_Ventral_DC_Outlier_DF_C[-which(Right_Ventral_DC_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Right_Ventral_DC_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Right_Ventral_DC_model_Controls_no_uni_out <- lmer(Right_Ventral_DC_log~ Total_Brain_Vol_log + Medication + 
                                                     + (1|SITE_ID2), Right_Ventral_DC_Outlier_DF_C)

summary(Right_Ventral_DC_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Right_Ventral_DC_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Right_Ventral_DC_model_no_outliers_coef <- summary(Right_Ventral_DC_model_Controls_no_uni_out)$coefficients[2]
Right_Ventral_DC_model_no_outliers_coef_DF <- data.frame(Right_Ventral_DC_model_no_outliers_coef)

# Rename columns & Rows 
names(Right_Ventral_DC_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Right_Ventral_DC_model_no_outliers_coef_DF) <- "Right Ventral_DC"
Right_Ventral_DC_model_no_outliers_coef_DF$Area <- rownames(Right_Ventral_DC_model_no_outliers_coef_DF)
Right_Ventral_DC_New_DF <- subset(Right_Ventral_DC_model_no_outliers_coef_DF, select=c(2,1))

CI_Right_Ventral_DC <- confint(Right_Ventral_DC_model_Controls_no_uni_out)
CI_Right_Ventral_DC_tidy <- tidy(CI_Right_Ventral_DC)
CI_Right_Ventral_DC_TBV <- CI_Right_Ventral_DC_tidy[c(4),]
CI_Right_Ventral_DC_TBV_DF <- data.frame(CI_Right_Ventral_DC_TBV)

names(CI_Right_Ventral_DC_TBV)[1] <- "Area"
names(CI_Right_Ventral_DC_TBV)[2] <- "CILow"
names(CI_Right_Ventral_DC_TBV)[3] <- "CIHigh"
CI_Right_Ventral_DC_TBV$Area <- "Right Ventral_DC"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Right_Ventral_DC_Isometry_Test <- Isometry_Test(Right_Ventral_DC_model_Controls_no_uni_out)
Right_Ventral_DC_Isometry_Test_Tidy <- tidy(Right_Ventral_DC_Isometry_Test)
Right_Ventral_DC_Isometry_Test_Tidy_Coef <- Right_Ventral_DC_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Right_Ventral_DC_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Right_Ventral_DC_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Right_Ventral_DC_Isometry_Test_Tidy_Coef) <- "Right Ventral_DC"
Right_Ventral_DC_Isometry_Test_Tidy_Coef$Area <- rownames(Right_Ventral_DC_Isometry_Test_Tidy_Coef)

Right_Ventral_DC_Isometry_DF <- inner_join(Right_Ventral_DC_New_DF, CI_Right_Ventral_DC_TBV,by = "Area")
Right_Ventral_DC_Final_Effect_Isometry_DF <- inner_join(Right_Ventral_DC_Isometry_DF, Right_Ventral_DC_Isometry_Test_Tidy_Coef, by = "Area")

##### Left_Ventral_DC
# 1. Remove comorbidities

Left_Ventral_DC_model_COntrols <- lmer(Left_Ventral_DC_log~ Total_Brain_Vol_log + Medication + 
                                         + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Left_Ventral_DC_model_COntrols)$coefficients 
p.adjust(summary(Left_Ventral_DC_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Left_Ventral_DC_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Left_Ventral_DC)
Left_Ventral_DC_outliers <- Left_Ventral_DC_Boxplot$out # You can get the actual values of the outliers with this
Left_Ventral_DC_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Left_Ventral_DC %in% Left_Ventral_DC_outliers),]
Left_Ventral_DC_Boxplot <- boxplot(Left_Ventral_DC_Outlier_DF_C$Left_Ventral_DC)

TBV_Boxplot <- boxplot(Left_Ventral_DC_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Left_Ventral_DC_Outlier_DF_C <- Left_Ventral_DC_Outlier_DF_C[-which(Left_Ventral_DC_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Left_Ventral_DC_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Left_Ventral_DC_model_Controls_no_uni_out <- lmer(Left_Ventral_DC_log~ Total_Brain_Vol_log + Medication + 
                                                    + (1|SITE_ID2), Left_Ventral_DC_Outlier_DF_C)

summary(Left_Ventral_DC_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Left_Ventral_DC_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Left_Ventral_DC_model_no_outliers_coef <- summary(Left_Ventral_DC_model_Controls_no_uni_out)$coefficients[2]
Left_Ventral_DC_model_no_outliers_coef_DF <- data.frame(Left_Ventral_DC_model_no_outliers_coef)

# Rename columns & Rows 
names(Left_Ventral_DC_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Left_Ventral_DC_model_no_outliers_coef_DF) <- "Left Ventral_DC"
Left_Ventral_DC_model_no_outliers_coef_DF$Area <- rownames(Left_Ventral_DC_model_no_outliers_coef_DF)
Left_Ventral_DC_New_DF <- subset(Left_Ventral_DC_model_no_outliers_coef_DF, select=c(2,1))

CI_Left_Ventral_DC <- confint(Left_Ventral_DC_model_Controls_no_uni_out)
CI_Left_Ventral_DC_tidy <- tidy(CI_Left_Ventral_DC)
CI_Left_Ventral_DC_TBV <- CI_Left_Ventral_DC_tidy[c(4),]
CI_Left_Ventral_DC_TBV_DF <- data.frame(CI_Left_Ventral_DC_TBV)

names(CI_Left_Ventral_DC_TBV)[1] <- "Area"
names(CI_Left_Ventral_DC_TBV)[2] <- "CILow"
names(CI_Left_Ventral_DC_TBV)[3] <- "CIHigh"
CI_Left_Ventral_DC_TBV$Area <- "Left Ventral_DC"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Left_Ventral_DC_Isometry_Test <- Isometry_Test(Left_Ventral_DC_model_Controls_no_uni_out)
Left_Ventral_DC_Isometry_Test_Tidy <- tidy(Left_Ventral_DC_Isometry_Test)
Left_Ventral_DC_Isometry_Test_Tidy_Coef <- Left_Ventral_DC_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Left_Ventral_DC_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Left_Ventral_DC_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Left_Ventral_DC_Isometry_Test_Tidy_Coef) <- "Left Ventral_DC"
Left_Ventral_DC_Isometry_Test_Tidy_Coef$Area <- rownames(Left_Ventral_DC_Isometry_Test_Tidy_Coef)

Left_Ventral_DC_Isometry_DF <- inner_join(Left_Ventral_DC_New_DF, CI_Left_Ventral_DC_TBV,by = "Area")
Left_Ventral_DC_Final_Effect_Isometry_DF <- inner_join(Left_Ventral_DC_Isometry_DF, Left_Ventral_DC_Isometry_Test_Tidy_Coef, by = "Area")
##### Right_CortexVol
# 1. Remove comorbidities

Right_CortexVol_model_COntrols <- lmer(Right_CortexVol_log~ Total_Brain_Vol_log + Medication + 
                                         + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Right_CortexVol_model_COntrols)$coefficients 
p.adjust(summary(Right_CortexVol_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Right_CortexVol_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Right_CortexVol)
Right_CortexVol_outliers <- Right_CortexVol_Boxplot$out # You can get the actual values of the outliers with this
Right_CortexVol_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Right_CortexVol %in% Right_CortexVol_outliers),]
Right_CortexVol_Boxplot <- boxplot(Right_CortexVol_Outlier_DF_C$Right_CortexVol)

TBV_Boxplot <- boxplot(Right_CortexVol_Outlier_DF_C$Total_Brain_Vol)
Right_CortexVol_Outlier_DF_C <- Right_CortexVol_Outlier_DF_C

#### 1. Removal of univariate outliers analyses 
Right_CortexVol_model_Controls_no_uni_out <- lmer(Right_CortexVol_log~ Total_Brain_Vol_log + Medication + 
                                                    + (1|SITE_ID2), Right_CortexVol_Outlier_DF_C)

summary(Right_CortexVol_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Right_CortexVol_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Right_CortexVol_model_no_outliers_coef <- summary(Right_CortexVol_model_Controls_no_uni_out)$coefficients[2]
Right_CortexVol_model_no_outliers_coef_DF <- data.frame(Right_CortexVol_model_no_outliers_coef)

# Rename columns & Rows 
names(Right_CortexVol_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Right_CortexVol_model_no_outliers_coef_DF) <- "Right CortexVol"
Right_CortexVol_model_no_outliers_coef_DF$Area <- rownames(Right_CortexVol_model_no_outliers_coef_DF)
Right_CortexVol_New_DF <- subset(Right_CortexVol_model_no_outliers_coef_DF, select=c(2,1))

CI_Right_CortexVol <- confint(Right_CortexVol_model_Controls_no_uni_out)
CI_Right_CortexVol_tidy <- tidy(CI_Right_CortexVol)
CI_Right_CortexVol_TBV <- CI_Right_CortexVol_tidy[c(4),]
CI_Right_CortexVol_TBV_DF <- data.frame(CI_Right_CortexVol_TBV)

names(CI_Right_CortexVol_TBV)[1] <- "Area"
names(CI_Right_CortexVol_TBV)[2] <- "CILow"
names(CI_Right_CortexVol_TBV)[3] <- "CIHigh"
CI_Right_CortexVol_TBV$Area <- "Right CortexVol"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Right_CortexVol_Isometry_Test <- Isometry_Test(Right_CortexVol_model_Controls_no_uni_out)
Right_CortexVol_Isometry_Test_Tidy <- tidy(Right_CortexVol_Isometry_Test)
Right_CortexVol_Isometry_Test_Tidy_Coef <- Right_CortexVol_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Right_CortexVol_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Right_CortexVol_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Right_CortexVol_Isometry_Test_Tidy_Coef) <- "Right CortexVol"
Right_CortexVol_Isometry_Test_Tidy_Coef$Area <- rownames(Right_CortexVol_Isometry_Test_Tidy_Coef)

Right_CortexVol_Isometry_DF <- inner_join(Right_CortexVol_New_DF, CI_Right_CortexVol_TBV,by = "Area")
Right_CortexVol_Final_Effect_Isometry_DF <- inner_join(Right_CortexVol_Isometry_DF, Right_CortexVol_Isometry_Test_Tidy_Coef, by = "Area")

##### Left_CortexVol
# 1. Remove comorbidities

Left_CortexVol_model_COntrols <- lmer(Left_CortexVol_log~ Total_Brain_Vol_log + Medication + 
                                        + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Left_CortexVol_model_COntrols)$coefficients 
p.adjust(summary(Left_CortexVol_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
Left_CortexVol_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Left_CortexVol)
Left_CortexVol_outliers <- Left_CortexVol_Boxplot$out # You can get the actual values of the outliers with this
Left_CortexVol_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Left_CortexVol %in% Left_CortexVol_outliers),]
Left_CortexVol_Boxplot <- boxplot(Left_CortexVol_Outlier_DF_C$Left_CortexVol)

TBV_Boxplot <- boxplot(Left_CortexVol_Outlier_DF_C$Total_Brain_Vol)
Left_CortexVol_Outlier_DF_C <- Left_CortexVol_Outlier_DF_C

#### 1. Removal of univariate outliers analyses 
Left_CortexVol_model_Controls_no_uni_out <- lmer(Left_CortexVol_log~ Total_Brain_Vol_log + Medication + 
                                                   + (1|SITE_ID2), Left_CortexVol_Outlier_DF_C)

summary(Left_CortexVol_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Left_CortexVol_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Left_CortexVol_model_no_outliers_coef <- summary(Left_CortexVol_model_Controls_no_uni_out)$coefficients[2]
Left_CortexVol_model_no_outliers_coef_DF <- data.frame(Left_CortexVol_model_no_outliers_coef)

# Rename columns & Rows 
names(Left_CortexVol_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Left_CortexVol_model_no_outliers_coef_DF) <- "Left CortexVol"
Left_CortexVol_model_no_outliers_coef_DF$Area <- rownames(Left_CortexVol_model_no_outliers_coef_DF)
Left_CortexVol_New_DF <- subset(Left_CortexVol_model_no_outliers_coef_DF, select=c(2,1))

CI_Left_CortexVol <- confint(Left_CortexVol_model_Controls_no_uni_out)
CI_Left_CortexVol_tidy <- tidy(CI_Left_CortexVol)
CI_Left_CortexVol_TBV <- CI_Left_CortexVol_tidy[c(4),]
CI_Left_CortexVol_TBV_DF <- data.frame(CI_Left_CortexVol_TBV)

names(CI_Left_CortexVol_TBV)[1] <- "Area"
names(CI_Left_CortexVol_TBV)[2] <- "CILow"
names(CI_Left_CortexVol_TBV)[3] <- "CIHigh"
CI_Left_CortexVol_TBV$Area <- "Left CortexVol"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Left_CortexVol_Isometry_Test <- Isometry_Test(Left_CortexVol_model_Controls_no_uni_out)
Left_CortexVol_Isometry_Test_Tidy <- tidy(Left_CortexVol_Isometry_Test)
Left_CortexVol_Isometry_Test_Tidy_Coef <- Left_CortexVol_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Left_CortexVol_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Left_CortexVol_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Left_CortexVol_Isometry_Test_Tidy_Coef) <- "Left CortexVol"
Left_CortexVol_Isometry_Test_Tidy_Coef$Area <- rownames(Left_CortexVol_Isometry_Test_Tidy_Coef)

Left_CortexVol_Isometry_DF <- inner_join(Left_CortexVol_New_DF, CI_Left_CortexVol_TBV,by = "Area")
Left_CortexVol_Final_Effect_Isometry_DF <- inner_join(Left_CortexVol_Isometry_DF, Left_CortexVol_Isometry_Test_Tidy_Coef, by = "Area")
##### TotalGrayVol
# 1. Remove comorbidities

TotalGrayVol_model_COntrols <- lmer(TotalGrayVol_log~ Total_Brain_Vol_log + Medication + 
                                      + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(TotalGrayVol_model_COntrols)$coefficients 
p.adjust(summary(TotalGrayVol_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
TotalGrayVol_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$TotalGrayVol)
TotalGrayVol_outliers <- TotalGrayVol_Boxplot$out # You can get the actual values of the outliers with this
TotalGrayVol_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$TotalGrayVol %in% TotalGrayVol_outliers),]
TotalGrayVol_Boxplot <- boxplot(TotalGrayVol_Outlier_DF_C$TotalGrayVol)

TBV_Boxplot <- boxplot(TotalGrayVol_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
TotalGrayVol_model_Controls_no_uni_out <- lmer(TotalGrayVol_log~ Total_Brain_Vol_log + Medication + 
                                                 + (1|SITE_ID2), TotalGrayVol_Outlier_DF_C)

summary(TotalGrayVol_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(TotalGrayVol_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

TotalGrayVol_model_no_outliers_coef <- summary(TotalGrayVol_model_Controls_no_uni_out)$coefficients[2]
TotalGrayVol_model_no_outliers_coef_DF <- data.frame(TotalGrayVol_model_no_outliers_coef)

# Rename columns & Rows 
names(TotalGrayVol_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(TotalGrayVol_model_no_outliers_coef_DF) <- "Total Grey Volume"
TotalGrayVol_model_no_outliers_coef_DF$Area <- rownames(TotalGrayVol_model_no_outliers_coef_DF)
TotalGrayVol_New_DF <- subset(TotalGrayVol_model_no_outliers_coef_DF, select=c(2,1))

CI_TotalGrayVol <- confint(TotalGrayVol_model_Controls_no_uni_out)
CI_TotalGrayVol_tidy <- tidy(CI_TotalGrayVol)
CI_TotalGrayVol_TBV <- CI_TotalGrayVol_tidy[c(4),]
CI_TotalGrayVol_TBV_DF <- data.frame(CI_TotalGrayVol_TBV)

names(CI_TotalGrayVol_TBV)[1] <- "Area"
names(CI_TotalGrayVol_TBV)[2] <- "CILow"
names(CI_TotalGrayVol_TBV)[3] <- "CIHigh"
CI_TotalGrayVol_TBV$Area <- "Total Grey Volume"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
TotalGrayVol_Isometry_Test <- Isometry_Test(TotalGrayVol_model_Controls_no_uni_out)
TotalGrayVol_Isometry_Test_Tidy <- tidy(TotalGrayVol_Isometry_Test)
TotalGrayVol_Isometry_Test_Tidy_Coef <- TotalGrayVol_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(TotalGrayVol_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(TotalGrayVol_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(TotalGrayVol_Isometry_Test_Tidy_Coef) <- "Total Grey Volume"
TotalGrayVol_Isometry_Test_Tidy_Coef$Area <- rownames(TotalGrayVol_Isometry_Test_Tidy_Coef)

TotalGrayVol_Isometry_DF <- inner_join(TotalGrayVol_New_DF, CI_TotalGrayVol_TBV,by = "Area")
TotalGrayVol_Final_Effect_Isometry_DF <- inner_join(TotalGrayVol_Isometry_DF, TotalGrayVol_Isometry_Test_Tidy_Coef, by = "Area")

##### Brain_Stem
# 1. Remove comorbidities

Brain_Stem_model_COntrols <- lmer(Brain_Stem_log~ Total_Brain_Vol_log + Medication + 
                                    + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(Brain_Stem_model_COntrols)$coefficients 
p.adjust(summary(Brain_Stem_model_COntrols)$coefficients[,5], method = 'fdr') 

## UNIVARIATE OUTLIERS 
Brain_Stem_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$Brain_Stem)
Brain_Stem_outliers <- Brain_Stem_Boxplot$out # You can get the actual values of the outliers with this
Brain_Stem_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$Brain_Stem %in% Brain_Stem_outliers),]
Brain_Stem_Boxplot <- boxplot(Brain_Stem_Outlier_DF_C$Brain_Stem)

TBV_Boxplot <- boxplot(Brain_Stem_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Brain_Stem_Outlier_DF_C <- Brain_Stem_Outlier_DF_C[-which(Brain_Stem_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(Brain_Stem_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
Brain_Stem_model_Controls_no_uni_out <- lmer(Brain_Stem_log~ Total_Brain_Vol_log + Medication + 
                                               + (1|SITE_ID2), Brain_Stem_Outlier_DF_C)

summary(Brain_Stem_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(Brain_Stem_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

Brain_Stem_model_no_outliers_coef <- summary(Brain_Stem_model_Controls_no_uni_out)$coefficients[2]
Brain_Stem_model_no_outliers_coef_DF <- data.frame(Brain_Stem_model_no_outliers_coef)

# Rename columns & Rows 
names(Brain_Stem_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(Brain_Stem_model_no_outliers_coef_DF) <- "Brain Stem"
Brain_Stem_model_no_outliers_coef_DF$Area <- rownames(Brain_Stem_model_no_outliers_coef_DF)
Brain_Stem_New_DF <- subset(Brain_Stem_model_no_outliers_coef_DF, select=c(2,1))

CI_Brain_Stem <- confint(Brain_Stem_model_Controls_no_uni_out)
CI_Brain_Stem_tidy <- tidy(CI_Brain_Stem)
CI_Brain_Stem_TBV <- CI_Brain_Stem_tidy[c(4),]
CI_Brain_Stem_TBV_DF <- data.frame(CI_Brain_Stem_TBV)

names(CI_Brain_Stem_TBV)[1] <- "Area"
names(CI_Brain_Stem_TBV)[2] <- "CILow"
names(CI_Brain_Stem_TBV)[3] <- "CIHigh"
CI_Brain_Stem_TBV$Area <- "Brain Stem"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
Brain_Stem_Isometry_Test <- Isometry_Test(Brain_Stem_model_Controls_no_uni_out)
Brain_Stem_Isometry_Test_Tidy <- tidy(Brain_Stem_Isometry_Test)
Brain_Stem_Isometry_Test_Tidy_Coef <- Brain_Stem_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(Brain_Stem_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(Brain_Stem_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(Brain_Stem_Isometry_Test_Tidy_Coef) <- "Brain Stem"
Brain_Stem_Isometry_Test_Tidy_Coef$Area <- rownames(Brain_Stem_Isometry_Test_Tidy_Coef)

Brain_Stem_Isometry_DF <- inner_join(Brain_Stem_New_DF, CI_Brain_Stem_TBV,by = "Area")
Brain_Stem_Final_Effect_Isometry_DF <- inner_join(Brain_Stem_Isometry_DF, Brain_Stem_Isometry_Test_Tidy_Coef, by = "Area")

##### CorticalWhiteMatterVol
# 1. Remove comorbidities

CorticalWhiteMatterVol_model_COntrols <- lmer(CorticalWhiteMatterVol_log~ Total_Brain_Vol_log + Medication + 
                                                + (1|SITE_ID2), Abide_Jong_Controls_No_Comorbidity)

summary(CorticalWhiteMatterVol_model_COntrols)$coefficients 
p.adjust(summary(CorticalWhiteMatterVol_model_COntrols)$coefficients[,5], method = 'fdr') 


## UNIVARIATE OUTLIERS 
CorticalWhiteMatterVol_Boxplot <- boxplot(Abide_Jong_Controls_No_Comorbidity$CorticalWhiteMatterVol)
CorticalWhiteMatterVol_outliers <- CorticalWhiteMatterVol_Boxplot$out # You can get the actual values of the outliers with this
CorticalWhiteMatterVol_Outlier_DF_C <- Abide_Jong_Controls_No_Comorbidity[-which(Abide_Jong_Controls_No_Comorbidity$CorticalWhiteMatterVol %in% CorticalWhiteMatterVol_outliers),]
CorticalWhiteMatterVol_Boxplot <- boxplot(CorticalWhiteMatterVol_Outlier_DF_C$CorticalWhiteMatterVol)

TBV_Boxplot <- boxplot(CorticalWhiteMatterVol_Outlier_DF_C$Total_Brain_Vol)
TBV_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
CorticalWhiteMatterVol_Outlier_DF_C <- CorticalWhiteMatterVol_Outlier_DF_C[-which(CorticalWhiteMatterVol_Outlier_DF_C$Total_Brain_Vol %in% TBV_outliers),]
Total_Brain_Vol_Boxplot <- boxplot(CorticalWhiteMatterVol_Outlier_DF_C$Total_Brain_Vol)

#### 1. Removal of univariate outliers analyses 
CorticalWhiteMatterVol_model_Controls_no_uni_out <- lmer(CorticalWhiteMatterVol_log~ Total_Brain_Vol_log + Medication + 
                                                           + (1|SITE_ID2), CorticalWhiteMatterVol_Outlier_DF_C)

summary(CorticalWhiteMatterVol_model_Controls_no_uni_out)$coefficients 
p.adjust(summary(CorticalWhiteMatterVol_model_Controls_no_uni_out)$coefficients[,5], method = 'fdr') 

CorticalWhiteMatterVol_model_no_outliers_coef <- summary(CorticalWhiteMatterVol_model_Controls_no_uni_out)$coefficients[2]
CorticalWhiteMatterVol_model_no_outliers_coef_DF <- data.frame(CorticalWhiteMatterVol_model_no_outliers_coef)

# Rename columns & Rows 
names(CorticalWhiteMatterVol_model_no_outliers_coef_DF)[1] <- "Alpha"
row.names(CorticalWhiteMatterVol_model_no_outliers_coef_DF) <- "Cortical White Matter"
CorticalWhiteMatterVol_model_no_outliers_coef_DF$Area <- rownames(CorticalWhiteMatterVol_model_no_outliers_coef_DF)
CorticalWhiteMatterVol_New_DF <- subset(CorticalWhiteMatterVol_model_no_outliers_coef_DF, select=c(2,1))

CI_CorticalWhiteMatterVol <- confint(CorticalWhiteMatterVol_model_Controls_no_uni_out)
CI_CorticalWhiteMatterVol_tidy <- tidy(CI_CorticalWhiteMatterVol)
CI_CorticalWhiteMatterVol_TBV <- CI_CorticalWhiteMatterVol_tidy[c(4),]
CI_CorticalWhiteMatterVol_TBV_DF <- data.frame(CI_CorticalWhiteMatterVol_TBV)

names(CI_CorticalWhiteMatterVol_TBV)[1] <- "Area"
names(CI_CorticalWhiteMatterVol_TBV)[2] <- "CILow"
names(CI_CorticalWhiteMatterVol_TBV)[3] <- "CIHigh"
CI_CorticalWhiteMatterVol_TBV$Area <- "Cortical White Matter"

Isometry_Test <- function (x) {linearHypothesis(x, c("Total_Brain_Vol_log = 1"))} 
CorticalWhiteMatterVol_Isometry_Test <- Isometry_Test(CorticalWhiteMatterVol_model_Controls_no_uni_out)
CorticalWhiteMatterVol_Isometry_Test_Tidy <- tidy(CorticalWhiteMatterVol_Isometry_Test)
CorticalWhiteMatterVol_Isometry_Test_Tidy_Coef <- CorticalWhiteMatterVol_Isometry_Test_Tidy[2, 2:3]

#Form list to a DF
names(CorticalWhiteMatterVol_Isometry_Test_Tidy_Coef)[1] <- "Chi-Square"
names(CorticalWhiteMatterVol_Isometry_Test_Tidy_Coef)[2] <- "p"
row.names(CorticalWhiteMatterVol_Isometry_Test_Tidy_Coef) <- "Cortical White Matter"
CorticalWhiteMatterVol_Isometry_Test_Tidy_Coef$Area <- rownames(CorticalWhiteMatterVol_Isometry_Test_Tidy_Coef)

CorticalWhiteMatterVol_Isometry_DF <- inner_join(CorticalWhiteMatterVol_New_DF, CI_CorticalWhiteMatterVol_TBV,by = "Area")
CorticalWhiteMatterVol_Final_Effect_Isometry_DF <- inner_join(CorticalWhiteMatterVol_Isometry_DF, CorticalWhiteMatterVol_Isometry_Test_Tidy_Coef, by = "Area")

sink("CSV/Allometric_Coef_Univariate_Outliers_Controls.csv")
print(Right_Accumbens_Area_Final_Effect_Isometry_DF)
print(Left_Accumbens_Area_Final_Effect_Isometry_DF)
print(Right_Caudate_Final_Effect_Isometry_DF)
print(Left_Caudate_Final_Effect_Isometry_DF)
print(Right_Amygdala_Final_Effect_Isometry_DF)
print(Left_Amygdala_Final_Effect_Isometry_DF)
print(Right_Hippocampus_Final_Effect_Isometry_DF)
print(Left_Hippocampus_Final_Effect_Isometry_DF)
print(Right_Hippocampus_Final_Effect_Isometry_DF)
print(Right_Putamen_Final_Effect_Isometry_DF)
print(Left_Putamen_Final_Effect_Isometry_DF)
print(Right_Pallidum_Final_Effect_Isometry_DF)
print(Left_Pallidum_Final_Effect_Isometry_DF)
print(Right_Thalamus_Proper_Final_Effect_Isometry_DF)
print(Left_Thalamus_Proper_Final_Effect_Isometry_DF)
print(Right_Cerebellum_Cortex_Final_Effect_Isometry_DF)
print(Left_Cerebellum_Cortex_Final_Effect_Isometry_DF)
print(Right_Ventral_DC_Final_Effect_Isometry_DF)
print(Left_Ventral_DC_Final_Effect_Isometry_DF)
print(Right_CortexVol_Final_Effect_Isometry_DF)
print(Left_CortexVol_Final_Effect_Isometry_DF)
print(TotalGrayVol_Final_Effect_Isometry_DF)
print(Brain_Stem_Final_Effect_Isometry_DF)
print(CorticalWhiteMatterVol_Final_Effect_Isometry_DF)
sink()