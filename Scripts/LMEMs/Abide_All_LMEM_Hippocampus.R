library(lmerTest) 
library(dplyr)
library(simr)
library(Stack)
library(ggplot2)

Abide_FSIQ = Abide_LMEM_log_Clean_scaled
Abide_FSIQ$FSIQ <- scale(Abide_FSIQ$FIQ2)
Abide_FSIQ$AGE <- scale(Abide_FSIQ$AGE_AT_SCAN)
Abide_FSIQ$AGE2 <- scale(Abide_FSIQ$Age2)
Abide_FSIQ$SITE_ID2 <- as.factor(Abide_FSIQ$SITE_ID2)

################################ Right Hippocampus ################################

################ RESEARCH Q1 

#### I. Corresponding LMEMs (// MGCFA) with outliers & comorbidity 

Abide_Age_12_20 <- Abide_FSIQ %>% 
  filter(AGE_AT_SCAN >= 12 & AGE_AT_SCAN < 20 )%>% 
  filter(SEX == "Male")

Abide_Age_12_20$DX_GROUP <- factor(Abide_Age_12_20$DX_GROUP, levels = c("Control", "ASD"))
table(Abide_Age_12_20$DX_GROUP) # C 141 ASD 138

Right_Hippocampus_model <- lmer(Right_Hippocampus_log~ Total_Brain_Vol_log*DX_GROUP
                                + (1|SITE_ID2), Abide_Age_12_20)

summary(Right_Hippocampus_model)$coefficients 
p.adjust(summary(Right_Hippocampus_model)$coefficients[,5], method = 'fdr') 
# Group: 0.22095374 0.08997605 269.83779  2.4556952 1.469209e-02 & 1.958945e-02 
# Group by TBV : -0.32965672 0.10096173 271.54818 -3.2651651 1.234381e-03 & 2.468761e-03 

Abide_Age_12_20_ASD = Abide_Age_12_20 %>%
  filter (DX_GROUP =="ASD")
mean(Abide_Age_12_20_ASD$Right_Hippocampus)

Abide_Age_12_20_C = Abide_Age_12_20 %>%
  filter (DX_GROUP =="Control")
mean(Abide_Age_12_20_C$Right_Hippocampus)

#Right_Hippocampus_MGCFA_model_power_sim <- powerSim(Right_Hippocampus_model,
                                                    test = fixed("Total_Brain_Vol_log:DX_GROUPASD", "sa"))
# Result: 89.10% (87.00, 90.96)
# ASD 
Abide_Age_12_20_ASD <- Abide_Age_12_20 %>% filter (DX_GROUP == "ASD")
Abide_Age_12_20_ASD$Right_Hippocampus_log_not_scaled <- log10(Abide_Age_12_20_ASD$Right_Hippocampus)
Abide_Age_12_20_ASD$Total_Brain_Vol_log_not_scaled <- log10(Abide_Age_12_20_ASD$Total_Brain_Vol)

Right_Hippocampus_model_no_outliers_ASD <- lmer(Right_Hippocampus_log_not_scaled~ Total_Brain_Vol_log_not_scaled
                                                + (1|SITE_ID2), Abide_Age_12_20_ASD)

summary(Right_Hippocampus_model_no_outliers_ASD)$coefficients #  0.6577306 0.08992488 135.8130  7.3142229 2.027613e-11
p.adjust(summary(Right_Hippocampus_model_no_outliers_ASD)$coefficients[,5], method = 'fdr') #  4.055227e-11 

# Controls 
Abide_Age_12_20_C <- Abide_Age_12_20 %>% filter (DX_GROUP == "Control")
Abide_Age_12_20_C$Right_Hippocampus_log_not_scaled <- log10(Abide_Age_12_20_C$Right_Hippocampus)
Abide_Age_12_20_C$Total_Brain_Vol_log_not_scaled <- log10(Abide_Age_12_20_C$Total_Brain_Vol)

Right_Hippocampus_model_no_outliers_C <- lmer(Right_Hippocampus_log_not_scaled~ Total_Brain_Vol_log_not_scaled
                                              + (1|SITE_ID2), Abide_Age_12_20_C)

summary(Right_Hippocampus_model_no_outliers_C)$coefficients # 1.077797  0.1040690 119.6523 10.356569 2.472617e-18
p.adjust(summary(Right_Hippocampus_model_no_outliers_C)$coefficients[,5], method = 'fdr') #4.945233e-18 

#### II. Corresponding LMEMs (// MGCFA) without outliers & comorbidity 

# 1. Remove comorbidities in C and ASD 
Abide_Age_12_20_No_Comorbidity<-dplyr::filter(Abide_Age_12_20,is.na(COMORBIDITY)) 
table(Abide_Age_12_20_No_Comorbidity$DX_GROUP) # C 140 ASD 129

Abide_Age_12_20_No_Comorbidity$MEDICATION_NAME 
#Left_Accumbens_Area_no_outliers_meds <- Left_Accumbens_Area_no_outliers_meds[!is.na(Left_Accumbens_Area_no_outliers_meds$MEDICATION_NAME),]
Abide_Age_12_20_No_Comorbidity$Medication<-if_else(is.na(Abide_Age_12_20_No_Comorbidity$MEDICATION_NAME), 0, 1)
Abide_Age_12_20_No_Comorbidity$Medication<- as.factor(Abide_Age_12_20_No_Comorbidity$Medication)
#Left_Accumbens_Area_no_outliers_no_meds <- subset(Abide_Over_FSIQ_No_Comorbidity, Abide_Over_FSIQ_No_Comorbidity$Medication == 0)
table(Abide_Age_12_20_No_Comorbidity$DX_GROUP) # c 164      63 

# 2. Remove Univariate Outliers 
# Create New DF for each Group 
Abide_Age_12_20_ASD<- Abide_Age_12_20_No_Comorbidity %>% 
  filter(DX_GROUP =="ASD")

Abide_Age_12_20_C <- Abide_Age_12_20_No_Comorbidity %>% 
  filter(DX_GROUP =="Control")

# Examine Boxplot Outliers & remove for Brain Volume DV and IV 
Right_Hippocampus_Boxplot <- boxplot(Abide_Age_12_20_ASD$Right_Hippocampus)
Right_Hippocampus_outliers <- Right_Hippocampus_Boxplot$out # You can get the actual values of the outliers with this
Right_Hippocampus_Outlier_DF_ASD <- Abide_Age_12_20_ASD[-which(Abide_Age_12_20_ASD$Right_Hippocampus %in% Right_Hippocampus_outliers),]
Right_Hippocampus_Boxplot <- boxplot(Right_Hippocampus_Outlier_DF_ASD$Right_Hippocampus)

Right_Hippocampus_Boxplot <- boxplot(Right_Hippocampus_Outlier_DF_ASD$Total_Brain_Vol)
Right_Hippocampus_outliers <- Right_Hippocampus_Boxplot$out # You can get the actual values of the outliers with this
Right_Hippocampus_Outlier_DF_ASD <- Right_Hippocampus_Outlier_DF_ASD[-which(Right_Hippocampus_Outlier_DF_ASD$Total_Brain_Vol %in% Right_Hippocampus_outliers),]
Right_Hippocampus_Boxplot <- boxplot(Right_Hippocampus_Outlier_DF_ASD$Total_Brain_Vol)

Right_Hippocampus_Boxplot <- boxplot(Abide_Age_12_20_C$Right_Hippocampus)
Right_Hippocampus_outliers <- Right_Hippocampus_Boxplot$out # You can get the actual values of the outliers with this
Right_Hippocampus_Outlier_DF_C <- Abide_Age_12_20_C[-which(Abide_Age_12_20_C$Right_Hippocampus %in% Right_Hippocampus_outliers),]
Right_Hippocampus_Boxplot <- boxplot(Right_Hippocampus_Outlier_DF_C$Right_Hippocampus)

# Create New DF with both groups and without univariate outliers 
Right_Hippocampus_NEW_DF <- Stack(Right_Hippocampus_Outlier_DF_C,Right_Hippocampus_Outlier_DF_ASD)
names(Right_Hippocampus_NEW_DF)[names(Right_Hippocampus_NEW_DF) == "DX_GROUP"] <- "Group"
Right_Hippocampus_NEW_DF$Group <- factor(Right_Hippocampus_NEW_DF$Group, levels = c("Control", "ASD"))

table(Right_Hippocampus_NEW_DF$Group) #  137     123 

#3. Test correpsonding LMEMs without univariate outliers and comorbidity 

Right_Hippocampus_model_MGCFA_no_Uni_Out <- lmer(Right_Hippocampus_log~ Total_Brain_Vol_log*Group+ Medication
                                                 + (1|SITE_ID2), Right_Hippocampus_NEW_DF)

summary(Right_Hippocampus_model_MGCFA_no_Uni_Out)$coefficients 
p.adjust(summary(Right_Hippocampus_model_MGCFA_no_Uni_Out)$coefficients[,5], method = 'fdr') 

#Right_Hippocampus_model_power_sim <- powerSim(Right_Hippocampus_model_MGCFA_no_Uni_Out,
                                              test = fixed("Total_Brain_Vol_log:GroupASD", "sa"))
# Result: 82.90% (80.42, 85.18)
# ASD 
Right_Hippocampus_NEW_DF_ASD <- Right_Hippocampus_NEW_DF %>% filter (Group == "ASD")
Right_Hippocampus_NEW_DF_ASD$Right_Hippocampus_log_not_scaled <- log10(Right_Hippocampus_NEW_DF_ASD$Right_Hippocampus)
Right_Hippocampus_NEW_DF_ASD$Total_Brain_Vol_log_not_scaled <- log10(Right_Hippocampus_NEW_DF_ASD$Total_Brain_Vol)

Right_Hippocampus_model_no_outliers_ASD <- lmer(Right_Hippocampus_log_not_scaled~ Total_Brain_Vol_log_not_scaled+Medication
                                                + (1|SITE_ID2), Right_Hippocampus_NEW_DF_ASD)

summary(Right_Hippocampus_model_no_outliers_ASD)$coefficients #
p.adjust(summary(Right_Hippocampus_model_no_outliers_ASD)$coefficients[,5], method = 'fdr') #

# Controls 
Right_Hippocampus_NEW_DF_C <- Right_Hippocampus_NEW_DF %>% filter (Group == "Control")
Right_Hippocampus_NEW_DF_C$Right_Hippocampus_log_not_scaled <- log10(Right_Hippocampus_NEW_DF_C$Right_Hippocampus)
Right_Hippocampus_NEW_DF_C$Total_Brain_Vol_log_not_scaled <- log10(Right_Hippocampus_NEW_DF_C$Total_Brain_Vol)

Right_Hippocampus_model_no_outliers_C <- lmer(Right_Hippocampus_log_not_scaled~ Total_Brain_Vol_log_not_scaled+Medication
                                              + (1|SITE_ID2), Right_Hippocampus_NEW_DF_C)

summary(Right_Hippocampus_model_no_outliers_C)$coefficients # 0.8227678 0.07771572 134.9922 10.58689 1.926945e-19
p.adjust(summary(Right_Hippocampus_model_no_outliers_C)$coefficients[,5], method = 'fdr') #3.853890e-19 

#################################################### RESEARCH Q2 ####################################################
#### II. Subsamples - Full LMEMs with outliers & comorbidity 

Right_Hippocampus_model_FSIQ <- lmer(Right_Hippocampus_log~ DX_GROUP*Total_Brain_Vol_log*FSIQ
                                     + (1|SITE_ID2), Abide_Age_12_20)

summary(Right_Hippocampus_model_FSIQ)$coefficients 
p.adjust(summary(Right_Hippocampus_model_FSIQ)$coefficients[,5], method = 'fdr') 

# ASD 
Abide_Age_12_20_ASD <- Abide_Age_12_20 %>% filter (DX_GROUP == "ASD")
Abide_Age_12_20_ASD$Right_Hippocampus_log_not_scaled <- log10(Abide_Age_12_20_ASD$Right_Hippocampus)
Abide_Age_12_20_ASD$Total_Brain_Vol_log_not_scaled <- log10(Abide_Age_12_20_ASD$Total_Brain_Vol)

Right_Hippocampus_model_no_outliers_ASD <- lmer(Right_Hippocampus_log_not_scaled~ Total_Brain_Vol_log_not_scaled*FSIQ
                                                + (1|SITE_ID2), Abide_Age_12_20_ASD)

summary(Right_Hippocampus_model_no_outliers_ASD)$coefficients #  0.6694551 0.09263419 133.4011  7.2268683 3.431593e-11
p.adjust(summary(Right_Hippocampus_model_no_outliers_ASD)$coefficients[,5], method = 'fdr') #    1.372637e-10     

# Controls 
Abide_Age_12_20_C <- Abide_Age_12_20 %>% filter (DX_GROUP == "Control")
Abide_Age_12_20_C$Right_Hippocampus_log_not_scaled <- log10(Abide_Age_12_20_C$Right_Hippocampus)
Abide_Age_12_20_C$Total_Brain_Vol_log_not_scaled <- log10(Abide_Age_12_20_C$Total_Brain_Vol)

Right_Hippocampus_model_no_outliers_C <- lmer(Right_Hippocampus_log_not_scaled~ Total_Brain_Vol_log_not_scaled*FSIQ
                                              + (1|SITE_ID2), Abide_Age_12_20_C)

summary(Right_Hippocampus_model_no_outliers_C)$coefficients # 1.10819595  0.1137065 106.2694  9.7461082 2.041583e-16
p.adjust(summary(Right_Hippocampus_model_no_outliers_C)$coefficients[,5], method = 'fdr') #  8.166332e-16      

#### III. Subsamples - Full LMEMs without outliers & comorbidity 

# 1. Make Model without Univariate Outliers for Cook's distance
Right_Hippocampus_model_no_univariate_outliers <- lmer(Right_Hippocampus_log~ Total_Brain_Vol_log*Group*FSIQ
                                                       + (1|SITE_ID2), Right_Hippocampus_NEW_DF)

summary(Right_Hippocampus_model_no_univariate_outliers)$coefficients
p.adjust(summary(Right_Hippocampus_model_no_univariate_outliers)$coefficients[,5], method = 'fdr')
table(Right_Hippocampus_NEW_DF$Group)

# 2. Remove multivariate outliers 
cooksd <- cooks.distance(Right_Hippocampus_model_no_univariate_outliers)
influential <- as.numeric(names(cooksd)[(cooksd > 5*mean(cooksd, na.rm=T))])  

Right_Hippocampus_no_outliers <- Right_Hippocampus_NEW_DF[-c(1, 14,35,123, 191, 197, 213 ,248), ] 
table(Right_Hippocampus_no_outliers$Group) # comorbidity: 133     119 

# 3. Run model without univariate and mutivariate outliers and comorbidity 
Right_Hippocampus_model_no_outliers <- lmer(Right_Hippocampus_log~ Total_Brain_Vol_log*Group*FSIQ+Medication
                                            + (1|SITE_ID2), Right_Hippocampus_no_outliers)

summary(Right_Hippocampus_model_no_outliers)$coefficients
p.adjust(summary(Right_Hippocampus_model_no_outliers)$coefficients[,5], method = 'fdr') 

# ASD 
Right_Hippocampus_no_outliers_ASD <- Right_Hippocampus_no_outliers %>% filter (Group == "ASD")
Right_Hippocampus_no_outliers_ASD$Right_Hippocampus_log_not_scaled <- log10(Right_Hippocampus_no_outliers_ASD$Right_Hippocampus)
Right_Hippocampus_no_outliers_ASD$Total_Brain_Vol_log_not_scaled <- log10(Right_Hippocampus_no_outliers_ASD$Total_Brain_Vol)

Right_Hippocampus_model_no_outliers_ASD_FSIQ <- lmer(Right_Hippocampus_log_not_scaled~ Total_Brain_Vol_log_not_scaled*FSIQ+ Medication
                                                     + (1|SITE_ID2), Right_Hippocampus_no_outliers_ASD)

summary(Right_Hippocampus_model_no_outliers_ASD_FSIQ)$coefficients # 
p.adjust(summary(Right_Hippocampus_model_no_outliers_ASD_FSIQ)$coefficients[,5], method = 'fdr') # 

# Controls 
Right_Hippocampus_no_outliers_C <- Right_Hippocampus_no_outliers %>% filter (Group == "Control")
Right_Hippocampus_no_outliers_C$Right_Hippocampus_log_not_scaled <- log10(Right_Hippocampus_no_outliers_C$Right_Hippocampus)
Right_Hippocampus_no_outliers_C$Total_Brain_Vol_log_not_scaled <- log10(Right_Hippocampus_no_outliers_C$Total_Brain_Vol)

Right_Hippocampus_model_no_outliers_C_FSIQ <- lmer(Right_Hippocampus_log_not_scaled~ Total_Brain_Vol_log_not_scaled*FSIQ+Medication
                                                   + (1|SITE_ID2), Right_Hippocampus_no_outliers_C)

summary(Right_Hippocampus_model_no_outliers_C_FSIQ)$coefficients # 
p.adjust(summary(Right_Hippocampus_model_no_outliers_C_FSIQ)$coefficients[,5], method = 'fdr') #  

Right_Hippocampus_model_power_sim_FSIQ_Group <- powerSim(Right_Hippocampus_model_no_outliers,
                                                   test = fixed("GroupASD", "sa"))
Right_Hippocampus_model_power_sim_FSIQ <- powerSim(Right_Hippocampus_model_no_outliers,
                                                   test = fixed("Total_Brain_Vol_log:GroupASD", "sa"))
Right_Hippocampus_model_power_sim_FSIQ

#### IV - Comparing Adjustment Techniques 
Right_Hippocampus_no_outliers$Total_Brain_Vol <- scale(Right_Hippocampus_no_outliers$Total_Brain_Vol)
Right_Hippocampus_no_outliers$Right_Hippocampus <- scale(Right_Hippocampus_no_outliers$Right_Hippocampus)

No_Adjustment <- lmer(Right_Hippocampus ~ Group * FSIQ + Medication + (1|SITE_ID2), Right_Hippocampus_no_outliers)
summary(No_Adjustment)$coefficients
p.adjust(summary(No_Adjustment)$coefficients[,5], method = 'fdr') 

Linear_Adjustment <- lmer(Right_Hippocampus ~ Total_Brain_Vol + Group * FSIQ + Medication + (1|SITE_ID2), Right_Hippocampus_no_outliers)
summary(Linear_Adjustment)$coefficients
p.adjust(summary(Linear_Adjustment)$coefficients[,5], method = 'fdr') 

Linear_Interactive_Adjustment <- lmer(Right_Hippocampus ~ Total_Brain_Vol * Group * FSIQ+ Medication  + (1|SITE_ID2), Right_Hippocampus_no_outliers)
summary(Linear_Interactive_Adjustment)$coefficients
p.adjust(summary(Linear_Interactive_Adjustment)$coefficients[,5], method = 'fdr') 

Allometric_Interactive_Adjustment <- lmer(Right_Hippocampus_log ~ Total_Brain_Vol_log * Group * FSIQ + Medication + (1|SITE_ID2), Right_Hippocampus_no_outliers)
summary(Allometric_Interactive_Adjustment)$coefficients
p.adjust(summary(Allometric_Interactive_Adjustment)$coefficients[,5], method = 'fdr') 

table(Right_Hippocampus_no_outliers$Group) #     133     121

#### IV - Figures with and without outliers 
My_Theme = theme(
  axis.title.x = element_text(size = 26),
  axis.text.y = element_text(size = 26),
  axis.text.x = element_text(size = 26),
  axis.title.y = element_text(size = 26),
  legend.title = element_text(size = 26),
  legend.text = element_text(size = 26),
  strip.text.x = element_text(size = 26))

Figure_Abide_Age_12_20 <- Abide_Age_12_20
names(Figure_Abide_Age_12_20)[names(Figure_Abide_Age_12_20) == "DX_GROUP"] <- "Group"

# With outliers and comorbidity 
Right_Hippocampus_figure <- ggplot(Figure_Abide_Age_12_20,
                                   aes(y=Right_Hippocampus_log, x= Total_Brain_Vol_log, shape = Group,color=Group)) +
  geom_point(size = 2)+ 
  geom_smooth(method=lm,fullrange=TRUE, aes(fill=Group)) +
  scale_shape_manual(values = c(20, 20)) +
  scale_color_manual(values=c('orangered3','navyblue')) + 
  xlab("log10(Total Brain Volume)") + 
  ylab("log10(Right Hippocampus)") + 
  theme_classic() + My_Theme

Right_Hippocampus_figure 


# without comorbidity and outliers 
Right_Hippocampus_figure_no_outlier<- ggplot(Right_Hippocampus_NEW_DF,
                                             aes(y=Right_Hippocampus_log, x= Total_Brain_Vol_log, shape = Group,color=Group)) +
  geom_point(size = 2)+ 
  geom_smooth(method=lm,fullrange=TRUE, aes(fill=Group)) +
  scale_shape_manual(values = c(20, 20)) +
  scale_color_manual(values=c('orangered3','navyblue')) + 
  xlab("log10(Total Brain Volume)") + 
  ylab("log10(Right Hippocampus)") + 
  theme_classic() + My_Theme

Right_Hippocampus_figure_no_outlier 

#### V - Post Hoc Behavioral Analyses
# Ask how to deal with missing values 
Right_Hippocampus_no_outliers_ASD = Right_Hippocampus_no_outliers %>%
  filter(Group == "ASD") 

Right_Hippocampus_no_outliers_ASD$ADOS_TOTAL <- scale(Right_Hippocampus_no_outliers_ASD$ADOS_TOTAL)
Right_Hippocampus_no_outliers_ASD$SRS_RAW_TOTAL <- scale(Right_Hippocampus_no_outliers_ASD$SRS_RAW_TOTAL)
Right_Hippocampus_no_outliers_ASD$ADOS_GOTHAM_TOTAL <- scale(Right_Hippocampus_no_outliers_ASD$ADOS_GOTHAM_TOTAL)

#### ADOS TOTAL 
Right_Hippocampus_no_outliers_ASD_ADOS_TOTAL= Right_Hippocampus_no_outliers_ASD %>%
  filter(ADOS_TOTAL != "NA") # N = 39 
table(Right_Hippocampus_no_outliers_ASD_ADOS_TOTAL$Group)

#### SRS TOTAL 
Right_Hippocampus_no_outliers_ASD_SRS_RAW_TOTAl = Right_Hippocampus_no_outliers_ASD %>%
  filter(SRS_RAW_TOTAL!= "NA") # N = 0
table(Right_Hippocampus_no_outliers_ASD_SRS_RAW_TOTAl$Group)

#### GOTHAM TOTAL 
Right_Hippocampus_no_outliers_ASD_ADOS_GOTHAM_TOTAL= Right_Hippocampus_no_outliers_ASD %>%
  filter(ADOS_GOTHAM_TOTAL != "NA") # 48 
table(Right_Hippocampus_no_outliers_ASD_ADOS_GOTHAM_TOTAL$Group)

### DSM_IV_TR
Right_Hippocampus_no_outliers$DSM_IV_TR <- as.factor(Right_Hippocampus_no_outliers$DSM_IV_TR)
Right_Hippocampus_no_outliers_DSM= Right_Hippocampus_no_outliers %>%
  filter(DSM_IV_TR == "Autism" | DSM_IV_TR == "Asperger" | DSM_IV_TR == "Control" ) 
Right_Hippocampus_no_outliers_DSM$DSM_IV_TR <- factor(Right_Hippocampus_no_outliers_DSM$DSM_IV_TR, levels = c("Control","Asperger", "Autism")) # remove extra factor
table(Right_Hippocampus_no_outliers_DSM$DSM_IV_TR)

Right_Hippocampus_no_outliers_ASD_ADOS_TOTAL_MODEL <- lmer(Right_Hippocampus_log~ Total_Brain_Vol_log*ADOS_TOTAL+Medication
                                                           + (1|SITE_ID2), Right_Hippocampus_no_outliers_DSM)

summary(Right_Hippocampus_no_outliers_ASD_ADOS_TOTAL_MODEL)$coefficients 
p.adjust(summary(Right_Hippocampus_no_outliers_ASD_ADOS_TOTAL_MODEL)$coefficients[,5], method = 'fdr') # 3.670649e-01 

