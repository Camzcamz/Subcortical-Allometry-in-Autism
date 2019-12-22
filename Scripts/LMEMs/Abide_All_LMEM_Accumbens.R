library(lmerTest)
library(dplyr)
library(simr)
library(Stack)
library(ggplot2)
library(extrafont)
font_import()
loadfonts(device="win")       
fonts()                       

Abide_FSIQ = Abide_LMEM_log_Clean_scaled
Abide_FSIQ$FSIQ <- scale(Abide_FSIQ$FIQ2)
Abide_FSIQ$AGE <- scale(Abide_FSIQ$AGE_AT_SCAN)
Abide_FSIQ$AGE2 <- scale(Abide_FSIQ$Age2)
Abide_FSIQ$SITE_ID2 <- as.factor(Abide_FSIQ$SITE_ID2)

################################ Right Hippocampus ################################

################ RESEARCH Q1

#### I. Corresponding LMEMs (// MGCFA) with outliers & comorbidity

Abide_Over_FSIQ<- Abide_FSIQ %>%
  filter(FIQ2 > 107.8)%>%
  filter(SEX == "Male")

Abide_Over_FSIQ$DX_GROUP <- factor(Abide_Over_FSIQ$DX_GROUP, levels = c("Control", "ASD"))
table(Abide_Over_FSIQ$DX_GROUP) # 174 C # 100 ASD

Left_Accumbens_area_model <- lmer(Left_Accumbens_Area_log~ Total_Brain_Vol_log*DX_GROUP
                                  + (1|SITE_ID2), Abide_Over_FSIQ)

summary(Left_Accumbens_area_model)$coefficients 
p.adjust(summary(Left_Accumbens_area_model)$coefficients[,5], method = 'fdr') 

Left_Accumbens_area_MGCFA_model_power_sim <- powerSim(Left_Accumbens_area_model,
                                                      test = fixed("Total_Brain_Vol_log:DX_GROUPASD", "sa"))
# ASD 
Abide_Over_FSIQ_ASD <- Abide_Over_FSIQ %>% filter (DX_GROUP == "ASD")
Abide_Over_FSIQ_ASD$Left_Accumbens_Area_log_not_scaled <- log10(Abide_Over_FSIQ_ASD$Left_Accumbens_Area)
Abide_Over_FSIQ_ASD$Total_Brain_Vol_log_not_scaled <- log10(Abide_Over_FSIQ_ASD$Total_Brain_Vol)

Left_Accumbens_Area_model_no_outliers_ASD <- lmer(Left_Accumbens_Area_log_not_scaled~ Total_Brain_Vol_log_not_scaled
                                                  + (1|SITE_ID2), Abide_Over_FSIQ_ASD)

summary(Left_Accumbens_Area_model_no_outliers_ASD)$coefficients 
p.adjust(summary(Left_Accumbens_Area_model_no_outliers_ASD)$coefficients[,5], method = 'fdr') 

# Controls 
Abide_Over_FSIQ_C <- Abide_Over_FSIQ %>% filter (DX_GROUP == "Control")
Abide_Over_FSIQ_C$Left_Accumbens_Area_log_not_scaled <- log10(Abide_Over_FSIQ_C$Left_Accumbens_Area)
Abide_Over_FSIQ_C$Total_Brain_Vol_log_not_scaled <- log10(Abide_Over_FSIQ_C$Total_Brain_Vol)

Left_Accumbens_Area_model_no_outliers_C <- lmer(Left_Accumbens_Area_log_not_scaled~ Total_Brain_Vol_log_not_scaled
                                                + (1|SITE_ID2), Abide_Over_FSIQ_C)

summary(Left_Accumbens_Area_model_no_outliers_C)$coefficients 
p.adjust(summary(Left_Accumbens_Area_model_no_outliers_C)$coefficients[,5], method = 'fdr') #

# 1. Remove comorbidities in C and ASD 
Abide_Over_FSIQ_No_Comorbidity<-dplyr::filter(Abide_Over_FSIQ,is.na(COMORBIDITY)) 
table(Abide_Over_FSIQ_No_Comorbidity$DX_GROUP) # C  172      88 

#### Medication 
Abide_Over_FSIQ_No_Comorbidity$MEDICATION_NAME 
Abide_Over_FSIQ_No_Comorbidity$Medication<-if_else(is.na(Abide_Over_FSIQ_No_Comorbidity$MEDICATION_NAME), 0, 1)
Abide_Over_FSIQ_No_Comorbidity$Medication<- as.factor(Abide_Over_FSIQ_No_Comorbidity$Medication)
Left_Accumbens_Area_no_outliers_no_meds <- subset(Abide_Over_FSIQ_No_Comorbidity, Abide_Over_FSIQ_No_Comorbidity$Medication == 0)
table(Left_Accumbens_Area_no_outliers_no_meds$DX_GROUP) # c 164      63 

# 2. Remove Univariate Outliers 
# Create New DF for each Group 
Abide_Over_FSIQ_ASD<- Abide_Over_FSIQ_No_Comorbidity %>% 
  filter(DX_GROUP =="ASD")

Abide_Over_FSIQ_C <- Abide_Over_FSIQ_No_Comorbidity %>% 
  filter(DX_GROUP =="Control")

# Examine Boxplot Outliers & remove for Brain Volume DV and IV 
Left_Accumbens_Area_Boxplot <- boxplot(Abide_Over_FSIQ_ASD$Left_Accumbens_Area)
Left_Accumbens_Area_outliers <- Left_Accumbens_Area_Boxplot$out # You can get the actual values of the outliers with this
Left_Accumbens_Area_Outlier_DF_ASD <- Abide_Over_FSIQ_ASD[-which(Abide_Over_FSIQ_ASD$Left_Accumbens_Area %in% Left_Accumbens_Area_outliers),]
Left_Accumbens_Area_Boxplot <- boxplot(Left_Accumbens_Area_Outlier_DF_ASD$Left_Accumbens_Area)

Left_Accumbens_Area_Boxplot <- boxplot(Left_Accumbens_Area_Outlier_DF_ASD$Total_Brain_Vol)
Left_Accumbens_Area_outliers <- Left_Accumbens_Area_Boxplot$out # You can get the actual values of the outliers with this
Left_Accumbens_Area_Outlier_DF_ASD <- Left_Accumbens_Area_Outlier_DF_ASD[-which(Left_Accumbens_Area_Outlier_DF_ASD$Total_Brain_Vol %in% Left_Accumbens_Area_outliers),]
Left_Accumbens_Area_Boxplot <- boxplot(Left_Accumbens_Area_Outlier_DF_ASD$Total_Brain_Vol)

Left_Accumbens_Area_Boxplot <- boxplot(Abide_Over_FSIQ_C$Left_Accumbens_Area)
Left_Accumbens_Area_outliers <- Left_Accumbens_Area_Boxplot$out # You can get the actual values of the outliers with this
Left_Accumbens_Area_Outlier_DF_C <- Abide_Over_FSIQ_C[-which(Abide_Over_FSIQ_C$Left_Accumbens_Area %in% Left_Accumbens_Area_outliers),]
Left_Accumbens_Area_Boxplot <- boxplot(Left_Accumbens_Area_Outlier_DF_C$Left_Accumbens_Area)

TBV_Boxplot <- boxplot(Left_Accumbens_Area_Outlier_DF_C$Total_Brain_Vol)
TBV_Boxplot_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Left_Accumbens_Area_Outlier_DF_C <- Left_Accumbens_Area_Outlier_DF_C[-which(Left_Accumbens_Area_Outlier_DF_C$Total_Brain_Vol %in% TBV_Boxplot_outliers),]
Left_Accumbens_Area_Boxplot <- boxplot(Left_Accumbens_Area_Outlier_DF_C$Total_Brain_Vol)

# Create New DF with both groups and without univariate outliers 
Left_Accumbens_Area_NEW_DF <- Stack(Left_Accumbens_Area_Outlier_DF_C,Left_Accumbens_Area_Outlier_DF_ASD)
names(Left_Accumbens_Area_NEW_DF)[names(Left_Accumbens_Area_NEW_DF) == "DX_GROUP"] <- "Group"
Left_Accumbens_Area_NEW_DF$Group <- factor(Left_Accumbens_Area_NEW_DF$Group, levels = c("Control", "ASD"))

table(Left_Accumbens_Area_NEW_DF$Group) #  167      85  


#3. Test correpsonding LMEMs without univariate outliers and comorbidity 

Left_Accumbens_Area_model_MGCFA_no_Uni_Out <- lmer(Left_Accumbens_Area_log~ Total_Brain_Vol_log*Group + Medication
                                                   + (1|SITE_ID2), Left_Accumbens_Area_NEW_DF)

summary(Left_Accumbens_Area_model_MGCFA_no_Uni_Out)$coefficients 
p.adjust(summary(Left_Accumbens_Area_model_MGCFA_no_Uni_Out)$coefficients[,5], method = 'fdr') 

Left_Accumbens_Area_model_power_sim <- powerSim(Left_Accumbens_Area_model_MGCFA_no_Uni_Out,
                                                test = fixed("Total_Brain_Vol_log:GroupASD", "sa"))

# ASD 
Left_Accumbens_Area_no_outliers_ASD <- Left_Accumbens_Area_NEW_DF %>% filter (Group == "ASD")
Left_Accumbens_Area_no_outliers_ASD$Left_Accumbens_Area_log_not_scaled <- log10(Left_Accumbens_Area_no_outliers_ASD$Left_Accumbens_Area)
Left_Accumbens_Area_no_outliers_ASD$Total_Brain_Vol_log_not_scaled <- log10(Left_Accumbens_Area_no_outliers_ASD$Total_Brain_Vol)

Left_Accumbens_Area_model_no_outliers_ASD <- lmer(Left_Accumbens_Area_log_not_scaled~ Total_Brain_Vol_log_not_scaled + Medication
                                                  + (1|SITE_ID2), Left_Accumbens_Area_no_outliers_ASD)

summary(Left_Accumbens_Area_model_no_outliers_ASD)$coefficients
p.adjust(summary(Left_Accumbens_Area_model_no_outliers_ASD)$coefficients[,5], method = 'fdr') 

# Controls 
Left_Accumbens_Area_no_outliers_C <- Left_Accumbens_Area_NEW_DF %>% filter (Group == "Control")
Left_Accumbens_Area_no_outliers_C$Left_Accumbens_Area_log_not_scaled <- log10(Left_Accumbens_Area_no_outliers_C$Left_Accumbens_Area)
Left_Accumbens_Area_no_outliers_C$Total_Brain_Vol_log_not_scaled <- log10(Left_Accumbens_Area_no_outliers_C$Total_Brain_Vol)

Left_Accumbens_Area_model_no_outliers_C <- lmer(Left_Accumbens_Area_log_not_scaled~ Total_Brain_Vol_log_not_scaled+ Medication
                                                + (1|SITE_ID2), Left_Accumbens_Area_no_outliers_C)

summary(Left_Accumbens_Area_model_no_outliers_C)$coefficients 
p.adjust(summary(Left_Accumbens_Area_model_no_outliers_C)$coefficients[,5], method = 'fdr') 

#################################################### RESEARCH Q2 ####################################################
#### I. Subsamples - Full LMEMs with outliers & comorbidity 

Left_Accumbens_Area_model_AGE<- lmer(Left_Accumbens_Area_log~ DX_GROUP*Total_Brain_Vol_log*AGE
                                       + (1|SITE_ID2), Abide_Over_FSIQ)

summary(Left_Accumbens_Area_model_AGE)$coefficients 
p.adjust(summary(Left_Accumbens_Area_model_AGE)$coefficients[,5], method = 'fdr') 

Left_Accumbens_Area_model_AGE2<- lmer(Left_Accumbens_Area_log~ DX_GROUP*Total_Brain_Vol_log*AGE2
                                      + (1|SITE_ID2), Abide_Over_FSIQ)

summary(Left_Accumbens_Area_model_AGE2)$coefficients 
p.adjust(summary(Left_Accumbens_Area_model_AGE2)$coefficients[,5], method = 'fdr') 

Abide_Over_FSIQ_ASD <- Abide_Over_FSIQ %>% filter (DX_GROUP == "ASD")
Abide_Over_FSIQ_ASD$Left_Accumbens_Area_log_not_scaled <- log10(Abide_Over_FSIQ_ASD$Left_Accumbens_Area)
Abide_Over_FSIQ_ASD$Total_Brain_Vol_log_not_scaled <- log10(Abide_Over_FSIQ_ASD$Total_Brain_Vol)

Left_Accumbens_Area_model_no_outliers_ASD <- lmer(Left_Accumbens_Area_log_not_scaled~ Total_Brain_Vol_log_not_scaled*AGE2
                                                  + (1|SITE_ID2), Abide_Over_FSIQ_ASD)

summary(Left_Accumbens_Area_model_no_outliers_ASD)$coefficients 
p.adjust(summary(Left_Accumbens_Area_model_no_outliers_ASD)$coefficients[,5], method = 'fdr')       

# Controls 
Abide_Over_FSIQ_C <- Abide_Over_FSIQ %>% filter (DX_GROUP == "Control")
Abide_Over_FSIQ_C$Left_Accumbens_Area_log_not_scaled <- log10(Abide_Over_FSIQ_C$Left_Accumbens_Area)
Abide_Over_FSIQ_C$Total_Brain_Vol_log_not_scaled <- log10(Abide_Over_FSIQ_C$Total_Brain_Vol)

Left_Accumbens_Area_model_no_outliers_C <- lmer(Left_Accumbens_Area_log_not_scaled~ Total_Brain_Vol_log_not_scaled*AGE2
                                                + (1|SITE_ID2), Abide_Over_FSIQ_C)

summary(Left_Accumbens_Area_model_no_outliers_C)$coefficients
p.adjust(summary(Left_Accumbens_Area_model_no_outliers_C)$coefficients[,5], method = 'fdr')    


#### II. Subsamples - Full LMEMs without outliers & comorbidity 

# 1. Make Model without Univariate Outliers for Cook's distance
Left_Accumbens_Area_model_no_univariate_outliers <- lmer(Left_Accumbens_Area_log~ Total_Brain_Vol_log*Group*AGE
                                                         + (1|SITE_ID2), Left_Accumbens_Area_NEW_DF)

summary(Left_Accumbens_Area_model_no_univariate_outliers)$coefficients
p.adjust(summary(Left_Accumbens_Area_model_no_univariate_outliers)$coefficients[,5], method = 'fdr')
table(Left_Accumbens_Area_NEW_DF$Group)
# 2. Remove multivariate outliers 
cooksd <- cooks.distance(Left_Accumbens_Area_model_no_univariate_outliers)
influential <- as.numeric(names(cooksd)[(cooksd > 5*mean(cooksd, na.rm=T))])  

Left_Accumbens_Area_no_outliers <- Left_Accumbens_Area_NEW_DF[-c(12,42,70,112,189,143,205,206,243,250,251), ] 
table(Left_Accumbens_Area_no_outliers$Group) # comorbidity:     162      79 

# 3. Run model without univariate and mutivariate outliers and comorbidity 
Left_Accumbens_Area_model_no_outliers_AGE <- lmer(Left_Accumbens_Area_log~ Total_Brain_Vol_log*Group*AGE+ Medication
                                              + (1|SITE_ID2), Left_Accumbens_Area_no_outliers)

summary(Left_Accumbens_Area_model_no_outliers_AGE)$coefficients
p.adjust(summary(Left_Accumbens_Area_model_no_outliers_AGE)$coefficients[,5], method = 'fdr') 
#    
Left_Accumbens_Area_model_no_outliers_AGE2 <- lmer(Left_Accumbens_Area_log~ Total_Brain_Vol_log*Group*AGE2 + Medication
                                                   + (1|SITE_ID2), Left_Accumbens_Area_no_outliers)

summary(Left_Accumbens_Area_model_no_outliers_AGE2)$coefficients
p.adjust(summary(Left_Accumbens_Area_model_no_outliers_AGE2)$coefficients[,5], method = 'fdr') 

#### Medication 

# ASD 
Left_Accumbens_Area_no_outliers_ASD <- Left_Accumbens_Area_no_outliers %>% filter (Group == "ASD")
Left_Accumbens_Area_no_outliers_ASD$Left_Accumbens_Area_log_not_scaled <- log10(Left_Accumbens_Area_no_outliers_ASD$Left_Accumbens_Area)
Left_Accumbens_Area_no_outliers_ASD$Total_Brain_Vol_log_not_scaled <- log10(Left_Accumbens_Area_no_outliers_ASD$Total_Brain_Vol)

Left_Accumbens_Area_model_no_outliers_ASD_FSIQ <- lmer(Left_Accumbens_Area_log_not_scaled~ Total_Brain_Vol_log_not_scaled*AGE2 + Medication # or AGE2
                                                       + (1|SITE_ID2), Left_Accumbens_Area_no_outliers_ASD)

summary(Left_Accumbens_Area_model_no_outliers_ASD_FSIQ)$coefficients
p.adjust(summary(Left_Accumbens_Area_model_no_outliers_ASD_FSIQ)$coefficients[,5], method = 'fdr') 

# Controls 
Left_Accumbens_Area_no_outliers_C <- Left_Accumbens_Area_no_outliers %>% filter (Group == "Control")
Left_Accumbens_Area_no_outliers_C$Left_Accumbens_Area_log_not_scaled <- log10(Left_Accumbens_Area_no_outliers_C$Left_Accumbens_Area)
Left_Accumbens_Area_no_outliers_C$Total_Brain_Vol_log_not_scaled <- log10(Left_Accumbens_Area_no_outliers_C$Total_Brain_Vol)

Left_Accumbens_Area_model_no_outliers_C_FSIQ <- lmer(Left_Accumbens_Area_log_not_scaled~ Total_Brain_Vol_log_not_scaled*AGE2+ Medication
                                                     + (1|SITE_ID2), Left_Accumbens_Area_no_outliers_C)

summary(Left_Accumbens_Area_model_no_outliers_C_FSIQ)$coefficients 
p.adjust(summary(Left_Accumbens_Area_model_no_outliers_C_FSIQ)$coefficients[,5], method = 'fdr')  

Left_Accumbens_Area_model_power_sim_FSIQ <- powerSim(Left_Accumbens_Area_model_no_outliers_AGE,
                                                     test = fixed("Total_Brain_Vol_log:GroupASD", "sa"))
Left_Accumbens_Area_model_power_sim_FSIQ


#### IV - Comparing Adjustment Techniques 
Left_Accumbens_Area_no_outliers$Total_Brain_Vol <- scale(Left_Accumbens_Area_no_outliers$Total_Brain_Vol)
Left_Accumbens_Area_no_outliers$Left_Accumbens_Area <- scale(Left_Accumbens_Area_no_outliers$Left_Accumbens_Area)

No_Adjustment <- lmer(Left_Accumbens_Area ~ Group * AGE + Medication + (1|SITE_ID2), Left_Accumbens_Area_no_outliers)
summary(No_Adjustment)$coefficients
p.adjust(summary(No_Adjustment)$coefficients[,5], method = 'fdr') 

#without optimizer : Model failed to converge with max|grad| = 0.0022112 (tol = 0.002, component 1)
Linear_Adjustment <- lmer(Left_Accumbens_Area ~ Total_Brain_Vol + Group *AGE  + Medication+ (1|SITE_ID2), 
                          Left_Accumbens_Area_no_outliers)
summary(Linear_Adjustment)$coefficients
p.adjust(summary(Linear_Adjustment)$coefficients[,5], method = 'fdr') 

#with optimizer Nelder_Mead : model converges and the same values are obtained - so previous warning is not considered as a problem (https://rdrr.io/cran/lme4/man/troubleshooting.html)
Linear_Adjustment <- lmer(Left_Accumbens_Area ~ Total_Brain_Vol + Group *AGE  + Medication+ (1|SITE_ID2), 
                          Left_Accumbens_Area_no_outliers, control = lmerControl(optimizer ="Nelder_Mead"))
summary(Linear_Adjustment)$coefficients
p.adjust(summary(Linear_Adjustment)$coefficients[,5], method = 'fdr') 

Linear_Interactive_Adjustment <- lmer(Left_Accumbens_Area ~ Total_Brain_Vol * Group * AGE  + Medication+ (1|SITE_ID2), Left_Accumbens_Area_no_outliers)
summary(Linear_Interactive_Adjustment)$coefficients
p.adjust(summary(Linear_Interactive_Adjustment)$coefficients[,5], method = 'fdr') 


Allometric_Interactive_Adjustment <- lmer(Left_Accumbens_Area_log ~ Total_Brain_Vol_log * Group * AGE  + Medication+ (1|SITE_ID2), Left_Accumbens_Area_no_outliers)
summary(Allometric_Interactive_Adjustment)$coefficients
p.adjust(summary(Allometric_Interactive_Adjustment)$coefficients[,5], method = 'fdr') 

#### V - Figures with and without outliers 
# colors : http://sape.inf.usi.ch/quick-reference/ggplot2/colour 
# shape points : http://sape.inf.usi.ch/quick-reference/ggplot2/shape

My_Theme = theme(
  axis.title.x = element_text(size = 26),
  axis.text.y = element_text(size = 26),
  axis.text.x = element_text(size = 26),
  axis.title.y = element_text(size = 26),
  legend.title = element_text(size = 26),
  legend.text = element_text(size = 26),
  strip.text.x = element_text(size = 26))
Figure_Abide_Over_FSIQ <- Abide_Over_FSIQ
names(Figure_Abide_Over_FSIQ)[names(Figure_Abide_Over_FSIQ) == "DX_GROUP"] <- "Group"

# With outliers and comorbidity 
Left_Accumbens_Area_figure <- ggplot(Figure_Abide_Over_FSIQ,
                                     aes(y=Left_Accumbens_Area_log, x= Total_Brain_Vol_log, shape = Group,color=Group)) +
  geom_point(size = 3)+ 
  geom_smooth(method=lm,fullrange=TRUE, aes(fill=Group)) + scale_shape_manual(values = c(18, 20)) + # 42
  scale_color_manual(values=c('orangered','navyblue')) + 
  xlab("log10(Total Brain Volume)") + 
  ylab("log10(Left Accumbens)") + 
  theme_classic() + My_Theme +
  theme(text=element_text(family="Times New Roman", face="bold", size=12))

Left_Accumbens_Area_figure

# without comorbidity and outliers 
Left_Accumbens_Area_figure_no_outlier<- ggplot(Left_Accumbens_Area_NEW_DF,
                                               aes(y=Left_Accumbens_Area_log, x= Total_Brain_Vol_log, shape = Group,color=Group)) +
  geom_point(size = 3)+ 
  geom_smooth(method=lm,fullrange=TRUE, aes(fill=Group)) + scale_shape_manual(values = c(18, 20)) + # 42
  scale_color_manual(values=c('orangered3','navyblue')) + 
  xlab("log10(Total Brain Volume)") + 
  ylab("log10(Left Accumbens)") + 
  theme_classic() + My_Theme + 
  theme(text=element_text(family="Times New Roman", face="bold", size=12))

Left_Accumbens_Area_figure_no_outlier 

#### V - Post Hoc Behavioral Analyses
# Ask how to deal with missing values 
Left_Accumbens_Area_no_outliers_ASD = Left_Accumbens_Area_no_outliers_no_meds %>%
  filter(Group == "ASD") 

Left_Accumbens_Area_no_outliers_ASD$ADOS_TOTAL <- scale(Left_Accumbens_Area_no_outliers_ASD$ADOS_TOTAL)
Left_Accumbens_Area_no_outliers_ASD$SRS_RAW_TOTAL <- scale(Left_Accumbens_Area_no_outliers_ASD$SRS_RAW_TOTAL)
Left_Accumbens_Area_no_outliers_ASD$ADOS_GOTHAM_TOTAL <- scale(Left_Accumbens_Area_no_outliers_ASD$ADOS_GOTHAM_TOTAL)

#### ADOS TOTAL 
Left_Accumbens_Area_no_outliers_ASD_ADOS_TOTAL= Left_Accumbens_Area_no_outliers_ASD %>%
  filter(ADOS_TOTAL != "NA") # N = 39 
table(Left_Accumbens_Area_no_outliers_ASD_ADOS_TOTAL$Group)

#### SRS TOTAL 
Left_Accumbens_Area_no_outliers_ASD_SRS_RAW_TOTAl = Left_Accumbens_Area_no_outliers_ASD %>%
  filter(SRS_RAW_TOTAL!= "NA") # N = 0
table(Left_Accumbens_Area_no_outliers_ASD_SRS_RAW_TOTAl$Group)

#### GOTHAM TOTAL 
Left_Accumbens_Area_no_outliers_ASD_ADOS_GOTHAM_TOTAL= Left_Accumbens_Area_no_outliers_ASD %>%
  filter(ADOS_GOTHAM_TOTAL != "NA") # 48 
table(Left_Accumbens_Area_no_outliers_ASD_ADOS_GOTHAM_TOTAL$Group)

### DSM_IV_TR
Left_Accumbens_Area_no_outliers$DSM_IV_TR <- as.factor(Left_Accumbens_Area_no_outliers$DSM_IV_TR)
Left_Accumbens_Area_no_outliers_DSM= Left_Accumbens_Area_no_outliers %>%
  filter(DSM_IV_TR == "Autism" | DSM_IV_TR == "Asperger" | DSM_IV_TR == "Control" ) 
Left_Accumbens_Area_no_outliers_DSM$DSM_IV_TR <- factor(Left_Accumbens_Area_no_outliers_DSM$DSM_IV_TR, levels = c( "Control", "Asperger","Autism")) # remove extra factor
table(Left_Accumbens_Area_no_outliers_DSM$DSM_IV_TR) # 148       50       18 

Left_Accumbens_Area_no_outliers_ASD_ADOS_TOTAL_MODEL <- lmer(Left_Accumbens_Area_log~ Total_Brain_Vol_log*ADOS_TOTAL+ Medication
                                                             + (1|SITE_ID2), Left_Accumbens_Area_no_outliers_DSM)

summary(Left_Accumbens_Area_no_outliers_ASD_ADOS_TOTAL_MODEL)$coefficients 
p.adjust(summary(Left_Accumbens_Area_no_outliers_ASD_ADOS_TOTAL_MODEL)$coefficients[,5], method = 'fdr') # 

#### Left_Hippocampus
Left_Hippocampus_model <- lmer(Left_Hippocampus_log~ Total_Brain_Vol_log*DX_GROUP
+ (1|SITE_ID2), Abide_Over_FSIQ)

summary(Left_Hippocampus_model)$coefficients # -0.28468496 0.09964841 261.37805 -2.8568941 4.622057e-03
p.adjust(summary(Left_Hippocampus_model)$coefficients[,5], method = 'fdr') #  9.244113e-03 

# Create New DF for each Group 
Abide_Over_FSIQ_No_Comorbidity<-dplyr::filter(Abide_Over_FSIQ,is.na(COMORBIDITY)) 
Abide_Over_FSIQ_No_Comorbidity$MEDICATION_NAME 
#Left_Accumbens_Area_no_outliers_meds <- Left_Accumbens_Area_no_outliers_meds[!is.na(Left_Accumbens_Area_no_outliers_meds$MEDICATION_NAME),]
Abide_Over_FSIQ_No_Comorbidity$Medication<-if_else(is.na(Abide_Over_FSIQ_No_Comorbidity$MEDICATION_NAME), 0, 1)
Abide_Over_FSIQ_No_Comorbidity$Medication<- as.factor(Abide_Over_FSIQ_No_Comorbidity$Medication)

Abide_Over_FSIQ_ASD<- Abide_Over_FSIQ_No_Comorbidity %>% 
  filter(DX_GROUP =="ASD")

Abide_Over_FSIQ_C <- Abide_Over_FSIQ_No_Comorbidity %>% 
  filter(DX_GROUP =="Control")

# Examine Boxplot Outliers & remove for Brain Volume DV and IV 
Left_Hippocampus_Outlier_DF_ASD <- Abide_Over_FSIQ_ASD

Left_Hippocampus_Boxplot <- boxplot(Left_Hippocampus_Outlier_DF_ASD$Total_Brain_Vol)
Left_Hippocampus_outliers <- Left_Hippocampus_Boxplot$out # You can get the actual values of the outliers with this
Left_Hippocampus_Outlier_DF_ASD <- Left_Hippocampus_Outlier_DF_ASD[-which(Left_Hippocampus_Outlier_DF_ASD$Total_Brain_Vol %in% Left_Hippocampus_outliers),]
Left_Hippocampus_Boxplot <- boxplot(Left_Hippocampus_Outlier_DF_ASD$Total_Brain_Vol)

Left_Hippocampus_Boxplot <- boxplot(Abide_Over_FSIQ_C$Left_Hippocampus)
Left_Hippocampus_outliers <- Left_Hippocampus_Boxplot$out # You can get the actual values of the outliers with this
Left_Hippocampus_Outlier_DF_C <- Abide_Over_FSIQ_C[-which(Abide_Over_FSIQ_C$Left_Hippocampus %in% Left_Hippocampus_outliers),]
Left_Hippocampus_Boxplot <- boxplot(Left_Hippocampus_Outlier_DF_C$Left_Hippocampus)

TBV_Boxplot <- boxplot(Left_Hippocampus_Outlier_DF_C$Total_Brain_Vol)
TBV_Boxplot_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Left_Hippocampus_Outlier_DF_C <- Left_Hippocampus_Outlier_DF_C[-which(Left_Hippocampus_Outlier_DF_C$Total_Brain_Vol %in% TBV_Boxplot_outliers),]
Left_Hippocampus_Boxplot <- boxplot(Left_Hippocampus_Outlier_DF_C$Total_Brain_Vol)

# Create New DF with both groups and without univariate outliers 
library(Stack)
Left_Hippocampus_NEW_DF <- Stack(Left_Hippocampus_Outlier_DF_C,Left_Hippocampus_Outlier_DF_ASD)
names(Left_Hippocampus_NEW_DF)[names(Left_Hippocampus_NEW_DF) == "DX_GROUP"] <- "Group"
Left_Hippocampus_NEW_DF$Group <- factor(Left_Hippocampus_NEW_DF$Group, levels = c("Control", "ASD"))

table(Left_Hippocampus_NEW_DF$Group) #  167      87 

#3. Test correpsonding LMEMs without univariate outliers and comorbidity 

Left_Hippocampus_model_MGCFA_no_Uni_Out <- lmer(Left_Hippocampus_log~ Total_Brain_Vol_log*Group+Medication
                                                + (1|SITE_ID2), Left_Hippocampus_NEW_DF)

summary(Left_Hippocampus_model_MGCFA_no_Uni_Out)$coefficients # -0.097061973 0.11097696 241.98951 -0.87461376 3.826513e-01
p.adjust(summary(Left_Hippocampus_model_MGCFA_no_Uni_Out)$coefficients[,5], method = 'fdr') # 7.653026e-01 

##### Left_Caudate 
Left_Caudate_model <- lmer(Left_Caudate_log~ Total_Brain_Vol_log*DX_GROUP
                           + (1|SITE_ID2), Abide_Over_FSIQ)

summary(Left_Caudate_model)$coefficients # -0.36198304 0.12099961 270 -2.991605 3.031645e-03
p.adjust(summary(Left_Caudate_model)$coefficients[,5], method = 'fdr') #   6.063289e-03 
# DX GROUP - 0.30759921 0.11768696 270  2.613707 9.459148e-03 & 1.261220e-02 

# Create New DF for each Group 
Abide_Over_FSIQ_ASD<- Abide_Over_FSIQ_No_Comorbidity %>% 
  filter(DX_GROUP =="ASD")

Abide_Over_FSIQ_C <- Abide_Over_FSIQ_No_Comorbidity %>% 
  filter(DX_GROUP =="Control")

mean(Abide_Over_FSIQ_ASD$`Left-Caudate`)
sd(Abide_Over_FSIQ_ASD$`Left-Caudate`)
mean(Abide_Over_FSIQ_C$`Left-Caudate`)
sd(Abide_Over_FSIQ_C$`Left-Caudate`)
# Examine Boxplot Outliers & remove for Brain Volume DV and IV 
Left_Caudate_Boxplot <- boxplot(Abide_Over_FSIQ_ASD$Left_Caudate)
Left_Caudate_Outlier_DF_ASD <- Abide_Over_FSIQ_ASD

Left_Caudate_Boxplot <- boxplot(Left_Caudate_Outlier_DF_ASD$Total_Brain_Vol)
Left_Caudate_outliers <- Left_Caudate_Boxplot$out # You can get the actual values of the outliers with this
Left_Caudate_Outlier_DF_ASD <- Left_Caudate_Outlier_DF_ASD[-which(Left_Caudate_Outlier_DF_ASD$Total_Brain_Vol %in% Left_Caudate_outliers),]
Left_Caudate_Boxplot <- boxplot(Left_Caudate_Outlier_DF_ASD$Total_Brain_Vol)

Left_Caudate_Boxplot <- boxplot(Abide_Over_FSIQ_C$Left_Caudate)
Left_Caudate_outliers <- Left_Caudate_Boxplot$out # You can get the actual values of the outliers with this
Left_Caudate_Outlier_DF_C <- Abide_Over_FSIQ_C[-which(Abide_Over_FSIQ_C$Left_Caudate %in% Left_Caudate_outliers),]
Left_Caudate_Boxplot <- boxplot(Left_Caudate_Outlier_DF_C$Left_Caudate)

TBV_Boxplot <- boxplot(Left_Caudate_Outlier_DF_C$Total_Brain_Vol)
TBV_Boxplot_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Left_Caudate_Outlier_DF_C <- Left_Caudate_Outlier_DF_C[-which(Left_Caudate_Outlier_DF_C$Total_Brain_Vol %in% TBV_Boxplot_outliers),]
Left_Caudate_Boxplot <- boxplot(Left_Caudate_Outlier_DF_C$Total_Brain_Vol)

# Create New DF with both groups and without univariate outliers 
Left_Caudate_NEW_DF <- Stack(Left_Caudate_Outlier_DF_C,Left_Caudate_Outlier_DF_ASD)
names(Left_Caudate_NEW_DF)[names(Left_Caudate_NEW_DF) == "DX_GROUP"] <- "Group"
Left_Caudate_NEW_DF$Group <- factor(Left_Caudate_NEW_DF$Group, levels = c("Control", "ASD"))

table(Left_Caudate_NEW_DF$Group) #  164      87 

#3. Test correpsonding LMEMs without univariate outliers and comorbidity 

Left_Caudate_model_MGCFA_no_Uni_Out <- lmer(Left_Caudate_log~ Total_Brain_Vol_log*Group+ Medication
                                            + (1|SITE_ID2), Left_Caudate_NEW_DF)

summary(Left_Caudate_model_MGCFA_no_Uni_Out)$coefficients 
p.adjust(summary(Left_Caudate_model_MGCFA_no_Uni_Out)$coefficients[,5], method = 'fdr') 
#  0.254091975 0.09910727 247  2.56380761 1.094590e-02 & 2.189179e-02   

Left_Caudate_figure_no_outlier<- ggplot(Left_Caudate_no_outliers,
                                        aes(y=Left_Caudate_log, x= Total_Brain_Vol_log, shape = Group,color=Group)) +
  geom_point(size = 6)+ 
  geom_smooth(method=lm,fullrange=TRUE)+ scale_shape_manual(values = c(21, 22)) +
  xlab("log10(Total Brain Volume)") + 
  ylab("log10(Right Hippocampus)") + 
  theme_classic() + My_Theme

Left_Caudate_figure_no_outlier 

#### Right Ventral DC 
Right_Ventral_DC_model <- lmer(Right_Ventral_DC_log~ Total_Brain_Vol_log*DX_GROUP
                               + (1|SITE_ID2), Abide_Over_FSIQ)

summary(Right_Ventral_DC_model)$coefficients 
p.adjust(summary(Right_Ventral_DC_model)$coefficients[,5], method = 'fdr') 

# Create New DF for each Group 
Abide_Over_FSIQ_ASD<- Abide_Over_FSIQ_No_Comorbidity %>% 
  filter(DX_GROUP =="ASD")

Abide_Over_FSIQ_C <- Abide_Over_FSIQ_No_Comorbidity %>% 
  filter(DX_GROUP =="Control")

# Examine Boxplot Outliers & remove for Brain Volume DV and IV 
Right_Ventral_DC_Boxplot <- boxplot(Abide_Over_FSIQ_ASD$Right_Ventral_DC)
Right_Ventral_DC_outliers <- Right_Ventral_DC_Boxplot$out # You can get the actual values of the outliers with this
Right_Ventral_DC_Outlier_DF_ASD <- Abide_Over_FSIQ_ASD[-which(Abide_Over_FSIQ_ASD$Right_Ventral_DC %in% Right_Ventral_DC_outliers),]
Right_Ventral_DC_Boxplot <- boxplot(Right_Ventral_DC_Outlier_DF_ASD$Right_Ventral_DC)

Right_Ventral_DC_Boxplot <- boxplot(Right_Ventral_DC_Outlier_DF_ASD$Total_Brain_Vol)
Right_Ventral_DC_outliers <- Right_Ventral_DC_Boxplot$out # You can get the actual values of the outliers with this
Right_Ventral_DC_Outlier_DF_ASD <- Right_Ventral_DC_Outlier_DF_ASD[-which(Right_Ventral_DC_Outlier_DF_ASD$Total_Brain_Vol %in% Right_Ventral_DC_outliers),]
Right_Ventral_DC_Boxplot <- boxplot(Right_Ventral_DC_Outlier_DF_ASD$Total_Brain_Vol)

Right_Ventral_DC_Boxplot <- boxplot(Abide_Over_FSIQ_C$Right_Ventral_DC)
Right_Ventral_DC_outliers <- Right_Ventral_DC_Boxplot$out # You can get the actual values of the outliers with this
Right_Ventral_DC_Outlier_DF_C <- Abide_Over_FSIQ_C[-which(Abide_Over_FSIQ_C$Right_Ventral_DC %in% Right_Ventral_DC_outliers),]
Right_Ventral_DC_Boxplot <- boxplot(Right_Ventral_DC_Outlier_DF_C$Right_Ventral_DC)

TBV_Boxplot <- boxplot(Right_Ventral_DC_Outlier_DF_C$Total_Brain_Vol)
TBV_Boxplot_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Right_Ventral_DC_Outlier_DF_C <- Right_Ventral_DC_Outlier_DF_C[-which(Right_Ventral_DC_Outlier_DF_C$Total_Brain_Vol %in% TBV_Boxplot_outliers),]
Right_Ventral_DC_Boxplot <- boxplot(Right_Ventral_DC_Outlier_DF_C$Total_Brain_Vol)

# Create New DF with both groups and without univariate outliers 
Right_Ventral_DC_NEW_DF <- Stack(Right_Ventral_DC_Outlier_DF_C,Right_Ventral_DC_Outlier_DF_ASD)
names(Right_Ventral_DC_NEW_DF)[names(Right_Ventral_DC_NEW_DF) == "DX_GROUP"] <- "Group"
Right_Ventral_DC_NEW_DF$Group <- factor(Right_Ventral_DC_NEW_DF$Group, levels = c("Control", "ASD"))

table(Right_Ventral_DC_NEW_DF$Group) #   165      86 

#3. Test correpsonding LMEMs without univariate outliers and comorbidity 

Right_Ventral_DC_model_MGCFA_no_Uni_Out <- lmer(Right_Ventral_DC_log~ Total_Brain_Vol_log*Group+Medication
                                                + (1|SITE_ID2), Right_Ventral_DC_NEW_DF)

summary(Right_Ventral_DC_model_MGCFA_no_Uni_Out)$coefficients 
p.adjust(summary(Right_Ventral_DC_model_MGCFA_no_Uni_Out)$coefficients[,5], method = 'fdr') 

##### Left_Pallidum 
Left_Pallidum_model <- lmer(Left_Pallidum_log~ Total_Brain_Vol_log*DX_GROUP
                            + (1|SITE_ID2), Abide_Over_FSIQ)

summary(Left_Pallidum_model)$coefficients 
p.adjust(summary(Left_Pallidum_model)$coefficients[,5], method = 'fdr') 
   

# Create New DF for each Group 
Abide_Over_FSIQ_ASD<- Abide_Over_FSIQ_No_Comorbidity %>% 
  filter(DX_GROUP =="ASD")

Abide_Over_FSIQ_C <- Abide_Over_FSIQ_No_Comorbidity %>% 
  filter(DX_GROUP =="Control")

# Examine Boxplot Outliers & remove for Brain Volume DV and IV 
Left_Pallidum_Boxplot <- boxplot(Abide_Over_FSIQ_ASD$Left_Pallidum)
Left_Pallidum_Outlier_DF_ASD <- Abide_Over_FSIQ_ASD

Left_Pallidum_Boxplot <- boxplot(Left_Pallidum_Outlier_DF_ASD$Total_Brain_Vol)
Left_Pallidum_outliers <- Left_Pallidum_Boxplot$out # You can get the actual values of the outliers with this
Left_Pallidum_Outlier_DF_ASD <- Left_Pallidum_Outlier_DF_ASD[-which(Left_Pallidum_Outlier_DF_ASD$Total_Brain_Vol %in% Left_Pallidum_outliers),]
Left_Pallidum_Boxplot <- boxplot(Left_Pallidum_Outlier_DF_ASD$Total_Brain_Vol)

Left_Pallidum_Boxplot <- boxplot(Abide_Over_FSIQ_C$Left_Pallidum)
Left_Pallidum_outliers <- Left_Pallidum_Boxplot$out # You can get the actual values of the outliers with this
Left_Pallidum_Outlier_DF_C <- Abide_Over_FSIQ_C[-which(Abide_Over_FSIQ_C$Left_Pallidum %in% Left_Pallidum_outliers),]
Left_Pallidum_Boxplot <- boxplot(Left_Pallidum_Outlier_DF_C$Left_Pallidum)

TBV_Boxplot <- boxplot(Left_Pallidum_Outlier_DF_C$Total_Brain_Vol)
TBV_Boxplot_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
Left_Pallidum_Outlier_DF_C <- Left_Pallidum_Outlier_DF_C[-which(Left_Pallidum_Outlier_DF_C$Total_Brain_Vol %in% TBV_Boxplot_outliers),]
Left_Pallidum_Boxplot <- boxplot(Left_Pallidum_Outlier_DF_C$Total_Brain_Vol)

# Create New DF with both groups and without univariate outliers 
Left_Pallidum_NEW_DF <- Stack(Left_Pallidum_Outlier_DF_C,Left_Pallidum_Outlier_DF_ASD)
names(Left_Pallidum_NEW_DF)[names(Left_Pallidum_NEW_DF) == "DX_GROUP"] <- "Group"
Left_Pallidum_NEW_DF$Group <- factor(Left_Pallidum_NEW_DF$Group, levels = c("Control", "ASD"))

table(Left_Pallidum_NEW_DF$Group) #  168      87 

#3. Test correpsonding LMEMs without univariate outliers and comorbidity 

Left_Pallidum_model_MGCFA_no_Uni_Out <- lmer(Left_Pallidum_log~ Total_Brain_Vol_log*Group+ Medication
                                             + (1|SITE_ID2), Left_Pallidum_NEW_DF)

summary(Left_Pallidum_model_MGCFA_no_Uni_Out)$coefficients 
p.adjust(summary(Left_Pallidum_model_MGCFA_no_Uni_Out)$coefficients[,5], method = 'fdr')  

Left_Pallidum_figure_no_outlier<- ggplot(Left_Pallidum_no_outliers,
                                         aes(y=Left_Pallidum_log, x= Total_Brain_Vol_log, shape = Group,color=Group)) +
  geom_point(size = 6)+ 
  geom_smooth(method=lm,fullrange=TRUE)+ scale_shape_manual(values = c(21, 22)) +
  xlab("log10(Total Brain Volume)") + 
  ylab("log10(Right Hippocampus)") + 
  theme_classic() + My_Theme

Left_Pallidum_figure_no_outlier 

##### CorticalWhiteMatterVol 
CorticalWhiteMatterVol_model <- lmer(CorticalWhiteMatterVol_log~ Total_Brain_Vol_log*DX_GROUP
                                     + (1|SITE_ID2), Abide_Over_FSIQ)

summary(CorticalWhiteMatterVol_model)$coefficients 
p.adjust(summary(CorticalWhiteMatterVol_model)$coefficients[,5], method = 'fdr') 

# Create New DF for each Group 
Abide_Over_FSIQ_ASD<- Abide_Over_FSIQ_No_Comorbidity %>% 
  filter(DX_GROUP =="ASD")

Abide_Over_FSIQ_C <- Abide_Over_FSIQ_No_Comorbidity %>% 
  filter(DX_GROUP =="Control")

# Examine Boxplot Outliers & remove for Brain Volume DV and IV 
CorticalWhiteMatterVol_Boxplot <- boxplot(Abide_Over_FSIQ_ASD$CorticalWhiteMatterVol)
CorticalWhiteMatterVol_Outlier_DF_ASD <- Abide_Over_FSIQ_ASD

CorticalWhiteMatterVol_Boxplot <- boxplot(CorticalWhiteMatterVol_Outlier_DF_ASD$Total_Brain_Vol)
CorticalWhiteMatterVol_outliers <- CorticalWhiteMatterVol_Boxplot$out # You can get the actual values of the outliers with this
CorticalWhiteMatterVol_Outlier_DF_ASD <- CorticalWhiteMatterVol_Outlier_DF_ASD[-which(CorticalWhiteMatterVol_Outlier_DF_ASD$Total_Brain_Vol %in% CorticalWhiteMatterVol_outliers),]
CorticalWhiteMatterVol_Boxplot <- boxplot(CorticalWhiteMatterVol_Outlier_DF_ASD$Total_Brain_Vol)

CorticalWhiteMatterVol_Boxplot <- boxplot(Abide_Over_FSIQ_C$CorticalWhiteMatterVol)
CorticalWhiteMatterVol_outliers <- CorticalWhiteMatterVol_Boxplot$out # You can get the actual values of the outliers with this
CorticalWhiteMatterVol_Outlier_DF_C <- Abide_Over_FSIQ_C[-which(Abide_Over_FSIQ_C$CorticalWhiteMatterVol %in% CorticalWhiteMatterVol_outliers),]
CorticalWhiteMatterVol_Boxplot <- boxplot(CorticalWhiteMatterVol_Outlier_DF_C$CorticalWhiteMatterVol)

TBV_Boxplot <- boxplot(CorticalWhiteMatterVol_Outlier_DF_C$Total_Brain_Vol)
TBV_Boxplot_outliers <- TBV_Boxplot$out # You can get the actual values of the outliers with this
CorticalWhiteMatterVol_Outlier_DF_C <- CorticalWhiteMatterVol_Outlier_DF_C[-which(CorticalWhiteMatterVol_Outlier_DF_C$Total_Brain_Vol %in% TBV_Boxplot_outliers),]
CorticalWhiteMatterVol_Boxplot <- boxplot(CorticalWhiteMatterVol_Outlier_DF_C$Total_Brain_Vol)

# Create New DF with both groups and without univariate outliers 
CorticalWhiteMatterVol_NEW_DF <- Stack(CorticalWhiteMatterVol_Outlier_DF_C,CorticalWhiteMatterVol_Outlier_DF_ASD)
names(CorticalWhiteMatterVol_NEW_DF)[names(CorticalWhiteMatterVol_NEW_DF) == "DX_GROUP"] <- "Group"
CorticalWhiteMatterVol_NEW_DF$Group <- factor(CorticalWhiteMatterVol_NEW_DF$Group, levels = c("Control", "ASD"))

table(CorticalWhiteMatterVol_NEW_DF$Group) #  168      87 

#3. Test correpsonding LMEMs without univariate outliers and comorbidity 

CorticalWhiteMatterVol_model_MGCFA_no_Uni_Out <- lmer(CorticalWhiteMatterVol_log~ Total_Brain_Vol_log*Group+ Medication
                                                      + (1|SITE_ID2), CorticalWhiteMatterVol_NEW_DF)

summary(CorticalWhiteMatterVol_model_MGCFA_no_Uni_Out)$coefficients 
p.adjust(summary(CorticalWhiteMatterVol_model_MGCFA_no_Uni_Out)$coefficients[,5], method = 'fdr')

CorticalWhiteMatterVol_figure_no_outlier<- ggplot(CorticalWhiteMatterVol_no_outliers,
                                                  aes(y=CorticalWhiteMatterVol_log, x= Total_Brain_Vol_log, shape = Group,color=Group)) +
  geom_point(size = 6)+ 
  geom_smooth(method=lm,fullrange=TRUE)+ scale_shape_manual(values = c(21, 22)) +
  xlab("log10(Total Brain Volume)") + 
  ylab("log10(Right Hippocampus)") + 
  theme_classic() + My_Theme

CorticalWhiteMatterVol_figure_no_outlier 


