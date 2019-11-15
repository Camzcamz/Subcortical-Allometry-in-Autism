library(QuantPsyc)
library(dplyr)
library(readr)
library(kableExtra)
library(data.table)
library(dplyr)
library(rcompanion)

# Load Data from path where saved by filling in [XXXX] accordingly and remove # infront of command line below 
#load("[XXXX]Abide/Data/Abide_LMEM_log_Clean.RData")

Abide_Clean_Seg <- Abide_LMEM_log_Clean
Abide_Clean_Seg$DX_GROUP <- factor(Abide_Clean_Seg$DX_GROUP, levels = c("Control", "ASD"))
Abide_Clean_Seg_ASD <- Abide_Clean_Seg %>% filter(DX_GROUP == "ASD")
Abide_Clean_Seg_Control <- Abide_Clean_Seg %>% filter(DX_GROUP == "Control")

############################ Paper ############################

### Sex Ratio 
SEX.data = table(Abide_Clean_Seg$DX_GROUP, Abide_Clean_Seg$SEX) 
chisq.test(SEX.data, simulate.p.value = T) 

### Age 
Abide_Clean_Seg$DX_GROUP <- as.factor(Abide_Clean_Seg$DX_GROUP)
hist(Abide_Clean_Seg$AGE_AT_SCAN)
kruskal.test(AGE_AT_SCAN ~ DX_GROUP, data = Abide_Clean_Seg) 

summary(Abide_Clean_Seg_Control$AGE_AT_SCAN)
sd(Abide_Clean_Seg_Control$AGE_AT_SCAN)

summary(Abide_Clean_Seg_ASD$AGE_AT_SCAN)
sd(Abide_Clean_Seg_ASD$AGE_AT_SCAN)

# SE = s / sqRoot(N) 

### Handedness  
func_hand <- function (x) {
  Hand.data = table(x$DX_GROUP, x$HANDEDNESS_CATEGORY) 
  ctbl = cbind(Hand.data[,"R"],+ Hand.data[,"Mixed"]+ Hand.data[,"Ambi"]+ Hand.data[,"L"]) 
  print(ctbl)
  print(chisq.test(ctbl)) }

func_hand (Abide_Clean_Seg) 

### FSIQ 
Abide_Clean_Seg$DX_GROUP <- as.factor(Abide_Clean_Seg$DX_GROUP)
hist(Abide_Clean_Seg$FIQ2)
t.test(FIQ2 ~ DX_GROUP, data = Abide_Clean_Seg) 
kruskal.test(FIQ2 ~ DX_GROUP, data = Abide_Clean_Seg) 

summary(Abide_Clean_Seg_Control$FIQ2)
sd(Abide_Clean_Seg_Control$FIQ2)

summary(Abide_Clean_Seg_ASD$FIQ2)
sd(Abide_Clean_Seg_ASD$FIQ2)

############################ Supplemental Analyses ############################

#### Generate Subsample DF 
ASD= Abide_Clean_Seg%>%
  filter(DX_GROUP == 'ASD') 

Controls = Abide_Clean_Seg%>%
  filter(DX_GROUP == 'Control') 

Boys= Abide_Clean_Seg%>%
  filter(SEX == 'Male') 

Girls= Abide_Clean_Seg%>%
  filter(SEX == 'Female') 

Boys_Age_6_12 = Abide_Clean_Seg%>%
  filter(AGE_AT_SCAN <12) %>%
  filter(SEX == 'Male') 
table(Boys_Age_6_12$DX_GROUP)

Boys_Age_13_19 = Abide_Clean_Seg%>%
  filter(AGE_AT_SCAN >= 12 & AGE_AT_SCAN <20) %>%
  filter(SEX == 'Male')

Boys_Age_20_27 = Abide_Clean_Seg%>%
  filter(AGE_AT_SCAN >=20) %>%
  filter(SEX == 'Male') 

ASD_Boys_Age_6_12 = Abide_Clean_Seg%>%
  filter(AGE_AT_SCAN <= 12) %>%
  filter(DX_GROUP== 'ASD') %>%
  filter(SEX == 'Male') 

ASD_Boys_Age_13_19 = Abide_Clean_Seg%>%
  filter(AGE_AT_SCAN >= 12 & AGE_AT_SCAN <20) %>%
  filter(DX_GROUP== 'ASD') %>%
  filter(SEX == 'Male') 

ASD_Boys_Age_20_27 = Abide_Clean_Seg%>%
  filter(AGE_AT_SCAN >=20) %>%
  filter(DX_GROUP== 'ASD') %>%
  filter(SEX == 'Male') 

Control_Boys_Age_6_12 = Abide_Clean_Seg%>%
  filter(AGE_AT_SCAN <= 12) %>%
  filter(DX_GROUP== 'Control') %>%
  filter(SEX == 'Male') 

Control_Boys_Age_13_19 = Abide_Clean_Seg%>%
  filter(AGE_AT_SCAN >= 12 & AGE_AT_SCAN <20) %>%
  filter(DX_GROUP== 'Control') %>%
  filter(SEX == 'Male') 

Control_Boys_FIQ_Under = Abide_Clean_Seg%>%
  filter(FIQ2 <= 107.8) %>%
  filter(DX_GROUP== 'Control')%>%
  filter(SEX == 'Male') 

ASD_Boys_FIQ_Under = Abide_Clean_Seg%>%
  filter(FIQ2 <= 107.8) %>%
  filter(DX_GROUP== 'ASD')%>%
  filter(SEX == 'Male') 

ASD_Boys_FIQ_Over = Abide_Clean_Seg%>%
  filter(FIQ2 > 107.8) %>%
  filter(DX_GROUP== 'ASD')%>%
  filter(SEX == 'Male') 

Control_Boys_FIQ_Over = Abide_Clean_Seg%>%
  filter(FIQ2 > 107.8) %>%
  filter(DX_GROUP== 'Control')%>%
  filter(SEX == 'Male') 

Boys_FIQ_Over = Abide_Clean_Seg%>%
  filter(FIQ2 <= 107.8) %>%
  filter(SEX == 'Male') 


##### Generate Tables 
Demographics_Table <- function (x) { 
  Variables_number <- c("AGE_AT_SCAN", "FIQ2", "Total_Brain_Vol","CorticalWhiteMatterVol","TotalGrayVol",
                        "Ventral_DC","Right-VentralDC", "Left-VentralDC" ,"Thalamus_Proper", "Right-Thalamus-Proper",
                        "Left-Thalamus-Proper","Putamen","Right-Putamen","Left-Putamen", 
                        "Pallidum","Right-Pallidum","Left-Pallidum","Hippocampus", "Right-Hippocampus",
                        "Left-Hippocampus","Accumbens_Area", "Right-Accumbens-area",
                        "Left-Accumbens-area" , "Amygdala" ,"Right-Amygdala",
                        "Left-Amygdala", "Caudate", "Right-Caudate", "Left-Caudate",
                        "Cerebellum_Cortex","Right-Cerebellum-Cortex","Left-Cerebellum-Cortex" , 
                        "Cortex","rhCortexVol","lhCortexVol", "Brain-Stem", "CSF")
  Variables_number_mean <- lapply(x[Variables_number], mean, na.rm=TRUE)
  Variables_number_min <- lapply(x[Variables_number], min, na.rm=TRUE)
  Variables_number_max <- lapply(x[Variables_number], max, na.rm=TRUE)
  Variables_number_med <- lapply(x[Variables_number], median, na.rm=TRUE)
  Variables_number_sd <- lapply(x[Variables_number], sd, na.rm=TRUE)
  N <- length(x$FIQ2)
  Variables_number_se <- lapply(Variables_number_sd, function (x) {x/N})
  Variables_number_table <- cbind(Mean = Variables_number_mean, SD = Variables_number_sd, SE = Variables_number_se, Min = Variables_number_min, Max = Variables_number_max, Median = Variables_number_med)
}

DF_list = list(ASD, Controls, Abide_Clean_Seg, Boys, Girls, ASD_Boys_Age_6_12, ASD_Boys_Age_13_19,Control_Boys_Age_6_12,Control_Boys_Age_13_19,
               Control_Boys_FIQ_Under,Control_Boys_FIQ_Over,ASD_Boys_FIQ_Under,ASD_Boys_FIQ_Over)
Demographic_Table_All_Samples <- lapply(DF_list, Demographics_Table)

sink("Demographic_Table_All_Samples.csv")
print(Demographic_Table_All_Samples)
sink()

#### AGE differences across sites
Abide_Clean_Seg$SITE_ID2 <- as.factor(Abide_Clean_Seg$SITE_ID2)
kruskal.test(AGE_AT_SCAN ~ SITE_ID2, data = Abide_Clean_Seg) 

#### SEX differences across sites
SEX.Site.data = table(Abide_Clean_Seg$SITE_ID2, Abide_Clean_Seg$SEX) 
chisq.test(SEX.Site.data, simulate.p.value = T) 

#### FSIQ differences across sites
Comp_Sites_FIQ2<-pairwise.wilcox.test(Abide_Clean_Seg$FIQ2, Abide_Clean_Seg$SITE_ID2,
                                      p.adjust.method = "BH")
kruskal.test(FIQ2 ~ SITE_ID2, data = Abide_Clean_Seg) 

#### Handedness differences across sites
Hand.data = table(Abide_Clean_Seg$SITE_ID2, Abide_Clean_Seg$HANDEDNESS_CATEGORY) 
ctbl = cbind(Hand.data[,"R"],+ Hand.data[,"Mixed"]+ Hand.data[,"Ambi"]+ Hand.data[,"L"]) 
ctbl = data.frame(ctbl)
ctbl = ctbl[-5,]
ctbl = ctbl [-13,]
print(ctbl)
chisq.test(ctbl,simulate.p.value = T)
