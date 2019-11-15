library(lmerTest)
library(dplyr)
library(simr) # lmerTest Power
library(lsr) # cohen's d 
library(rcompanion)

Abide_LMEM = Abide_LMEM_log_Clean_scaled
Abide_LMEM$FSIQ <- scale(Abide_LMEM$FIQ2)
Abide_LMEM$AGE <- scale(Abide_LMEM$AGE_AT_SCAN)
Abide_LMEM$AGE2 <- scale(Abide_LMEM$Age2)
Abide_LMEM$SITE_ID2 <- as.factor(Abide_LMEM$SITE_ID2)
Abide_LMEM$DX_GROUP <- factor(Abide_LMEM$DX_GROUP, levels = c("Control", "ASD"))

Abide_LMEM_listdv_all <- grep("_log", names(Abide_LMEM), fixed=T, value=T) 
Abide_LMEM_listdv <- Abide_LMEM_listdv_all[c(11, 13:30, 34:37)] 

### No age, sex, FSIQ effects 

LMEM_LIST <- as.list(paste(Abide_LMEM_listdv, "~  AGE * SEX * FSIQ * DX_GROUP * Total_Brain_Vol_log + (1|SITE_ID2)"))

LMEM_LIST_Models <- lapply(LMEM_LIST,function(x) {lmer(x,data=Abide_LMEM)})
names(LMEM_LIST_Models) <- LMEM_LIST 

coef <- function (x) {summary(x)$coefficients}
FDR_p <- function(x) {p.adjust(summary(x)$coefficients[,5], method = "fdr")}

A <- lapply(LMEM_LIST_Models, coef)
B <- lapply(LMEM_LIST_Models, FDR_p)

sink("Full_LMEMs_Entire_Sample.csv")
print(A)
print(B)
sink()

##### By Subsample 
Boys_Age_6_12 = Abide_LMEM%>%
  filter(AGE_AT_SCAN <= 12) %>%
  filter(SEX == 'Male') 

Boys_Age_12_20 = Abide_LMEM%>%
  filter(AGE_AT_SCAN >= 12 & AGE_AT_SCAN <20) %>%
  filter(SEX == 'Male') 

Boys_FIQ_Under = Abide_LMEM%>%
  filter(FIQ2 <= 107.8) %>%
  filter(SEX == 'Male') 

Boys_FIQ_Over = Abide_LMEM%>%
  filter(FIQ2 > 107.8) %>%
  filter(SEX == 'Male') 

No_Confounds_LMEM_LIST <- as.list(paste(Abide_LMEM_listdv, "~   DX_GROUP * Total_Brain_Vol_log + (1|SITE_ID2)"))
AGE_LMEM_LIST <- as.list(paste(Abide_LMEM_listdv, "~  FSIQ * DX_GROUP * Total_Brain_Vol_log + (1|SITE_ID2)"))
FSIQ_LMEM_LIST <- as.list(paste(Abide_LMEM_listdv, "~  AGE * DX_GROUP * Total_Brain_Vol_log + (1|SITE_ID2)")) 
FSIQ1_LMEM_LIST <- as.list(paste(Abide_LMEM_listdv, "~  AGE2 * DX_GROUP * Total_Brain_Vol_log + (1|SITE_ID2)"))


AGE_6_12_LMEM_LIST <- lapply(AGE_LMEM_LIST,function(x) {lmer(x,data=Boys_Age_6_12)}) 
names(AGE_6_12_LMEM_LIST) <- AGE_LMEM_LIST


AGE_12_20_LMEM_LIST <- lapply(AGE_LMEM_LIST,function(x) {lmer(x,data=Boys_Age_12_20)})
names(AGE_12_20_LMEM_LIST) <- AGE_LMEM_LIST

AGE_12_20_NO_CONFOUNDS_LMEM_LIST <- lapply(No_Confounds_LMEM_LIST,function(x) {lmer(x,data=Boys_Age_12_20)})
names(AGE_12_20_NO_CONFOUNDS_LMEM_LIST) <- No_Confounds_LMEM_LIST
lapply(AGE_12_20_NO_CONFOUNDS_LMEM_LIST, function(x) {summary(x)$coefficients[c(4,20)]})

### FSIQ was added in the "model"FSIQ_LMEM_LIST" for "Boys_FIQ_Under" since groups differed in terms of FSIQ 
Under_FSIQ_LMEM_LIST <- lapply(FSIQ_LMEM_LIST,function(x) {lmer(x,data=Boys_FIQ_Under)})
names(Under_FSIQ_LMEM_LIST) <- LMEM_LIST
Under_FSIQ1_LMEM_LIST <- lapply(FSIQ1_LMEM_LIST,function(x) {lmer(x,data=Boys_FIQ_Under)})
names(Under_FSIQ_LMEM_LIST) <- LMEM_LIST

Over_FSIQ_LMEM_LIST <- lapply(FSIQ_LMEM_LIST,function(x) {lmer(x,data=Boys_FIQ_Over)})
names(Over_FSIQ_LMEM_LIST) <- LMEM_LIST

Boys_FIQ_Over_NO_CONFOUNDS_LMEM_LIST <- lapply(No_Confounds_LMEM_LIST,function(x) {lmer(x,data=Boys_FIQ_Over)})
names(Boys_FIQ_Over_NO_CONFOUNDS_LMEM_LIST) <- No_Confounds_LMEM_LIST
lapply(Boys_FIQ_Over_NO_CONFOUNDS_LMEM_LIST, function(x) {summary(x)$coefficients[c(4,8,20)]})

Over_FSIQ1_LMEM_LIST <- lapply(FSIQ1_LMEM_LIST,function(x) {lmer(x,data=Boys_FIQ_Over)})
names(Under_FSIQ_LMEM_LIST) <- LMEM_LIST
