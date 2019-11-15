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

LMEM_LIST <- as.list(paste(Abide_LMEM_listdv, "~  DX_GROUP * Total_Brain_Vol_log + (1|SITE_ID2)"))

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

Small_ES_Allometry <- function (x) {fixef(x)["DX_GROUPASD:Total_Brain_Vol_log"] <- 0.2}
lapply(LMEM_LIST_Models, Small_ES_Allometry)

Power_Simulation_Allometry <- function(x) {powerSim(x, test = fixed("DX_GROUPASD:Total_Brain_Vol_log", "sa"))}
Power_Simulation_Group <- function(x) {powerSim(x, test = fixed("DX_GROUPASD", "sa"))}

Allometry_Simulation_List <- lapply(LMEM_LIST_Models, Power_Simulation_Allometry)
Allometry_Simulation_List
Group_Simulation_List <- lapply(LMEM_LIST_Models, Power_Simulation_Group)
Group_Simulation_List
