library(MatchIt)
library(dplyr)
library(pacman)
pacman::p_load(tableone)
library(tableone)
library(kable)
library(lavaan)

# Load Data from path where saved by filling in [XXXX] accordingly and remove # infront of command line below 
#load("[XXXX]Abide/Data/Abide.RData")

# compare the distribution of age and sex in both samples
ASD_Abide_6 = Abide_LMEM_Clean%>%
  filter(DX_GROUP == "ASD") %>%
  filter(AGE_AT_SCAN >= 12 & AGE_AT_SCAN < 20 ) 
Control_Abide_6 = Abide_LMEM_Clean%>%
  filter(DX_GROUP == "Control") %>%
  filter(AGE_AT_SCAN >= 12 & AGE_AT_SCAN < 20 ) 

t.test(ASD_Abide_6$AGE_AT_SCAN, Control_Abide_6$AGE_AT_SCAN)

Match_Abide <- data.frame(Abide_LMEM_Clean$SUB_ID, Abide_LMEM_Clean$DX_GROUP,
                          Abide_LMEM_Clean$AGE_AT_SCAN,Abide_LMEM_Clean$SITE_ID2,
                          Abide_LMEM_Clean$FIQ2,
                          Abide_LMEM_Clean$SEX)
levels(Match_Abide$Abide_LMEM_Clean.DX_GROUP) <- c(1,0)


table_Abide <- CreateTableOne(vars = c('Abide_LMEM_Clean.SEX', "Abide_LMEM_Clean.AGE_AT_SCAN",
                                       "Abide_LMEM_Clean.FIQ2","Abide_LMEM_Clean.SITE_ID2"), 
                              data = Match_Abide,factorVars = 'Abide_LMEM_Clean.SEX', 
                              strata = 'Abide_LMEM_Clean.DX_GROUP')

match.it <- matchit(Abide_LMEM_Clean.DX_GROUP ~ Abide_LMEM_Clean.FIQ2 + Abide_LMEM_Clean.AGE_AT_SCAN + Abide_LMEM_Clean.SEX +Abide_LMEM_Clean.SITE_ID2, data = Match_Abide, method="nearest", ratio=1)

df.match <- match.data(match.it)[1:ncol(Match_Abide)]
names(df.match)[names(df.match) == "Abide_LMEM_Clean.SUB_ID"] <- "SUB_ID"

View(df.match)

Abide_Match <- Abide_LMEM_Clean
Matched_Sample_DF <- inner_join(Abide_Match, df.match, by = "SUB_ID")
table(Matched_Sample_DF$DX_GROUP)

Match_Abide_log_scaled <- within (Matched_Sample_DF, {
  TotalGrayVol_log = scale(log10(`CortexVol`+`SubCortGrayVol`))
  Brain_Stem_log = scale(log10(`Brain-Stem`))
  Right_VentralDC_log = scale(log10(`Right-VentralDC`))
  Left_VentralDC_log =  scale(log10(`Left-VentralDC`))
  rhCortexVol_log = scale(log10(`rhCortexVol`))
  lhCortexVol_log = scale(log10(`lhCortexVol`))
  Right_CortexVol_log = scale(log10(`rhCortexVol`))
  Left_CortexVol_log = scale(log10(`lhCortexVol`))
  Right_Accumbens_area_log = scale(log10(`Right-Accumbens-area`))
  Right_Amygdala_log          = scale(log10(`Right-Amygdala`))
  Right_Caudate_log           = scale(log10(`Right-Caudate`))
  Right_Cerebellum_Cortex_log = scale(log10(`Right-Cerebellum-Cortex`))
  Right_Hippocampus_log = scale(log10(`Right-Hippocampus`))
  Right_Pallidum_log          = scale(log10(`Right-Pallidum`))
  Right_Putamen_log           = scale(log10(`Right-Putamen`))
  Right_Thalamus_Proper_log   = scale(log10(`Right-Thalamus-Proper`))
  Left_Accumbens_area_log     = scale(log10(`Left-Accumbens-area`))
  Left_Amygdala_log           = scale(log10(`Left-Amygdala`))
  Left_Caudate_log            = scale(log10(`Left-Caudate`))
  Left_Cerebellum_Cortex_log  = scale(log10(`Left-Cerebellum-Cortex`))
  Left_Hippocampus_log        = scale(log10(`Left-Hippocampus`))
  Left_Pallidum_log           = scale(log10(`Left-Pallidum`))
  Left_Putamen_log            = scale(log10(`Left-Putamen`))
  Left_Thalamus_Proper_log    = scale(log10(`Left-Thalamus-Proper`))
  CSF_log = scale(log10(`CSF`))
  CorticalWhiteMatterVol_log = scale(log10(CorticalWhiteMatterVol))
  Total_Brain_Vol_log = scale(log10(Total_Brain_Vol))
})

table(Match_Abide_log_scaled$DX_GROUP)
