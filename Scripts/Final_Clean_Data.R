######################## INSTALL & LOAD PACKAGES ########################
library(QuantPsyc)
library(dplyr)
library(readr)
library(kableExtra)
library(data.table)

######################## READ IN DATA #########################
### Import Data set into Abide_Tabbed and Quality Check (QC) into Bad_Seg_Abide
Abide_Tabbed <- read_csv("Data/Abide_Tabbed.csv")
Bad_Seg_Abide <- read_csv("Data/Bad_Data_QC3.csv")

Bad_Seg_Abide <- Bad_Seg_Abide %>%  
  mutate(SUB_ID = as.numeric(SUB_ID))
Abide_Tabbed <- inner_join(Bad_Seg_Abide, Abide_Tabbed, "SUB_ID") 

Abide = Abide_Tabbed # Merge Data set and QC 

######################## CLEANING DATA ########################
### AGE2 ###
Abide = Abide %>%
  mutate(Age2 = AGE_AT_SCAN*AGE_AT_SCAN)%>%  
  mutate(SEX = as.character(SEX))

Abide$SEX[Abide$SEX=="1"] <- "Male" 
Abide$SEX[Abide$SEX=="2"] <- "Female" 

### REMOVING BAD SEGMENTATION from  QC, remove subjects without "ok" in  OK_Comments ###
Abide <- Abide[!is.na(Abide$OK_Comments),]
Abide_LMEM <- Abide

#a) create DV and Scale 
Abide_LMEM <- Abide_LMEM %>% 
  mutate(Age2 = AGE_AT_SCAN*AGE_AT_SCAN)%>%  
  mutate(SEX = as.factor(SEX)) %>%
  mutate(Right_Ventral_DC = `Right-VentralDC` ) %>%
  mutate(Left_Ventral_DC = `Left-VentralDC`) %>%
  mutate(Right_Thalamus_Proper = `Right-Thalamus-Proper`) %>% 
  mutate(Left_Thalamus_Proper = `Left-Thalamus-Proper`) %>% 
  mutate(Left_Ventral_DC = `Left-VentralDC`) %>% 
  mutate(Right_Ventral_DC = `Right-VentralDC`) %>% 
  mutate(Right_Putamen = `Right-Putamen`) %>%
  mutate(Left_Putamen = `Left-Putamen`) %>%
  mutate(Right_Pallidum = `Right-Pallidum`) %>%
  mutate(Left_Pallidum = `Left-Pallidum`) %>%
  mutate(Right_Hippocampus = `Right-Hippocampus`) %>%
  mutate(Left_Hippocampus = `Left-Hippocampus`) %>%
  mutate(Right_Accumbens_Area = `Right-Accumbens-area`) %>% 
  mutate(Left_Accumbens_Area = `Left-Accumbens-area`) %>% 
  mutate(Right_Amygdala = `Right-Amygdala`) %>%
  mutate(Left_Amygdala = `Left-Amygdala`) %>%
  mutate(Right_Caudate =`Right-Caudate`) %>% 
  mutate(Left_Caudate =`Left-Caudate`) %>% 
  mutate(Right_Cerebellum_Cortex = `Right-Cerebellum-Cortex`)%>% 
  mutate(Left_Cerebellum_Cortex = `Left-Cerebellum-Cortex`)%>% 
  mutate(Right_CortexVol = `rhCortexVol`)%>%
  mutate(Left_CortexVol = `lhCortexVol`)%>%
  mutate(Ventral_DC = `Right-VentralDC`+`Left-VentralDC`) %>% 
  mutate(Thalamus_Proper = `Right-Thalamus-Proper` + `Left-Thalamus-Proper`) %>% 
  mutate(Ventral_DC = `Right-VentralDC` + `Left-VentralDC`) %>% 
  mutate(Putamen = `Right-Putamen`+ `Left-Putamen`) %>%
  mutate(Pallidum = `Right-Pallidum`+`Left-Pallidum`) %>%
  mutate(Hippocampus = `Right-Hippocampus`+`Left-Hippocampus`) %>%
  mutate(Accumbens_Area = `Right-Accumbens-area`+`Left-Accumbens-area`) %>% 
  mutate(Amygdala = `Right-Amygdala`+`Left-Amygdala`) %>%
  mutate(Caudate =`Right-Caudate`+`Left-Caudate`) %>% 
  mutate(Brain_Stem =`Brain-Stem`)%>% 
  mutate(Cerebellum_Cortex = `Right-Cerebellum-Cortex`+`Left-Cerebellum-Cortex`)%>% 
  mutate(Cortex = `rhCortexVol`+`lhCortexVol`) %>%
  mutate(Total_Brain_Vol = `TotalGrayVol`+`CorticalWhiteMatterVol`)

Abide_LMEM_log <- within (Abide_LMEM, {
  TotalGrayVol_log = log10(`CortexVol`+`SubCortGrayVol`)
  Brain_Stem_log = log10(`Brain-Stem`)
  Right_Ventral_DC_log = log10(`Right-VentralDC`)
  Left_Ventral_DC_log =  log10(`Left-VentralDC`)
  CortexVol_log = log10(`CortexVol`)
  rhCortexVol_log = log10(`rhCortexVol`)
  lhCortexVol_log = log10(`lhCortexVol`)
  Right_CortexVol_log = log10(`rhCortexVol`)
  Left_CortexVol_log = log10(`lhCortexVol`)
  Right_Accumbens_Area_log = log10(`Right-Accumbens-area`)
  Right_Amygdala_log          = log10(`Right-Amygdala`)
  Right_Caudate_log           = log10(`Right-Caudate`)
  Right_Cerebellum_Cortex_log = log10(`Right-Cerebellum-Cortex`)
  Right_Hippocampus_log = log10(`Right-Hippocampus`)
  Right_Pallidum_log          = log10(`Right-Pallidum`)
  Right_Putamen_log           = log10(`Right-Putamen`)
  Right_Thalamus_Proper_log   = log10(`Right-Thalamus-Proper`)
  Left_Accumbens_Area_log     = log10(`Left-Accumbens-area`)
  Left_Amygdala_log           = log10(`Left-Amygdala`)
  Left_Caudate_log            = log10(`Left-Caudate`)
  Left_Cerebellum_Cortex_log  = log10(`Left-Cerebellum-Cortex`)
  Left_Hippocampus_log        = log10(`Left-Hippocampus`)
  Left_Pallidum_log           = log10(`Left-Pallidum`)
  Left_Putamen_log            = log10(`Left-Putamen`)
  Left_Thalamus_Proper_log    = log10(`Left-Thalamus-Proper`)
  Caudate_log    = log10(`Left-Caudate`+`Right-Caudate`)
  CorticalWhiteMatterVol_log = log10(CorticalWhiteMatterVol)
  Total_Brain_Vol_log = log10(Total_Brain_Vol)
  Intra_Cranial_Volume_log = log10(`Total_Brain_Vol`+`CSF`)
  Accumbens_area_log = log10(Accumbens_Area)
  Caudate_log = log10(Caudate)
  CortexVol_log = log10(CortexVol)
  Amygdala_log = log10(Amygdala)
  Pallidum_log = log10(Pallidum)
  Putamen_log = log10(Putamen)
  Thalamus_Proper_log = log10(Thalamus_Proper)
  Hippocampus_log = log10(Hippocampus)
  Cerebellum_Cortex_log = log10(Cerebellum_Cortex)
  Ventral_DC_log = log10(Ventral_DC)
})

Abide_LMEM_log_scale <- within (Abide_LMEM, {
  TotalGrayVol_log = scale(log10(`CortexVol`+`SubCortGrayVol`))
  Total_Brain_Vol = (`TotalGrayVol`+`CorticalWhiteMatterVol`)
  Brain_Stem_log = scale(log10(`Brain-Stem`))
  Right_Ventral_DC_log = scale(log10(`Right-VentralDC`))
  Left_Ventral_DC_log =  scale(log10(`Left-VentralDC`))
  CortexVol_log = scale(log10(`CortexVol`))
  rhCortexVol_log = scale(log10(`rhCortexVol`))
  lhCortexVol_log = scale(log10(`lhCortexVol`))
  Right_CortexVol_log = scale(log10(`rhCortexVol`))
  Left_CortexVol_log = scale(log10(`lhCortexVol`))
  Right_Accumbens_Area_log = scale(log10(`Right-Accumbens-area`))
  Right_Amygdala_log          = scale(log10(`Right-Amygdala`))
  Right_Caudate_log           = scale(log10(`Right-Caudate`))
  Right_Cerebellum_Cortex_log = scale(log10(`Right-Cerebellum-Cortex`))
  Right_Hippocampus_log = scale(log10(`Right-Hippocampus`))
  Right_Pallidum_log          = scale(log10(`Right-Pallidum`))
  Right_Putamen_log           = scale(log10(`Right-Putamen`))
  Right_Thalamus_Proper_log   = scale(log10(`Right-Thalamus-Proper`))
  Left_Accumbens_Area_log     = scale(log10(`Left-Accumbens-area`))
  Left_Amygdala_log           = scale(log10(`Left-Amygdala`))
  Left_Caudate_log            = scale(log10(`Left-Caudate`))
  Left_Cerebellum_Cortex_log  = scale(log10(`Left-Cerebellum-Cortex`))
  Left_Hippocampus_log        = scale(log10(`Left-Hippocampus`))
  Left_Pallidum_log           = scale(log10(`Left-Pallidum`))
  Left_Putamen_log            = scale(log10(`Left-Putamen`))
  Left_Thalamus_Proper_log    = scale(log10(`Left-Thalamus-Proper`))
  Caudate_log    = scale(log10(`Left-Caudate`+`Right-Caudate`))
  CorticalWhiteMatterVol_log = scale(log10(CorticalWhiteMatterVol))
  Total_Brain_Vol_log = scale(log10(Total_Brain_Vol))
  Intra_Cranial_Volume_log = scale(log10(`Total_Brain_Vol`+`CSF`))
  Accumbens_area_log = scale(log10(Accumbens_Area))
  Caudate_log = scale(log10(Caudate))
  CortexVol_log = scale(log10(CortexVol))
  Amygdala_log = scale(log10(Amygdala))
  Pallidum_log = scale(log10(Pallidum))
  Putamen_log = scale(log10(Putamen))
  Thalamus_Proper_log = scale(log10(Thalamus_Proper))
  Hippocampus_log = scale(log10(Hippocampus))
  Cerebellum_Cortex_log = scale(log10(Cerebellum_Cortex))
  Ventral_DC_log = scale(log10(Ventral_DC))
})

# b) remove NA 
Abide_LMEM_Clean <- Abide_LMEM %>%
  filter(AGE_AT_SCAN <27) %>%
  filter(70<FIQ2) %>%
  filter(FIQ2<130) %>%
  filter(FIQ2 != 'NA') %>%
  filter(SITE_ID2 != 'NA') %>%
  filter(Total_Brain_Vol != 'NA') %>%
  filter(Left_Thalamus_Proper != 'NA') %>%
  filter(Right_Thalamus_Proper != 'NA') %>%
  filter(Left_Putamen != 'NA') %>%
  filter(Right_Putamen != 'NA') %>%
  filter(Left_Pallidum != 'NA') %>%
  filter(Right_Pallidum != 'NA') %>%
  filter(Left_Hippocampus != 'NA') %>%
  filter(Right_Hippocampus != 'NA') %>%
  filter(Left_Cerebellum_Cortex != 'NA') %>%
  filter(Right_Cerebellum_Cortex != 'NA') %>%
  filter(Left_Caudate != 'NA') %>%
  filter(Right_Caudate != 'NA') %>%
  filter(Left_Amygdala != 'NA') %>%
  filter(Right_Amygdala != 'NA') %>%
  filter(Left_Accumbens_Area != 'NA') %>%
  filter(Right_Accumbens_Area != 'NA') %>%
  filter(Brain_Stem != 'NA') %>%
  filter(Right_Ventral_DC != 'NA') %>%
  filter(Left_Ventral_DC != 'NA') %>%
  filter(CortexVol != 'NA') %>%
  filter(rhCortexVol != 'NA') %>%
  filter(lhCortexVol != 'NA') %>%
  filter(CorticalWhiteMatterVol != 'NA') %>%
  filter(QCFS_Preprocessed == 'Yes')

Abide_LMEM_log_Clean = Abide_LMEM_log %>%
  filter(AGE_AT_SCAN <27) %>%
  filter(70<FIQ2) %>%
  filter(FIQ2<130) %>%
  filter(FIQ2 != 'NA') %>%
  filter(SITE_ID2 != 'NA') %>%
  filter(Intra_Cranial_Volume_log != 'NA') %>%
  filter(Total_Brain_Vol_log != 'NA') %>%
  filter(Left_Thalamus_Proper_log != 'NA') %>%
  filter(Right_Thalamus_Proper_log != 'NA') %>%
  filter(Left_Putamen_log != 'NA') %>%
  filter(Right_Putamen_log != 'NA') %>%
  filter(Left_Pallidum_log != 'NA') %>%
  filter(Right_Pallidum_log != 'NA') %>%
  filter(Left_Hippocampus_log != 'NA') %>%
  filter(Right_Hippocampus_log != 'NA') %>%
  filter(Left_Cerebellum_Cortex_log != 'NA') %>%
  filter(Right_Cerebellum_Cortex_log != 'NA') %>%
  filter(Left_Caudate_log != 'NA') %>%
  filter(Right_Caudate_log != 'NA') %>%
  filter(Left_Amygdala_log != 'NA') %>%
  filter(Right_Amygdala_log != 'NA') %>%
  filter(Left_Accumbens_Area_log != 'NA') %>%
  filter(Right_Accumbens_Area_log != 'NA') %>%
  filter(Brain_Stem_log != 'NA') %>%
  filter(Right_Ventral_DC_log != 'NA') %>%
  filter(Left_Ventral_DC_log != 'NA') %>%
  filter(CortexVol_log != 'NA') %>%
  filter(rhCortexVol_log != 'NA') %>%
  filter(lhCortexVol_log != 'NA') %>%
  filter(CorticalWhiteMatterVol_log != 'NA') %>%
  filter(QCFS_Preprocessed == 'Yes')

Abide_LMEM_log_Clean_scaled = Abide_LMEM_log_scale %>%
  filter(AGE_AT_SCAN <27) %>%
  filter(70<FIQ2) %>%
  filter(FIQ2<130) %>%
  filter(FIQ2 != 'NA') %>%
  filter(SITE_ID2 != 'NA') %>%
  filter(Intra_Cranial_Volume_log != 'NA') %>%
  filter(Total_Brain_Vol_log != 'NA') %>%
  filter(Left_Thalamus_Proper_log != 'NA') %>%
  filter(Right_Thalamus_Proper_log != 'NA') %>%
  filter(Left_Putamen_log != 'NA') %>%
  filter(Right_Putamen_log != 'NA') %>%
  filter(Left_Pallidum_log != 'NA') %>%
  filter(Right_Pallidum_log != 'NA') %>%
  filter(Left_Hippocampus_log != 'NA') %>%
  filter(Right_Hippocampus_log != 'NA') %>%
  filter(Left_Cerebellum_Cortex_log != 'NA') %>%
  filter(Right_Cerebellum_Cortex_log != 'NA') %>%
  filter(Left_Caudate_log != 'NA') %>%
  filter(Right_Caudate_log != 'NA') %>%
  filter(Left_Amygdala_log != 'NA') %>%
  filter(Right_Amygdala_log != 'NA') %>%
  filter(Left_Accumbens_Area_log != 'NA') %>%
  filter(Right_Accumbens_Area_log != 'NA') %>%
  filter(Brain_Stem_log != 'NA') %>%
  filter(Right_Ventral_DC_log != 'NA') %>%
  filter(Left_Ventral_DC_log != 'NA') %>%
  filter(CortexVol_log != 'NA') %>%
  filter(rhCortexVol_log != 'NA') %>%
  filter(lhCortexVol_log != 'NA') %>%
  filter(CorticalWhiteMatterVol_log != 'NA') %>%
  filter(QCFS_Preprocessed == 'Yes')


save(Abide_LMEM_Clean, file = "Data/Abide_LMEM_Clean.RData")
save(Abide_LMEM_log_Clean, file = "Data/Abide_LMEM_log_Clean.RData")
save(Abide_LMEM_log_Clean_scaled, file = "Data/Abide_LMEM_log_Clean_scaled.RData")


##################### Zhang Replication Data Sets ##################### 
library (tidyr)

##### Log Transformed 
names(Abide_LMEM_log_Clean)
table(Abide_LMEM_log_Clean$DX_GROUP)
Abide_Clean_Seg_Hemi <- dplyr::select(Abide_LMEM_log_Clean, AGE_AT_SCAN, SUB_ID, SITE_ID2, DX_GROUP,CorticalWhiteMatterVol_log,
                                      SEX, FIQ2, Age2, Total_Brain_Vol_log,
                                      Left_Thalamus_Proper_log:Right_CortexVol_log, 
                                      Left_Ventral_DC_log, Right_Ventral_DC_log, Brain_Stem_log, 
                                      TotalGrayVol_log)
Abide_Clean_Seg_Hemi$SUB_ID <- factor(Abide_Clean_Seg_Hemi$SUB_ID)

Abide_Clean_Seg_data_long <- gather(Abide_Clean_Seg_Hemi, Region, Volume,Left_Thalamus_Proper_log:Right_CortexVol_log, Left_Ventral_DC_log:Right_Ventral_DC_log)
Abide_Zhang_log_DF <- Abide_Clean_Seg_data_long %>% separate(Region, c("Hemisphere", "Region"), convert = T, sep = "\\_")
as.factor(Abide_Zhang_log_DF$Hemisphere)
Abide_Zhang_log_DF <- Abide_Zhang_log_DF %>% tidyr::spread(Region, Volume)
table(Abide_Zhang_log_DF$DX_GROUP)
save(Abide_Zhang_log_DF, file = "Data/Abide_Zhang_log_DF.Rdata")

##### Not log Transformed 
names(Abide_LMEM_Clean)
table(Abide_LMEM_Clean$DX_GROUP)
Abide_LMEM_Rep_Clean_Hemi <- dplyr::select(Abide_LMEM_Clean, AGE_AT_SCAN, SUB_ID, SITE_ID2, DX_GROUP, SEX, FIQ2, TotalGrayVol,
                                           CorticalWhiteMatterVol, Total_Brain_Vol, Brain_Stem, Right_Ventral_DC:Left_CortexVol)
Abide_LMEM_Rep_Clean_Hemi$SUB_ID <- factor(Abide_LMEM_Rep_Clean_Hemi$SUB_ID)
Abide_LMEM_Rep_Clean_data_long <- gather(Abide_LMEM_Rep_Clean_Hemi, Region, Volume, Right_Ventral_DC:Left_CortexVol, factor_key=TRUE) # from DF add two new columns region and vol and input

Abide_Zhang_DF <- Abide_LMEM_Rep_Clean_data_long %>% separate(Region, c("Hemisphere", "Region"), convert = T, sep = "\\_")
Abide_Zhang_DF <- Abide_Zhang_DF %>% tidyr::spread(Region, Volume)
Abide_Zhang_DF$Hemisphere <- as.factor(Abide_Zhang_DF$Hemisphere)
table(Abide_Zhang_DF$DX_GROUP)
save(Abide_Zhang_DF, file = "Data/Abide_Zhang_DF.Rdata")

## CHECK 
check1<- dplyr::select(Abide_Zhang_DF, Hemisphere, SUB_ID, Total_Brain_Vol, CortexVol)
check <- dplyr::filter(check1, SUB_ID == 51306)

