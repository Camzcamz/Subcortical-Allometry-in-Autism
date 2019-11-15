whelibrary(dplyr)
ls("package:MASS")
library("Hmisc")
library(corrplot)
library("gridExtra")
library(ggplot2)
library(cocor)
library(dplyr)
library(rcompanion)
library(car)

# Load Data from path where saved by filling in [XXXX] accordingly and remove # infront of command line below 
#load("[XXXX]Abide/Data/Abide_LMEM_log_Clean.RData")

#correlation plots : http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

######################################## ENTIRE SAMPLE #######################################
Abide_Clean_Seg_corr <- dplyr::select(Abide_LMEM_log_Clean, AGE_AT_SCAN, SITE_ID2, DX_GROUP, SEX, FIQ2, TotalGrayVol, Total_Brain_Vol,
                                           CorticalWhiteMatterVol, Brain_Stem, Age2:Left_CortexVol)
colnames(Abide_Clean_Seg_corr)[colnames(Abide_Clean_Seg_corr)=="TotalGrayVol"] <- "Total Grey Matter"
colnames(Abide_Clean_Seg_corr)[colnames(Abide_Clean_Seg_corr)=="CorticalWhiteMatterVol"] <- "Cortical White Matter"
colnames(Abide_Clean_Seg_corr)[colnames(Abide_Clean_Seg_corr)=="DX_GROUP"] <- "Group"
colnames(Abide_Clean_Seg_corr)[colnames(Abide_Clean_Seg_corr)=="FIQ2"] <- "FSIQ"
colnames(Abide_Clean_Seg_corr)[colnames(Abide_Clean_Seg_corr)=="AGE_AT_SCAN"] <- "Age"
colnames(Abide_Clean_Seg_corr)[colnames(Abide_Clean_Seg_corr)=="SITE_ID2"] <- "Scanner Site"
colnames(Abide_Clean_Seg_corr)[colnames(Abide_Clean_Seg_corr)=="SEX"] <- "Sex"
names(Abide_Clean_Seg_corr) <- gsub("Vol", "Volume", names(Abide_Clean_Seg_corr))
names(Abide_Clean_Seg_corr) <- gsub("_", " ", names(Abide_Clean_Seg_corr))
names(Abide_Clean_Seg_corr) <- gsub("CortexVolume", "Cortex Volume", names(Abide_Clean_Seg_corr))

# Change Factor to Numeric 
Abide_Clean_Seg_corr$"Scanner Site" <- as.numeric(as.factor(Abide_Clean_Seg_corr$"Scanner Site")) 
Abide_Clean_Seg_corr$Group <- as.numeric(as.factor(Abide_Clean_Seg_corr$Group)) # 2 control 1 ASD 
Abide_Clean_Seg_corr$Sex <- as.numeric(as.factor(Abide_Clean_Seg_corr$Sex)) # 2 males 1 females 

Abide_Clean_Seg_corr_ASD <- Abide_Clean_Seg_corr%>% filter(Group == 1)
Abide_Clean_Seg_corr_ASD <- subset(Abide_Clean_Seg_corr_ASD, select = -c(Group))
Abide_Clean_Seg_corr_C <- Abide_Clean_Seg_corr%>% filter(Group == 2)
Abide_Clean_Seg_corr_C <- subset(Abide_Clean_Seg_corr_C, select = -c(Group))
### Table S5 - Correlations of IV in LMEMs Entire Sampel across Groups 

# TBV & FSIQ 
cor.test(Abide_Clean_Seg_corr_ASD$FSIQ, Abide_Clean_Seg_corr_ASD$"Total Brain Volume", method = "pearson" )
cor.test(Abide_Clean_Seg_corr_C$FSIQ, Abide_Clean_Seg_corr_C$"Total Brain Volume", method = "pearson" )
cor.test(Abide_Clean_Seg_corr$FSIQ, Abide_Clean_Seg_corr$"Total Brain Volume", method = "pearson" )

# TBV & Age 
cor.test(Abide_Clean_Seg_corr_ASD$Age, Abide_Clean_Seg_corr_ASD$"Total Brain Volume", method = "pearson" )
cor.test(Abide_Clean_Seg_corr_C$Age, Abide_Clean_Seg_corr_C$"Total Brain Volume", method = "pearson" )
cor.test(Abide_Clean_Seg_corr$Age, Abide_Clean_Seg_corr$"Total Brain Volume", method = "pearson" )

# TBV & Age2
cor.test(Abide_Clean_Seg_corr_ASD$Age2, Abide_Clean_Seg_corr_ASD$"Total Brain Volume", method = "pearson" )
cor.test(Abide_Clean_Seg_corr_C$Age2, Abide_Clean_Seg_corr_C$"Total Brain Volume", method = "pearson" )
cor.test(Abide_Clean_Seg_corr$Age2, Abide_Clean_Seg_corr$"Total Brain Volume", method = "pearson" )

# FSIQ & Age
cor.test(Abide_Clean_Seg_corr_ASD$Age, Abide_Clean_Seg_corr_ASD$FSIQ, method = "pearson" )
cor.test(Abide_Clean_Seg_corr_C$Age, Abide_Clean_Seg_corr_C$FSIQ, method = "pearson" )
cor.test(Abide_Clean_Seg_corr$Age, Abide_Clean_Seg_corr$FSIQ, method = "pearson" )

# FSIQ & Age2
cor.test(Abide_Clean_Seg_corr_ASD$Age2, Abide_Clean_Seg_corr_ASD$FSIQ, method = "pearson" )
cor.test(Abide_Clean_Seg_corr_C$Age2, Abide_Clean_Seg_corr_C$FSIQ, method = "pearson" )
cor.test(Abide_Clean_Seg_corr$Age2, Abide_Clean_Seg_corr$FSIQ, method = "pearson" )

####### PLOTING CORRELATIONS ACROSS ALL OBSERVED VARIABLES FOR MGCFA 
# Create correlation matrix of DV of interest 
Correlation_Matrix <- cor(Abide_Clean_Seg_corr, method = "pearson") # pearson default can add cor (x, method = c("pearson", "kendall", "spearman"))
Correlation_Matrix_ASD <- cor(Abide_Clean_Seg_corr_ASD, method = "pearson") # pearson default can add cor (x, method = c("pearson", "kendall", "spearman"))
Correlation_Matrix_C <- cor(Abide_Clean_Seg_corr_C, method = "pearson") # pearson default can add cor (x, method = c("pearson", "kendall", "spearman"))

# Correlation with p values
Correlation_Matrix_p_value <- rcorr(as.matrix(Correlation_Matrix))

#needed for Corr_Plot_Sig
res1 <- cor.mtest(Correlation_Matrix, conf.level = .95) # P values and CI lower and upper bounds at .95 
res1_ASD <- cor.mtest(Correlation_Matrix_ASD, conf.level = .95) # P values and CI lower and upper bounds at .95 
res1_C <- cor.mtest(Correlation_Matrix_C, conf.level = .95) # P values and CI lower and upper bounds at .95 

pdf("Figures/Corr_Plot_Sig.pdf") 
Corr_Plot_Sig <- corrplot(Correlation_Matrix, p.mat = res1$p, sig.level = .05, insig = "blank", type = "upper", 
                            tl.pos = "td",
                            title = "Correlations of all Subjects (n = 654)", 
                            mar=c(0,0,1,0), addCoef.col = "black",  number.cex = 0.5,
                            method = "circle", tl.cex = 0.6, tl.col = 'black', order = "hclust", diag = FALSE)
dev.off() 
##################################### ASD ########################################
pdf("Figures/Corr_Plot_Sig_ASD.pdf") 
Corr_Plot_Sig_ASD <- corrplot(Correlation_Matrix_ASD, p.mat = res1_ASD$p, sig.level = .05, insig = "blank", type = "upper", 
                              tl.pos = "td",
                              title = "Correlations in ASD Subjects (n = 302)", 
                              mar=c(0,0,1,0), addCoef.col = "black",  number.cex = 0.5,
                              method = "circle", tl.cex = 0.6, tl.col = 'black', order = "hclust", diag = FALSE)
dev.off() 
##################################### CONTROLS ########################################
pdf("Figures/Corr_Plot_Sig_C.pdf") 
Corr_Plot_Sig_C <- corrplot(Correlation_Matrix_C, p.mat = res1_C$p, sig.level = .05, insig = "blank", type = "upper", 
                              tl.pos = "td",
                              title = "Correlations in Control Subjects (n = 352)", 
                              mar=c(0,0,1,0), addCoef.col = "black",  number.cex = 0.5,
                              method = "circle", tl.cex = 0.6, tl.col = 'black', order = "hclust", diag = FALSE)
dev.off() 
