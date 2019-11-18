library(ggplot2)
library(dplyr)
library(lmerTest)

Abide_Clean_Seg_figures <- Abide_LMEM_log_Clean
Abide_Clean_Seg_figures$SITE_ID2 <- as.factor(Abide_Clean_Seg_figures$SITE_ID2)
Abide_Clean_Seg_figures$SITE_ID2 <- as.numeric(Abide_Clean_Seg_figures$SITE_ID2)
Abide_Clean_Seg_figures$Group <- Abide_Clean_Seg_figures$DX_GROUP

My_Theme = theme(
  axis.title.x = element_text(size = 26),
  axis.text.y = element_text(size = 26),
  axis.text.x = element_text(size = 26),
  axis.title.y = element_text(size = 26),
  legend.title = element_text(size = 26),
  legend.text = element_text(size = 26),
  strip.text.x = element_text(size = 26),
  strip.text.y = element_text(size = 26))

# TotalGrayVol
TGV_Site2 <-ggplot(Abide_Clean_Seg_figures, aes(x = SITE_ID2)) +
  geom_point(aes(x = as.factor(SITE_ID2), y = TotalGrayVol))+
  facet_grid(DX_GROUP~.)+
  geom_errorbar(data = Abide_Clean_Seg_figures%>%
                  group_by(SITE_ID2) %>% 
                  summarise(TotalGrayVol  = mean(TotalGrayVol)), aes(x = SITE_ID2, ymin= TotalGrayVol, ymax = TotalGrayVol)) +
  geom_hline(aes(yintercept = mean(TotalGrayVol)), lty = 2)+
  ylim(0, 1000000) + 
  ylab("Total Grey Matter Volume (mm3)") +
  xlab("Site ID") + 
  theme_classic() + My_Theme

pdf("Figures/TGV_Site2.pdf")
TGV_Site2
dev.off()

# CorticalWhiteMatterVol
CWM_Site2 <- ggplot(Abide_Clean_Seg_figures, aes(x = SITE_ID2)) +
  geom_point(aes(x = as.factor(SITE_ID2), y = CorticalWhiteMatterVol))+
  facet_grid(DX_GROUP~.) +
  geom_errorbar(data = Abide_Clean_Seg_figures%>%
                  group_by(SITE_ID2) %>% 
                  summarise(CorticalWhiteMatterVol  = mean(CorticalWhiteMatterVol)), aes(x = SITE_ID2, ymin= CorticalWhiteMatterVol, ymax = CorticalWhiteMatterVol)) +
  geom_hline(aes(yintercept = mean(CorticalWhiteMatterVol)), lty = 2)+
  ylim(200000, 750000) +
  ylab("Cortical White Matter Volume (mm3)") +
  xlab("Site ID") + 
  theme_classic()+ My_Theme

pdf("Figures/CWM_Site2.pdf")
CWM_Site2
dev.off()

# Hippocampus 
Hippocampus_Site2 <- ggplot(Abide_Clean_Seg_figures, aes(x = SITE_ID2)) +
  geom_point(aes(x = as.factor(SITE_ID2), y = Hippocampus))+
  facet_grid(DX_GROUP~.) +
  geom_errorbar(data = Abide_Clean_Seg_figures%>%
                  group_by(SITE_ID2) %>% 
                  summarise(Hippocampus  = mean(Hippocampus)), aes(x = SITE_ID2, ymin= Hippocampus, ymax = Hippocampus)) +
  geom_hline(aes(yintercept = mean(Hippocampus)), lty = 2)+
  ylim(0, 15000) + 
  ylab("Hippocampus Volume (mm3)") +
  xlab("Site ID") + 
  theme_classic()+ My_Theme

pdf("Figures/Hippocampus_Site2.pdf")
Hippocampus_Site2
dev.off()

# Amygdala 
Amygdala_Site2 <- ggplot(Abide_Clean_Seg_figures, aes(x = SITE_ID2)) +
  geom_point(aes(x = as.factor(SITE_ID2), y = Amygdala))+
  facet_grid(DX_GROUP~.) +
  geom_errorbar(data = Abide_Clean_Seg_figures%>%
                  group_by(SITE_ID2) %>% 
                  summarise(Amygdala  = mean(Amygdala)), aes(x = SITE_ID2, ymin= Amygdala, ymax = Amygdala)) +
  geom_hline(aes(yintercept = mean(Amygdala)), lty = 2)+
  ylim(0, 6000) + 
  xlab("Site ID") + 
  ylab("Amygdala Volume (mm3)") +
  theme_classic()+ My_Theme

pdf("Figures/Amygdala_Site2.pdf")
Amygdala_Site2
dev.off()

# Accumbens 
Accumbens_Site2 = ggplot(Abide_Clean_Seg_figures, aes(x = SITE_ID2)) +
  geom_point(aes(x = as.factor(SITE_ID2), y = Accumbens_Area))+
  facet_grid(DX_GROUP~.) +
  geom_errorbar(data = Abide_Clean_Seg_figures%>%
                  group_by(SITE_ID2) %>% 
                  summarise(Accumbens_Area  = mean(Accumbens_Area)), aes(x = SITE_ID2, ymin= Accumbens_Area, ymax = Accumbens_Area)) +
  geom_hline(aes(yintercept = mean(Accumbens_Area)), lty = 2)+
  ylim(0, 3000) + 
  ylab("Accumbens Area Volume (mm3)") + 
  xlab("Site ID") + 
  theme_classic()+ My_Theme

pdf("Figures/Accumbens_Site2.pdf")
Accumbens_Site2
dev.off()

# Thalamus 
Thalamus_Site2 <- ggplot(Abide_Clean_Seg_figures, aes(x = SITE_ID2)) +
  geom_point(aes(x = as.factor(SITE_ID2), y = Thalamus_Proper))+
  facet_grid(DX_GROUP~.) +
  geom_errorbar(data = Abide_Clean_Seg_figures%>%
                  group_by(SITE_ID2) %>% 
                  summarise(Thalamus_Proper  = mean(Thalamus_Proper)), aes(x = SITE_ID2, ymin= Thalamus_Proper, ymax = Thalamus_Proper)) +
  geom_hline(aes(yintercept = mean(Thalamus_Proper)), lty = 2)+
  ylim(7500, 25000) + 
  xlab("Site ID") + 
  ylab("Thalamus Volume (mm3)") +
  theme_classic()+ My_Theme

pdf("Figures/Thalamus_Site2.pdf")
Thalamus_Site2
dev.off()

# Caudate 
Caudate_Site2 <- ggplot(Abide_Clean_Seg_figures, aes(x = SITE_ID2)) +
  geom_point(aes(x = as.factor(SITE_ID2), y = Caudate))+
  facet_grid(DX_GROUP~.) +
  geom_errorbar(data = Abide_Clean_Seg_figures%>%
                  group_by(SITE_ID2) %>% 
                  summarise(Caudate  = mean(Caudate)), aes(x = SITE_ID2, ymin= Caudate, ymax = Caudate)) +
  geom_hline(aes(yintercept = mean(Caudate)), lty = 2)+
  ylim(1500, 15000) + 
  xlab("Site ID") + 
  ylab("Caudate Volume (mm3)") +
  theme_classic()+ My_Theme

pdf("Figures/Caudate_Site2.pdf")
Caudate_Site2
dev.off()

# Putamen 
Putamen_Site2 <- ggplot(Abide_Clean_Seg_figures, aes(x = SITE_ID2)) +
  geom_point(aes(x = as.factor(SITE_ID2), y = Putamen))+
  facet_grid(DX_GROUP~.) +
  geom_errorbar(data = Abide_Clean_Seg_figures%>%
                  group_by(SITE_ID2) %>% 
                  summarise(Putamen  = mean(Putamen)), aes(x = SITE_ID2, ymin= Putamen, ymax = Putamen)) +
  geom_hline(aes(yintercept = mean(Putamen)), lty = 2)+
  ylim(0, 20000) + 
  xlab("Site ID") + 
  ylab("Putamen Volume (mm3)") +
  theme_classic()+ My_Theme

pdf("Figures/Putamen_Site2.pdf")
Putamen_Site2
dev.off()

# Pallidum 
Pallidum_Site2 <- ggplot(Abide_Clean_Seg_figures, aes(x = SITE_ID2)) +
  geom_point(aes(x = as.factor(SITE_ID2), y = Pallidum))+
  facet_grid(DX_GROUP~.) +
  geom_errorbar(data = Abide_Clean_Seg_figures%>%
                  group_by(SITE_ID2) %>% 
                  summarise(Pallidum  = mean(Pallidum)), aes(x = SITE_ID2, ymin= Pallidum, ymax = Pallidum)) +
  geom_hline(aes(yintercept = mean(Pallidum)), lty = 2)+
  ylim(500, 6000) + 
  xlab("Site ID") + 
  ylab("Pallidum Volume (mm3)") +
  theme_classic()+ My_Theme

pdf("Figures/Pallidum_Site2.pdf")
Pallidum_Site2
dev.off()

# AGE_AT_SCAN 
AGE_AT_SCAN_Site2<- ggplot(Abide_Clean_Seg_figures, aes(x = SITE_ID2)) +
  geom_point(aes(x = as.factor(SITE_ID2), y = AGE_AT_SCAN))+
  facet_grid(DX_GROUP~.) +
  geom_errorbar(data = Abide_Clean_Seg_figures%>%
                  group_by(SITE_ID2) %>% 
                  summarise(AGE_AT_SCAN  = mean(AGE_AT_SCAN)), aes(x = SITE_ID2, ymin= AGE_AT_SCAN, ymax = AGE_AT_SCAN)) +
  geom_hline(aes(yintercept = mean(AGE_AT_SCAN)), lty = 2)+
  ylim(0, 30) + 
  xlab("Site ID") +
  ylab("Age at Scan (years)") + 
  theme_classic()+ My_Theme

pdf("Figures/AGE_AT_SCAN_Site2.pdf")
AGE_AT_SCAN_Site2
dev.off()

# FIQ2 
FIQ2_Site2<- ggplot(Abide_Clean_Seg_figures, aes(x = SITE_ID2)) +
  geom_point(aes(x = as.factor(SITE_ID2), y = FIQ2))+
  facet_grid(DX_GROUP~.) +
  geom_errorbar(data = Abide_Clean_Seg_figures%>%
                  group_by(SITE_ID2) %>% 
                  summarise(FIQ2  = mean(FIQ2)), aes(x = SITE_ID2, ymin= FIQ2, ymax = FIQ2)) +
  geom_hline(aes(yintercept = mean(FIQ2)), lty = 2)+
  ylim(50, 150) + 
  xlab("Site ID") + 
  ylab("Full Scale Intelligence Quotient (FSIQ)") + 
  theme_classic()+ My_Theme

pdf("Figures/FIQ2_Site2.pdf")
FIQ2_Site2
dev.off()

# FIQ by TBV scatterplot 
FIQ2_TBV<- ggplot(Abide_Clean_Seg_figures, aes(y=FIQ2, x= Total_Brain_Vol, shape=Group, color=Group)) +
  geom_point(size = 2)+ 
  geom_smooth(method=lm,fullrange=TRUE, aes(fill=Group)) +
  scale_shape_manual(values = c(20, 20)) +
  scale_color_manual(values=c('orangered3','navyblue')) + 
  ylim(70, 130) + 
  xlab("Total Brain Volume (mm3)") + 
  ylab("Full Scale Intelligence Quotient (FSIQ)") + 
  theme_classic() + My_Theme
FIQ2_TBV

pdf("Figures/FIQ2_TBV.pdf")
FIQ2_TBV
dev.off()

Abide_Clean_Seg_figures$Sex <- Abide_Clean_Seg_figures$SEX
CWM_Age_Group_Sex <- ggplot(Abide_Clean_Seg_figures, aes(x=AGE_AT_SCAN, y=CorticalWhiteMatterVol, shape=SEX, color=SEX)) +
  geom_point(size = 2)+ 
  geom_smooth(method=lm,fullrange=TRUE, aes(fill=SEX)) +
  scale_shape_manual(values = c(20, 20)) +
  scale_color_manual(values=c('orangered3','navyblue')) + 
  xlab("Age At Scan (Years)") + 
  ylab("Cortical White Matter Volume (mm3)") + 
  xlim(5,30)+
  facet_grid(Group~.)+
  theme_classic() + My_Theme

pdf("Figures/CWM_Age_Group_Sex.pdf")
CWM_Age_Group_Sex
dev.off()

# The palette with black:

Count_Site_ID_Group_Bar.plot <- ggplot(Abide_Clean_Seg_figures, aes(x = SITE_ID2, fill = Group)) +
  geom_bar(position = "stack") +
  xlab("Site ID") + 
  ylab("Number of Participants") + 
  theme_classic()+ My_Theme

pdf("Figures/Count_Site_ID_Group.pdf")
Count_Site_ID_Group_Bar.plot
dev.off()

# SAME AS ZHANG : Count by Group by Sex By Site_ID2 
Abide_Clean_Seg_figures <- Abide_Clean_Seg
Abide_Clean_Seg_figures$Group = factor(Abide_Clean_Seg_figures$DX_GROUP, levels=c('Control','ASD'))
Abide_Clean_Seg_figures$SITE_ID2 <- factor(Abide_Clean_Seg_figures$SITE_ID2 )
Abide_Clean_Seg_figures$SITE_ID2 <- as.integer(Abide_Clean_Seg_figures$SITE_ID2)
Abide_Clean_Seg_figures$Sex <- Abide_Clean_Seg_figures$SEX

Count_Site_ID_Group_Sex_Bar.plot = ggplot(Abide_Clean_Seg_figures, aes(x = SITE_ID2, fill = Sex)) +
  geom_bar() + facet_grid(Group~.)+
  ylab("Number of Participants") + 
  ylab("Site ID") +
  scale_x_discrete(Abide_Clean_Seg_figures$SITE_ID2) + 
  theme_classic()+ My_Theme

pdf("Figures/Count_Site_ID_Group_Sex_Bar.plot.pdf")
Count_Site_ID_Group_Sex_Bar.plot
dev.off()

# SAME AS ZHANG : Count by Group by Sex By Site_ID2 
Abide_Clean_Seg_figures$SITE_ID2 <- as.factor(Abide_Clean_Seg_figures$SITE_ID2)
Abide_Clean_Seg_figures$DSM <- as.factor(Abide_Clean_Seg_figures$DSM_IV_TR)

Count_Site_ID_DSM_Group_Bar.plot = ggplot(Abide_Clean_Seg_figures, aes(x = SITE_ID2, fill = DSM)) +
  geom_bar() +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#0072B2", "#D55E00", "#CC79A7")) + 
  ylab("Number of Participants") + 
  ylab("Site ID") +
  scale_x_discrete(Abide_Clean_Seg_figures$SITE_ID2) + 
  theme_classic() + My_Theme

pdf("Figures/Count_Site_ID_DSM_Group_Bar.plot.pdf")
Count_Site_ID_DSM_Group_Bar.plot
dev.off()

jpeg("Figures/All_Zhang_Figures1.jpeg")
TGV_Site2
CWM_Site2
Hippocampus_Site2
Amygdala_Site2
Accumbens_Site2
Thalamus_Site2
Caudate_Site2
Putamen_Site2
Pallidum_Site2
AGE_AT_SCAN_Site2
FIQ2_Site2
FIQ2_TBV
CWM_Age_Group_Sex
Count_Site_ID_Group_Bar.plot
Count_Site_ID_Group_Sex_Bar.plot
Count_Site_ID_DSM_Group_Bar.plot
dev.off()

