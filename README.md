# Adjusting for Allometric Scaling in ABIDE I Challenges Subcortical Differences in Autism

This repository contains the scripts used to analyses the [Autism Brain Imaging Data Exchange I](http://fcon_1000.projects.nitrc.org/indi/abide/abide_I.html) (ABIDE I) dataset by Williams et al. (in prep). The study preregistration is available [here](https://osf.io/6bjcg). The data was obtained from [Lefebvre and colleagues (2015)](https://www.sciencedirect.com/science/article/pii/S0006322315001018?via%3Dihub). 

All scripts are in the Scripts folder, figures generated in a script will be stored in the Figures folder and all csv files in the CSV folder. Further description in each folder can be found in the Readme.md of that folder. 

## 1. Data Cleaning & Descriptive Statistics 
Data cleaning, inclusion, exclusion and transformations script is available in [Scripts/Final_Clean_Data.R](https://github.com/Camzcamz/Subcortical-Allometry-in-Autism/blob/master/Scripts/Final_Clean_Data.R).
Descriptive statistics scripts are available in [Scripts/Demographics](https://github.com/Camzcamz/Subcortical-Allometry-in-Autism/tree/master/Scripts/Demographics).

## 2. Scaling Coefficients
The scripts used to examine whether scaling coefficients of each local volume with Total brain volume is isometric, hypoallometric, or hyperallometric are in the [Scripts/Allometry_Isometry](https://github.com/Camzcamz/Subcortical-Allometry-in-Autism/tree/master/Scripts/Allometry_Isometry). 

## 3. Multiple Group Confirmatory Analyses (MGCFA) 
The scripts used to conduct the MGCFAs are in the [Scripts/MGCFA](https://github.com/Camzcamz/Subcortical-Allometry-in-Autism/tree/master/Scripts/MGCFA). 

## 4. Linear Mixed Effects Models (LMEMs) 
The scripts used to conduct (i) the corresponding LMEMs (corresponding to the MGCFA analyses), (ii) the LMEMs examining whether neuroanatomical group differences depende on age, sex, and or full scale intelligence quotient (FSIQ), and (iii) the effect of TBV adjustment technique on the results from the exploratory analyses are in the [Scripts/LMEM](https://github.com/Camzcamz/Subcortical-Allometry-in-Autism/tree/master/Scripts/LMEMs). 

## 4. Replication of[Zhang and colleagues (2018)](https://www.cambridge.org/core/journals/psychological-medicine/article/revisiting-subcortical-brain-volume-correlates-of-autism-in-the-abide-dataset-effects-of-age-and-sex/CB66FFA7347DBE59C446BA66B1BA1A66).
The scripts used to replicate the results and the [supplemental figures from Zhang and colleagues (2018)](https://www.cambridge.org/core/journals/psychological-medicine/article/revisiting-subcortical-brain-volume-correlates-of-autism-in-the-abide-dataset-effects-of-age-and-sex/CB66FFA7347DBE59C446BA66B1BA1A66#fndtn-supplementary-materials) are in the [Scripts/Replication](https://github.com/Camzcamz/Subcortical-Allometry-in-Autism/tree/master/Scripts/Replication).
