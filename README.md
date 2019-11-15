# Subcortical-Allometry-in-Autism


This repository contains the scripts used to analyses the [Autism Brain Imaging Data Exchange I](http://fcon_1000.projects.nitrc.org/indi/abide/abide_I.html) (ABIDE I) dataset by Williams et al. (in prep). The study preregistration is available [here](https://osf.io/6bjcg). The data was obtained from [Lefebvre and colleagues (2015)](https://www.sciencedirect.com/science/article/pii/S0006322315001018?via%3Dihub). 

All figures generated in a script will be stored in the Figures folder and all csv files in the CSV folder. 

1. The data cleaning, inclusion, exclusion and transformations script is available in Scripts/Final_Clean_Data.R
2. Descriptive statistics scripts are available in Scripts/Demographics folder. 
3. Scaling Coefficent Estimations: The scripts used to examine whether scaling coefficients of each local volume with Total brain volume is isometric, hypoallometric, or hyperallometric are in the Scripts/Allometry_Isometry folder. 
4. MGCFA: The scripts used to conduct the MGCFAs are in the Scripts/MGCFA folder. 
5. LMEMs: The scripts used to conduct (i) the corresponding LMEMs (corresponding to the MGCFA analyses), (ii) the LMEMs to examining age, sex, and or full scale intelligence quotient (FSIQ) effects on neuroanatomical group differences, and (iii) the effect of TBV adjustment technique on the results from the exploratory analyses are in the Scripts/LMEM folder. 
6. Replication of [Zhang and colleagues (2018)](https://www.cambridge.org/core/journals/psychological-medicine/article/revisiting-subcortical-brain-volume-correlates-of-autism-in-the-abide-dataset-effects-of-age-and-sex/CB66FFA7347DBE59C446BA66B1BA1A66): The scripts used to conduct the corresponding LMEMs (corresponding to the MGCFA analyses) and the LMEMs to explore age, sex, and or full scale intelligence quotient (FSIQ) effects are in the Scripts/LMEM folder. 

