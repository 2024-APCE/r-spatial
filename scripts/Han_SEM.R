
#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis woody cover 
# Paper:
# browseURL("https://drive.google.com/file/d/1aasI7uIFj7zcjuu0A3QhPDU4X-oQtcnx/view?usp=sharing")

# restore libraries

rm(list = ls()) # clear environment

library(tidyverse)
# load the lavaan library
library(lavaan)


# dataset:
browseURL("https://docs.google.com/spreadsheets/d/1wk3UTAN7Cp7ZeoB0wpfW2C2eE_VoyKnJQpJ0Zrjk3yM/edit?usp=sharing")

# read the data from the google docs link:
SEMdata<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTOcfomHnvwYrglRSyVwSWujgWH9VbczV4GKMS2xihFRg8Rdj4U5LkpBbe_DJJltRFV6Y8uQwI9HUUz/pub?gid=898690457&single=true&output=csv")
SEMdata

names(SEMdata)
# standardize all variables to mean 0 and standard deviation 1
Anderson2007std <- Anderson2007 |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
Anderson2007std
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(Anderson2007 %>% select(RES_LHU,BIOMASS,FIRE_FRQ,
                                            NMS,LF_N),
                    stars = T, ellipses = F)
psych::pairs.panels(Anderson2007std %>% select(RES_LHU,BIOMASS,FIRE_FRQ,
                                               NMS,LF_N),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
multreg_std<-lm(LF_N~RES_LHU+BIOMASS+FIRE_FRQ+NMS,data=Anderson2007std)
summary(multreg_std)
# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
Leaf_N_model<-'LF_N~BIOMASS+RES_LHU+FIRE_FRQ+NMS
               BIOMASS~FIRE_FRQ+RES_LHU
               NMS~FIRE_FRQ+RES_LHU'
Leaf_N_model
Leaf_N_fit<-lavaan::sem(Leaf_N_model,data=Anderson2007std)
# show the model results
summary(Leaf_N_fit,standardized=T,fit.measures=T,rsquare=T)

# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR

# visualise the model

# also explore the models as shown in fig 5b and 5c of the Anderson2007 paper
# so repeat the model for leaf P content



