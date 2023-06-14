#---------------------------------------------
# platform       x86_64-w64-mingw32          
# arch           x86_64                      
# os             mingw32                     
# system         x86_64, mingw32             
# status                                     
# major          4                           
# minor          1.0                         
# year           2021                        
# month          05                          
# day            18                          
# svn rev        80317                       
# language       R                           
# version.string R version 4.1.0 (2021-05-18)
# nickname       Camp Pontanezen 
#---------------------------------------------
# Packages:
#   - tidyverse_1.3.0
#     - ggplot2_3.3.3
#     - dplyr_1.0.4
#     - purrr_0.3.4
#     - tidyr_1.1.3
#     - stringr_1.4.0
#---------------------------------------------
rm(list = ls())

#### 0. Packages ####
libs <- c('tidyverse')
invisible(lapply(libs, 
                 library, 
                 character.only = T))

#### 1. Data ####
df = read.csv(file =  "df.csv")

#### 2. Analyses ####
#### 2.1. Tree diversity effects on C and N loss ####
mod.c = lm(data = df, 
           formula = "C.loss_Ma1 ~ log(neigh.sp.rich)")

summary(mod.c)

mod.n = lm(data = df, 
           formula = "N.loss_Ma1 ~ log(neigh.sp.rich)")

summary(mod.n)

#### 2.2. Tree diversity effects on microbial C and N loss ####
mod.c.micro = lm(data = df, 
                 formula = "C.loss_Mi1 ~ log(neigh.sp.rich)")

summary(mod.c.micro)

mod.n.micro = lm(data = df, 
                 formula = "N.loss_Mi1 ~ log(neigh.sp.rich)")

summary(mod.n.micro)

#### 2.3. Tree diversity effects on litter decomposability ####
mod.c.cg = lm(data = df, 
              formula = "C.loss_CG ~ log(neigh.sp.rich)")

summary(mod.c.cg)

mod.n.cg = lm(data = df, 
              formula = "N.loss_CG ~ log(neigh.sp.rich)")

summary(mod.n.cg)

#### 3. Save models ####

save.image(file = "Fig2.RData")
