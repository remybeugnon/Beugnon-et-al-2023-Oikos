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
#   - vegan_2.5-7 
#   - lavaan_0.6-7
#---------------------------------------------
rm(list = ls())

#### 0. Packages ####
libs <- c('tidyverse', 
          'vegan', 
          'lavaan')
invisible(lapply(libs, 
                 library, 
                 character.only = T))

#### 1. Data ####
df = read.csv(file = "df.csv")

#### 2. Analyses ####
#### 2.1. Part of microbial decomposition in total decomposition ####
ggplot(data = NULL) + 
  geom_violin(data = df, 
              aes(x = 1, 
                  y = (100*(C.loss_Mi1/C.loss_Ma1)))) +
  geom_violin(data = df, 
              aes(x = 2, 
                  y = (100*(N.loss_Mi1/N.loss_Ma1)))) + 
  geom_hline(yintercept = 50, lty = 2) + 
  geom_hline(yintercept = 100, lty = 2) + 
    labs(x = '', y = ' Microbial decomposition / Total decomposition (%)') + 
  lims(y = c(0 , 130)) + 
  theme_bw()

#### 2.2. Tree species richness effect on litterfall ####
#### 2.2.1 Tree species richness effect on litterfall biomass ####
mod.fall = lm(data = df, 
              formula = 'fall ~ log(neigh.sp.rich)')
# summary(mod.fall)

#### 2.2.2 Tree species richness effect on litter species richness ####
mod.lit.rich = lm(data = df, 
                  formula = 'log(lit.rich) ~ log(neigh.sp.rich)')

#### 2.3. SEM model ####
df.2 = df %>% 
  select(neigh.sp.rich, fall, lit.rich, 
         C.loss_Ma1, C.loss_Mi1, C.loss_CG,
         N.loss_Ma1, N.loss_Mi1, N.loss_CG) %>%
  mutate(log.neigh.sp.rich = log(neigh.sp.rich)) %>%
  mutate(log.lit.rich = log(lit.rich)) %>%
  apply(., 2, scale) %>%
  data.frame()

form.sem = 
  '
  C.loss_Ma1 ~ C.loss_Mi1 + fall + log.lit.rich
  N.loss_Ma1 ~ N.loss_Mi1 + fall + log.lit.rich
  C.loss_Ma1 ~~ N.loss_Ma1

  C.loss_Mi1 ~ C.loss_CG + fall + log.lit.rich
  N.loss_Mi1 ~ N.loss_CG + fall + log.lit.rich
  C.loss_Mi1 ~~ N.loss_Mi1
  
  C.loss_CG ~ log.lit.rich
  N.loss_CG ~ log.lit.rich
  C.loss_CG ~~ N.loss_CG
  
  fall ~ log.neigh.sp.rich
  log.lit.rich ~ log.neigh.sp.rich
  fall ~~ log.lit.rich
'

mod.sem = sem(model = form.sem, 
              data = df.2)

#### 3. saving results ####
save.image(file = "Figure3.RData")
