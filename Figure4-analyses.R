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
libs <- c('tidyverse', 
          'vegan', 
          'lavaan')
invisible(lapply(libs, library, character.only = T))

#### 1. Data ####
df = read.csv(file =  "df-3.csv")
df = df %>% distinct() %>% filter(C.loss > 50)
df[, 6:25] = df[, 6:25] %>% apply(., 2, scale)
df$CP = df$ini.C / df$ini.P %>% scale()

df.2 = read.csv(file =  "df-2.csv")

#### 2. Analyses ####
#### 2.1. Decomposability drivers ####
# Final models
mod.c = lm(C.loss ~ ini.SLA + ini.LDMC + ini.C + CN +  CP + var.x + log(lit.rich), 
           data = df) %>% 
  step(direction = 'both', trace = F) 

# summary(mod.c)
# performance::check_model(mod.c)

mod.n = lm(N.loss ~ ini.SLA + ini.LDMC + ini.C + CN +  CP + var.x + log(lit.rich), 
                       data = df) %>% 
  step(direction = 'both', trace = F)

# Plot C and N loss drivers
df.results = data.frame(
  var = c('ini.SLA','ini.LDMC', 'ini.C','CN','CP','var.x', 'log(lit.rich)') %>% 
    factor
  ) %>%
  left_join(., 
            summary(mod.c)$coefficients %>%
              data.frame() %>%
              select(est.c = Estimate, se.c = `Std..Error`, p.c = `Pr...t..`) %>%
              mutate(var = row.names(.)) %>%
              mutate(int.pos.c = est.c + 1.96 * se.c) %>%
              mutate(int.neg.c = est.c - 1.96 * se.c),
            by = 'var') %>%
  left_join(., 
            summary(mod.n)$coefficients %>%
              data.frame() %>%
              select(est.n = Estimate, se.n = `Std..Error`, p.n = `Pr...t..`) %>%
              mutate(var = row.names(.)) %>%
              mutate(int.pos.n = est.n + 1.96 * se.n) %>%
              mutate(int.neg.n = est.n - 1.96 * se.n),
            by = 'var')

df.results$var = df.results$var %>% 
  factor(., 
         levels =  c('log(lit.rich)','var.x','CP','CN','ini.C', 'ini.LDMC','ini.SLA'))
df.results$sign.c = ifelse(df.results$p.c < 0.05, 'sign', 'sign-')
df.results$sign.n = ifelse(df.results$p.n < 0.05, 'sign', 'sign-')

df.results[, c(5,6,10,11)] = 
  df.results[, c(5,6,10,11)] %>% 
  apply(., 2, function(x) ifelse(x > 4, Inf, x)) %>% 
  apply(., 2, function(x) ifelse(x < -4, -Inf, x))

p = 
  ggplot(data = df.results, 
         aes(x = 1:8, y = est.c)) +
  geom_hline(aes(yintercept = 0), 
             lty = 2) + 
  geom_point(data = df.results, 
             aes(x = as.numeric(var) + .15, 
                 y = est.c)) + 
  geom_errorbar(data = df.results, 
                aes(x = as.numeric(var) + .15, 
                    ymin = int.neg.c, 
                    ymax = int.pos.c, 
                    lty = sign.c), 
                width = .1) + 
  geom_point(data = df.results, 
             aes(x = as.numeric(var) - .15, 
                 y = est.n), 
             color = 'red') + 
  geom_errorbar(data = df.results, 
                aes(x = as.numeric(var) - .15, 
                    ymin = int.neg.n, 
                    ymax = int.pos.n, 
                    lty = sign.n), 
                width = .1, color = 'red') +
  scale_x_continuous(breaks = 1:7,
                     labels = c('1' = 'Litter spe. richnness', 
                                '2' = 'Chemistry coupling', 
                                '3' = '[C]/[P]', 
                                '4' = '[C]/[N]' ,
                                '5' = '[C]', 
                                '6' = "LDMC", "7" = 'SLA')) +
  labs(y = 'Estimate', x = '') + 
  lims(y = c(-4,4)) +
  coord_flip() + 
  theme_classic() + 
  theme(legend.position = 'none', 
        panel.grid = element_blank()) 

ggsave(p,
       filename = "decomposability-drivers-trait.png",
       height = 12, width = 7.5, unit = 'cm')

#### 2.2. Litterfall drivers ####
mf = "log.litter.biomass.area ~ log.biomass + dist + LDMC + SLA + C + N + (1|litter.biomass)" %>%
  as.formula() %>%
  lmerTest::lmer(data = df.2, formula = .) %>%
  lmerTest::step() %>%
  lmerTest::get_model()

MuMIn::r.squaredGLMM(mf)

df.p = summary(mf)$coefficients %>% 
  data.frame() %>%
  filter(row.names(.) != "(Intercept)") %>%
  mutate(variable = row.names(.) %>% 
           factor(., levels = c("N", "C", "SLA", "LDMC", "dist", "log.biomass"))) %>%
  mutate(lty = if_else(condition = Pr...t..<0.05,
                       true = '1',
                       false = '2'))

p = 
  df.p %>%
  ggplot(data = ., 
         aes(x = as.numeric(variable), 
             y = Estimate)) + 
  geom_hline(yintercept = 0) + 
  geom_point() + 
  geom_errorbar(aes(ymin = Estimate - 1.96 * Std..Error, 
                    ymax = Estimate + 1.96 * Std..Error, 
                    lty = lty), 
                width = .1) + 
  labs(y = "Estimate", x = "") +
  scale_x_continuous(breaks = c(1:6), 
                   labels = c('1' = '[N]', 
                              '2' = '[C]', 
                              '3' = 'SLA', 
                              '4' = "LDMC" ,
                              '5' = "1/dist", 
                              '6' = "log(biomass)")) + 
  lims(y = c(-1,1)) + 
  coord_flip() + 
  theme_classic() + 
  theme(legend.position = 'none', 
        plot.background = element_rect(fill = NA),
        panel.background = element_rect(fill = NA))

ggsave(p, 
       filename = "litterfall-drivers-trait.png",
       height = 10, width = 7.5, unit = 'cm')

#### 3. Saving results ####
save.image(file = "3-2-paper-scripts/Figure4.RData")
