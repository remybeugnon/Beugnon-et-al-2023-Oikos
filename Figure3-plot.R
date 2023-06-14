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
#   - ggeffects_1.0.2
#   - ggforce_0.3.2
#   - ggplotify_0.0.5
#   - ggpubr_0.4.0
#   - ggimage_0.2.8
#---------------------------------------------
rm(list = ls())
set.seed(1232)

#### 0. Packages ####
libs <- c('tidyverse', 
          'ggeffects','eulerr',
          'ggforce',"ggplotify",
          'ggpubr', 'ggimage')
invisible(lapply(libs, library, character.only = T))

#### 1. Data ####
load(file = "Figure3.RData")

#### 2. Plots ####
thm = theme(axis.title.x = element_blank()) + 
  theme_bw()

#### 2.1 Litterfall ####
pred.mod.fall = ggpredict(mod.fall, terms = 'neigh.sp.rich')

signif = summary(mod.fall)$coefficients[2,4] 

signif.code = if(signif < 0.001){
  'p-value < 0.001 ***'
} else {
  paste0('p-value = ',round(signif, 3), if(signif < 0.01){
    ' **'
  } else if(signif >= 0.01 & signif < 0.05){
    " *"
  } else if(signif >= 0.05 & signif < 0.1){
    " ."
  } else {' '})
}

p.fall =
  ggplot(data = pred.mod.fall, 
         aes(x = x , y = predicted)) + 
  geom_ribbon(data = pred.mod.fall, 
              aes(x = x , 
                  ymin = conf.low, ymax = conf.high), 
              color = 'gray', alpha = 0.2) + 
  geom_line(data = pred.mod.fall, 
            aes(x = x , y = predicted)) + 
  geom_jitter(data = df, 
              aes(y = fall, x = neigh.sp.rich)) + 
  annotate(geom = 'text',
           x = 1, y = 300, 
           label = signif.code, hjust = 0) +
  labs(x = "", y = "Litterfall [g/m2]") + 
  scale_x_continuous(trans = 'log', breaks = c(1,2,4,8)) + 
  thm

#### 2.2 Litter species richness ####
pred.mod.lit.rich = ggpredict(mod.lit.rich, terms = 'neigh.sp.rich')

signif = summary(mod.lit.rich)$coefficients[2,4] 

signif.code = if(signif < 0.001){
  'p-value < 0.001 ***'
} else {
  paste0('p-value = ',round(signif, 3), if(signif < 0.01){
    ' **'
  } else if(signif >= 0.01 & signif < 0.05){
    " *"
  } else if(signif >= 0.05 & signif < 0.1){
    " ."
  } else {' '})
}

p.lit.rich =
  ggplot(data = pred.mod.lit.rich, 
         aes(x = x , y = predicted)) + 
  geom_abline(intercept = 0, slope = 1, 
              lty = 2 , color = 'gray') + 
  geom_ribbon(data = pred.mod.lit.rich, 
              aes(x = x ,
                  ymin = conf.low, ymax = conf.high), 
              color = 'gray', alpha = 0.2) + 
  geom_line(data = pred.mod.lit.rich, 
            aes(x = x , y = predicted)) + 
  geom_jitter(data = df, 
              aes(y = lit.rich, x = neigh.sp.rich)) + 
  annotate(geom = 'text', 
           x = 1, y = 9.5, 
           label = signif.code, hjust = 0) +
  labs(x = "", y = "Litterfall species richness") + 
  scale_x_continuous(trans = 'log', breaks = c(1,2,4,8)) + 
  scale_y_continuous(trans = 'log', breaks = c(1,2,4,8)) + 
  thm

#### 2.3. Litterfall drivers ####
p.litterfall = 
  ggarrange(
    p.fall, p.lit.rich,
    ncol = 2,
    align = 'hv'
  )

#### 2.4. Variance partitioning plot ####
#### 2.4.1 Decomposability ####
#### 2.4.1.1 Decomposability C ####
p.cg.c =
  ggplot(data = NULL, 
         aes(x0 = 0, y0 = 0)) + 
  annotate(geom = 'text', 
           x = 0 , y = 0, 
           label = bquote(bold("No effect of litter spe. richness"))) + 
  theme_void() + 
  labs(subtitle = paste0("  C loss [%]: R2 = 0%")) + 
  theme(plot.title.position = 'plot')

#### 2.4.1.2 Decomposability N ####
a = round(summary(mod.n.cg)$adj.r.squared * 100, digits = 1)

p.cg.n =
  ggplot(data = NULL, 
         aes(x0 = 0, y0 = 0)) + 
  geom_circle(aes(r = 1), 
              fill =  "#288928", 
              alpha = .5) +
  annotate(geom = 'text', 
           x = 0 , y = 0, 
           label = bquote(bold('Litter rich.') ~ .(a) ~ '%')) + 
  theme_void() +
  lims(x = c(-5,5)) + 
  labs(subtitle = paste0("N loss [%]: R2 = ", 
                         round(summary(mod.n.cg)$adj.r.squared * 100), 
                         "%")) + 
  theme(plot.title.position = 'plot')

#### 2.4.2 Microbial decomposition ####
#### 2.4.2.1 Microbial decomposition C ####
a.mic.c = c(mod.c.micro.part$part$indfract[1:7 ,'Adj.R.square']) %>% unlist
a.mic.c[a.mic.c<0] = 0
a.mic.c = (a.mic.c * 100) %>% round(.,digits = 1)

names(a.mic.c) = NULL
names(a.mic.c) = c("A", "B", "C", "A&B", "B&C", "A&C", "A&B&C")

VennDiag <- euler(a.mic.c)
p.mic.c <-  plot(VennDiag, counts = TRUE,  
                 quantities = list(type = c("counts")),
                font = 1, cex = .5, alpha = 0.5,
                fill=c("#D0A159", "#74DB69", "#288928"), 
                color = 'black',
                labels = c('Decomp.', 'Fall', 'Litter rich.')) %>% 
  as.ggplot + 
  labs(subtitle = paste0("  C loss [%]: R2 = ", 
                         round(100 - mod.c.micro.part$part$indfract[8,3] * 100), 
                         "%"))

#### 2.4.2.2 Microbial decomposition N ####
a.mic.n = c(mod.n.micro.part$part$indfract[1:7 ,'Adj.R.square']) %>% unlist
a.mic.n[a.mic.n<0] = 0
a.mic.n = (a.mic.n * 100) %>% round(.,digits = 1)

names(a.mic.n) = NULL
names(a.mic.n) = c("A", "B", "C", "A&B", "B&C", "A&C", "A&B&C")

VennDiag <- euler(a.mic.n)

p.mic.n <-  plot(VennDiag, counts = TRUE,  
                 quantities = list(type = c("counts")),
                 font = 1, cex = .5, alpha = 0.5,
                 fill=c("#D0A159", "#74DB69", "#288928"), 
                 color = 'black',
                 labels = c('Decomp.', 'Fall', 'Litter rich.')) %>% 
  as.ggplot() +
  labs(subtitle = paste0("N loss [%]: R2 = ", 
                         round(100 - mod.n.micro.part$part$indfract[8,3] * 100), 
                         "%"))

#### 2.4.2 Decomposition ####
#### 2.4.3.1 Decomposition C ####
a.c = c(mod.c.part$part$indfract[1:7 ,'Adj.R.square']) %>% unlist
a.c[a.c<0] = 0
a.c = (a.c * 100)  %>% round(.,digits = 1)

names(a.c) = NULL
names(a.c) = c("A", "B", "C", "A&B", "B&C", "A&C", "A&B&C")

VennDiag <- euler(a.c)

p.c <-  plot(VennDiag, counts = TRUE, 
             quantities = list(type = c("counts")),
                 font = 1, cex = .5, alpha = 0.5,
                 fill=c("#A67505", "#74DB69", "#288928"), 
                 color = 'black',
                 labels = c('Micro. Decomp.', 'Fall', 'Litter rich.'))  %>% 
  as.ggplot() + 
  labs(subtitle = paste0("  C loss [%]: R2 = ", 
                         round(100 - mod.c.part$part$indfract[8,3] * 100), 
                         "%"))

#### 2.4.3.2 Microbial decomposition N ####
a.n = c(mod.n.part$part$indfract[1:7 ,'Adj.R.square']) %>% unlist
a.n[a.n<0] = 0
a.n = (a.n * 100)  %>% round(.,digits = 1)

names(a.n) = NULL
names(a.n) = c("A", "B", "C", "A&B", "B&C", "A&C", "A&B&C")

VennDiag <- euler(a.n)
p.n <-  plot(VennDiag, counts = TRUE,  
             quantities = list(type = c("counts")),
                 font = 1, cex = .5, alpha = 0.5,
                 fill=c("#A67505", "#74DB69", "#288928"), 
                 color = 'black',
                 labels = c('Micro. Decomp.', 'Fall', 'Litter rich.')) %>% 
  as.ggplot() +
  labs(subtitle = paste0("N loss [%]: R2 = ", 
                         round(100 - mod.n.part$part$indfract[8,3] * 100), 
                         "%"))


p.varpart =
  ggarrange(
    ggarrange(p.cg.c, p.cg.n) + theme(plot.background = element_rect(fill = alpha("#D0A159",.5), color = NA)), 
    ggarrange(p.mic.c, p.mic.n) + theme(plot.background = element_rect(fill = alpha("#A67505",.5), color = NA)), 
    ggarrange(p.c, p.n) + theme(plot.background = element_rect(fill = alpha("#AA6939",.5), color = NA)), 
            align = 'hv',
            heights = c(.2,.4,.4),
            ncol = 1, nrow = 3)

p.part.micro =
  ggplot(data = NULL) + 
  geom_violin(data = df, fill = 'gray', aes(x = 'C.loss', y = (100*(C.loss_Mi1/C.loss_Ma1)))) +
  geom_boxplot(data = df, width = .25, aes(x = 'C.loss', y = (100*(C.loss_Mi1/C.loss_Ma1)))) +
  geom_violin(data = df, fill = 'gray', aes(x = 'N.loss', y = (100*(N.loss_Mi1/N.loss_Ma1)))) + 
  geom_boxplot(data = df, width = .25, aes(x = 'N.loss', y = (100*(N.loss_Mi1/N.loss_Ma1)))) + 
  geom_hline(yintercept = 50, lty = 2) + 
  geom_hline(yintercept = 100, lty = 2) + 
  labs(x = '', y = expression(paste(frac('Microbial decomposition', 'Total decomposition'), '   (%)'))) +
  scale_x_discrete(labels = c("C loss", "N loss")) + 
  lims(y = c(0 , 130)) + 
  theme_bw()

d = data.frame(x=0, 
               y=0, 
               image="~/TreeDi_Project/Results/Decomposition/3-paper-analyses/3-2-1-3_sem.svg")

p.sem = ggplot(d, aes(x, y, 
              image = image)) + 
  geom_image(size = 1) + 
  coord_fixed() + 
  theme(plot.background = element_blank(),
        panel.background = element_rect(fill = 'white')) +
  theme_void()

#### 3. Figure 3 ####
fig2 =
  ggarrange(
    ggarrange(
      ggarrange(
                p.fall + theme(plot.margin = margin(.5,.2,0,0, "cm")), 
                p.lit.rich + theme(plot.margin = margin(.5,0,0,.2, "cm")),
                ncol = 2,
                align = 'v'
              ) %>% 
        annotate_figure(.,
                        bottom = text_grob("Neighborhood species richness")) + 
        theme(plot.margin = margin(.5,.7,.1,.6, "cm")), 
        p.part.micro + theme(plot.margin = margin(1,1,1,1, "cm")), 
        heights = c(.5,.5), 
        nrow = 2,
        labels = c("A", "B")) ,
    p.sem,
    ncol = 2,
    labels = c('','C'),
    widths = c(.4,.5))

ggsave(plot = fig2,
       filename = "Figure3.png",
       height = 6, width = 10)

