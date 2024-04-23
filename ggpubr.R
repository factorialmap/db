#packages
library(tidyverse)  
library(ggpubr)

#Info about this data
?ToothGrowth

ToothGrowth %>% 
  mutate(dose = as.factor(dose)) %>% 
  ggbarplot(x = "supp",
            y = "len",
            add = "mean_se",
            fill = "supp",
            facet.by = "dose",
            strip.position = "bottom")+
  stat_compare_means(label = "p.signif")+
  theme_minimal()

ToothGrowth %>% 
  ggbarplot(x = "dose", 
            y = "len",
            add = "mean_se",
            fill = "supp",
            position = position_dodge(0.8))+
  stat_compare_means(aes(group = supp),
                     label = "p.signif",
                     label.y = 29)

ToothGrowth %>% 
  ggline(x = "dose", 
         y = "len",
         add = "mean_se",
         color = "supp") +
  stat_compare_means(aes(group = supp), 
                     label = "p.signif", 
                     label.y = c(16, 25, 29))

ToothGrowth %>% 
  ggline(x = "dose", 
         y = "len",
         add = "mean_se",
         color = "supp") +
  stat_compare_means(aes(group = supp), 
                     label = "p.format", 
                     label.y = c(16, 25, 29))

#check
ToothGrowth_dose1 <- ToothGrowth %>% filter(dose == 1)
compare_means(len~supp, data = ToothGrowth_dose1)

#source
#http://sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/