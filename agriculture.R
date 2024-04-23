
# goals         -------------------------------------------------------------------
"exercises of statistical in agriculture"
"using base R"
"using ggpubr"
"using gtsummary"

# package       -----------------------------------------------------------------
library(tidyverse)
library(gtsummary)
library(ggpubr)
library(broom)
library(labelled)
library(scales)
library(glue)

# yield         -------------------------------------------------------------------

#generate some data
yld <- c(2280, 2690, 2080, 2820, 1340, 2080, 2480, 2420, 2150, 1880)
yld_diff <- yld - 1810
data_yield <-data.frame(yld, yld_diff)


#show means base R and tidyverse
colMeans(data_yield)
data_yield %>% summarise(across(everything(), mean))

#t.test using differenced
t.test(yld_diff)
tidy(t.test(yld_diff))

#t.test MU sample mean usign base R and broom
t.test(yld, mu = 1810)
tidy(t.test(yld, mu = 1810))

#using grader than
t.test(yld, alternative = "greater", mu = 1810)


# t.te 2 sample ---------------------------------------------------------

#get sugarbeet data
base_sugarbeet <- read.csv("http://rstats4ag.org/data/sugarbeet.csv")


#data wrangling
data_sugarbeet <-
  base_sugarbeet %>% 
  janitor::clean_names() %>%    #var vanmes to lower
  mutate(yield = yield * 2.24)  #convert data to ton/ha


#calculate the mean between groups
data_sugarbeet %>% 
  summarise(mean(yield), .by  = type)

#t.test using base R
t.test(yield~type, data = data_sugarbeet)
tidy(t.test(yield~type, data = data_sugarbeet))

#using ggpubr
data_sugarbeet %>% 
  ggboxplot(x = "type",
            y = "yield",
            fill = "type")+
  stat_compare_means(method = "t.test")

#using gt summary
data_sugarbeet %>% 
  tbl_summary(by = type,
              digits = yield ~ 1) %>% 
  add_p() %>% 
  add_q(method = "bonferroni")


theme_gtsummary_compact()

data_sugarbeet %>% 
  tbl_summary(by = type,
              digits = yield ~ 1) %>% 
  add_difference(
    test = list(all_continuous()~ "t.test"),
    pvalue_fun = ~style_pvalue(.x, digits = 3)) %>% 
  bold_labels() %>% 
  bold_p()

# one-way anova -----------------------------------------------------------

#herbicide trial conducted in Wyoming durring three dif years
#5 different herbicides + nontreated + handweeded = 7 treatments

data_beans <- 
  read.csv("http://rstats4ag.org/data/FlumiBeans.csv") %>% 
  janitor::clean_names() %>% 
  mutate(population_4wk = population_4wk * 2.47) %>% 
  mutate(across(c(year, block, treatment), as.factor)) 

data_beans_2009 <- 
  read.csv("http://rstats4ag.org/data/FlumiBeans.csv") %>% 
  janitor::clean_names() %>% 
  mutate(population_4wk = population_4wk * 2.47) %>% 
  mutate(across(c(year, block, treatment), as.factor)) %>% 
  filter(year == 2009)
  
data_beans_2009
data_beans %>% sjPlot::view_df()
summary(data_beans)

#anova test using base R and tidy
mdl_anova_beans <- aov(population_4wk ~ block + treatment, data = data_beans_2009)
tidy(aov(population_4wk ~ block + treatment, data = data_beans_2009))


#check normality
shapiro.test(mdl_anova_beans$residuals)
stem(mdl_anova_beans$residuals)
qqnorm(mdl_anova_beans$residuals)
qqline(mdl_anova_beans$residuals)

#homogeneity of variance test
bartlett.test(data_beans_2009$population_4wk, data_beans_2009$treatment)
fligner.test(data_beans_2009$population_4wk, data_beans_2009$treatment)


#compare means
data_beans%>% 
  ggplot(aes(x = fct_reorder(treatment, population_4wk),
             y = population_4wk,
             fill = treatment))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~year)+
  coord_flip()+
  stat_compare_means(method = "anova",
                     label = "p.signif",
                     label.x = 7,
                     label.y = 35000)+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  xlab("")


#compare means with rain cloud
data_beans_2009 %>% 
  ggplot(aes(x = fct_reorder(treatment, population_4wk),
             y = population_4wk,
             fill = treatment))+
  stat_halfeye(adjust =.5, 
               width =.6, 
               justification = -.2,
               .width = 0.5,
               point_color = NA)+
  geom_boxplot(width = .10,
               outlier.color = NA)+
  #geom_half_point(side = "l", range_scale = .1, alpha = .3)+
  coord_flip()+
  stat_compare_means(method = "anova",
                     label.x = 7,
                     label.y = 35000)+
  theme_classic()+
  xlab("")


compare_means(population_4wk~treatment, data = data_beans)


https://rstats4ag.org/anova.html
https://rpkgs.datanovia.com/ggpubr/reference/stat_compare_means.html


?stat_compare_means()


