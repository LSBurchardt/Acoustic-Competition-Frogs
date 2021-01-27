# Acoustic Competition in Frogs
# author: Lara S. Burchardt
# collaborators: A. Filer, B. van Rendsburg (University of Queensland)
# project to compare call rates between WSF and ESF, when in competition


# 00: Load Packages --------------------------------------------------------------------------

library(tidyverse)
library(PMCMR)
library(PMCMRplus)

# 01: Load Data ------------------------------------------------------------------------------

# IOI Data

ioi <- read_delim("all_IOIs_WSF_ESF.csv", delim = ";")
ioi <- ioi %>% 
  as.data.frame() %>% 
  mutate(group = as.factor(group))

rhythm <- read_delim("all_rhythms_frog.csv", delim = ";")
rhythm <- rhythm %>% 
  as.data.frame() %>% 
  mutate(group = as.factor(group))
  


#02: Stats -----------------------------------------------------------------------------------

#ioi data
summary(ioi)

# WSF iois: max = 55.69; n = 1298
# ESF iois: max = 38.86, n =  876
# WSF when ESF present iois: max = 24.22, n =  847
# ESF when WSF present iois: max = 22.18, n = 945


kruskal.test(ioi~group, data = ioi)
test1 <- posthoc.kruskal.dunn.test(ioi$ioi, ioi$group, p.adjust.method = "bonferroni") #bonferroni most conservative correction; other p.adjust methods: "holm", "none", "BH" ...
test1

#rhythm data

kruskal.test(rhythm~group, data = rhythm)
test2 <- posthoc.kruskal.dunn.test(rhythm$rhythm, rhythm$group, p.adjust.method = "bonferroni")
test2

# 03: Histograms -----------------------------------------------------------------------------

#quick and dirty histograms with 'hist'

hist(ioi$`WSF `)
hist(ioi$ESF)
hist(ioi$WSF_ESF_present)
hist(ioi$ESF_WSF_present)

#quick and dirty boxplot

boxplot(ioi~group, data = ioi)
boxplot(rhythm~group, data = rhythm)

