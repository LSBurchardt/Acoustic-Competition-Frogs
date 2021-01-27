# visualizations for: Acoutsic COmpetition in Frogs
# collaborators: Alannah Filer, Berndt Janse van Rensburg, University of Queensland
# author code: Lara S. Burchardt
########################################################################################

#00: packages -------------------------------------------------------------------------

library(tidyverse)
library(effsize)

#01: data -----------------------------------------------------------------------------

frog_rhythm <- read_delim("frogs_all_rhythms_WSF_ESF.csv", delim = ";")

frog_ioi <- read_delim("all_IOIs_WSF_ESF.csv", delim = ";")
frog_ioi <- frog_ioi %>% 
  as.data.frame() %>% 
  mutate(group = as.factor(group))

frog_beat_development <- read_delim("beat_development_in_chorus.csv", delim = ";")

# 02: Plots----------------------------------------------------------------------------

# 02a: boxplots--------------------------------

# boxplot rhythm ESF vs. WSF 

frog_rhythm %>%
  group_by(species) %>% 
  ggplot(aes(x = species, y = rhythm_ioi))+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(shape= 1, size= 0.6, width = 0.2)+
  labs(x="Species", y= "Rhythm [Hz]")+
  #scale_fill_manual(name = "Species",
                    #labels = c("Eastern Sedge Frog", "Wellum Sedge Frog"),
                    #values = c("#117733", "#6699CC"))+
  theme_dark()+
  theme(legend.position = "none",
    axis.title=element_text(size=14,face="bold"), 
    axis.text = element_text(size = 12), 
    plot.title=element_text(size=14, face="bold"))

# boxplot rhythm both species, both situations


frog_rhythm %>%
  #group_by(species, situation) %>% 
  ggplot(aes(x = species, y = rhythm_ioi, fill = situation, color = situation))+
  geom_boxplot(outlier.colour = NA)+
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape = 15, size = 0.6) +
  #geom_jitter(shape= 1, size= 0.6, width = 0.1)+
  labs(x="Species", y= "Rhythm [Hz]")+
  scale_color_manual(name = "Situation",
                     labels = c("alone", "competitor present"),
                     values = c("black", "black"))+
  scale_fill_manual(name = "Situation",
    labels = c("alone", "competitor present"),
    values = c("#117733", "#6699CC"))+
  theme_dark()+
  theme(#legend.position = "none",
        axis.title=element_text(size=14,face="bold"), 
        axis.text = element_text(size = 12), 
        plot.title=element_text(size=14, face="bold"))

# boxplot ioi species, situation
frog_ioi %>%
  #group_by(species, situation) %>% 
  ggplot(aes(x = species, y = ioi, fill = situation, color = situation))+
  geom_violin()+
  #geom_point(position = position_jitterdodge(dodge.width = 0.9), shape = 15, size = 0.6) +
  #geom_jitter(shape= 1, size= 0.6, width = 0.1)+
  labs(x="Species", y= "IOI [sec]")+
  scale_color_manual(name = "Situation",
                    labels = c("alone", "competitor present"),
                 values = c("black", "black"))+
scale_fill_manual(name = "Situation",
                    labels = c("alone", "competitor present"),
                    values = c("#117733", "#6699CC"))+
  theme_dark()+
  scale_y_log10(limits = c(0.1,100))+
  theme(#legend.position = "none",
    axis.title=element_text(size=14,face="bold"), 
    axis.text = element_text(size = 12), 
    plot.title=element_text(size=14, face="bold"))

# 02b: Histograms --------------------------------------------

wsf <- frog_ioi %>% 
  filter(species == "WSF") %>% 
  select(ioi)

wsf_hist <- wsf %>% 
  ggplot(aes(x=ioi))+
  geom_histogram(aes(y=stat((count)/sum(stat(count))*100)),
                 binwidth = 0.5,
                 stat = "bin",
                 color = "white", fill = "darkblue")+
  ylab("Percentage[%]")+
  xlab("IOI duration [sec]")+
  theme_dark(base_size = 14)+
  scale_x_continuous(limits= c(0,20))+
  scale_y_continuous(limits = c(0,40))

  ########ESF###########
  
  esf <- frog_ioi %>% 
    filter(species == "ESF") %>% 
    select(ioi)
  
esf_hist <- esf %>% 
    ggplot(aes(x=ioi))+
    geom_histogram(aes(y=stat((count)/sum(stat(count))*100)),
                   binwidth = 0.5,
                   stat = "bin",
                   color = "white", fill = "darkblue")+
    ylab("Percentage[%]")+
    xlab("IOI duration [sec]")+
    theme_dark(base_size = 14)+
    scale_x_continuous(limits= c(0,20))+
    scale_y_continuous(limits = c(0,40))


cowplot::plot_grid(esf_hist, wsf_hist, align = "h", ncol = 2, rel_heights = c(0.7, 0.3))

#03: stats ----------------------------------------

mean_rhythm <- frog_rhythm %>% 
  group_by(species) %>% 
  summarise_at("rhythm_ioi", list(min,max, mean))

# effect size of mean difference, beat development within chorus

effectsize <- cohen.d(frog_beat_development$`#1`, frog_beat_development$`#2`, paired = TRUE, )



