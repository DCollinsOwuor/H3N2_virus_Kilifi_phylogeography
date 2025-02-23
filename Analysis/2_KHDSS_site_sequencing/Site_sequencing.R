# Title: "Plotting sequenced influenza A H3N2 virus by site".
# Author: "D. Collins Owuor"
# Date:   "June 22, 2020"
# R version 1.3.1093 
# Clear data in memory, load necessary packages, and load data.

### Clear workspace:
rm(list=ls())


### Install and load required packages:
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("tidyverse")
BiocManager::install("reshape2")
BiocManager::install("extrafont")

library(tidyverse)
library(reshape2)
library(extrafont)


### Load data:
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

df <- read.csv("KHDSS_site_sequencing.csv", sep = ",", header = T, stringsAsFactors = T)

names(df)

pdf("Figure_3.2.pdf", width = 4, height = 3)

### How do the sequenced influenza H3N2 viruses vary over time?:
plot_data <- df %>%
  melt(id.vars="site") %>%
  rename(Month_collected=variable, No_cases=value) %>%
  mutate(Month_collected=factor(Month_collected, levels = c("Dec_15", "Jan_16", "Feb_16", 
                                                          "Mar_16", "Apr_16", "May_16", 
                                                          "Jun_16", "Jul_16", "Aug_16", 
                                                          "Sep_16", "Oct_16", "Nov_16", 
                                                          "Dec_16"))) %>%
  mutate(site=factor(site, levels = c("Junju", "Pingilikani","Chasimba", "Mavueni", 
                                    "Jaribuni", "Mtondia", "Sokoke", "Ngerenya", 
                                    "Matsangoni"))) %>%
  mutate(No_cases=replace(No_cases, No_cases==0, NA))

ggplot(plot_data, aes(x=Month_collected, y=site, size=(No_cases)*0.6)) +
  geom_point()+
  #scale_color_manual(values=c("#FF0000", "#00FF00", "#0000FF", "#00FFFF","#FF00FF","#808080", "#800000","#808000", "#800080"))+
  labs(x="Month", y="Health facility") +
  scale_size_identity() +
  theme_classic() +
  theme(axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 19),
        #legend.position = "none",
        legend.key.size = unit(0.45, "cm"),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.text = element_text(size = 11),
        legend.title =element_blank(),
        legend.box.background = element_blank()) +
  guides(color = FALSE)
dev.off()
