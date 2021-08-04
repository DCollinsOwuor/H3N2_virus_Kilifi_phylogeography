# Title: "Plotting H3N2 virus genetic groups by KHDSS site".
# Author: "D. Collins Owuor"
# Date:   "January 20, 2020"
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

df <- read.csv("Genetic_groups_by_location.csv", sep = ",", header = T, stringsAsFactors = T)

names(df)

pdf("Figure_3.3.pdf", width = 7, height = 5)

### How do the sequenced H3N2 virus genetic groups vary by KHDSS site?:
plot_data <- df %>%
  melt(id.vars="clade") %>%
  rename(location=variable, No_cases=value) %>%
  mutate(location=factor(location, levels = c("Chasimba",	 "Sokoke",	 "Pingilikani",	 
                                            "Ngerenya",	 "Mtondia",	 "Mavueni",	 
                                            "Matsangoni"	, "Junju"	, "Jaribuni"))) %>%
  mutate(clade=factor(clade, levels = c("3C.2A", "3C.2A1b","3C.2A2", "3C.2A3"))) %>%
  mutate(No_cases=replace(No_cases, No_cases==0, NA))

ggplot(plot_data, aes(x=clade, y=location, size=(No_cases)*0.9)) +
  geom_point(aes(color=location)) +
  scale_color_manual(values = c( "#FF0000","#654321","#FF00FF","#808080","#FFA500","#0000FF",
                               "#00FF00","#00FFFF","#000000")) + 
  labs(x="Genetic group", y="Location") +
  scale_size_identity() +
  theme_classic() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        #legend.position = "none",
        legend.key.size = unit(0.45, "cm"),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.text = element_text(size = 12),
        legend.title =element_blank(),
        legend.box.background = element_blank()) +
  guides(color = FALSE)
dev.off()
