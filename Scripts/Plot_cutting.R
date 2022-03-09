rm(list=ls())

# Plots data of cutting rates 0, 0.2, 0.4, 0.6, 0.8, 1, when maternal deposition cutting rate is 0

library(tidyverse)

#read in both loops and not-loops

cut0 <- read.csv("Output/cutting_investigation/aggFolder/F_aggregate_run001_patch001.csv")
cut02 <- read.csv("Output/cutting_investigation/aggFolder/F_aggregate_run002_patch001.csv")
cut04 <- read.csv("Output/cutting_investigation/aggFolder/F_aggregate_run003_patch001.csv")
cut06 <- read.csv("Output/cutting_investigation/aggFolder/F_aggregate_run004_patch001.csv")
cut08 <- read.csv("Output/cutting_investigation/aggFolder/F_aggregate_run005_patch001.csv")
cut1 <- read.csv("Output/cutting_investigation/aggFolder/F_aggregate_run006_patch001.csv")
# Make a total column and a technology column

cut0 <- mutate(cut0, cutting = "0") 

cut02 <- mutate(cut02, cutting = "0.2") 

cut04 <- mutate(cut04, cutting = "0.4") 

cut06 <- mutate(cut06, cutting = "0.6") 

cut08 <- mutate(cut08, cutting = "0.8") 

cut1 <- mutate(cut1, cutting = "1") 

totals <- rbind(cut0, cut02, cut04, cut06, cut08, cut1) 
totals <- mutate(totals, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
         WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)
totals <- summarise(totals, cutting, total, Time)
totals <- mutate(totals, total = ifelse(total < 1, 0, total)) 
totals <- mutate(totals, Time = Time - 60)

# Make ggplot

ggplot(data = totals, mapping = aes(x = Time, y = total, colour = cutting)) +
  geom_line(size = 0.5) + theme_light() + 
  xlab("Time (days)") +
  ylab("Number of Females") +
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(expand = c(0, 0), limits = c(0,365)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5,300)) +
  scale_colour_discrete(name = "Cutting Frequency")

ggsave(filename = "cutting investigation.png", path="plots")
