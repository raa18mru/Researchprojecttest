rm(list=ls())

# Plots data of resistance rates 0, 0.01, 0.02, 0.03, 0.04, 0.05

library(tidyverse)

#read in both loops and not-loops

res0 <- read.csv("output/resistance_investigation/aggFolder/F_aggregate_run001_patch001.csv")
res01 <- read.csv("output/resistance_investigation/aggFolder/F_aggregate_run002_patch001.csv")
res10 <- read.csv("output/resistance_investigation/aggFolder/F_aggregate_run003_patch001.csv")
res25 <- read.csv("output/resistance_investigation/aggFolder/F_aggregate_run004_patch001.csv")
res50 <- read.csv("output/resistance_investigation/aggFolder/F_aggregate_run005_patch001.csv")
res100 <- read.csv("output/resistance_investigation/aggFolder/F_aggregate_run006_patch001.csv")

# Make a total column and a resistance column

res0 <- mutate(res0, resistance = "0")

res01 <- mutate(res01, resistance = "0.01") 

res10 <- mutate(res10, resistance = "0.10") 

res25 <- mutate(res25, resistance = "0.25")

res50 <- mutate(res50, resistance = "0.50") 

res100 <- mutate(res100, resistance = "1") 



totals <- rbind(res0, res01, res10, res25, res50, res100)
totals <- mutate(totals,total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
                   WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)
totals <- summarise(totals, resistance, total, Time)
totals <- mutate(totals, total = ifelse(total < 1, 0, total))
totals <- mutate(totals, Time = Time - 60)

# Make ggplot

ggplot(data = totals, mapping = aes(x = Time, y = total, colour = resistance)) +
  #  geom_line(data = maletotals, size = 2, alpha = 0.6, colour = "pink") +
  geom_line(size = 0.5) + theme_light() +
  labs(title = "The Impact of Cas-9 resistance on Medfly Suppression by TARE Sex-Switching")+ 
  xlab("Time (days)") +
  ylab("Number of Females") +
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(expand = c(0, 0), limits = c(0,365)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5,300)) +
  scale_colour_discrete(name = "resistance Frequency")

ggsave(filename = "resistance investigation.png", path="plots")