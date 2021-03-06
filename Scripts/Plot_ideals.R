rm(list=ls())

library(tidyverse)

#read in both loops and not-loops

tarecsv <- read.csv("Output/Ideal_Tare/F_Aggregate_Run001_Patch001.csv")
ridlcsv <- read.csv("Output/Ideal_RIDL/F_Aggregate_run001_Patch001.csv")
malecsv <- read.csv("Output/Ideal_RIDL/M_run001_Patch001.csv")
# Make a total column and a technology column


ridltotals <- mutate(ridlcsv, technology= "RIDL") %>% 
  mutate(ridlcsv, total= WW+WR+RR)
taretotals <- mutate(tarecsv, technology= "TARE") %>% 
  mutate(tarecsv, total= WWWW+WHWB+WWWB) 
ridltotals <- summarise(ridltotals, technology, total, Time)
taretotals <- summarise(taretotals, technology, total, Time)
#  set <1 to 0
taretotals <- mutate(taretotals, total = ifelse(total < 1, 0, total))

totals <- rbind(ridltotals, taretotals)
totals <- mutate(totals, Time = Time - 60)

# Prepare male data 
maletotals <- mutate(malecsv, technology= "Homozygous Males") %>% 
  mutate(malecsv, total= RR)
maletotals <- summarise(maletotals, technology, total, Time)
maletotals <- mutate(maletotals, Time = Time - 60)
# Make ggplot

ggplot(data = totals, mapping = aes(x = Time, y = total, colour = technology, )) +
  geom_line(data = maletotals, size = 2, alpha = 0.6, colour = "pink") +
  geom_line(size = 2) + theme_light() +
  labs(title = "The Impact of TARE Sex-Switching and RIDL on Female Medfly in Ideal parameters")+ 
  xlab("Time (days)") +
  ylab("Number of Females") +
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(expand = c(0, 0), limits = c(0,365)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5,520)) +
  scale_colour_discrete(name = "Technology")

ggsave(filename = "Ideals.png", path="plots")