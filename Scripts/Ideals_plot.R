rm(list=ls())

library(tidyverse)

#read in both loops and not-loops

tarecsv <- read.csv("Ideal_TARE/F_aggregate_run001_Patch001.csv")
ridlcsv <- read.csv("Ideal_RIDL/F_Aggregate_run001_Patch001.csv")
# Make a total column and a technology column


ridltotals <- mutate(ridlcsv, technology= "RIDL") %>% 
  mutate(ridlcsv, total= WW+WR+RR)
taretotals <- mutate(tarecsv, technology= "TARE") %>% 
  mutate(tarecsv, total= WWWW+WHWB+WWWB)
ridltotals <- summarise(ridltotals, technology,total,Time)
taretotals <- summarise(taretotals, technology,total,Time)
totals <- rbind(ridltotals, taretotals)

# Make ggplot

ggplot(data = totals, mapping = aes(x = Time, y = total, colour = technology)) +
  geom_line(size=2) + theme_light() +
  labs(title= "The Impact of TARE Sex-Switching and RIDL on Female Medfly in Ideal parameters")+ 
  xlab("Time (days)") +
  ylab("Number of Females") +
  scale_color_brewer(palette="Dark2") + 
  scale_x_continuous(expand = c(0, 0)) 
