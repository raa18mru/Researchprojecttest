#read in both loops and not-loops
rm(list=ls())
install.packages("tidyverse")

library(tidyverse)

tarecsv <- read.csv("medfly_tare_no_loop/F_aggregate_run001_Patch001.csv")
ridlcsv <- read.csv("medfly_ridl/F_Aggregate_run001_Patch001.csv")
# Make a total column and a tehcnology column
ridltotals <- mutate(ridlcsv, technology= "ridl")
ridltotals <- mutate(ridlcsv, total= WW+WR+RR)
taretotals <- mutate(tarecsv, technology= "tare")
taretotals <- mutate(tarecsv, total= WWWW+WHWB+WWWB)
ridltotals <- summarise(ridltotals, technology,total,Time)
taretotals <- summarise(taretotals, technology,total,Time)
totals <- rbind(ridltotals, taretotals)


# Make ggplot

ggplot(data = totals, mapping = aes(x = Time, y = total, colour = technology)) +
  geom_point()