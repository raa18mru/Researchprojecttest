rm(list=ls())

# Plots data of resistance rates 0, 0.01, 0.02, 0.03, 0.04, 0.05

library(tidyverse)

#read in both loops and not-loops

res0 <- read.csv("output/resistance_investigation_allele/aggFolder/F_aggregate_run001_patch001.csv")
ressingle <- read.csv("output/resistance_investigation_allele/aggFolder/F_aggregate_run002_patch001.csv")
reshomo <- read.csv("output/resistance_investigation_allele/aggFolder/F_aggregate_run003_patch001.csv")

# Make a total column and a resistance column

res0 <- mutate(res0, resistance = "0") 

ressingle <- mutate(ressingle, resistance = "single") 

reshomo <- mutate(reshomo, resistance = "double") 


#  set <1 to 0

totals <- rbind(res0, ressingle, reshomo)
totals <- mutate(totals, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
                   WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)
totals <- summarise(totals, resistance, total, Time)
totals <- mutate(totals, total = ifelse(total < 1, 0, total))
totals <- mutate(totals, Time = Time - 60)

# Make ggplot

ggplot(data = totals, mapping = aes(x = Time, y = total, colour = resistance)) +
  geom_line(size = 0.5) + theme_minimal() +
  labs(title = "The Impact of Cas-9 resistance on Medfly Suppression by TARE Sex-Switching")+ 
  xlab("Time (days)") +
  ylab("Number of Females") +
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(expand = c(0, 0), limits = c(0,365)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5,300)) +
  scale_colour_discrete(name = "Resistance Frequency", labels = c("No resistance", "Single Allele 1% rate", "Homozygosity required 1% resistance"))

ggsave(filename = "resistance allele investigation.png", path="plots")