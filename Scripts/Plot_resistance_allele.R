rm(list=ls())

# Plots data of resistance rates 0, 0.01, 0.02, 0.03, 0.04, 0.05

library(tidyverse)

#read in both loops and not-loops

res0 <- read.csv("resistance_investigation_allele/aggFolder/F_aggregate_run001_patch001.csv")
ressingle <- read.csv("resistance_investigation_allele/aggFolder/F_aggregate_run002_patch001.csv")
reshomo <- read.csv("resistance_investigation_allele/aggFolder/F_aggregate_run003_patch001.csv")

# Make a total column and a technology column

res0 <- mutate(res0, resistance = "0") %>% 
  mutate(res0, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

ressingle <- mutate(ressingle, resistance = "single") %>% 
  mutate(ressingle, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

reshomo <- mutate(reshomo, resistance = "double") %>% 
  mutate(reshomo, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)



res0 <- summarise(res0, resistance, total, Time)
ressingle <- summarise(ressingle, resistance, total, Time)
reshomo <- summarise(reshomo, resistance, total, Time)

#  set <1 to 0

totals <- rbind(res0, ressingle, reshomo)
totals <- mutate(totals, total = ifelse(total < 1, 0, total))
totals <- mutate(totals, Time = Time - 60)

# Prepare male data 
# maletotals <- mutate(malecsv, technology= "Homozygous Males") %>% 
# mutate(malecsv, total= RR)
# maletotals <- summarise(maletotals, technology, total, Time)

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
  scale_colour_discrete(name = "Resistance Frequency", labels = c("No resistance", "Single Allele 1% rate", "Homozygosity required 1% resistance"))

ggsave(device = "png", filename = "resistance allele investigation", path="plots")