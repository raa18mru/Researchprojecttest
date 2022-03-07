#plot
rm(list=ls())

# Plots data of resistance rates 0, 0.01, 0.02, 0.03, 0.04, 0.05

library(tidyverse)

#read in both loops and not-loops

res0 <- read.csv("resistance_investigation/aggFolder/F_aggregate_run001_patch001.csv")
res01 <- read.csv("resistance_investigation/aggFolder/F_aggregate_run002_patch001.csv")
res10 <- read.csv("resistance_investigation/aggFolder/F_aggregate_run003_patch001.csv")
res25 <- read.csv("resistance_investigation/aggFolder/F_aggregate_run004_patch001.csv")
res50 <- read.csv("resistance_investigation/aggFolder/F_aggregate_run005_patch001.csv")
res100 <- read.csv("resistance_investigation/aggFolder/F_aggregate_run006_patch001.csv")

# Make a total column and a resistance column

res0 <- mutate(res0, resistance = "0") %>% 
  mutate(res0, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

res01 <- mutate(res01, resistance = "0.01") %>% 
  mutate(res01, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

res10 <- mutate(res10, resistance = "0.10") %>% 
  mutate(res10, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

res25 <- mutate(res25, resistance = "0.25") %>% 
mutate(res25, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
         WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

res50 <- mutate(res50, resistance = "0.50") %>% 
mutate(res50, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
         WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

res100 <- mutate(res100, resistance = "1") %>% 
mutate(res100, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
         WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)


res0 <- summarise(res0, resistance, total, Time)
res01 <- summarise(res01, resistance, total, Time)
res10 <- summarise(res10, resistance, total, Time)
res25 <- summarise(res25, resistance, total, Time)
res50 <- summarise(res50, resistance, total, Time)
res100 <- summarise(res100, resistance, total, Time)
#  set <1 to 0

totals <- rbind(res0, res01, res10, res25, res50, res100)
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
  scale_colour_discrete(name = "resistance Frequency", labels = c("0%", "1%", "10%", "25%","50%", "100%"))

ggsave(device = "png", filename = "resistance investigation", path="plots")