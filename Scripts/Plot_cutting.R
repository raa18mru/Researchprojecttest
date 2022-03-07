rm(list=ls())

# Plots data of cutting rates 0, 0.2, 0.4, 0.6, 0.8, 1, when maternal deposition cutting rate is 0

library(tidyverse)

#read in both loops and not-loops

cut0 <- read.csv("cutting_investigation/aggFolder/F_aggregate_run001_patch001.csv")
cut02 <- read.csv("cutting_investigation/aggFolder/F_aggregate_run002_patch001.csv")
cut04 <- read.csv("cutting_investigation/aggFolder/F_aggregate_run003_patch001.csv")
cut06 <- read.csv("cutting_investigation/aggFolder/F_aggregate_run004_patch001.csv")
cut08 <- read.csv("cutting_investigation/aggFolder/F_aggregate_run005_patch001.csv")
cut1 <- read.csv("cutting_investigation/aggFolder/F_aggregate_run006_patch001.csv")
# Make a total column and a technology column

cut0 <- mutate(cut0, cutting = "0") %>% 
  mutate(cut0, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

cut02 <- mutate(cut02, cutting = "0.2") %>% 
  mutate(cut02, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

cut04 <- mutate(cut04, cutting = "0.4") %>% 
  mutate(cut04, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

cut06 <- mutate(cut06, cutting = "0.6") %>% 
  mutate(cut06, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

cut08 <- mutate(cut08, cutting = "0.8") %>% 
  mutate(cut08, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

cut1 <- mutate(cut1, cutting = "1") %>% 
  mutate(cut1, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)


cut0 <- summarise(cut0, cutting, total, Time)
cut02 <- summarise(cut02, cutting, total, Time)
cut04 <- summarise(cut04, cutting, total, Time)
cut06 <- summarise(cut06, cutting, total, Time)
cut08 <- summarise(cut08, cutting, total, Time)
cut1 <- summarise(cut1, cutting, total, Time)
#  set <1 to 0

totals <- rbind(cut0, cut02, cut04, cut06, cut08, cut1)
totals <- mutate(totals, total = ifelse(total < 1, 0, total))
totals <- mutate(totals, Time = Time - 60)

# Prepare male data 
# maletotals <- mutate(malecsv, technology= "Homozygous Males") %>% 
# mutate(malecsv, total= RR)
# maletotals <- summarise(maletotals, technology, total, Time)

# Make ggplot

ggplot(data = totals, mapping = aes(x = Time, y = total, colour = cutting)) +
#  geom_line(data = maletotals, size = 2, alpha = 0.6, colour = "pink") +
  geom_line(size = 0.5) + theme_light() +
  labs(title = "The Impact of Cas-9 Cutting Frequency in Absence of Maternal Cas-9 Deposition")+ 
  xlab("Time (days)") +
  ylab("Number of Females") +
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(expand = c(0, 0), limits = c(0,365)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5,300)) +
  scale_colour_discrete(name = "Cutting Frequency", labels = c("0%", "20%", "40%", "60%","80%", "100%"))

ggsave(device = "png", filename = "cutting investigation", path="plots")
