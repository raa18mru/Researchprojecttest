rm(list=ls())

library(tidyverse)

#read in both loops and not-loops

dep0 <- read.csv("Deposition_investigation/aggFolder/F_aggregate_run001_patch001.csv")
dep02 <- read.csv("Deposition_investigation/aggFolder/F_aggregate_run002_patch001.csv")
dep04 <- read.csv("Deposition_investigation/aggFolder/F_aggregate_run003_patch001.csv")
dep06 <- read.csv("Deposition_investigation/aggFolder/F_aggregate_run004_patch001.csv")
dep08 <- read.csv("Deposition_investigation/aggFolder/F_aggregate_run005_patch001.csv")
dep1 <- read.csv("Deposition_investigation/aggFolder/F_aggregate_run006_patch001.csv")
# Make a total column and a technology column

dep0 <- mutate(dep0, deposition = "0") %>% 
  mutate(dep0, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

dep02 <- mutate(dep02, deposition = "0.2") %>% 
  mutate(dep02, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

dep04 <- mutate(dep04, deposition = "0.4") %>% 
  mutate(dep04, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

dep06 <- mutate(dep06, deposition = "0.6") %>% 
  mutate(dep06, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

dep08 <- mutate(dep08, deposition = "0.8") %>% 
  mutate(dep08, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

dep1 <- mutate(dep1, deposition = "1") %>% 
  mutate(dep1, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)


dep0 <- summarise(dep0, deposition, total, Time)
dep02 <- summarise(dep02, deposition, total, Time)
dep04 <- summarise(dep04, deposition, total, Time)
dep06 <- summarise(dep06, deposition, total, Time)
dep08 <- summarise(dep08, deposition, total, Time)
dep1 <- summarise(dep1, deposition, total, Time)
#  set <1 to 0

totals <- rbind(dep0, dep02, dep04, dep06, dep08, dep1)
totals <- mutate(totals, total = ifelse(total < 1, 0, total))
totals <- mutate(totals, Time = Time - 60)

# Prepare male data 
# maletotals <- mutate(malecsv, technology= "Homozygous Males") %>% 
# mutate(malecsv, total= RR)
# maletotals <- summarise(maletotals, technology, total, Time)

# Make ggplot

ggplot(data = totals, mapping = aes(x = Time, y = total, colour = deposition)) +
#  geom_line(data = maletotals, size = 2, alpha = 0.6, colour = "pink") +
  geom_line(size = 0.5) + theme_light() +
  labs(title = "The Impact of Maternally Deposited Cas-9 Cutting Frequency on Female Medfly Population Suppression")+ 
  xlab("Time (days)") +
  ylab("Number of Females") +
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(expand = c(0, 0), limits = c(0,365)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5,300)) +
  scale_colour_discrete(name = "Cutting Frequency", labels = c("0%", "20%", "40%", "60%","80%", "100%"))

ggsave(device = "png", filename = "deposition investigation", path="plots")
