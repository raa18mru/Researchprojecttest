rm(list=ls())

library(tidyverse)

#read in both loops and not-loops

swi100 <- read.csv("Sex_switching/aggFolder/F_aggregate_run001_patch001.csv")
swi80 <- read.csv("Sex_switching/aggFolder/F_aggregate_run002_patch001.csv")
swi60 <- read.csv("Sex_switching/aggFolder/F_aggregate_run003_patch001.csv")
swi40 <- read.csv("Sex_switching/aggFolder/F_aggregate_run004_patch001.csv")
swi20 <- read.csv("Sex_switching/aggFolder/F_aggregate_run005_patch001.csv")
swi0 <- read.csv("Sex_switching/aggFolder/F_aggregate_run006_patch001.csv")

# Make a total column and a efficiency column

swi100 <- mutate(swi100, switching = "100") %>% 
  mutate(swi100, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

swi80 <- mutate(swi80, switching = "80") %>% 
  mutate(swi80, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

swi60 <- mutate(swi60, switching = "60") %>% 
  mutate(swi60, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

swi40 <- mutate(swi40, switching = "40") %>% 
  mutate(swi40, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

swi20 <- mutate(swi20, switching = "20") %>% 
  mutate(swi20, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)

swi0 <- mutate(swi0, switching = "0") %>% 
  mutate(swi0, total = WWWW+WWWR+WWWB+WWRR+WWRB+WWBB+WHWW+WHWR+WHWB+WHRR+WHRB+
           WHBB+HHWW+HHWR+HHWB+HHRR+HHRB+HHBB)


swi100 <- summarise(swi100, switching, total, Time)
swi80 <- summarise(swi80, switching, total, Time)
swi60 <- summarise(swi60, switching, total, Time)
swi40 <- summarise(swi40, switching, total, Time)
swi20 <- summarise(swi20, switching, total, Time)
swi0 <- summarise(swi0, switching, total, Time)
#  set <1 to 0

totals <- rbind(swi100, swi80, swi60, swi40, swi20, swi0)
totals <- mutate(totals, total = ifelse(total < 1, 0, total))
totals <- mutate(totals, Time = Time - 60)

totals$switching <- factor(totals$switching, levels = c("0", "20", "40", "60", "80", "100"))
# Prepare male data 
# maletotals <- mutate(malecsv, technology= "Homozygous Males") %>% 
# mutate(malecsv, total= RR)
# maletotals <- summarise(maletotals, technology, total, Time)

# Make ggplot

ggplot(data = totals, mapping = aes(x = Time, y = total, colour = switching)) +
  #  geom_line(data = maletotals, size = 2, alpha = 0.6, colour = "pink") +
  geom_line(size = 1.5) + theme_light() +
  labs(title = "The Impact of Sex Switching Efficacy on Female Medfly Population Suppression")+ 
  xlab("Time (days)") +
  ylab("Number of Females") +
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(expand = c(0, 0), limits = c(0,365)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5,305)) +
  scale_colour_discrete(name = "Sex Switching Efficacy", 
  labels = c("0%", "20%", "40%", "60%", "80%", "100%"))

ggsave(device = "png", filename = "sex_switching", path="plots")
