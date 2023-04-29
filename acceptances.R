library(tidyverse)
library(readxl)

d0 <- 
  read_excel("acceptances, A. Pisor.xlsx", skip = 1) %>% 
  mutate(
    Arch = `Arch MA` + `Arch PhD`,
    Cult = `Cult MA` + `Cult PhD`,
    Evo  = `Evo MA`  + `Evo PhD`,
    Total = Arch + Cult + Evo
  ) 

d <- 
  d0 %>% 
  pivot_longer(c(Arch, Cult, Evo, Total), names_to = 'Stream', values_to = 'Admits')

ggplot(d, aes(Year, Admits, colour=Stream, group=Stream)) +
  geom_line(linewidth=2) +
  theme_minimal(15)

ggplot(d[d$Stream != 'Total',], aes(Year, Admits, fill=Stream)) +
  geom_col() +
  scale_x_continuous(breaks=2014:2022) +
  scale_fill_viridis_d(end = 0.9, option = 'G') +
  labs(
    title='Anthropology graduate admissions',
    x = '',
    y = 'Number of admissions'
  ) +
  theme_minimal(15)

mean_arch <- mean(d0$Arch[d0$Year %in% 2020:2022])
mean_cult <- mean(d0$Cult[d0$Year %in% 2020:2022])
mean_evo  <- mean(d0$Evo[d0$Year %in% 2020:2022])

# mean_arch = 3.7
# mean_cult = 1.7
# mean_evo = 2
