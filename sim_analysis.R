library(tidyverse)

source('simulation.R')

years <- seq(2010.5, 2030, 0.5)
d <- sim(years)

d_sum <-
  d %>%
  mutate(
    semester = ifelse(year - floor(year) == 0, 'Spring', 'Fall')
  ) %>%
  group_by(year, semester, course) %>%
  summarise(
    N = n()
  )

# Elective course numbers must be divided by the average number
# of elective courses offered each semester
ggplot(d_sum, aes(N, course)) +
  geom_count() +
  theme_minimal(15)
