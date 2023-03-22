
source('simulation.R')

years <- seq(2000.5, 2050, 0.5)
d <- sim(years)

# Summarize enrollments by year, semester, and course
d_sum <-
  d %>%
  mutate(
    semester = ifelse(year - floor(year) == 0, 'spring', 'fall')
  ) %>%
  group_by(year, semester, course) %>%
  summarise(
    N = n()
  )

d_median <-
  d_sum %>% 
  group_by(course) %>% 
  summarise(
    median = median(N)
  ) %>% 
  mutate(
    course = fct_reorder(course, median)
  )

d_sum$course <- factor(d_sum$course, levels = levels(d_median$course))

# Elective course numbers must be divided by the average number
# of elective courses offered each semester
ggplot(d_sum, aes(N, course)) +
  geom_count() +
  geom_point(data = d_median, aes(median, course), colour = 'red', shape = 3, size = 5) +
  xlim(0,NA) +
  labs(
    title = 'Course enrollment distribution',
    subtitle = 'Red cross: median enrollment',
    x = '\nEnrollment per semester',
    y = ''
  ) +
  theme_minimal(15)
