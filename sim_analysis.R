#+ message=F
library(furrr)
source('simulation.R')
source('params.R')

plan(multisession, workers = 5)

years <- seq(2000.5, 2050, 0.5)

# Create all combos of orgs, reqs, and lambdas
params <- expand_grid(
  arch_reqs, 
  cult_reqs, 
  evo_reqs, 
  arch_lambda = 1:4,
  cult_lambda = 1:4,
  evo_lambda = 1:4
)

params$streams <- pmap(params, create_streams)
# params$out <- map(params$streams, \(x) sim(years, x), .progress = T)
out <- future_map(params$streams[1:10], \(x) sim(years, x), .progress = T, .options = furrr_options(seed = 123))


grads <- out$grads

grads %>% 
  mutate(
    MA_time = MA_completed - year_admitted + 0.5,
    PhD_time = PhD_completed - year_admitted + 0.5
  ) %>% 
  group_by(stream) %>% 
  summarise(
    MeanMATime = mean(MA_time, na.rm = T),
    MeanPhDTime = mean(PhD_time, na.rm = T)
  )
  
d <- out$courses

# Summarize enrollments by year, semester, and course
d_sum <-
  d %>%
  mutate(
    semester = ifelse(year - floor(year) == 0, 'spring', 'fall')
  ) %>%
  group_by(year, semester, course) %>%
  summarise(
    N = n()
  ) %>% 
  left_join(course_catalog[c('course', 'Frequency')])

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

#+ fig.width=10
ggplot(d_sum, aes(N, course, colour=Frequency)) +
  geom_count() +
  geom_point(data = d_median, aes(median, course), colour = 'black', shape = 3, size = 6) +
  xlim(0,NA) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  labs(
    title = 'Course enrollment distribution',
    subtitle = 'Cross: median enrollment',
    x = '\nEnrollment per semester',
    y = ''
  ) +
  theme_minimal(15)

