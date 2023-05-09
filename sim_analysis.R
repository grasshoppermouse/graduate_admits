#+ message=F, warning=F

library(tidyverse)
library(ggcorrplot)
library(hagenutils)

load('params.rds')

#' # Time to MA

# These are averaging over mean admit rates from 1 to 4

# Mean time to Arch MA by freq of electives (rows) and nonelectives (cols)
# Low freq: once a semester, every other year
# Med freq: once a semester, every year
# High freq: both semesters, every year

# Arch
#+ message=F, warning=F
ggplot(params, aes(MeanMAtime_arch)) +
  geom_histogram() +
  facet_grid(elective_freq ~ nonelective_freq)

# Cult
#+ message=F, warning=F
ggplot(params, aes(MeanMAtime_cult)) +
  geom_histogram() +
  facet_grid(elective_freq ~ nonelective_freq)

# Evo
#+ message=F, warning=F
ggplot(params, aes(MeanMAtime_evo)) +
  geom_histogram() +
  facet_grid(elective_freq ~ nonelective_freq)

#' # Time to PhD

# These are averaging over mean admit rates from 1 to 4

# Arch
#+ message=F, warning=F
ggplot(params, aes(MeanPhDtime_arch)) +
  geom_histogram() +
  facet_grid(elective_freq ~ nonelective_freq)

# Cult
#+ message=F, warning=F
ggplot(params, aes(MeanPhDtime_cult)) +
  geom_histogram() +
  facet_grid(elective_freq ~ nonelective_freq)

# Evo
#+ message=F, warning=F
ggplot(params, aes(MeanPhDtime_evo)) +
  geom_histogram() +
  facet_grid(elective_freq ~ nonelective_freq)

#' # Proportion of courses that "make" with minimum of 6 students

# These are averaging over mean admit rates from 1 to 4
#+ message=F, warning=F
ggplot(params, aes(overall)) +
  geom_histogram() +
  facet_grid(elective_freq ~ nonelective_freq)

#+ message=F, warning=F
m1 <- glm(overall ~ arch_lambda + cult_lambda + evo_lambda + elective_freq + nonelective_freq, family = binomial, params)
summary(m1)

m2 <- glm(median_courses_semester ~ arch_lambda + cult_lambda + evo_lambda + elective_freq + nonelective_freq, family = poisson, params)
summary(m2)

# visreg(m2, scale = 'response')
# 
# params2 %>% 
#   dplyr::select(where(is.numeric)) %>% 
#   cor() %>% 
#   ggcorrplot()
# 
# params2 %>% 
#   dplyr::select(where(is.numeric)) %>% 
#   t() %>% 
#   hagenheat(., scale. = 'column', method = 'hclust')
# 
# tmp <-
#   params2[c('overall', 'elective_freq2', 'nonelective_freq2', 'arch_lambda', 'cult_lambda', 'evo_lambda', 'MeanMAtime_arch', 'MeanPhDtime_arch', 'MeanMAtime_cult', 'MeanPhDtime_cult', 'MeanMAtime_evo', 'MeanPhDtime_evo', 'median_courses_semester')]
# 
# ggcorrplot(cor(tmp), hc.order = T, hc.method = 'ward.D')
# 