#+ message=F
library(furrr)
source('simulation.R')
source('params.2.R')

plan(multicore)

years <- seq(2000.5, 2050, 0.5)

# high: every term; medium: every year; low: every other year
elective_freq <- c('low', 'medium', 'high')
nonelective_freq <- c('low', 'medium', 'high')

# Create all combos of reqs, freqs, and lambdas
params <- expand_grid(
  arch_reqs, 
  cult_reqs, 
  evo_reqs,
  elective_freq,
  nonelective_freq,
  arch_lambda = 1:4,
  cult_lambda = 1:4,
  evo_lambda = 1:4
)

params$arch_version <- names(params$arch_reqs)
params$cult_version <- names(params$cult_reqs)
params$evo_version <- names(params$evo_reqs)

create_catalog <- function(arch_reqs, cult_reqs, evo_reqs, elective_freq, nonelective_freq){
  
  freq2schedule <- function(courses, freq){
    year <- case_when(
      freq == 'low' ~ sample(c('Even years', 'Odd years'), length(courses), replace = T), 
      freq == 'medium' ~ 'Every year',
      freq == 'high' ~ 'Every year'
    )
    semester <- case_when(
      freq == 'low' ~ sample(c('Fall', 'Spring'), length(courses), replace = T), 
      freq == 'medium' ~ sample(c('Fall', 'Spring'), length(courses), replace = T),
      freq == 'high' ~ 'Both semesters'
    )
    tibble(
      course = courses,
      year = year,
      semester = semester
    )
  }
  
  allreqs <- unique(unlist(c(arch_reqs, cult_reqs, evo_reqs)))
  allreqs <- setdiff(allreqs, c('arch', 'cult', 'evo')) # remove the cross-stream elective reqs
  electives <- allreqs[str_detect(allreqs, 'elective|lab')]
  nonelectives <- setdiff(allreqs, electives)
  
  bind_rows(
    freq2schedule(electives, elective_freq), 
    freq2schedule(nonelectives, nonelective_freq)
  )
}

args <- c('arch_reqs', 'cult_reqs', 'evo_reqs', 'arch_lambda', 'cult_lambda', 'evo_lambda')
params$streams <- pmap(params[args], create_streams)

# Could take a few hours, depending...
args <- c('arch_reqs', 'cult_reqs', 'evo_reqs', 'elective_freq', 'nonelective_freq')
params$catalog <- future_pmap(params[args], create_catalog, .options = furrr_options(seed = 123), .progress = T)
params$out <- future_map2(params$streams, params$catalog, \(streams, catalog) sim(years, streams, catalog), .options = furrr_options(seed = 123), .progress = T)
# params$out <- map2(params$streams, params$catalog, \(streams, catalog) sim(years, streams, catalog), .progress = T)

make_stats <- function(out, threshold = 6){
  
  enrollment <-
    out$courses %>%
    mutate(
      semester = ifelse(year - floor(year) == 0, 'spring', 'fall')
    ) %>%
    group_by(year, semester, course) %>%
    summarise(
      N = n(),
      .groups = 'drop'
    ) 
  
  stats <-
    enrollment %>% 
    group_by(course) %>% 
    summarise(
      made = sum(N >= threshold)/n(),
      .groups = 'drop'
    ) %>% 
    pivot_wider(names_from = 'course', values_from = 'made') 
  
  stats$overall <- sum(enrollment$N[enrollment$course != 'open elective'] >= threshold)/nrow(enrollment[enrollment$course != 'open elective',])
  stats$median_courses_semester <- median(tapply(out$courses$course, out$courses$year, \(v) length(unique(v))))
  
  timeTocompletion <-
    out$grads %>% 
    mutate(
      MA_time = MA_completed - year_admitted + 0.5,
      PhD_time = PhD_completed - year_admitted + 0.5
    ) %>% 
    group_by(stream) %>% 
    summarise(
      MeanMAtime = mean(MA_time, na.rm = T),
      MeanPhDtime = mean(PhD_time, na.rm = T)
    ) %>% 
    pivot_wider(names_from = stream, values_from = c(MeanMAtime, MeanPhDtime))
  
  return(bind_cols(stats, timeTocompletion))
}


params <- bind_cols(params, list_rbind(map(params$out, make_stats)))

params$elective_freq <- ordered(params$elective_freq, levels = c('low', 'medium', 'high'))
params$nonelective_freq <- ordered(params$nonelective_freq, levels = c('low', 'medium', 'high'))

params$elective_freq2 <- as.numeric(params$elective_freq)
params$nonelective_freq2 <- as.numeric(params$nonelective_freq)

save(params, file = 'params.rds')
