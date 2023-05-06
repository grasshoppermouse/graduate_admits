#+ message=F
library(furrr)
source('simulation.R')
source('params.R')

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

#   ~course,            ~year,            ~semester,
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
  electives <- allreqs[str_detect(allreqs, 'elective|lab')]
  nonelectives <- setdiff(allreqs, electives)
  
  bind_rows(
    freq2schedule(electives, elective_freq), 
    freq2schedule(nonelectives, nonelective_freq)
  )
}

args <- c('arch_reqs', 'cult_reqs', 'evo_reqs', 'arch_lambda', 'cult_lambda', 'evo_lambda')
params$streams <- pmap(params[args], create_streams)

# Takes several minutes
args <- c('arch_reqs', 'cult_reqs', 'evo_reqs', 'elective_freq', 'nonelective_freq')
params$catalog <- future_pmap(params[args], create_catalog, .options = furrr_options(seed = 123), .progress = T)
out <- future_map2(params$streams[1:20], params$catalog[1:20], \(streams, catalog) sim(years, streams, catalog), .options = furrr_options(seed = 123), .progress = T)

save(out, file = 'out.rds')