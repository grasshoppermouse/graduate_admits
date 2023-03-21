
# Arch MA 
# reqs: 530, 537, 1 cult elective, 1 evo elective
# 2 arch electives
# 3 arch labs
# 4 units of 700
# 
# Arch PhD
# 3 arch electives
# 1 arch lab
# 4 open electives
# 20 units of 800

archMA<- c("530", "537", "cult elective", "evo elective", "arch elective", "arch elective", "arch lab", "arch lab", "arch lab")
archPhD <- c("arch elective", "arch elective", "arch elective", "arch lab", "open elective", "open elective", "open elective", "open elective")

# Cult MA
# reqs: 537, 554, 1 arch elective, 1 evo elective
# 2 cult theory electives
# 2 cult ethnography electives
# 1 cult linguistic elective
# 2 open electives
# 4 units of 700
#
# Cult PhD
# 1 cult profession communication
# 2 open electives
# 20 units of 800

cultMA <- c("537", "554", "arch elective", "evo elective", "cult theory", "cult theory", "cult ethnography", "cult ethnography", "cult linguistic", "open elective", "open elective")
cultPhD <- c("cult comm", "open elective", "open elective")

# Evo MA
# reqs: 537, 3 evo elect, 1 arch elect, 1 cult elect
# 3 open electives
# 4 units of 700
#
# Evo PhD
# 3 evo electives
# 5 open electives
# 20 units 800

evoMA <- c("537", "evo elective", "evo elective", "evo elective", "arch elective", "cult elective", "open elective", "open elective", "open elective")
evoPhD <- c("evo elective", "evo elective", "evo elective", "open elective", "open elective", "open elective", "open elective", "open elective")

course_schedule <- function(year){
  semester <- ifelse(year - floor(year) == 0, 'Spring', 'Fall')
  spring <- c('cult comm', 'cult linguistic')
  fall <- c('530', '537', '554')
  both <- c('arch lab', 'arch elective', 'cult elective', 'evo elective', 'open elective', 'cult theory', 'cult ethnography')
  if (semester == 'Spring') return(c(spring, both))
  return(c(fall, both))
}

student <- function(year, stream){
  
  MA <- switch(
    stream,
    arch = archMA,
    cult = cultMA,
    evo = evoMA
  )
  
  PhD <- switch(
    stream,
    arch = archPhD,
    cult = cultPhD,
    evo = evoPhD
  )
  
  list(
    year_admitted = year,
    stream = stream,
    MA = sample(MA), # randomize order
    PhD = sample(PhD), # randomize order
    completed = FALSE
  )
}

stream_cohort <- function(year, stream, lambda){
  n <- rpois(1, lambda)
  cohort <- list()
  for (i in 1:n){
    cohort <- append(cohort, list(student(year, stream)))
  }
  return(cohort)
}

cohort <- function(year, grads, arch_lambda, cult_lambda, evo_lambda){

  for (s in stream_cohort(year, 'arch', arch_lambda)){
    grads[[length(grads)+1]] <- s
  }
  for (s in stream_cohort(year, 'cult', cult_lambda)){
    grads[[length(grads)+1]] <- s
  }
  for (s in stream_cohort(year, 'evo', evo_lambda)){
    grads[[length(grads)+1]] <- s
  }
  return(grads)
}

sim <- function(years, arch_lambda=3.7, cult_lambda = 1.7, evo_lambda = 2){
  
  # Stream admits: Poisson rate param defaults
  # From mean stream admits 2020-2022
  # arch_lambda = 3.7
  # cult_lambda = 1.7
  # evo_lambda = 2
  
  grads <- list()
  
  courses <- data.frame(
    year = numeric(),
    course = character(),
    student_stream = character()
  )
  
  for (year in years){
    
    semester <- ifelse(year - floor(year) == 0, 'Spring', 'Fall')
    
    if (semester == 'Fall'){
      grads <- cohort(year, grads, arch_lambda, cult_lambda, evo_lambda)
      
      completed_grads <- sum(map_lgl(grads, function(x) x$completed))
      cat(paste('\n\nActive grad cohort:', length(grads)-completed_grads))
      cat(paste('\nCompleted:', completed_grads))
    }
    
    for (i in seq_along(grads)){
      
      grad <- grads[[i]]
      if (grad$completed) next
      
      program_year <- year - grad$year_admitted + 1
      
      # MA or PhD?
      if (program_year <= 2){
        degree <- "MA"
      } else {
        degree <- "PhD"
      }
      
      available <- intersect(grad[[degree]], course_schedule(year))
      if (length(available) == 0) next
      
      num_courses <- min(length(available), 3)
      grad_schedule <- available[1:num_courses]
      grads[[i]][[degree]] <- grad[[degree]][-(match(grad_schedule, grad[[degree]]))]
      
      if (program_year == 2 & rbinom(1, 1, 0.6)) grads[[i]]$completed <- TRUE
      if (length(grad$PhD) == 0) grads[[i]]$completed <- TRUE
      
      courses <- rbind(
        courses,
        data.frame(
          year = rep(year, num_courses),
          course = grad_schedule,
          stream = rep(grad$stream, num_courses)
        )
      )
    }
  }
  return(courses)
}
