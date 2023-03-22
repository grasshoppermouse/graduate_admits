

# Stream degree requirements ----------------------------------------------

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

# Each vector contains the courses needed to complete the MA and PhD for each stream
# If, e.g., multiple "arch electives" are required, then they appear
# the necessary number of times

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

# This function spits out the course offerings for each semester, e.g.,
# 2020.0 is Spring semester, and 2020.5 is Fall semester
course_schedule <- function(year){
  semester <- ifelse(year - floor(year) == 0, 'Spring', 'Fall')
  spring <- c('cult comm', 'cult linguistic')
  fall <- c('530', '537', '554')
  both <- c('arch lab', 'arch elective', 'cult elective', 'evo elective', 'open elective', 'cult theory', 'cult ethnography')
  if (semester == 'Spring') return(c(spring, both))
  return(c(fall, both))
}

# This function creates a grad student,
# which is a list containing the courses needed
# for each degree for each stream
student <- function(year, stream){
  
  # Assign courses for stream MA defined earlier
  MA <- switch(
    stream,
    arch = archMA,
    cult = cultMA,
    evo = evoMA
  )
  
  # Assign courses for stream PhD defined earlier
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
    completed = FALSE # Finished taking all courses or left program
  )
}

# This function creates a cohort of students for
# given year for a given stream, using a Poisson
# distribution with rate parameter computed from
# the file from Kam
stream_cohort <- function(year, stream, lambda){
  n <- rpois(1, lambda)
  cohort <- list()
  for (i in 1:n){
    cohort <- append(cohort, list(student(year, stream)))
  }
  return(cohort)
}

# This function creates the entire cohort for year (all streams combined)
# and adds it to the existing grads
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

# This function runs the simulation for a range of years
# And returns course enrollments per year per semester
sim <- function(years, arch_lambda=3.7, cult_lambda = 1.7, evo_lambda = 2){
  
  # Stream admits: Poisson rate param defaults
  # From mean stream admits 2020-2022
  # arch_lambda = 3.7
  # cult_lambda = 1.7
  # evo_lambda = 2
  
  # No grads to begin with
  grads <- list() 
  
  # No course enrollments to begin with
  courses <- data.frame(
    year = numeric(),
    course = character(),
    student_stream = character()
  )
  
  for (year in years){
    
    # E.g., 2020.0 is Spring, 2020.5 is Fall
    semester <- ifelse(year - floor(year) == 0, 'Spring', 'Fall')
    
    # Add new cohort to existing grads in the Fall
    # and print out basic stats
    if (semester == 'Fall'){
      grads <- cohort(year, grads, arch_lambda, cult_lambda, evo_lambda)
      
      completed_grads <- sum(map_lgl(grads, function(x) x$completed))
      cat(paste('\n\nActive grad cohort:', length(grads)-completed_grads))
      cat(paste('\nCompleted:', completed_grads))
    }
    
    # Loop through all grads and check if they still
    # need to take courses and if those courses are offered
    # this semester
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
      
      # Course offerings that fulfill degree requirements
      available <- intersect(grad[[degree]], course_schedule(year))
      if (length(available) == 0) next
      
      # Everyone takes 3 courses every semester until degree requirements are met 
      # (or at least as many needed courses are available)
      num_courses <- min(length(available), 3)
      grad_schedule <- available[1:num_courses]
      
      # Remove these courses from list of courses still needed for degree
      grads[[i]][[degree]] <- grad[[degree]][-(match(grad_schedule, grad[[degree]]))]
      
      # At end of second year, an average of 60% of students leave
      if (program_year == 2 & rbinom(1, 1, 0.6)) grads[[i]]$completed <- TRUE
      if (length(grad$PhD) == 0) grads[[i]]$completed <- TRUE
      
      # Add this students courses to data frame of all courses taken
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
