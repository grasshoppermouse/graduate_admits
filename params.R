
# version1, version2, etc, can be given more informative names

arch_reqs = list(
  version1 = list(
    MA = c("arch theory 530", "quant 537", "cult elective", "evo elective", "arch elective", "arch elective", "arch lab", "arch lab", "arch lab"),
    PhD = c("arch elective", "arch elective", "arch elective", "arch lab", "open elective", "open elective", "open elective", "open elective")
  ),
  
  # This is just a copy of version1 for testing
  version2 = list(
    MA = c("arch theory 530", "quant 537", "cult elective", "evo elective", "arch elective", "arch elective", "arch lab", "arch lab", "arch lab"),
    PhD = c("arch elective", "arch elective", "arch elective", "arch lab", "open elective", "open elective", "open elective", "open elective")
  )
)

cult_reqs = list(
  version1 = list(
    MA = c("quant 537", "field methods 554", "arch elective", "evo elective", "cult theory", "cult theory", "cult ethnography", "cult ethnography", "cult linguistic", "open elective", "open elective"),
    PhD = c("cult comm", "open elective", "open elective")
  ),
  
  # This is just a copy of version1 for testing
  version2 = list(
    MA = c("quant 537", "field methods 554", "arch elective", "evo elective", "cult theory", "cult theory", "cult ethnography", "cult ethnography", "cult linguistic", "open elective", "open elective"),
    PhD = c("cult comm", "open elective", "open elective")
  )
)

evo_reqs = list(
  version1 = list(
    MA = c("quant 537", "evo elective", "evo elective", "evo elective", "arch elective", "cult elective", "open elective", "open elective", "open elective"),
    PhD = c("evo elective", "evo elective", "evo elective", "open elective", "open elective", "open elective", "open elective", "open elective")
  ),
  
  # This is just a copy of version1 for testing
  version2 = list(
    MA = c("quant 537", "evo elective", "evo elective", "evo elective", "arch elective", "cult elective", "open elective", "open elective", "open elective"),
    PhD = c("evo elective", "evo elective", "evo elective", "open elective", "open elective", "open elective", "open elective", "open elective")
  )
)

##### Course availability #####

### Current offerings ###

course_catalog <- tribble(
  ~course,            ~year,            ~semester,
  'arch lab',         'Every year',    'Both semesters',
  'arch elective',    'Every year',    'Both semesters',
  'cult elective',    'Every year',    'Both semesters',
  'evo elective',     'Every year',    'Both semesters',
  'open elective',    'Every year',    'Both semesters',
  'cult theory',      'Every year',    'Both semesters',
  'cult ethnography', 'Every year',    'Both semesters',
  'arch theory 530',  'Every year',    'Fall',
  'quant 537',        'Every year',    'Fall',
  'field methods 554','Every year',    'Fall',
  'cult comm',        'Even years',    'Spring',
  'cult linguistic',  'Odd years',     'Spring'
) %>%
  mutate(
    Frequency = paste(semester, year)
  )

### Anne's original proposal ###

# course_catalog <- tribble(
#   ~course,            ~year,            ~semester,
#   'arch lab',         'Every year',    'Both semesters',
#   'arch elective',    'Every year',    'Both semesters',
#   'cult elective',    'Every year',    'Both semesters',
#   'evo elective',     'Every year',    'Both semesters',
#   'open elective',    'Every year',    'Both semesters',
#   'cult theory 510',      'Every year',    'Spring',
#   'evo theory 562',       'Every year',    'Spring',
#   'cult ethnography', 'Even years',    'Fall',
#   'arch theory 530',  'Every year',    'Fall',
#   'quant 537',        'Every year',    'Fall',
#   'field methods 554','Odd years',    'Fall',
#   'comm-grants',        'Even years',    'Spring',
#   'cult linguistic',  'Odd years',     'Spring'
# ) %>% 
#   mutate(
#     Frequency = paste(semester, year)
#   )