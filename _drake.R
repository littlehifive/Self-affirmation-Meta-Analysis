# All r_* functions in drake use this file to create/restore
# child R sessions to improve reproducibility.

# Load all associated functions and packages with the plan
# utils.R: All utility functions that don't depend on other packages
# environment.R: Global definitions of environment variable names
# packages.R: All packages you need to attach using library()
# functions.R: All other functions used in your project
# cleaning.R: Functions related to cleaning steps
# plan.R: The drake plan
source(here::here("R/packages.R"))
source(here::here("R/functions.R"))
source(here::here("R/calculations.R"))
source(here::here("R/cleaning.R"))
source(here::here("R/analysis.R"))
source(here::here("R/plan.R"))

# configure the drake plan
drake_config(the_plan)
