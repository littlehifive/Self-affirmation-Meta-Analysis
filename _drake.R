
# Load all associated functions and packages with the plan

source(here::here("R/packages.R"))
source(here::here("R/functions.R"))
source(here::here("R/calculations.R"))
source(here::here("R/cleaning.R"))
source(here::here("R/analysis.R"))
source(here::here("R/plan.R"))

# configure the drake plan
drake_config(the_plan)
