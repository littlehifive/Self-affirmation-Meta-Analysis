#!/usr/bin/env Rscript

# Before running this project, be sure to do the following:
# 1. Open a new R session in this folder
# 2. Install the 'devtools' package
# 3. Run `devtools::install_deps(dependencies = TRUE)`
# 4. Run the project in the terminal with:
#   a. `Rscript make.R`
#   b. `./make.R` if on a UNIX-like OS 
#       (be sure to run `chmod +x make.R` to make the file executable)

# When working interactively with the drake pipeline, there are some handy
# functions you need to make life easy:
# readd(<target name>): Reads the target object from the cache
# r_outdated(): Determines which targets in your plan are outdated
# r_vis_drake_graph(): Renders and displays the state of the drake pipeline
#
# Source this file and then use these to inspect your progress

library(drake)

r_make()
r_sankey_drake_graph()
r_vis_drake_graph()
r_outdated()