the_plan <- drake_plan(


# 1. Import data -------------------------------------------------------------
  
  # effect sizes from these three studies were calculated using the original datasets
  kost = haven::read_dta(file.path(here::here(),"Imports/Kost-smith.dta")),
  purdie = haven::read_sav(file.path(here::here(),"Imports/Purdie-Greenaway.sav")),
  turetsky = read.csv(file.path(here::here(),"Imports/Turetsky under review.csv"), as.is = T),


# 2. Calculation and cleaning -----------------------------------------------------------

  # derive cleaned master dataset
  dat_cleaned = clean_master(kost, purdie, turetsky),


# 3. Write data --------------------------------------------------------------

  #  run the following line to allow writing data:
  # Sys.setenv(F_EXPORT_DATA = "TRUE")
  
  export_processed_dat = target(
    command = {
      if (isTRUE(F_EXPORT_DATA)) {
        message("Writing processed datasets")
        
        write.csv(dat_cleaned, file.path(here::here(), "Exports/master.csv"), row.names = F)
      }
      
      Sys.time()
    },
    trigger = trigger(
      condition = isTRUE(F_EXPORT_DATA),
      mode = "condition"
    )
  ),


# 4. Data analysis --------------------------------------------------------

# multilevel analysis

combined_mlm_results = get_mlm_results(dat_cleaned),

export_mlm_results = target(
  command = {
    if (isTRUE(F_EXPORT_DATA)) {
      message("Writing multilevel model results")
      
      write.csv(combined_mlm_results, file.path(here::here(), "Exports/MLM results.csv"), row.names = F)
    }
    
    Sys.time()
  },
  trigger = trigger(
    condition = isTRUE(F_EXPORT_DATA),
    mode = "condition"
  )
),

# study-level pooled analysis

combined_study_level_results = get_study_level_results(dat_cleaned),

export_study_level_results = target(
  command = {
    if (isTRUE(F_EXPORT_DATA)) {
      message("Writing study-level model results")
      
      write.csv(combined_study_level_results, file.path(here::here(), "Exports/Study-level results.csv"), row.names = F)
    }
    
    Sys.time()
  },
  trigger = trigger(
    condition = isTRUE(F_EXPORT_DATA),
    mode = "condition"
  )
),

# study-level plots...

  
)
