the_plan <- drake_plan(

  # effect sizes from these three studies were calculated using the original datasets
  kost = haven::read_dta(file.path(here::here(),"Imports/Kost-smith.dta")),
  purdie = haven::read_sav(file.path(here::here(),"Imports/Purdie-Greenaway.sav")),
  turetsky = read.csv(file.path(here::here(),"Imports/Turetsky under review.csv"), as.is = T),

  # derive cleaned master dataset
  dat_cleaned = clean_master(kost, purdie, turetsky),
  
  #  run the following line to allow writing data:
  # Sys.setenv(F_EXPORT_DATA = "TRUE")
  
  export_processed_dat = target(
    command = {
      if (isTRUE(F_EXPORT_DATA)) {
        message("Writing processed attendance datasets")
        
        write.csv(dat_cleaned, file.path(here::here(), "Exports/master.csv"), row.names = F)
      }
      
      Sys.time()
    },
    trigger = trigger(
      condition = isTRUE(F_EXPORT_DATA),
      mode = "condition"
    )
  )
  
)
