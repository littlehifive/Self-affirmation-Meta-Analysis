the_plan <- drake_plan(
  
  dat <- data.frame(study = NA, author = NA, year = NA, adapted = NA, type = NA,  outcome = NA, adjusted = NA, es = NA, v = NA, lowerCI = NA, upperCI = NA),
  
  dat <- effsize_cal(dat),
  
  #  Sys.setenv(F_EXPORT_DATA = "TRUE")
  
  export_processed_dat = target(
    command = {
      if (isTRUE(F_EXPORT_DATA)) {
        message("Writing processed attendance datasets")
        
        write.csv(dat, file.path(here::here(), "Exports"), row.names = F)
      }
      
      Sys.time()
    },
    trigger = trigger(
      condition = isTRUE(F_EXPORT_DATA),
      mode = "condition"
    )
  )
  
)
