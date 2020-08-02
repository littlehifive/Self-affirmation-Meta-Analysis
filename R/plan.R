the_plan <- drake_plan(


# 1. Import data -------------------------------------------------------------
  
# effect sizes from these three studies were calculated using the original datasets
  kost = haven::read_dta(file.path(here::here(),"Imports/Kost-smith.dta")),
  purdie = haven::read_sav(file.path(here::here(),"Imports/Purdie-Greenaway.sav")),
  turetsky = read.csv(file.path(here::here(),"Imports/Turetsky under review.csv"), as.is = T),
  
# extraction sheet for basic info and moderators

  mod = read.csv(file.path(here::here(),"Imports/extraction_basic_revised.csv"), as.is = T),

# 2. Calculation and cleaning -----------------------------------------------------------

  # derive the cleaned master dataset
  dat.s = clean_master(kost, purdie, turetsky),
  
  dat.mod = clean_moderator(mod),

  dat = left_join(dat.s, 
                  select(dat.mod, -c(year, author)),
                  by = "id"),


# 3. Write data --------------------------------------------------------------

  # run the following line to allow writing data:
  # Sys.setenv(F_EXPORT_DATA = "TRUE")
  
  export_processed_dat = target(
    command = {
      if (isTRUE(F_EXPORT_DATA)) {
        message("Writing processed datasets")
        
        write.csv(dat.s, file.path(here::here(), "Exports/master.csv"), row.names = F)
        write.csv(dat.mod, file.path(here::here(), "Exports/moderators.csv"), row.names = F)
        write.csv(dat, file.path(here::here(), "Exports/master_mod_merged.csv"), row.names = F)

      }
      
      Sys.time()
    },
    trigger = trigger(
      condition = isTRUE(F_EXPORT_DATA),
      mode = "condition"
    )
  ),


# 4. Main analysis --------------------------------------------------------
## 4.1 Prepare data subsets --------------------------------------------------------
## Minority
data_all_outcomes_minority = dat %>%
  filter(type_s %in% c("Minority subgroup", "Interaction"),
         !group %in% c("Male", "Asian","White", "nonFSM", "Continuing generation", "White and Asian")),
data_all_primary_outcomes_minority = dat %>%
  filter(type_s %in% c("Minority subgroup", "Interaction"),
         !group %in% c("Male", "Asian","White", "nonFSM", "Continuing generation", "White and Asian"),
         grepl("GPA",outcome)),
data_all_secondary_outcomes_minority = dat %>%
  filter(type_s %in% c("Minority subgroup", "Interaction"),
         !group %in% c("Male", "Asian","White", "nonFSM", "Continuing generation", "White and Asian"),
         !grepl("GPA",outcome)),
data_adjusted_outcomes_minority = dat %>%
  filter(adjusted == "Yes",
         type_s %in% c("Minority subgroup", "Interaction"),
         !group %in% c("Male", "Asian","White", "nonFSM", "Continuing generation", "White and Asian")),
data_adjusted_primary_outcomes_minority = dat %>%
  filter(adjusted == "Yes",
         type_s %in% c("Minority subgroup", "Interaction"),
         !group %in% c("Male", "Asian","White", "nonFSM", "Continuing generation", "White and Asian"),
         grepl("GPA",outcome)),
data_adjusted_secondary_outcomes_minority = dat %>%
  filter(adjusted == "Yes",
         type_s %in% c("Minority subgroup", "Interaction"),
         !group %in% c("Male", "Asian","White", "nonFSM", "Continuing generation", "White and Asian"),
         !grepl("GPA",outcome)),

## Majority
data_all_outcomes_majority = dat %>%
  filter(type_s %in% c("Majority subgroup", "Interaction"),
         group %in% c("Male", "Asian","White", "nonFSM", "Continuing generation", "White and Asian")),
data_all_primary_outcomes_majority = dat %>%
  filter(type_s %in% c("Majority subgroup", "Interaction"),
         group %in% c("Male", "Asian","White", "nonFSM", "Continuing generation", "White and Asian"),
         grepl("GPA",outcome)),
data_all_secondary_outcomes_majority = dat %>%
  filter(type_s %in% c("Majority subgroup", "Interaction"),
         group %in% c("Male", "Asian","White", "nonFSM", "Continuing generation", "White and Asian"),
         !grepl("GPA",outcome)),
data_adjusted_outcomes_majority = dat %>%
  filter(adjusted == "Yes",
         type_s %in% c("Majority subgroup", "Interaction"),
         group %in% c("Male", "Asian","White", "nonFSM", "Continuing generation", "White and Asian")),
data_adjusted_primary_outcomes_majority = dat %>%
  filter(adjusted == "Yes",
         type_s %in% c("Majority subgroup", "Interaction"),
         group %in% c("Male", "Asian","White", "nonFSM", "Continuing generation", "White and Asian"),
         grepl("GPA",outcome)),
data_adjusted_secondary_outcomes_majority = dat %>%
  filter(adjusted == "Yes",
         type_s %in% c("Majority subgroup", "Interaction"),
         group %in% c("Male", "Asian","White", "nonFSM", "Continuing generation", "White and Asian"),
         !grepl("GPA",outcome)),

data_all_outcomes_main = dat %>% 
  filter(type_s %in% c("Main"),
         study %in% c("Churchill, 2018", "De Clercq, 2019", "Lauer, 2013", "Peters, 2017", "Rapa, 2016")),


## 4.2 Multilevel analysis --------------------------------------------------------
model_mlm_all_outcomes_minority = run_meta_mlm(
  data = data_all_outcomes_minority,
  random = list(~ 1|cluster)
),

model_mlm_all_primary_outcomes_minority = run_meta_mlm(
  data = data_all_primary_outcomes_minority,
  random = list(~ 1|cluster)
),

model_mlm_all_secondary_outcomes_minority = run_meta_mlm(
  data = data_all_secondary_outcomes_minority,
  random = list(~ 1|cluster)
),

model_mlm_adjusted_outcomes_minority = run_meta_mlm(
  data = data_adjusted_outcomes_minority,
  random = list(~ 1|cluster)
),

model_mlm_adjusted_primary_outcomes_minority = run_meta_mlm(
  data = data_adjusted_primary_outcomes_minority,
  random = list(~ 1|cluster)
),

model_mlm_adjusted_secondary_outcomes_minority = run_meta_mlm(
  data = data_adjusted_secondary_outcomes_minority,
  random = list(~ 1|cluster)
),

combined_mlm_results_minority = print_mlm_results(model_mlm_all_outcomes_minority,
                                                  model_mlm_all_primary_outcomes_minority,
                                                  model_mlm_all_secondary_outcomes_minority,
                                                  model_mlm_adjusted_outcomes_minority,
                                                  model_mlm_adjusted_primary_outcomes_minority,
                                                  model_mlm_adjusted_secondary_outcomes_minority),


model_mlm_all_outcomes_majority = run_meta_mlm(
  data = data_all_outcomes_majority,
  random = list(~ 1|cluster)
),

model_mlm_all_primary_outcomes_majority = run_meta_mlm(
  data = data_all_primary_outcomes_majority,
  random = list(~ 1|cluster)
),

model_mlm_all_secondary_outcomes_majority = run_meta_mlm(
  data = data_all_secondary_outcomes_majority,
  random = list(~ 1|cluster)
),

model_mlm_adjusted_outcomes_majority = run_meta_mlm(
  data = data_adjusted_outcomes_majority,
  random = list(~ 1|cluster)
),

model_mlm_adjusted_primary_outcomes_majority = run_meta_mlm(
  data = data_adjusted_primary_outcomes_majority,
  random = list(~ 1|cluster)
),

model_mlm_adjusted_secondary_outcomes_majority = run_meta_mlm(
  data = data_adjusted_secondary_outcomes_majority,
  random = list(~ 1|cluster)
),


combined_mlm_results_majority = print_mlm_results(model_mlm_all_outcomes_majority,
                                                  model_mlm_all_primary_outcomes_majority,
                                                  model_mlm_all_secondary_outcomes_majority,
                                                  model_mlm_adjusted_outcomes_majority,
                                                  model_mlm_adjusted_primary_outcomes_majority,
                                                  model_mlm_adjusted_secondary_outcomes_majority),


export_mlm_results = target(
  command = {
    if (isTRUE(F_EXPORT_DATA)) {
      message("Writing multilevel model results")
      
      write.csv(combined_mlm_results_minority, file.path(here::here(), "Exports/MLM results_minority.csv"), row.names = F)
      write.csv(combined_mlm_results_majority, file.path(here::here(), "Exports/MLM results_majority.csv"), row.names = F)
      
      }
    
    Sys.time()
  },
  trigger = trigger(
    condition = isTRUE(F_EXPORT_DATA),
    mode = "condition"
  )
),

## 4.3 Study-level pooled analysis--------------------------------------------------------
model_study_level_all_outcomes_minority = run_meta_level(
  data = data_all_outcomes_minority,
  level = "study"
),

model_study_level_all_primary_outcomes_minority = run_meta_level(
  data = data_all_primary_outcomes_minority,
  level = "study"
),

model_study_level_all_secondary_outcomes_minority = run_meta_level(
  data = data_all_secondary_outcomes_minority,
  level = "study"
),

model_study_level_adjusted_outcomes_minority = run_meta_level(
  data = data_adjusted_outcomes_minority,
  level = "study"
),

model_study_level_adjusted_primary_outcomes_minority = run_meta_level(
  data = data_adjusted_primary_outcomes_minority,
  level = "study"
),

model_study_level_adjusted_secondary_outcomes_minority = run_meta_level(
  data = data_adjusted_secondary_outcomes_minority,
  level = "study"
),

combined_study_level_results_minority = print_level_results(model_study_level_all_outcomes_minority,
                                                  model_study_level_all_primary_outcomes_minority,
                                                  model_study_level_all_secondary_outcomes_minority,
                                                  model_study_level_adjusted_outcomes_minority,
                                                  model_study_level_adjusted_primary_outcomes_minority,
                                                  model_study_level_adjusted_secondary_outcomes_minority),


model_study_level_all_outcomes_majority = run_meta_level(
  data = data_all_outcomes_majority,
  level = "study"
),

model_study_level_all_primary_outcomes_majority = run_meta_level(
  data = data_all_primary_outcomes_majority,
  level = "study"
),

model_study_level_all_secondary_outcomes_majority = run_meta_level(
  data = data_all_secondary_outcomes_majority,
  level = "study"
),

model_study_level_adjusted_outcomes_majority = run_meta_level(
  data = data_adjusted_outcomes_majority,
  level = "study"
),

model_study_level_adjusted_primary_outcomes_majority = run_meta_level(
  data = data_adjusted_primary_outcomes_majority,
  level = "study"
),

model_study_level_adjusted_secondary_outcomes_majority = run_meta_level(
  data = data_adjusted_secondary_outcomes_majority,
  level = "study"
),


combined_study_level_results_majority = print_level_results(model_study_level_all_outcomes_majority,
                                                  model_study_level_all_primary_outcomes_majority,
                                                  model_study_level_all_secondary_outcomes_majority,
                                                  model_study_level_adjusted_outcomes_majority,
                                                  model_study_level_adjusted_primary_outcomes_majority,
                                                  model_study_level_adjusted_secondary_outcomes_majority),


model_study_level_all_outcomes_main = run_meta_level(
  data = data_all_outcomes_main,
  level = "study"
),

combined_study_level_results_main = extract_level_results(model = model_study_level_all_outcomes_main),

export_study_level_results = target(
  command = {
    if (isTRUE(F_EXPORT_DATA)) {
      message("Writing study-level model results")
      
      write.csv(combined_study_level_results_minority, file.path(here::here(), "Exports/study-level results_minority.csv"), row.names = F)
      write.csv(combined_study_level_results_majority, file.path(here::here(), "Exports/study-level results_majority.csv"), row.names = F)
      write.csv(combined_study_level_results_main, file.path(here::here(), "Exports/study-level results_main.csv"), row.names = F)
    }
    
    Sys.time()
  },
  trigger = trigger(
    condition = isTRUE(F_EXPORT_DATA),
    mode = "condition"
  )
),


## 4.4 Study-level plots--------------------------------------------------------

export_study_level_plots = target(
  command = {
    if (isTRUE(F_EXPORT_DATA)) {
      message("Writing study-level plots")
      
      pdf(file = file.path(here::here(),"Exports/study-level forestplot_minority.pdf"), 
                          width = 10, height = 11)
      
      print_level_forest(model = model_study_level_all_outcomes_minority,
                               level = "study")
      
      pdf(file = file.path(here::here(),"Exports/study-level forestplot_majority.pdf"), 
          width = 10, height = 8)
      
      print_level_forest(model = model_study_level_all_outcomes_majority,
                               level = "study")
      
      pdf(file = file.path(here::here(),"Exports/study-level forestplot_main.pdf"), 
          width = 10, height = 4)
      
      print_level_forest(model = model_study_level_all_outcomes_main,
                         level = "study")
      
      pdf(file = file.path(here::here(),"Exports/study-level funnelplot_minority.pdf"), width = 18, height = 10)
      par(mar = c(6, 6, 2, 2))
      print_study_level_funnel(model = model_study_level_all_outcomes_minority)
      
      pdf(file = file.path(here::here(),"Exports/study-level trim&fill_minority.pdf"), width = 8, height = 6)
      par(mar = c(6, 6, 2, 2))
      print_level_trimfill(model = model_study_level_all_outcomes_minority)
      
      pdf(file = file.path(here::here(),"Exports/study-level pcurve_minority.pdf"),
          width = 8, height = 6) 
      pcurve(model_study_level_all_outcomes_minority)
      
      dev.off()
      
    }
    
    Sys.time()
  },
  trigger = trigger(
    condition = isTRUE(F_EXPORT_DATA),
    mode = "condition"
  )
),

## 4.5 Study level diagnostic tests--------------------------------------------------------

eggers_test_study_level_results = eggers.test(model_study_level_all_outcomes_minority),

failsafe_study_level_results = gtools::smartbind(
  failsafe.test(data = data_all_outcomes_minority,
                level = "study",
                type = "Rosenthal"),
  failsafe.test(data = data_all_outcomes_minority,
                level = "study",
                type = "Orwin")
),

export_study_level_diagnostics = target(
  command = {
    if (isTRUE(F_EXPORT_DATA)) {
      message("Writing study-level model results")
      
      write.csv(eggers_test_study_level_results, file.path(here::here(), "Exports/study-level eggers_minority.csv"), row.names = F)
      write.csv(failsafe_study_level_results, file.path(here::here(), "Exports/study-level failsafe_minority.csv"), row.names = F)
    }
    
    Sys.time()
  },
  trigger = trigger(
    condition = isTRUE(F_EXPORT_DATA),
    mode = "condition"
  )
),

  
## 4.6 Cluster-level pooled analysis--------------------------------------------------------
model_cluster_level_all_outcomes_minority = run_meta_level(
  data = data_all_outcomes_minority,
  level = "cluster"
),

model_cluster_level_all_primary_outcomes_minority = run_meta_level(
  data = data_all_primary_outcomes_minority,
  level = "cluster"
),

model_cluster_level_all_secondary_outcomes_minority = run_meta_level(
  data = data_all_secondary_outcomes_minority,
  level = "cluster"
),

model_cluster_level_adjusted_outcomes_minority = run_meta_level(
  data = data_adjusted_outcomes_minority,
  level = "cluster"
),

model_cluster_level_adjusted_primary_outcomes_minority = run_meta_level(
  data = data_adjusted_primary_outcomes_minority,
  level = "cluster"
),

model_cluster_level_adjusted_secondary_outcomes_minority = run_meta_level(
  data = data_adjusted_secondary_outcomes_minority,
  level = "cluster"
),

combined_cluster_level_results_minority = print_level_results(model_cluster_level_all_outcomes_minority,
                                                                  model_cluster_level_all_primary_outcomes_minority,
                                                                  model_cluster_level_all_secondary_outcomes_minority,
                                                                  model_cluster_level_adjusted_outcomes_minority,
                                                                  model_cluster_level_adjusted_primary_outcomes_minority,
                                                                  model_cluster_level_adjusted_secondary_outcomes_minority),


model_cluster_level_all_outcomes_majority = run_meta_level(
  data = data_all_outcomes_majority,
  level = "cluster"
),

model_cluster_level_all_primary_outcomes_majority = run_meta_level(
  data = data_all_primary_outcomes_majority,
  level = "cluster"
),

model_cluster_level_all_secondary_outcomes_majority = run_meta_level(
  data = data_all_secondary_outcomes_majority,
  level = "cluster"
),

model_cluster_level_adjusted_outcomes_majority = run_meta_level(
  data = data_adjusted_outcomes_majority,
  level = "cluster"
),

model_cluster_level_adjusted_primary_outcomes_majority = run_meta_level(
  data = data_adjusted_primary_outcomes_majority,
  level = "cluster"
),

model_cluster_level_adjusted_secondary_outcomes_majority = run_meta_level(
  data = data_adjusted_secondary_outcomes_majority,
  level = "cluster"
),


combined_cluster_level_results_majority = print_level_results(model_cluster_level_all_outcomes_majority,
                                                                  model_cluster_level_all_primary_outcomes_majority,
                                                                  model_cluster_level_all_secondary_outcomes_majority,
                                                                  model_cluster_level_adjusted_outcomes_majority,
                                                                  model_cluster_level_adjusted_primary_outcomes_majority,
                                                                  model_cluster_level_adjusted_secondary_outcomes_majority),


export_cluster_level_results = target(
  command = {
    if (isTRUE(F_EXPORT_DATA)) {
      message("Writing cluster-level model results")
      
      write.csv(combined_cluster_level_results_minority, file.path(here::here(), "Exports/cluster-level results_minority.csv"), row.names = F)
      write.csv(combined_cluster_level_results_majority, file.path(here::here(), "Exports/cluster-level results_majority.csv"), row.names = F)
    }
    
    Sys.time()
  },
  trigger = trigger(
    condition = isTRUE(F_EXPORT_DATA),
    mode = "condition"
  )
),


## 4.7 Cluster-level plots--------------------------------------------------------

export_cluster_level_plots = target(
  command = {
    if (isTRUE(F_EXPORT_DATA)) {
      message("Writing cluster-level plots")
      
      pdf(file = file.path(here::here(),"Exports/cluster-level forestplot_minority.pdf"), 
          width = 10, height = 9)
      
      print_level_forest(model = model_cluster_level_all_outcomes_minority,
                               level = "cluster")
      
      pdf(file = file.path(here::here(),"Exports/cluster-level forestplot_majority.pdf"), 
          width = 10, height = 8)
      
      print_level_forest(model = model_cluster_level_all_outcomes_majority,
                               level = "cluster")
      
      pdf(file = file.path(here::here(),"Exports/cluster-level funnelplot_minority.pdf"), width = 18, height = 10)
      par(mar = c(6, 6, 2, 2))
      print_cluster_level_funnel(model = model_cluster_level_all_outcomes_minority)
      
      pdf(file = file.path(here::here(),"Exports/cluster-level trim&fill_minority.pdf"), width = 8, height = 6)
      par(mar = c(6, 6, 2, 2))
      print_level_trimfill(model = model_cluster_level_all_outcomes_minority)
      
      pdf(file=file.path(here::here(),"Exports/cluster-level pcurve_minority.pdf"),
          width = 8, height = 6) 
      pcurve(model_cluster_level_all_outcomes_minority)
      
      dev.off()
      
    }
    
    Sys.time()
  },
  trigger = trigger(
    condition = isTRUE(F_EXPORT_DATA),
    mode = "condition"
  )
),

## 4.8 Cluster-level diagnostic tests--------------------------------------------------------

eggers_test_cluster_level_results = eggers.test(model_cluster_level_all_outcomes_minority),

failsafe_cluster_level_results = gtools::smartbind(
  failsafe.test(data = data_all_outcomes_minority,
                level = "cluster",
                type = "Rosenthal"),
  failsafe.test(data = data_all_outcomes_minority,
                level = "cluster",
                type = "Orwin")
),

export_cluster_level_diagnostics = target(
  command = {
    if (isTRUE(F_EXPORT_DATA)) {
      message("Writing cluster-level model results")
      
      write.csv(eggers_test_cluster_level_results, file.path(here::here(), "Exports/cluster-level eggers_minority.csv"), row.names = F)
      write.csv(failsafe_cluster_level_results, file.path(here::here(), "Exports/cluster-level failsafe_minority.csv"), row.names = F)
    }
    
    Sys.time()
  },
  trigger = trigger(
    condition = isTRUE(F_EXPORT_DATA),
    mode = "condition"
  )
)

)

# 5. Subgroup analysis --------------------------------------------------------




