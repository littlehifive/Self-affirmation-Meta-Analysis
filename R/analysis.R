# Main analysis of average treatment effect

# run multilevel analysis
run_meta_mlm <- function(data, random){
  
  full.model <- metafor::rma.mv(es, 
                              v, 
                              random = random, # e.g. "cluster"
                              tdist = TRUE, 
                              data = data,
                              method = "REML")
  results <- summary(full.model)
  
  output <- coef(results) %>% 
    mutate(
          Qdf = results$dfs,
          Q = results$QE,
          Qp = results$QEp) %>%
    round(digits = 5)
  
  return(output)
  
}

# Multilevel models 
get_mlm_results <- function(dat){
  
  # all outcomes
  
results_mlm_all_outcomes =  
  run_meta_mlm(
    data = dat %>%
      filter(type_s %in% c("Minority subgroup", "Interaction"),
             !group %in% c("White", "Asian","Male","White and Asian", "Continuing generation")),
    random = list(
      ~ 1 | cluster
    )
  )

results_mlm_all_primary_outcome =
  run_meta_mlm(
    data = dat %>%
      filter(type_s %in% c("Minority subgroup", "Interaction"),
             !group %in% c("White", "Asian","Male","White and Asian", "Continuing generation"),
             grepl("GPA",outcome)),
    random = list(
      ~ 1 | cluster
    )
  )

results_mlm_all_secondary_outcomes =
  run_meta_mlm(
    data = dat %>%
      filter(type_s %in% c("Minority subgroup", "Interaction"),
             !group %in% c("White", "Asian","Male","White and Asian", "Continuing generation"),
             !grepl("GPA",outcome)),
    random = list(
      ~ 1 | cluster
    )
  )


# adjusted outcomes only

results_mlm_adjusted_outcomes =  
  run_meta_mlm(
    data = dat %>%
      filter(adjusted == "Yes",
             type_s %in% c("Minority subgroup", "Interaction"),
             !group %in% c("White", "Asian","Male","White and Asian", "Continuing generation")),
    random = list(
      ~ 1 | cluster
    )
  )

results_mlm_adjusted_primary_outcome =
  run_meta_mlm(
    data = dat %>%
      filter(adjusted == "Yes",
             type_s %in% c("Minority subgroup", "Interaction"),
             !group %in% c("White", "Asian","Male","White and Asian", "Continuing generation"),
             grepl("GPA",outcome)),
    random = list(
      ~ 1 | cluster
    )
  )

results_mlm_adjusted_secondary_outcomes =
  run_meta_mlm(
    data = dat %>%
      filter(adjusted == "Yes",
             type_s %in% c("Minority subgroup", "Interaction"),
             !group %in% c("White", "Asian","Male","White and Asian", "Continuing generation"),
             !grepl("GPA",outcome)),
    random = list(
      ~ 1 | cluster
    )
  )

combined_mlm_results = bind_rows(
  results_mlm_all_outcomes,
  results_mlm_all_primary_outcome,
  results_mlm_all_secondary_outcomes,
  results_mlm_adjusted_outcomes,
  results_mlm_adjusted_primary_outcome,
  results_mlm_adjusted_secondary_outcomes) %>%
  add_column(type = c("all both", "all primary", "all secondary",
               "adjusted both", "adjusted primary", "adjusted secondary"), .before = 1)
  
  return(combined_mlm_results)
}


# run study level pooled models

run_meta_study_level_for_plots <- function(data, level){
  
  dat.s.pool <- data %>% 
    group_by(study) %>% summarise(es = mean(es),
                                  se = mean(se))
  
  model <- meta::metagen(es,
                         se,
                         data = dat.s.pool,
                         studlab = level,
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         method.tau = "SJ",
                         hakn = TRUE,
                         prediction=TRUE,
                         sm="g")
  
  return(model)
}

run_meta_study_level <- function(data, level){
  
  dat.s.pool <- data %>% 
    group_by(eval(as.name(!!level))) %>% summarize(es = mean(es),
                                  se = mean(se),
                                  .groups = "drop_last") 
  names(dat.s.pool)[1] <- level # rename
  
  model <- meta::metagen(es,
                         se,
                         data = dat.s.pool,
                         studlab = eval(as.name(level)),
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         method.tau = "SJ",
                         hakn = TRUE,
                         prediction=TRUE,
                         sm="g")
  
  results <- summary(model)
  
  output <- as.data.frame(results$random) %>% 
    mutate(
      Qdf = results$df.Q,
      Q = results$Q,
      Qp = model$pval.Q,
      pred.lb = results$predict$lower,
      pred.lb = results$predict$upper,
      I2 = results$I2$TE,
      I2.lb = results$I2$lower,
      I2.ub = results$I2$upper
      ) %>%
    round(digits = 5)
  
  return(output)
}

get_study_level_results <- function(dat){
  
  # all outcomes
  
  results_study_level_all_outcomes =  
    run_meta_study_level(
      data = dat %>%
        filter(type_s %in% c("Minority subgroup", "Interaction"),
               !group %in% c("White", "Asian","Male","White and Asian", "Continuing generation")),
      level = "study"
    )
  
  results_study_level_all_primary_outcome =
    run_meta_study_level(
      data = dat %>%
        filter(type_s %in% c("Minority subgroup", "Interaction"),
               !group %in% c("White", "Asian","Male","White and Asian", "Continuing generation"),
               grepl("GPA",outcome)),
      level = "study"
    )
  
  results_study_level_all_secondary_outcomes =
    run_meta_study_level(
      data = dat %>%
        filter(type_s %in% c("Minority subgroup", "Interaction"),
               !group %in% c("White", "Asian","Male","White and Asian", "Continuing generation"),
               !grepl("GPA",outcome)),
      level = "study"
    )
  
  
  # adjusted outcomes only
  
  results_study_level_adjusted_outcomes =  
    run_meta_study_level(
      data = dat %>%
        filter(adjusted == "Yes",
               type_s %in% c("Minority subgroup", "Interaction"),
               !group %in% c("White", "Asian","Male","White and Asian", "Continuing generation")),
      level = "study"
    )
  
  results_study_level_adjusted_primary_outcome =
    run_meta_study_level(
      data = dat %>%
        filter(adjusted == "Yes",
               type_s %in% c("Minority subgroup", "Interaction"),
               !group %in% c("White", "Asian","Male","White and Asian", "Continuing generation"),
               grepl("GPA",outcome)),
      level = "study"
    )
  
  results_study_level_adjusted_secondary_outcomes =
    run_meta_study_level(
      data = dat %>%
        filter(adjusted == "Yes",
               type_s %in% c("Minority subgroup", "Interaction"),
               !group %in% c("White", "Asian","Male","White and Asian", "Continuing generation"),
               !grepl("GPA",outcome)),
      level = "study"
    )
  
  combined_study_level_results = bind_rows(
    results_study_level_all_outcomes,
    results_study_level_all_primary_outcome,
    results_study_level_all_secondary_outcomes,
    results_study_level_adjusted_outcomes,
    results_study_level_adjusted_primary_outcome,
    results_study_level_adjusted_secondary_outcomes) %>%
    add_column(type = c("all both", "all primary", "all secondary",
                        "adjusted both", "adjusted primary", "adjusted secondary"), .before = 1)
  
  return(combined_study_level_results)
  
}

