# Main analysis of average treatment effect

# run multilevel analysis
run_meta_mlm <- function(data, random){
  
  model <- metafor::rma.mv(es, 
                              v, 
                              random = random, # e.g. "cluster"
                              tdist = TRUE, 
                              data = data,
                              method = "REML")
  
  return(model)

}

extract_mlm_results <- function(model){
  
  results <- summary(model)
  
  output <- coef(results) %>% 
    mutate(
      Qdf = results$dfs,
      Q = results$QE,
      Qp = results$QEp) %>%
    round(digits = 5)
  
  return(output)
  
}


# Multilevel models 
print_mlm_results <- function(model_mlm_all_outcomes,
                              model_mlm_all_primary_outcomes,
                              model_mlm_all_secondary_outcomes,
                              model_mlm_adjusted_outcomes,
                              model_mlm_adjusted_primary_outcomes,
                              model_mlm_adjusted_secondary_outcomes){
  # all outcomes
  
results_mlm_all_outcomes =  
  extract_mlm_results(model_mlm_all_outcomes)

results_mlm_all_primary_outcomes = 
  extract_mlm_results(model_mlm_all_primary_outcomes)
  
results_mlm_all_secondary_outcomes =
  extract_mlm_results(model_mlm_all_secondary_outcomes)

# adjusted outcomes only

results_mlm_adjusted_outcomes =  
  extract_mlm_results(model_mlm_adjusted_outcomes)

results_mlm_adjusted_primary_outcomes =
  extract_mlm_results(model_mlm_adjusted_primary_outcomes)

results_mlm_adjusted_secondary_outcomes =
  extract_mlm_results(model_mlm_adjusted_secondary_outcomes)

combined_mlm_results = bind_rows(
  results_mlm_all_outcomes,
  results_mlm_all_primary_outcomes,
  results_mlm_all_secondary_outcomes,
  results_mlm_adjusted_outcomes,
  results_mlm_adjusted_primary_outcomes,
  results_mlm_adjusted_secondary_outcomes) %>%
  add_column(type = c("all both", "all primary", "all secondary",
               "adjusted both", "adjusted primary", "adjusted secondary"), .before = 1)
  
  return(combined_mlm_results)
}

# run study/cluster level pooled models

run_meta_level <- function(data, level){
  
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
  
  return(model)
  
}

extract_level_results <- function(model){
  
  results <- summary(model)
  
  output <- as.data.frame(results$random) %>% 
    mutate(
      Qdf = results$df.Q,
      Q = results$Q,
      Qp = model$pval.Q,
      pred.lb = results$predict$lower,
      pred.ub = results$predict$upper,
      I2 = results$I2$TE,
      I2.lb = results$I2$lower,
      I2.ub = results$I2$upper
      ) %>%
    round(digits = 5)
  
  return(output)
}

print_level_results <- function(model_level_all_outcomes,
                                model_level_all_primary_outcomes,
                                model_level_all_secondary_outcomes,
                                model_level_adjusted_outcomes,
                                model_level_adjusted_primary_outcomes,
                                model_level_adjusted_secondary_outcomes){

  # all outcomes
  
  results_level_all_outcomes =  
    extract_level_results(model_level_all_outcomes)
  
  results_level_all_primary_outcomes = 
    extract_level_results(model_level_all_primary_outcomes)
    
  
  results_level_all_secondary_outcomes = 
    extract_level_results(model_level_all_secondary_outcomes)
    
  # adjusted outcomes only
  
  results_level_adjusted_outcomes = 
    extract_level_results(model_level_adjusted_outcomes)
    
  
  results_level_adjusted_primary_outcomes =
    extract_level_results(model_level_adjusted_primary_outcomes)
  
  results_level_adjusted_secondary_outcomes =
    extract_level_results(model_level_adjusted_secondary_outcomes)
  
  combined_level_results = bind_rows(
    results_level_all_outcomes,
    results_level_all_primary_outcomes,
    results_level_all_secondary_outcomes,
    results_level_adjusted_outcomes,
    results_level_adjusted_primary_outcomes,
    results_level_adjusted_secondary_outcomes) %>%
    add_column(type = c("all both", "all primary", "all secondary",
                        "adjusted both", "adjusted primary", "adjusted secondary"), .before = 1)
  
  return(combined_level_results)
  
}

# forest plot

print_level_forest <- function(model, level){
  
  forest <- meta::forest(model,
               leftlabs = c(level, "TE","seTE"),
               rightlabs = c("g","95% CI","weight"),
               text.random = "Overall effect",
               print.tau2 = FALSE,
               smlab = "")
  
  return(forest)
}

# this is specific to study level
print_study_level_funnel <- function(model){

  # some manual editing here to avoid overlapping texts 
  # because base plot does not have built functions to avoid overlap

  meta::funnel(model,
               xlab = "Hedges' g",
               ylab = "Reverse-scaled Standard Errors\n(Top = More Precision)",
               studlab = F, cex.lab = 1.2, font.lab = 2)

  studlist <-c("Harackiewicz, 2014",
               "Tibbetts (Study 1a), 2016",
               "Schwalbe, 2018",
               "Bayly, 2017",
               "Dee, 2015",
               "Hanselman (Study 2), 2017",
               "Borman, 2012",
               "Baker, 2019",
               "Hanselman (Study 1), 2017",
               "Cohen (Study 1), 2006",
               "Protzko, 2016",
               "Lokhande, 2019",
               "Kinias (Study 2), 2016",
               "Woolf, 2009",
               "Borman, 2016")
               
  text(model$TE[!model$studlab %in% studlist],
       model$seTE[!model$studlab %in% studlist]+0.005,
       model$studlab[!model$studlab %in% studlist],cex=.8)

  text(model$TE[model$studlab %in% c("Harackiewicz, 2014")]+0.065,
       model$seTE[model$studlab %in% c("Harackiewicz, 2014")],
       model$studlab[model$studlab %in% c("Harackiewicz, 2014")],cex=.8)

  text(model$TE[model$studlab %in% c("Tibbetts (Study 1a), 2016")]+0.085,
       model$seTE[model$studlab %in% c("Tibbetts (Study 1a), 2016")],
       model$studlab[model$studlab %in% c("Tibbetts (Study 1a), 2016")],cex=.8)

  text(model$TE[model$studlab %in% c("Schwalbe, 2018")]+0.055,
       model$seTE[model$studlab %in% c("Schwalbe, 2018")],
       model$studlab[model$studlab %in% c("Schwalbe, 2018")],cex=.8)

  text(model$TE[model$studlab %in% c("Bayly, 2017")]-0.041,
       model$seTE[model$studlab %in% c("Bayly, 2017")],
       model$studlab[model$studlab %in% c("Bayly, 2017")],cex=.8)

  text(model$TE[model$studlab %in% c("Dee, 2015")],
       model$seTE[model$studlab %in% c("Dee, 2015")]-0.006,
       model$studlab[model$studlab %in% c("Dee, 2015")],cex=.8)

  text(model$TE[model$studlab %in% c("Hanselman (Study 2), 2017")]-0.088,
       model$seTE[model$studlab %in% c("Hanselman (Study 2), 2017")],
       model$studlab[model$studlab %in% c("Hanselman (Study 2), 2017")],cex=.8)

  text(model$TE[model$studlab %in% c("Borman, 2012")],
       model$seTE[model$studlab %in% c("Borman, 2012")]-0.006,
       model$studlab[model$studlab %in% c("Borman, 2012")],cex=.8)

  text(model$TE[model$studlab %in% c("Baker, 2019")]+0.042,
       model$seTE[model$studlab %in% c("Baker, 2019")],
       model$studlab[model$studlab %in% c("Baker, 2019")],cex=.8)

  text(model$TE[model$studlab %in% c("Hanselman (Study 1), 2017")]-0.085,
       model$seTE[model$studlab %in% c("Hanselman (Study 1), 2017")],
       model$studlab[model$studlab %in% c("Hanselman (Study 1), 2017")],cex=.8)

  text(model$TE[model$studlab %in% c("Cohen (Study 1), 2006")],
       model$seTE[model$studlab %in% c("Cohen (Study 1), 2006")]-0.006,
       model$studlab[model$studlab %in% c("Cohen (Study 1), 2006")],cex=.8)

  text(model$TE[model$studlab %in% c("Protzko, 2016")]+0.05,
       model$seTE[model$studlab %in% c("Protzko, 2016")],
       model$studlab[model$studlab %in% c("Protzko, 2016")],cex=.8)

  text(model$TE[model$studlab %in% c("Lokhande, 2019")]+0.055,
       model$seTE[model$studlab %in% c("Lokhande, 2019")],
       model$studlab[model$studlab %in% c("Lokhande, 2019")],cex=.8)

  text(model$TE[model$studlab %in% c("Kinias (Study 2), 2016")]+0.07,
       model$seTE[model$studlab %in% c("Kinias (Study 2), 2016")],
       model$studlab[model$studlab %in% c("Kinias (Study 2), 2016")],cex=.8)
  
  text(model$TE[model$studlab %in% c("Woolf, 2009")]-0.06,
       model$seTE[model$studlab %in% c("Woolf, 2009")],
       model$studlab[model$studlab %in% c("Woolf, 2009")],cex=.8)

  text(model$TE[model$studlab %in% c("Borman, 2016")]+0.05,
       model$seTE[model$studlab %in% c("Borman, 2016")],
       model$studlab[model$studlab %in% c("Borman, 2016")],cex=.8)
}

print_cluster_level_funnel <- function(model){
  
  # some manual editing here to avoid overlapping texts 
  # because base plot does not have built functions to avoid overlap
  
  meta::funnel(model,
               xlab = "Hedges' g",
               ylab = "Reverse-scaled Standard Errors\n(Top = More Precision)",
               studlab = F, cex.lab = 1.2, font.lab = 2)
  
  studlist <-c("Baker, 2019",
               "Borman, 2012",
               "MWAP",
               "Bratter et al.",
               "Bayly, 2017",
               "Kim, 2019",
               "Harackiewicz et al.",
               "Protzko, 2016"
               )
  
  text(model$TE[!model$studlab %in% studlist],
       model$seTE[!model$studlab %in% studlist]+0.005,
       model$studlab[!model$studlab %in% studlist],cex=.8)
  
  text(model$TE[model$studlab %in% c("Baker, 2019")]+0.04,
       model$seTE[model$studlab %in% c("Baker, 2019")]+0.002,
       model$studlab[model$studlab %in% c("Baker, 2019")],cex=.8)
  
  text(model$TE[model$studlab %in% c("Borman, 2012")]+0.05,
       model$seTE[model$studlab %in% c("Borman, 2012")],
       model$studlab[model$studlab %in% c("Borman, 2012")],cex=.8)
  
  text(model$TE[model$studlab %in% c("MWAP")],
       model$seTE[model$studlab %in% c("MWAP")]+0.005,
       model$studlab[model$studlab %in% c("MWAP")],cex=.8)
  
  text(model$TE[model$studlab %in% c("Bratter et al.")]+0.04,
       model$seTE[model$studlab %in% c("Bratter et al.")],
       model$studlab[model$studlab %in% c("Bratter et al.")],cex=.8)
  
  text(model$TE[model$studlab %in% c("Bayly, 2017")]-0.038,
       model$seTE[model$studlab %in% c("Bayly, 2017")],
       model$studlab[model$studlab %in% c("Bayly, 2017")],cex=.8)

  text(model$TE[model$studlab %in% c("Kim, 2019")]-0.038,
       model$seTE[model$studlab %in% c("Kim, 2019")],
       model$studlab[model$studlab %in% c("Kim, 2019")],cex=.8)
  
  text(model$TE[model$studlab %in% c("Harackiewicz et al.")],
       model$seTE[model$studlab %in% c("Harackiewicz et al.")]-0.006,
       model$studlab[model$studlab %in% c("Harackiewicz et al.")],cex=.8)
  
  text(model$TE[model$studlab %in% c("Protzko, 2016")]+0.05,
       model$seTE[model$studlab %in% c("Protzko, 2016")],
       model$studlab[model$studlab %in% c("Protzko, 2016")],cex=.8)
  

}



