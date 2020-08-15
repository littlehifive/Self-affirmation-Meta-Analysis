# cleaning script

clean_master <- function(kost, purdie, turetsky, goyer){
   dat <- rbind(
     Baker2019(),
     Bancroft2017(),
     Bayly2017(),
     BinningUR(),
     Borman2012(),
     Borman2015(),
     Borman2016(),
     Borman2018(),
     Bowen2013(),
     Bratter2016(),
     Churchill2018(),
     Cohen2006(),
     Cohen2009(),
     Cook2012(),
     DeClercq2019(),
     Dee2015(),
     deJong2016(),
     Goyer2017(goyer),
     Gutmann2019(),
     Hadden2019(),
     Hanselman2014(),
     Hanselman2017(),
     Harackiewicz2014(),
     Harackiewicz2016(),
     Hayes2019(),
     Jordt2017(),
     Kim2019(),
     Kinias2016(),
     Kostsmith2010(),
     Kostsmith2012(kost),
     Lauer2013(),
     Lokhande2019(),
     Miyake2010(),
     Peters2017(),
     Powers2016(),
     Protzko2016(),
     PurdieGreenawayUR(purdie),
     Rapa2016(),
     Rozek2015(),
     Schwalbe2018(),
     SerraGarciaUR(),
     Sherman2013(),
     Shnabel2013(),
     Silverman2014(),
     Simmons2011(),
     Tibbetts2016(),
     Tibbetts2018(),
     TuretskyUR(turetsky),
     Woolf2009(),
     Wynne2011()
   )
 
  
# clean master data and create project clusters
   
dat <- dat %>% 
  as.data.frame() %>% 
  `colnames<-`( c("author", "year", "adapted", "type",  "outcome", 
                  "adjusted", "es", "v", "lowerCI", "upperCI")) %>% 
  mutate_at(vars(es:upperCI), as.numeric) %>% 
  mutate(
    study = paste(author, year, sep = ", "),
    se = sqrt(v)) %>%
  group_by(study) %>% 
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id = case_when(
    study %in% c("Hanselman (Study 1), 2017") ~ 25L,
    study %in% c("Hanselman (Study 2), 2017") ~ 26L,
    study %in% c("Hanselman, 2014") ~ 24L,
    TRUE ~ id)
  ) %>%
  mutate(cluster = case_when(
    study %in% c("Bancroft, 2017","Bratter, 2016") ~ "Bratter et al.",
    study %in% c("Cohen (Study 1), 2006","Cohen (Study 2), 2006","Cohen, 2009", 
                 "Cook (Study 1), 2012","Goyer (Study 2), 2017","Powers (Study 1), 2016",
                 "Powers (Study 2), 2016","Shnabel (Study 1), 2013") ~ "Cohen et al.",
    study %in% c("Harackiewicz, 2014","Tibbetts (Study 1a), 2016") ~ "Harackiewicz et al.",
    study %in% c("Kost-Smith, 2010","Kost-Smith, 2012","Miyake, 2010",
                 "Serra-Garcia (Study 1), under review") ~ "Miyake et al.",
    study %in% c("Borman, 2015","Borman, 2016","Borman, 2018","Hanselman, 2014",
                 "Hanselman (Study 1), 2017","Hanselman (Study 2), 2017","Rozek, 2015") ~ "MWAP",
    study %in% c("Goyer (Study 1), 2017","Sherman (Study 1), 2013") ~ "Sherman et al.",
    TRUE ~ study
    )) %>%
  mutate(type_s = gsub("(.*)(-)(.*)", "\\1", type),
         group = gsub("(.*)(-)(.*)", "\\3", type)) %>% 
  mutate(group_s = case_when(
    group %in% c("Black", "Hispanic", "Asian","URM", "White", "Black and Hispanic", 
                 "Turkish", "Arabic", "Eastern European", "White and Asian", 
                 "Black Caribbeans") ~ "Race",
    group %in% c("First generation","Continuing generation") ~ "First generation",
    group %in% c("URM or First generation", "URM and Female", "White and Male") ~ "Mixed",
    group %in% c("FSM","nonFSM") ~ "FSM status",
    group %in% c("Female","Male")  ~ "Gender",
    group %in% c("Blind") ~ "Disability",
    group %in% c("Main") ~ "None"
  )) %>% 
  #mutate(      
  #  # z-scored adapted for the use of moderator composite
  #  adapted_z = if_else(adapted == "Yes", 1, 0),
  #  adapted_z = binary_scale(1 - adapted_z)) %>%
  select(id, cluster, study, author, year, type_s, group, group_s, 
         adapted, outcome, adjusted, es, v, se, lowerCI, upperCI) %>% 
  mutate_at(vars(es:upperCI), round, digits = 4) %>%
  arrange(id)

return(dat)

}


clean_moderator <- function(mod){
  
  dat.mod <- mod %>%
    janitor::clean_names() %>%
    mutate(
      timing = case_when(
      early == 1 & before_stress == 0 ~ "early",
      early == 0 & before_stress == 1 ~ "before_stress",
      early == 1 & before_stress == 1 ~ "both",
      early == 0 & before_stress == 0 ~ "neither"),
    
      setting_type = recode(setting_type, 
                            `university` = "tertiary",
                            `high school` = "secondary",
                            `middle school` = "pre-secondary",
                            `primary school` = "primary")
    ) %>%
    mutate(
      
      # z-score promising moderators to create composites
      
      # timing
      timing_z = case_when(
        early == 1 & before_stress == 0 ~ 2L,
        early == 0 & before_stress == 1 ~ 1L, 
        early == 1 & before_stress == 1 ~ 1L,
        early == 0 & before_stress == 0 ~ 0L),
      timing_z = as.numeric(scale(timing_z)),
      
      # ordinary
      ordinary_z = binary_scale(ordinary),
      
      # duration
      duration_rough_month_z = as.numeric(scale(duration_rough_month)),
      
      # density_freelunch
      density_freelunch_z = as.numeric(scale(1 - density_freelunch)),
      
      # in class
      delivered_in_classroom_z = case_when(
        delivered_in_classroom == "class" ~ 2L,
        delivered_in_classroom == "online" ~ 1L, 
        delivered_in_classroom == "mixed" ~ 0L),
      delivered_in_classroom_z = as.numeric(scale(delivered_in_classroom_z)),
      
      # affirm other in control group
      
      control_type_z = if_else(control_type == "other", 1, 0),
      control_type_z = binary_scale(control_type_z)
        ) %>%
    select(id:before_stress, timing, random_sequence_generation:other_sources_of_bias, timing_z:control_type_z)
  
  return(dat.mod)
}
