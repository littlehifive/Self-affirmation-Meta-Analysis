# meta-regression

model.mods<-rma.mv(es, 
                   v, 
                   random = list( 
                     ~ 1 | cluster/study), 
                   tdist = TRUE, 
                   data = data_all_outcomes_minority %>% filter(group_s == "Gender" & 
                                                                cluster %in% c("Baker, 2019",
                                                                             "Gutmann, 2019",
                                                                             "Jordt, 2017",
                                                                             "Kim, 2019",
                                                                             "Kinias (Study 2), 2016",
                                                                             "Miyake et al.",
                                                                             "Serra-Garcia (Study 2), under review",
                                                                             "Turetsky, under review")),
                   method = "REML")
model.mods


model.mods<-rma.mv(es, 
                   v, 
                   random = list( 
                     ~ 1 | cluster/study), 
                   tdist = TRUE, 
                   data = data_all_outcomes_minority,
                   method = "REML",
                   mods = ~ adapted - 1)
model.mods

model.mods<-rma.mv(es, 
                   v, 
                   random = list( 
                     ~ 1 | cluster/study), 
                   tdist = TRUE, 
                   data = data_all_outcomes_minority,
                   method = "REML",
                   mods = ~ duration_rough_month + number_rough)
model.mods


data_all_outcomes_minority <- data_all_outcomes_minority %>%
  mutate(before_stress = if_else(timing == "before_stress", 1, 0),
         both = if_else(timing == "both", 1, 0),
         early = if_else(timing == "early", 1, 0),
         neither = if_else(timing == "neither", 1, 0)
         )

data_all_outcomes_minority$timing <- factor(data_all_outcomes_minority$timing,
                                            levels = c( "early", "neither","before_stress","both"))
model.mods<-rma.mv(es, 
                    v, 
                    random = list( 
                          ~ 1 | cluster), 
                    tdist = TRUE, 
                    data = data_all_outcomes_minority,
                    method = "REML",
                   mods = ~ early + before_stress)
model.mods

model.mods<-rma.mv(es, 
                   v, 
                   random = list( 
                     ~ 1 | cluster/study), 
                   tdist = TRUE, 
                   data = data_all_outcomes_minority,
                   method = "REML",
                   mods = ~ duration_rough_month + ordinary + adapted + timing + delivered_in_classroom + density_freelunch)
model.mods

# exploratory analysis
dat$fidelity_implementation <- rowSums(dat[,c("ordinary_z", 
                                              "duration_rough_month_z")], na.rm = T)
dat$fidelity_implementation_z <- as.numeric(scale(dat$fidelity_implementation))

dat$fidelity_contextual <- rowSums(dat[,c("density_freelunch_z", "control_residual_gap_z")], na.rm = T)
dat$fidelity_contextual_z <- as.numeric(scale(dat$fidelity_contextual))


dat.s <- dat %>%
  filter(type_s %in% c("Minority subgroup", "Interaction"),
         !group %in% c("Male", "Asian","White", "nonFSM", "Continuing generation", "White and Asian"))


implementation <-rma.mv(es, 
                        v, 
                        random = list(~ 1 | cluster/study), 
                        tdist = TRUE, 
                        data = dat.s,
                        method = "REML",
                        mods = ~  fidelity_implementation_z)
summary(implementation, digits = 5)

contextual <-rma.mv(es, 
                        v, 
                        random = list(~ 1 | cluster/study), 
                        tdist = TRUE, 
                        data = dat.s,
                        method = "REML",
                        mods = ~  fidelity_contextual_z)
summary(contextual, digits = 5)


dat.s$fidelity_implementation_p1sd <- dat.s$fidelity_implementation_z + sd(dat.s$fidelity_implementation_z,na.rm = T)

dat.s$fidelity_implementation_m1sd <- dat.s$fidelity_implementation_z - sd(dat.s$fidelity_implementation_z,na.rm = T)

dat.s$fidelity_contextual_p1sd <- dat.s$fidelity_contextual_z + sd(dat.s$fidelity_contextual_z,na.rm = T)

dat.s$fidelity_contextual_m1sd <- dat.s$fidelity_contextual_z - sd(dat.s$fidelity_contextual_z,na.rm = T)


model.mods<-rma.mv(es, 
                   v, 
                   random = list(~ 1 | cluster/study), 
                   tdist = TRUE, 
                   data = dat.s,
                   method = "REML",
                   mods = ~  fidelity_implementation_p1sd + fidelity_contextual_z)
summary(model.mods, digits = 5)

model.mods<-rma.mv(es, 
                   v, 
                   random = list(~ 1 | cluster), 
                   tdist = TRUE, 
                   data = dat.s,
                   method = "REML",
                   mods = ~  fidelity_implementation_m1sd + fidelity_contextual_z)
summary(model.mods, digits = 5)

model.mods<-rma.mv(es, 
                   v, 
                   random = list(~ 1 | cluster), 
                   tdist = TRUE, 
                   data = dat.s,
                   method = "REML",
                   mods = ~  fidelity_contextual_p1sd + fidelity_implementation_z)
summary(model.mods, digits = 5)

model.mods<-rma.mv(es, 
                   v, 
                   random = list(~ 1 | cluster), 
                   tdist = TRUE, 
                   data = dat.s,
                   method = "REML",
                   mods = ~  fidelity_contextual_m1sd + fidelity_implementation_z)
summary(model.mods, digits = 5)


model.mods<-rma.mv(es, 
           v, 
           random = list(~ 1 | cluster), 
           tdist = TRUE, 
           data = dat.s,
           method = "REML",
           mods = ~  fidelity_implementation_m1sd + fidelity_contextual_m1sd)
summary(model.mods, digits = 5)

model.mods<-rma.mv(es, 
           v, 
           random = list(~ 1 | cluster), 
           tdist = TRUE, 
           data = dat.s,
           method = "REML",
           mods = ~  fidelity_implementation_p1sd + fidelity_contextual_p1sd)
summary(model.mods, digits = 5)


