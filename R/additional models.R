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
dat$fidelity_implementation <- rowSums(dat[,c("timing_z","ordinary_z", 
                                                      "duration_rough_month_z", "density_freelunch_z",
                                                      "delivered_in_classroom_z", "control_type_z")], na.rm = T)

dat$fidelity_contextual <- dat$density_freelunch_z

dat.s <- dat %>%
  filter(type_s %in% c("Minority subgroup", "Interaction"),
         !group %in% c("Male", "Asian","White", "nonFSM", "Continuing generation", "White and Asian"))


implementation <-rma.mv(es, 
                        v, 
                        random = list(~ 1 | cluster/study), 
                        tdist = TRUE, 
                        data = dat.s,
                        method = "REML",
                        mods = ~  fidelity_implementation)
summary(implementation, digits = 5)

contextual <-rma.mv(es, 
                        v, 
                        random = list(~ 1 | cluster/study), 
                        tdist = TRUE, 
                        data = dat.s,
                        method = "REML",
                        mods = ~  fidelity_contextual)
summary(contextual, digits = 5)


dat.s$fidelity_implementation_p1sd <- dat.s$fidelity_implementation + sd(dat.s$fidelity_implementation,na.rm = T)

dat.s$fidelity_implementation_m1sd <- dat.s$fidelity_implementation - sd(dat.s$fidelity_implementation,na.rm = T)

dat.s$fidelity_contextual_p1sd <- dat.s$fidelity_contextual + sd(dat.s$fidelity_contextual,na.rm = T)

dat.s$fidelity_contextual_m1sd <- dat.s$fidelity_contextual - sd(dat.s$fidelity_contextual,na.rm = T)


model.mods<-rma.mv(es, 
                   v, 
                   random = list(~ 1 | cluster/study), 
                   tdist = TRUE, 
                   data = dat.s,
                   method = "REML",
                   mods = ~  fidelity_implementation_p1sd)
summary(model.mods, digits = 5)

model.mods<-rma.mv(es, 
                   v, 
                   random = list(~ 1 | cluster), 
                   tdist = TRUE, 
                   data = dat.s,
                   method = "REML",
                   mods = ~  fidelity_implementation_m1sd)
summary(model.mods, digits = 5)

model.mods<-rma.mv(es, 
                   v, 
                   random = list(~ 1 | cluster), 
                   tdist = TRUE, 
                   data = dat.s,
                   method = "REML",
                   mods = ~  fidelity_contextual_p1sd)
summary(model.mods, digits = 5)

model.mods<-rma.mv(es, 
                   v, 
                   random = list(~ 1 | cluster), 
                   tdist = TRUE, 
                   data = dat.s,
                   method = "REML",
                   mods = ~  fidelity_contextual_m1sd)
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


