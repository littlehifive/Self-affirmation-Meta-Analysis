# additional models

# as per editor's request, create percentage of "threatened students" as a moderator
# to match the targeted threatening status in the study

#purdie.s <- purdie[is.na(purdie$Condition) == F,] # 74 in affirmation & control
#purdie.s$Condition <- ifelse(purdie.s$Condition == 1, "Affirm", "Control")
#prop.table(table(purdie.s$Race, purdie.s$Gender))

# prop.table(table(turetsky$Race, turetsky$Gender))

data_all_outcomes_minority <- data_all_outcomes_minority %>%
  mutate(density_dis = case_when(
    identity_type == "first-generation" ~ density_firstgen,
    identity_type == "gender" ~ density_female,
    identity_type == "race" ~ density_urm,
    identity_type == "FSM" ~ density_freelunch,
    study == "Baker, 2019" ~ density_urm,
    study == "Borman, 2012" ~ density_urm,
    study == "Harackiewicz, 2016" ~ density_firstgen,
    study == "Jordt, 2017" ~ density_urm,
    study == "Lokhande, 2019" ~ density_urm,
    study == "Purdie-Greenaway, under review" ~ 0.8918919,
    study == "Turetsky, under review" ~ 0.9014085,
    study == "Silverman (Study 2), 2014" ~ 1)) # sample has 100% blind students


model.mods<-rma.mv(es, 
                   v, 
                   random = list( 
                     ~ 1 | cluster/study), 
                   tdist = TRUE, 
                   data = data_all_outcomes_minority,
                   method = "REML",
                   mods = ~ density_dis)
model.mods # no moderation effect


# check if studies with gender as the threatening identity have large effect size
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

# percentage of free lunch status only for non-college students
model.mods<-rma.mv(es, 
                   v, 
                   random = list( 
                     ~ 1 | cluster/study), 
                   tdist = TRUE, 
                   data = data_all_outcomes_minority[data_all_outcomes_minority$setting_type != "tertiary",],
                   method = "REML",
                   mods = ~ density_freelunch)
model.mods

# duration controlling for number
model.mods<-rma.mv(es, 
                   v, 
                   random = list( 
                     ~ 1 | cluster/study), 
                   tdist = TRUE, 
                   data = data_all_outcomes_minority,
                   method = "REML",
                   mods = ~ duration_rough_month + number_rough)
model.mods

# interaction between resources and gap size
data_all_outcomes_minority$resources <- 1-data_all_outcomes_minority$density_freelunch
model.mods<-rma.mv(es, 
                   v, 
                   random = list( 
                     ~ 1 | cluster/study), 
                   tdist = TRUE, 
                   data = data_all_outcomes_minority[data_all_outcomes_minority$setting_type != "tertiary",],
                   method = "REML",
                   mods = ~ control_residual_gap * resources)
model.mods

data_all_outcomes_minority$resources_z <- as.numeric(scale(data_all_outcomes_minority$resources))
data_all_outcomes_minority$control_residual_gap_z <- as.numeric(scale(data_all_outcomes_minority$control_residual_gap))

data_all_outcomes_minority$resources_p1sd <- data_all_outcomes_minority$resources_z + sd(data_all_outcomes_minority$resources_z, na.rm = T)
data_all_outcomes_minority$resources_m1sd <- data_all_outcomes_minority$resources_z - sd(data_all_outcomes_minority$resources_z, na.rm = T)
data_all_outcomes_minority$control_residual_gap_p1sd <- data_all_outcomes_minority$control_residual_gap_z + sd(data_all_outcomes_minority$control_residual_gap_z, na.rm = T)
data_all_outcomes_minority$control_residual_gap_m1sd <- data_all_outcomes_minority$control_residual_gap_z - sd(data_all_outcomes_minority$control_residual_gap_z, na.rm = T)

# simple effect test
model.mods<-rma.mv(es, 
                   v, 
                   random = list( 
                     ~ 1 | cluster/study), 
                   tdist = TRUE, 
                   data = data_all_outcomes_minority[data_all_outcomes_minority$setting_type != "tertiary",],
                   method = "REML",
                   mods = ~ control_residual_gap_m1sd + resources_m1sd)
model.mods

model.mods<-rma.mv(es, 
                   v, 
                   random = list( 
                     ~ 1 | cluster/study), 
                   tdist = TRUE, 
                   data = data_all_outcomes_minority[data_all_outcomes_minority$setting_type != "tertiary",],
                   method = "REML",
                   mods = ~ control_residual_gap_p1sd + resources_p1sd)
model.mods


# simple slope plot
test <- data_all_outcomes_minority %>% select(es, resources, control_residual_gap) %>% na.omit()

x <- test$resources
test$resources_3group <-
  case_when(x > mean(x,na.rm = T)+sd(x, na.rm = T) ~ "high (+1SD)",
            x < mean(x,na.rm = T)+sd(x, na.rm = T) & x > mean(x, na.rm = T)-sd(x, na.rm = T) ~ "average",
            x < mean(x,na.rm = T)-sd(x, na.rm = T) ~ "low (-1SD)")

test$resources_3group <- factor(test$resources_3group, levels = c("low (-1SD)", "average", "high (+1SD)"))

ggplot(test,aes(x = control_residual_gap, y = es, group = resources_3group, linetype = resources_3group)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", fill = NA, fullrange = T, color = "black") + coord_cartesian(ylim = c(-0.5,1))+
  labs(linetype = "Percentage ineligible\nfor free/reduced meal/lunch\n(availability of resources)", 
       x = "\nResidual gap in control group\n(presence of psychological threat)", 
       y = "Affirmation effect among\ndisadvantaged students\n") +
  scale_linetype_manual( values = c("dotted", "dashed", "solid")) +
  theme_bw() +
  theme(    legend.position = c(.95, 0.06),
            legend.justification = c("right", "bottom"),
            legend.box.just = "right",
            legend.margin = margin(6, 6, 6, 6),
            legend.box.background = element_rect(colour = "black"))

# timing
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



# exploratory analysis
dat$fidelity_implementation <- rowSums(dat[,c("ordinary_z", 
                                              "duration_rough_month_z")], na.rm = T)
dat$fidelity_implementation_z <- as.numeric(scale(dat$fidelity_implementation))

dat$fidelity_contextual <- rowSums(dat[,c("density_freelunch_z", "control_both_gap_z")], na.rm = T)
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


# multimodal inference
# library(dmetar)
#multimodel.inference(TE = "es", 
#                     seTE = "se",
#                     data = data_all_outcomes_minority,
#                     predictors = c("density_freelunch", "ordinary", "duration_rough_month","adapted"),
#                     interaction = TRUE)

