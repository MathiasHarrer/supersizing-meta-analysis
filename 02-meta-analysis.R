# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                             #
#  FEATURES AND FINDINGS OF THE METAPSY META-ANALYTIC RESEARCH DOMAIN         #
#  FOR PSYCHOLOGICAL TREATMENTS                                               #
#                                                                             #
#  II. Meta-Analysis ----                                                     
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## 0. Load dependencies and define models to be run ----------------------------

library(tidyverse)
library(xlsx)
library(metapsyTools)
source("utils/utils.R")

modelList = c("threelevel.che", "combined", "outliers",
              "influence", "lowest.highest", "rob")


## 1. Meta-Analyses ------------------------------------------------------------

### 1.1 Depression -------------------------------------------------------------

#### 1.1.1 Pooled Effect -------------------------------------------------------

# Load data
dat.dep = read.csv("data/depression.csv") %>% filter(format != "ush")

# Run meta-analysis and correct publication bias
m.dep = runMetaAnalysis(dat.dep, which.run = modelList,
                        which.outliers = "combined", 
                        which.influence = "combined",
                        which.rob = "combined", low.rob.filter = "rob > 3",
                        nnt.cer = .18) %>% 
          correctPublicationBias(which.run = "combined")

# Save the model & results
save(m.dep, file="results/depression/m.dep.rda")
write.xlsx(m.dep$summary, file = "results/depression/results.xlsx", 
           sheetName = "pooled_effects")
write.xlsx(m.dep$model.threelevel.che.var.comp, 
           file = "results/depression/results.xlsx", 
           sheetName = "variance_components", append = TRUE)
write.xlsx(m.dep$correctPublicationBias$summary, 
           file = "results/depression/results.xlsx", 
           sheetName = "publication_bias", append = TRUE)

# Generate profile plots of the variance components
pdf("results/depression/profile_che_model.pdf")
metafor::profile.rma.mv(m.dep$model.threelevel.che)
dev.off()


#### 1.1.2 Subgroup Analysis ---------------------------------------------------

rbind(
  dat.dep %>% 
    filterPoolingData(condition_arm2 %in% c("wl", "wlc")) %>% 
    runMetaAnalysis(which.run = "combined") %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "wl"; .$summary},
  dat.dep %>% 
    filterPoolingData(condition_arm2 %in% "cau") %>% 
    runMetaAnalysis(which.run = "combined") %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "cau"; .$summary}
) -> m.dep.sg

# Save the results
write.xlsx(m.dep.sg, file = "results/depression/results.xlsx", 
           sheetName = "subgroup_analysis", append = TRUE)


#### 1.1.3 Study Characteristics -----------------------------------------------

# Number of participants (intervention, control, overall)
dat.dep.sum = m.dep$model.combined$data
with(dat.dep.sum, {cbind(n_arm1, n_change_arm1, n_change_arm1)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.dep.sum$n1
with(dat.dep.sum, {cbind(n_arm2, n_change_arm2, n_change_arm2)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.dep.sum$n2
dat.dep.sum %>% 
  mutate(study_ig = paste(study, condition_arm1, sep = "_"),
         study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  { distinct(., study_ig, .keep_all = TRUE) %>% 
      summarise(sum(n1, na.rm=T)) %>% pull(1) %>% round() ->> n1;. } %>% 
  { distinct(., study_cg, .keep_all = TRUE) %>% 
      summarise(sum(n2, na.rm=T)) %>% pull(1) %>% round() ->> n2 } %>% 
  { n1 + n2 } -> n

# Number of studies
dat.dep %>% {unique(.$study) %>% length()} -> k

# Number of unique comparisons
m.dep$model.combined$k.all -> ncomp

# Number of effect sizes
m.dep$model.threelevel.che$k.all -> neffs

# Recruitment type
dat.dep.sum %>% distinct(study, .keep_all = T) %>% 
  {cbind(k.clin = sum(.$recruitment == "clin"),
         perc.clin = sum(.$recruitment == "clin")/nrow(.))} -> recr.clin

# Mean age
dat.dep.sum %>% pull(mean_age) %>% as.numeric() %>% 
  {cbind(age.mean = mean(., na.rm = T),
         age.sd = sd(., na.rm = T))} -> age.mean

# Percentage of women
dat.dep.sum %>% pull(percent_women) %>% as.numeric() %>% 
  ifelse(.>1, ./100, .) %>% 
  {cbind(perc.women = mean(., na.rm = T),
         perc.women.sd = sd(., na.rm = T))} -> women.prop

# Type of control groups
dat.dep.sum %>% 
  mutate(study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_cg, .keep_all = TRUE) %>% pull(condition_arm2) %>% 
  recode(`wlc` = "wl") %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> controls

# Countries of origin
dat.dep.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(country) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> country

# Low risk of bias
dat.dep.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(rob) %>% 
  {ifelse(. > 3, "low rob", "high rob")} %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> rob
  
# Intervention format
dat.dep.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(format) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> format

# Number of sessions
dat.dep.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(n_sessions_arm1) %>% 
  {cbind(sess = mean(., na.rm = T),
         sess.sd = sd(., na.rm = T))} -> sessions

# Combined characteristics & save
cbind(n1, n2, n, ncomp, neffs, k, recr.clin, age.mean, women.prop, controls,
      country, rob, format, sessions) -> characteristics

write.xlsx(characteristics, 
           file = "results/depression/results.xlsx", 
           sheetName = "characteristics", append = TRUE)

# Save full references of included studies
dat.dep.sum %>% 
  distinct(study, .keep_all = T) %>% select(full_ref) %>% 
  write.xlsx(file = "results/depression/results.xlsx", 
             sheetName = "references", append = TRUE)




### 1.2 Social Anxiety Disorder ------------------------------------------------

#### 1.2.1 Pooled Effect -------------------------------------------------------

# Load data
dat.sad = read.csv("data/social_anxiety.csv") %>% filter(format != "ush")

# Run meta-analysis and correct publication bias
m.sad = runMetaAnalysis(dat.sad, which.run = modelList,
                        which.outliers = "combined", 
                        which.influence = "combined",
                        which.rob = "combined", low.rob.filter = "rob > 3",
                        nnt.cer = .11) %>% 
  correctPublicationBias(which.run = "combined")

# Save the model & results
save(m.sad, file="results/social_anxiety/m.sad.rda")
write.xlsx(m.sad$summary, file = "results/social_anxiety/results.xlsx", 
           sheetName = "pooled_effects")
write.xlsx(m.sad$model.threelevel.che.var.comp, 
           file = "results/social_anxiety/results.xlsx", 
           sheetName = "variance_components", append = TRUE)
write.xlsx(m.sad$correctPublicationBias$summary, 
           file = "results/social_anxiety/results.xlsx", 
           sheetName = "publication_bias", append = TRUE)

# Generate profile plots of the variance components
pdf("results/social_anxiety/profile_che_model.pdf")
metafor::profile.rma.mv(m.sad$model.threelevel.che)
dev.off()


#### 1.2.2 Subgroup Analysis ---------------------------------------------------

rbind(
  dat.sad %>% 
    mutate(condition_arm1 = recode(condition_arm1, 
      `Exposure + Applied relaxation` = "Exposure",
      `Other psy` = "other psy", `CT` = "cbt")) %>% 
    filterPoolingData(condition_arm2 %in% c("wl", "wlc")) %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .11) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "wl"; .$summary},
  dat.sad %>% 
    mutate(condition_arm1 = recode(condition_arm1, 
      `Exposure + Applied relaxation` = "Exposure",
      `Other psy` = "other psy", `CT` = "cbt")) %>% 
    filterPoolingData(condition_arm2 %in% "cau") %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .11) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "cau"; .$summary}
) -> m.sad.sg

# Save the results
write.xlsx(m.sad.sg, file = "results/social_anxiety/results.xlsx", 
           sheetName = "subgroup_analysis", append = TRUE)


#### 1.2.3 Study Characteristics -----------------------------------------------

# Number of participants (intervention, control, overall)
dat.sad.sum = m.sad$model.combined$data
with(dat.sad.sum, {cbind(n_arm1, n_change_arm1, n_change_arm1)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.sad.sum$n1
with(dat.sad.sum, {cbind(n_arm2, n_change_arm2, n_change_arm2)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.sad.sum$n2
dat.sad.sum %>% 
  mutate(study_ig = paste(study, condition_arm1, sep = "_"),
         study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  { distinct(., study_ig, .keep_all = TRUE) %>% 
      summarise(sum(n1, na.rm=T)) %>% pull(1) %>% round() ->> n1;. } %>% 
  { distinct(., study_cg, .keep_all = TRUE) %>% 
      summarise(sum(n2, na.rm=T)) %>% pull(1) %>% round() ->> n2 } %>% 
  { n1 + n2 } -> n

# Number of studies
dat.sad %>% {unique(.$study) %>% length()} -> k

# Number of unique comparisons
m.sad$model.combined$k.all -> ncomp

# Number of effect sizes
m.sad$model.threelevel.che$k.all -> neffs

# Recruitment type
dat.sad.sum %>% distinct(study, .keep_all = T) %>% 
  {cbind(k.clin = sum(.$recruitment == "clin"),
         perc.clin = sum(.$recruitment == "clin")/nrow(.))} -> recr.clin

# Mean age
dat.sad.sum %>% pull(mean_age) %>% as.numeric() %>% 
  {cbind(age.mean = mean(., na.rm = T),
         age.sd = sd(., na.rm = T))} -> age.mean

# Percentage of women
dat.sad.sum %>% pull(percent_women) %>% as.numeric() %>% 
  ifelse(.>1, ./100, .) %>% 
  {cbind(perc.women = mean(., na.rm = T),
         perc.women.sd = sd(., na.rm = T))} -> women.prop

# Type of control groups
dat.sad.sum %>% 
  mutate(study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_cg, .keep_all = TRUE) %>% pull(condition_arm2) %>% 
  recode(`wlc` = "wl") %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> controls

# Countries of origin
dat.sad.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(country) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> country

# Low risk of bias
dat.sad.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(rob) %>% 
  {ifelse(. > 3, "low rob", "high rob")} %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> rob

# Intervention format
dat.sad.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(format) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> format

# Number of sessions
dat.sad.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(n_sessions_arm1) %>% 
  as.numeric() %>% 
  {cbind(sess = mean(., na.rm = T),
         sess.sd = sd(., na.rm = T))} -> sessions

# Combined characteristics & save
cbind(n1, n2, n, ncomp, neffs, k, recr.clin, age.mean, women.prop, controls,
      country, rob, format, sessions) -> characteristics

write.xlsx(characteristics, 
           file = "results/social_anxiety/results.xlsx", 
           sheetName = "characteristics", append = TRUE)



### 1.3 Panic Disorder ---------------------------------------------------------

#### 1.3.1 Pooled Effect -------------------------------------------------------

# Load data
dat.pan = read.csv("data/panic.csv") %>% filter(format != "ush")
dat.pan$rob = as.numeric(dat.pan$rob == "low")

# Run meta-analysis and correct publication bias
m.pan = runMetaAnalysis(dat.pan, which.run = modelList,
                        which.outliers = "combined", 
                        which.influence = "combined",
                        which.rob = "combined", low.rob.filter = "rob > 0",
                        nnt.cer = .14) %>% 
  correctPublicationBias(which.run = "combined")

# Save the model & results
save(m.pan, file="results/panic/m.pan.rda")
write.xlsx(m.pan$summary, file = "results/panic/results.xlsx", 
           sheetName = "pooled_effects")
write.xlsx(m.pan$model.threelevel.che.var.comp, 
           file = "results/panic/results.xlsx", 
           sheetName = "variance_components", append = TRUE)
write.xlsx(m.pan$correctPublicationBias$summary, 
           file = "results/panic/results.xlsx", 
           sheetName = "publication_bias", append = TRUE)

# Generate profile plots of the variance components
pdf("results/panic/profile_che_model.pdf")
metafor::profile.rma.mv(m.pan$model.threelevel.che)
dev.off()


#### 1.3.2 Subgroup Analysis ---------------------------------------------------

rbind(
  dat.pan %>% 
    filterPoolingData(condition_arm2 %in% "WL") %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .14) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "wl"; .$summary},
  dat.pan %>% 
    filterPoolingData(condition_arm2 %in% "TAU/NT") %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .14) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "cau"; .$summary}
) -> m.pan.sg

# Save the results
write.xlsx(m.pan.sg, file = "results/panic/results.xlsx", 
           sheetName = "subgroup_analysis", append = TRUE)


#### 1.3.3 Study Characteristics -----------------------------------------------

# Number of participants (intervention, control, overall)
dat.pan.sum = m.pan$model.combined$data
with(dat.pan.sum, {cbind(n_arm1, n_change_arm1, n_change_arm1)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.pan.sum$n1
with(dat.pan.sum, {cbind(n_arm2, n_change_arm2, n_change_arm2)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.pan.sum$n2
dat.pan.sum %>% 
  mutate(study_ig = paste(study, condition_arm1, sep = "_"),
         study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  { distinct(., study_ig, .keep_all = TRUE) %>% 
      summarise(sum(n1, na.rm=T)) %>% pull(1) %>% round() ->> n1;. } %>% 
  { distinct(., study_cg, .keep_all = TRUE) %>% 
      summarise(sum(n2, na.rm=T)) %>% pull(1) %>% round() ->> n2 } %>% 
  { n1 + n2 } -> n

# Number of studies
dat.pan %>% {unique(.$study) %>% length()} -> k

# Number of unique comparisons
m.pan$model.combined$k.all -> ncomp

# Number of effect sizes
m.pan$model.threelevel.che$k.all -> neffs

# Recruitment type
dat.pan.sum %>% distinct(study, .keep_all = T) %>% 
  {cbind(k.clin = sum(.$recruitment == "clinical", na.rm=T),
         perc.clin = sum(.$recruitment == "clinical", na.rm=T)/nrow(.))
    } -> recr.clin

# Mean age
dat.pan.sum %>% pull(mean_age) %>% as.numeric() %>% 
  {cbind(age.mean = mean(., na.rm = T),
         age.sd = sd(., na.rm = T))} -> age.mean

# Percentage of women
dat.pan.sum %>% pull(percent_women) %>% as.numeric() %>% 
  ifelse(.>1, ./100, .) %>% 
  {cbind(perc.women = mean(., na.rm = T),
         perc.women.sd = sd(., na.rm = T))} -> women.prop

# Type of control groups
dat.pan.sum %>% 
  mutate(study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_cg, .keep_all = TRUE) %>% pull(condition_arm2) %>% 
  recode(`wlc` = "wl") %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> controls

# Countries of origin
dat.pan.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(country) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> country

# Low risk of bias
dat.pan.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(rob) %>% 
  {ifelse(. > 0, "low rob", "high rob")} %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> rob

# Intervention format
dat.pan.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(format) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> format

# Number of sessions
dat.pan.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(n_sessions) %>% 
  as.numeric() %>% 
  {cbind(sess = mean(., na.rm = T),
         sess.sd = sd(., na.rm = T))} -> sessions

# Combined characteristics & save
cbind(n1, n2, n, ncomp, neffs, k, recr.clin, age.mean, women.prop, controls,
      country, rob, format, sessions) -> characteristics

write.xlsx(characteristics, 
           file = "results/panic/results.xlsx", 
           sheetName = "characteristics", append = TRUE)

# Save full references of included studies
dat.pan.sum %>% 
  distinct(study, .keep_all = T) %>% select(full_ref) %>% 
  write.xlsx(file = "results/panic/results.xlsx", 
             sheetName = "references", append = TRUE)



### 1.4 Generalized Anxiety Disorder -------------------------------------------

#### 1.4.1 Pooled Effect -------------------------------------------------------

# Load data
dat.gad = read.csv("data/generalized_anxiety.csv")

# Run meta-analysis and correct publication bias
m.gad = runMetaAnalysis(dat.gad, which.run = modelList,
                        which.outliers = "combined", 
                        which.influence = "combined",
                        which.rob = "combined", low.rob.filter = "rob > 0",
                        nnt.cer = .14) %>% 
  correctPublicationBias(which.run = "combined")

# Save the model & results
save(m.gad, file="results/generalized_anxiety/m.gad.rda")
write.xlsx(m.gad$summary, file = "results/generalized_anxiety/results.xlsx", 
           sheetName = "pooled_effects")
write.xlsx(m.gad$model.threelevel.che.var.comp, 
           file = "results/generalized_anxiety/results.xlsx", 
           sheetName = "variance_components", append = TRUE)
write.xlsx(m.gad$correctPublicationBias$summary, 
           file = "results/generalized_anxiety/results.xlsx", 
           sheetName = "publication_bias", append = TRUE)

# Generate profile plots of the variance components
pdf("results/generalized_anxiety/profile_che_model.pdf")
metafor::profile.rma.mv(m.gad$model.threelevel.che)
dev.off()


#### 1.4.2 Subgroup Analysis ---------------------------------------------------

rbind(
  dat.gad %>% 
    filterPoolingData(condition_arm2 %in% "wl") %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .14) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "wl"; .$summary},
  dat.gad %>% 
    filterPoolingData(condition_arm2 %in% "cau") %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .14) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "cau"; .$summary}
) -> m.gad.sg

# Save the results
write.xlsx(m.gad.sg, file = "results/generalized_anxiety/results.xlsx", 
           sheetName = "subgroup_analysis", append = TRUE)


#### 1.4.3 Study Characteristics -----------------------------------------------

# Number of participants (intervention, control, overall)
dat.gad.sum = m.gad$model.combined$data

with(dat.gad.sum, {cbind(n_arm1, totaln_arm1)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.gad.sum$n1

with(dat.gad.sum, {cbind(n_arm2, totaln_arm2)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.gad.sum$n2

dat.gad.sum %>% 
  mutate(study_ig = paste(study, condition_arm1, sep = "_"),
         study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  { distinct(., study_ig, .keep_all = TRUE) %>% 
      summarise(sum(n1, na.rm=T)) %>% pull(1) %>% round() ->> n1;. } %>% 
  { distinct(., study_cg, .keep_all = TRUE) %>% 
      summarise(sum(n2, na.rm=T)) %>% pull(1) %>% round() ->> n2 } %>% 
  { n1 + n2 } -> n

# Number of studies
dat.gad %>% {unique(.$study) %>% length()} -> k

# Number of unique comparisons
m.gad$model.combined$k.all -> ncomp

# Number of effect sizes
m.gad$model.threelevel.che$k.all -> neffs

# Recruitment type
dat.gad.sum %>% distinct(study, .keep_all = T) %>% 
  {cbind(k.clin = sum(.$recruitment == "clin", na.rm=T),
         perc.clin = sum(.$recruitment == "clin", na.rm=T)/nrow(.))
  } -> recr.clin

# Mean age
dat.gad.sum %>% pull(mean_age) %>% as.numeric() %>% 
  {cbind(age.mean = mean(., na.rm = T),
         age.sd = sd(., na.rm = T))} -> age.mean

# Percentage of women
dat.gad.sum %>% pull(percent_women) %>% as.numeric() %>% 
  ifelse(.>1, ./100, .) %>% 
  {cbind(perc.women = mean(., na.rm = T),
         perc.women.sd = sd(., na.rm = T))} -> women.prop

# Type of control groups
dat.gad.sum %>% 
  mutate(study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_cg, .keep_all = TRUE) %>% pull(condition_arm2) %>% 
  recode(`wlc` = "wl") %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> controls

# Countries of origin
dat.gad.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(country) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> country

# Low risk of bias
dat.gad.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(rob) %>% 
  {ifelse(. > 0, "low rob", "high rob")} %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> rob

# Intervention format
dat.gad.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(format) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> format

# Number of sessions
dat.gad.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(n_sessions_ig) %>% 
  as.numeric() %>% 
  {cbind(sess = mean(., na.rm = T),
         sess.sd = sd(., na.rm = T))} -> sessions

# Combined characteristics & save
cbind(n1, n2, n, ncomp, neffs, k, recr.clin, age.mean, women.prop, controls,
      country, rob, format, sessions) -> characteristics

write.xlsx(characteristics, 
           file = "results/generalized_anxiety/results.xlsx", 
           sheetName = "characteristics", append = TRUE)



### 1.5 Obsessive-Compulsive Disorder ------------------------------------------

#### 1.5.1 Pooled Effect -------------------------------------------------------

# Load data
dat.ocd = read.csv("data/ocd.csv") %>% 
  filter(target_group %in% c("adul", "oth", "ppd"))

# Run meta-analysis and correct publication bias
m.ocd = runMetaAnalysis(dat.ocd, which.run = modelList,
                        which.outliers = "combined", 
                        which.influence = "combined",
                        which.rob = "combined", low.rob.filter = "rob < 2",
                        nnt.cer = .05) %>% 
  correctPublicationBias(which.run = "combined")

# Save the model & results
save(m.ocd, file="results/ocd/m.ocd.rda")
write.xlsx(m.ocd$summary, file = "results/ocd/results.xlsx", 
           sheetName = "pooled_effects")
write.xlsx(m.ocd$model.threelevel.che.var.comp, 
           file = "results/ocd/results.xlsx", 
           sheetName = "variance_components", append = TRUE)
write.xlsx(m.ocd$correctPublicationBias$summary, 
           file = "results/ocd/results.xlsx", 
           sheetName = "publication_bias", append = TRUE)

# Generate profile plots of the variance components
pdf("results/ocd/profile_che_model.pdf")
metafor::profile.rma.mv(m.ocd$model.threelevel.che)
dev.off()


#### 1.5.2 Subgroup Analysis ---------------------------------------------------

rbind(
  dat.ocd %>% 
    filterPoolingData(condition_arm2 %in% "wl") %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .05) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "wl"; .$summary},
  dat.ocd %>% 
    filterPoolingData(
      condition_arm2 %in% c("cau", "no treatment", "psychological placebo")) %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .05) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "cau"; .$summary}
) -> m.ocd.sg

# Save the results
write.xlsx(m.ocd.sg, file = "results/ocd/results.xlsx", 
           sheetName = "subgroup_analysis", append = TRUE)


#### 1.5.3 Study Characteristics -----------------------------------------------

# Number of participants (intervention, control, overall)
dat.ocd.sum = m.ocd$model.combined$data

with(dat.ocd.sum, {cbind(n_arm1, totaln_arm1)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.ocd.sum$n1

with(dat.ocd.sum, {cbind(n_arm2, totaln_arm2)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.ocd.sum$n2

dat.ocd.sum %>% 
  mutate(study_ig = paste(study, condition_arm1, sep = "_"),
         study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  { distinct(., study_ig, .keep_all = TRUE) %>% 
      summarise(sum(n1, na.rm=T)) %>% pull(1) %>% round() ->> n1;. } %>% 
  { distinct(., study_cg, .keep_all = TRUE) %>% 
      summarise(sum(n2, na.rm=T)) %>% pull(1) %>% round() ->> n2 } %>% 
  { n1 + n2 } -> n

# Number of studies
dat.ocd %>% {unique(.$study) %>% length()} -> k

# Number of unique comparisons
m.ocd$model.combined$k.all -> ncomp

# Number of effect sizes
m.ocd$model.threelevel.che$k.all -> neffs

# Recruitment type
dat.ocd.sum %>% distinct(study, .keep_all = T) %>% 
  {cbind(k.clin = sum(.$recruitment == "clin", na.rm=T),
         perc.clin = sum(.$recruitment == "clin", na.rm=T)/nrow(.))
  } -> recr.clin

# Mean age
dat.ocd.sum %>% pull(mean_age) %>% as.numeric() %>% 
  {cbind(age.mean = mean(., na.rm = T),
         age.sd = sd(., na.rm = T))} -> age.mean

# Percentage of women
dat.ocd.sum %>% pull(percent_women) %>% as.numeric() %>% 
  ifelse(.>1, ./100, .) %>% 
  {cbind(perc.women = mean(., na.rm = T),
         perc.women.sd = sd(., na.rm = T))} -> women.prop

# Type of control groups
dat.ocd.sum %>% 
  mutate(study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_cg, .keep_all = TRUE) %>% pull(condition_arm2) %>% 
  recode(`wlc` = "wl", `no treatment` = "cau", 
         `psychological placebo` = "cau") %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> controls

# Countries of origin
dat.ocd.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(country) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> country

# Low risk of bias
dat.ocd.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(rob) %>% 
  {ifelse(. < 2, "low rob", "high rob")} %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> rob

# Intervention format
dat.ocd.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(format) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> format

# Number of sessions
dat.ocd.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(n_sessions_arm1) %>% 
  as.numeric() %>% 
  {cbind(sess = mean(., na.rm = T),
         sess.sd = sd(., na.rm = T))} -> sessions

# Combined characteristics & save
cbind(n1, n2, n, ncomp, neffs, k, recr.clin, age.mean, women.prop, controls,
      country, rob, format, sessions) -> characteristics

write.xlsx(characteristics, 
           file = "results/ocd/results.xlsx", 
           sheetName = "characteristics", append = TRUE)


### 1.6 Specific Phobia --------------------------------------------------------

#### 1.6.1 Pooled Effect -------------------------------------------------------

# Load data
dat.pho = read.csv("data/phobia.csv") %>% filter(format != "ush")

# Run meta-analysis and correct publication bias
m.pho = runMetaAnalysis(dat.pho, which.run = modelList,
                        which.outliers = "combined", 
                        which.influence = "combined",
                        which.rob = "combined", low.rob.filter = "rob > 2",
                        nnt.cer = .09) %>% 
  correctPublicationBias(which.run = "combined")

# Save the model & results
save(m.pho, file="results/phobia/m.pho.rda")
write.xlsx(m.pho$summary, file = "results/phobia/results.xlsx", 
           sheetName = "pooled_effects")
write.xlsx(m.pho$model.threelevel.che.var.comp, 
           file = "results/phobia/results.xlsx", 
           sheetName = "variance_components", append = TRUE)
write.xlsx(m.pho$correctPublicationBias$summary, 
           file = "results/phobia/results.xlsx", 
           sheetName = "publication_bias", append = TRUE)

# Generate profile plots of the variance components
pdf("results/phobia/profile_che_model.pdf")
metafor::profile.rma.mv(m.pho$model.threelevel.che)
dev.off()


#### 1.6.2 Subgroup Analysis ---------------------------------------------------

rbind(
  dat.pho %>% 
    filterPoolingData(condition_arm2 %in% "wlc") %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .09) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "wl"; .$summary},
  dat.pho %>% 
    filterPoolingData(
      condition_arm2 %in% c("cau", "other ctr")) %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .09) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "cau"; .$summary}
) -> m.pho.sg

# Save the results
write.xlsx(m.pho.sg, file = "results/phobia/results.xlsx", 
           sheetName = "subgroup_analysis", append = TRUE)


#### 1.6.3 Study Characteristics -----------------------------------------------

# Number of participants (intervention, control, overall)
dat.pho.sum = m.pho$model.combined$data

with(dat.pho.sum, {cbind(n_arm1, n_change_arm1, totaln_arm1)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.pho.sum$n1

with(dat.pho.sum, {cbind(n_arm2, n_change_arm2, totaln_arm2)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.pho.sum$n2

dat.pho.sum %>% 
  mutate(study_ig = paste(study, condition_arm1, sep = "_"),
         study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  { distinct(., study_ig, .keep_all = TRUE) %>% 
      summarise(sum(n1, na.rm=T)) %>% pull(1) %>% round() ->> n1;. } %>% 
  { distinct(., study_cg, .keep_all = TRUE) %>% 
      summarise(sum(n2, na.rm=T)) %>% pull(1) %>% round() ->> n2 } %>% 
  { n1 + n2 } -> n

# Number of studies
dat.pho %>% {unique(.$study) %>% length()} -> k

# Number of unique comparisons
m.pho$model.combined$k.all -> ncomp

# Number of effect sizes
m.pho$model.threelevel.che$k.all -> neffs

# Recruitment type
dat.pho.sum %>% distinct(study, .keep_all = T) %>% 
  {cbind(k.clin = sum(.$recruitment == "clin", na.rm=T),
         perc.clin = sum(.$recruitment == "clin", na.rm=T)/nrow(.))
  } -> recr.clin

# Mean age
dat.pho.sum %>% pull(mean_age) %>% as.numeric() %>% 
  {cbind(age.mean = mean(., na.rm = T),
         age.sd = sd(., na.rm = T))} -> age.mean

# Percentage of women
dat.pho.sum %>% pull(percent_women) %>% as.numeric() %>% 
  ifelse(.>1, ./100, .) %>% 
  {cbind(perc.women = mean(., na.rm = T),
         perc.women.sd = sd(., na.rm = T))} -> women.prop

# Type of control groups
dat.pho.sum %>% 
  mutate(study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_cg, .keep_all = TRUE) %>% pull(condition_arm2) %>% 
  recode(`wlc` = "wl", `no treatment` = "cau", 
         `psychological placebo` = "cau") %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> controls

# Countries of origin
dat.pho.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(country) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> country

# Low risk of bias
dat.pho.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(rob) %>% 
  {ifelse(. > 2, "low rob", "high rob")} %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> rob

# Intervention format
dat.pho.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(format) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> format

# Number of sessions
dat.pho.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(n_sessions_ig) %>% 
  as.numeric() %>% 
  {cbind(sess = mean(., na.rm = T),
         sess.sd = sd(., na.rm = T))} -> sessions

# Combined characteristics & save
cbind(n1, n2, n, ncomp, neffs, k, recr.clin, age.mean, women.prop, controls,
      country, rob, format, sessions) -> characteristics

write.xlsx(characteristics, 
           file = "results/phobia/results.xlsx", 
           sheetName = "characteristics", append = TRUE)


### 1.7 Borderline Personality Disorder ----------------------------------------

#### 1.7.1 Pooled Effect -------------------------------------------------------

# Load data
dat.bpd = read.csv("data/borderline.csv") 

# Run meta-analysis and correct publication bias
m.bpd = runMetaAnalysis(dat.bpd, which.run = modelList,
                        which.outliers = "combined", 
                        which.influence = "combined",
                        which.rob = "combined", low.rob.filter = "rob > 0",
                        nnt.cer = .15) %>% 
  correctPublicationBias(which.run = "combined")

# Save the model & results
save(m.bpd, file="results/borderline/m.bpd.rda")
write.xlsx(m.bpd$summary, file = "results/borderline/results.xlsx", 
           sheetName = "pooled_effects")
write.xlsx(m.bpd$model.threelevel.che.var.comp, 
           file = "results/borderline/results.xlsx", 
           sheetName = "variance_components", append = TRUE)
write.xlsx(m.bpd$correctPublicationBias$summary, 
           file = "results/borderline/results.xlsx", 
           sheetName = "publication_bias", append = TRUE)

# Generate profile plots of the variance components
pdf("results/borderline/profile_che_model.pdf")
metafor::profile.rma.mv(m.bpd$model.threelevel.che)
dev.off()


#### 1.7.2 Subgroup Analysis ---------------------------------------------------

rbind(
  dat.bpd %>% 
    filterPoolingData(
      condition_arm2 %in% c("cau")) %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .15) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "cau"; .$summary}
) -> m.bpd.sg

# Save the results
write.xlsx(m.bpd.sg, file = "results/borderline/results.xlsx", 
           sheetName = "subgroup_analysis", append = TRUE)

# Save full references of included studies
dat.bpd.sum %>% 
  distinct(study, .keep_all = T) %>% select(full_ref) %>% 
  write.xlsx(file = "results/borderline/results.xlsx", 
             sheetName = "references", append = TRUE)



#### 1.7.3 Study Characteristics -----------------------------------------------

# Number of participants (intervention, control, overall)
dat.bpd.sum = m.bpd$model.combined$data

with(dat.bpd.sum, {cbind(n_arm1, n_change_arm1, totaln_arm1)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.bpd.sum$n1

with(dat.bpd.sum, {cbind(n_arm2, n_change_arm2, totaln_arm2)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.bpd.sum$n2

dat.bpd.sum %>% 
  mutate(study_ig = paste(study, condition_arm1, sep = "_"),
         study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  { distinct(., study_ig, .keep_all = TRUE) %>% 
      summarise(sum(n1, na.rm=T)) %>% pull(1) %>% round() ->> n1;. } %>% 
  { distinct(., study_cg, .keep_all = TRUE) %>% 
      summarise(sum(n2, na.rm=T)) %>% pull(1) %>% round() ->> n2 } %>% 
  { n1 + n2 } -> n

# Number of studies
dat.bpd %>% {unique(.$study) %>% length()} -> k

# Number of unique comparisons
m.bpd$model.combined$k.all -> ncomp

# Number of effect sizes
m.bpd$model.threelevel.che$k.all -> neffs

# Recruitment type
dat.bpd.sum %>% distinct(study, .keep_all = T) %>% 
  {cbind(k.clin = sum(.$recruitment == "clin", na.rm=T),
         perc.clin = sum(.$recruitment == "clin", na.rm=T)/nrow(.))
  } -> recr.clin

# Mean age
dat.bpd.sum %>% pull(mean_age) %>% as.numeric() %>% 
  {cbind(age.mean = mean(., na.rm = T),
         age.sd = sd(., na.rm = T))} -> age.mean

# Percentage of women
dat.bpd.sum %>% pull(percent_women) %>% as.numeric() %>% 
  ifelse(.>1, ./100, .) %>% 
  {cbind(perc.women = mean(., na.rm = T),
         perc.women.sd = sd(., na.rm = T))} -> women.prop

# Type of control groups
dat.bpd.sum %>% 
  mutate(study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_cg, .keep_all = TRUE) %>% pull(condition_arm2) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> controls

# Countries of origin
dat.bpd.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(country) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> country

# Low risk of bias
dat.bpd.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(rob) %>% 
  {ifelse(. > 0, "low rob", "high rob")} %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> rob

# Intervention format
dat.bpd.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(format) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> format

# Number of sessions
dat.bpd.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(n_sessions_ig) %>% 
  as.numeric() %>% 
  {cbind(sess = mean(., na.rm = T),
         sess.sd = sd(., na.rm = T))} -> sessions

# Combined characteristics & save
cbind(n1, n2, n, ncomp, neffs, k, recr.clin, age.mean, women.prop, controls,
      country, rob, format, sessions) -> characteristics

write.xlsx(characteristics, 
           file = "results/borderline/results.xlsx", 
           sheetName = "characteristics", append = TRUE)



### 1.8 Post-Traumatic Stress Disorder -----------------------------------------

#### 1.8.1 Pooled Effect -------------------------------------------------------

# Load data
dat.ptsd = read.csv("data/ptsd.csv") 
dat.ptsd$rob = ifelse(dat.ptsd$rob %in% c("Low", "Some Concerns"), 1, 0)

# Run meta-analysis and correct publication bias
m.ptsd = runMetaAnalysis(dat.ptsd, which.run = modelList,
                        which.outliers = "combined", 
                        which.influence = "combined",
                        which.rob = "combined", low.rob.filter = "rob > 0",
                        nnt.cer = .10) %>% 
  correctPublicationBias(which.run = "combined")

# Save the model & results
save(m.ptsd, file="results/ptsd/m.ptsd.rda")
write.xlsx(m.ptsd$summary, file = "results/ptsd/results.xlsx", 
           sheetName = "pooled_effects")
write.xlsx(m.ptsd$model.threelevel.che.var.comp, 
           file = "results/ptsd/results.xlsx", 
           sheetName = "variance_components", append = TRUE)
write.xlsx(m.ptsd$correctPublicationBias$summary, 
           file = "results/ptsd/results.xlsx", 
           sheetName = "publication_bias", append = TRUE)

# Generate profile plots of the variance components
pdf("results/ptsd/profile_che_model.pdf")
metafor::profile.rma.mv(m.ptsd$model.threelevel.che)
dev.off()


#### 1.8.2 Subgroup Analysis ---------------------------------------------------

rbind(
  dat.ptsd %>% 
    filterPoolingData(
      condition_arm2 %in% c("TAU")) %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .10) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "cau"; .$summary},
  dat.ptsd %>% 
    filterPoolingData(
      condition_arm2 %in% c("Waitlist")) %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .10) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "wl"; .$summary}
) -> m.ptsd.sg

# Save the results
write.xlsx(m.ptsd.sg, file = "results/ptsd/results.xlsx", 
           sheetName = "subgroup_analysis", append = TRUE)


#### 1.8.3 Study Characteristics -----------------------------------------------

# Number of participants (intervention, control, overall)
dat.ptsd.sum = m.ptsd$model.combined$data

with(dat.ptsd.sum, {cbind(n_arm1, totaln_arm1, n_randomized_arm1)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.ptsd.sum$n1

with(dat.ptsd.sum, {cbind(n_arm2, totaln_arm2, n_randomized_arm1)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.ptsd.sum$n2

dat.ptsd.sum %>% 
  mutate(study_ig = paste(study, condition_arm1, sep = "_"),
         study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  { distinct(., study_ig, .keep_all = TRUE) %>% 
      summarise(sum(n1, na.rm=T)) %>% pull(1) %>% round() ->> n1;. } %>% 
  { distinct(., study_cg, .keep_all = TRUE) %>% 
      summarise(sum(n2, na.rm=T)) %>% pull(1) %>% round() ->> n2 } %>% 
  { n1 + n2 } -> n

# Number of studies
dat.ptsd %>% {unique(.$study) %>% length()} -> k

# Number of unique comparisons
m.ptsd$model.combined$k.all -> ncomp

# Number of effect sizes
m.ptsd$model.threelevel.che$k.all -> neffs

# Recruitment type
dat.ptsd.sum %>% distinct(study, .keep_all = T) %>% 
  {cbind(k.clin = sum(.$percent_community < 1, na.rm=T),
         perc.clin = sum(.$percent_community < 1, na.rm=T)/nrow(.))
  } -> recr.clin

# Mean age
dat.ptsd.sum %>% 
  pull(mean_age) %>% as.numeric() %>% 
  {cbind(age.mean = mean(., na.rm = T),
         age.sd = sd(., na.rm = T))} -> age.mean

# Percentage of women
dat.ptsd.sum %>% pull(female_percent) %>% as.numeric() %>% 
  ifelse(.>1, ./100, .) %>% 
  {cbind(perc.women = mean(., na.rm = T),
         perc.women.sd = sd(., na.rm = T))} -> women.prop

# Type of control groups
dat.ptsd.sum %>% 
  mutate(study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_cg, .keep_all = TRUE) %>% pull(condition_arm2) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> controls

# Countries of origin
dat.ptsd.sum %>% 
  mutate(country = recode(country, 
    "Turkey" = "Middle East", "Iran" = "Middle East", 
    "Oman" = "Middle East", "Saudi Arabia" = "Middle East",
    "Jordan" = "Middle East", "The Netherlands" = "Europe",
    "Japan" = "Asia",  "North America" = "USA",  "U.K." = "Europe",
    "Germany" = "Europe", "Canada" = "North America", 
    "Denmark" = "Europe", "Indonesia" = "Asia",
    "Egypt" = "Middle East", "Sweden" = "Europe",
    "Rwanda" = "Africa", "Mexico" = "Latin America",  "Germany, Iraq" = "Mixed",
    "Pakistan" = "Middle East", "U.S., Canada" = "North America",
    "U.S." = "North America",
    "Israel" = "Middle East", "Cambodia" = "Asia", "China" = "Asia",
    "Iraq" = "Middle East")) %>% 
  distinct(study, .keep_all = TRUE) %>% pull(country) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> country

# Low risk of bias
dat.ptsd.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(rob) %>% 
  {ifelse(. > 0, "low rob", "high rob")} %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> rob

# Intervention format
dat.ptsd.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(format) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> format

# Number of sessions
dat.ptsd.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(psychotherapy_sessions_mean) %>% 
  as.numeric() %>% 
  {cbind(sess = mean(., na.rm = T),
         sess.sd = sd(., na.rm = T))} -> sessions

# Combined characteristics & save
cbind(n1, n2, n, ncomp, neffs, k, recr.clin, age.mean, women.prop, controls,
      country, rob, format, sessions) -> characteristics

write.xlsx(characteristics, 
           file = "results/ptsd/results.xlsx", 
           sheetName = "characteristics", append = TRUE)

# Save full references of included studies
dat.ptsd.sum %>% 
  distinct(study, .keep_all = T) %>% select(full_ref) %>% 
  write.xlsx(file = "results/ptsd/results.xlsx", 
             sheetName = "references", append = TRUE)



### 1.9 Complicated Grief ------------------------------------------------------

#### 1.9.1 Pooled Effect -------------------------------------------------------

# Load data
dat.grief = read.csv("data/grief.csv") 

# Run meta-analysis and correct publication bias
m.grief = runMetaAnalysis(dat.grief, which.run = modelList,
                         which.outliers = "combined", 
                         which.influence = "combined",
                         which.rob = "combined", low.rob.filter = "rob > 0",
                         nnt.cer = .5) %>% 
  correctPublicationBias(which.run = "combined")

# Save the model & results
save(m.grief, file="results/grief/m.grief.rda")
write.xlsx(m.grief$summary, file = "results/grief/results.xlsx", 
           sheetName = "pooled_effects")
write.xlsx(m.grief$model.threelevel.che.var.comp, 
           file = "results/grief/results.xlsx", 
           sheetName = "variance_components", append = TRUE)
write.xlsx(m.grief$correctPublicationBias$summary, 
           file = "results/grief/results.xlsx", 
           sheetName = "publication_bias", append = TRUE)

# Generate profile plots of the variance components
pdf("results/grief/profile_che_model.pdf")
metafor::profile.rma.mv(m.grief$model.threelevel.che)
dev.off()


#### 1.9.2 Subgroup Analysis ---------------------------------------------------

rbind(
  dat.grief %>% 
    filterPoolingData(
      condition_arm2 %in% c("tau")) %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .5) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "cau"; .$summary},
  dat.grief %>% 
    filterPoolingData(
      condition_arm2 %in% c("wl")) %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .5) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "wl"; .$summary}
) -> m.grief.sg

# Save the results
write.xlsx(m.grief.sg, file = "results/grief/results.xlsx", 
           sheetName = "subgroup_analysis", append = TRUE)


#### 1.9.3 Study Characteristics -----------------------------------------------

# Number of participants (intervention, control, overall)
dat.grief.sum = m.grief$model.combined$data

with(dat.grief.sum, {cbind(n_arm1, totaln_arm1, rand_arm1)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.grief.sum$n1

with(dat.grief.sum, {cbind(n_arm2, totaln_arm2, rand_arm1)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.grief.sum$n2

dat.grief.sum %>% 
  mutate(study_ig = paste(study, condition_arm1, sep = "_"),
         study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  { distinct(., study_ig, .keep_all = TRUE) %>% 
      summarise(sum(n1, na.rm=T)) %>% pull(1) %>% round() ->> n1;. } %>% 
  { distinct(., study_cg, .keep_all = TRUE) %>% 
      summarise(sum(n2, na.rm=T)) %>% pull(1) %>% round() ->> n2 } %>% 
  { n1 + n2 } -> n

# Number of studies
dat.grief %>% {unique(.$study) %>% length()} -> k

# Number of unique comparisons
m.grief$model.combined$k.all -> ncomp

# Number of effect sizes
m.grief$model.threelevel.che$k.all -> neffs

# Recruitment type
dat.grief.sum %>% distinct(study, .keep_all = T) %>% 
  {cbind(k.clin = sum(.$recr== 1, na.rm=T),
         perc.clin = sum(.$recr== 1, na.rm=T)/nrow(.))
  } -> recr.clin

# Mean age
dat.grief.sum %>% 
  mutate(mean_age = parse_number(meanage, locale=locale(decimal=","))) %>% 
  pull(mean_age) %>% as.numeric() %>% 
  {cbind(age.mean = mean(., na.rm = T),
         age.sd = sd(., na.rm = T))} -> age.mean

# Percentage of women
dat.grief.sum %>% pull(propwomen) %>% parse_number(loc=locale(decimal=",")) %>% 
  ifelse(.>1, ./100, .) %>% 
  {cbind(perc.women = mean(., na.rm = T),
         perc.women.sd = sd(., na.rm = T))} -> women.prop

# Type of control groups
dat.grief.sum %>% 
  mutate(study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_cg, .keep_all = TRUE) %>% pull(condition_arm2) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> controls

# Countries of origin
dat.grief.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(country) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> country

# Low risk of bias
dat.grief.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(rob) %>% 
  {ifelse(. > 0, "low rob", "high rob")} %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> rob

# Intervention format
dat.grief.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(format) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> format

# Number of sessions
dat.grief.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(nsess_cont) %>% 
  as.numeric() %>% 
  {cbind(sess = mean(., na.rm = T),
         sess.sd = sd(., na.rm = T))} -> sessions

# Combined characteristics & save
cbind(n1, n2, n, ncomp, neffs, k, recr.clin, age.mean, women.prop, controls,
      country, rob, format, sessions) -> characteristics

write.xlsx(characteristics, 
           file = "results/grief/results.xlsx", 
           sheetName = "characteristics", append = TRUE)


### 1.10 Problem Gambling ------------------------------------------------------

#### 1.10.1 Pooled Effect ------------------------------------------------------

# Load data
dat.gam = read.csv("data/gambling.csv") %>% 
  filter(outcome_domain == "gambling", time == "post")

# Run meta-analysis and correct publication bias
m.gam = runMetaAnalysis(dat.gam, which.run = modelList,
                          which.outliers = "combined", 
                          which.influence = "combined",
                          which.rob = "combined", low.rob.filter = "rob > 3",
                          nnt.cer = .10) %>% 
  correctPublicationBias(which.run = "combined")

# Save the model & results
save(m.gam, file="results/gambling/m.gam.rda")
write.xlsx(m.gam$summary, file = "results/gambling/results.xlsx", 
           sheetName = "pooled_effects")
write.xlsx(m.gam$model.threelevel.che.var.comp, 
           file = "results/gambling/results.xlsx", 
           sheetName = "variance_components", append = TRUE)
write.xlsx(m.gam$correctPublicationBias$summary, 
           file = "results/gambling/results.xlsx", 
           sheetName = "publication_bias", append = TRUE)

# Generate profile plots of the variance components
pdf("results/gambling/profile_che_model.pdf")
metafor::profile.rma.mv(m.gam$model.threelevel.che)
dev.off()


#### 1.10.2 Subgroup Analysis --------------------------------------------------

rbind(
  dat.gam %>% 
    filterPoolingData(
      condition_arm2 %in% c("cau")) %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .10) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "cau"; .$summary},
  dat.gam %>% 
    filterPoolingData(
      condition_arm2 %in% c("wl")) %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .10) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "wl"; .$summary}
) -> m.gam.sg

# Save the results
write.xlsx(m.gam.sg, file = "results/gambling/results.xlsx", 
           sheetName = "subgroup_analysis", append = TRUE)


#### 1.10.3 Study Characteristics ------------------------------------------------

# Number of participants (intervention, control, overall)
dat.gam.sum = m.gam$model.combined$data

dat.gam.sum %>% 
  mutate(study_ig = paste(study, condition_arm1, sep = "_"),
         study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  { distinct(., study_ig, .keep_all = TRUE) %>% 
      summarise(sum(n_arm1, na.rm=T)) %>% pull(1) %>% round() ->> n1;. } %>% 
  { distinct(., study_cg, .keep_all = TRUE) %>% 
      summarise(sum(n_arm2, na.rm=T)) %>% pull(1) %>% round() ->> n2 } %>% 
  { n1 + n2 } -> n

# Number of studies
dat.gam %>% {unique(.$study) %>% length()} -> k

# Number of unique comparisons
m.gam$model.combined$k.all -> ncomp

# Number of effect sizes
m.gam$model.threelevel.che$k.all -> neffs

# Recruitment type
NA -> recr.clin

# Mean age
dat.gam.sum %>% 
  pull(mean_age) %>% as.numeric() %>% 
  {cbind(age.mean = mean(., na.rm = T),
         age.sd = sd(., na.rm = T))} -> age.mean

# Percentage of women
dat.gam.sum %>% pull(percent_women) %>% 
  {cbind(perc.women = mean(., na.rm = T),
         perc.women.sd = sd(., na.rm = T))} -> women.prop

# Type of control groups
dat.gam.sum %>% 
  mutate(study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_cg, .keep_all = TRUE) %>% pull(condition_arm2) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> controls

# Countries of origin
dat.gam.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(country) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> country

# Low risk of bias
dat.gam.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(rob) %>% 
  {ifelse(. > 3, "low rob", "high rob")} %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> rob

# Intervention format
dat.gam.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(format) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> format

# Number of sessions
NA -> sessions

# Combined characteristics & save
cbind(n1, n2, n, ncomp, neffs, k, recr.clin, age.mean, women.prop, controls,
      country, rob, format, sessions) -> characteristics

write.xlsx(characteristics, 
           file = "results/gambling/results.xlsx", 
           sheetName = "characteristics", append = TRUE)

# Save full references of included studies
dat.gam.sum %>% 
  distinct(study, .keep_all = T) %>% select(full_ref) %>% 
  write.xlsx(file = "results/gambling/results.xlsx", 
             sheetName = "references", append = TRUE)


### 1.11 Psychosis -------------------------------------------------------------

#### 1.11.1 Pooled Effect ------------------------------------------------------

# Load data
dat.psy = read.csv("data/psychosis.csv") 

# Run meta-analysis and correct publication bias
m.psy = runMetaAnalysis(dat.psy, which.run = modelList,
                        which.outliers = "combined", 
                        which.influence = "combined",
                        which.rob = "combined", low.rob.filter = "rob > 0",
                        nnt.cer = .10) %>% 
  correctPublicationBias(which.run = "combined")

# Save the model & results
save(m.psy, file="results/psychosis/m.psy.rda")
write.xlsx(m.psy$summary, file = "results/psychosis/results.xlsx", 
           sheetName = "pooled_effects")
write.xlsx(m.psy$model.threelevel.che.var.comp, 
           file = "results/psychosis/results.xlsx", 
           sheetName = "variance_components", append = TRUE)
write.xlsx(m.psy$correctPublicationBias$summary, 
           file = "results/psychosis/results.xlsx", 
           sheetName = "publication_bias", append = TRUE)

# Generate profile plots of the variance components
pdf("results/psychosis/profile_che_model.pdf")
metafor::profile.rma.mv(m.psy$model.threelevel.che)
dev.off()


#### 1.11.2 Subgroup Analysis --------------------------------------------------

rbind(
  dat.psy %>% 
    mutate(condition_arm1 = recode(condition_arm1, `CBTp` = "CBT",
                                   `CRSS` = "CR", `CRmeta` = "CR",
                                   `CR_CRSS` = "CR", `CR_SST` = "CR",
                                   `CBT_CAT` = "CBT")) %>% 
    filterPoolingData(
      condition_arm2 %in% c("cau")) %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .10) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "cau"; .$summary}
) -> m.psy.sg

# Save the results
write.xlsx(m.psy.sg, file = "results/psychosis/results.xlsx", 
           sheetName = "subgroup_analysis", append = TRUE)


#### 1.11.3 Study Characteristics ------------------------------------------------

# Number of participants (intervention, control, overall)
dat.psy.sum = m.psy$model.combined$data

with(dat.psy.sum, {cbind(n_arm1, totaln_arm1, totaln_arm1)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.psy.sum$n1

with(dat.psy.sum, {cbind(n_arm2, totaln_arm2, totaln_arm2)}) %>%  
  apply(1, max, na.rm=TRUE) %>% ifelse(. < 0, NA, .) -> dat.psy.sum$n2

dat.psy.sum %>% 
  mutate(study_ig = paste(study, condition_arm1, sep = "_"),
         study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  { distinct(., study_ig, .keep_all = TRUE) %>% 
      summarise(sum(n1, na.rm=T)) %>% pull(1) %>% round() ->> n1;. } %>% 
  { distinct(., study_cg, .keep_all = TRUE) %>% 
      summarise(sum(n2, na.rm=T)) %>% pull(1) %>% round() ->> n2 } %>% 
  { n1 + n2 } -> n

# Number of studies
dat.psy %>% {unique(.$study) %>% length()} -> k

# Number of unique comparisons
m.psy$model.combined$k.all -> ncomp

# Number of effect sizes
m.psy$model.threelevel.che$k.all -> neffs

# Recruitment type
# Recruitment type
dat.psy.sum %>% distinct(study, .keep_all = T) %>% 
  {cbind(k.clin = sum(
    .$recruitment %in% c("Outpatient", "Both Inpatient & Outpatient")),
         perc.clin = sum(
    .$recruitment %in% c("Outpatient", 
                         "Both Inpatient & Outpatient"))/nrow(.))} -> recr.clin

# Mean age
NA -> age.mean

# Percentage of women
NA -> women.prop

# Type of control groups
dat.psy.sum %>% 
  mutate(study_cg = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_cg, .keep_all = TRUE) %>% pull(condition_arm2) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> controls

# Countries of origin
dat.psy.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(country) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> country

# Low risk of bias
dat.psy.sum %>% 
  distinct(study, .keep_all = TRUE) %>% pull(rob) %>% 
  {ifelse(. > 0, "low rob", "high rob")} %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> rob

# Intervention format
dat.psy.sum %>% 
  mutate(study_ig = paste(study, condition_arm2, sep = "_")) %>% 
  distinct(study_ig, .keep_all = TRUE) %>% pull(format) %>% 
  {cbind(t(table(.)), t(table(.)/length(.)))} -> format

# Number of sessions
NA -> sessions

# Combined characteristics & save
cbind(n1, n2, n, ncomp, neffs, k, recr.clin, age.mean, women.prop, controls,
      country, rob, format, sessions) -> characteristics

write.xlsx(characteristics, 
           file = "results/psychosis/results.xlsx", 
           sheetName = "characteristics", append = TRUE)

# Save full references of included studies
dat.psy.sum %>% 
  distinct(study, .keep_all = T) %>% select(reference) %>% 
  write.xlsx(file = "results/psychosis/results.xlsx", 
             sheetName = "references", append = TRUE)



## 2. Forest Plot --------------------------------------------------------------

# Compile data
list(m.dep, m.sad, m.pan, m.gad, m.ocd, m.pho, 
     m.bpd, m.ptsd, m.grief, m.gam, m.psy, m.gad) %>% 
  map(resultExtractor) %>% do.call(rbind, .) %>% as.data.frame() %>% 
  mutate(.TE = as.numeric(.TE), .seTE = as.numeric(.seTE),
         Disorder = c("Depressive Disorders", "Social Anxiety Disorder", 
                      "Panic Disorder", "Generalized Anxiety", 
                      "Obsessive-Compulsive Disorder", "Specific Phobia",
                      "Borderline Personality Disorder", 
                      "Post-Traumatic Stress Disorder",
                      "Prolonged Grief Disorder²", "Problem Gambling³",
                      "Psychotic Disorders¹", "Suicidal Ideation")
         ) -> dat.forest

# Clean digits of prediction interval
dat.forest[1,"pi"] = "[-0.50; 1.96]"

# Generate plot
meta::metagen(.TE, .seTE, data = dat.forest) %>% 
  meta::forest.meta(
    sortvar = TE, 
    leftcols = c("Disorder", "k", "n", "g", "pi", "nnt"),
    leftlabs = c(expression(bold(Disorder)), expression(bold(n[effects])), 
                 expression(bold(k)), expression(bold("g (95%-CI)")), 
                 expression(bold("95%-PI")), expression(bold("NNT"))),
    rightcols = c("tau.between", "tau.within"),
    rightlabs = c(expression(bold(hat(tau)[between]^2)~(bold(I)^2)), 
                  expression(bold(hat(tau)[within]^2)~(bold(I)^2))),
    xlim = c(0.20,1.5), col.square = "dodgerblue",
    overall = FALSE, hetstat = FALSE, 
    just.addcols.left = c("center", "center", "left", "left", "center"),
    just.addcols.right = c("left", "left"),
    fontfamily = "Roboto Slab")


