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

#### 1.1.3 Study References ----------------------------------------------------

dat.dep %>% 
  distinct(full_ref) %>% 
  write.xlsx(file = "results/depression/results.xlsx", 
             sheetName = "references", append = TRUE)



### 1.2 Social Anxiety Disorder ------------------------------------------------

#### 1.2.1 Pooled Effect -------------------------------------------------------

# Load data
dat.sad = read.csv("data/social_anxiety.csv")

# Run meta-analysis and correct publication bias
m.sad = runMetaAnalysis(dat.sad, which.run = modelList,
                        which.outliers = "combined", 
                        which.influence = "combined",
                        which.rob = "combined", low.rob.filter = "rob == 3",
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

#### 1.2.3 Study References ----------------------------------------------------

dat.sad %>% 
  distinct(full_ref) %>% 
  write.xlsx(file = "results/social_anxiety/results.xlsx", 
             sheetName = "references", append = TRUE)



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

#### 1.3.3 Study References ----------------------------------------------------

# Save full references of included studies
dat.pan %>% 
  distinct(full_ref) %>% 
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

#### 1.4.3 Study References ----------------------------------------------------

# Save full references of included studies
dat.gad %>% 
  distinct(full_ref) %>% 
  write.xlsx(file = "results/generalized_anxiety/results.xlsx", 
             sheetName = "references", append = TRUE)



### 1.5 Obsessive-Compulsive Disorder ------------------------------------------

#### 1.5.1 Pooled Effect -------------------------------------------------------

# Load data
dat.ocd = read.csv("data/ocd.csv") %>% 
  filter(time == "post", format != "ush", 
         target_group %in% c("adul", "ppd", "oth"))

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
      condition_arm2 %in% c("cau")) %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .05) %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "cau"; .$summary}
) -> m.ocd.sg

# Save the results
write.xlsx(m.ocd.sg, file = "results/ocd/results.xlsx", 
           sheetName = "subgroup_analysis", append = TRUE)


#### 1.5.3 Study References ----------------------------------------------------

dat.ocd %>% 
  distinct(full_ref) %>% 
  write.xlsx(file = "results/ocd/results.xlsx", 
             sheetName = "references", append = TRUE)



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

#### 1.6.3 Study References ----------------------------------------------------

dat.pho %>% 
  distinct(full_ref) %>% 
  write.xlsx(file = "results/phobia/results.xlsx", 
             sheetName = "references", append = TRUE)



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

#### 1.7.3 Study References ----------------------------------------------------

dat.bpd %>% 
  distinct(full_ref) %>% 
  write.xlsx(file = "results/borderline/results.xlsx", 
             sheetName = "references", append = TRUE)



### 1.8 Post-Traumatic Stress Disorder -----------------------------------------

#### 1.8.1 Pooled Effect -------------------------------------------------------

# Load data
dat.ptsd = read.csv("data/ptsd.csv") 
dat.ptsd$rob = ifelse(dat.ptsd$rob %in% c("Low", "Some Concerns"), 1, 0)
dat.ptsd$format = ifelse(dat.ptsd$delivery_method == "Technology Assisted",
                         "Guided Self-Help", dat.ptsd$format)

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
    subgroupAnalysis(condition_arm1.lumped_category) %>% 
    {.$summary$control = "cau"; .$summary},
  dat.ptsd %>% 
    filterPoolingData(
      condition_arm2 %in% c("Waitlist")) %>% 
    runMetaAnalysis(which.run = "combined", nnt.cer = .10) %>% 
    subgroupAnalysis(condition_arm1.lumped_category) %>% 
    {.$summary$control = "wl"; .$summary}
) -> m.ptsd.sg

# Save the results
write.xlsx(m.ptsd.sg, file = "results/ptsd/results.xlsx", 
           sheetName = "subgroup_analysis", append = TRUE)

#### 1.8.3 Study References ----------------------------------------------------

dat.ptsd %>% 
  distinct(full_ref) %>% 
  write.xlsx(file = "results/ptsd/results.xlsx", 
             sheetName = "references", append = TRUE)



### 1.9 Complicated Grief ------------------------------------------------------

#### 1.9.1 Pooled Effect -------------------------------------------------------

# Load data
dat.grief = read.csv("data/grief.csv") %>% 
  filter(age_group != 1)

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

#### 1.9.3 Study References ----------------------------------------------------

dat.grief %>% 
  distinct(full_ref) %>% 
  write.xlsx(file = "results/grief/results.xlsx", 
             sheetName = "references", append = TRUE)



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

#### 1.10.3 Study References ---------------------------------------------------

dat.gam %>% 
  distinct(full_ref) %>% 
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

#### 1.11.3 Study References ---------------------------------------------------

dat.psy %>% 
  distinct(reference) %>% 
  write.xlsx(file = "results/psychosis/results.xlsx", 
             sheetName = "references", append = TRUE)



### 1.12 Suicide ---------------------------------------------------------------

#### 1.12.1 Pooled Effect ------------------------------------------------------

# Load data
dat.sui = read.csv("data/suicide.csv") %>% 
  filter(age_group %in% c("adult", "yadul", "old"))

# Run meta-analysis and correct publication bias
m.sui = runMetaAnalysis(dat.sui, which.run = modelList,
                        which.outliers = "combined", 
                        which.influence = "combined",
                        which.rob = "combined", low.rob.filter = "rob == 1",
                        nnt.cer = .18) %>% 
  correctPublicationBias(which.run = "combined")

# Save the model & results
save(m.sui, file="results/suicide/m.sui.rda")
write.xlsx(m.sui$summary, file = "results/suicide/results.xlsx", 
           sheetName = "pooled_effects")
write.xlsx(m.sui$model.threelevel.che.var.comp, 
           file = "results/suicide/results.xlsx", 
           sheetName = "variance_components", append = TRUE)
write.xlsx(m.sui$correctPublicationBias$summary, 
           file = "results/suicide/results.xlsx", 
           sheetName = "publication_bias", append = TRUE)

# Generate profile plots of the variance components
pdf("results/suicide/profile_che_model.pdf")
metafor::profile.rma.mv(m.sui$model.threelevel.che)
dev.off()

#### 1.12.2 Subgroup Analysis --------------------------------------------------

rbind(
  dat.sui %>% 
    filterPoolingData(condition_arm2 %in% c("wl", "wlc")) %>% 
    runMetaAnalysis(which.run = "combined") %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "wl"; .$summary},
  dat.sui %>% 
    filterPoolingData(condition_arm2 %in% c("cau", "ecau", "cams", "supp")) %>% 
    runMetaAnalysis(which.run = "combined") %>% 
    subgroupAnalysis(condition_arm1) %>% 
    {.$summary$control = "cau"; .$summary}
) -> m.sui.sg

# Save the results
write.xlsx(m.sui.sg, file = "results/suicide/results.xlsx", 
           sheetName = "subgroup_analysis", append = TRUE)

#### 1.12.3 Study References ----------------------------------------------

dat.sui %>% 
  distinct(full_reference) %>% 
  write.xlsx(file = "results/suicide/results.xlsx", 
             sheetName = "references", append = TRUE)



## 2. Forest Plot --------------------------------------------------------------

# Compile data
list(m.dep, m.sad, m.pan, m.gad, m.ocd, m.pho, 
     m.bpd, m.ptsd, m.grief, m.gam, m.psy, m.sui) %>% 
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
dat.forest[4,"pi"] = "[-0.18; 1.90]"
dat.forest[12,"pi"] = "[-0.52; 1.20]"

# Generate plot
png("results/plots/forest.png", res=800, width=8550, height=2800)
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
    just.addcols.right = c("left", "left")); dev.off()
