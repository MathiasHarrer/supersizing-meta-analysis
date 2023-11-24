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
library(writexl)
library(metapsyTools)
source("utils/utils.R")

modelList = c("threelevel.che", "combined", "outliers",
              "lowest.highest", "rob")


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

#### 1.1.3 Save Results --------------------------------------------------------

res.list = list(pooled_effects = m.dep$summary,
                variance_components = m.dep$model.threelevel.che.var.comp,
                publication_bias = m.dep$correctPublicationBias$summary,
                subgroup_analysis = m.dep.sg,
                references = dat.dep %>% distinct(full_ref))
writexl::write_xlsx(res.list, path="results/depression/results.xlsx")



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

#### 1.2.3 Save Results --------------------------------------------------------

res.list = list(pooled_effects = m.sad$summary,
                variance_components = m.sad$model.threelevel.che.var.comp,
                publication_bias = m.sad$correctPublicationBias$summary,
                subgroup_analysis = m.sad.sg)
writexl::write_xlsx(res.list, path="results/social_anxiety/results.xlsx")



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

#### 1.3.3 Save Results --------------------------------------------------------

res.list = list(pooled_effects = m.pan$summary,
                variance_components = m.pan$model.threelevel.che.var.comp,
                publication_bias = m.pan$correctPublicationBias$summary,
                subgroup_analysis = m.pan.sg,
                references = dat.pan %>% distinct(full_ref))
writexl::write_xlsx(res.list, path="results/panic/results.xlsx")


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

#### 1.4.3 Save Results --------------------------------------------------------

res.list = list(pooled_effects = m.gad$summary,
                variance_components = m.gad$model.threelevel.che.var.comp,
                publication_bias = m.gad$correctPublicationBias$summary,
                subgroup_analysis = m.gad.sg,
                references = dat.gad %>% distinct(full_ref))
writexl::write_xlsx(res.list, path="results/generalized_anxiety/results.xlsx")



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

#### 1.5.3 Save Results --------------------------------------------------------

res.list = list(pooled_effects = m.ocd$summary,
                variance_components = m.ocd$model.threelevel.che.var.comp,
                publication_bias = m.ocd$correctPublicationBias$summary,
                subgroup_analysis = m.ocd.sg,
                references = dat.ocd %>% distinct(full_ref))
writexl::write_xlsx(res.list, path="results/ocd/results.xlsx")



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

#### 1.6.3 Save Results --------------------------------------------------------

res.list = list(pooled_effects = m.pho$summary,
                variance_components = m.pho$model.threelevel.che.var.comp,
                publication_bias = m.pho$correctPublicationBias$summary,
                subgroup_analysis = m.pho.sg,
                references = dat.pho %>% distinct(full_ref))
writexl::write_xlsx(res.list, path="results/phobia/results.xlsx")



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

#### 1.7.3 Save Results --------------------------------------------------------

res.list = list(pooled_effects = m.bpd$summary,
                variance_components = m.bpd$model.threelevel.che.var.comp,
                publication_bias = m.bpd$correctPublicationBias$summary,
                subgroup_analysis = m.bpd.sg,
                references = dat.bpd %>% distinct(full_ref))
writexl::write_xlsx(res.list, path="results/borderline/results.xlsx")



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

#### 1.8.3 Save Results --------------------------------------------------------

res.list = list(pooled_effects = m.ptsd$summary,
                variance_components = m.ptsd$model.threelevel.che.var.comp,
                publication_bias = m.ptsd$correctPublicationBias$summary,
                subgroup_analysis = m.ptsd.sg,
                references = dat.ptsd %>% distinct(full_ref))
writexl::write_xlsx(res.list, path="results/ptsd/results.xlsx")



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

#### 1.9.3 Save Results --------------------------------------------------------

res.list = list(pooled_effects = m.grief$summary,
                variance_components = m.grief$model.threelevel.che.var.comp,
                publication_bias = m.grief$correctPublicationBias$summary,
                subgroup_analysis = m.grief.sg,
                references = dat.grief %>% distinct(full_ref))
writexl::write_xlsx(res.list, path="results/grief/results.xlsx")



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

#### 1.10.3 Save Results -------------------------------------------------------

res.list = list(pooled_effects = m.gam$summary,
                variance_components = m.gam$model.threelevel.che.var.comp,
                publication_bias = m.gam$correctPublicationBias$summary,
                subgroup_analysis = m.gam.sg,
                references = dat.gam %>% distinct(full_ref))
writexl::write_xlsx(res.list, path="results/gambling/results.xlsx")



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

#### 1.11.3 Save Results -------------------------------------------------------

res.list = list(pooled_effects = m.psy$summary,
                variance_components = m.psy$model.threelevel.che.var.comp,
                publication_bias = m.psy$correctPublicationBias$summary,
                subgroup_analysis = m.psy.sg,
                references = dat.psy %>% distinct(full_ref))
writexl::write_xlsx(res.list, path="results/psychosis/results.xlsx")



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

#### 1.12.3 Save Results -------------------------------------------------------

res.list = list(pooled_effects = m.sui$summary,
                variance_components = m.sui$model.threelevel.che.var.comp,
                publication_bias = m.sui$correctPublicationBias$summary,
                subgroup_analysis = m.sui.sg,
                references = dat.sui %>% distinct(full_ref))
writexl::write_xlsx(res.list, path="results/suicide/results.xlsx")



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
