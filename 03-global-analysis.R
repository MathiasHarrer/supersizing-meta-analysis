# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                             #
#  FEATURES AND FINDINGS OF THE METAPSY META-ANALYTIC RESEARCH DOMAIN         #
#  FOR PSYCHOLOGICAL TREATMENTS                                               #
#                                                                             #
#  III. Global Analysis ----                                                     
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(tidyverse)
library(xlsx)
library(metapsyTools)
library(maps)
library(paletteer)
library(cowplot)
library(gt)
library(sjPlot)
library(zoo)
library(caret)
library(metafor)
library(emmeans)
library(RANN)

source("utils/utils.R")


## 1. World Maps ---------------------------------------------------------------

# Load combined country data
read.csv("data/income/combined.csv") %>% 
  distinct(study, country_origin, .keep_all = T) -> dat.comb

# Load world map data
world = map_data("world")

# Check countries not matched by worldmap
setdiff(dat.comb$country_origin, world$region)

# Rename to fit 'world' object
dat.comb %>% 
  mutate(country_origin = 
           recode(country_origin, "United Kingdom" = "UK")) -> dat.comb

# Create world plot data
dat.comb %>% 
  group_by(country_origin) %>% 
  summarise(k = n()) %>% 
  rename(region = country_origin) %>% 
  full_join(world, "region") -> dat.plt

# Define theme for world map
plain = theme(
  text = element_text(family="Roboto Slab"),
  legend.title = element_blank(),
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0)
)

# Plot 1: Trials per country
dat.plt %>% 
  filter(region != "Antarctica") %>% 
  ggplot(mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) +
    geom_polygon(aes(fill = k), color = "black", linewidth = .2) +
    scale_fill_distiller(palette ="Greens", direction = 1, na.value="lightgray") + 
    ggtitle("Number of Psychotherapy Trials") +
    plain -> p1


# Plot 2: Trials per country, per capita
# Worldwide population estimates 1960-2017
# Source: World Bank, https://data.worldbank.org/indicator/SP.POP.TOTL
data("countrypops")  
countrypops %>% filter(year == 2017) -> countrypops

# Check countries not matched by countrypops
setdiff(dat.plt %>% filter(k>0) %>% pull(region), 
        countrypops$country_name)

# Adapt country names in countrypop 
countrypops %>% 
  mutate(region = 
           recode(country_name, 
                  "United States" = "USA",
                  "Korea (Republic of)" = "South Korea",
                  "United Kingdom" = "UK",
                  "Iran (Islamic Republic)" = "Iran")) -> countrypops

# Add population estimate of Taiwan
rbind(countrypops,
      data.frame(country_name = NA, country_code_2 = NA, 
                 country_code_3 = NA, year = 2017, 
                 population = 23570000,
                 region = "Taiwan")) -> countrypops

# Generate plot
full_join(dat.plt, countrypops, "region") %>% 
  mutate(k.s = k/(population/1e6)) %>% 
  filter(region != "Antarctica") %>% 
    ggplot(mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) +
    geom_polygon(aes(fill = k.s), color = "black", linewidth = .2) +
    scale_fill_distiller(palette ="Blues", direction = 1, na.value="lightgray") + 
    ggtitle("Number of Psychotherapy Trials, Per Capita") +
    plain -> p2

# Save combined plot
png("results/00-global_analysis/maps.png", 
    3000, 3000, res = 300, bg = "white")
cowplot::plot_grid(p1, p2, ncol=1, nrow=2)
dev.off()

# Save world map data
full_join(dat.plt, countrypops, "region") %>% 
  mutate(studies.per.million = k/(population/1e6),
         studies = k) %>% 
  select(region, studies, studies.per.million) %>% 
  distinct(region, .keep_all = TRUE) %>% 
  arrange(-studies) %>% drop_na() %>% 
  write.xlsx("results/00-global_analysis/studies_world.xlsx")


## 2. Income-Level Analysis ----------------------------------------------------

# Load combined data
read.csv("data/income/combined.csv") -> dat.comb

# Create Poisson data
dat.comb %>% distinct(study, .keep_all=T) %>% 
  mutate(year = 
           str_extract(study, "\\b(19[0-9]{2}|20[0-9]{2})[a-z]?\\b") %>% 
           parse_number()) %>% 
  group_by(income, year) %>% 
  summarise(k = n()) -> dat.comb.pois

# Fit Poisson model
m = glm(k ~ scale(year) * income, 
        family = poisson(), data = dat.comb.pois)

# Save Poisson model & data
write.xlsx(summary(m)$coefficients, 
           file = "results/00-global_analysis/poisson.xlsx",
           sheetName = "coefficients")
write.xlsx(dat.comb.pois %>% 
             pivot_wider(names_from = income, values_from = k), 
           file = "results/00-global_analysis/poisson.xlsx",
           sheetName = "data", showNA = FALSE, append = TRUE)

# Save count data by database
dat.comb %>% 
  distinct(study, .keep_all=T) %>% 
  group_by(db, income) %>% 
  summarise(k=n()) %>% 
  pivot_wider(values_from = k, names_from = income) %>% 
  select(-`NA`) %>% 
  as.data.frame() %>% 
  write.xlsx(file = "results/00-global_analysis/poisson.xlsx",
             sheetName = "data_disorder", showNA = FALSE, append = TRUE)

# Create data matrix and predict counts
map_dfr(as.list(c("L", "LM", "UM", "H")), function(x){
  data.frame(year = 1973:2022, 
             income = x)}) -> newdata
newdata$k = predict(m, newdata, "response")

# Plot predicted number of trials by income group
newdata %>% 
  mutate(income = fct_reorder(income, k, max, .desc=T)) %>% 
  ggplot(aes(x = year, y = k, color = income)) +
  geom_line(linewidth = 0.8, lineend = "round") +
  geom_point(size = 0.7) +
  scale_colour_paletteer_d("ggsci::nrc_npg", 
     labels = c("High", "Upper-Middle", "Lower-Middle", "Low")) +
  xlab("Year") + ylab("Predicted Number of Studies") +
  guides(size = "legend", alpha = "none") +
  theme_sjplot() +
  theme(text = element_text(family = "Roboto Slab"),
        legend.title = element_blank()) -> p1

# Plot percentage of trials by income group
dat.comb.pois %>% 
  pivot_wider(names_from = income, values_from = k) %>% 
  rowwise() %>% 
  mutate(total = sum(L,LM,UM,H, na.rm = T)) %>% 
  mutate(H=H/total, L=L/total, UM=UM/total, LM=LM/total) %>% 
  replace_na(list(H=0, L=0, LM=0, UM=0)) %>% select(-`NA`) %>% 
  filter(year < 2023) %>% 
  {rbind(data.frame(year = .$year[-c(1:2,47:49)], H = .$H %>% rollmean(5), 
                    L = .$L %>% rollmean(5), LM = .$LM %>% rollmean(5),
                    UM = .$UM %>% rollmean(5)),
         data.frame(year = .$year[47:48], H = .$H[47:48], L = .$L[47:48],
                    LM = .$LM[47:48], UM = .$UM[47:48]))} %>% 
  pivot_longer(H:UM) %>% 
  mutate(name = fct_reorder(name, value, sum, .desc=T)) %>% 
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line(linewidth = 0.8, lineend = "round") +
  geom_point(size = 0.7) +
  scale_colour_paletteer_d("ggsci::nrc_npg", 
    labels = c("High", "Upper-Middle", "Lower-Middle", "Low")) +
  xlab("Year") + ylab("Share of Studies") +
  guides(size = "legend", alpha = "none") +
  theme_sjplot() +
  scale_y_continuous(position = "right", 
                     labels = function(x) paste0(x*100, "%")) +
  theme(text = element_text(family = "Roboto Slab"),
        legend.title = element_blank()) -> p2


## 3. Cultural Region Analysis -------------------------------------------------

# Load combined data
read.csv("data/income/combined.csv") -> dat.comb

culture.recoder = 
  c("Italy" = "A - Western", "United Kingdom" = "A - Western", 
    "Australia" = "A - Western", "France" = "A - Western", 
    "USA" = "A - Western", "Germany" = "A - Western", "Finland" = "A - Western", 
    "Netherlands" = "A - Western", "Iran" = "B - Non-Western", 
    "Sweden" = "A - Western", "Spain" = "A - Western", 
    "Denmark" = "A - Western", "Zimbabwe" = "B - Non-Western", 
    "Portugal" = "A - Western", "Canada" = "A - Western", 
    "Peru" = "B - Non-Western", "Brazil" = "B - Non-Western", 
    "Indonesia" = "B - Non-Western", "China" = "A - Western", 
    "Switzerland" = "A - Western", "Uganda" = "B - Non-Western", 
    "Ireland" = "A - Western", "Taiwan" = "B - Non-Western", 
    "South Korea" = "B - Non-Western", "India" = "B - Non-Western", 
    "Lebanon" = "B - Non-Western", "Turkey" = "B - Non-Western", 
    "Nigeria" = "B - Non-Western", "Japan" = "B - Non-Western", 
    "Mexico" = "B - Non-Western", "Norway" = "A - Western", 
    "Jordan" = "B - Non-Western", "Pakistan" = "B - Non-Western", 
    "Nepal" = "B - Non-Western", "South Africa" = "B - Non-Western", 
    "Egypt" = "B - Non-Western", "Romania" = "A - Western", 
    "Malaysia" = "B - Non-Western", "Singapore" = "B - Non-Western", 
    "Croatia" = "A - Western", "Greece" = "A - Western", 
    "Colombia" = "B - Non-Western", "Austria" = "A - Western", 
    "Thailand" = "B - Non-Western", "Poland" = "A - Western", 
    "Israel" = "A - Western", "Chile" = "B - Non-Western", 
    "Belgium" = "A - Western", "Puerto Rico" = "B - Non-Western", 
    "Oman" = "B - Non-Western", "Saudi Arabia" = "B - Non-Western", 
    "Rwanda" = "B - Non-Western", "Iraq" ="B - Non-Western", 
    "Cambodia" = "B - Non-Western", "Sri Lanka" = "B - Non-Western")

dat.comb$country_origin %>% 
  recode(!!!culture.recoder) -> dat.comb$culture

# Create Poisson data
dat.comb %>% distinct(study, .keep_all=T) %>% 
  mutate(year = 
           str_extract(study, "\\b(19[0-9]{2}|20[0-9]{2})[a-z]?\\b") %>% 
           parse_number()) %>% 
  group_by(culture, year) %>% 
  summarise(k = n()) -> dat.comb.pois

# Fit Poisson model
m = glm(k ~ scale(year) * culture, 
        family = poisson(), data = dat.comb.pois)

# Save Poisson model & data
write.xlsx(summary(m)$coefficients, 
           file = "results/00-global_analysis/poisson.xlsx",
           sheetName = "coefficients_culture",
           showNA = FALSE, append = TRUE)
write.xlsx(dat.comb.pois %>% 
             pivot_wider(names_from = culture, values_from = k), 
           file = "results/00-global_analysis/poisson.xlsx",
           sheetName = "data_culture", showNA = FALSE, append = TRUE)
write.xlsx(dat.comb.pois %>% 
             pivot_wider(names_from = culture, values_from = k), 
           file = "results/00-global_analysis/poisson.xlsx",
           sheetName = "data_culture_disorder", showNA = FALSE, append = TRUE)

# Create data matrix and predict counts
map_dfr(as.list(c("A - Western", "B - Non-Western")), function(x){
  data.frame(year = 1973:2022, 
             culture = x)}) -> newdata
newdata$k = predict(m, newdata, "response")

# Plot predicted number of trials by income group
newdata %>% 
  mutate(culture = fct_reorder(culture, k, max, .desc=T)) %>% 
  ggplot(aes(x = year, y = k, color = culture)) +
  geom_line(linewidth = 0.8, lineend = "round") +
  geom_point(size = 0.7) +
  scale_colour_paletteer_d("ggsci::nrc_npg", 
                           labels = c("Western", "Non-Western")) +
  xlab("Year") + ylab("Predicted Number of Studies") +
  guides(size = "legend", alpha = "none") +
  theme_sjplot() +
  theme(text = element_text(family = "Roboto Slab"),
        legend.title = element_blank()) -> p1.culture

# Plot percentage of trials by culture group
dat.comb.pois %>% 
  pivot_wider(names_from = culture, values_from = k) %>% 
  rowwise() %>% 
  mutate(total = sum(`A - Western`, `B - Non-Western`, na.rm = T)) %>% 
  mutate(western = `A - Western`/total, 
         non_western = `B - Non-Western`/total) %>% 
  replace_na(list(western=0, non_western=0)) %>% 
  select(year, western, non_western) %>% 
  filter(year < 2023) %>% 
  { rbind(data.frame(year = .$year[-c(1:2,47:49)],
                     western = .$western %>% rollmean(5), 
                     non_western = .$non_western %>% rollmean(5)),
          data.frame(year = .$year[47:48],
                     western = .$western[47:48], 
                     non_western = .$non_western[47:48])) } %>% 
  pivot_longer(western:non_western) %>% 
  mutate(name = fct_reorder(name, value, sum, .desc=T)) %>% 
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line(linewidth = 0.8, lineend = "round") +
  geom_point(size = 0.7) +
  scale_colour_paletteer_d("ggsci::nrc_npg", 
                           labels = c("Western", "Non-Western")) +
  xlab("Year") + ylab("Share of Studies") +
  guides(size = "legend", alpha = "none") +
  theme_sjplot() +
  scale_y_continuous(position = "right", 
                     labels = function(x) paste0(x*100, "%")) +
  theme(text = element_text(family = "Roboto Slab"),
        legend.title = element_blank()) -> p2.culture

# Combine plots and save
png("results/00-global_analysis/counts.png", 
    3300, 2000, res = 350, bg = "white")
cowplot::plot_grid(p1, p2, p1.culture, p2.culture, ncol = 2, nrow = 2)
dev.off()



## 4. Meta-Regression ----------------------------------------------------------

# Load predictors for confounding adjustment
# Predictors were removed from the matrix under these conditions:
# - Zero or near zero variance
# - Model fitting leads to redundant predictors
# - Variable contains large number of missing values
# - Variable is not available
load("data/income/pred.rda")


### 4.1 Depression -------------------------------------------------------------

# Load data
read.csv("data/income/depression.csv") -> d
load("results/depression/m.dep.rda")
m.dep$model.influence -> m.inf
m.dep$nnt.cer -> nnt.cer

# Run income regression
f = makeFormula(pred$income[,"depression"])
labels(terms(f)) -> predictors
m.inf$data[m.inf$exclude,".studlab"] -> studies.exclude

d %>% select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "knnImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg
emmprep(m.reg, rg.limit = 4e4) %>% 
  emmeans("income", type="response", weights="proportional") -> m.emm

# Run culture regression
f = makeFormula(pred$culture[,"depression"])
labels(terms(f)) -> predictors

d %>% 
  select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "knnImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg.cul
emmprep(m.reg.cul, rg.limit = 4e4) %>% 
  emmeans("culture", type="response", weights="proportional") -> m.emm.cul

# Extract results
data.frame(cat = summary(m.emm)$income,
           k = d %>% filter(!study %in% studies.exclude) %>%  
             distinct(study, .keep_all = TRUE) %>% 
             group_by(income) %>% summarise(n()) %>% {.[,2]},
           g = summary(m.emm)$emmean,
           lower = summary(m.emm)$lower.CL,
           higher = summary(m.emm)$upper.CL,
           nnt = metapsyNNT(summary(m.emm)$emmean, nnt.cer),
           coef = coefficients(m.reg)[1:4],
           t = m.reg$zval[1:4],
           p = m.reg$pval[1:4]) -> r1
data.frame(cat = summary(m.emm.cul)$culture,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(culture) %>% summarise(n()) %>% {.[,2]},
           g = summary(m.emm.cul)$emmean,
           lower = summary(m.emm.cul)$lower.CL,
           higher = summary(m.emm.cul)$upper.CL,
           nnt = metapsyNNT(summary(m.emm.cul)$emmean, nnt.cer),
           coef = coefficients(m.reg.cul)[1:2],
           t = m.reg$zval[1:2],
           p = m.reg$pval[1:2]) -> r2
rbind(r1, r2) %>% 
  write.xlsx("results/00-global_analysis/regression.xlsx", sheet = "dep")



### 4.2 Panic ------------------------------------------------------------------

# Load data
read.csv("data/income/panic.csv") -> d
load("results/panic/m.pan.rda")
m.pan$model.influence -> m.inf
m.pan$nnt.cer -> nnt.cer

# Run income regression
f = makeFormula(pred$income[,"panic"])
labels(terms(f)) -> predictors
m.inf$data[m.inf$exclude,".studlab"] -> studies.exclude

d %>% select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "knnImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg
emmprep(m.reg, rg.limit = 4e4) %>% 
  emmeans("income", type="response", weights="proportional") -> m.emm

# Run culture regression
f = makeFormula(pred$culture[,"panic"])
labels(terms(f)) -> predictors

d %>% 
  select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "knnImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg.cul
emmprep(m.reg.cul, rg.limit = 4e4) %>% 
  emmeans("culture", type="response", weights="proportional") -> m.emm.cul

# Extract results
data.frame(cat = summary(m.emm)$income,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(income) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm)$emmean,
           lower = summary(m.emm)$lower.CL,
           higher = summary(m.emm)$upper.CL,
           nnt = metapsyNNT(summary(m.emm)$emmean, nnt.cer),
           coef = coefficients(m.reg)[1:2],
           t = m.reg$zval[1:2],
           p = m.reg$pval[1:2]) -> r1
data.frame(cat = summary(m.emm.cul)$culture,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(culture) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm.cul)$emmean,
           lower = summary(m.emm.cul)$lower.CL,
           higher = summary(m.emm.cul)$upper.CL,
           nnt = metapsyNNT(summary(m.emm.cul)$emmean, nnt.cer),
           coef = coefficients(m.reg.cul)[1:2],
           t = m.reg.cul$zval[1:2],
           p = m.reg.cul$pval[1:2]) -> r2
rbind(r1, r2) %>% 
  write.xlsx("results/00-global_analysis/regression.xlsx", sheet = "pan",
             append = TRUE)


### 4.3 SAD --------------------------------------------------------------------

# Load data
read.csv("data/income/social_anxiety.csv") -> d
load("results/social_anxiety/m.sad.rda")
m.sad$model.influence -> m.inf
m.sad$nnt.cer -> nnt.cer

# Run income regression
f = makeFormula(pred$income[,"social_anxiety"])
labels(terms(f)) -> predictors
m.inf$data[m.inf$exclude,".studlab"] -> studies.exclude

d %>% select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "knnImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg
emmprep(m.reg, rg.limit = 4e4) %>% 
  emmeans("income", type="response", weights="proportional") -> m.emm

# Run culture regression
f = makeFormula(pred$culture[,"social_anxiety"])
labels(terms(f)) -> predictors

d %>% 
  select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "knnImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg.cul
emmprep(m.reg.cul, rg.limit = 4e4) %>% 
  emmeans("culture", type="response", weights="proportional") -> m.emm.cul

# Extract results
data.frame(cat = summary(m.emm)$income,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(income) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm)$emmean,
           lower = summary(m.emm)$lower.CL,
           higher = summary(m.emm)$upper.CL,
           nnt = metapsyNNT(summary(m.emm)$emmean, nnt.cer),
           coef = coefficients(m.reg)[1:3],
           t = m.reg$zval[1:3],
           p = m.reg$pval[1:3]) -> r1
data.frame(cat = summary(m.emm.cul)$culture,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(culture) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm.cul)$emmean,
           lower = summary(m.emm.cul)$lower.CL,
           higher = summary(m.emm.cul)$upper.CL,
           nnt = metapsyNNT(summary(m.emm.cul)$emmean, nnt.cer),
           coef = coefficients(m.reg.cul)[1:2],
           t = m.reg.cul$zval[1:2],
           p = m.reg.cul$pval[1:2]) -> r2
rbind(r1, r2) %>% 
  write.xlsx("results/00-global_analysis/regression.xlsx", sheet = "sad",
             append = TRUE)


### 4.4 GAD --------------------------------------------------------------------

# Load data
read.csv("data/income/generalized_anxiety.csv") -> d
load("results/generalized_anxiety/m.gad.rda")
m.gad$model.influence -> m.inf
m.gad$nnt.cer -> nnt.cer

# Run income regression
f = makeFormula(pred$income[,"generalized_anxiety"])
labels(terms(f)) -> predictors
m.inf$data[m.inf$exclude,".studlab"] -> studies.exclude

d %>% select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg
emmprep(m.reg, rg.limit = 4e4) %>% 
  emmeans("income", type="response", weights="proportional") -> m.emm

# Run culture regression
f = makeFormula(pred$culture[,"generalized_anxiety"])
labels(terms(f)) -> predictors

d %>% 
  select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg.cul
emmprep(m.reg.cul, rg.limit = 4e4) %>% 
  emmeans("culture", type="response", weights="proportional") -> m.emm.cul

# Extract results
data.frame(cat = summary(m.emm)$income,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(income) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm)$emmean,
           lower = summary(m.emm)$lower.CL,
           higher = summary(m.emm)$upper.CL,
           nnt = metapsyNNT(summary(m.emm)$emmean, nnt.cer),
           coef = coefficients(m.reg)[1:3],
           t = m.reg$zval[1:3],
           p = m.reg$pval[1:3]) -> r1
data.frame(cat = summary(m.emm.cul)$culture,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(culture) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm.cul)$emmean,
           lower = summary(m.emm.cul)$lower.CL,
           higher = summary(m.emm.cul)$upper.CL,
           nnt = metapsyNNT(summary(m.emm.cul)$emmean, nnt.cer),
           coef = coefficients(m.reg.cul)[1:2],
           t = m.reg.cul$zval[1:2],
           p = m.reg.cul$pval[1:2]) -> r2
rbind(r1, r2) %>% 
  write.xlsx("results/00-global_analysis/regression.xlsx", sheet = "gad",
             append = TRUE)



### 4.5 Phobia -----------------------------------------------------------------

# Load data
read.csv("data/income/phobia.csv") -> d
load("results/phobia/m.pho.rda")
m.pho$model.influence -> m.inf
m.pho$nnt.cer -> nnt.cer

# Run income regression
f = makeFormula(pred$income[,"phobia"])
labels(terms(f)) -> predictors
m.inf$data[m.inf$exclude,".studlab"] -> studies.exclude

d %>% select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg
emmprep(m.reg, rg.limit = 4e4) %>% 
  emmeans("income", type="response", weights="proportional") -> m.emm

# Run culture regression
f = makeFormula(pred$culture[,"phobia"])
labels(terms(f)) -> predictors

d %>% 
  select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg.cul
emmprep(m.reg.cul, rg.limit = 4e4) %>% 
  emmeans("culture", type="response", weights="proportional") -> m.emm.cul

# Extract results
data.frame(cat = summary(m.emm)$income,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(income) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm)$emmean,
           lower = summary(m.emm)$lower.CL,
           higher = summary(m.emm)$upper.CL,
           nnt = metapsyNNT(summary(m.emm)$emmean, nnt.cer),
           coef = coefficients(m.reg)[1:2],
           t = m.reg$zval[1:2],
           p = m.reg$pval[1:2]) -> r1
data.frame(cat = summary(m.emm.cul)$culture,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(culture) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm.cul)$emmean,
           lower = summary(m.emm.cul)$lower.CL,
           higher = summary(m.emm.cul)$upper.CL,
           nnt = metapsyNNT(summary(m.emm.cul)$emmean, nnt.cer),
           coef = coefficients(m.reg.cul)[1:2],
           t = m.reg.cul$zval[1:2],
           p = m.reg.cul$pval[1:2]) -> r2
rbind(r1, r2) %>% 
  write.xlsx("results/00-global_analysis/regression.xlsx", sheet = "pho",
             append = TRUE)


### 4.6 PTSD -------------------------------------------------------------------

# Load data
read.csv("data/income/ptsd.csv") -> d
load("results/ptsd/m.ptsd.rda")
m.ptsd$model.influence -> m.inf
m.ptsd$nnt.cer -> nnt.cer

# Run income regression
f = makeFormula(pred$income[,"ptsd"])
labels(terms(f)) -> predictors
m.inf$data[m.inf$exclude,".studlab"] -> studies.exclude

d %>% select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg
emmprep(m.reg, rg.limit = 4e4) %>% 
  emmeans("income", type="response", weights="proportional") -> m.emm

# Run culture regression
f = makeFormula(pred$culture[,"ptsd"])
labels(terms(f)) -> predictors

d %>% 
  select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg.cul
emmprep(m.reg.cul, rg.limit = 4e4) %>% 
  emmeans("culture", type="response", weights="proportional") -> m.emm.cul

# Extract results
data.frame(cat = summary(m.emm)$income,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(income) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm)$emmean,
           lower = summary(m.emm)$lower.CL,
           higher = summary(m.emm)$upper.CL,
           nnt = metapsyNNT(summary(m.emm)$emmean, nnt.cer),
           coef = coefficients(m.reg)[1:4],
           t = m.reg$zval[1:4],
           p = m.reg$pval[1:4]) -> r1
data.frame(cat = summary(m.emm.cul)$culture,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(culture) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm.cul)$emmean,
           lower = summary(m.emm.cul)$lower.CL,
           higher = summary(m.emm.cul)$upper.CL,
           nnt = metapsyNNT(summary(m.emm.cul)$emmean, nnt.cer),
           coef = coefficients(m.reg.cul)[1:2],
           t = m.reg.cul$zval[1:2],
           p = m.reg.cul$pval[1:2]) -> r2
rbind(r1, r2) %>% 
  write.xlsx("results/00-global_analysis/regression.xlsx", sheet = "ptsd",
             append = TRUE)


### 4.7 OCD --------------------------------------------------------------------

# Load data
read.csv("data/income/ocd.csv") -> d
load("results/ocd/m.ocd.rda")
m.ocd$model.influence -> m.inf
m.ocd$nnt.cer -> nnt.cer

# Run income regression
f = makeFormula(pred$income[,"ocd"])
labels(terms(f)) -> predictors
m.inf$data[m.inf$exclude,".studlab"] -> studies.exclude

d %>% select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg
emmprep(m.reg, rg.limit = 4e4) %>% 
  emmeans("income", type="response", weights="proportional") -> m.emm

# Run culture regression
f = makeFormula(pred$culture[,"ocd"])
labels(terms(f)) -> predictors

d %>% 
  select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg.cul
emmprep(m.reg.cul, rg.limit = 4e4) %>% 
  emmeans("culture", type="response", weights="proportional") -> m.emm.cul

# Extract results
data.frame(cat = summary(m.emm)$income,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(income) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm)$emmean,
           lower = summary(m.emm)$lower.CL,
           higher = summary(m.emm)$upper.CL,
           nnt = metapsyNNT(summary(m.emm)$emmean, nnt.cer),
           coef = coefficients(m.reg)[1:3],
           t = m.reg$zval[1:3],
           p = m.reg$pval[1:3]) -> r1
data.frame(cat = summary(m.emm.cul)$culture,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(culture) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm.cul)$emmean,
           lower = summary(m.emm.cul)$lower.CL,
           higher = summary(m.emm.cul)$upper.CL,
           nnt = metapsyNNT(summary(m.emm.cul)$emmean, nnt.cer),
           coef = coefficients(m.reg.cul)[1:2],
           t = m.reg.cul$zval[1:2],
           p = m.reg.cul$pval[1:2]) -> r2
rbind(r1, r2) %>% 
  write.xlsx("results/00-global_analysis/regression.xlsx", sheet = "ocd",
             append = TRUE)



### 4.8 BPD --------------------------------------------------------------------

# Load data
read.csv("data/income/borderline.csv") -> d
load("results/borderline/m.bpd.rda")
m.bpd$model.influence -> m.inf
m.bpd$nnt.cer -> nnt.cer

# Run income regression
f = makeFormula(pred$income[,"bpd"])
labels(terms(f)) -> predictors
m.inf$data[m.inf$exclude,".studlab"] -> studies.exclude

d %>% select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg
emmprep(m.reg, rg.limit = 4e4) %>% 
  emmeans("income", type="response", weights="proportional") -> m.emm

# Run culture regression
f = makeFormula(pred$culture[,"bpd"])
labels(terms(f)) -> predictors

d %>% 
  select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg.cul
emmprep(m.reg.cul, rg.limit = 4e4) %>% 
  emmeans("culture", type="response", weights="proportional") -> m.emm.cul

# Extract results
data.frame(cat = summary(m.emm)$income,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(income) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm)$emmean,
           lower = summary(m.emm)$lower.CL,
           higher = summary(m.emm)$upper.CL,
           nnt = metapsyNNT(summary(m.emm)$emmean, nnt.cer),
           coef = coefficients(m.reg)[1:2],
           t = m.reg$zval[1:2],
           p = m.reg$pval[1:2]) -> r1
data.frame(cat = summary(m.emm.cul)$culture,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(culture) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm.cul)$emmean,
           lower = summary(m.emm.cul)$lower.CL,
           higher = summary(m.emm.cul)$upper.CL,
           nnt = metapsyNNT(summary(m.emm.cul)$emmean, nnt.cer),
           coef = coefficients(m.reg.cul)[1:2],
           t = m.reg.cul$zval[1:2],
           p = m.reg.cul$pval[1:2]) -> r2
rbind(r1, r2) %>% 
  write.xlsx("results/00-global_analysis/regression.xlsx", sheet = "bpd",
             append = TRUE)


### 4.9 Grief ------------------------------------------------------------------

# Load data
read.csv("data/income/grief.csv") -> d
load("results/grief/m.grief.rda")
m.grief$model.influence -> m.inf
m.grief$nnt.cer -> nnt.cer

# Run income regression
f = makeFormula(pred$culture[,"grief"])
labels(terms(f)) -> predictors
m.inf$data[m.inf$exclude,".studlab"] -> studies.exclude

d %>% 
  select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg.cul
emmprep(m.reg.cul, rg.limit = 4e4) %>% 
  emmeans("culture", type="response", weights="proportional") -> m.emm.cul

# Extract results
data.frame(cat = summary(m.emm.cul)$culture,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(culture) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm.cul)$emmean,
           lower = summary(m.emm.cul)$lower.CL,
           higher = summary(m.emm.cul)$upper.CL,
           nnt = metapsyNNT(summary(m.emm.cul)$emmean, nnt.cer),
           coef = coefficients(m.reg.cul)[1:2],
           t = m.reg.cul$zval[1:2],
           p = m.reg.cul$pval[1:2]) -> r2
r2 %>% 
  write.xlsx("results/00-global_analysis/regression.xlsx", sheet = "grief",
             append = TRUE)

### 4.10 Gambling --------------------------------------------------------------

# Load data
read.csv("data/income/gambling.csv") -> d
load("results/gambling/m.gam.rda")
m.gam$model.influence -> m.inf
m.gam$nnt.cer -> nnt.cer

# Run income regression
f = makeFormula(pred$income[,"gambling"])
labels(terms(f)) -> predictors
m.inf$data[m.inf$exclude,".studlab"] -> studies.exclude

d %>% select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg
emmprep(m.reg, rg.limit = 4e4) %>% 
  emmeans("income", type="response", weights="proportional") -> m.emm

# Run culture regression
f = makeFormula(pred$culture[,"gambling"])
labels(terms(f)) -> predictors

d %>% 
  select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg.cul
emmprep(m.reg.cul, rg.limit = 4e4) %>% 
  emmeans("culture", type="response", weights="proportional") -> m.emm.cul

# Extract results
data.frame(cat = summary(m.emm)$income,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(income) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm)$emmean,
           lower = summary(m.emm)$lower.CL,
           higher = summary(m.emm)$upper.CL,
           nnt = metapsyNNT(summary(m.emm)$emmean, nnt.cer),
           coef = coefficients(m.reg)[1:2],
           t = m.reg$zval[1:2],
           p = m.reg$pval[1:2]) -> r1
data.frame(cat = summary(m.emm.cul)$culture,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(culture) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm.cul)$emmean,
           lower = summary(m.emm.cul)$lower.CL,
           higher = summary(m.emm.cul)$upper.CL,
           nnt = metapsyNNT(summary(m.emm.cul)$emmean, nnt.cer),
           coef = coefficients(m.reg.cul)[1:2],
           t = m.reg.cul$zval[1:2],
           p = m.reg.cul$pval[1:2]) -> r2
rbind(r1, r2) %>% 
  write.xlsx("results/00-global_analysis/regression.xlsx", sheet = "gam",
             append = TRUE)

### 4.11 Psychosis -------------------------------------------------------------

# Load data
read.csv("data/income/psychosis.csv") -> d
load("results/psychosis/m.psy.rda")
m.psy$model.influence -> m.inf
m.psy$nnt.cer -> nnt.cer

# Run income regression
f = makeFormula(pred$income[,"psychosis"])
labels(terms(f)) -> predictors
m.inf$data[m.inf$exclude,".studlab"] -> studies.exclude

d %>% select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg
emmprep(m.reg, rg.limit = 4e4) %>% 
  emmeans("income", type="response", weights="proportional") -> m.emm

# Run culture regression
f = makeFormula(pred$culture[,"psychosis"])
labels(terms(f)) -> predictors

d %>% 
  select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg.cul
emmprep(m.reg.cul, rg.limit = 4e4) %>% 
  emmeans("culture", type="response", weights="proportional") -> m.emm.cul

# Extract results
data.frame(cat = summary(m.emm)$income,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(income) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm)$emmean,
           lower = summary(m.emm)$lower.CL,
           higher = summary(m.emm)$upper.CL,
           nnt = metapsyNNT(summary(m.emm)$emmean, nnt.cer),
           coef = coefficients(m.reg)[1:3],
           t = m.reg$zval[1:3],
           p = m.reg$pval[1:3]) -> r1
data.frame(cat = summary(m.emm.cul)$culture,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(culture) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm.cul)$emmean,
           lower = summary(m.emm.cul)$lower.CL,
           higher = summary(m.emm.cul)$upper.CL,
           nnt = metapsyNNT(summary(m.emm.cul)$emmean, nnt.cer),
           coef = coefficients(m.reg.cul)[1:2],
           t = m.reg.cul$zval[1:2],
           p = m.reg.cul$pval[1:2]) -> r2
rbind(r1, r2) %>% 
  write.xlsx("results/00-global_analysis/regression.xlsx", sheet = "psy",
             append = TRUE)

### 4.12 Suicide ---------------------------------------------------------------

# Load data
read.csv("data/income/suicide.csv") -> d
load("results/suicide/m.sui.rda")
m.sui$model.influence -> m.inf
m.sui$nnt.cer -> nnt.cer

# Run income regression
f = makeFormula(pred$income[,"suicide"])
labels(terms(f)) -> predictors
m.inf$data[m.inf$exclude,".studlab"] -> studies.exclude

d %>% select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg
emmprep(m.reg, rg.limit = 4e4) %>% 
  emmeans("income", type="response", weights="proportional") -> m.emm

# Run culture regression
f = makeFormula(pred$culture[,"suicide"])
labels(terms(f)) -> predictors

d %>% 
  select(all_of(predictors)) -> d.preds
preProcess(d.preds, c("center", "scale", "medianImpute")) %>% 
  predict(d.preds) -> d.preds
cbind(d %>% select(study, .g, .g_se), d.preds) -> d.imp

d.imp %>% filter(!study %in% studies.exclude) -> d.imp
runMetaAnalysis(d.imp, "threelevel.che") -> m
metaRegression(m$model.threelevel.che, f) -> m.reg.cul
emmprep(m.reg.cul, rg.limit = 4e4) %>% 
  emmeans("culture", type="response", weights="proportional") -> m.emm.cul

# Extract results
data.frame(cat = summary(m.emm)$income,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(income) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm)$emmean,
           lower = summary(m.emm)$lower.CL,
           higher = summary(m.emm)$upper.CL,
           nnt = metapsyNNT(summary(m.emm)$emmean, nnt.cer),
           coef = coefficients(m.reg)[1:3],
           t = m.reg$zval[1:3],
           p = m.reg$pval[1:3]) -> r1
data.frame(cat = summary(m.emm.cul)$culture,
           k = d %>% filter(!study %in% studies.exclude) %>% 
             distinct(study, .keep_all = TRUE) %>% 
             group_by(culture) %>% summarise(n()) %>% drop_na() %>% {.[,2]},
           g = summary(m.emm.cul)$emmean,
           lower = summary(m.emm.cul)$lower.CL,
           higher = summary(m.emm.cul)$upper.CL,
           nnt = metapsyNNT(summary(m.emm.cul)$emmean, nnt.cer),
           coef = coefficients(m.reg.cul)[1:2],
           t = m.reg.cul$zval[1:2],
           p = m.reg.cul$pval[1:2]) -> r2
rbind(r1, r2) %>% 
  write.xlsx("results/00-global_analysis/regression.xlsx", sheet = "sui",
             append = TRUE)


### 4.13 Combine results -------------------------------------------------------

rbind(
  read.xlsx("results/00-global_analysis/regression.xlsx","dep") %>% mutate(db="dep"),
  read.xlsx("results/00-global_analysis/regression.xlsx","pan") %>% mutate(db="pan"),
  read.xlsx("results/00-global_analysis/regression.xlsx","sad") %>% mutate(db="sad"),
  read.xlsx("results/00-global_analysis/regression.xlsx","gad") %>% mutate(db="gad"),
  read.xlsx("results/00-global_analysis/regression.xlsx","pho") %>% mutate(db="pho"),
  read.xlsx("results/00-global_analysis/regression.xlsx","ptsd") %>% mutate(db="ptsd"),
  read.xlsx("results/00-global_analysis/regression.xlsx","ocd") %>% mutate(db="ocd"),
  read.xlsx("results/00-global_analysis/regression.xlsx","bpd") %>% mutate(db="bpd"),
  read.xlsx("results/00-global_analysis/regression.xlsx","grief") %>% mutate(db="grief"),
  read.xlsx("results/00-global_analysis/regression.xlsx","gam") %>% mutate(db="gam"),
  read.xlsx("results/00-global_analysis/regression.xlsx","psy") %>% mutate(db="psy"),
  read.xlsx("results/00-global_analysis/regression.xlsx","sui") %>% mutate(db="sui")
) %>% 
  mutate(cat = recode(cat, `H` = "1 - high", `L` = "4 - low", 
                      `LM` = "3 - low middle", `UM` = "2 - upper middle"),
         g = format(round(g, 2), nsmall=2),
         ci = paste0("[", format(round(lower, 2), nsmall = 2) %>% trimws(), "; ",
                     format(round(higher, 2), nsmall = 2), "]"),
         nnt = format(round(nnt, 2), nsmall=2),
         delta.g = format(round(coef, 2), nsmall=2),
         t = format(round(t, 3), nsmall=3),
         p = scales::pvalue(p)) %>% 
  select(db, cat, `n..`, g, ci, nnt, delta.g, t, p) %>% 
  arrange(db, cat) %>% 
  write.xlsx("results/00-global_analysis/regression.xlsx", 
             sheet = "combined",
             append = TRUE)

