library(xlsx)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(paletteer)
library(cowplot)

dat = read.xlsx("data/publications.xlsx", sheetIndex = 1)
dat.rev = read.xlsx("data/publications.xlsx", sheetIndex = 2)


dat %>% 
  pivot_longer(-year, names_to = "group", values_to = "publications") %>% 
  mutate(
    group = recode(
      group, "overall" = "Overall",
        "psychometrics_methodology" = "Psychometrics & Methodology",
        "experimental_psychology_neuroscience" = "Experimental Psychology & Neuroscience",
        "social_psychology" = "Social Psychology",
        "clinical_health_psychology" = "Clinical & Health Psychology",
        "child_educational_psychology" = "Developmental & Educational Psychology",
        "organizational_psychology_human_factors" = "Organizational Psychology & Human Factors"),
    group = fct_reorder(group, -publications, function(x) x[length(x)])
    ) %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, y = publications, group = group, 
             fill = group, color = group)) +
  geom_line() + geom_point(size=0.25) +
  theme_minimal() + geom_hline(yintercept = 0) + 
  scale_colour_paletteer_d("ggsci::nrc_npg") +
  xlab("") + ylab("") + ggtitle("Newly Published") + 
  theme(legend.position = "none",
        text=element_text(family="Roboto Slab"),
        plot.title = element_text(hjust = 1, size=9)) -> plt1


dat %>% 
  mutate(overall = cumsum(overall),
         psychometrics_methodology = cumsum(psychometrics_methodology),
         experimental_psychology_neuroscience = cumsum(experimental_psychology_neuroscience),
         social_psychology = cumsum(social_psychology),
         clinical_health_psychology = cumsum(clinical_health_psychology),
         child_educational_psychology = cumsum(child_educational_psychology),
         organizational_psychology_human_factors = cumsum(organizational_psychology_human_factors)) %>% 
  pivot_longer(-year, names_to = "group", values_to = "publications") %>% 
  mutate(
    group = recode(
      group, "overall" = "Overall",
      "psychometrics_methodology" = "Psychometrics & Methodology",
      "experimental_psychology_neuroscience" = "Experimental Psychology & Neuroscience",
      "social_psychology" = "Social Psychology",
      "clinical_health_psychology" = "Clinical & Health Psychology",
      "child_educational_psychology" = "Developmental & Educational Psychology",
      "organizational_psychology_human_factors" = "Organizational Psychology & Human Factors"),
    group = fct_reorder(group, -publications, function(x) x[length(x)])
  ) %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, y = publications, group = group, 
             fill = group, color = group)) +
  geom_line() + geom_point(size=0.25) +
  theme_minimal() + geom_hline(yintercept = 0) + 
  scale_colour_paletteer_d("ggsci::nrc_npg") +
  xlab("") + ylab("") + ggtitle("Available") + 
  theme(legend.title = element_blank(),
        text=element_text(family="Roboto Slab"),
        plot.title = element_text(hjust = 1, size=9)) -> plt2


dat.rev %>% 
  pivot_longer(-year, names_to = "group", values_to = "publications") %>% 
  mutate(
    group = recode(
      group, "overall" = "Overall",
      "psychometrics_methodology" = "Psychometrics & Methodology",
      "experimental_psychology_neuroscience" = "Experimental Psychology & Neuroscience",
      "social_psychology" = "Social Psychology",
      "clinical_health_psychology" = "Clinical & Health Psychology",
      "child_educational_psychology" = "Developmental & Educational Psychology",
      "organizational_psychology_human_factors" = "Organizational Psychology & Human Factors"),
    group = fct_reorder(group, -publications, function(x) x[length(x)])
  ) %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, y = publications, group = group, 
             fill = group, color = group)) +
  geom_line() + geom_point(size=0.25) +
  theme_minimal() + geom_hline(yintercept = 0) + 
  scale_y_continuous(breaks = c(0, 2.5e3, 5e3, 7.5e3, 10e3)) +
  scale_colour_paletteer_d("ggsci::nrc_npg") +
  xlab("") + ylab("") + ggtitle("Newly Published") + 
  theme(legend.position = "none",
        text=element_text(family="Roboto Slab"),
        plot.title = element_text(hjust = 1, size=9)) -> plt1.rev


dat.rev %>% 
  mutate(overall = cumsum(overall),
         psychometrics_methodology = cumsum(psychometrics_methodology),
         experimental_psychology_neuroscience = cumsum(experimental_psychology_neuroscience),
         social_psychology = cumsum(social_psychology),
         clinical_health_psychology = cumsum(clinical_health_psychology),
         child_educational_psychology = cumsum(child_educational_psychology),
         organizational_psychology_human_factors = cumsum(organizational_psychology_human_factors)) %>% 
  pivot_longer(-year, names_to = "group", values_to = "publications") %>% 
  mutate(
    group = recode(
      group, "overall" = "Overall",
      "psychometrics_methodology" = "Psychometrics & Methodology",
      "experimental_psychology_neuroscience" = "Experimental Psychology & Neuroscience",
      "social_psychology" = "Social Psychology",
      "clinical_health_psychology" = "Clinical & Health Psychology",
      "child_educational_psychology" = "Developmental & Educational Psychology",
      "organizational_psychology_human_factors" = "Organizational Psychology & Human Factors"),
    group = fct_reorder(group, -publications, function(x) x[length(x)])
  ) %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, y = publications, group = group, 
             fill = group, color = group)) +
  geom_line() + geom_point(size=0.25) +
  theme_minimal() + geom_hline(yintercept = 0) + 
  scale_colour_paletteer_d("ggsci::nrc_npg") +
  xlab("") + ylab("") + ggtitle("Available") + 
  theme(legend.title = element_blank(),
        text=element_text(family="Roboto Slab"),
        plot.title = element_text(hjust = 1, size=9)) -> plt2.rev


plot_grid(plt1, plt2, plt1.rev, plt2.rev,
          rel_widths = c(1.25,2), label_fontfamily = "Roboto Slab",
          labels = c("Articles", NA, "Reviews", NA),
          hjust = -0.25) -> grob

ggsave(grob, file="results/plots/publications_plot.png",
       bg = "white", width = 22, height = 11, dpi = 400,
       scale = 0.5)



