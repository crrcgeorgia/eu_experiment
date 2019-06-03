# Set working directory
# setwd()

## Load libraries

library(tidyverse)
library(sandwich)
library(lmtest)
library(ggeffects)
library(sjPlot)
library(survey)
library(haven)
library(multcomp)
library(plotly)

# Read and recode data

ndi_apr <- read_dta("NDI_2019_April_22.04.19.dta")

ndi_apr <- read_dta("NDI_2019_April_22.04.19.dta") %>%
  mutate(
  eu_sup = case_when(
    eu_exp == 1 ~ 1,
    eu_exp %in% c(-2, -1, 2, 3) ~ 0,
    TRUE ~ as.numeric(eu_exp)
  ),
  eu_op = case_when(
    eu_exp == 2 ~ 1,
    eu_exp %in% c(-2, -1, 1, 3) ~ 0,
    TRUE ~ as.numeric(eu_exp)
  ),
  eu_amb = case_when(
    eu_exp == 3 | eu_exp == -1 ~ 1,
    eu_exp %in% c(-2, 1, 2) ~ 0,
    TRUE ~ as.numeric(eu_exp)
  ),
  ussr = case_when(
    q41 == 1 ~ 1,
    TRUE ~ 0
  ),
  rr = case_when(
    q42 == 1 ~ 1,
    TRUE ~ 0
  ),
  gc = case_when(
    q43 == 1 ~ 1,
    TRUE ~ 0
  ),
  exposure = case_when(
    age > 41 ~ 1,
    TRUE ~ 0
  ),
  high_ed = case_when(
    q67 > 4 ~ 1,
    TRUE ~ 0
  ),
  tbilisi = case_when(
    psu < 2311003 ~ 1,
    TRUE ~ 0
  ),
  party = case_when(
    q5_1 == 8 ~ 1,
    q5_1 == 6 ~ 2,
    q5_1 == 25 ~ 4,
    q5_1 == -2 ~ 5,
    q5_1 == -1 ~ 6,
    TRUE ~ 3
  ),
  eu_pos = case_when(
    (q24 == 4 | q24 == 5) ~ 1,
    is.na(q24) ~ NA_real_,
    TRUE ~ 0
  ),
  eu_neg = case_when(
    (q24 == 1 | q24 == 2) ~ 1,
    is.na(q24) ~ NA_real_,
    TRUE ~ 0
  ),
  eu_neut = case_when(
    (q24 == 3 | q24 == -1) ~ 1,
    is.na(q24) ~ NA_real_,
    TRUE ~ 0
  ),
  ethnic_id = case_when(
    q66 == 3 ~ 1,
    q66 == 1 ~ 2,
    q66 == 2 ~ 3,
    is.na(q66) ~ NA_real_,
    TRUE ~ 4
  ),
  eu_beneficial = case_when(
    q19 == 1 | q19 == 2 ~ 1,
    is.na(q19) ~ NA_real_,
    TRUE ~ 0
  ),
  eeu_beneficial = case_when(
    q19 == 3 | q19 == 4 ~ 1,
    is.na(q19) ~ NA_real_,
    TRUE ~ 0
  ),
  hh_index=rowSums(ndi_apr[, 300:310], na.rm = TRUE),
  travel_eu = case_when(
    (q29 == 1 | q29 == 2) ~ 1,
    is.na(q29) ~ NA_real_,
    TRUE ~ 0
  ),
  q22r = case_when(
    q22r == -3 ~ NA_real_,
    TRUE ~ as.numeric(q22r)
  ),
  eu = case_when(
    q14 == 1 ~ 1,
    is.na(q14) ~ NA_real_,
    TRUE ~ 0
  ),
  lugar = case_when(
    q53_1 == 1 ~ 1,
    TRUE ~ 0
  ),
  market = case_when(
    q53_2 == 1 ~ 1,
    TRUE ~ 0
  ),
  refugees = case_when(
    q53_3 == 1 ~ 1,
    TRUE ~ 0
  ),
  military_base = case_when(
    q53_4 == 1 ~ 1,
    TRUE ~ 0
  ),
  traditions = case_when(
    q25_3 < 0  ~ NA_real_,
    TRUE ~ as.numeric(q25_3)
  ),
  empire = case_when(
    q25_5 < 0  ~ NA_real_,
    TRUE ~ as.numeric(q25_5)
  )
  )

# Run binary logistic model per each outcome

eu_sup_mod <- glm(eu_sup~factor(q22r), data=ndi_apr)
summary(eu_sup_mod)

eu_op_mod <- glm(eu_op~factor(q22r), data=ndi_apr)
summary(eu_op_mod)

eu_amb_mod <- glm(eu_amb~factor(q22r), data=ndi_apr)
summary(eu_amb_mod)

# Test multiple comparisons

glht_test <- glht(eu_op_mod)
summary(glht_test, test = adjusted("bonferroni"))


## Gather predictions

sup_pred <- as.data.frame((ggpredict(eu_sup_mod, terms = c("q22r"))))%>%
  mutate(group=1)

opp_pred <- as.data.frame((ggpredict(eu_op_mod, terms = c("q22r"))))%>%
  mutate(group=2)

amb_pred <- as.data.frame((ggpredict(eu_amb_mod, terms = c("q22r"))))%>%
  mutate(group=3)

pred <- sup_pred %>%
  bind_rows(opp_pred, amb_pred)%>%
  mutate(group=factor(group, levels = c(1:3), labels = c("Support", "Oppose", "Ambivalent")),
x=factor(x, levels = c(1, 2, 3, 4, 5),labels = c("Control",
                                                "EU economic benefit",
                                                "EU economic loss",
                                                "EU economic benefit + Russia military threat",
                                                "EU economic loss + Russia military threat"), ordered = TRUE))

# Build a plot

ggplot(pred, aes(x, predicted))+
  geom_point(aes(group=group, color=group), position = position_dodge2(width = 0.5, padding = 0.5))+
  geom_linerange(aes(ymin=conf.low, ymax=conf.high, color=group), position = position_dodge2(width = 0.5, padding = 0.5), size= 2)+
  ylim(0, 1)+
  geom_hline(yintercept=0.6324042, color="#377eb8")+
  geom_hline(yintercept=0.1553398, color="#e41a1c")+
  geom_hline(yintercept=0.2125436, color="#4daf4a")+
  labs(title="Would you vote for the EU membership\non a referendum held tomorrow?",
       subtitle="By Vignettes describing\npotential costs and benefits",
       x="Groups",
       y="Predicted probabilities",
       caption="Bars show 95% confidence intervals")+
  scale_color_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  coord_flip()

## Save interative and static plots

ggplotly()

ggsave("eu_exp_combined.png", height=10, width=15.5, unit="cm")


