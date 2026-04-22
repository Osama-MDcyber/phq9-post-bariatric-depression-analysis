
library(gtsummary)
library(broom.helpers)
library(patchwork)
library(rstatix)

source(file = "Data Upload and CLeaning.R")

# Summarize pre-operative and demographic variables for Table 1.
# Outputs a formatted flex table suitable for Word/HTML reports.
post_bariatric_depression_data %>% 
  select(Age, Gender, `Maraital Status`, `BMI PreOp`,`Educational Level`,
         Hypertension, Diabetes, `Heart Disease`, Asthma,
        `High Cholesterol`, `Other Comorbidities`,
        Surgery_Type, `Date since operation`, `Post Surgical Complications`,
        `Visiting Psychiatric Pre-Operation`) %>% 
  tbl_summary() %>% bold_labels() %>% as_flex_table()

# Convert raw numeric PHQ-9 item scores to labeled ordered factors to display it in the table
post_bariatric_depression_data <- post_bariatric_depression_data %>% mutate(
  across(c("Little interest or pleasure in doing things", "Feeling down, depressed, or hopeless",
           "Trouble falling or staying asleep, or sleeping too much",
           "Feeling tired or having little energy", "Poor appetite or overeating",
           "Feeling bad about yourself — or that you are a failure or have let yourself or your family down",
           "Trouble concentrating on things, such as reading the newspaper or watching television",
           "Moving or speaking so slowly that other people could have noticed? Or the opposite — being so fidgety or restless that you have been moving .around a lot more than usual",
           "Thoughts that you would be better off dead or of hurting yourself in some way"),
         ~ factor(as.character(.x), 
                  levels = c("0", "1", "2", "3"),
                  labels = c("Not at all", "Several days", 
                             "More than half days", "Nearly everyday"))))

# Summarize the distribution of responses for each PHQ-9 item
# along with the total PHQ-9 score; outputs a formatted flex table.
post_bariatric_depression_data %>% select(
  `Little interest or pleasure in doing things`,
  `Feeling down, depressed, or hopeless`, 
  `Trouble falling or staying asleep, or sleeping too much`, 
  `Feeling tired or having little energy`, 
  `Poor appetite or overeating`, 
  `Feeling bad about yourself — or that you are a failure or have let yourself or your family down`,
  `Trouble concentrating on things, such as reading the newspaper or watching television`,
  `Moving or speaking so slowly that other people could have noticed? Or the opposite — being so fidgety or restless that you have been moving .around a lot more than usual`,
  `Thoughts that you would be better off dead or of hurting yourself in some way`,
  `Score of PHQ-9`
) %>% tbl_summary() %>% bold_labels() %>% as_flex_table()


# Run separate univariate binary logistic regressions for each predictor
# against the Depression outcome. Results are exponentiated to give
# crude odds ratios (OR) with 95% CIs.
univariate_regressoin <- post_bariatric_depression_data %>% select(
  Age, Gender, `Maraital Status`, `Educational Level`, Hypertension, Diabetes, `Heart Disease`,
  Asthma, `Other Comorbidities`, `High Cholesterol`, `BMI PreOp`, Surgery_Type, `Date since operation`, `Post Surgical Complications`,
  Depression, `Post Surgical Complications`, `Percentage of Weight Loss`
) %>% tbl_uvregression(label = list(
  Surgery_Type ~ "Type of Surgery"
),
method = glm,
  y = Depression, 
  method.args = list(family = binomial),
  exponentiate = T, hide_n = T
) 

# Fit a full multivariable binary logistic regression model including all
# candidate predictors. This produces adjusted odds ratios (aOR).
multivariable_model <- glm(Depression ~ Age + Gender + `Maraital Status`+ 
                             `Educational Level`+ Hypertension+ Diabetes+ 
                             `Heart Disease`+ Asthma + `Other Comorbidities` + 
                             `High Cholesterol`+ `BMI PreOp`+ Surgery_Type + 
                             `Date since operation`+ `Post Surgical Complications`+ `Post Surgical Complications`+ `Percentage of Weight Loss`,
                           data = post_bariatric_depression_data, 
    family = binomial)

# Format the multivariable model output as a gtsummary table.
# p-values <= 0.05 are bolded for quick identification of significant predictors.
multivariate_regression <- tbl_regression(multivariable_model, 
               exponentiate = T) %>% bold_p(t = 0.05)

# Side-by-side display of crude and adjusted ORs for all predictors.
# tab_spanner labels clearly distinguish the two models.
tbl_merge(
  tbls = list(univariate_regressoin, multivariate_regression),
  tab_spanner = c("**Unadjusted**", "**Adjusted**")
) %>% bold_labels() %>% as_flex_table()


# Quick exploratory bar chart of depression severity distribution.
# Percentages are calculated from counts and displayed above each bar.
post_bariatric_depression_data %>%
  count(`Depression Severity`) %>%
  mutate(
    Percentage = (n / sum(n)) * 100,
    `Depression Severity` = fct_reorder(`Depression Severity`, Percentage, .desc = TRUE)
  ) %>%
  ggplot(aes(x = `Depression Severity`, y = Percentage, fill = `Depression Severity`)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(
    aes(label = paste0(round(Percentage, 1), "%")),
    vjust = -0.5,
    fontface = "bold",
    size = 4
  ) +
  scale_fill_brewer(palette = "Reds", direction = 1) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(
    title = "Distribution of Depression Severity",
    subtitle = "Post-Bariatric Surgery Patients",
    x = "",
    y = "Percentage (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(face = "bold", size = 11, angle = 15)
  )

