library(readxl)
library(tidyverse)
library(forcats)

# importing the dataset 
post_bariatric_depression_data <- read_excel(path = "week 3 data.xlsx") 


# After first inspection, Abnormal height values were found 
# Doing further investigation 
post_bariatric_depression_data %>% filter(Height < 150)

# Changing the abnormal values 
post_bariatric_depression_data <- post_bariatric_depression_data %>% mutate(Height = 
                                            ifelse(Height < 100, Height + 100, Height))

# Calculating BMI pre and post operation 
post_bariatric_depression_data <- post_bariatric_depression_data %>% mutate(`BMI PreOp` = round(
  `Weight before surgery`/(Height/100)^2, digits = 1),
  `BMI PostOp` = round(
    `Current weight`/(Height/100)^2, digits = 1
  )) %>% select(-`Diagnosed with psychiatric illness?`, -`If yes, what?`)

# Expanding the comorbidities column into binary indicator variables for each condition
# And computing the total number of comorbidities for each patient
post_bariatric_depression_data <- post_bariatric_depression_data %>% mutate(
  `Other Comorbidities` = as.integer(str_detect(Comorbidities, "others")),
  Hypertension = as.integer(str_detect(Comorbidities, "Hypertension")),
  Diabetes = as.integer(str_detect(Comorbidities, "Diabetes")),
  `High Cholesterol` = as.integer(str_detect(Comorbidities, "high cholesterol")),
  Asthma = as.integer(str_detect(Comorbidities, "asthma")),
  `Heart Disease` = as.integer(str_detect(Comorbidities, "heart disease")),
  `Number of Comorbidities` = as.numeric(`Other Comorbidities` + Hypertension + Diabetes + `High Cholesterol` + Asthma + 
    `Heart Disease`)
  # Reordering variables
) %>% select(-Comorbidities) %>% relocate(Hypertension, Diabetes, `Heart Disease`, Asthma, 
                                          `Other Comorbidities`,
                                          `Number of Comorbidities`, `High Cholesterol`,
                                          .after = `Educational Level`)

# Cleaning type of surgery column and segmenting it based on clinical knowledge to restrictive and malabsorptive surgereis 
post_bariatric_depression_data <- post_bariatric_depression_data %>% mutate(
  Surgery_Type = case_when(
    `Type of surgery` %in% c("Sleeve gastrectomy", "Sleeve gastrectomy, Laparoscopic adjustable gastric banding",
                             "Laparoscopic adjustable gastric banding", "Sleeve gastrectomy, Balloon insertion",
                             "Balloon insertion") ~ "Restrictive",
    TRUE ~ "Malabsorptive or Mixed"
  ) 
) %>% relocate(Surgery_Type, .after = `Type of surgery`)

# Factoring and Reordering time since surgery variable
post_bariatric_depression_data <- post_bariatric_depression_data %>% mutate(
  `Date since operation` = factor(`Date since operation`,levels = c("< 1 year", "1 - 2 years", "> 2 - 3 years",
                                             "> 3 years"))
)

# Factor coding the variables of the PHQ-9
post_bariatric_depression_data <- post_bariatric_depression_data %>% mutate(
  across(c("Little interest or pleasure in doing things", "Feeling down, depressed, or hopeless",
           "Trouble falling or staying asleep, or sleeping too much",
           "Feeling tired or having little energy", "Poor appetite or overeating",
           "Feeling bad about yourself — or that you are a failure or have let yourself or your family down",
           "Trouble concentrating on things, such as reading the newspaper or watching television",
           "Moving or speaking so slowly that other people could have noticed? Or the opposite — being so fidgety or restless that you have been moving .around a lot more than usual",
           "Thoughts that you would be better off dead or of hurting yourself in some way"),
         ~ as.numeric(as.character(fct_recode(.x,
                                 "0" = "Not at all",
                                 "1" = "Several days",
                                 "2" = "More than half days",
                                 "3" = "Nearly everyday")))))

# Computing the PHQ-9 Score
post_bariatric_depression_data <- post_bariatric_depression_data %>% 
  mutate(`Score of PHQ-9` = `Little interest or pleasure in doing things` + 
           `Trouble falling or staying asleep, or sleeping too much` + 
           `Feeling tired or having little energy` + 
           `Feeling bad about yourself — or that you are a failure or have let yourself or your family down` + 
           `Trouble concentrating on things, such as reading the newspaper or watching television` + 
           `Moving or speaking so slowly that other people could have noticed? Or the opposite — being so fidgety or restless that you have been moving .around a lot more than usual` +
           `Thoughts that you would be better off dead or of hurting yourself in some way` +
           `Feeling down, depressed, or hopeless`)

# Categorize PHQ-9 total scores into standard depression severity levels
# based on Kroenke et al. (2001) scoring guidelines, then convert to
# an ordered factor for proper sorting in tables and plots
post_bariatric_depression_data <- post_bariatric_depression_data %>%
  mutate(`Depression Severity` = 
           case_when(
             `Score of PHQ-9` >= 0 & `Score of PHQ-9` <= 4 ~ "None-minimal",
             `Score of PHQ-9` >= 5 & `Score of PHQ-9` <= 9 ~ "Mild",
             `Score of PHQ-9` >= 10 & `Score of PHQ-9` <= 14 ~ "Moderate",
             `Score of PHQ-9` >= 15 & `Score of PHQ-9` <= 19 ~ "Moderately Severe",
             `Score of PHQ-9` >= 20 & `Score of PHQ-9` <= 27 ~ "Severe"
           )) %>% mutate(
             `Depression Severity` = factor(`Depression Severity`, levels = c(
               "None-minimal", "Mild", "Moderate", "Moderately Severe", "Severe"
             ))
           )



# Convert selected categorical columns to factors with title-cased labels
post_bariatric_depression_data <- post_bariatric_depression_data %>% mutate(
  across(c("Gender", "Maraital Status", "Educational Level", "Postsurgical-complications",
           "visitng psychiatric PreOp", "visitng psychiatric PostOp"), ~ factor(str_to_title(.x)))
)

post_bariatric_depression_data <- post_bariatric_depression_data %>%
  mutate(`Educational Level` = fct_recode(`Educational Level`, 
                                          "Masters Or PhD" = "Masters Or Phd"))

# Reclassify "Illitrate" under "High School Or Less"
# to consolidate low-education categories into a single meaningful group
post_bariatric_depression_data <- post_bariatric_depression_data %>% mutate(
  `Educational Level` = case_when(
    `Educational Level` %in% "Illetrate" ~ "High School Or Less", 
    TRUE ~ `Educational Level`
  )
)

# Re-apply factor with the correct ordered levels after recoding
post_bariatric_depression_data <- post_bariatric_depression_data %>% mutate(
  `Educational Level` = factor(`Educational Level`, levels = c("High School Or Less" , "Diploma",
                                                           "Bachelor Degree","Masters Or PhD")))

post_bariatric_depression_data <- post_bariatric_depression_data %>% mutate(`Maraital Status` = 
                                            factor(`Maraital Status`, levels = c(
                                              "Single", "Married", "Divorced", "Widow"
                                            )))
  

# Rename columns to cleaner, more descriptive names for readability
# in output tables and figures
post_bariatric_depression_data <- post_bariatric_depression_data %>% rename("Visiting Psychiatric Pre-Operation" = `visitng psychiatric PreOp`,
                                          "Visiting Psychiatric Post-Operation" = `visitng psychiatric PostOp`,
                                          "Post Surgical Complications" = `Postsurgical-complications`) 

post_bariatric_depression_data <- post_bariatric_depression_data %>% mutate(`Post Surgical Complications` = 
                                            case_when(
                                              str_detect(`Post Surgical Complications`,"no|No") ~ "No",
                                              TRUE ~ `Post Surgical Complications`
                                            ))

# Calculate percent weight loss from pre-surgery weight to current weight
# Formula: ((Weight Before - Current Weight) / Weight Before) * 100
post_bariatric_depression_data <- post_bariatric_depression_data %>% mutate(
  `Percentage of Weight Loss` = ((`Weight before surgery` - `Current weight`)/`Weight before surgery`)*100
)


# Apply the PHQ-9 diagnostic algorithm to flag depressive syndromes.
# Items scored >= 2 indicate symptoms present "more than half the days"
# or "nearly every day."
post_bariatric_depression_data <- post_bariatric_depression_data %>%
  mutate(
    # Count how many items are scored >= 2 (more than half the days or nearly everyday)
    items_high_frequency = rowSums(
      select(., 
             `Little interest or pleasure in doing things`,
             `Feeling down, depressed, or hopeless`,
             `Trouble falling or staying asleep, or sleeping too much`,
             `Feeling tired or having little energy`,
             `Poor appetite or overeating`,
             `Feeling bad about yourself — or that you are a failure or have let yourself or your family down`,
             `Trouble concentrating on things, such as reading the newspaper or watching television`,
             `Moving or speaking so slowly that other people could have noticed? Or the opposite — being so fidgety or restless that you have been moving .around a lot more than usual`,
             `Thoughts that you would be better off dead or of hurting yourself in some way`
      ) >= 2, na.rm = TRUE
    ),
    
    # Check if Item 1 OR Item 2 is >= 2
    core_symptoms_present = (
      `Little interest or pleasure in doing things` >= 2 | 
        `Feeling down, depressed, or hopeless` >= 2
    ),
    
    # MDD is suggested if BOTH conditions are met
    `Major Depressive Syndrome Suggested` = case_when(
      items_high_frequency >= 5 & core_symptoms_present == TRUE ~ "Yes",
      items_high_frequency < 5 | core_symptoms_present == FALSE ~ "No",
      TRUE ~ NA_character_
    ),
    `Other Depressive Syndrome Suggested` = case_when(
      items_high_frequency >= 2 & items_high_frequency <= 4 & core_symptoms_present == T ~ "Yes",
      TRUE ~ "No"
    ),
    `No Depression Suggested` = case_when(
      `Major Depressive Syndrome Suggested` == "No" & `Other Depressive Syndrome Suggested` == "No" ~ "Yes",
      TRUE ~ "No"
    )
  ) 


# Create a binary Depression variable: "Yes" if either depressive syndrome
# is suggested, "No" otherwise — used as the primary outcome variable
post_bariatric_depression_data <- post_bariatric_depression_data %>% mutate(
  Depression = factor(case_when(
    `Major Depressive Syndrome Suggested` == "Yes" | `Other Depressive Syndrome Suggested` == "Yes" ~ "Yes",
    TRUE ~ "No"
  ))
) 



