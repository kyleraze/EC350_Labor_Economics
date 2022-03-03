# Preliminaries -----------------------------------------------------------

library(pacman)
p_load(fixest, tidyverse, broom)

cps_df <- read_rds("cps_data.rds")


# Data wrangling ----------------------------------------------------------

cps_df <- cps_df %>%
  # sample restrictions
  filter(
    AGE %in% c(seq(16, 85, 1)),
    LABFORCE == "Yes, in the labor force",
    AHRSWORKT != "NIU (Not in universe)",
    AHRSWORKT != "Don't Know",
    HOURWAGE != 999.99,
    YEAR == 2019
  ) %>% 
  # clean variables 
  mutate(
    asian = case_when(RACE == "Asian only" &
                        HISPAN == "Not Hispanic" ~ 1, T ~ 0),
    black = case_when(RACE == "Black/Negro" &
                        HISPAN == "Not Hispanic" ~ 1, T ~ 0),
    hispanic = case_when(HISPAN != "Not Hispanic" ~ 1, T ~ 0),
    white = case_when(RACE == "White" &
                        HISPAN == "Not Hispanic" ~ 1, T ~ 0),
    female = case_when(SEX == "Female" ~ 1, T ~ 0),
    educ = case_when(
      EDUC %in% c(
        "None or preschool",
        "Grades 1, 2, 3, or 4",
        "Grades 5 or 6",
        "Grades 7 or 8",
        "Grade 9",
        "Grade 10",
        "Grade 11",
        "12th grade, no diploma"
      ) ~ "Less than high school",
      EDUC == "High school diploma or equivalent" ~ "High school or equivalent",
      EDUC == "Some college but no degree" ~ "Some college",
      EDUC %in% c(
        "Associate's degree, occupational/vocational program",
        "Associate's degree, academic program"
      ) ~ "Associate's degree",
      EDUC == "Bachelor's degree" ~ "Bachelor's degree",
      EDUC %in% c(
        "Master's degree",
        "Professional school degree",
        "Doctorate degree"
      ) ~ "Advanced degree",
      T ~ NA_character_
    ),
    age = as.numeric(AGE),
    occupation = as.factor(OCC),
    industry = as.factor(IND),
    hours_worked = as.numeric(AHRSWORKT)
  ) %>%
  rename(
    sample_weight = EARNWT,
    wages = HOURWAGE,
    person_id = CPSIDP,
    month = MONTH,
    state = STATEFIP
  ) %>% 
  # select variables
  select(
    month,
    person_id,
    state,
    sample_weight,
    age,
    female,
    asian,
    black,
    hispanic,
    white,
    educ,
    occupation,
    industry,
    wages
  ) %>%
  # additional sample restriction
  filter(asian + black + hispanic + white == 1)



feols(
  log(wages) ~ female | sw0(occupation),
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df
) %>% 
  etable()
