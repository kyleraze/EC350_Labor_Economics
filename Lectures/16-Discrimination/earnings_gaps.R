# Preliminaries -----------------------------------------------------------

library(pacman)
p_load(fixest, tidyverse, broom)

cps_df <- read_rds("cps_data.rds")


# Data wrangling ----------------------------------------------------------

cps_df <- cps_df %>%
  # sample restrictions
  filter(AGE %in% c(seq(16, 85, 1)),
         LABFORCE == "Yes, in the labor force",
         AHRSWORKT != "NIU (Not in universe)",
         AHRSWORKT != "Don't Know",
         # EARNWEEK != 9999.99) %>% 
         HOURWAGE != 999.99) %>% 
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
    sample_weight = EARNWT / 12,
    hours_worked = as.numeric(AHRSWORKT)
  ) %>%
  rename(
    wage = HOURWAGE,
    earnings = EARNWEEK,
    person_id = CPSIDP,
    year = YEAR,
    month = MONTH,
    state = STATEFIP
  ) %>% 
  # select variables
  select(
    year,
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
    wage,
    earnings,
    hours_worked
  ) %>%
  # additional sample restriction
  filter(asian + black + hispanic + white == 1)

# cps_df %>% group_by(year) %>% summarize(n = n())


# Race regressions --------------------------------------------------------

# men 
race_m1 <- feols(
  log(wage) ~ asian + black + hispanic,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, female == 0)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("asian", "black", "hispanic")) %>% 
  mutate(controls = "None")

race_m2 <- feols(
  log(wage) ~ asian + black + hispanic + educ,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, female == 0)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("asian", "black", "hispanic")) %>% 
  mutate(controls = "educ")

race_m3 <- feols(
  log(wage) ~ asian + black + hispanic + educ * age,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, female == 0)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("asian", "black", "hispanic")) %>% 
  mutate(controls = "educ + exper")

race_m4 <- feols(
  log(wage) ~ asian + black + hispanic + educ * age |
    industry,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, female == 0)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("asian", "black", "hispanic")) %>% 
  mutate(controls = "educ + exper + industry")

race_m5 <- feols(
  log(wage) ~ asian + black + hispanic + educ * age |
    industry + occupation,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, female == 0)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("asian", "black", "hispanic")) %>% 
  mutate(controls = "educ + exper + industry + occupation")

race_m6 <- feols(
  log(wage) ~ asian + black + hispanic + educ * age |
    industry + occupation + state,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, female == 0)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("asian", "black", "hispanic")) %>% 
  mutate(controls = "educ + exper + industry + occupation + state")

race_gaps_men <- bind_rows(race_m1, race_m2, race_m3, race_m4, race_m5, race_m6) %>% 
  mutate(gender = "Men")

rm(race_m1, race_m2, race_m3, race_m4, race_m5, race_m6)

# women 
race_w1 <- feols(
  log(wage) ~ asian + black + hispanic,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, female == 1)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("asian", "black", "hispanic")) %>% 
  mutate(controls = "None")

race_w2 <- feols(
  log(wage) ~ asian + black + hispanic + educ,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, female == 1)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("asian", "black", "hispanic")) %>% 
  mutate(controls = "educ")

race_w3 <- feols(
  log(wage) ~ asian + black + hispanic + educ * age,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, female == 1)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("asian", "black", "hispanic")) %>% 
  mutate(controls = "educ + exper")

race_w4 <- feols(
  log(wage) ~ asian + black + hispanic + educ * age |
    industry,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, female == 1)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("asian", "black", "hispanic")) %>% 
  mutate(controls = "educ + exper + industry")

race_w5 <- feols(
  log(wage) ~ asian + black + hispanic + educ * age |
    industry + occupation,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, female == 1)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("asian", "black", "hispanic")) %>% 
  mutate(controls = "educ + exper + industry + occupation")

race_w6 <- feols(
  log(wage) ~ asian + black + hispanic + educ * age |
    industry + occupation + state,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, female == 1)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("asian", "black", "hispanic")) %>% 
  mutate(controls = "educ + exper + industry + occupation + state")

race_gaps_women <- bind_rows(race_w1, race_w2, race_w3, race_w4, race_w5, race_w6) %>% 
  mutate(gender = "Women")

rm(race_w1, race_w2, race_w3, race_w4, race_w5, race_w6)

race_gaps_2021 <- bind_rows(race_gaps_women, race_gaps_men) %>% 
  mutate(estimate = round(estimate, 3),
         std.error = round(std.error, 3))

rm(race_gaps_women, race_gaps_men)

saveRDS(race_gaps_2021, "race_wage_gap_2021.rds")

unadjusted_race_gaps <-
  pmap_dfr(
    expand_grid(year = c(seq(2010, 2021)), gender = c(0, 1)),
    ~ feols(
      log(wage) ~ asian + black + hispanic,
      cluster = ~ person_id,
      weights = ~ sample_weight,
      data = cps_df %>% filter(year == ..1, female == ..2)
    ) %>% 
      tidy(conf.int = T) %>%
      filter(term %in% c("asian", "black", "hispanic")) %>%
      mutate(analysis = "Unadjusted",
             year = ..1,
             female = ..2)
  )

adjusted_race_gaps <-
  pmap_dfr(
    expand_grid(year = c(seq(2010, 2021)), gender = c(0, 1)),
    ~ feols(
      log(wage) ~ asian + black + hispanic + educ * age |
        industry + occupation + state,
      cluster = ~ person_id,
      weights = ~ sample_weight,
      data = cps_df %>% filter(year == ..1, female == ..2)
    ) %>% 
      tidy(conf.int = T) %>%
      filter(term %in% c("asian", "black", "hispanic")) %>%
      mutate(analysis = "Adjusted",
             year = ..1,
             female = ..2)
  )

annual_race_gaps <- bind_rows(unadjusted_race_gaps, adjusted_race_gaps)

rm(unadjusted_race_gaps, adjusted_race_gaps)

saveRDS(annual_race_gaps, "annual_race_wage_gaps.rds")


# Gender regressions ------------------------------------------------------

cps_df <- cps_df %>% 
  mutate(race = case_when(white == 1 ~ "White", black == 1 ~ "Black", asian == 1 ~ "Asian", hispanic == 1 ~ "Hispanic"))

# white 
gender_w1 <- feols(
  log(wage) ~ female,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, white == 1)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("female")) %>% 
  mutate(controls = "None")

gender_w2 <- feols(
  log(wage) ~ female + educ,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, white == 1)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("female")) %>% 
  mutate(controls = "educ")

gender_w3 <- feols(
  log(wage) ~ female + educ * age,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, white == 1)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("female")) %>% 
  mutate(controls = "educ + exper")

gender_w4 <- feols(
  log(wage) ~ female + educ * age |
    industry,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, white == 1)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("female")) %>% 
  mutate(controls = "educ + exper + industry")

gender_w5 <- feols(
  log(wage) ~ female + educ * age |
    industry + occupation,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, white == 1)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("female")) %>% 
  mutate(controls = "educ + exper + industry + occupation")

gender_w6 <- feols(
  log(wage) ~ female + educ * age |
    industry + occupation + state,
  cluster = ~ person_id,
  weights = ~ sample_weight,
  data = cps_df %>% filter(year == 2021, white == 1)
) %>% 
  tidy(conf.int = T) %>% 
  filter(term %in% c("female")) %>% 
  mutate(controls = "educ + exper + industry + occupation + state")

gender_gap_white <- bind_rows(gender_w1, gender_w2, gender_w3, gender_w4, gender_w5, gender_w6) %>% 
  mutate(race = "White", 
         estimate = round(estimate, 3),
         std.error = round(std.error, 3))

rm(gender_w1, gender_w2, gender_w3, gender_w4, gender_w5, gender_w6)

saveRDS(gender_gap_white, "gender_wage_gap_2021.rds")

unadjusted_gender_gaps <-
  pmap_dfr(
    expand_grid(year = c(seq(2010, 2021)), race = c("Black", "White", "Hispanic", "Asian")),
    ~ feols(
      log(wage) ~ female,
      cluster = ~ person_id,
      weights = ~ sample_weight,
      data = cps_df %>% filter(year == ..1, race == ..2)
    ) %>% 
      tidy(conf.int = T) %>%
      filter(term %in% c("female")) %>%
      mutate(analysis = "Unadjusted",
             year = ..1,
             race = ..2)
  )

adjusted_gender_gaps <-
  pmap_dfr(
    expand_grid(year = c(seq(2010, 2021)), race = c("Black", "White", "Hispanic", "Asian")),
    ~ feols(
      log(wage) ~ female + educ * age |
        industry + occupation + state,
      cluster = ~ person_id,
      weights = ~ sample_weight,
      data = cps_df %>% filter(year == ..1, race == ..2)
    ) %>% 
      tidy(conf.int = T) %>%
      filter(term %in% c("female")) %>%
      mutate(analysis = "Adjusted",
             year = ..1,
             race = ..2)
  )

annual_gender_gaps <- bind_rows(unadjusted_gender_gaps, adjusted_gender_gaps)

rm(unadjusted_gender_gaps, adjusted_gender_gaps)

saveRDS(annual_gender_gaps, "annual_gender_wage_gaps.rds")


# Sample sizes ------------------------------------------------------------

sample_sizes <- cps_df %>% 
  group_by(race, female, year) %>% 
  summarize(n = n()) %>% 
  ungroup()

saveRDS(sample_sizes, "sample_sizes.rds")


