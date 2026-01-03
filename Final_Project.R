install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("janitor")
install.packages("caret")
install.packages("randomForest")

library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)
library(janitor)
library(tidyr)
library(scales)
library(caret)
library(randomForest)

# --- 1. Load Data using your specific path ---
df <- read.csv("/Users/aryanpatel/CS/R/IPL.csv", stringsAsFactors = FALSE)

## Basic cleaning
df <- df %>%
  mutate(
    season = as.numeric(season),
    review_decision = trimws(review_decision),
    team_reviewed = trimws(team_reviewed),
    umpire = trimws(umpire),
    umpires_call = trimws(umpires_call)
  )

######### Prateek #############

# -------------------------------
# 1. Filter years 2018–2025
# -------------------------------
df_filtered <- df %>%
  filter(year >= 2018, year <= 2025)

team_reviews_by_year <- df_filtered %>%
  filter(!is.na(team_reviewed)) %>% 
  group_by(year) %>%
  summarise(total_team_reviews = n())

print(team_reviews_by_year)

ggplot(team_reviews_by_year, aes(x = factor(year), y = total_team_reviews)) +
  geom_col() +
  labs(title = "Total Team Reviews per Year (2018–2025)",
       x = "Year",
       y = "Number of Team Reviews") +
  theme_minimal()

reviews <- df %>%
  filter(year >= 2018, year <= 2025,
         !is.na(team_reviewed), 
         !is.na(review_decision)) 

# Fixed syntax error
reviews <- reviews %>%
  mutate(umpire_wrong = review_decision == "upheld")

umpire_accuracy_by_year <- reviews %>%
  group_by(year) %>%
  summarise(
    n_reviews = n(),
    n_wrong = sum(umpire_wrong),
    n_correct = sum(!umpire_wrong),
    pct_wrong = 100 * n_wrong / n_reviews,
    pct_correct = 100 * n_correct / n_reviews
  )

print(umpire_accuracy_by_year)

plot_data <- umpire_accuracy_by_year %>%
  select(year, pct_correct, pct_wrong) %>%
  pivot_longer(cols = c(pct_correct, pct_wrong),
               names_to = "decision_type",
               values_to = "percentage")

ggplot(plot_data, aes(x = factor(year),
                      y = percentage,
                      fill = decision_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(
    "pct_correct" = "blue",
    "pct_wrong" = "red"
  ),
  labels = c(
    "pct_correct" = "Umpire Correct",
    "pct_wrong" = "Umpire Wrong"
  )) +
  labs(
    title = "Umpire Accuracy by Year (Correct vs Wrong)",
    x = "Year",
    y = "Percentage (%)",
    fill = "Decision Outcome"
  ) +
  theme_minimal()

prop.table(table(reviews$review_decision))


########## Hanumath #########

reviews_clean <- reviews %>%
  mutate(
    decision_lower = tolower(trimws(review_decision)),
    review_outcome = case_when(
      decision_lower %in% c("upheld", "umpire's call") ~ "Upheld",
      decision_lower == "struck down" ~ "Overturned",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(review_outcome))


# ---- Clean & filter ----
df_clean <- df %>%
  mutate(
    year = as.numeric(year),
    review_decision = trimws(tolower(review_decision))
  ) %>%
  filter(
    year >= 2018,
    !is.na(review_decision),
    review_decision != ""
  )

# ---- Define umpire correctness ----

df_clean <- df_clean %>%
  mutate(
    umpire_result = case_when(
      review_decision == "upheld" ~ "Umpire Wrong",
      review_decision == "struck down" ~ "Umpire Correct",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(umpire_result))

# ---- Create time periods ----
df_clean <- df_clean %>%
  mutate(
    period = ifelse(year <= 2021, "2018–2021", "2022–2025")
  )

# ---- Percentage calculation ----
plot_data <- df_clean %>%
  group_by(period, umpire_result) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(period) %>%
  mutate(percentage = n / sum(n))

# ---- Bar graph ----
ggplot(plot_data,
       aes(x = period,
           y = percentage,
           fill = umpire_result)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c(
      "Umpire Correct" = "blue",
      "Umpire Wrong" = "red"
    )
  ) +
  labs(
    title = "Overall Distribution of Review Outcomes (Before and after 2021)",
    x = "Season Group",
    y = "Percentage of Decisions",
    fill = "Decision Outcome"
  ) +
  theme_minimal()

########## Aryan #############

# -------------------------------
# Two-Proportion Z-Test
# Goal: Compare the proportion of "Umpire Correct" (Overturned) decisions
#       between two time periods: 2018-2021 vs 2022-2025.
# H0: p1 = p2 (No difference in accuracy/overturn rate between periods)
# H1: p1 != p2 (There is a significant difference)
# -------------------------------

# 1. Create a table of counts using Hanumath's cleaned data (df_clean)
# We use 'umpire_result' because Hanumath already cleaned it into "Umpire Correct"/"Umpire Wrong"
counts_table <- table(df_clean$period, df_clean$umpire_result)

print("--- Counts Table (Period vs Result) ---")
print(counts_table)

# 2. Extract values for prop.test
# We want the count of "Umpire Correct" (successes) and the total reviews for each period
successes <- counts_table[, "Umpire Correct"]
totals <- rowSums(counts_table)

print("--- Successes (Correct) vs Totals ---")
print(successes)
print(totals)

# 3. Run Two-Proportion Z-Test
prop_test_result <- prop.test(successes, totals)

print("--- Two-Proportion Z-Test Results ---")
print(prop_test_result)


########## Yash #############

########## 4 — Prediction Model #############

model_data <- reviews_clean %>%
  mutate(
    target = ifelse(review_outcome == "Overturned", 1, 0),
    umpires_call_flag = ifelse(tolower(umpires_call) == "yes", 1, 0),
    season = as.numeric(season)
  ) %>%
  select(target, umpire, team_reviewed, umpires_call_flag, season)

model_data <- na.omit(model_data)

model_data$umpire <- as.factor(model_data$umpire)
model_data$team_reviewed <- as.factor(model_data$team_reviewed)

set.seed(42)
train_index <- createDataPartition(model_data$target, p = 0.8, list = FALSE)
train <- model_data[train_index, ]
test <- model_data[-train_index, ]

logit_model <- glm(
  target ~ umpire + team_reviewed + umpires_call_flag + season,
  data = train,
  family = binomial
)

summary(logit_model)

test$pred_prob <- predict(logit_model, newdata = test, type = "response")
test$pred_class <- ifelse(test$pred_prob > 0.5, 1, 0)

confusionMatrix(
  factor(test$pred_class),
  factor(test$target),
  positive = "1"
)

varImp(logit_model)

rf_model <- randomForest(
  factor(target) ~ umpire + team_reviewed + umpires_call_flag + season,
  data = train,
  ntree = 300,
  importance = TRUE
)

print(rf_model)
varImpPlot(rf_model)