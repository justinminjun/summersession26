# 1. Read in training and scoring data

training_data <- readRDS('data/training_data.RDS')
scoring_data <- readRDS('data/scoring_data.RDS')

# 2. Define logistic regression model for failure

fail_model <- glm(fail ~ facility_group +
    days_observed + I(days_observed^2) +
    I(prior_fails/inspections) + new*complaints,
  family=binomial(link='logit'),data=training_data)

# 3. Score the scoring data

scores <- predict(fail_model,newdata=scoring_data,type='response')
scores <- cbind(scoring_data,fail_risk=scores)

# 4. Save the scores and the model

saveRDS(fail_model,'data/model_object.RDS')
saveRDS(scores,'data/model_scores.RDS')
