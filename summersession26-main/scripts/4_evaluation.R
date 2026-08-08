# 1. Read in model scores
scores <- readRDS('data/model_scores.RDS')
scores <- scores[!is.na(scores$community),]

# 2. Rank targets by efficiency and by fairness
scores <- scores[order(-scores$fail_risk),]
scores$rank_efficiency <- 1:nrow(scores)

scores <- scores[order(scores$community,-scores$fail_risk),]

rank_in_community <- ave(
  scores$fail_risk,
  scores$community,
  FUN = seq_along)

scores <- scores[order(rank_in_community,-scores$fail_risk),]
scores$rank_fairness <- 1:nrow(scores)

# 4. Compute lift and fairness metrics for top 730 targets
efficient <- scores[scores$rank_efficiency <= 730,]
fair <- scores[scores$rank_fairness <= 730,]
baseline <- mean(scores$fail_risk)

# 5. Export data for R Shiny app
saveRDS(list(efficient = efficient,
             fair = fair,
             baseline = baseline),file='data/results.RDS')
