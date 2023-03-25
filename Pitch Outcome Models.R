#General format
#
# ratios <- dataframe grouping by model parameters with outcome_ratio representing the 
#           proportion of pitches with that outcome
#
# model_table_outcome <- filtered version of ratios that eliminates small sample matchups
#
# ggplot showing relationship between hitter and pitcher outcome percentages and 
#  the outcome percentage of pitches between them (commented out)
#
# set.seed for replicable results
# sample, train, test splits data into training and testing sets
#
# outcome_model <- wls model to predict the likelihood of the outcome 
#
# outcome_predictions <- predicts likelihood of outcome on testing set (commented out)
#
# plot of predictions vs true results (commented out)
#
# model summary (commented out)
#
# mse is mean squared error (commented out)
#
#
#
## All model summaries are available at the bottom of the file


# Swinging Strikes Model ---------

ratios <- total_pitches %>%
  dplyr::select(batter, pitcher, description, p_throws, stand, balls, strikes) %>%
  unite("count", balls:strikes, remove = TRUE) %>%
  group_by(batter) %>%
  mutate(hitter_ss_pct = sum(description == "swinging_strike")/length(batter)) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pitcher_ss_pct = sum(description == "swinging_strike")/length(pitcher)) %>%
  ungroup() %>%
  mutate(hitter_ss_pct = round(hitter_ss_pct, 2),
         pitcher_ss_pct = round(pitcher_ss_pct, 2),
         platoon = case_when(p_throws == stand ~ 0,
                             TRUE ~ 1)) %>%
  unite("handedness", p_throws:stand, remove = TRUE) %>%
  mutate(swinging_strike = case_when(description == "swinging_strike" ~ 1,
                                     TRUE ~ 0)) %>%
  group_by(handedness, hitter_ss_pct, pitcher_ss_pct, count, platoon) %>%
  summarise(ss_ratio = mean(swinging_strike), num_instances = n())

model_table_ss <- ratios %>%
  filter(num_instances >= 300)

# ggplot(model_table_ss, aes(x = hitter_ss_pct, y = pitcher_ss_pct, fill = ss_ratio)) +
#   geom_tile()

set.seed(151) 
sample = sample.split(model_table_ss$ss_ratio, SplitRatio = .8)
train = subset(model_table_ss, sample == TRUE)
test  = subset(model_table_ss, sample == FALSE)

ss_model <- lm(I(ss_ratio^0.5) ~ handedness + count + I(hitter_ss_pct^0.5) + I(pitcher_ss_pct^0.5), 
               data = train,
               weights = num_instances)

### Uncomment below to verify results


# ss_predictions <- predict(ss_model, newdata = test)
# 
# plot(ss_predictions^2, test$ss_ratio)
# 
# summary(ss_model)
# 
# mse = mean((ss_predictions^2 - test$ss_ratio)^2)
# print(mse)

# Called Strikes Model ----------

ratios <- total_pitches %>%
  dplyr::select(batter, pitcher, description, p_throws, stand, balls, strikes) %>%
  unite("count", balls:strikes, remove = TRUE) %>%
  dplyr::group_by(batter) %>%
  mutate(hitter_cs_pct = sum(description == "called_strike")/length(batter)) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pitcher_cs_pct = sum(description == "called_strike")/length(pitcher)) %>%
  ungroup() %>%
  mutate(hitter_cs_pct = round(hitter_cs_pct, 2),
         pitcher_cs_pct = round(pitcher_cs_pct, 2),
         platoon = case_when(p_throws == stand ~ 0,
                             TRUE ~ 1)) %>%
  unite("handedness", p_throws:stand, remove = TRUE) %>%
  mutate(called_strike = case_when(description == "called_strike" ~ 1,
                                   TRUE ~ 0)) %>%
  group_by(hitter_cs_pct, pitcher_cs_pct, count, platoon) %>%
  dplyr::summarise(cs_ratio = mean(called_strike), num_instances = n())

model_table_cs <- ratios %>%
  filter(num_instances >= 400)

# ggplot(model_table_cs, aes(x = hitter_cs_pct, y = pitcher_cs_pct, fill = cs_ratio)) +
#   geom_tile()

set.seed(1051) 
sample = sample.split(model_table_cs$cs_ratio, SplitRatio = .8)
train = subset(model_table_cs, sample == TRUE)
test  = subset(model_table_cs, sample == FALSE)

cs_model <- lm(cs_ratio ~ count + platoon + hitter_cs_pct + pitcher_cs_pct, 
               data = train,
               weights = num_instances)

### Uncomment below to verify results

# cs_predictions <- predict(cs_model, newdata = test)
# 
# plot(cs_predictions, test$cs_ratio)
# summary(cs_model)
# 
# 
# mse = mean((cs_predictions - test$cs_ratio)^2)
# print(mse)

# Balls Model -----------

ratios <- total_pitches %>%
  dplyr::select(batter, pitcher, description, p_throws, stand, balls, strikes) %>%
  unite("count", balls:strikes, remove = TRUE) %>%
  dplyr::group_by(batter) %>%
  mutate(hitter_b_pct = sum(description == "ball")/length(batter)) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pitcher_b_pct = sum(description == "ball")/length(pitcher)) %>%
  ungroup() %>%
  mutate(hitter_b_pct = round(hitter_b_pct, 2),
         pitcher_b_pct = round(pitcher_b_pct, 2),
         platoon = case_when(p_throws == stand ~ 0,
                             TRUE ~ 1)) %>%
  unite("handedness", p_throws:stand, remove = TRUE) %>%
  mutate(ball = case_when(description == "ball" ~ 1,
                          TRUE ~ 0)) %>%
  group_by(hitter_b_pct, pitcher_b_pct, count, platoon) %>%
  dplyr::summarise(b_ratio = mean(ball), num_instances = n())

model_table_b <- ratios %>%
  filter(num_instances >= 500)

# ggplot(model_table_b, aes(x = hitter_b_pct, y = pitcher_b_pct, fill = b_ratio)) +
#   geom_tile()

set.seed(1051) 
sample = sample.split(model_table_b$b_ratio, SplitRatio = .8)
train = subset(model_table_b, sample == TRUE)
test  = subset(model_table_b, sample == FALSE)

b_model <- lm(b_ratio ~ count + platoon + hitter_b_pct + pitcher_b_pct, 
              data = train,
              weights = num_instances)

### Uncomment below to verify results

# b_predictions <- predict(b_model, newdata = test)
# 
# plot(b_predictions, test$b_ratio)
# summary(b_model)
# 
# mse = mean((b_predictions - test$b_ratio)^2)
# print(mse)

# Foul Model ---------

ratios <- total_pitches %>%
  dplyr::select(batter, pitcher, description, p_throws, stand, balls, strikes) %>%
  unite("count", balls:strikes, remove = TRUE) %>%
  dplyr::group_by(batter) %>%
  mutate(hitter_foul_pct = sum(description == "foul")/length(batter)) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pitcher_foul_pct = sum(description == "foul")/length(pitcher)) %>%
  ungroup() %>%
  mutate(hitter_foul_pct = round(hitter_foul_pct, 2),
         pitcher_foul_pct = round(pitcher_foul_pct, 2),
         platoon = case_when(p_throws == stand ~ 0,
                             TRUE ~ 1)) %>%
  unite("handedness", p_throws:stand, remove = TRUE) %>%
  mutate(foul = case_when(description == "foul" ~ 1,
                          TRUE ~ 0)) %>%
  group_by(handedness, hitter_foul_pct, pitcher_foul_pct, count, platoon) %>%
  dplyr::summarise(foul_ratio = mean(foul), num_instances = n())

model_table_foul <- ratios %>%
  filter(num_instances >= 400)

# ggplot(model_table_foul, aes(x = hitter_foul_pct, y = pitcher_foul_pct, fill = foul_ratio)) +
#   geom_tile()

set.seed(1051) 
sample = sample.split(model_table_foul$foul_ratio, SplitRatio = .8)
train = subset(model_table_foul, sample == TRUE)
test  = subset(model_table_foul, sample == FALSE)

foul_model <- lm(foul_ratio ~ count + handedness + hitter_foul_pct+pitcher_foul_pct, 
                 data = train,
                 weights = num_instances)

### Uncomment below to verify results

# foul_predictions <- predict(foul_model, newdata = test)
# 
# plot(foul_predictions, test$foul_ratio)
# summary(foul_model)
# 
# mse = mean((foul_predictions - test$foul_ratio)^2)
# print(mse)

# Hit Into Play Model ---------

ratios <- total_pitches %>%
  dplyr::select(batter, pitcher, description, p_throws, stand, balls, strikes) %>%
  unite("count", balls:strikes, remove = TRUE) %>%
  dplyr::group_by(batter) %>%
  mutate(hitter_hit_pct = sum(description == "hit_into_play")/length(batter)) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pitcher_hit_pct = sum(description == "hit_into_play")/length(pitcher)) %>%
  ungroup() %>%
  mutate(hitter_hit_pct = round(hitter_hit_pct, 2),
         pitcher_hit_pct = round(pitcher_hit_pct, 2),
         platoon = case_when(p_throws == stand ~ 0,
                             TRUE ~ 1)) %>%
  unite("handedness", p_throws:stand, remove = TRUE) %>%
  mutate(hit_into_play = case_when(description == "hit_into_play" ~ 1,
                                   TRUE ~ 0)) %>%
  group_by(hitter_hit_pct, pitcher_hit_pct, count, platoon) %>%
  dplyr::summarise(hit_ratio = mean(hit_into_play), num_instances = n())

model_table_hit <- ratios %>%
  filter(num_instances >= 500)

# ggplot(model_table_hit, aes(x = hitter_hit_pct, y = pitcher_hit_pct, fill = hit_ratio)) +
#   geom_tile()

set.seed(101) 
sample = sample.split(model_table_hit$hit_ratio, SplitRatio = .8)
train = subset(model_table_hit, sample == TRUE)
test  = subset(model_table_hit, sample == FALSE)

hit_model <- lm(hit_ratio ~ count + platoon + hitter_hit_pct+pitcher_hit_pct, 
                data = train,
                weights = num_instances)

### Uncomment below to verify results

# hit_predictions <- predict(hit_model, newdata = test)
# 
# plot(hit_predictions, test$hit_ratio)
# summary(hit_model)
# 
# mse = mean((hit_predictions - test$hit_ratio)^2)
# print(mse)

# Hit into play Data Processing ----------

# Since not all ball in play outcomes are given directly in the data, 
#  we infered certain outcomes from the play description

bip_pitches <- total_pitches %>%
  filter(
    description == "hit_into_play",
    events != "catcher_interf",
    events != "field_error",
    grepl("batter interference", des) == FALSE
  ) %>%
  mutate(
    outcome = case_when(
      events == "single" ~ "single",
      events == "double" ~ "double",
      events == "triple" ~ "triple",
      events == "home_run" ~ "home_run",
      events == "double_play" ~ "ground_out",
      events == "triple_play" ~ "ground_out",
      events == "grounded_into_double_play" ~ "ground_out",
      events == "force_out" ~ "ground_out",
      events == "sac_fly" ~ "fly_out",
      events == "fielders_choice" ~ "ground_out",
      events == "fielders_choice_out" ~ "ground_out",
      events == "sac_fly_double_play" ~ "fly_out",
      events == "sac_bunt" ~ "ground_out",
      events == "sac_bunt_double_play" ~ "ground_out",
      grepl("grounds", des) == TRUE ~ "ground_out",
      grepl("lines", des) == TRUE ~ "line_out",
      grepl("pops", des) == TRUE ~ "pop_out",
      grepl("flies", des) == TRUE ~ "fly_out",
      TRUE ~ ""
    )
  ) 

# Singles Model ---------

ratios <- bip_pitches %>%
  dplyr::select(batter, pitcher, outcome, p_throws, stand, balls, strikes) %>%
  unite("count", balls:strikes, remove = TRUE) %>%
  dplyr::group_by(batter) %>%
  mutate(hitter_s1_pct = sum(outcome == "single")/length(batter)) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pitcher_s1_pct = sum(outcome == "single")/length(pitcher)) %>%
  ungroup() %>%
  mutate(hitter_s1_pct = round(hitter_s1_pct, 2),
         pitcher_s1_pct = round(pitcher_s1_pct, 2),
         platoon = case_when(p_throws == stand ~ 0,
                             TRUE ~ 1)) %>%
  unite("handedness", p_throws:stand, remove = TRUE) %>%
  mutate(single = case_when(outcome == "single" ~ 1,
                            TRUE ~ 0)) %>%
  group_by(hitter_s1_pct, pitcher_s1_pct, platoon) %>%
  dplyr::summarise(s1_ratio = mean(single), num_instances = n())

model_table_s1 <- ratios %>%
  filter(num_instances >= 300)

# ggplot(model_table_s1, aes(x = hitter_s1_pct, y = pitcher_s1_pct, fill = s1_ratio)) +
#   geom_tile()

set.seed(1561) 
sample = sample.split(model_table_s1$s1_ratio, SplitRatio = .8)
train = subset(model_table_s1, sample == TRUE)
test  = subset(model_table_s1, sample == FALSE)

s1_model <- lm(s1_ratio ~ platoon + hitter_s1_pct, 
               data = train,
               weights = num_instances)

### Uncomment below to verify results

# s1_predictions <- predict(s1_model, newdata = test)
# 
# plot(s1_predictions, test$s1_ratio)
# summary(s1_model)
# 
# mse = mean((s1_predictions - test$s1_ratio)^2)
# print(mse)

# Home Run Model ---------

ratios <- bip_pitches %>%
  dplyr::select(batter, pitcher, outcome, p_throws, stand, balls, strikes) %>%
  unite("count", balls:strikes, remove = TRUE) %>%
  dplyr::group_by(batter) %>%
  mutate(hitter_hr_pct = sum(outcome == "home_run")/length(batter)) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pitcher_hr_pct = sum(outcome == "home_run")/length(pitcher)) %>%
  ungroup() %>%
  mutate(hitter_hr_pct = round(hitter_hr_pct, 2),
         pitcher_hr_pct = round(pitcher_hr_pct, 2),
         platoon = case_when(p_throws == stand ~ 0,
                             TRUE ~ 1)) %>%
  unite("handedness", p_throws:stand, remove = TRUE) %>%
  mutate(home_run = case_when(outcome == "home_run" ~ 1,
                              TRUE ~ 0)) %>%
  group_by(hitter_hr_pct, platoon) %>%
  dplyr::summarise(hr_ratio = mean(home_run), num_instances = n())

model_table_hr <- ratios %>%
  filter(num_instances >= 300 & hitter_hr_pct > 0 & pitcher_hr_pct > 0 & hr_ratio > 0)

# ggplot(model_table_hr, aes(x = hitter_hr_pct, y = pitcher_hr_pct, fill = hr_ratio)) +
#   geom_tile()

set.seed(101) 
sample = sample.split(model_table_hr$hr_ratio, SplitRatio = .8)
train = subset(model_table_hr, sample == TRUE)
test  = subset(model_table_hr, sample == FALSE)

hr_model <- lm(hr_ratio ~ platoon + hitter_hr_pct, 
               data = train,
               weights = num_instances)

### Uncomment below to verify results

# hr_predictions <- predict(hr_model, newdata = test)
# 
# summary(hr_model)
# 
# mse = mean((hr_predictions - test$hr_ratio)^2)
# print(mse)


# Ground Out Model ---------

ratios <- bip_pitches %>%
  dplyr::select(batter, pitcher, outcome, p_throws, stand, balls, strikes) %>%
  unite("count", balls:strikes, remove = TRUE) %>%
  dplyr::group_by(batter) %>%
  mutate(hitter_gro_pct = sum(outcome == "ground_out")/length(batter)) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pitcher_gro_pct = sum(outcome == "ground_out")/length(pitcher)) %>%
  ungroup() %>%
  mutate(hitter_gro_pct = round(hitter_gro_pct, 2),
         pitcher_gro_pct = round(pitcher_gro_pct, 2),
         platoon = case_when(p_throws == stand ~ 0,
                             TRUE ~ 1)) %>%
  unite("handedness", p_throws:stand, remove = TRUE) %>%
  mutate(ground_out = case_when(outcome == "ground_out" ~ 1,
                                TRUE ~ 0)) %>%
  group_by(hitter_gro_pct, pitcher_gro_pct, platoon) %>%
  dplyr::summarise(gro_ratio = mean(ground_out), num_instances = n())

model_table_gro <- ratios %>%
  filter(num_instances >= 320)

# ggplot(model_table_gro, aes(x = hitter_gro_pct, y = pitcher_gro_pct, fill = gro_ratio)) +
#   geom_tile()

set.seed(11) 
sample = sample.split(model_table_gro$gro_ratio, SplitRatio = .8)
train = subset(model_table_gro, sample == TRUE)
test  = subset(model_table_gro, sample == FALSE)

gro_model <- lm(gro_ratio ~ platoon + hitter_gro_pct + pitcher_gro_pct, 
                data = train,
                weights = num_instances)

### Uncomment below to verify results

# gro_predictions <- predict(gro_model, newdata = test)
# 
# summary(gro_model)
# 
# mse = mean((gro_predictions - test$gro_ratio)^2)
# print(mse)


# Fly Out Model ---------

ratios <- bip_pitches %>%
  dplyr::select(batter, pitcher, outcome, p_throws, stand, balls, strikes) %>%
  unite("count", balls:strikes, remove = TRUE) %>%
  dplyr::group_by(batter) %>%
  mutate(hitter_flo_pct = sum(outcome == "fly_out")/length(batter)) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pitcher_flo_pct = sum(outcome == "fly_out")/length(pitcher)) %>%
  ungroup() %>%
  mutate(hitter_flo_pct = round(hitter_flo_pct, 2),
         pitcher_flo_pct = round(pitcher_flo_pct, 2),
         platoon = case_when(p_throws == stand ~ 0,
                             TRUE ~ 1)) %>%
  unite("handedness", p_throws:stand, remove = TRUE) %>%
  mutate(fly_out = case_when(outcome == "fly_out" ~ 1,
                             TRUE ~ 0)) %>%
  group_by(hitter_flo_pct, pitcher_flo_pct, platoon) %>%
  dplyr::summarise(flo_ratio = mean(fly_out), num_instances = n())

model_table_flo <- ratios %>%
  filter(num_instances >= 200)

# ggplot(model_table_flo, aes(x = hitter_flo_pct, y = pitcher_flo_pct, fill = flo_ratio)) +
#   geom_tile()

set.seed(11) 
sample = sample.split(model_table_flo$flo_ratio, SplitRatio = .8)
train = subset(model_table_flo, sample == TRUE)
test  = subset(model_table_flo, sample == FALSE)

flo_model <- lm(flo_ratio ~ platoon + hitter_flo_pct * pitcher_flo_pct, 
                data = train,
                weights = num_instances)

### Uncomment below to verify results

# flo_predictions <- predict(flo_model, newdata = test)
# 
# summary(flo_model)
# 
# mse = mean((flo_predictions - test$flo_ratio)^2)
# print(mse)


# Pop Out Model ---------

ratios <- bip_pitches %>%
  dplyr::select(batter, pitcher, outcome, p_throws, stand, balls, strikes) %>%
  unite("count", balls:strikes, remove = TRUE) %>%
  dplyr::group_by(batter) %>%
  mutate(hitter_pop_pct = sum(outcome == "pop_out")/length(batter)) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pitcher_pop_pct = sum(outcome == "pop_out")/length(pitcher)) %>%
  ungroup() %>%
  mutate(hitter_pop_pct = round(hitter_pop_pct, 2),
         pitcher_pop_pct = round(pitcher_pop_pct, 2),
         platoon = case_when(p_throws == stand ~ 0,
                             TRUE ~ 1)) %>%
  unite("handedness", p_throws:stand, remove = TRUE) %>%
  mutate(pop_out = case_when(outcome == "pop_out" ~ 1,
                             TRUE ~ 0)) %>%
  group_by(hitter_pop_pct, pitcher_pop_pct, platoon) %>%
  dplyr::summarise(pop_ratio = mean(pop_out), num_instances = n())

model_table_pop <- ratios %>%
  filter(num_instances >= 300)

# ggplot(model_table_pop, aes(x = hitter_pop_pct, y = pitcher_pop_pct, fill = pop_ratio)) +
#   geom_tile()

set.seed(11) 
sample = sample.split(model_table_pop$pop_ratio, SplitRatio = .8)
train = subset(model_table_pop, sample == TRUE)
test  = subset(model_table_pop, sample == FALSE)

pop_model <- lm(pop_ratio ~ platoon + hitter_pop_pct * pitcher_pop_pct, 
                data = train,
                weights = num_instances)

### Uncomment below to verify results

# pop_predictions <- predict(pop_model, newdata = test)
# summary(pop_model)
# 
# mse = mean((pop_predictions - test$pop_ratio)^2)
# print(mse)


# Model Summaries ----------
summary(ss_model)
summary(cs_model)
summary(b_model)
summary(foul_model)
summary(hit_model)
summary(s1_model)
summary(hr_model)
summary(gro_model)
summary(flo_model)
summary(pop_model)


