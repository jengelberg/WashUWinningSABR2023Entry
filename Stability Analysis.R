pitches_copy = total_pitches

#this dataframe adds the total pitches each batter and pitcher has and numbers them
pitches_copy = pitches_copy %>%
  group_by(batter) %>%
  mutate(batterNumPitch = ntile(n = length(batter)), batterTotalPitches = n()) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pitcherNumPitch = ntile(n = length(pitcher)), pitcherTotalPitches = n()) %>%
  ungroup() 


#CRONBACH'S ALPHA FUNCTION ----------------

#This function finds Cronbach's alpha for a given pitch result for sample sizes of 10 to maxSamples by 10
#Input is the pitch by pitch dataframe, the pitch result we want to examine, and the maximum number of samples
#Output is a dataframe with one column the number of samples and the other columns the alpha for batters and pitchers

cronbach_alpha_func <- function(df, pitch_result = "", maxSamples = 100) {
  
  #Initialize alphas
  batter_alphas = c(0)
  pitcher_alphas = c(0)
  
  i = 10 # minimum/starting sample size
  iter = 1 # iteration to keep track of row in output dataframe
  
  while(i<=maxSamples) {
    
    #batter_df formats our dataframe such that Cronbach's alpha can be calculated for batters
    batter_df <- df %>%
      ungroup() %>%
      filter(batterNumPitch <= i, batterTotalPitches >= maxSamples) %>% 
      # above line filters out batters for whom we cannot calculate alpha
      mutate(pitch_result = case_when(description == pitch_result ~ 1, TRUE ~ 0)) %>%
      dplyr::select(batter, pitch_result, batterNumPitch) %>%
      pivot_wider(names_from = batterNumPitch, values_from = pitch_result) %>%
      dplyr::select(-batter) %>%
      na.omit()
    
    batter_alphas[iter] <- cronbach.alpha(batter_df)$alpha #calculates and stores Cronbach's alpha
    
    
    #pitcher_df formats our dataframe such that Cronbach's alpha can be calculated for pitchers
    pitcher_df <- df %>%
      ungroup() %>%
      filter(pitcherNumPitch <= i, pitcherTotalPitches >= maxSamples) %>%
      # above line filters out pitchers for whom we cannot calculate alpha
      mutate(pitch_result = case_when(description == pitch_result ~ 1, TRUE ~ 0)) %>%
      dplyr::select(pitcher, pitch_result, pitcherNumPitch) %>%
      pivot_wider(names_from = pitcherNumPitch, values_from = pitch_result) %>%
      dplyr::select(-pitcher) %>%
      na.omit()
    
    pitcher_alphas[iter] <- cronbach.alpha(pitcher_df)$alpha #calculates and stores Cronbach's alpha
    
    i = i+10
    iter = iter + 1
  }
  
  num_pitches = seq(10, maxSamples, 10)
  alphas = cbind(batter_alphas, pitcher_alphas, num_pitches)
  return(as.data.frame(alphas))
}

#CRONBACH'S ALPHA FOR PITCH OUTCOMES -------------------

#Creates Cronbach's Alpha dataframes for all pitch outcomes
CA_swinging_strike = cronbach_alpha_func(pitches_copy, pitch_result = "swinging_strike",
                                         maxSamples = 500)
CA_called_strike = cronbach_alpha_func(pitches_copy, pitch_result = "called_strike",
                                       maxSamples = 1500)
CA_foul = cronbach_alpha_func(pitches_copy, pitch_result = "foul",
                              maxSamples = 1500)
CA_ball = cronbach_alpha_func(pitches_copy, pitch_result = "ball",
                              maxSamples = 2000)
CA_hit_by_pitch = cronbach_alpha_func(pitches_copy, pitch_result = "hit_by_pitch",
                                      maxSamples = 2000)
CA_hit_into_play = cronbach_alpha_func(pitches_copy, pitch_result = "hit_into_play",
                                       maxSamples = 1000)


#VISUALIZING CRONBACH'S ALPHA -----------------

# currently set up for swinging strikes

x = seq(10, 500, 10) #replace 500 with maxSamples value (i.e. 1500 for called_strike, 2000 for ball, etc.)
CA_df <- as.data.frame(cbind(CA_swinging_strike, x)) #replace CA_swinging_strike with dataframe of interest


ggplot(data = CA_df) +
  geom_line(aes(x = x, y = batter_alphas), col = "red") +
  geom_line(aes(x = x, y = pitcher_alphas), col = "blue") +
  geom_hline(yintercept = 1/sqrt(2)) +
  guides(color = guide_legend(title = "Cronbach's Alpha for Swinging Strikes")) +
  ggtitle("Swinging Strike") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Cronbach's Alpha") + 
  xlab("Sample Size")


# CRONBACH'S ALPHA FOR BIP OUTCOMES -----------

#Ball In Play CA Analysis

#All code mirrors the pitch outcome stability analysis done above

bip_pitches_copy <- bip_pitches %>%
  group_by(batter) %>%
  mutate(batterNumPitch = ntile(n = length(batter)), batterTotalPitches = n()) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pitcherNumPitch = ntile(n = length(pitcher)), pitcherTotalPitches = n()) %>%
  ungroup() 

cronbach_alpha_func_bip <- function(df, pitch_result = "", maxSamples = 100) {
  
  batter_alphas = c(0)
  pitcher_alphas = c(0)
  
  i = 10
  iter = 1
  while(i<=maxSamples){
    batter_df <- df %>%
      ungroup() %>%
      filter(batterNumPitch <= i, batterTotalPitches >= maxSamples) %>%
      mutate(pitch_result = case_when(outcome == pitch_result ~ 1, TRUE ~ 0)) %>%
      dplyr::select(batter, pitch_result, batterNumPitch) %>%
      pivot_wider(names_from = batterNumPitch, values_from = pitch_result) %>%
      dplyr::select(-batter) %>%
      na.omit()
    
    batter_alphas[iter] <- cronbach.alpha(batter_df)$alpha
    
    pitcher_df <- df %>%
      ungroup() %>%
      filter(pitcherNumPitch <= i, pitcherTotalPitches >= maxSamples) %>%
      mutate(pitch_result = case_when(outcome == pitch_result ~ 1, TRUE ~ 0)) %>%
      dplyr::select(pitcher, pitch_result, pitcherNumPitch) %>%
      pivot_wider(names_from = pitcherNumPitch, values_from = pitch_result) %>%
      dplyr::select(-pitcher) %>%
      na.omit()
    
    pitcher_alphas[iter] <- cronbach.alpha(pitcher_df)$alpha
    
    i = i+10
    iter = iter + 1
  }
  
  num_pitches = seq(10, maxSamples, 10)
  alphas = cbind(batter_alphas, pitcher_alphas, num_pitches)
  return(as.data.frame(alphas))
}

#Cronbach's Alpha dataframes for all BIP results

CA_singles = cronbach_alpha_func_bip(bip_pitches_copy, pitch_result = "single", 
                                       maxSamples = 1000)
CA_doubles = cronbach_alpha_func_bip(bip_pitches_copy, pitch_result = "double", 
                                     maxSamples = 1000)
CA_triples = cronbach_alpha_func_bip(bip_pitches_copy, pitch_result = "triple", 
                                     maxSamples = 1000)
CA_home_run = cronbach_alpha_func_bip(bip_pitches_copy, pitch_result = "home_run", 
                                     maxSamples = 1000)
CA_ground_out = cronbach_alpha_func_bip(bip_pitches_copy, pitch_result = "ground_out", 
                                     maxSamples = 1000)
CA_fly_out = cronbach_alpha_func_bip(bip_pitches_copy, pitch_result = "fly_out", 
                                     maxSamples = 1000)
CA_pop_out = cronbach_alpha_func_bip(bip_pitches_copy, pitch_result = "pop_out", 
                                     maxSamples = 1000)
CA_line_out = cronbach_alpha_func_bip(bip_pitches_copy, pitch_result = "line_out", 
                                     maxSamples = 1000)


#Analyzed to see if hits are stable together because they are not independently (they are not for pitchers)

bip_pitches_hits <- bip_pitches %>%
  mutate(outcome = case_when(outcome == "single" ~ "hit",
                             outcome == "double" ~ "hit",
                             outcome == "triple" ~ "hit",
                             TRUE ~ outcome)) %>%
  group_by(batter) %>%
  mutate(batterNumPitch = ntile(n = length(batter)), batterTotalPitches = n()) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pitcherNumPitch = ntile(n = length(pitcher)), pitcherTotalPitches = n()) %>%
  ungroup()

CA_hit = cronbach_alpha_func_bip(bip_pitches_hits, pitch_result = "hit", 
                                      maxSamples = 1000)
