# Most comments are left in the data initialization, singles cleaning, and singles function tabs. 
# The rest of the tabs behave similarly.

# DATA INITIALIZATION -----------

#total_pitches2 is original, unfiltered df. Just the output of the rbind from 2017-22
total_pitches3 <- total_pitches2
  
#Simplify outcomes
rep_str = c('blocked_ball'='ball',
            'foul_tip'='swinging_strike',
            'foul_bunt'='swinging_strike',
            'swinging_strike_blocked'='swinging_strike', 
            'missed_bunt' = 'swinging_strike', 
            "pitchout" = "ball", 
            "bunt_foul_tip" = "swinging_strike", 
            "bunt_swinging_strike" = "swinging_strike",
            "foul_ball" = "foul")
total_pitches3$description <- str_replace_all(total_pitches3$description, rep_str)

#Creates dataframe of pitches in order by game, at bat, and pitch number
p_tot <- total_pitches3 %>%
  arrange(game_pk, at_bat_number, pitch_number) %>%
  dplyr::select(game_date, player_name, batter, pitcher, events, description, des, balls, strikes, 
         on_3b, on_2b, on_1b, outs_when_up, inning, inning_topbot, game_pk, at_bat_number, 
         pitch_number, home_score, away_score, bat_score, fld_score, post_home_score, 
         post_away_score, post_bat_score, post_fld_score)

# includes outcome for fly outs and ground outs
p_tot_o <- total_pitches3 %>%
  arrange(game_pk, at_bat_number, pitch_number) %>%
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
    


# SINGLES CLEANING ------------
# This section must be run as global variables
s1_indices <- which(p_tot$events == "single")   #identify singles
post_s1_indices <- s1_indices + 1

#this dataframe identifies the base state when a single is hit
p_tot_s1 = p_tot %>%
  slice(s1_indices) %>%
  mutate(on_3b = case_when(on_3b > 0 ~ 1,
                           TRUE ~ 0),
         on_2b = case_when(on_2b > 0 ~ 1,
                           TRUE ~ 0),
         on_1b = case_when(on_1b > 0 ~ 1,
                           TRUE ~ 0)) 

#this dataframe identifies the base state after a single is hit
p_tot_post_s1 = p_tot %>%
  slice(post_s1_indices) %>%
  mutate(on_3b_post = case_when(on_3b > 0 ~ 1,
                           TRUE ~ 0),
         on_2b_post = case_when(on_2b > 0 ~ 1,
                           TRUE ~ 0),
         on_1b_post = case_when(on_1b > 0 ~ 1,
                           TRUE ~ 0),
         inning_topbot_post = inning_topbot,
         outs_post_s1 = outs_when_up,
         inning_post = inning,
         post_field_score = fld_score) %>%
  dplyr::select(on_3b_post, on_2b_post, on_1b_post, outs_post_s1, inning_post, inning_topbot_post)

p_tot_s1_bind <- cbind(p_tot_s1, p_tot_post_s1)

#this dataframe shows how often the game goes from state x to state y after a single
#the pre single state is represented as "'runner on 3rd indicator'_'runner on 2nd indicator'_'runner on 1st indicator'"
#the post single state is represented as "'runner on 3rd indicator'_'runner on 2nd indicator'_'runner on 1st indicator'_'outs made on single'_'runs scored on single'"
p_tot_s1_full <- p_tot_s1_bind %>%
  filter(!(inning >= 9 & inning_post == 1)) %>%     #filter out walk-offs
  mutate(out_on_s1 = case_when(inning_topbot != inning_topbot_post ~ 3 - outs_when_up,
                               TRUE ~ outs_post_s1 - outs_when_up),
         runs_scored = case_when(inning_topbot != inning_topbot_post ~ post_field_score - bat_score,
                                 TRUE ~ post_bat_score - bat_score)) %>% 
  # marks how many outs are made and runs are scored on the single
  
  unite("post_s1_state", on_3b_post,on_2b_post,on_1b_post,out_on_s1,runs_scored, remove = FALSE) %>%
  # creates state of the game before and after the single
  unite("pre_s1_state", on_3b, on_2b, on_1b, outs_when_up, remove = FALSE) %>%
  group_by(pre_s1_state, post_s1_state) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(pre_s1_state) %>%
  pivot_wider(names_from = post_s1_state, values_from = count) %>%
  replace(is.na(.), 0)

#this is the final probability matrix from pre state to post state
p_tot_s1_full_pct <- cbind(p_tot_s1_full[,1], p_tot_s1_full[,-1]/rowSums(p_tot_s1_full[,-1]))



# SINGLES FUNCTION --------------

#This function will be called every time a single is hit to determine the state of the game post-single 
#  in terms of outs, runs, and runners on base

#outcome is formatted (3b, 2b, 1b, additional outs, additional runs)
singles <- function(on_3b, on_2b, on_1b, outs) {
  
  #Initialize game state
  state = paste(on_3b, on_2b, on_1b, outs, sep = "_")
  
  #Initialize while loop
  good_data = FALSE
  
  #While loop checks for data errors
  while (good_data == FALSE) {
    
    #randomly generate outcome of single based on current game state
    outcome = sample(x = colnames(p_tot_s1_full_pct[,-1]), replace = TRUE, size = 1, 
                     prob = p_tot_s1_full_pct[which(p_tot_s1_full_pct$pre_s1_state == state),-1])

    #organize return
    outcome_sep = str_split(outcome, "_")[[1]]
    
    on_3b_post = as.integer(outcome_sep[1])
    on_2b_post = as.integer(outcome_sep[2])
    on_1b_post = as.integer(outcome_sep[3])
    outs_post = as.integer(outcome_sep[4])
    runs_post = as.integer(outcome_sep[5])
    
    
    #check data error
    #if neither of these conditions are met, there must be a data error and we will just resample
    if(outs + outs_post >= 3) {
      good_data = TRUE
    } else if((on_3b + on_2b + on_1b + 1) == 
              (on_3b_post + on_2b_post + on_1b_post + outs_post + runs_post)) {
      good_data = TRUE
    }

  }
  
  return(c(on_3b_post, on_2b_post, on_1b_post, outs_post, runs_post))
}


# DOUBLES CLEANING ------------
# This section must be run as global variables
d2_indices <- which(p_tot$events == "double")
post_d2_indices <- d2_indices + 1

p_tot_d2 = p_tot %>%
  slice(d2_indices) %>%
  mutate(on_3b = case_when(on_3b > 0 ~ 1,
                           TRUE ~ 0),
         on_2b = case_when(on_2b > 0 ~ 1,
                           TRUE ~ 0),
         on_1b = case_when(on_1b > 0 ~ 1,
                           TRUE ~ 0)) 

p_tot_post_d2 = p_tot %>%
  slice(post_d2_indices) %>%
  mutate(on_3b_post = case_when(on_3b > 0 ~ 1,
                                TRUE ~ 0),
         on_2b_post = case_when(on_2b > 0 ~ 1,
                                TRUE ~ 0),
         on_1b_post = case_when(on_1b > 0 ~ 1,
                                TRUE ~ 0),
         inning_topbot_post = inning_topbot,
         outs_post_d2 = outs_when_up,
         inning_post = inning) %>%
  dplyr::select(on_3b_post, on_2b_post, on_1b_post, outs_post_d2, inning_post, inning_topbot_post)

p_tot_d2_bind <- cbind(p_tot_d2, p_tot_post_d2)

p_tot_d2_full <- p_tot_d2_bind %>%
  filter(!(inning >= 9 & inning_post == 1)) %>%
  mutate(out_on_d2 = case_when(inning_topbot != inning_topbot_post ~ 3 - outs_when_up,
                               TRUE ~ outs_post_d2 - outs_when_up),
         runs_scored = post_bat_score - bat_score) %>%
  unite("post_d2_state", on_3b_post,on_2b_post,on_1b_post,out_on_d2,runs_scored, remove = FALSE) %>%
  unite("pre_d2_state", on_3b, on_2b, on_1b, outs_when_up, remove = FALSE) %>%
  group_by(pre_d2_state, post_d2_state) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(pre_d2_state) %>%
  pivot_wider(names_from = post_d2_state, values_from = count) %>%
  replace(is.na(.), 0)

p_tot_d2_full_pct <- cbind(p_tot_d2_full[,1], p_tot_d2_full[,-1]/rowSums(p_tot_d2_full[,-1]))



# DOUBLES FUNCTION --------------
#outcome is formatted (3b, 2b, 1b, additional outs, additional runs)
doubles <- function(on_3b, on_2b, on_1b, outs) {
  
  #Initialize game state
  state = paste(on_3b, on_2b, on_1b, outs, sep = "_")
  
  #Initialize while loop
  good_data = FALSE
  
  #While loop checks for data errors
  while (good_data == FALSE) {
    
    outcome = sample(x = colnames(p_tot_d2_full_pct[,-1]), replace = TRUE, size = 1, 
                     prob = p_tot_d2_full_pct[which(p_tot_d2_full_pct$pre_d2_state == state),-1])

    #organize return
    outcome_sep = str_split(outcome, "_")[[1]]
    
    on_3b_post = as.integer(outcome_sep[1])
    on_2b_post = as.integer(outcome_sep[2])
    on_1b_post = as.integer(outcome_sep[3])
    outs_post = as.integer(outcome_sep[4])
    runs_post = as.integer(outcome_sep[5])
    
    
    #check data error
    if(outs + outs_post >= 3) {
      good_data = TRUE
    } else if((on_3b + on_2b + on_1b + 1) == 
              (on_3b_post + on_2b_post + on_1b_post + outs_post + runs_post)) {
      good_data = TRUE
    }
    
  }
  
  return(c(on_3b_post, on_2b_post, on_1b_post, outs_post, runs_post))
}



# FLY OUT CLEANING -------------
# This section must be run as global variables
flo_indices <- which(p_tot_o$outcome == "fly_out" & p_tot_o$events != "")
post_flo_indices <- flo_indices + 1

p_tot_flo = p_tot_o %>%
  slice(flo_indices) %>%
  mutate(on_3b = case_when(on_3b > 0 ~ 1,
                           TRUE ~ 0),
         on_2b = case_when(on_2b > 0 ~ 1,
                           TRUE ~ 0),
         on_1b = case_when(on_1b > 0 ~ 1,
                           TRUE ~ 0)) 

p_tot_post_flo = p_tot_o %>%
  slice(post_flo_indices) %>%
  mutate(on_3b_post = case_when(on_3b > 0 ~ 1,
                                TRUE ~ 0),
         on_2b_post = case_when(on_2b > 0 ~ 1,
                                TRUE ~ 0),
         on_1b_post = case_when(on_1b > 0 ~ 1,
                                TRUE ~ 0),
         inning_topbot_post = inning_topbot,
         outs_post_flo = outs_when_up,
         inning_post = inning) %>%
  dplyr::select(on_3b_post, on_2b_post, on_1b_post, outs_post_flo, inning_post, inning_topbot_post)

p_tot_flo_bind <- cbind(p_tot_flo, p_tot_post_flo)

p_tot_flo_full <- p_tot_flo_bind %>%
  filter(!(inning >= 9 & inning_post == 1)) %>%
  mutate(out_on_flo = case_when(inning_topbot != inning_topbot_post ~ 3 - outs_when_up,
                               TRUE ~ outs_post_flo - outs_when_up),
         runs_scored = post_bat_score - bat_score) %>%
  filter(out_on_flo >= 0) %>%
  unite("post_flo_state", on_3b_post,on_2b_post,on_1b_post,out_on_flo,runs_scored, remove = FALSE) %>%
  unite("pre_flo_state", on_3b, on_2b, on_1b, outs_when_up, remove = FALSE) %>%
  group_by(pre_flo_state, post_flo_state) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(pre_flo_state) %>%
  pivot_wider(names_from = post_flo_state, values_from = count) %>%
  replace(is.na(.), 0)

p_tot_flo_full_pct <- cbind(p_tot_flo_full[,1], p_tot_flo_full[,-1]/rowSums(p_tot_flo_full[,-1]))


# FLY OUT FUNCTION --------------
#outcome is formatted (3b, 2b, 1b, additional outs, additional runs)
fly_outs <- function(on_3b, on_2b, on_1b, outs) {
  
  #Initialize game state
  state = paste(on_3b, on_2b, on_1b, outs, sep = "_")
  
  #Initialize while loop
  good_data = FALSE
  
  #While loop checks for data errors
  while (good_data == FALSE) {
    
    outcome = sample(x = colnames(p_tot_flo_full_pct[,-1]), replace = TRUE, size = 1, 
                     prob = p_tot_flo_full_pct[which(p_tot_flo_full_pct$pre_flo_state == state),-1])
    
    #organize return
    outcome_sep = str_split(outcome, "_")[[1]]
    
    on_3b_post = as.integer(outcome_sep[1])
    on_2b_post = as.integer(outcome_sep[2])
    on_1b_post = as.integer(outcome_sep[3])
    outs_post = as.integer(outcome_sep[4])
    runs_post = as.integer(outcome_sep[5])
    
    
    #check data error
    if(outs + outs_post >= 3) {
      good_data = TRUE
    } else if((on_3b + on_2b + on_1b + 1) == 
              (on_3b_post + on_2b_post + on_1b_post + outs_post + runs_post)) {
      good_data = TRUE
    }
    
  }
  
  return(c(on_3b_post, on_2b_post, on_1b_post, outs_post, runs_post))
}


# GROUND OUT CLEANING -------------
# This section must be run as global variables
gro_indices <- which(p_tot_o$outcome == "ground_out" & p_tot_o$events != "")
post_gro_indices <- gro_indices + 1

p_tot_gro = p_tot_o %>%
  slice(gro_indices) %>%
  mutate(on_3b = case_when(on_3b > 0 ~ 1,
                           TRUE ~ 0),
         on_2b = case_when(on_2b > 0 ~ 1,
                           TRUE ~ 0),
         on_1b = case_when(on_1b > 0 ~ 1,
                           TRUE ~ 0)) 

p_tot_post_gro = p_tot_o %>%
  slice(post_gro_indices) %>%
  mutate(on_3b_post = case_when(on_3b > 0 ~ 1,
                                TRUE ~ 0),
         on_2b_post = case_when(on_2b > 0 ~ 1,
                                TRUE ~ 0),
         on_1b_post = case_when(on_1b > 0 ~ 1,
                                TRUE ~ 0),
         inning_topbot_post = inning_topbot,
         outs_post_gro = outs_when_up,
         inning_post = inning) %>%
  dplyr::select(on_3b_post, on_2b_post, on_1b_post, outs_post_gro, inning_post, inning_topbot_post)

p_tot_gro_bind <- cbind(head(p_tot_gro, -1), p_tot_post_gro)

p_tot_gro_full <- p_tot_gro_bind %>%
  filter(!(inning >= 9 & inning_post == 1)) %>%
  mutate(out_on_gro = case_when(inning_topbot != inning_topbot_post ~ 3 - outs_when_up,
                                TRUE ~ outs_post_gro - outs_when_up),
         runs_scored = post_bat_score - bat_score) %>%
  filter(out_on_gro >= 0) %>%
  unite("post_gro_state", on_3b_post,on_2b_post,on_1b_post,out_on_gro,runs_scored, remove = FALSE) %>%
  unite("pre_gro_state", on_3b, on_2b, on_1b, outs_when_up, remove = FALSE) %>%
  group_by(pre_gro_state, post_gro_state) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(pre_gro_state) %>%
  pivot_wider(names_from = post_gro_state, values_from = count) %>%
  replace(is.na(.), 0)

p_tot_gro_full_pct <- cbind(p_tot_gro_full[,1], p_tot_gro_full[,-1]/rowSums(p_tot_gro_full[,-1]))


# GROUND OUT FUNCTION --------------
#outcome is formatted (3b, 2b, 1b, additional outs, additional runs)
ground_outs <- function(on_3b, on_2b, on_1b, outs) {
  
  #Initialize game state
  state = paste(on_3b, on_2b, on_1b, outs, sep = "_")
  
  #Initialize while loop
  good_data = FALSE
  
  #While loop checks for data errors
  while (good_data == FALSE) {
    
    outcome = sample(x = colnames(p_tot_gro_full_pct[,-1]), replace = TRUE, size = 1, 
                     prob = p_tot_gro_full_pct[which(p_tot_gro_full_pct$pre_gro_state == state),-1])
    
    #organize return
    outcome_sep = str_split(outcome, "_")[[1]]
    
    on_3b_post = as.integer(outcome_sep[1])
    on_2b_post = as.integer(outcome_sep[2])
    on_1b_post = as.integer(outcome_sep[3])
    outs_post = as.integer(outcome_sep[4])
    runs_post = as.integer(outcome_sep[5])
    
    
    #check data error
    if(outs + outs_post >= 3) {
      good_data = TRUE
    } else if((on_3b + on_2b + on_1b + 1) == 
              (on_3b_post + on_2b_post + on_1b_post + outs_post + runs_post)) {
      good_data = TRUE
    }
    
  }
  
  return(c(on_3b_post, on_2b_post, on_1b_post, outs_post, runs_post))
}
