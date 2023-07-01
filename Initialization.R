library(baseballr)
library(tidyverse)
library(jtools)
library(caTools)
library(ltm)

# Code to download and clean the data, and initialize all functions necessary to run WashU's 2023 SABR Case Competition entry.
# The github repository for all code can be found at https://github.com/jengelberg/WashUWinningSABR2023Entry
# Created by Cooper Yan, Jake Engelberg, Max Hanley, and Simon Todreas

# Set working directory. Replace the below filepath with the path to the downloaded unzipped file:
setwd("<Your File Path>")

#Toggle which game to simulate
NYY_at_MIL = TRUE
SFG_at_ATL = FALSE

# Pitch Level Initialization [RUN FIRST] ---------------------
# Cleans the season and pitch level data files. The code in this block only needs to be run once on setup.

# Read in raw pitch-level statcast data. total_pitches contains combined data from 2017-2022. This will
# take a few minutes to run.
total_pitches = read.csv("total_pitches.csv")
total_pitches2 = total_pitches # preserving the original dataframe

# Read in raw game-level statcast data.
NYY_at_MIL_9_16_22 <- read_csv("NYY at MIL 9-16-22.csv")
SFG_at_ATL_6_22_22 <- read_csv("SFG at ATL 6-22-22.csv")

# Read in bullpen and starting pitching data
giants_bullpen <- readxl::read_excel("bullpens/giants.xlsx")
braves_bullpen <- readxl::read_excel("bullpens/braves.xlsx")
yankees_bullpen <- readxl::read_excel("bullpens/yankees.xlsx")
brewers_bullpen <- readxl::read_excel("bullpens/brewers.xlsx")
starters <- readxl::read_excel("bullpens/starters.xlsx")

# Clean total_pitches
total_pitches <- total_pitches %>% mutate_at(c("batter", "pitcher"), as.integer)

# Simplify the pitch outcomes contained in the data to outcomes understandable by our models
rep_str = c('blocked_ball'='ball',
            'foul_tip'='swinging_strike',
            'foul_bunt'='swinging_strike',
            'swinging_strike_blocked'='swinging_strike', 
            'missed_bunt' = 'swinging_strike', 
            "pitchout" = "ball", 
            "bunt_foul_tip" = "swinging_strike", 
            "bunt_swinging_strike" = "swinging_strike",
            "foul_ball" = "foul")
total_pitches$description <- str_replace_all(total_pitches$description, rep_str)

### Remove instances of pitchers hitting and position players pitching in all 3 dataframes
#Create player_key of players that both pitched and hit in our dataset
hitting_pitchers <- total_pitches %>% mutate(found = batter %in% pitcher)
temp_batter = hitting_pitchers %>% 
  group_by(batter) %>% 
  summarise(num_pitches_batter = n()) 
temp_batter = temp_batter[-(1:6), , drop = FALSE]
temp_batter = transform(temp_batter, batter = as.integer(batter))
temp_pitcher = hitting_pitchers %>% 
  group_by(pitcher) %>% 
  summarise(num_pitches_pitcher = n()) 
player_key <- inner_join(temp_batter, temp_pitcher, by = c("batter" = "pitcher")) %>% 
  filter(batter != "660271" & batter != "547179") # Michael Lorenzen and Shohei Ohtani are manually removed as two way players

#Infer which players had instances of position players pitching versus pitchers hitting
position_players_pitching <- player_key %>% filter(num_pitches_batter > num_pitches_pitcher)
pitchers_hitting <- player_key %>% filter(num_pitches_pitcher > num_pitches_batter)

#Filter out position players pitching and pitchers hitting
total_pitches <- total_pitches %>% 
  filter(!batter %in% pitchers_hitting$batter) %>% 
  filter(!pitcher %in% position_players_pitching$batter) %>%
  filter(balls < 4 & strikes < 3) #remove obvious data errors

#Create dataframes for specific years
pitches_2021 = total_pitches %>%
  filter(game_year == 2021)
pitches_2022 = total_pitches %>%
  filter(game_year == 2022)

#Input the game dataframe and filtered (TRUE means only the missed calls are displayed, FALSE means all calls)
#Output is game dataframe with a column "flag" that is 1 if the pitch is a missed call and 0 otherwise
missed_calls <- function(df, filtered = FALSE) {
  df <- df %>%
    mutate(
      horiz_ball = case_when(abs(plate_x) > ((8.5+1.45)/12) ~ 1,
                             TRUE ~ 0),
      vert_ball = case_when(plate_z > (sz_top+(1.45/12)) ~ 1,
                            plate_z < (sz_bot-(1.45/12)) ~ 1,
                            TRUE ~ 0),
      correct_call = case_when(horiz_ball + vert_ball == 0 ~ "called_strike",
                               TRUE ~ "ball"),
      flag = case_when(description == "ball" & correct_call == "called_strike" ~ 1,
                       description == "called_strike" & correct_call == "ball" ~ 1,
                       TRUE ~ 0))
  
  if(filtered == TRUE) {
    df <- df %>%
      filter(flag == 1) %>%
      dplyr::select(batter, pitcher...8, events, description, des, on_1b, on_2b, on_3b, balls, strikes, outs_when_up, inning, inning_topbot, home_score, away_score, flag)
    
  }
  return(df)
}

missed_calls_NYY_at_MIL <- missed_calls(NYY_at_MIL_9_16_22, filtered = FALSE) #we say 24, US says 22
missed_calls_SFG_at_ATL <- missed_calls(SFG_at_ATL_6_22_22, filtered = FALSE) #we say 8, US says 5

#These are bench players who were available to appear in the relevant games
extra_batters <- as.data.frame(as.matrix(c(
  592200, 
  518595, 628338, 645801, 
  672724, 664314, 641796, 
  649966, 621438, 670712))) %>%
  rename("batter" = "V1") %>%
  mutate(game = case_when(batter == 592200 ~ "ATL",
                          batter == 518595 ~ "ATL",
                          batter == 628338 ~ "ATL",
                          batter == 645801 ~ "ATL",
                          TRUE ~ "MIL"))  #game refers to the home team of the game, not the team the player is on

#All batters who can appear in the two games
batter_rosters <- rbind(SFG_at_ATL_6_22_22 %>%
                          group_by(batter) %>%
                          summarise(batter = mean(batter)) %>%
                          mutate(game = "ATL"),
                        NYY_at_MIL_9_16_22 %>%
                          group_by(batter) %>%
                          summarise(batter = mean(batter)) %>%
                          mutate(game = "MIL"),
                        extra_batters)

#Available relievers for any of the 4 teams for their respective game
extra_pitchers <- as.data.frame(as.matrix(c(
  666808, 608678, 669674, 641793, 606424,
  543521, 445926, 445276, 669276, 503285, 623451,
  547973, 676760, 657376,
  608718, 606131, 665001, 456713))) %>%
  rename("pitcher" = "V1")

#All pitchers who can appear in the two games
pitcher_rosters <- rbind(
  SFG_at_ATL_6_22_22 %>%
    group_by(pitcher...8) %>%
    summarise(pitcher = mean(pitcher...8)) %>%
    dplyr::select(pitcher),
  NYY_at_MIL_9_16_22 %>%
    group_by(pitcher...8) %>%
    summarise(pitcher = mean(pitcher...8)) %>%
    dplyr::select(pitcher),
  extra_pitchers
)

###Identify batter and pitcher handedness

temp_batters <- total_pitches %>%
  dplyr::select(batter, stand) %>%
  group_by(batter, stand) %>%
  summarise(batter = mean(batter))

switch_hitters <- temp_batters$batter[duplicated(temp_batters$batter)]

# Adding batter handedness to the roster
batter_rosters <- batter_rosters %>%
  left_join(total_pitches %>%
              dplyr::select(batter, stand) %>%
              group_by(batter, stand) %>%
              summarise(batter = mean(batter)) %>%
              mutate(stand = case_when(batter %in% switch_hitters ~ "S",
                                       TRUE ~ stand)) %>%
              unique())

# Adding pitcher handedness to the roster
pitcher_rosters <- pitcher_rosters %>%
  left_join(total_pitches %>%
              dplyr::select(pitcher, p_throws) %>%
              group_by(pitcher, p_throws) %>%
              summarise(pitcher = mean(pitcher)) %>%
              unique())


#Create dataframe with descriptions of ball in play events
bip_pitches_2022 <- rbind(pitches_2022, pitches_2021) %>%
  filter(  #filter for balls hit into play and remove rare events
    description == "hit_into_play",
    events != "catcher_interf",
    events != "field_error",
    grepl("batter interference", des) == FALSE
  ) %>%
  mutate(  #simplify outcomes
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

#this update of batter rosters calculates the proportion of pitches (for pitch level outcomes) or
#  balls hit into play that end in each outcome for each batter

batter_rosters <- batter_rosters %>%
  left_join(rbind(pitches_2022, pitches_2021) %>%
              dplyr::select(batter, pitcher, description) %>%
              group_by(batter) %>%
              mutate(hitter_ss_pct = sum(description == "swinging_strike")/length(batter),
                     hitter_cs_pct = sum(description == "called_strike")/length(batter),
                     hitter_b_pct = sum(description == "ball")/length(batter),
                     hitter_foul_pct = sum(description == "foul")/length(batter),
                     hitter_hbp_pct = sum(description == "hit_by_pitch")/length(batter),
                     hitter_hip_pct = sum(description == "hit_into_play")/length(batter)) %>%
              summarise(num_pitches = n(), 
                        ss_pct = mean(hitter_ss_pct),
                        cs_pct = mean(hitter_cs_pct),
                        b_pct = mean(hitter_b_pct),
                        foul_pct = mean(hitter_foul_pct),
                        hbp_pct = mean(hitter_hbp_pct),
                        hip_pct = mean(hitter_hip_pct))) %>%
  left_join(bip_pitches_2022 %>%
              dplyr::select(batter, pitcher, outcome, p_throws, stand) %>%
              dplyr::group_by(batter) %>%
              mutate(hitter_s1_pct = sum(outcome == "single")/length(batter),
                     hitter_d2_pct = sum(outcome == "double")/length(batter),
                     hitter_hr_pct = sum(outcome == "home_run")/length(batter),
                     hitter_gro_pct = sum(outcome == "ground_out")/length(batter),
                     hitter_flo_pct = sum(outcome == "fly_out")/length(batter),
                     hitter_pop_pct = sum(outcome == "pop_out")/length(batter),
                     hitter_line_pct = sum(outcome == "line_out")/length(batter)) %>%
              summarise(num_pitches_ip = n(),
                        s1_pct = mean(hitter_s1_pct),
                        d2_pct = mean(hitter_d2_pct),
                        hr_pct = mean(hitter_hr_pct),
                        gro_pct = mean(hitter_gro_pct),
                        flo_pct = mean(hitter_flo_pct),
                        pop_pct = mean(hitter_pop_pct),
                        line_pct = mean(hitter_line_pct)), 
            by = "batter")

# The corresponding update for pitcher rosters

pitcher_rosters <- pitcher_rosters %>%
  left_join(rbind(pitches_2022, pitches_2021) %>%
              dplyr::select(batter, pitcher, description, p_throws, stand) %>%
              group_by(pitcher) %>%
              mutate(pitcher_ss_pct = sum(description == "swinging_strike")/length(pitcher),
                     pitcher_cs_pct = sum(description == "called_strike")/length(pitcher),
                     pitcher_b_pct = sum(description == "ball")/length(pitcher),
                     pitcher_foul_pct = sum(description == "foul")/length(pitcher),
                     pitcher_hbp_pct = sum(description == "hit_by_pitch")/length(pitcher),
                     pitcher_hip_pct = sum(description == "hit_into_play")/length(pitcher)) %>%
              summarise(num_pitches = n(), 
                        ss_pct = mean(pitcher_ss_pct),
                        cs_pct = mean(pitcher_cs_pct),
                        b_pct = mean(pitcher_b_pct),
                        foul_pct = mean(pitcher_foul_pct),
                        hbp_pct = mean(pitcher_hbp_pct),
                        hip_pct = mean(pitcher_hip_pct))) %>%
  left_join(bip_pitches_2022 %>%
              dplyr::select(batter, pitcher, outcome, p_throws, stand) %>%
              dplyr::group_by(pitcher) %>%
              mutate(pitcher_s1_pct = sum(outcome == "single")/length(pitcher),
                     pitcher_d2_pct = sum(outcome == "double")/length(pitcher),
                     pitcher_hr_pct = sum(outcome == "home_run")/length(pitcher),
                     pitcher_gro_pct = sum(outcome == "ground_out")/length(pitcher),
                     pitcher_flo_pct = sum(outcome == "fly_out")/length(pitcher),
                     pitcher_pop_pct = sum(outcome == "pop_out")/length(pitcher),
                     pitcher_line_pct = sum(outcome == "line_out")/length(pitcher)) %>%
              summarise(num_pitches_ip = n(),
                        s1_pct = mean(pitcher_s1_pct),
                        d2_pct = mean(pitcher_d2_pct),
                        hr_pct = mean(pitcher_hr_pct),
                        gro_pct = mean(pitcher_gro_pct),
                        flo_pct = mean(pitcher_flo_pct),
                        pop_pct = mean(pitcher_pop_pct),
                        line_pct = mean(pitcher_line_pct)), 
            by = "pitcher")

#Normalize data

#these values are from the stability analysis done in Stablity Analysis.R
stabil_points <- data.frame(position = c("batter", "pitcher"),
                            ss = c(190, 470),
                            cs = c(370, 1450),
                            ball = c(770, 1270),
                            foul = c(790, 1170),
                            hip = c(535, 745),
                            s1 = c(790, NaN),
                            d2 = c(NaN, NaN),
                            hr = c(210, NaN),
                            gro = c(320, 160),
                            flo = c(380, 310),
                            pop = c(310, 410),
                            line = c(NaN, NaN))

#Calculating league-wide averages for every outcome
total_pitches_sum <- rbind(pitches_2022, pitches_2021) %>%
  dplyr::select(description) %>%
  group_by(description) %>%
  count() %>%
  ungroup() %>%
  mutate(pct = n/sum(n)) %>%
  rename("outcome" = "description")

total_bip_pitches_sum <- bip_pitches_2022 %>%
  dplyr::select(outcome) %>%
  group_by(outcome) %>%
  count() %>%
  ungroup() %>%
  mutate(pct = n/sum(n))

t_outcome_total_prob <- rbind(total_pitches_sum, total_bip_pitches_sum)
outcome_total_prob2 = data.frame(t(t_outcome_total_prob[,-c(1,2)]))
colnames(outcome_total_prob2) <- t(t_outcome_total_prob[,1]) 
outcome_total_prob <- outcome_total_prob2 %>%
  rename("ss" = "swinging_strike",
         "cs" = "called_strike",
         "hip" = "hit_into_play",
         "s1" = "single",
         "d2" = "double",
         "hr" = "home_run",
         "gro" = "ground_out",
         "flo" = "fly_out",
         "line" = "line_out",
         "pop" = "pop_out") %>%
  dplyr::select(-c(triple, hit_by_pitch)) %>%
  mutate(position = "lg_avg_rate")

#This dataframe will be used to normalize the inputs into the pitch model
lg_avgs_and_wghts <- rbind(stabil_points, outcome_total_prob)


#This version of batter rosters regresses the players' outcome proportions to the league average according to
# the dataframe above
batter_rosters2 <- batter_rosters %>%
  dplyr::select(-c(hbp_pct, d2_pct, line_pct)) %>%
  mutate(ss_pct = (ss_pct*num_pitches + lg_avgs_and_wghts$ss[1]*
                     lg_avgs_and_wghts$ss[3])/
           (num_pitches+lg_avgs_and_wghts$ss[1]),
         cs_pct = (cs_pct*num_pitches + lg_avgs_and_wghts$cs[1]*
                     lg_avgs_and_wghts$cs[3])/
           (num_pitches+lg_avgs_and_wghts$cs[1]),
         b_pct = (b_pct*num_pitches + lg_avgs_and_wghts$ball[1]*
                    lg_avgs_and_wghts$ball[3])/
           (num_pitches+lg_avgs_and_wghts$ball[1]),
         foul_pct = (foul_pct*num_pitches + lg_avgs_and_wghts$foul[1]*
                       lg_avgs_and_wghts$foul[3])/
           (num_pitches+lg_avgs_and_wghts$foul[1]),
         hip_pct = (hip_pct*num_pitches + lg_avgs_and_wghts$hip[1]*
                      lg_avgs_and_wghts$hip[3])/
           (num_pitches+lg_avgs_and_wghts$hip[1]),
         s1_pct = (s1_pct*num_pitches_ip + lg_avgs_and_wghts$s1[1]*
                     lg_avgs_and_wghts$s1[3])/
           (num_pitches_ip+lg_avgs_and_wghts$s1[1]),
         hr_pct = (hr_pct*num_pitches_ip + lg_avgs_and_wghts$hr[1]*
                     lg_avgs_and_wghts$hr[3])/
           (num_pitches_ip+lg_avgs_and_wghts$hr[1]),
         gro_pct = (gro_pct*num_pitches_ip + lg_avgs_and_wghts$gro[1]*
                      lg_avgs_and_wghts$gro[3])/
           (num_pitches_ip+lg_avgs_and_wghts$gro[1]),
         flo_pct = (flo_pct*num_pitches_ip + lg_avgs_and_wghts$flo[1]*
                      lg_avgs_and_wghts$flo[3])/
           (num_pitches_ip+lg_avgs_and_wghts$flo[1]),
         pop_pct = (pop_pct*num_pitches_ip + lg_avgs_and_wghts$pop[1]*
                      lg_avgs_and_wghts$pop[3])/
           (num_pitches_ip+lg_avgs_and_wghts$pop[1]))

#Pulling park adjustments
fg_park_hand <- fg_park_hand(2022)
fg_park <- fg_park(2022)

#This is the final version of batter rosters and it is park adjusted
batter_rosters3 <- batter_rosters2 %>%
  mutate(s1_pct = case_when(game == "ATL" & stand == "R" ~ 
                              s1_pct*fg_park_hand$single_as_RHH[16]/100,
                            game == "ATL" & stand == "L" ~ 
                              s1_pct*fg_park_hand$single_as_LHH[16]/100,
                            game == "MIL" & stand == "R" ~ 
                              s1_pct*fg_park_hand$single_as_RHH[23]/100,
                            game == "MIL" & stand == "L" ~ 
                              s1_pct*fg_park_hand$single_as_LHH[23]/100,
                            game == "ATL" & stand == "S" ~ 
                              s1_pct*fg_park$single[16]/100,
                            game == "MIL" & stand == "S" ~ 
                              s1_pct*fg_park$single[23]/100),
         hr_pct = case_when(game == "ATL" & stand == "R" ~ 
                              hr_pct*fg_park_hand$hr_as_RHH[16]/100,
                            game == "ATL" & stand == "L" ~ 
                              hr_pct*fg_park_hand$hr_as_LHH[16]/100,
                            game == "MIL" & stand == "R" ~ 
                              hr_pct*fg_park_hand$hr_as_RHH[23]/100,
                            game == "MIL" & stand == "L" ~ 
                              hr_pct*fg_park_hand$hr_as_LHH[23]/100,
                            game == "ATL" & stand == "S" ~ 
                              hr_pct*fg_park$hr[16]/100,
                            game == "MIL" & stand == "S" ~ 
                              hr_pct*fg_park$hr[23]/100),
         d2_pct = case_when(game == "ATL" & stand == "R" ~ 
                              lg_avgs_and_wghts$d2[3]*fg_park_hand$double_as_RHH[16]/100,
                            game == "ATL" & stand == "L" ~ 
                              lg_avgs_and_wghts$d2[3]*fg_park_hand$double_as_LHH[16]/100,
                            game == "MIL" & stand == "R" ~ 
                              lg_avgs_and_wghts$d2[3]*fg_park_hand$double_as_RHH[23]/100,
                            game == "MIL" & stand == "L" ~ 
                              lg_avgs_and_wghts$d2[3]*fg_park_hand$double_as_LHH[23]/100,
                            game == "ATL" & stand == "S" ~ 
                              lg_avgs_and_wghts$d2[3]*fg_park$double[16]/100,
                            game == "MIL" & stand == "S" ~ 
                              lg_avgs_and_wghts$d2[3]*fg_park$double[23]/100),
         t3_pct = case_when(game == "ATL" & stand == "R" ~ 
                              outcome_total_prob2$triple[1]*fg_park_hand$triple_as_RHH[16]/100,
                            game == "ATL" & stand == "L" ~ 
                              outcome_total_prob2$triple[1]*fg_park_hand$triple_as_LHH[16]/100,
                            game == "MIL" & stand == "R" ~ 
                              outcome_total_prob2$triple[1]*fg_park_hand$triple_as_RHH[23]/100,
                            game == "MIL" & stand == "L" ~ 
                              outcome_total_prob2$triple[1]*fg_park_hand$triple_as_LHH[23]/100,
                            game == "ATL" & stand == "S" ~ 
                              outcome_total_prob2$triple[1]*fg_park$triple[16]/100,
                            game == "MIL" & stand == "S" ~ 
                              outcome_total_prob2$triple[1]*fg_park$triple[23]/100),
         hbp_pct = outcome_total_prob2$hit_by_pitch[1],
         line_pct = outcome_total_prob2$line_out[1])

#This is the final version of pitcher roster and it is regressed to the league average rate.
# It is not park adjusted in order to not double count the effect
pitcher_rosters3 <- pitcher_rosters %>%
  dplyr::select(-c(hbp_pct, d2_pct, line_pct)) %>%
  mutate(ss_pct = (ss_pct*num_pitches + lg_avgs_and_wghts$ss[2]*
                     lg_avgs_and_wghts$ss[3])/
           (num_pitches+lg_avgs_and_wghts$ss[2]),
         cs_pct = (cs_pct*num_pitches + lg_avgs_and_wghts$cs[2]*
                     lg_avgs_and_wghts$cs[3])/
           (num_pitches+lg_avgs_and_wghts$cs[2]),
         b_pct = (b_pct*num_pitches + lg_avgs_and_wghts$ball[2]*
                    lg_avgs_and_wghts$ball[3])/
           (num_pitches+lg_avgs_and_wghts$ball[2]),
         foul_pct = (foul_pct*num_pitches + lg_avgs_and_wghts$foul[2]*
                       lg_avgs_and_wghts$foul[3])/
           (num_pitches+lg_avgs_and_wghts$foul[2]),
         hip_pct = (hip_pct*num_pitches + lg_avgs_and_wghts$hip[2]*
                      lg_avgs_and_wghts$hip[3])/
           (num_pitches+lg_avgs_and_wghts$hip[2]),
         s1_pct = (s1_pct*num_pitches_ip + lg_avgs_and_wghts$s1[2]*
                     lg_avgs_and_wghts$s1[3])/
           (num_pitches_ip+lg_avgs_and_wghts$s1[2]),
         hr_pct = (hr_pct*num_pitches_ip + lg_avgs_and_wghts$hr[2]*
                     lg_avgs_and_wghts$hr[3])/
           (num_pitches_ip+lg_avgs_and_wghts$hr[2]),
         gro_pct = (gro_pct*num_pitches_ip + lg_avgs_and_wghts$gro[2]*
                      lg_avgs_and_wghts$gro[3])/
           (num_pitches_ip+lg_avgs_and_wghts$gro[2]),
         flo_pct = (flo_pct*num_pitches_ip + lg_avgs_and_wghts$flo[2]*
                      lg_avgs_and_wghts$flo[3])/
           (num_pitches_ip+lg_avgs_and_wghts$flo[2]),
         pop_pct = (pop_pct*num_pitches_ip + lg_avgs_and_wghts$pop[2]*
                      lg_avgs_and_wghts$pop[3])/
           (num_pitches_ip+lg_avgs_and_wghts$pop[2]))


#remove unnecessary variables
rm(hitting_pitchers, pitchers_hitting, player_key, position_players_pitching, temp_batter, temp_pitcher,
   batter_rosters, batter_rosters2, pitcher_rosters)

# Game Level Initialization [RUN SECOND] --------------
# Cleans the game level data for use in the simulation. This code must be run each time the game chosen
# to simulate is changed.

#Set the "real" timeline to the correct game with missed calls identified
if(NYY_at_MIL == TRUE){
  original_game_data <- missed_calls_NYY_at_MIL
  roadBattingOrder <- c("592450", "519317", "650402", "518626", "665828", "643396", "503556", "624431", "543305")
  homeBattingOrder <- c("592885", "642715", "642133", "592669", "543939", "457705", "607054", "553882", "669003")
  home_bullpen <<- brewers_bullpen
  road_bullpen <<- yankees_bullpen
}
if (SFG_at_ATL == TRUE){
  original_game_data <- missed_calls_SFG_at_ATL
  roadBattingOrder <- c("592626","573262","527038","474832","573131","446334","600303","642731","642851")
  homeBattingOrder <- c("660670","621020","542303","621566","661388","606115","594807","594838","671739")
  home_bullpen <<- braves_bullpen
  road_bullpen <<- giants_bullpen
}

#Divide the original game data into top and bottom splits
rat_bats <- original_game_data[original_game_data$inning_topbot == "Top",] #road at bats
hat_bats <- original_game_data[original_game_data$inning_topbot == "Bot",] #home at bats
rat_bats <- rat_bats %>% rename("pitcher" = "pitcher...8")
hat_bats <- hat_bats %>% rename("pitcher" = "pitcher...8")
real_road_state <- rat_bats[c("batter", "pitcher", "events", "description", "des", "on_1b","on_2b", "on_3b", "balls", "strikes", "outs_when_up", "inning","inning_topbot", "away_score", "flag")]
real_home_state <- hat_bats[c("batter", "pitcher", "events", "description", "des", "on_1b","on_2b", "on_3b", "balls", "strikes", "outs_when_up", "inning","inning_topbot", "home_score", "flag")]

#Add pitch count to the real game states
real_road_state <- real_road_state %>% group_by(pitcher) %>% mutate(pitch_count = row_number())
real_home_state <- real_home_state %>% group_by(pitcher) %>% mutate(pitch_count = row_number())

#Add order spot to the real game states
roadBattingOrder <- setNames(roadBattingOrder, roadBattingOrder)
homeBattingOrder <- setNames(homeBattingOrder, homeBattingOrder)
road_batting_order <- data.frame(batter = names(roadBattingOrder), order_spot = seq_along(roadBattingOrder))
home_batting_order <- data.frame(batter = names(homeBattingOrder), order_spot = seq_along(homeBattingOrder))
real_road_state <- merge(real_road_state, road_batting_order, by = "batter")
real_home_state <- merge(real_home_state, home_batting_order, by = "batter")

#Reclassify certain outcomes to be understandable by the pitch models
rep_str = c('blocked_ball'='ball',
            'foul_tip'='swinging_strike',
            'foul_bunt'='swinging_strike',
            'swinging_strike_blocked'='swinging_strike', 
            'missed_bunt' = 'swinging_strike', 
            "pitchout" = "ball", 
            "bunt_foul_tip" = "swinging_strike", 
            "bunt_swinging_strike" = "swinging_strike",
            "foul_ball" = "foul")
real_road_state$description <- str_replace_all(real_road_state$description, rep_str)
real_home_state$description <- str_replace_all(real_home_state$description, rep_str)

real_road_state <- real_road_state %>%
  mutate(
    outcome = case_when(
      events == "walk" ~ "walk",
      events == "strikeout" ~ "strikeout",
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
    ))

real_home_state <- real_home_state %>%
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
    ) )

real_road_state$outcome <- ifelse(real_road_state$description == "hit_into_play", real_road_state$outcome, real_road_state$description)
real_home_state$outcome <- ifelse(real_home_state$description == "hit_into_play", real_home_state$outcome, real_home_state$description)
real_home_state <- real_home_state[c("batter", "pitcher", "on_1b","on_2b", "on_3b", "balls", "strikes", "outs_when_up", "inning", "inning_topbot", "order_spot", "home_score", "pitch_count", "outcome", "flag")]
real_road_state <- real_road_state[c("batter", "pitcher", "on_1b","on_2b", "on_3b", "balls", "strikes", "outs_when_up", "inning", "order_spot", "inning_topbot", "away_score", "pitch_count", "outcome", "flag")]
colnames(real_home_state)[colnames(real_home_state) == "outs_when_up"] <- "outs"
colnames(real_road_state)[colnames(real_road_state) == "outs_when_up"] <- "outs"
colnames(real_road_state)[colnames(real_road_state) == "away_score"] <- "score"
colnames(real_home_state)[colnames(real_home_state) == "home_score"] <- "score"

#Reclassify runners on base to binary
real_road_state$on_1b <- ifelse(is.na(real_road_state$on_1b), 0, 1)
real_road_state$on_2b <- ifelse(is.na(real_road_state$on_2b), 0, 1)
real_road_state$on_3b <- ifelse(is.na(real_road_state$on_3b), 0, 1)
real_home_state$on_1b <- ifelse(is.na(real_home_state$on_1b), 0, 1)
real_home_state$on_2b <- ifelse(is.na(real_home_state$on_2b), 0, 1)
real_home_state$on_3b <- ifelse(is.na(real_home_state$on_3b), 0, 1)

#Order the real game states by pitch_count, then by inning
real_road_state <- real_road_state[order(real_road_state$inning, real_road_state$pitch_count),]
real_home_state <- real_home_state[order(real_home_state$inning, real_home_state$pitch_count),]

#Only get missed calls
real_road_state_missed_calls <- real_road_state %>% filter(flag == 1)
real_home_state_missed_calls <- real_home_state %>% filter(flag == 1)

#Initialize pitching change functions. Pitching change determines if a change needs to occur, and 
#switch_pitcher swaps the pitcher out for a reliever
pitching_change <- function(sim_game){
  rng = runif(1)
  if(sim_game$inning <= 9){
    inning = sim_game$inning}
  else{inning = 9}
  # Check to see if current pitcher is a starter
  if(sim_game$pitcher == 605288 || sim_game$pitcher == 593423 || sim_game$pitcher == 450203){
    # If they're a starter, see if they potentially need to be swapped this inning
    if(rng  < starters[inning,paste(sim_game$pitcher)]){
      # pull them
      sim_game = switch_pitcher(sim_game)
      return(sim_game)
    }
    # keep them in
    return(sim_game)
  }
  else{
    #if the pitcher isn't a starter, they're a reliever and are only lasting one inning so need to be changed
    if(sim_game$inning_topbot == "Top" & ncol(home_bullpen) > 0){
      sim_game <- switch_pitcher(sim_game)}
    else if(sim_game$inning_topbot == "Bot" & ncol(road_bullpen) > 0){
      sim_game <- switch_pitcher(sim_game)}
    else(return(sim_game))
    
    return(sim_game)
  }
}
switch_pitcher <- function(sim_game){
  #switches a pitcher to the most likely bullpen arm
  if(sim_game$inning <= 9){
    inning = sim_game$inning}
  else{inning = 10}
  
  if(is.null(ncol(home_bullpen)) || ncol(home_bullpen) < 3 || nrow(home_bullpen) < 10){
    return(sim_game)
  }
  if(is.null(ncol(road_bullpen)) || ncol(road_bullpen) < 3 || nrow(road_bullpen) < 10){
    return(sim_game)
  }
  
  if(sim_game$inning_topbot == "Top"){
    #Home Team Bullpen
    pitcher_id <- as.integer(sample(x = colnames(home_bullpen), size = 1, prob = home_bullpen[inning,]))
    sim_game$pitcher = pitcher_id
    sim_game$pitch_count = 0
    home_bullpen <<- home_bullpen[, !colnames(home_bullpen) %in% sim_game$pitcher]
    home_bullpen <<- home_bullpen/rowSums(home_bullpen, na.rm=TRUE)
    return(sim_game)
  }
  if(sim_game$inning_topbot == "Bot"){
    #Road Team Bullpen
    pitcher_id <- as.integer(sample(x = colnames(road_bullpen), size = 1, prob = road_bullpen[inning,]))
    sim_game$pitcher = pitcher_id
    sim_game$pitch_count = 0
    road_bullpen <<- road_bullpen[, !colnames(road_bullpen) %in% sim_game$pitcher]
    road_bullpen <<- road_bullpen/rowSums(road_bullpen, na.rm=TRUE)
    return(sim_game)
  }
  
}



# Below is the initialization for our extra bases functions

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

# SINGLES CLEANING
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
  dplyr::select(on_3b_post, on_2b_post, on_1b_post, outs_post_s1, 
                inning_post, inning_topbot_post, post_field_score)

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

# SINGLES FUNCTION

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

# DOUBLES CLEANING
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

# DOUBLES FUNCTION
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

# FLY OUT CLEANING
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

# FLY OUT FUNCTION
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

# GROUND OUT CLEANING
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

p_tot_gro_bind <- cbind(head(p_tot_gro, -1), head(p_tot_post_gro, -1))

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

# GROUND OUT FUNCTION
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

# Set the simulation game start starting point
if(NYY_at_MIL == TRUE){
  # #NYY v MIL: Set first missed call as top 1, called strike should have been a ball
  sim_game_state_top <- real_road_state[27,]
  sim_game_state_top$outcome = "ball"
  sim_game_state_top$balls = sim_game_state_top$balls + 1
  
  sim_game_state_bottom <- real_home_state[1,]
  sim_game_state_bottom$outcome = "strike"
  sim_game_state_bottom$strikes = sim_game_state_bottom$strikes+1
}
if(SFG_at_ATL == TRUE){
  #SFG v ATL: Set first missed call as top 4, called strike should have been a ball
  sim_game_state_top <- real_road_state[57,]
  sim_game_state_top$outcome = "ball"
  sim_game_state_top$balls = sim_game_state_top$balls + 1
  
  sim_game_state_bottom <- real_home_state[92,]
  sim_game_state_bottom$outcome = "ball"
  sim_game_state_bottom$strikes = sim_game_state_bottom$balls+1
}








