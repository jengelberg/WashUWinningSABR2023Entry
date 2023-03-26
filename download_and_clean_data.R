library(baseballr)
library(tidyverse)
library(jtools)
library(caTools)
library(ltm)

# Code to download and clean the data necessary to run WashU's 2023 SABR Case Competition entry.
# The github repository for all code can be found at https://github.com/jengelberg/WashUWinningSABR2023Entry
# Created by Cooper Yan. Jake Engelberg, Max Hanley, and Simon Todreas

# Set working directory. Replace the below filepath with the path to the github repository:
setwd("/Users/maxhanley/Desktop/Work/SABR 2023/WashU SABR 2023 Winning Entry")

#Toggle which game to simulate
NYY_at_MIL = TRUE
SFG_at_ATL = FALSE

# Pitch Level Data Cleaning [RUN FIRST] ---------------------
# Cleans the season and pitch level data files. The code in this block only needs to be run once on setup.

# Read in raw pitch-level statcast data. total_pitches contains combined data from 2017-2022, whereas
# pitches_2021 and pitches_2022 contain pitch-level data from those respective years. This will
# take a few minutes to run.
total_pitches = read.csv("WashU SABR 2023 Winning Entry Data/total_pitches.csv")
pitches_2021 = read.csv("WashU SABR 2023 Winning Entry Data/pitches_2021.csv")
pitches_2022 = read.csv("WashU SABR 2023 Winning Entry Data/pitches_2022.csv")
total_pitches2 = total_pitches # preserving the original dataframe

# Read in raw game-level statcast data.
NYY_at_MIL_9_16_22 <- read_csv("WashU SABR 2023 Winning Entry Data/NYY at MIL 9-16-22.csv")
SFG_at_ATL_6_22_22 <- read_csv("WashU SABR 2023 Winning Entry Data/SFG at ATL 6-22-22.csv")

# Read in bullpen and starting pitching data
giants_bullpen <- readxl::read_excel("WashU SABR 2023 Winning Entry Data/bullpens/giants.xlsx")
braves_bullpen <- readxl::read_excel("WashU SABR 2023 Winning Entry Data/bullpens/braves.xlsx")
yankees_bullpen <- readxl::read_excel("WashU SABR 2023 Winning Entry Data/bullpens/yankees.xlsx")
brewers_bullpen <- readxl::read_excel("WashU SABR 2023 Winning Entry Data/bullpens/brewers.xlsx")
starters <- readxl::read_excel("WashU SABR 2023 Winning Entry Data/bullpens/starters.xlsx")

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
pitches_2021$description <- str_replace_all(pitches_2021$description, rep_str)
pitches_2022$description <- str_replace_all(pitches_2022$description, rep_str)

# Remove instances of pitchers hitting and position players pitching in all 3 dataframes
hitting_pitchers <- total_pitches %>%  mutate(found = batter %in% pitcher)
temp_batter = hitting_pitchers %>% 
  group_by(batter) %>% 
  summarise(num_pitches_batter = n()) 
temp_batter = temp_batter[-(1:6), , drop = FALSE]
temp_batter = transform(temp_batter, batter = as.integer(batter))
temp_pitcher = hitting_pitchers %>% 
  group_by(pitcher) %>% 
  summarise(num_pitches_pitcher = n()) 
player_key <- inner_join(temp_batter, temp_pitcher, by = c("batter" = "pitcher")) %>% 
  filter(batter != "660271" & batter != "547179")

position_players_pitching <- player_key %>% filter(num_pitches_batter > num_pitches_pitcher)
pitchers_hitting <- player_key %>% filter(num_pitches_pitcher > num_pitches_batter)

total_pitches <- total_pitches %>% 
  filter(!batter %in% pitchers_hitting$batter) %>% 
  filter(!pitcher %in% position_players_pitching$batter) %>%
  filter(balls < 4 & strikes < 3)

hitting_pitchers <- pitches_2021 %>%  mutate(found = batter %in% pitcher)
temp_batter = hitting_pitchers %>% 
  group_by(batter) %>% 
  summarise(num_pitches_batter = n()) 
temp_pitcher = hitting_pitchers %>% 
  group_by(pitcher) %>% 
  summarise(num_pitches_pitcher = n()) 
player_key <- inner_join(temp_batter, temp_pitcher, by = c("batter" = "pitcher")) %>% 
  filter(batter != "660271" & batter != "547179")

position_players_pitching <- player_key %>% filter(num_pitches_batter > num_pitches_pitcher)
pitchers_hitting <- player_key %>% filter(num_pitches_pitcher > num_pitches_batter)

pitches_2021 <- pitches_2021 %>% 
  filter(!batter %in% pitchers_hitting$batter) %>% 
  filter(!pitcher %in% position_players_pitching$batter) %>%
  filter(balls < 4 & strikes < 3)

hitting_pitchers <- pitches_2022 %>%  mutate(found = batter %in% pitcher)
temp_batter = hitting_pitchers %>% 
  group_by(batter) %>% 
  summarise(num_pitches_batter = n()) 
temp_pitcher = hitting_pitchers %>% 
  group_by(pitcher) %>% 
  summarise(num_pitches_pitcher = n()) 
player_key <- inner_join(temp_batter, temp_pitcher, by = c("batter" = "pitcher")) %>% 
  filter(batter != "660271" & batter != "547179")

position_players_pitching <- player_key %>% filter(num_pitches_batter > num_pitches_pitcher)
pitchers_hitting <- player_key %>% filter(num_pitches_pitcher > num_pitches_batter)

pitches_2022 <- pitches_2022 %>% 
  filter(!batter %in% pitchers_hitting$batter) %>% 
  filter(!pitcher %in% position_players_pitching$batter) %>%
  filter(balls < 4 & strikes < 3)

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
                          TRUE ~ "MIL"))
batter_rosters <- rbind(SFG_at_ATL_6_22_22 %>%
                          group_by(batter) %>%
                          summarise(batter = mean(batter)) %>%
                          mutate(game = "ATL"),
                        NYY_at_MIL_9_16_22 %>%
                          group_by(batter) %>%
                          summarise(batter = mean(batter)) %>%
                          mutate(game = "MIL"),
                        extra_batters)

extra_pitchers <- as.data.frame(as.matrix(c(
  666808, 608678, 669674, 641793, 606424,
  543521, 445926, 445276, 669276, 503285, 623451,
  547973, 676760, 657376,
  608718, 606131, 665001, 456713))) %>%
  rename("pitcher" = "V1")

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


temp_batters <- total_pitches %>%
  dplyr::select(batter, stand) %>%
  group_by(batter, stand) %>%
  summarise(batter = mean(batter))

switch_hitters <- temp_batters$batter[duplicated(temp_batters$batter)]

batter_rosters <- batter_rosters %>%
  left_join(total_pitches %>%
              dplyr::select(batter, stand) %>%
              group_by(batter, stand) %>%
              summarise(batter = mean(batter)) %>%
              mutate(stand = case_when(batter %in% switch_hitters ~ "S",
                                       TRUE ~ stand)) %>%
              unique())

pitcher_rosters <- pitcher_rosters %>%
  left_join(total_pitches %>%
              dplyr::select(pitcher, p_throws) %>%
              group_by(pitcher, p_throws) %>%
              summarise(pitcher = mean(pitcher)) %>%
              unique())


pitches_2022$description <- str_replace_all(pitches_2022$description, rep_str)
pitches_2021$description <- str_replace_all(pitches_2021$description, rep_str)
hitting_pitchers <- rbind(pitches_2022, pitches_2021) %>% mutate(found = batter %in% pitcher)
temp_batter = hitting_pitchers %>% 
  group_by(batter) %>% 
  summarise(num_pitches_batter = n()) 
temp_pitcher = hitting_pitchers %>% 
  group_by(pitcher) %>% 
  summarise(num_pitches_pitcher = n()) 
player_key <- inner_join(temp_batter, temp_pitcher, by = c("batter" = "pitcher")) %>% 
  filter(batter != "660271" & batter != "547179")

position_players_pitching <- player_key %>% filter(num_pitches_batter > num_pitches_pitcher)
pitchers_hitting <- player_key %>% filter(num_pitches_pitcher > num_pitches_batter)

pitches_2022 <- pitches_2022 %>% 
  filter(!batter %in% pitchers_hitting$batter) %>% 
  filter (!pitcher %in% position_players_pitching$batter)

pitches_2021 <- pitches_2021 %>% 
  filter(!batter %in% pitchers_hitting$batter) %>% 
  filter (!pitcher %in% position_players_pitching$batter)


bip_pitches_2022 <- rbind(pitches_2022, pitches_2021) %>%
  filter(
    description == "hit_into_play",
    events != "catcher_interf",
    events != "field_error",
    grepl("batter interference", des) == FALSE
  ) %>%
  #  dplyr::select(events, des) %>%
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

lg_avgs_and_wghts <- rbind(stabil_points, outcome_total_prob)


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

fg_park_hand <- fg_park_hand(2022)
fg_park <- fg_park(2022)

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


rm(hitting_pitchers, pitchers_hitting, player_key, position_players_pitching, temp_batter, temp_pitcher)
# Game Level Data Cleaning [RUN SECOND] --------------
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
rat_bats <- original_game_data[original_game_data$inning_topbot == "Top",]
hat_bats <- original_game_data[original_game_data$inning_topbot == "Bot",]
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
    #print(inning)
    #print(paste(sim_game$pitcher))
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









