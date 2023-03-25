#PITCH LOCATION
#plate_x
#plate_z

#STRIKE ZONE
#sz_top
#sz_bot


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
      select(at_bat_number, pitch_number, player_name, des, pitch_type, release_speed,
             balls, strikes, outs_when_up, inning, inning_topbot, home_score, away_score, on_1b, on_2b, on_3b,
             description, plate_x, plate_z, sz_top, sz_bot, 
             horiz_ball, vert_ball, correct_call, 
             flag)
  }
  return(df)
}

view(missed_calls(NYY_at_MIL_9_16_22, filtered = T)) #we say 24, Umpire Scorecards says 22
view(missed_calls(SFG_at_ATL_6_22_22, filtered = T)) #we say 8, Umpire Scorecards says 5
