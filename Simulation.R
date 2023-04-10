# Code to #Code to simulate one of two baseball games: NYY v MIL on 9/16/22 or SFG v ATL on 6/22/22 for WashU's 2023 SABR Case Competition entry.
# The github repository for all code can be found at https://github.com/jengelberg/WashUWinningSABR2023Entry
# Created by Cooper Yan. Jake Engelberg, Max Hanley, and Simon Todreas

#To Run 1 Simulation:
#1) Run Initialization.R in its entirety (and specify which game to simulate)
#2) Run Pitch Outcome Models.R in its entirety
#3) Run this file in its entirety to initialize the simulation. Then run line 423 again to run additional simulations of the same game.
#   if you would like to switch games, toggle the option in Initialization.R, re-run the Game Level Initialization, then re-run line 429

# GAME ------------
# "game" function takes four arguments to simulate the baseball game: sim_game_state_top and bottom: the current inning, score, outs, and baserunners for the visiting and home team respectively. real_road/home_state: the batting and pitching abilities of the two teams.
# The function first sets the bullpen for both teams to the default value and then simulates the game inning by inning until the end of the ninth inning.
# After the ninth inning, the function checks the score to see if the game is over. If the visiting team is winning, it simulates the bottom of the ninth inning. If the home team wins or ties the game in the bottom of the ninth, the function returns the final score. If the visiting team wins, the game is over and the function returns the final score.
# If the score is tied after the ninth inning, the function simulates extra innings until one team wins. In extra innings, each team takes turns batting and pitching until one team scores more runs in a complete inning than the other team.

game <- function(sim_game_state_top, sim_game_state_bottom, real_road_state, real_home_state){
  home_bullpen <<- braves_bullpen
  road_bullpen <<- giants_bullpen
  while(sim_game_state_top$inning <= 9){
    sim_game_state_top <- half_inning(sim_game_state_top, real_road_state, 0, top = TRUE)
    sim_game_state_top$inning = sim_game_state_top$inning + 1
  }
  
  while(sim_game_state_bottom$inning <= 8){
    sim_game_state_bottom <- half_inning(sim_game_state_bottom, real_home_state, sim_game_state_top$score, top = FALSE)
    sim_game_state_bottom$inning = sim_game_state_bottom$inning + 1
  }
  if(sim_game_state_top$score >= sim_game_state_bottom$score){
    sim_game_state_bottom <- half_inning(sim_game_state_bottom, real_home_state, sim_game_state_top$score,  top = FALSE)
    sim_game_state_bottom$inning = sim_game_state_bottom$inning + 1
  }
  else if (sim_game_state_top$score < sim_game_state_bottom$score){
    return(c(sim_game_state_top$score,sim_game_state_bottom$score))
  }
  
  # extra innings!
  if (sim_game_state_top$score == sim_game_state_bottom$score){
    while(sim_game_state_top$score == sim_game_state_bottom$score){
      sim_game_state_top <- half_inning(sim_game_state_top, real_road_state, 0, top = TRUE)
      sim_game_state_bottom <- half_inning(sim_game_state_bottom, real_home_state, sim_game_state_top$score, top = FALSE)
      sim_game_state_bottom$inning = sim_game_state_bottom$inning + 1
      sim_game_state_top$inning = sim_game_state_top$inning + 1
    }}
  
  return(c(sim_game_state_top$score,sim_game_state_bottom$score))
}
# HALF INNING -------
# The function starts by setting the score variable to zero and making a pitching change in the sim_game list.
# The function then enters a while loop that continues until there are three outs. 
# Within the while loop, the function calls the plate_appearance function to simulate a plate appearance and updates the sim_game list with the result.
# Depending on the outcome of the plate appearance, the function updates the on_1b, on_2b, and on_3b variables and the number of outs.
# If the outcome is a single, double, triple, or home run, the function also updates the score variable. If the outcome is a hit-by-pitch or walk, the function updates the base running based on the current state of the bases.
# If the outcome is a line out, pop out, or ground out, the function simply increments the number of outs and resets the strikes and balls variables.

half_inning <- function(sim_game, real_game_state, roadScore, top) {
  score = 0
  sim_game <- pitching_change(sim_game)
  
  if(sim_game$inning > 9){sim_game$on_2b = 1}
  
  while(sim_game$outs < 3){
    
    sim_game <- plate_appearance(sim_game,real_game_state)
    
    if(sim_game$outcome == "single"){
      single = singles(sim_game$on_3b, sim_game$on_2b, sim_game$on_1b, sim_game$outs)
      sim_game$on_3b = single[1]
      sim_game$on_2b = single[2]
      sim_game$on_1b = single[3]
      sim_game$outs = single[4] + sim_game$outs
      score = single[5] + score
    }
    else if(sim_game$outcome == "double"){
      double = doubles(sim_game$on_3b, sim_game$on_2b, sim_game$on_1b, sim_game$outs)
      sim_game$on_3b = double[1]
      sim_game$on_2b = double[2]
      sim_game$on_1b = double[3]
      sim_game$outs = double[4] + sim_game$outs
      score = double[5] + score
    }
    else if(sim_game$outcome == "triple"){
      score = score + sim_game$on_3b + sim_game$on_2b + sim_game$on_1b
      sim_game$on_3b = 1
      sim_game$on_1b = 0
      sim_game$on_2b = 0
    }
    else if(sim_game$outcome == "home_run"){
      score = score + sim_game$on_3b + sim_game$on_2b + sim_game$on_1b + 1
      sim_game$on_3b = 0
      sim_game$on_1b = 0
      sim_game$on_2b = 0
    }
    else if(sim_game$outcome == "hit_by_pitch" || sim_game$outcome == "walk"){
      #bases empty
      if (sim_game$on_1b == 0 & sim_game$on_2b == 0 & sim_game$on_3b == 0){
        sim_game$on_1b = 1}
      #runner on first
      else if (sim_game$on_1b == 1 & sim_game$on_2b == 0 & sim_game$on_3b == 0){
        sim_game$on_1b = 1 
        sim_game$on_2b = 1}
      #runners on first and second
      else if (sim_game$on_1b == 1 & sim_game$on_2b == 1 & sim_game$on_3b == 0){
        sim_game$on_1b = 1 
        sim_game$on_2b = 1
        sim_game$on_3b = 1}
      #bases loaded
      else if (sim_game$on_1b == 1 & sim_game$on_2b == 1 & sim_game$on_3b == 1){
        score =score+1}
      #runner on second
      else if (sim_game$on_1b == 0 & sim_game$on_2b == 1 & sim_game$on_3b == 0){
        sim_game$on_1b = 1 
        sim_game$on_2b = 1
        sim_game$on_3b = 0}
      #runner on third
      else if (sim_game$on_1b == 0 & sim_game$on_2b == 0 & sim_game$on_3b == 1){
        sim_game$on_1b = 1 
        sim_game$on_2b = 0
        sim_game$on_3b = 1}
      #runners on second and third
      else if (sim_game$on_1b == 0 & sim_game$on_2b == 1 & sim_game$on_3b == 1){
        sim_game$on_1b = 1 
        sim_game$on_2b = 1
        sim_game$on_3b = 1}
      sim_game$strikes = 0
      sim_game$balls = 0
    }
    else if(sim_game$outcome == "line_out"){
      sim_game$outs = sim_game$outs + 1
      sim_game$strikes = 0
      sim_game$balls = 0
    }
    else if(sim_game$outcome == "pop_out"){
      sim_game$outs = sim_game$outs + 1
      sim_game$strikes = 0
      sim_game$balls = 0
    }
    else if(sim_game$outcome == "ground_out"){
      gro = ground_outs(sim_game$on_3b, sim_game$on_2b, sim_game$on_1b, sim_game$outs)
      sim_game$on_3b = gro[1]
      sim_game$on_2b = gro[2]
      sim_game$on_1b = gro[3]
      sim_game$outs = gro[4] + sim_game$outs
      score = gro[5] + score
      
      sim_game$strikes = 0
      sim_game$balls = 0
    }
    else if(sim_game$outcome == "fly_out"){
      flo = fly_outs(sim_game$on_3b, sim_game$on_2b, sim_game$on_1b, sim_game$outs)
      sim_game$on_3b = flo[1]
      sim_game$on_2b = flo[2]
      sim_game$on_1b = flo[3]
      sim_game$outs = flo[4] + sim_game$outs
      score = flo[5] + score
      
      sim_game$strikes = 0
      sim_game$balls = 0
    }
    else if(sim_game$outcome == "strikeout"){
      sim_game$outs = sim_game$outs + 1
      sim_game$strikes = 0
      sim_game$balls = 0
    }
    
    if(sim_game$order_spot == 9){sim_game$order_spot = 1}
    else{sim_game$order_spot <- sim_game$order_spot + 1}
    
    sim_game$score = sim_game$score + score
    score = 0
    
    if((sim_game$inning > 8 & top == FALSE)) {
      if((sim_game$score+score > roadScore)){
        sim_game$outs = 3}}
    
    
    
  }
  sim_game$outs = 0
  sim_game$on_1b = 0
  sim_game$on_2b = 0
  sim_game$on_3b = 0
  return(sim_game)
}

#PLATE APPEARANCE --------
# This function simulates the result of a plate appearance by repeatedly calling the pitch function and tracking the count until a
# outcome is reached. Like the other functions it takes the sim_game state and the real_game state as paramters, and returns
# the updated sim_game

plate_appearance <- function(sim_game, real_game) {
  while (sim_game$balls < 4 & sim_game$strikes < 3) {
    sim_game$outcome = pitch(sim_game$balls, sim_game$strikes, sim_game$pitcher, sim_game$batter)
    
    sim_game$pitch_count <- sim_game$pitch_count + 1
    
    # This is where we check to see if it is possible to rejoin the original timeline. If at any point the real_game state matches
    # the current sim_game, we set the sim_game equal to the game state of the next missed call in the real timeline following the
    # matched state, then continue that plate appearance
    if(sim_game$inning < 9){
      if(any(apply(real_game[c("batter", "pitcher", "on_1b","on_2b", "on_3b", "balls", "strikes", "outs", "inning", "order_spot", "score")], 1, function(row) identical(row, sim_game[c("batter", "pitcher", "on_1b","on_2b", "on_3b", "balls", "strikes", "outs", "inning", "order_spot", "score")])))){
        match <- which(apply(real_game, 1, identical, sim_game))
        next_missed_call_index <- min(which(real_game$flag[(match+1):nrow(real_game)] == 1)) + match
        sim_game = real_game[next_missed_call_index,]
        if(sim_game$outcome == "called_strike" || sim_game$outcome == "swinging_strike"){sim_game$outcome = "ball"}
        else if (sim_game$outcome == "ball"){sim_game$outcome == "strike"}}
    }
    # Update the count
    if(sim_game$outcome == "called_strike" || sim_game$outcome == "swinging_strike") {
      sim_game$strikes = sim_game$strikes + 1
    } else if (sim_game$outcome == "ball") {
      sim_game$balls = sim_game$balls + 1
    } else if (sim_game$outcome == "foul") {
      if(sim_game$strikes < 2) {
        sim_game$strikes = sim_game$strikes + 1
      }
    } else {
      sim_game$strikes = 0
      sim_game$balls = 0
      return(sim_game)
    }
  }
  # The pitch model only outputs hits or count changes, so these check for strikeouts and walks
  if(sim_game$balls == 4) {
    sim_game$outcome = "walk"
    sim_game$strikes = 0
    sim_game$balls = 0
    return(sim_game)
  } else if(sim_game$strikes == 3) {
    sim_game$outcome = "strikeout"
    sim_game$strikes = 0
    sim_game$balls = 0
    return(sim_game)
  }
}
#PITCH ------------
# The pitch model is the base case of our function. It takes in the current count, pitcher, and hitter and uses the pitch models
# created in Pitch Outcome Models.R to return the outcome of that pitch
pitch <- function(balls,
                  strikes,
                  onMound,
                  hitter) {
  
  # Get the batter and pitcher's statistics
  current_batter <- batter_rosters3[batter_rosters3$batter == hitter,]
  current_pitcher <- pitcher_rosters3[pitcher_rosters3$pitcher == onMound,]
  
  # Define whether there is a platoon advantage (we assume switch hitters always opt for the platoon advantage)
  platoon = ifelse(current_batter$stand == current_pitcher$p_throws, 0, 1)
  platoon = ifelse(current_batter$stand == "S", 1, 0)
  p_throws = current_pitcher$p_throws
  stand = current_batter$stand
  count = paste(balls, strikes, sep = "_")
  if(stand == "S" & p_throws == "L"){
    stand = "R"
  }
  else if (stand == "S" & p_throws == "R"){
    stand = "L"
  }
  handedness = paste(p_throws, stand, sep = "_")
  
  # Calculate the probabilities of each outcome, given the pitcher and hitter statistics
  ss_temp <-
    data.frame(
      "count" = count, 
      "handedness" = handedness,
      "hitter_ss_pct" =  current_batter$ss_pct,
      "pitcher_ss_pct" = current_pitcher$ss_pct
    ) 
  ss_prob_odds <- predict(ss_model, newdata = ss_temp)
  ss_prob_odds <- ss_prob_odds ^ 2
  
  cs_temp <-
    data.frame(
      "count" = count, 
      "handedness" = handedness,
      "hitter_cs_pct" =  current_batter$cs_pct,
      "pitcher_cs_pct" = current_pitcher$cs_pct,
      platoon
    ) 
  cs_prob_odds <- predict(cs_model, newdata = cs_temp)
  
  b_temp <-
    data.frame(
      "count" = count, 
      "platoon" = platoon,
      "hitter_b_pct" =  current_batter$b_pct,
      "pitcher_b_pct" = current_pitcher$b_pct
    ) 
  b_prob_odds <- predict(b_model, newdata = b_temp)
  
  foul_temp <-
    data.frame(
      "count" = count, 
      "handedness" = handedness,
      "hitter_foul_pct" =  current_batter$foul_pct,
      "pitcher_foul_pct" = current_pitcher$foul_pct
    ) 
  foul_prob_odds <- predict(foul_model, newdata = foul_temp)
  
  hbp_prob_odds <- 0.002945786
  
  hit_temp <-
    data.frame(
      "count" = count, 
      "platoon" = platoon,
      "hitter_hit_pct" =  current_batter$hip_pct,
      "pitcher_hit_pct" = current_pitcher$hip_pct
    ) 
  hit_prob_odds <- predict(hit_model, newdata = hit_temp)
  
  total_prob_odds <-
    ss_prob_odds + cs_prob_odds + b_prob_odds + foul_prob_odds + hbp_prob_odds +
    hit_prob_odds
  ss_prob_odds = ss_prob_odds / total_prob_odds
  cs_prob_odds = cs_prob_odds / total_prob_odds
  b_prob_odds = b_prob_odds / total_prob_odds
  foul_prob_odds = foul_prob_odds / total_prob_odds
  hbp_prob_odds = hbp_prob_odds / total_prob_odds
  hit_prob_odds = hit_prob_odds / total_prob_odds
  
  # Use the probabilites calculated above to determine the outcome of the pitch
  rng = runif (1)
  if (rng < ss_prob_odds) {
    outcome = "swinging_strike"
  } else if (rng < ss_prob_odds + cs_prob_odds) {
    outcome = "called_strike"
  } else if (rng < ss_prob_odds + cs_prob_odds + b_prob_odds) {
    outcome = "ball"
  } else if (rng < ss_prob_odds + cs_prob_odds + b_prob_odds + foul_prob_odds) {
    outcome = "foul"
  } else if (rng < ss_prob_odds + cs_prob_odds + b_prob_odds + foul_prob_odds +
             hbp_prob_odds) {
    outcome = "hit_by_pitch"
  } else{
    outcome = "balls_in_play"
  }
  if (outcome == "balls_in_play") {
    s1_temp <-
      data.frame(
        "hitter_s1_pct" = current_batter$s1_pct,
        "pitcher_s1_pct" = current_pitcher$s1_pct,
        platoon
      )
    s1_prob_odds <- predict(s1_model, newdata = s1_temp)
    
    d2_prob_odds <- current_batter$d2_pct
    
    t3_prob_odds <- current_batter$t3_pct
    
    hr_temp <-
      data.frame(
        "hitter_hr_pct" = current_batter$hr_pct,
        "pitcher_hr_pct" = current_pitcher$hr_pct,
        "platoon" = platoon
      )
    hr_prob_odds <- predict(hr_model, newdata = hr_temp)
    
    gro_temp <-
      data.frame(
        "hitter_gro_pct" = current_batter$gro_pct,
        "pitcher_gro_pct" = current_pitcher$gro_pct,
        "platoon" = platoon
      )
    gro_prob_odds <- predict(gro_model, newdata = gro_temp)
    
    flo_temp <-
      data.frame(
        "hitter_flo_pct" = current_batter$flo_pct,
        "pitcher_flo_pct" = current_pitcher$flo_pct,
        "platoon" = platoon
      )
    flo_prob_odds <- predict(flo_model, newdata = flo_temp)
    
    pop_temp <-
      data.frame(
        "hitter_pop_pct" = current_batter$pop_pct,
        "pitcher_pop_pct" = current_pitcher$pop_pct,
        "platoon" = platoon
      )
    pop_prob_odds <- predict(pop_model, newdata = pop_temp)
    
    line_prob_odds <- 0.08398726
    
    rng = runif(1)
    
    total_prob_odds <-
      s1_prob_odds + d2_prob_odds + t3_prob_odds + hr_prob_odds + gro_prob_odds +
      flo_prob_odds + line_prob_odds
    s1_prob_odds = s1_prob_odds / total_prob_odds
    d2_prob_odds = d2_prob_odds / total_prob_odds
    t3_prob_odds = t3_prob_odds / total_prob_odds
    hr_prob_odds = hr_prob_odds / total_prob_odds
    gro_prob_odds = gro_prob_odds / total_prob_odds
    flo_prob_odds = flo_prob_odds / total_prob_odds
    line_prob_odds = line_prob_odds / total_prob_odds
    if (rng < s1_prob_odds) {
      outcome = "single"
    } else if (rng < s1_prob_odds + d2_prob_odds) {
      outcome = "double"
    } else if (rng < s1_prob_odds + d2_prob_odds + t3_prob_odds) {
      outcome = "triple"
    } else if (rng < s1_prob_odds + d2_prob_odds + t3_prob_odds + hr_prob_odds) {
      outcome = "home_run"
    } else if (rng < s1_prob_odds + d2_prob_odds + t3_prob_odds + hr_prob_odds +
               gro_prob_odds) {
      outcome = "ground_out"
    } else if (rng < s1_prob_odds + d2_prob_odds + t3_prob_odds + hr_prob_odds +
               gro_prob_odds + flo_prob_odds) {
      outcome = "fly_out"
    } else if (rng < s1_prob_odds + d2_prob_odds + t3_prob_odds + hr_prob_odds +
               gro_prob_odds + flo_prob_odds + line_prob_odds) {
      outcome = "line_out"
    } else{
      outcome = "pop_out"
    }
  }
  
  return(outcome)
}

#RUN SIMULATION -------
game(sim_game_state_top, sim_game_state_bottom, real_road_state, real_home_state)