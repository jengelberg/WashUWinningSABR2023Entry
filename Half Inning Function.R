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
