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
