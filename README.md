# WashUWinningSABR2023Entry
This is the code behind the winning presentation given by Washington University in St. Louis at the 2023 Diamond Dollars Case Competition regarding how correcting all umpire missed calls would impact the outcome of two specific games.

## Table of Contents
[Introduction](#introduction)

[Code Instructions](#code-instructions)

[Prompt](#prompt)

[Approach](#approach)

[Models](#models)

[Results](#results)

## Introduction
This past spring we (Simon Todreas, Max Hanley, Jake Engelberg, Cooper Yan) competed in and won the 2023 Diamond Dollars Case Competition at the SABR Analytics Conference. We were selected to give the encore presentation to the entire conference which can be viewed at the link below. Our slides are also available to view in the SABR 2023 PPT.pdf file.
https://youtu.be/mhe8CWTuzZE

## Code Instructions
To Run 1 Simulation:
1) Download the zip file located in this google drive https://drive.google.com/drive/folders/10VO4m385z2CbjsdbpyiqmRgHVrJH2nim?usp=drive_link
2) Unzip the file
3) In Initialization.R, switch line 12 to reflect your working directory. The last layer should be the unzipped folder.
4) Run Initialization.R in its entirety (and specify which game to simulate in lines 15-16)
5) Run Pitch Outcome Models.R in its entirety
6) Run Simulation.R to get the result of 1 simulation. The score will be displayed "road_score home_score". You can run line 423 again to get additional simulations of the same game.
7) If you would like to see our stability analysis results, run the Stability Analysis.R file for whichever outcomes you are interested in.

Note: If you would like to switch games, toggle the option in Initialization.R (lines 15-16), re-run the Game Level Initialization, then re-run line 423

## Prompt
The prompt was “to analyze the way in which [ball/strike] missed calls might have affected the final score and outcome of selected games.” We were asked to “effectively re-write history, by producing an alternative final score of two games.” The only data we were given was the pitch-by-pitch data for the games we could choose to analyze.

The games we chose to analyze were the Yankees @ Brewers game on 9/16/22 and the Giants @ Braves game on 6/22/22.

## Approach
Since we were looking at umpire impact on specific games, simply examining the run value of each missed call is not good enough. Instead, we took into account the butterfly effect by building a simulation that began running at a missed call. For example, if the first missed call of the game was the first pitch of the 2nd inning, our simulation would begin after that call was corrected and what actually happened no longer matches what “should” have happened. 

Once the simulation begins, each pitch is compared to what happened in the real game, and if the simulated timeline and real timeline ever align, we go back to the actual timeline and skip ahead to the next missed call where we restart the simulation (or to the end of the game of there are no more missed calls). The key assumption of the rejoining timelines is that baseball games behave like a Markov chain where only the current game state affects future outcomes. So if the simulated timeline ever aligns exactly with the real timeline, it does not matter how the two timelines got to that point. Previous studies support this assumption including “Does momentum exist in a baseball game?” by Rebecca Sala and Jeffrey Simonoff, both of New York University. 

Using the combining simulated timeline allows us to use as much real game data as possible and stay as far away as possible from simply simulating independent baseball games.

## Models
In our simulation, we built independent weighted least squares models for every pitch level event with hit-into-play models conditional on the ball being hit into play. Our models were trained on regular season MLB data from 2017-2022 and took in pitcher event rate, batter event rate, count, and handedness for batter and pitcher as inputs. After all the models output the event likelihood, we converted these likelihoods to probabilities and randomly selected the outcome of the pitch.

We also including functions in our simulation to determine baserunning and pitching changes. For baserunning, we only took into account runners advancing on balls hit into play and we went solely off of historical (2017-22) data. For example, if we simulated a fly out with runners on 2nd and 3rd and 1 out, we would look at the historical distribution of runs scored, outs made, and where the runners ended up in this exact situation, and randomly select an outcome based on the distribution. If given more time, we would have liked to incorporate baserunner speed and fielding strength.

For pitching changes, we made a function that selected whether a starting pitcher will be pulled based off of how often they were removed in that inning given they made it to the previous inning, and then which reliever will come in given that the pitcher was removed. We made several simplifications, namely that pitching changes only happen between innings and all relievers pitch exactly one inning. Which relief pitcher enters was determined based on how often they entered in that inning. For this function, only 2021-22 data was used to reflect current pitcher usage.

## Results
9/16/22
Yankees @ Brewers

Actual Score:
NYY 6 - MIL 7

WUSTL Mean Score:
NYY 4.72 - MIL 3.80

WUSTL Median Score:
NYY 4 - MIL 3

Win Percentages:
NYY 58.59% - MIL 41.41%



6/22/22
Giants @ Braves

Actual Score:
SFG 3 - ATL 4

WUSTL Mean Score:
SFG 4.01 - ATL 1.70

WUSTL Median Score:
SFG 4 - ATL 1

Win Percentages:
SFG 88.88% - ATL 11.12%

