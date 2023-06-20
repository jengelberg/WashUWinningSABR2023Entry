# WashUWinningSABR2023Entry
This is the code behind the winning presentation given by Washington University in St. Louis at the 2023 Diamond Dollars Case Competition regarding how correcting all umpire missed calls would impact the outcome of a specific game.

To Run 1 Simulation:
1) Download the zip file located in this google drive https://drive.google.com/drive/folders/10VO4m385z2CbjsdbpyiqmRgHVrJH2nim?usp=drive_link
2) Unzip the file
3) In Initialization.R, switch line 12 to reflect your working directory. The last layer should be the unzipped folder.
4) Run Initialization.R in its entirety (and specify which game to simulate in lines 15-16)
5) Run Pitch Outcome Models.R in its entirety
6) Run Simulation.R to get the result of 1 simulation. The score will be displayed "road_score home_score". You can run line 423 again to get additional simulations of the same game.
7) If you would like to see our stability analysis results, run the Stability Analysis.R file for whichever outcomes you are interested in.

Note: If you would like to switch games, toggle the option in Initialization.R (lines 15-16), re-run the Game Level Initialization, then re-run line 423

