### Example code for implementing the Negative Binomial model to predict a league table

# Load in functions from football_stan.R
source('R/predict_table.R')

# Reading in the example data set found in Data folder
PL1718 = read_data('Data/E01718.csv') 

# Split the data in half
first = PL1718[1:180,]
end = PL1718[181:nrow(PL1718),]

# Use the first half of the season to try predict a league table for the remainder of the season
# Note: this can take very long for large number of league simulations
test = simulate_table(stan_file = 'Stan/model_chan.stan',
                      updating_stan_file = 'Stan/model_chan_update.stan',
                      data = first,
                      remaining_games = end,
                      model = 'NB',
                      number_of_league_simulations = 10000,
                      number_of_game_simulations = 500)

# Obtains the number of points each team got and their final league positions in each simulation
summary = simulation_summary(test, PL1718)

# Obtain probability estimates for each team to finish as champions
predict_league_position(test, PL1718, 1)

# Plotting histograms of points obtained for each team
points_histogram(test, PL1718, xlim = c(20, 100))
