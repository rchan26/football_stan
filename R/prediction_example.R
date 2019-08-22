### Example code for implementing the Negative Binomial model

# Load in functions from football_stan.R and predict.R
source('R/football_stan.R')
source('R/predict.R')

# Reading in the example data set found in Data folder
PL1718 = read_data('Data/E01718.csv') 

# Running Stan model
ptm <- proc.time()
NB = fit_model(stan_file = 'Stan/model_chan.stan', data = PL1718, iterations = 10000, chains = 4) 
proc.time() - ptm

# Suppose we want to predict a future match between Manchester United (home) vs. Arsenal (away)
outcome_prob = predict_game(model_fit = NB, data = PL1718, home_team = 'Man United', away_team = 'Arsenal', model = 'NB')
print(outcome_prob)

# If we wanted the probabilities for each scoreline
scoreline_prob = predict_game(model_fit = NB, data = PL1718, home_team = 'Man United', away_team = 'Arsenal', model = 'NB', score_lines = T)
print(scoreline_prob)
