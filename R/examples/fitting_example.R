### Example code for implementing the Negative Binomial model

# Load in functions from football_stan.R
source('../functions/R/football_stan.R')

# Reading in the example data set found in Data folder
PL1718 = read_data('../../Data/E01718.csv') 

# Running Stan model
NB = fit_model(stan_file = '../../Stan/model_chan.stan', data = PL1718, iterations = 10000, chains = 4) 

# Obtaining parameter plots
plot_parameters(model_fit = NB, data = PL1718, model = 'NB') # figure 1
plot_parameters_2d(model_fit = NB, data = PL1718, model = 'NB') # figure 2

# Might want to add colour to the labels in the 2d plot to correspond to team colours
new_cols = c('red','firebrick4','cyan', 'brown','darkblue','blue2','darkblue','cyan','blue','firebrick',
             'deepskyblue','firebrick1','black','red','red','gray0','blue4', 'gold1', 'darkblue' ,'brown4')

plot_parameters_2d(model_fit = NB, data = PL1718, model = 'NB', point_est = 'mean', cols = new_cols, overall =  T) # figure 3

# Can use the shinystan package to explore the Stanfit object
shinystan::launch_shinystan(NB)

