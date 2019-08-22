# Load in functions from football_stan.R
source('R/football_stan.R')
source('R/predict.R')

# Loading necessary packages
library(plyr)

get_table = function(data) {
  # Obtains league table given a dataframe of results
  # Returns league table in a dataframe
  
  # Getting the team names in alphabetical order
  teams = as.character(sort(unique(c(as.vector(data$HomeTeam), as.vector(data$AwayTeam)))))
  
  # Creating empty league table
  # GP = games played
  # HW = home win, HD = home draw, HL = home loss, 
  # HF = home goals for, HA = home goals against, HGD = home goal difference
  # AW = away win, AD = away draw, AL = away loss,
  # AF = away goals for, AA = away goals against, AGD = away goal difference
  # W = win, D = draw, L = loss, GF = goals for, GA = goals against, GD = goal difference
  table = data.frame(Team = teams, GP = 0, HW = 0, HD = 0, HL = 0, HF = 0, HA = 0, HGD = 0,
                     AW = 0, AD = 0, AL = 0, AF = 0, AA = 0, AGD = 0, W = 0, D = 0, L = 0, 
                     GF = 0, GA = 0, GD = 0, Points = 0) 
  
  for (i in 1:nrow(data)) {
    # Updating the goal tallys
    # Adding goals scored by home team
    table[(which(teams == data[i,]$HomeTeam)), 'GF'] = table[(which(teams == data[i,]$HomeTeam)), 'GF'] + data[i,]$FTHG
    table[(which(teams == data[i,]$HomeTeam)), 'HF'] = table[(which(teams == data[i,]$HomeTeam)), 'HF'] + data[i,]$FTHG
    table[(which(teams == data[i,]$AwayTeam)), 'GA'] = table[(which(teams == data[i,]$AwayTeam)), 'GA'] + data[i,]$FTHG
    table[(which(teams == data[i,]$AwayTeam)), 'AA'] = table[(which(teams == data[i,]$AwayTeam)), 'AA'] + data[i,]$FTHG
    
    # Adding goals scored by away team
    table[(which(teams == data[i,]$AwayTeam)), 'GF'] = table[(which(teams == data[i,]$AwayTeam)), 'GF'] + data[i,]$FTAG
    table[(which(teams == data[i,]$AwayTeam)), 'AF'] = table[(which(teams == data[i,]$AwayTeam)), 'AF'] + data[i,]$FTAG
    table[(which(teams == data[i,]$HomeTeam)), 'GA'] = table[(which(teams == data[i,]$HomeTeam)), 'GA'] + data[i,]$FTAG
    table[(which(teams == data[i,]$HomeTeam)), 'HA'] = table[(which(teams == data[i,]$HomeTeam)), 'HA'] + data[i,]$FTAG
    
    # Updating goal difference for home team
    table[(which(teams == data[i,]$HomeTeam)), 'GD'] = table[(which(teams == data[i,]$HomeTeam)), 'GF'] - table[(which(teams == data[i,]$HomeTeam)), 'GA']
    table[(which(teams == data[i,]$HomeTeam)), 'HGD'] = table[(which(teams == data[i,]$HomeTeam)), 'HF'] - table[(which(teams == data[i,]$HomeTeam)), 'HA']
    
    # Updating goal difference for away team
    table[(which(teams == data[i,]$AwayTeam)), 'GD'] = table[(which(teams == data[i,]$AwayTeam)), 'GF'] - table[(which(teams == data[i,]$AwayTeam)), 'GA']
    table[(which(teams == data[i,]$AwayTeam)), 'AGD'] = table[(which(teams == data[i,]$AwayTeam)), 'AF'] - table[(which(teams == data[i,]$AwayTeam)), 'AA']
    
    # Update games played
    table[(which(teams == data[i,]$HomeTeam)), 'GP'] = table[(which(teams == data[i,]$HomeTeam)), 'GP'] + 1
    table[(which(teams == data[i,]$AwayTeam)), 'GP'] = table[(which(teams == data[i,]$AwayTeam)), 'GP'] + 1
    
    # Updating the point tallys and team records
    if (data[i,]$FTHG > data[i,]$FTAG) {
      # Update points
      table[(which(teams == data[i,]$HomeTeam)), 'Points'] = table[(which(teams == data[i,]$HomeTeam)), 'Points'] + 3
      
      # Update win / losses
      table[(which(teams == data[i,]$HomeTeam)), 'HW'] = table[(which(teams == data[i,]$HomeTeam)), 'HW'] + 1
      table[(which(teams == data[i,]$HomeTeam)), 'W'] = table[(which(teams == data[i,]$HomeTeam)), 'W'] + 1
      table[(which(teams == data[i,]$AwayTeam)), 'AL'] = table[(which(teams == data[i,]$AwayTeam)), 'AL'] + 1
      table[(which(teams == data[i,]$AwayTeam)), 'L'] = table[(which(teams == data[i,]$AwayTeam)), 'L'] + 1
    } else if (data[i,]$FTHG == data[i,]$FTAG) {
      # Update points
      table[(which(teams == data[i,]$HomeTeam)), 'Points'] = table[(which(teams == data[i,]$HomeTeam)), 'Points'] + 1
      table[(which(teams == data[i,]$AwayTeam)), 'Points'] = table[(which(teams == data[i,]$AwayTeam)), 'Points'] + 1
      
      # Update draws
      table[(which(teams == data[i,]$HomeTeam)), 'HD'] = table[(which(teams == data[i,]$HomeTeam)), 'HD'] + 1
      table[(which(teams == data[i,]$HomeTeam)), 'D'] = table[(which(teams == data[i,]$HomeTeam)), 'D'] + 1
      table[(which(teams == data[i,]$AwayTeam)), 'AD'] = table[(which(teams == data[i,]$AwayTeam)), 'AD'] + 1
      table[(which(teams == data[i,]$AwayTeam)), 'D'] = table[(which(teams == data[i,]$AwayTeam)), 'D'] + 1
    } else if (data[i,]$FTHG < data[i,]$FTAG) {
      # Update points
      table[(which(teams == data[i,]$AwayTeam)), 'Points'] = table[(which(teams == data[i,]$AwayTeam)), 'Points'] + 3
      
      # Update draws
      table[(which(teams == data[i,]$HomeTeam)), 'HL'] = table[(which(teams == data[i,]$HomeTeam)), 'HL'] + 1
      table[(which(teams == data[i,]$HomeTeam)), 'L'] = table[(which(teams == data[i,]$HomeTeam)), 'L'] + 1
      table[(which(teams == data[i,]$AwayTeam)), 'AW'] = table[(which(teams == data[i,]$AwayTeam)), 'AW'] + 1
      table[(which(teams == data[i,]$AwayTeam)), 'W'] = table[(which(teams == data[i,]$AwayTeam)), 'W'] + 1
    }
  }
  
  # Sorting the teams by total points and by goal difference
  table = table[order(-table$Points, -table$GD),]
  
  # Returing table
  return(table)
}

track_points_progress = function(data, results = T, start_values = matrix(data = rep(0, length(as.character(sort(unique(c(as.vector(data$HomeTeam), as.vector(data$AwayTeam))))))))) {
  # Obtains a dataframe that tracks the number of points that a team has accumulated over a period
  # Returns league table in a dataframe
  
  ### Arguments
  
  # data = dataframe of match outcomes
  # results = bool - if set to TRUE, indicates that the data that was passed is a set of final scoreline results,
  #                  if set to FALSE, indicates that the data that was passed was just a set of match outcomes (HW/D/AW)
  # start_values = can start off with inital set of points or by default starts with all teams on zero points
  
  # Getting the team names in alphabetical order
  teams = as.character(sort(unique(c(as.vector(data$HomeTeam), as.vector(data$AwayTeam)))))
  
  # Creating a data frame with each team as a column
  progress = matrix(data = start_values, ncol = 1, nrow = length(teams))
  rownames(progress) = teams
  
  # Need to make a data frame to count how many games each team has played
  # Then we know at what position in the matrix we want to add the new value of points
  GP = setNames(data.frame(matrix(data = 0, ncol = length(teams), nrow = 1)),teams)
  
  for (i in 1:nrow(data)) {
    # Updating the number of games played
    GP[,(which(teams == data[i,]$HomeTeam))] = GP[,(which(teams == data[i,]$HomeTeam))] + 1
    GP[,(which(teams == data[i,]$AwayTeam))] = GP[,(which(teams == data[i,]$AwayTeam))] + 1
    
    # Making sure that there is a row to add the next points tally for the teams
    if (!(ncol(progress) >= (GP[,(which(teams == data[i,]$HomeTeam))]+1))) {
      progress = cbind(progress, matrix(data=0, ncol = 1, nrow = length(teams)))
    }
    if (!(ncol(progress) >= (GP[,(which(teams == data[i,]$AwayTeam))]+1))) {
      progress = cbind(progress, matrix(data=0, ncol = 1, nrow = length(teams)))
    }
    
    # Adding to the points tally and adding this value to the next column in the matrix for the team
    if (results == T) {
      if (data[i,]$FTHG > data[i,]$FTAG) {
        progress[(which(teams == data[i,]$HomeTeam)), (GP[,(which(teams == data[i,]$HomeTeam))]+1)] = progress[(which(teams == data[i,]$HomeTeam)), 
                                                                                                               (GP[,(which(teams == data[i,]$HomeTeam))])] + 3
        progress[(which(teams == data[i,]$AwayTeam)), (GP[,(which(teams == data[i,]$AwayTeam))]+1)] = progress[(which(teams == data[i,]$AwayTeam)), 
                                                                                                               (GP[,(which(teams == data[i,]$AwayTeam))])]
      } else if (data[i,]$FTHG == data[i,]$FTAG) {
        progress[(which(teams == data[i,]$HomeTeam)), (GP[,(which(teams == data[i,]$HomeTeam))]+1)] = progress[(which(teams == data[i,]$HomeTeam)), 
                                                                                                               (GP[,(which(teams == data[i,]$HomeTeam))])] + 1
        progress[(which(teams == data[i,]$AwayTeam)), (GP[,(which(teams == data[i,]$AwayTeam))]+1)] = progress[(which(teams == data[i,]$AwayTeam)), 
                                                                                                               (GP[,(which(teams == data[i,]$AwayTeam))])] + 1
      } else if (data[i,]$FTHG < data[i,]$FTAG) {
        progress[(which(teams == data[i,]$HomeTeam)), (GP[,(which(teams == data[i,]$HomeTeam))]+1)] = progress[(which(teams == data[i,]$HomeTeam)), 
                                                                                                               (GP[,(which(teams == data[i,]$HomeTeam))])]
        progress[(which(teams == data[i,]$AwayTeam)), (GP[,(which(teams == data[i,]$AwayTeam))]+1)] = progress[(which(teams == data[i,]$AwayTeam)), 
                                                                                                               (GP[,(which(teams == data[i,]$AwayTeam))])] + 3
      }
    } else if (results == F) {
      if (data[i,]$Result == 'HW') {
        progress[(which(teams == data[i,]$HomeTeam)), (GP[,(which(teams == data[i,]$HomeTeam))]+1)] = progress[(which(teams == data[i,]$HomeTeam)), 
                                                                                                               (GP[,(which(teams == data[i,]$HomeTeam))])] + 3
        progress[(which(teams == data[i,]$AwayTeam)), (GP[,(which(teams == data[i,]$AwayTeam))]+1)] = progress[(which(teams == data[i,]$AwayTeam)), 
                                                                                                               (GP[,(which(teams == data[i,]$AwayTeam))])]
      } else if (data[i,]$Result == 'D') {
        progress[(which(teams == data[i,]$HomeTeam)), (GP[,(which(teams == data[i,]$HomeTeam))]+1)] = progress[(which(teams == data[i,]$HomeTeam)), 
                                                                                                               (GP[,(which(teams == data[i,]$HomeTeam))])] + 1
        progress[(which(teams == data[i,]$AwayTeam)), (GP[,(which(teams == data[i,]$AwayTeam))]+1)] = progress[(which(teams == data[i,]$AwayTeam)), 
                                                                                                               (GP[,(which(teams == data[i,]$AwayTeam))])] + 1
      } else if (data[i,]$Result == 'AW') {
        progress[(which(teams == data[i,]$HomeTeam)), (GP[,(which(teams == data[i,]$HomeTeam))]+1)] = progress[(which(teams == data[i,]$HomeTeam)), 
                                                                                                               (GP[,(which(teams == data[i,]$HomeTeam))])]
        progress[(which(teams == data[i,]$AwayTeam)), (GP[,(which(teams == data[i,]$AwayTeam))]+1)] = progress[(which(teams == data[i,]$AwayTeam)), 
                                                                                                               (GP[,(which(teams == data[i,]$AwayTeam))])] + 3
      }
    }
  }
  
  # Return points progress matrix
  return(progress)
}

extract_parameters = function(model_fit, model) {
  # Obtains the mean vector and covariance matrix for the posterior samples
  # Returns the mean vector and covariance matrix in a list
  
  ### Arguments:
  
  # model_fit = a Stanfit object of the fitted Stan model
  # model = define which model was implemented - "NB" for Negative Binomial, 
  #                                              "BB" for Baio & Blangiardo
  
  if (model == 'NB') {
    # Obtaining the posterior samples for the parameters 
    parameter_samples = as.matrix(model_fit, pars=c('mu_home_att', 'mu_away_att', 'mu_home_def', 'mu_away_def',
                                                    'sigma2_att', 'sigma2_def', 'phi_home', 'phi_away',
                                                    'home_att_raw', 'away_att_raw', 
                                                    'home_def_raw', 'away_def_raw'))
    
    # And calculating mean vector and covariance matrix
    mu = colMeans(parameter_samples)
    Sigma = cov(parameter_samples)
    post_est = list('mu' = mu, 'Sigma' = Sigma)
  } else if (model == 'BB') {
    # Obtaining the posterior samples for the parameters 
    parameter_samples = as.matrix(model_fit, pars=c('home', 'mu_att', 'mu_def', 
                                                    'tau_att', 'tau_def', 'att_free', 'def_free'))
    
    # Calculating the mean vector and precision matrix
    mu = colMeans(parameter_samples)
    precision = solve(cov(parameter_samples))
    post_est = list('mu' = mu, 'Precision' = precision)
  }

  # Return in a list
  return(post_est)
}

update_model = function(stan_file, model_fit, new_data, original_data, model, iterations, chains) {
  # Fit an updated Stan model to the new data and return the output
  
  ### Arguments: 
  
  # stan_file = .stan file containing the model
  # model_fit = a Stanfit object of the fitted Stan model
  # new_data = data containing the new data of football scores in a data frame
  # original_data = data that was used to fit the original Stan model - needed incase the new data set does not have all teams playing
  # model = define which model was implemented - "NB" for Negative Binomial, 
  #                                              "BB" for Baio & Blangiardo
  # iterations = positive integer specifying the number of iterations for each chain including warmup
  # chains = positive integer specifying the number of Markov Chains
  
  # Building Stan data object
  teams = as.character(sort(unique(c(as.vector(original_data$HomeTeam), as.vector(original_data$AwayTeam)))))
  if (model == 'NB') {
    # Obtaining mean vector and covariance matrix for posterior parameters
    posterior_data = extract_parameters(model_fit, model = 'NB')
    training_data = list(nteams = length(teams), 
                         ngames = nrow(new_data), 
                         home_team = new_data$HomeIndex,
                         away_team = new_data$AwayIndex,
                         home_goals = new_data$FTHG,
                         away_goals = new_data$FTAG, 
                         mu = posterior_data$mu,
                         Sigma = posterior_data$Sigma)
  } else if (model == 'BB') {
    # Obtaining mean vector and covariance matrix for posterior parameters
    posterior_data = extract_parameters(model_fit, model = 'BB')
    training_data = list(nteams = length(teams), 
                         ngames = nrow(new_data), 
                         home_team = new_data$HomeIndex,
                         away_team = new_data$AwayIndex,
                         home_goals = new_data$FTHG,
                         away_goals = new_data$FTAG, 
                         mu = posterior_data$mu,
                         precision = posterior_data$Precision)
  }
  
  # Fitting a stan model to our training data set
  fitting_model = stan_model(file = stan_file)
  model = sampling(object = fitting_model, 
                   data = training_data, 
                   iter = iterations,
                   chains = chains,
                   control = list(max_treedepth = 15, adapt_delta = 0.99))
  
  # Returning the fitted model
  return(model)
}

simulate_table = function(stan_file, updating_stan_file, data, remaining_games, model, number_of_simulations) {
  # Simulates many league tables to obtain estimate for probabilities of league positions
  
  ### Arguments:
  
  # stan_file = .stan file containing the model we wish to fit to the data
  # updating_stan_file = .stan file containing the code to update the posterior estimates with new data
  # data = data containing football scores in a data frame
  # remaining_games = remaining games for the season wish to predict
  # model = define which model was implemented - "NB" for Negative Binomial, 
  #                                              "BB" for Baio & Blangiardo
  # number_of_simulatioms = number of league tables we wish to generate
  
  # Fit initial model to the data
  initial_fit = fit_model(stan_file, data, iterations = 5000, chains = 4)
  
  # Create list for the simulated league tables to be added in
  simulated_tables = list()
  
  # Predicting the remaining games over and over again
  # Run simulations 'hot' so each teams ratings change and vary over the simulated season
  for (sim in 1:number_of_simulations) {
    # ptm = proc.time() # calcuting time per simulation
    current_fit = initial_fit
    total_number_of_games = nrow(data) + nrow(remaining_games)
    season_results = data
    games_to_predict = remaining_games
    
    # Set loop to carry on while there are still games to and predict
    while(!(nrow(season_results)==total_number_of_games)){
      # To obtain the next test set, we go through each game carry on until we find a repeat in team
      # If we add only 1 team or 0 teams to the vector of teams playing, then we have found a repeating team
      teams = NULL; additions = 2; i=1
      while (additions == 2) {
        additions = 0
        if (!(is.element(games_to_predict[i,]$HomeIndex, teams))) {
          teams = c(teams, games_to_predict[i,]$HomeIndex)
          additions = additions + 1
        } 
        if (!(is.element(games_to_predict[i,]$AwayIndex, teams))) {
          teams = c(teams, games_to_predict[i,]$AwayIndex)
          additions = additions + 1
        }
        if (additions == 2) {
          i = i + 1
        }
      }
      
      # next_round is the next round of games to predict
      # games_to_predict now is the dataframe containing the remaining games to predict after this round
      next_round = games_to_predict[1:i-1,]
      games_to_predict = games_to_predict[i:nrow(games_to_predict),]
      
      # print('Original games')
      # print(next_round)
      
      game_predictions = NULL
      for (i in 1:nrow(next_round)) {
        # Predicting the next round of games by running one simulation of a game
        simulated_game = simulate_game(model_fit = current_fit, 
                                       data = data, 
                                       home_team = next_round[i,]$HomeTeam, 
                                       away_team = next_round[i,]$AwayTeam, 
                                       model = model, 
                                       number_of_simulations = 1)
        
        # print(predict_game(model_fit = current_fit,
        #                    data = data,
        #                    home_team = next_round[i,]$HomeTeam, 
        #                    away_team = next_round[i,]$AwayTeam, 
        #                    model = model))
        
        # Add this simulated game to game_predictions to use to update the model
        game = data.frame('Date' = next_round[i,]$Date,
                          'HomeTeam' = next_round[i,]$HomeTeam,
                          'AwayTeam' = next_round[i,]$AwayTeam,
                          'FTHG' = simulated_game[,1][1],
                          'FTAG' = simulated_game[,2][1],
                          'HomeIndex' = next_round[i,]$HomeIndex,
                          'AwayIndex' = next_round[i,]$AwayIndex)
        
        game_predictions = rbind(game_predictions, game)
      }
      
      # print('Game predictions')
      # print(game_predictions)
      # 
      # print('Games left to predict')
      # print(games_to_predict)
      
      # Update our parameters after these games to predict future games
      current_fit = update_model(stan_file = updating_stan_file, 
                                 model_fit = current_fit, 
                                 new_data = game_predictions, 
                                 original_data = data, 
                                 model = 'NB', 
                                 iterations = 400, 
                                 chains = 4)
      
      # Updating the season_results dataframe
      # If the number of games in this season_results dataframe equals total_number_of_games, loop ends
      season_results = plyr::rbind.fill(season_results, game_predictions)
      
      # Print progress of the algorithm
      print(paste('There are', (total_number_of_games - nrow(season_results)),
                  'games left to predict for simulation number', sim))
    }
    
    # We have now predicted the games for the remainder of the season
    # Simualted results are added to the season_results data frame, which includes past games
    # Calculating league table from these predictions
    # print('Season results')
    # print(season_results)
    simulated_tables[[sim]] = get_table(season_results)
    
    # Print progress of the algroithm
    # print(paste('Simulation', sim, 'time:', proc.time() - ptm))
    print(paste(sim, '/', number_of_simulations, 'simulations completed.'))
    
    # print('Simulated tables')
    # print(simulated_tables)
  }
  
  # Return list of simulated tables
  return(simulated_tables)
}

PL1718 = read_data('Data/E01718.csv') 
first = PL1718[1:180,]
end = PL1718[181:nrow(PL1718),]

test = simulate_table(stan_file = 'Stan/model_chan.stan',
                      updating_stan_file = 'Stan/model_chan_update.stan',
                      data = first,
                      remaining_games = end,
                      model = 'NB',
                      number_of_simulations = 1000)