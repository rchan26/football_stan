# Load in functions from football_stan.R
source('R/football_stan.R')
source('R/predict.R')

# Loading necessary packages
library(plyr)

get_table_detailed = function(data) {
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
      
      # Update wins / losses
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
      
      # Update wins / losses
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

get_table_simple = function(data) {
  # Obtains league table given a dataframe of results
  # Returns league table in a dataframe
  
  # Getting the team names in alphabetical order
  teams = as.character(sort(unique(c(as.vector(data$HomeTeam), as.vector(data$AwayTeam)))))
  
  # Creating empty league table
  # GP = games played
  # W = win, D = draw, L = loss, GF = goals for, GA = goals against, GD = goal difference
  table = data.frame(Team = teams, GP = 0, W = 0, D = 0, L = 0, GF = 0, GA = 0, GD = 0, Points = 0) 
  
  for (i in 1:nrow(data)) {
    # Updating the goal tallys
    # Adding goals scored by home team
    table[(which(teams == data[i,]$HomeTeam)), 'GF'] = table[(which(teams == data[i,]$HomeTeam)), 'GF'] + data[i,]$FTHG
    table[(which(teams == data[i,]$AwayTeam)), 'GA'] = table[(which(teams == data[i,]$AwayTeam)), 'GA'] + data[i,]$FTHG
    
    # Adding goals scored by away team
    table[(which(teams == data[i,]$AwayTeam)), 'GF'] = table[(which(teams == data[i,]$AwayTeam)), 'GF'] + data[i,]$FTAG
    table[(which(teams == data[i,]$HomeTeam)), 'GA'] = table[(which(teams == data[i,]$HomeTeam)), 'GA'] + data[i,]$FTAG
    
    # Updating goal difference for home team
    table[(which(teams == data[i,]$HomeTeam)), 'GD'] = table[(which(teams == data[i,]$HomeTeam)), 'GF'] - table[(which(teams == data[i,]$HomeTeam)), 'GA']
    
    # Updating goal difference for away team
    table[(which(teams == data[i,]$AwayTeam)), 'GD'] = table[(which(teams == data[i,]$AwayTeam)), 'GF'] - table[(which(teams == data[i,]$AwayTeam)), 'GA']
    
    # Update games played
    table[(which(teams == data[i,]$HomeTeam)), 'GP'] = table[(which(teams == data[i,]$HomeTeam)), 'GP'] + 1
    table[(which(teams == data[i,]$AwayTeam)), 'GP'] = table[(which(teams == data[i,]$AwayTeam)), 'GP'] + 1
    
    # Updating the point tallys and team records
    if (data[i,]$FTHG > data[i,]$FTAG) {
      # Update points
      table[(which(teams == data[i,]$HomeTeam)), 'Points'] = table[(which(teams == data[i,]$HomeTeam)), 'Points'] + 3
      
      # Update wins / losses
      table[(which(teams == data[i,]$HomeTeam)), 'W'] = table[(which(teams == data[i,]$HomeTeam)), 'W'] + 1
      table[(which(teams == data[i,]$AwayTeam)), 'L'] = table[(which(teams == data[i,]$AwayTeam)), 'L'] + 1
    } else if (data[i,]$FTHG == data[i,]$FTAG) {
      # Update points
      table[(which(teams == data[i,]$HomeTeam)), 'Points'] = table[(which(teams == data[i,]$HomeTeam)), 'Points'] + 1
      table[(which(teams == data[i,]$AwayTeam)), 'Points'] = table[(which(teams == data[i,]$AwayTeam)), 'Points'] + 1
      
      # Update draws
      table[(which(teams == data[i,]$HomeTeam)), 'D'] = table[(which(teams == data[i,]$HomeTeam)), 'D'] + 1
      table[(which(teams == data[i,]$AwayTeam)), 'D'] = table[(which(teams == data[i,]$AwayTeam)), 'D'] + 1
    } else if (data[i,]$FTHG < data[i,]$FTAG) {
      # Update points
      table[(which(teams == data[i,]$AwayTeam)), 'Points'] = table[(which(teams == data[i,]$AwayTeam)), 'Points'] + 3
      
      # Update wins / losses
      table[(which(teams == data[i,]$HomeTeam)), 'L'] = table[(which(teams == data[i,]$HomeTeam)), 'L'] + 1
      table[(which(teams == data[i,]$AwayTeam)), 'W'] = table[(which(teams == data[i,]$AwayTeam)), 'W'] + 1
    }
  }
  
  # Sorting the teams by total points and by goal difference
  table = table[order(-table$Points, -table$GD, -table$GF),]
  
  # Returing table
  return(table)
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

simulate_table = function(stan_file, updating_stan_file, data, remaining_games, model, number_of_league_simulations, number_of_game_simulations) {
  # Simulates many league tables to obtain estimate for probabilities of league positions
  
  ### Arguments:
  
  # stan_file = .stan file containing the model we wish to fit to the data
  # updating_stan_file = .stan file containing the code to update the posterior estimates with new data
  # data = data containing football scores in a data frame
  # remaining_games = remaining games for the season wish to predict
  # model = define which model was implemented - "NB" for Negative Binomial, 
  #                                              "BB" for Baio & Blangiardo
  # number_of_league_simulatioms = number of league tables we wish to generate
  # number_of_game_simulations = after each fitting of a model, we simulate number_of_game_simulation games
  #                            - MUST BE ABLE TO DIVIDE number_of_league_simulations
  
  # Fit initial model to the data
  initial_fit = fit_model(stan_file, data, iterations = 5000, chains = 4)
  
  # Create list for the simulated league tables to be added in
  simulated_tables = list()
  
  # Predicting the remaining games over and over again
  # Run simulations 'hot' so each teams ratings change and vary over the simulated season
  for (sim in 1:(number_of_league_simulations/number_of_game_simulations)) {
    ptm = proc.time() # calcuting time per simulation
    current_fit = initial_fit
    total_number_of_games = nrow(data) + nrow(remaining_games)
    past_results = data
    games_to_predict = remaining_games
    
    season_results = list()
    for (i in 1:500) {
      season_results[[i]] = past_results
    }
    
    # Set loop to carry on while there are still games to and predict
    while(!(nrow(season_results[[1]])==total_number_of_games)){
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
      
      game_predictions = list()
      for (i in 1:nrow(next_round)) {
        # Predicting the next round of games by running one simulation of a game
        simulated_games = simulate_game(model_fit = current_fit, 
                                       data = data, 
                                       home_team = next_round[i,]$HomeTeam, 
                                       away_team = next_round[i,]$AwayTeam, 
                                       model = model, 
                                       number_of_simulations = number_of_game_simulations)
        
        # Add this simulated game to game_predictions to use to update the model
        for (game in 1:number_of_game_simulations) {
          simed_game = data.frame('Date' = next_round[i,]$Date,
                            'HomeTeam' = next_round[i,]$HomeTeam,
                            'AwayTeam' = next_round[i,]$AwayTeam,
                            'FTHG' = simulated_games[,1][game],
                            'FTAG' = simulated_games[,2][game],
                            'HomeIndex' = next_round[i,]$HomeIndex,
                            'AwayIndex' = next_round[i,]$AwayIndex)
          
          # Updating the season_results dataframe
          season_results[[game]] = plyr::rbind.fill(season_results[[game]], simed_game)
          
          if (class(try({rbind(game_predictions[[game]], simed_game)}, silent = TRUE)) == 'try-error') {
            game_predictions[[game]] = simed_game
          } else {
            game_predictions[[game]] = rbind(game_predictions[[game]], simed_game)
          }
        }
      }
      
      # Update our parameters after these games to predict future games
      current_fit = update_model(stan_file = updating_stan_file,
                                 model_fit = current_fit,
                                 new_data = game_predictions[[sample(500, 1)]],
                                 original_data = data,
                                 model = 'NB',
                                 iterations = 450,
                                 chains = 4)
      
      # Print progress of the algorithm
      print(paste('There are', (total_number_of_games - nrow(season_results[[1]])),
                  'games left to predict for simulation number', sim))
    }
    
    # We have now predicted the games for the remainder of the season
    # Simulatted results are added to the season_results data frame, which includes past games
    # Calculating league table from these predictions
    for (i in 1:number_of_game_simulations) {
      simulated_tables[[(i + ((sim-1)*number_of_game_simulations))]] = get_table_simple(season_results[[i]])
      print(paste((i + ((sim-1)*number_of_game_simulations)), '/', number_of_league_simulations, 'simulations completed.'))
    }
    
    # Print progress of the algroithm
    print(paste('Simulation', sim, 'time:', (proc.time() - ptm)[3]))
  }
  
  # Return list of simulated tables
  return(simulated_tables)
}

simulation_summary = function(simulated_tables, data) {
  # Obtains summary of the simulated tables
  # Returns a matrix which records the final league position in each simulated season and the points obtained
  
  ### Arguments:
  
  # simulated_tables = list of simulated league tables
  # data = data containing football scores in a data frame
  
  teams = as.character(sort(unique(c(as.vector(data$HomeTeam), as.vector(data$AwayTeam)))))

  # Creating a matrix to record the final league position in each simulated table
  final_position = matrix(data = 0, ncol = length(teams), nrow = length(simulated_tables))
  colnames(final_position) = teams
  
  # Creating a matrix to record the points obtained in each simulated table
  points_obtained = matrix(data = 0, ncol = length(teams), nrow = length(simulated_tables))
  colnames(points_obtained) = teams
  
  for (table in 1:length(simulated_tables)) {
    for (team in 1:length(teams)) {
      final_position[table, team] = which(simulated_tables[[table]]$Team == teams[team])
      points_obtained[table, team] = simulated_tables[[table]]$Points[final_position[table, team]]
    }
  }
  
  # Return in a list
  summary = list('points' = points_obtained, 'final_position' = final_position)
  return(summary)
}

predict_league_position = function(simulated_tables, data, position) {
  # Uses the simulated league tables to obtain probabilities of each team finishing in a specified position
  # Counts the number of times a team finished in a specified position in the simulated tables
  
  ### Arguments
  
  # simulated_tables = list of simulated league tables
  # data = data containing football scores in a data frame
  # position = specified final league position
  
  teams = as.character(sort(unique(c(as.vector(data$HomeTeam), as.vector(data$AwayTeam)))))
  summary = simulation_summary(simulated_tables, data)
  
  position_predictions = data.frame('Team' = teams, 'Probability' = rep(0, length(teams)))
  for (team in 1:length(teams)) {
    position_predictions$Probability[team] = sum(summary$final_position[,teams[team]] == position) / length(simulated_tables)
  }
  
  # Returns in a datafrrame 
  return(position_predictions[order(-position_predictions$Probability),])
}

points_histogram = function(simulated_tables, data, xlim = c(0,100)) {
  # Plots the histogram of the points obtained in the simulated tables
  
  ### Arguments
  
  # simulated_tables = list of simulated league tables
  # data = data containing football scores in a data frame
  # xlim = the range of x values for the histogram plot - set c(0, 100) by default to compare teams easily
  
  teams = as.character(sort(unique(c(as.vector(data$HomeTeam), as.vector(data$AwayTeam)))))
  summary = simulation_summary(simulated_tables, data)
  
  for (team in 1:length(teams)) {
    hist(summary$points[,teams[team]], breaks = length(simulated_tables), xlim = xlim,
         xlab = 'Points', main = paste('Histogram of points for', teams[team]))
  }
}

##########################################################################################################

# Following functions is useful to assessing model performance for predicting games over the course of the season
# Not to be used to predict league tables

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

predict_table_assessment = function(stan_file, data, model, minimum_games) {
  # Obtains a league table from the predictions made throughout the season
  # Returns league table in a dataframe and the predicted points progression for each team in a list
  
  # Splitting the data set to initialise the training and test sets
  total_number_of_games = nrow(data)
  train_set = data[1:minimum_games,]
  test_set = data[(minimum_games+1):nrow(data),]
  data = data[(minimum_games+1):nrow(data),]
  
  # Getting the current points progress of train set
  points_progress = track_points_progress(observed_games)
  
  # Need to predict scores for the rest of the data set
  game_predictions = NULL
  
  # Set loop to carry on while there are still games to and predict
  while(!(nrow(train_set)==total_number_of_games)){
    # To obtain the next test set, we go through each game carry on until we find a repeat in team
    # If we add only 1 team or 0 teams to the vector of teams playing, then we have found a repeating team
    teams = NULL; additions = 2; i=1
    while (additions == 2) {
      additions = 0
      if (!(is.element(data[i,]$HomeIndex, teams))) {
        teams = c(teams, data[i,]$HomeIndex)
        additions = additions + 1
      } 
      if (!(is.element(data[i,]$AwayIndex, teams))) {
        teams = c(teams, data[i,]$AwayIndex)
        additions = additions + 1
      }
      if (additions == 2) {
        i = i + 1
      }
    }
    
    # Test_set now becomes the data frame with the games where no team plays twice in
    # Re-label the remaining data as the data minus the train_set games
    test_set = data[1:i-1,]
    data = data[i:nrow(data),]
    
    # Building Stan data object for 
    teams = as.character(sort(unique(c(as.vector(data$HomeTeam), as.vector(data$AwayTeam)))))
    training_data = list(nteams = length(teams), 
                         ngames = nrow(data), 
                         home_team = data$HomeIndex,
                         away_team = data$AwayIndex,
                         home_goals = data$FTHG,
                         away_goals = data$FTAG)
    
    # Fitting a stan model to our training data set
    fitting_model = stan_model(file = stan_file)
    model = sampling(object = fitting_model, 
                     data = training_data, 
                     iter = 5000,
                     chains = 4,
                     control = list(max_treedepth = 15, adapt_delta = 0.99))
    
    for (i in 1:nrow(test_set)) {
      # Obtain outcome prediction for scoreline in the game
      scores = predict_game(model_fit = train,
                            data = train_set, 
                            home_team = test_set[i,]$HomeTeam,
                            away_team = test_set[i,]$AwayTeam,
                            model = model, 
                            score_lines = T)
      
      # Record the score prediction in the data frame predictions
      game = data.frame('Date' = test_set[i,]$Date,
                        'HomeTeam' = test_set[i,]$HomeTeam,
                        'AwayTeam' = test_set[i,]$AwayTeam,
                        'FTHG' = getmode(score$FTHG),
                        'FTAG' = getmode(score$FTAG),
                        'B365H' = test_set[i,]$B365H,
                        'B365D' = test_set[i,]$B365D,
                        'B365A' = test_set[i,]$B365A,
                        'HomeIndex' = test_set[i,]$HomeIndex,
                        'AwayIndex' = test_set[i,]$AwayIndex)
      
      # Combinine this with predictions
      game_predictions = rbind(game_predictions, game)
    }
    # Print progress of the algorithm
    print(paste('There are ', nrow(data), ' games left in the data set'))
    
    # Train_set now becomes all the data and games that have happened before the test_set games
    train_set = rbind(train_set, data[1:i-1,])
  }
  
  # Create table with the new predicted outcomes
  predicted_outcomes = rbind(observed_games, game_predictions)
  predicted_table = get_table(predicted_outcomes)
  
  # Obtain complete predicted progress for the whole data set
  predicted_progress = track_points_progress(data = game_predictions, start_values = points_progress[,ncol(points_progress)])
  overall_progress = cbind(points_progress[,1:(ncol(points_progress)-1)], predicted_progress)
  
  # Return in list form
  predictions = list('predicted_progress' = overall_progress, 'predicted_table' = predicted_table) 
  return(predictions)
}
