scores_probability_table = function(predicted_scores, home_team, away_team, rounding) {
  # Obtains a scoreline probability table by assuming independence and multiplying posterior probability of goals scored
  # Returns list containing: data frame with number of goals scored by the home team along the columns and 
  #                          number of goals scored along the rows 
  
  ### Arguments:
  
  # predicted_scores - data containing frequency tables of the number of goals scored in the predicted games in a list form
  # home_team = specified home team 
  # away_team = specified away team
  # rounding - number of significant figures desired
  
  A = c(); B = c()
  for (i in 1:6) {
    A[i] = predicted_scores[[1]][i]/sum(predicted_scores[[1]])
    B[i] = predicted_scores[[2]][i]/sum(predicted_scores[[1]])
    
    A[7] = 1 - sum(A[1:6])
    B[7] = 1 - sum(B[1:6])
    name = c("0","1","2","3","4","5", "6+")
    
    C = data.frame(row.names=name)
    for(i in 1:7) {
      for(j in 1:7) {
        C[i,j] = A[j]*B[i] 
      }
    }
    
    colnames(C) = name
  }
  
  # Return predicted scores along with the home and away team for reference
  predictions = list('HomeTeam' = home_team, 'AwayTeam' = away_team, 'score_probabilities' = round(C, rounding))
  return(predictions)
}

predict_game = function(model_fit, data, home_team, away_team, model, score_lines = F) {
  # Predicts a game between specified teams (either match outcome probabilities or predicts scorelines)
  
  ### Arguments:
  
  # model_fit = a Stanfit object of the fitted Stan model
  # data = data containing football score_lines in a data frame
  # home_team = specified home team (team spelling much match that given by football-data.co.uk)
  # away_team = specified away team (team spelling much match that given by football-data.co.uk)
  # model = define which model was implemented - "NB" for Negative Binomial, 
  #                                              "PO" for Poisson, 
  #                                              "BB" for Baio & Blangiardo
  # score_lines = bool - if set to TRUE, will return predicted scorelines or returns match outcome predictions

  teams = sort(as.character(unique(c(as.vector(data$HomeTeam), as.vector(data$AwayTeam)))))
  
  # Checking that the teams are spelt correctly
  if (!(home_team %in% teams) | !(away_team %in% teams)) {
    print('Team specified is not in the dataset.')
    print(paste('Game: ', home_team, 'vs.', away_team, 'not predicted'))
    return(NULL)
  }
  
  # Getting index locations for teams
  home_index = which(teams == home_team)
  away_index = which(teams == away_team)
  
  list_of_draws = extract(model_fit)
  number_of_samples = length(list_of_draws$lp__)
  
  if (model == 'NB') {
    # Extracting the samples that we need
    home_att = list_of_draws$home_att[,home_index]
    home_def = list_of_draws$home_def[,home_index]
    away_att = list_of_draws$away_att[,away_index]
    away_def = list_of_draws$away_def[,away_index]
    size_home = list_of_draws$phi_home
    size_away = list_of_draws$phi_away
    
    # Creating the log_mu parameters
    log_mu1 = home_att + away_def
    log_mu2 = home_def + away_att
    
    # Simulating from a negative binomial distribution to obtain predictive distribution
    y1 = NULL; y2 = NULL
    for (i in 1:number_of_samples) {
      y1[i] = rnbinom(1, size = size_home, mu = exp(log_mu1[i]))
      y2[i] = rnbinom(1, size = size_away, mu = exp(log_mu2[i]))
    }
  } else if (model == 'PO') {
    # Extracting the samples that we need
    home_att = list_of_draws$home_att[,home_index]
    home_def = list_of_draws$home_def[,home_index]
    away_att = list_of_draws$away_att[,away_index]
    away_def = list_of_draws$away_def[,away_index]
    
    # Creating the log_mu parameters
    log_mu1 = home_att + away_def
    log_mu2 = home_def + away_att
    
    # Simulating from a negative binomial distribution to obtain predictive distribution
    y1 = NULL; y2 = NULL
    for (i in 1:number_of_samples) {
      y1[i] = rpois(1, lambda = exp(log_mu1[i]))
      y2[i] = rpois(1, lambda = exp(log_mu2[i]))
    }
  } else if (model == 'BB') {
    # Extracting the samples that we need
    home = list_of_draws$home
    att_home = list_of_draws$att[,home_index]
    att_away = list_of_draws$att[,away_index]
    def_home = list_of_draws$def[,home_index]
    def_away = list_of_draws$def[,away_index]
    
    # Creating the log_theta parameters
    log_theta1 = home + att_home + def_away
    log_theta2 = att_away + def_home
    
    # Simulating from a Poisson distribution to obtain predictive distribution
    y1 = NULL; y2 = NULL
    for (i in 1:number_of_samples) {
      y1[i] = rpois(1, lambda = exp(log_theta1[i]))
      y2[i] = rpois(1, lambda = exp(log_theta2[i]))
    }
  } 
  
  if (score_lines == T) {
    # Creating a list that has the table for the simulated goals scored
    predicted_scores = list(table(y1), table(y2))
    scores = scores_probability_table(predicted_scores, home_team, away_team, rounding = 3)
    
    # Return the predicted score probabilities
    return(scores)
  } else {
    # Calculating the estimated probabilities of events in data frame format
    outcome_probabilities = data.frame(sum(y1 > y2)/number_of_samples,
                                       sum(y1 == y2)/number_of_samples,
                                       sum(y1 < y2)/number_of_samples)
    colnames(outcome_probabilities) = c(home_team, 'Draw', away_team)
    
    # Return data frame of probabilities for the match outcomes
    return(outcome_probabilities)
  }
}

simulate_game = function(model_fit, data, home_team, away_team, model, number_of_simulations = 1) {
  # Predicts a game between specified teams (either match outcome probabilities or predicts scorelines)
  
  ### Arguments:
  
  # model_fit = a Stanfit object of the fitted Stan model
  # data = data containing football score_lines in a data frame
  # home_team = specified home team (team spelling much match that given by football-data.co.uk)
  # away_team = specified away team (team spelling much match that given by football-data.co.uk)
  # model = define which model was implemented - "NB" for Negative Binomial, 
  #                                              "PO" for Poisson, 
  #                                              "BB" for Baio & Blangiardo
  # number_of_simulatios = number of games we wish to simulate between the two specified teams - set to 1 by default
  
  teams = sort(as.character(unique(c(as.vector(data$HomeTeam), as.vector(data$AwayTeam)))))
  
  # Checking that the teams are spelt correctly
  if (!(home_team %in% teams) | !(away_team %in% teams)) {
    print('Team specified is not in the dataset.')
    print(paste('Game: ', home_team, 'vs.', away_team, 'not simulated'))
    return(NULL)
  }
  
  # Getting index locations for teams
  home_index = which(teams == home_team)
  away_index = which(teams == away_team)
  
  list_of_draws = extract(model_fit)
  number_of_samples = length(list_of_draws$lp__)

  if (model == 'NB') {
    # Extracting a random sample from the model fit
    random = sample(number_of_samples, number_of_simulations)
    home_att = list_of_draws$home_att[,home_index][random]
    home_def = list_of_draws$home_def[,home_index][random]
    away_att = list_of_draws$away_att[,away_index][random]
    away_def = list_of_draws$away_def[,away_index][random]
    size_home = list_of_draws$phi_home[random]
    size_away = list_of_draws$phi_away[random]
    
    # Creating the log_mu parameters
    log_mu1 = home_att + away_def
    log_mu2 = home_def + away_att
    
    # Simulating from a negative binomial distribution to obtain predictive distribution
    y1 = NULL; y2 = NULL
    for (i in 1:number_of_simulations) {
      y1[i] = rnbinom(1, size = size_home, mu = exp(log_mu1[i]))
      y2[i] = rnbinom(1, size = size_away, mu = exp(log_mu2[i]))
    }
  } else if (model == 'PO') {
    # Extracting a random sample from the model fit
    random = sample(number_of_samples, number_of_simulations)
    home_att = list_of_draws$home_att[,home_index][random]
    home_def = list_of_draws$home_def[,home_index][random]
    away_att = list_of_draws$away_att[,away_index][random]
    away_def = list_of_draws$away_def[,away_index][random]
    
    # Creating the log_mu parameters
    log_mu1 = home_att + away_def
    log_mu2 = home_def + away_att
    
    # Simulating from a negative binomial distribution to obtain predictive distribution
    y1 = NULL; y2 = NULL
    for (i in 1:number_of_simulations) {
      y1[i] = rpois(1, lambda = exp(log_mu1[i]))
      y2[i] = rpois(1, lambda = exp(log_mu2[i]))
    }
  } else if (model == 'BB') {
    # Extracting a random sample from the model fit
    random = sample(number_of_samples, number_of_simulations)
    home = list_of_draws$home[random]
    att_home = list_of_draws$att[,home_index][random]
    att_away = list_of_draws$att[,away_index][random]
    def_home = list_of_draws$def[,home_index][random]
    def_away = list_of_draws$def[,away_index][random]
    
    # Creating the log_theta parameters
    log_theta1 = home + att_home + def_away
    log_theta2 = att_away + def_home
    
    # Simulating from a Poisson distribution to obtain predictive distribution
    y1 = NULL; y2 = NULL
    for (i in 1:number_of_simulations) {
      y1[i] = rpois(1, lambda = exp(log_theta1[i]))
      y2[i] = rpois(1, lambda = exp(log_theta2[i]))
    }
  } 
  
  simulated_games = data.frame(y1, y2)
  colnames(simulated_games) = c(home_team, away_team)
  
  # Return data frame of simulated games
  return(simulated_games)
}
