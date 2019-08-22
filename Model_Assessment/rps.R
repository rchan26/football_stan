calculate_RPS = function(stan_file, test_set, train_set, model, mean = F) {
  # Calculate the Rank Probability Score for a set of games given in the test_set
  # Returns the sum of the RPS scores for each game by default
  
  ### Arguments:
  
  # stan_file = .stan file containing the model
  # test_set = data containing football scores that are to be predicted in a data frame
  # train_set = data containing football scores that are to be used as the training data
  # model = define which model was implemented - "NB" for Negative Binomial, 
  #                                              "PO" for Poisson, 
  #                                              "BB" for Baio & Blangiardo
  # mean = bool - if set to TRUE, will return the mean RPS for each game

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
  
  # Calulating RPS for games in the test set
  RPS = 0
  for (i in 1:nrow(test_set)) {
    # Obtain outcome probabilities for the game
    probabilities = predict_game(model_fit = train,
                                 home_team = test_set[i,]$HomeTeam,
                                 away_team = test_set[i,]$AwayTeam,
                                 data = train_set,
                                 model = model)
    
    # Calculate RPS for the game
    sum_probabilities_vector = c(sum(probabilities[1]), sum(probabilities[1:2]), sum(probabilities[1:3]))
    if (test_set[i,]$FTHG > test_set[i,]$FTAG) {
      observed_vector = c(1,1,1)
    } else if (test_set[i,]$FTHG == test_set[i,]$FTAG) {
      observed_vector = c(0,1,1)
    } else if  (test_set[i,]$FTHG < test_set[i,]$FTAG) {
      observed_vector = c(0,0,1)
    }
    
    # Add to RPS tally for the test set
    RPS = RPS + ((sum_probabilities_vector[1] - observed_vector[1])^(2) +
                 (sum_probabilities_vector[2] - observed_vector[2])^(2) +
                 (sum_probabilities_vector[3] - observed_vector[3])^(2)) / 2
  }
  
  if (mean == T) {
    # Return mean average RPS per game
    return(RPS/nrow(test_set)) 
  } else {
    # Return total sum of RPS for the games in the test_set
    return(RPS)
  }
}

MCRPS = function(stan_file, iterations, test_percentage, data, model) {
  # Monte Rank Probability Score
  # Obtain Monte Carlo RPS by repeatedly splitting the data into training and test sets
  # Return the Monte Carlo RPS and the vector of RPSs in a list
  
  RPSs = NULL
  for (i in 1:iterations) {
    # Splitting up the data into (test_percentage)% and (1-test_percentage)% randomly
    random = sample(nrow(data), test_percentage*nrow(data))
    test_set = data[random,]
    train_set = data[-random,]
    
    # Finding a RPS for this particular test/train set combination and adding this to the vector of RPSs
    RPSs[i] = calculate_RPS(stan_file, test_set, train_set, model, mean = T)
    
    # Print progress of the algorithm
    print(paste(i, '. RPS: ', RPSs[i]))
  }
  
  # Return the Monte Carlo RPS and the vector of RPSs in a list
  scores = list('MCRPS' = mean(RPSs), 'RPSs' = RPSs)
  return(scores)
}

seqRPS = function(stan_file, data, model, minimum_games) {
  # Sequential Rank Probability Score
  # Function to obtain RPS for larger dataset
  # The training set will progressively get larger as more games become available after prediction
  # Returns a list containing: the mean RPS per game and the total RPS for the dataset
  
  ### Arguments:

  # stan_file = .stan file containing the model
  # data = data containing football scores in a data frame
  # model = define which model was implemented - "NB" for Negative Binomial, 
  #                                              "PO" for Poisson, 
  #                                              "BB" for Baio & Blangiardo
  # minimum_games = initial number of games in the training set
  
  RPS = 0
  total_number_of_test_games = nrow(data) - minimum_games
  
  # Splitting the data set to initialise the training and test sets
  total_number_of_games = nrow(data)
  train_set = data[1:minimum_games,]
  test_set = data[(minimum_games+1):nrow(data),]
  data = data[(minimum_games+1):nrow(data),]
  
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
    
    # Calculating RPS for test data set and adding to the overall RPS
    RPS = RPS + calculate_RPS(stan_file, test_set, train_set, model, mean = F)
    
    # Print progress of the algorithm
    print(paste('There are ', nrow(data), ' games left in the data set'))
    
    # Train_set now becomes all the data and games that have happened before the test_set games and loop back to top
    train_set = rbind(train_set, data[1:i-1,])
  }
  
  # Return the mean RPS per game and the total RPS for the dataset
  results = list('mean_seqRPS' = RPS/(total_number_of_test_games), 'total_seqRPS' = RPS)
  return(results)
}