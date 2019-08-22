WLD_validate = function(stan_file, test_set, train_set, model) {
  # Validate the correct outcome of a match by choosing the outcome with the highest probability
  # Returns a vector of TRUEs and FALSEs corresponding to whether the model predicted the right outcome of the game
  # To find cross validation score, take the mean of this vector (i.e. percentage of correct estimates)
  
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
  
  # Choosing event with highest probability
  prediction_log = NULL
  for (i in 1:nrow(test_set)) {
    # Obtain outcome probabilities for the game
    probabilities = predict_game(model_fit = train,
                                 home_team = test_set[i,]$HomeTeam,
                                 away_team = test_set[i,]$AwayTeam,
                                 data = train_set,
                                 model = model)
    
    # Adding TRUE value to prediction_log if prediction correct and FALSE otherwise
    if (test_set[i,]$FTHG > test_set[i,]$FTAG) {
      prediction_log = c(prediction_log, (probabilities[,1] == max(probabilities)))
    } else if (test_set[i,]$FTHG == test_set[i,]$FTAG) {
      prediction_log = c(prediction_log, (probabilities[,2] == max(probabilities)))
    } else if (test_set[i,]$FTHG < test_set[i,]$FTAG) {
      prediction_log = c(prediction_log, (probabilities[,3] == max(probabilities)))
    }
  }
  
  # Returning the vector of TRUEs and FALSEs for each game prediction
  return(prediction_log)
}

MCCV = function(stan_file, iterations, test_percentage, data, model) {
  # Monte Carlo Cross Validation (MCCV)
  # Obtain MCCV score by repeatedly splitting the data into training and test sets
  # Return the Monte Carlo Cross Validation Score and the vector of CV scores in a list
  
  CV_scores = NULL
  for (i in 1:iterations) {
    # Splitting up the data into (test_percentage)% and (1-test_percentage)% randomly
    random = sample(nrow(data), test_percentage*nrow(data))
    test_set = data[random,]
    train_set = data[-random,]
    
    # Finding a CV score for this particular test/train set combination and adding this to the vector of MCCV scores
    CV_scores[i] = mean(WLD_validate(stan_file, test_set, train_set, model))
    
    # Print progress of the algorithm
    print(paste(i, '. MCCV Score: ', CV_scores[i]))
  }
  
  # Return the Monte Carlo Cross Validation Score and the vector of CV scores
  scores = list('MCCV' = mean(MCCV_scores), 'CV_Scores' = CV_scores)
  return(scores)
}

seqCV = function(stan_file, data, model, minimum_games) {
  # Seqential Cross Validation
  # Function to obtain CV score for larger dataset
  # The training set will progressively get larger as more games become available after prediction
  # Returns a list containing: a vector of TRUEs and FALSEs corresponding to whether the model predicted the right
  #                            outcome of the game (game_prediction_log) and the CV score for the dataset

  game_predictions_log = NULL
  
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
    
    # Predicting games by choosing the highest probable outcome
    game_predictions_log = c(game_predictions_log, WLD_validate(stan_file, test_set, train_set, model))
    
    # Print progress of the algorithm
    print(paste('There are ', nrow(data), ' games left in the data set'))
    
    # Train_set now becomes all the data and games that have happened before the test_set games
    train_set = rbind(train_set, data[1:i-1,])
  }
  
  # Return the Sequential Cross Validation Score and the vector that logged the results of forecasts of the games
  results = list('seqCV_Score' = mean(game_predictions_log), 'game_predictions_log' = game_predictions_log)
  return(results)
}
