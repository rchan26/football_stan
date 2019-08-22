calculate_BS = function(stan_file, test_set, train_set, model) {
  # Calculate the Brier Score for a set of games given in the test_set
  # Returns the Brier score for the set of games in the test_set
  
  ### Arguments:
  
  # stan_file = .stan file containing the model
  # test_set = data containing football scores that are to be predicted in a data frame
  # train_set = data containing football scores that are to be used as the training data
  # model = define which model was implemented - "NB" for Negative Binomial, 
  #                                              "PO" for Poisson, 
  #                                              "BB" for Baio & Blangiardo
  
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
  
  # Calulating Brier score by choosing event with highest probability and seeing if the event occured
  Brier_score = 0
  for (i in 1:nrow(test_set)) {
    # Obtain outcome probabilities for the game
    probabilities = predict_game(model_fit = train,
                                 home_team = test_set[i,]$HomeTeam,
                                 away_team = test_set[i,]$AwayTeam,
                                 data = train_set,
                                 model = model)
    
    # Need to find our forecast (i.e. highest probable event) and then check if it occured
    # Then we add appropriately to the Brier score
    if (test_set[i,]$FTHG > test_set[i,]$FTAG) {
      Brier_score = Brier_score + (1-probabilities[,1])^(2) + (0-probabilities[,2])^(2) + (0-probabilities[,3])^(2)
    } else if (test_set[i,]$FTHG == test_set[i,]$FTAG) {
      Brier_score = Brier_score + (0-probabilities[,1])^(2) + (1-probabilities[,2])^(2) + (0-probabilities[,3])^(2)
    } else if  (test_set[i,]$FTHG < test_set[i,]$FTAG) {
      Brier_score = Brier_score + (0-probabilities[,1])^(2) + (0-probabilities[,2])^(2) + (1-probabilities[,3])^(2)
    }
  }
  
  # Return Brier score
  Brier_score = (Brier_score/nrow(test_set))
  return(Brier_score)
}

MCBS = function(stan_file, iterations, test_percentage, data, model) {
  # Monte Carlo Brier score
  # Obtain Monte Carlo Brier score by repeatedly splitting the data into training and test sets
  # Return the Monte Carlo Brier Score and the vector of Brier scores in a list
  
  Brier_scores = NULL
  for (i in 1:iterations) {
    # Splitting up the data into (test_percentage)% and (1-test_percentage)% randomly
    random = sample(nrow(data), test_percentage*nrow(data))
    test_set = data[random,]
    train_set = data[-random,]
    
    # Finding a Brier score for this particular test/train set combination and adding this to the vector of Brier scores
    Brier_scores[i] = calculate_BS(stan_file, test_set, train_set, model)
    
    # Print progress of the algorithm
    print(paste(i, '. Brier Score: ', Brier_scores[i]))
  }
  
  # Return the Monte Carlo Brier Score and the vector of Brier scores
  scores = list('MCBS' = mean(Brier_scores), 'Brier_Scores' = Brier_scores)
  return(scores)
}

seqBrier = function(stan_file, data, model, minimum_games) {
  # Seqential Brier score
  # Function to obtain Brier score for larger dataset
  # The training set will progressively get larger as more games become available after prediction
  # Returns Brier score for the dataset

  Brier_score = 0
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
    
    # Calculating BS for test data set and adding to the overall Brier score
    # Need to multiply by nrow(test_set) since calculate_BS divides by nrow(test_set)
    Brier_score = Brier_score + (nrow(test_set) * calculate_BS(stan_file, test_set, train_set, model))
    
    # Print progress of the algorithm
    print(paste('There are ', nrow(data), ' games left in the data set'))
    
    # Train_set now becomes all the data and games that have happened before the test_set games
    train_set = rbind(train_set, data[1:i-1,])
  }
  
  # Return the Brier score
  Brier_score = (Brier_score/total_number_of_test_games)
  return(Brier_score)
}
