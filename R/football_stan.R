# Loading necessary packages
library(tidyverse)
library(rstan)
library(shinystan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options("scipen"=100)

read_data = function(csv_file) {
  # Read data from football-data csv file
  data = read.csv(csv_file)
  data = cbind(data[2:6],data[24:26])
  data$Date = as.Date(data$Date, format='%d/%m/%y')
  data$HomeIndex = as.numeric(factor(data$HomeTeam))
  data$AwayIndex = as.numeric(factor(data$AwayTeam))
  
  # Return new data frame
  return(data)
}

fit_model = function(stan_file, data, iterations, chains) {
  # Fit a Stan model to the data and return the output
  
  ### Arguments: 
  
  # stan_file = .stan file containing the model
  # data = data containing football scores in a data frame
  # iterations = positive integer specifying the number of iterations for each chain including warmup
  # chains = positive integer specifying the number of Markov Chains

  # Building Stan data object
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
                   iter = iterations,
                   chains = chains,
                   control = list(max_treedepth = 15, adapt_delta = 0.99))

  
  # Returning the fitted model
  return(model)
}

plot_parameters = function(model_fit, data, model) {
  # Plots one dimensional plot for parameters (attack and defence parmeters for each team)
  
  ### Arguments:
  
  # model_fit = a Stanfit object of the fitted Stan model
  # data = data containing football scores in a data frame
  # model = define which model was implemented - "NB" for Negative Binomial, 
  #                                              "PO" for Poisson, 
  #                                              "BB" for Baio & Blangiardo
  
  teams = as.character(sort(unique(c(as.vector(data$HomeTeam), as.vector(data$AwayTeam)))))
  
  # Option to abbreviate team names for the plot
  # for (i in 1:length(teams)) {
  #   if (length(strsplit(teams[i], " ")[[1]]) == 1) {
  #     teams[i] = toupper(substr(teams[i], 1, 3))
  #   } else {
  #     teams[i] = paste(toupper(substr(strsplit(teams[i], " ")[[1]][1], 1, 1)),
  #                      toupper(substr(strsplit(teams[i], " ")[[1]][2], 1, 2)), sep='')
  #   }
  # }

  if (model == 'NB' | model == 'PO') {
    # Gives parameter plots for home_att, home_def, away_att, away_def
    
    # Plotting home attack parameter
    home_att_figure = stan_plot(model_fit, pars=c('home_att')) + ggtitle("Home Attack Estimates\n")
    home_att_figure = home_att_figure + scale_y_continuous(labels=rev(teams), breaks=1:20)
    plot(home_att_figure)

    # Plotting home defence parameter
    home_def_figure = stan_plot(model_fit, pars=c('home_def')) + ggtitle("Home Defence Estimates\n")
    home_def_figure = home_def_figure + scale_y_continuous(labels=rev(teams), breaks=1:20)
    plot(home_def_figure)

    # Plotting away attack parameter
    away_att_figure = stan_plot(model_fit, pars=c('away_att')) + ggtitle("Away Attack Estimates\n") 
    away_att_figure = away_att_figure + scale_y_continuous(labels=rev(teams), breaks=1:20)
    plot(away_att_figure)
    
    # Plotting away defence parameter
    away_def_figure = stan_plot(model_fit, pars=c('away_def')) + ggtitle("Away Defence Estimates\n")
    away_def_figure = away_def_figure + scale_y_continuous(labels=rev(teams), breaks=1:20)
    plot(away_def_figure)

  } else if (model == 'BB') {
    # Gives parameter plots for att and def for each team
    # Also plots the home advantage parameter in the model
    
    # Plotting the home advantage parameter
    home_figure = stan_plot(model_fit, pars = c('home')) + ggtitle("Home Advantage Estimate")
    plot(home_figure)
    
    # Plotting attack parameter
    att_figure = stan_plot(model_fit, pars = c('att')) + ggtitle("Attack Estimates")
    att_figure = att_figure + scale_y_continuous(labels=rev(teams), breaks=1:20)
    plot(att_figure)
    
    # Plotting defence parameter
    def_figure = stan_plot(model_fit, pars = c('def')) + ggtitle("Defence Estimates")
    def_figure = def_figure + scale_y_continuous(labels=rev(teams), breaks=1:20)
    plot(def_figure)
  }
}

plot_parameters_2d = function(model_fit, data, model, point_est = 'median', cols = 'black', overall = F) {
  # Plots two dimensional plots (mean attack vs. mean defence parmeters)
  
  ### Arguments:
  
  # model_fit = a Stanfit object of the fitted Stan model
  # data = data containing football scores in a data frame
  # model = define which model was implemented - "NB" for Negative Binomial, 
  #                                              "PO" for Poisson, 
  #                                              "BB" for Baio & Blangiardo
  # point_est = the point estimate to show - either "median" or "mean"
  # cols = colour labels can be given, set to black otherwise
  # overall = bool - for NB and PO models, if set to TRUE, rather than obtaining 2 plots for home and away effects
  #           function returns an overall plot, where the average is taken between the home and away effects for
  #           each team
  
  teams = as.character(sort(unique(c(as.vector(data$HomeTeam), as.vector(data$AwayTeam)))))
  
  # Abbreviating team names
  for (i in 1:length(teams)) {
    if (length(strsplit(teams[i], " ")[[1]]) == 1) {
      teams[i] = toupper(substr(teams[i], 1, 3))
    } else {
      first = toupper(substr(strsplit(teams[i], " ")[[1]][1], 1, 1))
      second = toupper(substr(strsplit(teams[i], " ")[[1]][2], 1, 2))
      teams[i] = paste(first, second, sep='')
    }
  }
  
  if (model == 'NB' | model == 'PO') {
    # Obtain summary statistics for the relevant parameters
    home_att_sum = summary(model_fit, pars = c('home_att'))
    home_def_sum = summary(model_fit, pars = c('home_def'))
    away_att_sum = summary(model_fit, pars = c('away_att'))
    away_def_sum = summary(model_fit, pars = c('away_def'))
    
    if (point_est == 'mean') {
      # Home mean estimate
      home_att_pe = home_att_sum$summary[,'mean']
      home_def_pe = home_def_sum$summary[,'mean']
      # Away mean estimate
      away_att_pe = away_att_sum$summary[,'mean']
      away_def_pe = away_def_sum$summary[,'mean']
    } else {
      # Home median estimates
      home_att_pe = home_att_sum$summary[,'50%']
      home_def_pe = home_def_sum$summary[,'50%']
      # Away median estimates
      away_att_pe = away_att_sum$summary[,'50%']
      away_def_pe = away_def_sum$summary[,'50%']
    } 
    
    if (overall == T) {
      # Taking the overall effects as the mean of the home and away effect
      attack_average = (home_att_pe + away_att_pe)/2
      defence_average = (home_def_pe + away_def_pe)/2
      overall_data = data.frame(attack_average, defence_average, teams)
      
      # Plotting overall effects
      plot(attack_average, defence_average, col = cols, xlab = 'Attack', ylab = 'Defence', main='Overall Effects')
      abline(v=0); abline(h=0)
      text(attack_average, defence_average, labels = teams, cex=0.7, pos=2)
    } else {
      # Plotting home effects
      home_data = data.frame(home_att_pe, home_def_pe, teams)
      plot(home_att_pe, home_def_pe, col = cols, xlab = 'Attack', ylab = 'Defence', main='Home Effects')
      abline(v=0); abline(h=0)
      text(home_att_pe, home_def_pe, labels = teams, cex=0.7, pos=2)
      
      # Plotting away effects
      away_data = data.frame(away_att_pe, away_def_pe, teams)
      plot(away_att_pe, away_def_pe, col = cols, xlab = 'Attack', ylab = 'Defence', main='Away Effects')
      abline(v=0); abline(h=0)
      text(away_att_pe, away_def_pe, labels = teams, cex=0.7, pos=2)
    }
  } else if (model == 'BB') {
    # Obtain summary statistics for the relevant parameters
    att_sum = summary(model_fit, pars = c('att'))
    def_sum = summary(model_fit, pars = c('def'))
    
    if (point_est == 'mean') {
      att_pe = att_sum$summary[,'mean']
      def_pe = def_sum$summary[,'mean']
    } else {
      att_pe = att_sum$summary[,'50%']
      def_pe = def_sum$summary[,'50%']
    }
    
    # Plotting overall effects
    overall_data = data.frame(att_pe, def_pe, teams)
    plot(att_pe, def_pe, col= cols, xlab = 'Attack', ylab = 'Defence', main='Team Effects')
    abline(v=0); abline(h=0)
    text(att_pe, def_pe, labels = teams, cex=0.7, pos=2)
  } 
}
