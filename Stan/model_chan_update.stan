data {
  int nteams;
  int ngames;
  int home_team[ngames];
  int away_team[ngames];
  int<lower=0> home_goals[ngames];
  int<lower=0> away_goals[ngames];
  
  vector[8 + 4 * (nteams - 1)] mu;
  cov_matrix[rows(mu)] Sigma;
}
parameters {
  real mu_home_att;
  real mu_away_att;
  real mu_home_def;
  real mu_away_def;
  real<lower=0> sigma2_att;
  real<lower=0> sigma2_def;
  real<lower=0> phi_home;
  real<lower=0> phi_away;
  
  vector[nteams-1] home_att_raw;
  vector[nteams-1] away_att_raw;
  vector[nteams-1] home_def_raw;
  vector[nteams-1] away_def_raw;
}
transformed parameters {
  vector[ngames] log_mu_home;
  vector[ngames] log_mu_away;
  vector[nteams] home_att;
  vector[nteams] away_att;
  vector[nteams] home_def;
  vector[nteams] away_def;
  
  for (k in 1:(nteams-1)) {
    home_att[k] = home_att_raw[k];
    away_att[k] = away_att_raw[k];
    home_def[k] = home_def_raw[k];
    away_def[k] = away_att_raw[k];
  }
  home_att[nteams] = -sum(home_att_raw);
  away_att[nteams] = -sum(away_att_raw);
  home_def[nteams] = -sum(home_def_raw);
  away_def[nteams] = -sum(away_def_raw);
  
  log_mu_home = home_att[home_team] + away_def[away_team];
  log_mu_away = away_att[away_team] + home_def[home_team]; 
}
model {
  vector[8 + 4 * (nteams - 1)] theta = append_row(append_row([mu_home_att, mu_away_att, mu_home_def, mu_away_def,
                                                  sigma2_att, sigma2_def, phi_home, phi_away]', 
                                                  append_row(home_att_raw, away_att_raw)), 
                                                  append_row(home_def_raw, away_def_raw));
  theta ~ multi_normal(mu, Sigma);

  home_goals ~ neg_binomial_2_log(log_mu_home, phi_home); 
  away_goals ~ neg_binomial_2_log(log_mu_away, phi_away); 
}
