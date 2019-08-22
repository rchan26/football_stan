data {
  int nteams;
  int ngames;
  int home_team[ngames];
  int away_team[ngames];
  int<lower=0> home_goals[ngames];
  int<lower=0> away_goals[ngames];
}
parameters {
  real mu_home_att;
  real mu_away_att;
  real mu_home_def;
  real mu_away_def;
  real<lower=0> sigma2_att;
  real<lower=0> sigma2_def;
  
  simplex[nteams] home_att_raw;
  simplex[nteams] away_att_raw;
  simplex[nteams] home_def_raw;
  simplex[nteams] away_def_raw;
  
  real home_att_scale;
  real away_att_scale;
  real home_def_scale;
  real away_def_scale;
}
transformed parameters {
  vector[ngames] log_mu_home;
  vector[ngames] log_mu_away;
  vector[nteams] home_att;
  vector[nteams] away_att;
  vector[nteams] home_def;
  vector[nteams] away_def;
  
  home_att = home_att_scale * (home_att_raw - 1.0/nteams);
  away_att = away_att_scale * (away_att_raw - 1.0/nteams);
  home_def = home_def_scale * (home_def_raw - 1.0/nteams);
  away_def = away_def_scale * (away_def_raw - 1.0/nteams);
  
  log_mu_home = home_att[home_team] + away_def[away_team];
  log_mu_away = away_att[away_team] + home_def[home_team];
}
model {
  mu_home_att ~ normal(0.2, 1);
  mu_away_att ~ normal(0, 1);
  mu_home_def ~ normal(-0.2, 1);
  mu_away_def ~ normal(0, 1);
  sigma2_att ~ gamma(10, 10);
  sigma2_def ~ gamma(10, 10);

  home_att_scale ~ normal(0, 10);
  away_att_scale ~ normal(0, 10);
  home_def_scale ~ normal(0, 10);
  away_def_scale ~ normal(0, 10);
  
  home_att_raw ~ normal(mu_home_att, sigma2_att);
  away_att_raw ~ normal(mu_away_att, sigma2_att);
  home_def_raw ~ normal(mu_home_def, sigma2_def);
  away_def_raw ~ normal(mu_away_def, sigma2_def);

  home_goals ~ poisson_log(log_mu_home); 
  away_goals ~ poisson_log(log_mu_away); 
}
