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
  
  // need to make sum(att)=sum(def)=0 
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
  
  // getting mu in log form 
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
  phi_home ~ normal(10, 10);
  phi_away ~ normal(10, 10);
  
  home_att_raw ~ normal(mu_home_att, sigma2_att);
  away_att_raw ~ normal(mu_away_att, sigma2_att);
  home_def_raw ~ normal(mu_home_def, sigma2_def);
  away_def_raw ~ normal(mu_away_def, sigma2_def);

  home_goals ~ neg_binomial_2_log(log_mu_home, phi_home); 
  away_goals ~ neg_binomial_2_log(log_mu_away, phi_away); 
}
