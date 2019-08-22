functions {
  real chan_2_log(int goals, real log_mu, real phi, real epsilon, real time) {
    real log_prob;
    log_prob = (exp(-epsilon * time) * neg_binomial_2_log_lpmf(goals| log_mu, phi));
    return log_prob;
  }
}
data {
  int<lower=1> nteams;
  int<lower=1> ngames;
  int<lower=1, upper=nteams> home_team[ngames];
  int<lower=1, upper=nteams> away_team[ngames];
  int<lower=0> home_goals[ngames];
  int<lower=0> away_goals[ngames];
  int<lower=0> days_diff_home[ngames];
  int<lower=0> days_diff_away[ngames];
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
  real<lower=0> epsilon;
  
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
  vector[nteams] home_att;
  vector[nteams] away_att;
  vector[nteams] home_def;
  vector[nteams] away_def;
  vector[ngames] log_mu_home;
  vector[ngames] log_mu_away;
  
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
  phi_home ~ gamma(2.5, 0.05);
  phi_away ~ gamma(2.5, 0.05);
  epsilon ~ gamma(5, 200);
  
  home_att_scale ~ normal(0, 10);
  away_att_scale ~ normal(0, 10);
  home_def_scale ~ normal(0, 10);
  away_def_scale ~ normal(0, 10);
  
  home_att_raw ~ normal(mu_home_att, sigma2_att);
  away_att_raw ~ normal(mu_away_att, sigma2_att);
  home_def_raw ~ normal(mu_home_def, sigma2_def);
  away_def_raw ~ normal(mu_away_def, sigma2_def);
  
  for (g in 1:ngames) {
    target += (exp(-epsilon*days_diff_home[g]) * neg_binomial_2_log_lpmf(home_goals[g] | log_mu_home[g], phi_home));
    target += (exp(-epsilon*days_diff_away[g]) * neg_binomial_2_log_lpmf(away_goals[g] | log_mu_away[g], phi_away));
  }
}
