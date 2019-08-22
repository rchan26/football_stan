data {
  int nteams;
  int ngames;
  int home_team[ngames];
  int away_team[ngames];
  int<lower=0> home_goals[ngames];
  int<lower=0> away_goals[ngames];
}
parameters {
  real home;
  real mu_att;
  real mu_def;
  real<lower=0> tau_att;
  real<lower=0> tau_def;

  vector[nteams-1] att_free;
  vector[nteams-1] def_free;
}
transformed parameters {
  vector[nteams] att;
  vector[nteams] def;
  vector[ngames] log_theta_home;
  vector[ngames] log_theta_away;

  // need to make sum(att)=sum(def)=0
  for (k in 1:(nteams-1)) {
    att[k] = att_free[k];
    def[k] = def_free[k];
  }
  att[nteams] = -sum(att_free);
  def[nteams] = -sum(def_free);

  log_theta_home = home + att[home_team] + def[away_team];
  log_theta_away = att[away_team] + def[home_team];
}
model {
  home ~ normal(0, 10000);
  mu_att ~ normal(0, 10000);
  mu_def ~ normal(0, 10000);
  tau_att ~ gamma(0.1, 0.1);
  tau_def ~ gamma(0.1, 0.1);

  att_free ~ normal(mu_att, 1/tau_att);
  def_free ~ normal(mu_def, 1/tau_def);

  home_goals ~ poisson_log(log_theta_home);
  away_goals ~ poisson_log(log_theta_away);
}
