data {
  int nteams;
  int ngames;
  int home_team[ngames];
  int away_team[ngames];
  int<lower=0> home_goals[ngames];
  int<lower=0> away_goals[ngames];
  
  vector[5 + 2 * (nteams - 1)] mu;
  cov_matrix[rows(mu)] precision;
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
  vector[5 + 2 * (nteams - 1)] theta = append_row([home, mu_att, mu_def, tau_att, tau_def]', append_row(att_free, def_free));
  theta ~ multi_normal_prec(mu, precision);
  
  home_goals ~ poisson_log(log_theta_home);
  away_goals ~ poisson_log(log_theta_away);
}
