functions {
  // mufun returns dCt, the delta Ct below the LOD:
  real mufun(real t, real tp, real wp, real wr, real dp){
  // Viral load rises before peak: 
  if(t<=tp)
    return(dp/wp*(t-(tp-wp)));
  // Viral load falls after peak: 
  else 
    return(dp - dp/wr*(t-tp));
  }
}

data{
  int<lower=0> N;           // Number of concatenated data points
  int<lower=0> n_id;       // Number of infections
  real<lower=0> lod;        // Limit of detection
  int<lower=0> id[N];       // Vector marking which datum belongs to which id
  int<lower=0> category[n_id];  // Analysis categories
  int<lower=0> max_category;     // Highest analysis category
  int<lower=0> n_adj;        // Number of adjustment variables 
  int<lower=0> adjust[n_id,n_adj];  // Adjustment categories
  int<lower=0> max_adjust;     // Highest adjustment category, across all adjustment variables
  real t[N];                // Vector marking the time for each data point 
  real<lower=0, upper=lod> y[N]; // Concatenated data vector 
  real tp_prior[2];              // Prior sd for the onset time (days)
  real dp_midpoint;         // Prior mean peak Ct (delta from lod)
  real wp_midpoint;         // Prior mean proliferation duration
  real wr_midpoint;         // Prior mean clearance duration 
  real sigma_prior[2];      // Prior observation noise Cauchy scale
  real<lower=0, upper=1> lambda; // Mixture probability (~1-sensitivity)
  real<lower=0> fpmean;     // False positive mean Ct
  real<lower=0> priorsd;    // Standard deviation for the lognormal priors
}


transformed data {
  real<lower=0, upper=lod> ydrop[N];  // Concatenated deviation from LOD 
  
  real loglambda;
  real log1mlambda;
  real is_lod[N];
  real log_is_lod[N];

  for(i in 1:N){
    ydrop[i] = lod-y[i];
    if(ydrop[i]==0) 
      is_lod[i]=1;
    else 
      is_lod[i]=0;
  }
  
  loglambda = log(lambda);
  log1mlambda = log1m(lambda);
  log_is_lod = log(is_lod);

}


parameters{

  real tp[n_id];

  real log_dp_mean;
  real<lower = 0> log_dp_sd;
  real log_dpadj_cat_raw[max_category-1];
  real log_dpadj_adjust_raw[max_adjust-1, n_adj];
  real dp_raw[n_id];

  real log_wp_mean;
  real<lower = 0> log_wp_sd;
  real log_wpadj_cat_raw[max_category-1];
  real log_wpadj_adjust_raw[max_adjust-1, n_adj];
  real wp_raw[n_id];

  real log_wr_mean;
  real<lower = 0> log_wr_sd;
  real log_wradj_cat_raw[max_category-1];
  real log_wradj_adjust_raw[max_adjust-1, n_adj];
  real wr_raw[n_id];

  real<lower = 0> sigma;

}


transformed parameters{

  real dp[n_id];
  real wp[n_id];
  real wr[n_id];
  real log_dpadj[max_category];
  real log_wpadj[max_category];
  real log_wradj[max_category];
  real log_dpadj_adjust[max_adjust,n_adj];
  real log_wpadj_adjust[max_adjust,n_adj];
  real log_wradj_adjust[max_adjust,n_adj];
  real mu[N];
  real zeroarray[1];
  real zeroarray_adjust[1,n_adj];
  real dpadj_add;
  real wpadj_add;
  real wradj_add;

  real num_arg[N,3];

  zeroarray[1] = 0;
  for(i in 1:n_adj){
    zeroarray_adjust[1,i] = 0;  
  }
  
  log_dpadj = append_array(zeroarray,log_dpadj_cat_raw);
  log_wpadj = append_array(zeroarray,log_wpadj_cat_raw);
  log_wradj = append_array(zeroarray,log_wradj_cat_raw);

  log_dpadj_adjust = append_array(zeroarray_adjust,log_dpadj_adjust_raw);
  log_wpadj_adjust = append_array(zeroarray_adjust,log_wpadj_adjust_raw);
  log_wradj_adjust = append_array(zeroarray_adjust,log_wradj_adjust_raw);

  for(i in 1:n_id){

    dpadj_add = 0;
    wpadj_add = 0;
    wradj_add = 0;
    for(j in 1:n_adj){
      dpadj_add = dpadj_add + log_dpadj_adjust[adjust[i,j],j];
      wpadj_add = wpadj_add + log_wpadj_adjust[adjust[i,j],j];
      wradj_add = wradj_add + log_wradj_adjust[adjust[i,j],j];
    }

    dp[i] = exp(log_dp_mean + 
      log_dpadj[category[i]] + 
      dpadj_add + 
      log_dp_sd*dp_raw[i])*dp_midpoint;

    wp[i] = exp(log_wp_mean + 
      log_wpadj[category[i]] + 
      wpadj_add + 
      log_wp_sd*wp_raw[i])*wp_midpoint;

    wr[i] = exp(log_wr_mean + 
      log_wradj[category[i]] + 
      wradj_add + 
      log_wr_sd*wr_raw[i])*wr_midpoint;

  }

  for(i in 1:N){
    mu[i] = mufun(t[i], tp[id[i]], wp[id[i]], wr[id[i]], dp[id[i]]);
    num_arg[i,1] = loglambda + exponential_lpdf(ydrop[i] | 1/fpmean);
    num_arg[i,2] = log1mlambda + normal_lpdf(ydrop[i] | mu[i], sigma);
    num_arg[i,3] = log1mlambda + log_is_lod[i] + normal_lcdf(0 | mu[i], sigma);
  }

}


model{

  tp ~ normal(tp_prior[1], tp_prior[2]);

  log_dp_mean ~ normal(0, priorsd); // hierarchical mean
  log_dp_sd ~ normal(0, priorsd) T[0,]; // sd of the individual-level draws
  log_dpadj_cat_raw ~ normal(0, priorsd); 
  for(indexA in 1:(max_adjust-1)){
    log_dpadj_adjust_raw[indexA] ~ normal(0, priorsd);
  }
  dp_raw ~ std_normal(); // for generating individual-level guesses 

  log_wp_mean ~ normal(0, priorsd); // hierarchical mean
  log_wp_sd ~ normal(0, priorsd) T[0,]; // sd of the individual-level draws
  log_wpadj_cat_raw ~ normal(0, priorsd); 
  for(indexA in 1:(max_adjust-1)){
    log_wpadj_adjust_raw[indexA] ~ normal(0, priorsd); 
  }
  wp_raw ~ std_normal(); // for generating individual-level guesses

  log_wr_mean ~ normal(0, priorsd); // hierarchical mean
  log_wr_sd ~ normal(0, priorsd) T[0,]; // sd of the individual-level draws
  log_wradj_cat_raw ~ normal(0, priorsd); 
  for(indexA in 1:(max_adjust-1)){
    log_wradj_adjust_raw[indexA] ~ normal(0, priorsd); 
  }
  wr_raw ~ std_normal(); // for generating individual-level guesses

  sigma ~ normal(sigma_prior[1], sigma_prior[2]) T[0,]; // process noise sd 

  for(i in 1:N){

      target += log_sum_exp(num_arg[i]);

    }
  
}



