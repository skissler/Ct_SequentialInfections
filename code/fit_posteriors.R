ct_model <- stan_model("code/fit_posteriors.stan") 

fit_startq <- Sys.time()
ct_fit <- sampling(ct_model, 
	data=list(
		N=nrow(indiv_data), 
		n_id=length(unique(indiv_data$id_clean)),
		lod=global_pars[["lod"]], 
		id=indiv_data$id_clean,
		category=prior_pars$category,
		max_category=prior_pars$max_category,
		n_adj=ncol(prior_pars$adjust),
		adjust=prior_pars$adjust,
		max_adjust=prior_pars$max_adjust,
		t=indiv_data$t, 
		y=indiv_data$y, 
		tp_prior=prior_pars$tp_prior,
		dp_midpoint=prior_pars$dp_midpoint,
		wp_midpoint=prior_pars$wp_midpoint,
		wr_midpoint=prior_pars$wr_midpoint,
		sigma_prior=prior_pars$sigma_prior,
		lambda=prior_pars$lambda,
		fpmean=prior_pars$fpmean,
		priorsd=prior_pars$priorsd), 
	iter=1000, chains=4) # iter=2000
# , control = list(adapt_delta=0.85)
# , control = list(adapt_delta=0.99)
# control = list(adapt_delta=0.95, max_treedepth=15)
fit_endq <- Sys.time()
print(paste0("Fit time: ",difftime(fit_endq, fit_startq, units="min")," mins"))

# launch_shinystan_nonblocking(ct_fit)
