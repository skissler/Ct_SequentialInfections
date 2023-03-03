library(tidyverse)
library(lazyeval)
library(rstan) 
library(shinystan) 
library(purrr)
library(data.table)
options(mc.cores=parallel::detectCores())
source('code/utils.R')
source("code/set_global_pars.R")

cat_df <- run_pars$analysis_rows %>% 
	imap(~ tibble(category=.y,RowID=.x)) %>% 
	bind_rows() 


if(length(run_pars$adjustment_rows)>0){
		adj_df <- run_pars$adjustment_rows %>% 
			imap(~ tibble(adj=.y,RowID=.x)) %>% 
			bind_rows() 	
	} else {
		adj_df <- tibble(adj=1, RowID=ct_dat_refined$RowID)
	}

indiv_data <- ct_dat_refined %>% 
	filter(!(RowID %in% run_pars$excluded_rows)) %>%
	left_join(cat_df, by="RowID") %>% 
	left_join(adj_df, by="RowID") %>% 
	mutate(id=InfectionEvent) %>%
	mutate(t=TestDateIndex) %>%
	mutate(y=CtT1) %>%
	trim_negatives(global_pars) %>% 
	clean_infection_events() %>%
	mutate(id_clean=InfectionEventClean) %>% 
	ungroup() %>% 
	select(id, id_clean, t, y, category, adj) %>% 
	arrange(id_clean, t)

# Store the number of infection events we've kept: 
n_indiv <- length(unique(indiv_data$id))

catlist <- indiv_data %>%
	group_by(id) %>%
	slice(1) %>%
	select(id, category) %>%
	arrange(id) %>%
	pull(category)

adjlist <- indiv_data %>%
	group_by(id) %>%
	slice(1) %>%
	select(id, adj) %>%
	arrange(id) %>%
	pull(adj)

prior_pars <- list(
	tp_prior=run_pars$tp_prior,
	dp_midpoint=run_pars$dp_midpoint,
	wp_midpoint=run_pars$wp_midpoint,
	wr_midpoint=run_pars$wr_midpoint,
	sigma_prior=run_pars$sigma_prior,
	lambda=run_pars$lambda,
	fpmean=run_pars$fpmean,		# so that 90% of mass is <1 and 99% is <2
	category=catlist,
	max_category=max(catlist),
	adjust=adjlist,
	max_adjust=max(adjlist)
)	

