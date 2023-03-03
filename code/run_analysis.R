# =============================================================================
# Import
# =============================================================================

library(tidyverse) 
library(purrr)
library(lubridate) 
library(scales) 

ct_dat_refined <- read_csv("data/ct_dat_refined.csv")

source('code/utils.R')
source('code/utils_private.R')
source("code/set_global_pars.R")
source("code/set_run_pars.R")

# =============================================================================
# Run the analysis
# =============================================================================

for(run_pars_index in 1:6){

	run_pars <- run_pars_list[[run_pars_index]]

	source("code/fit_posteriors_preamble.R")
	source("code/fit_posteriors.R")

	save(indiv_data, file=paste0("output/run_pars_adj_",run_pars_index,"/indiv_data.RData"))
	save(ct_fit, file=paste0("output/run_pars_adj_",run_pars_index,"/ct_fit.RData"))
	
	source("code/make_figures.R")
	source("code/save_figures.R")

	source("code/clearbigvars.R")

	print(paste0("Done with index ",run_pars_index," ------------"))

}

# =============================================================================
# Individual-level comparisons (Spearman correlations)
# =============================================================================

for(run_pars_index in 2:2){

	run_pars <- run_pars_list[[run_pars_index]]

	load(paste0("output/run_pars_adj_",run_pars_index,"/indiv_data.RData"))
	load(paste0("output/run_pars_adj_",run_pars_index,"/ct_fit.RData"))

	source("code/fit_posteriors_preamble.R")
	source("code/make_figures.R")

	source("code/indiv_analysis_revised.R")
}

# =============================================================================
# Post-hoc figure generation
# =============================================================================

for(run_pars_index in 1:6){

	run_pars <- run_pars_list[[run_pars_index]]

	source("code/fit_posteriors_preamble.R")

	load(paste0("output/run_pars_adj_",run_pars_index,"/indiv_data.RData"))
	load(paste0("output/run_pars_adj_",run_pars_index,"/ct_fit.RData"))
	
	source("code/make_figures.R")
	source("code/save_figures.R")

	print(paste0("Done with index ",run_pars_index," ------------"))

}