# Viral kinetics of sequential SARS-CoV-2 infections
__[Stephen M. Kissler](mailto:skissler@hsph.harvard.edu), James A. Hay, Joseph R. Fauver, Christina Mack, Caroline G. Tai, Deverick J. Anderson, David D. Ho, Nathan D. Grubaugh, Yonatan H. Grad__

This repository accompanies the manuscript "Viral kinetics of sequential SARS-CoV-2 infections". The scripts (in `code/`) are: 

- `run_analysis.R`: This is the main runfile, calling all of the other scripts to generate the figures and tables presented in the manuscript. 
- `utils.R`: contains key functions 
- `utils_private.R`: contains a single line formatted as `extdrive <- "~/Local/Output/Directory/"` for storing large output files 
- `set_global_pars.R` sets global parameters (currently just the qPCR limit of detection) 
- `set_run_pars.R` defines any exclusions, stratifications, and priors for the model fits. Each element of the list defined in this script defines a different analysis (i.e., a different set of comparison groups). 
- `fit_posteriors_preamble.R` formats the data in a way that's interpretable by Stan.
- `fit_posteriors.R` calls the Stan script for fitting the model.
- `fit_posteriors.stan` is the Stan script where the model is implemented and fit. 
- `make_figures.R` makes essential figures displayed in the manuscript
- `save_figures.R` saves those figures 
- `clearbigvars.R` clears storage-heavy variables to avoid crashing R or needlessly slowing down operations. 
- `indiv_analysis_revised.R` runs the Spearman correlation analysis to detect persistence in viral kinetic parameters within individuals across first and second infections. 


---

## Reviews

-[ ]