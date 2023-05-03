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

- [ ] Conduct restricted analysis, considdering only individuals with well-documented first and second infections 
- [ ] Complete editorial policy checklist
- [ ] Complete reporting summary 
- [ ] Note in cover letter that sex/gender data were not systematically collected and so not analyzed 


- [ ] Find a way to make the comparisons even more concrete... 
- [ ] Generate schematic to summarize the overall conclusions in the various groups 
- [ ] Check if there is a relationship between the number of exposures and the clearance rate 
- [ ] Describe statistical models in full, including: the form of the model, the parameters that are estimated, and the priors. Also: which Ct values were below LOD? How were these treated? Consider incorporating these rather than fully excluding them. 
- [ ] Clarify that the pairwise clearance analysis really is just Spearman correlation. Maybe reduce some of the explanatory text there. Consider using standard approaches to calculate the p-value rather than bootstrapping. 
- [ ] Consider including the clearance time consistency in the model itself, or at least including uncertainty in the clearance time estimates in the spearman correlation. 


- [ ] Re-phrase the caption in figure 1: "I would suggest to rephrase it and say "no well-documented pre-Omicron infection" or something similar"
- [ ] Come up with a simplified representation of the comparison groups and present it as Fig 1 panel C (fits in with a previous comment too) - because Supp Table 1 is too confusing. 
- [ ] Explain unexpected differences in Supp Table 1 counts in the caption. 
- [ ] 88-90: define mean proliferation time, clearance time, and peak viral load. 
- [ ] Note that people continued testing daily when possible even after testing positive 
- [ ] Ask Joseph about standard curve stability over time 
- [ ] Note brackets in Supp Table 2 are 95% CIs. 
- [ ] Note in the Discussion/limitations that we didn't measure infectious virus. 


- [ ] Clarify "summary of recorded infections" since Supp Table 1 is hard to read. 
- [ ] Clarify who is being compared in lines 90-96 
- [ ] Figure 1B: clarify why n = 235 in the caption is greater than 193 in the text. 
- [ ] Focus main figures on 58 infections with well-documented first and second infections and leave the other analyses to the supplement... (I don't agree) 
- [ ] Repeat Figure 2 but only including the 58 with two well-documented infections. 
- [ ] Explain why the confidence intervals in Figure 2A are narrow
- [ ] 115-117: explain what "greater symmetry" means (rephrase, and maybe a brief mention of why we care) 
- [ ] 123-126: Clarify differences in numbers here. 








