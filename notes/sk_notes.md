__Jump to:__ 

- [5 May 2023](#5-May-2023)

# 5 May 2023

I've just fit the model to the full dataset but specially extracted the results for first/second infections in those with two well-documented infections. That lets me pull information about the adjustment factors (age, variant, vaccination status) from the full dataset, but to focus the analysis on the people with two well-documented infections. I think that's what we want. 

Next step is figuring out how to incorporate this into the paper's narrative. As it stands: 

- In omicron (BA.1/BA.2) infections, prior infection correlates with faster clearance. 
- In individuals with multiple infections, second infections are cleared faster than first infections. 
- The kinetics of the second infection don't depend on the lineage of the first infection 
- Clearance speed is conserved across infections. 

The idea now is to focus the entire analysis on the people who have two well-documented infections, and then to incorporate any supporting information from other subsets of the population as minor/supplementary points. 

So, the way forward I think is to do something like this: 

- In individuals with two well-documented infections, second infections are cleared more quickly than first infections. 
- This holds more generally: among all omicron infections, we see faster clearance when people had a prior infection (well-documented or not), even when we adjust for age and vaccination status. 
- In the people with two well-documented infections, the lineage of the first infection doesn't impact the kinetics of the second. 
- However, we do see that fast clearers for first infections also tend to be fast clearers in their second infection, and _vice versa_. 

Let's pin this down. The biggest outstanding thing is to repeat the spearman correlation. I should also incorporate uncertainty in that calculation. I might want a new script for that. 

I think the most straightforward thing for the spearman correlation is to compare adjusted clearance times - so adjust for age, variant, and vaccination status directly, and compare the ranks across all posterior draws. 

