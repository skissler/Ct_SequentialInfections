

temp <- ct_dat_refined %>% 
	group_by(InfectionEvent) %>% 
	slice(1) %>% 
	group_by(PersonID) %>% 
	mutate(NWD=n()) %>% 
	filter(NWD>1) %>% 
	select(ID=PersonID, IE=InfectionEvent, CtT1, VaxStatus=VaccinationStatus, BoostStatus=BoosterStatus, Lin=LineageBroad, PLin=PrevLineageBroad, InfNum) %>% 
	arrange(ID, InfNum) 

print(temp, n=Inf)


temp %>% 
	mutate(omi=grepl("BA",Lin)) %>% 
	filter(InfNum==2 & omi==TRUE) %>% 
	filter(Lin %in% c("BA.1","BA.2")) %>% 
	print(n=Inf)

# maybe for the analysis: adjust by age and variant (alpha, delta, omicron, other) and vaccination status, and look at effect of first vs second infection.... that might be the fullest analysis. Not sure there will be numbers for this, but we'll see. 

# then maybe also include an adjustment for the lineage of the previous infection, when it's a second infection; and we'll really see nothing I think. 

WD2set <- ct_dat_refined %>% 
	group_by(InfectionEvent) %>% 
	slice(1) %>% 
	group_by(PersonID) %>% 
	summarise(NWD=n()) %>% 
	filter(NWD>1) %>% 
	pull(PersonID)