# FIRST INFECTION =============================================================

# By variant:
ct_dat_refined %>% 
	group_by(InfectionEvent) %>% 
	slice(1) %>% 
	filter(InfNum==1) %>%
	mutate(LineageBroad=case_when(
		LineageBroad=="BA.1"~"BA.1/BA.2", 
		LineageBroad=="BA.2"~"BA.1/BA.2", 
		LineageBroad=="BA.4"~"BA.4/BA.5", 
		LineageBroad=="BA.5"~"BA.4/BA.5",
		LineageBroad=="Other"~"Other/Unspecified",
		LineageBroad=="None"~"Other/Unspecified",
		TRUE~LineageBroad 
		)) %>% 
	mutate(LineageBroad=factor(LineageBroad, levels=c("Alpha","Delta","BA.1/BA.2","BA.4/BA.5","Other/Unspecified"))) %>% 
	group_by(LineageBroad) %>% 
	summarise(N=n()) %>% 
	ungroup() %>% 
	mutate(pct=round(100*N/sum(N),1)) %>% 
	print(n=Inf)

# By variant + vax status:
ct_dat_refined %>% 
	group_by(InfectionEvent) %>% 
	slice(1) %>% 
	filter(InfNum==1) %>%
	mutate(LineageBroad=case_when(
		LineageBroad=="BA.1"~"BA.1/BA.2", 
		LineageBroad=="BA.2"~"BA.1/BA.2", 
		LineageBroad=="BA.4"~"BA.4/BA.5", 
		LineageBroad=="BA.5"~"BA.4/BA.5",
		LineageBroad=="Other"~"Other/Unspecified",
		LineageBroad=="None"~"Other/Unspecified",
		TRUE~LineageBroad 
		)) %>% 
	mutate(LineageBroad=factor(LineageBroad, levels=c("Alpha","Delta","BA.1/BA.2","BA.4/BA.5","Other/Unspecified"))) %>% 
	mutate(VaccinationStatus=case_when(is.na(VaccinationStatus)~"Not Reported",TRUE~VaccinationStatus)) %>% 
	mutate(VaccinationStatus=factor(VaccinationStatus, levels=c("Not Vaccinated","Fully Vaccinated","Not Reported"))) %>% 
	group_by(LineageBroad, VaccinationStatus) %>% 
	summarise(N=n()) %>% 
	ungroup() %>% 
	mutate(pct=round(100*N/sum(N),1)) %>% 
	print(n=Inf)


# By variant + boost status:
ct_dat_refined %>% 
	group_by(InfectionEvent) %>% 
	slice(1) %>% 
	filter(InfNum==1) %>%
	mutate(LineageBroad=case_when(
		LineageBroad=="BA.1"~"BA.1/BA.2", 
		LineageBroad=="BA.2"~"BA.1/BA.2", 
		LineageBroad=="BA.4"~"BA.4/BA.5", 
		LineageBroad=="BA.5"~"BA.4/BA.5",
		LineageBroad=="Other"~"Other/Unspecified",
		LineageBroad=="None"~"Other/Unspecified",
		TRUE~LineageBroad 
		)) %>% 
	mutate(LineageBroad=factor(LineageBroad, levels=c("Alpha","Delta","BA.1/BA.2","BA.4/BA.5","Other/Unspecified"))) %>% 
	mutate(BoosterStatus=case_when(is.na(BoosterStatus)~"Not Reported",TRUE~BoosterStatus)) %>% 
	mutate(BoosterStatus=factor(BoosterStatus, levels=c("Not Boosted","Boosted","Not Reported"))) %>% 
	group_by(LineageBroad, BoosterStatus) %>% 
	summarise(N=n()) %>% 
	ungroup() %>% 
	mutate(pct=round(100*N/sum(N),1)) %>% 
	print(n=Inf)


# By age group:
ct_dat_refined %>% 
	group_by(InfectionEvent) %>% 
	slice(1) %>% 
	filter(InfNum==1) %>%
	group_by(AgeGrp) %>% 
	summarise(N=n()) %>% 
	ungroup() %>% 
	mutate(pct=round(100*N/sum(N),1)) %>% 
	print(n=Inf)





# SECOND INFECTION ===========================================================

# By variant:
ct_dat_refined %>% 
	group_by(InfectionEvent) %>% 
	slice(1) %>% 
	filter(InfNum==2) %>%
	mutate(LineageBroad=case_when(
		LineageBroad=="BA.1"~"BA.1/BA.2", 
		LineageBroad=="BA.2"~"BA.1/BA.2", 
		LineageBroad=="BA.4"~"BA.4/BA.5", 
		LineageBroad=="BA.5"~"BA.4/BA.5",
		LineageBroad=="Other"~"Other/Unspecified",
		LineageBroad=="None"~"Other/Unspecified",
		TRUE~LineageBroad 
		)) %>% 
	mutate(LineageBroad=factor(LineageBroad, levels=c("Alpha","Delta","BA.1/BA.2","BA.4/BA.5","Other/Unspecified"))) %>% 
	group_by(LineageBroad) %>% 
	summarise(N=n()) %>% 
	ungroup() %>% 
	mutate(pct=round(100*N/sum(N),1)) %>% 
	print(n=Inf)

# By variant + vax status:
ct_dat_refined %>% 
	group_by(InfectionEvent) %>% 
	slice(1) %>% 
	filter(InfNum==2) %>%
	mutate(LineageBroad=case_when(
		LineageBroad=="BA.1"~"BA.1/BA.2", 
		LineageBroad=="BA.2"~"BA.1/BA.2", 
		LineageBroad=="BA.4"~"BA.4/BA.5", 
		LineageBroad=="BA.5"~"BA.4/BA.5",
		LineageBroad=="Other"~"Other/Unspecified",
		LineageBroad=="None"~"Other/Unspecified",
		TRUE~LineageBroad 
		)) %>% 
	mutate(LineageBroad=factor(LineageBroad, levels=c("Alpha","Delta","BA.1/BA.2","BA.4/BA.5","Other/Unspecified"))) %>% 
	mutate(VaccinationStatus=case_when(is.na(VaccinationStatus)~"Not Reported",TRUE~VaccinationStatus)) %>% 
	mutate(VaccinationStatus=factor(VaccinationStatus, levels=c("Not Vaccinated","Fully Vaccinated","Not Reported"))) %>% 
	group_by(LineageBroad, VaccinationStatus) %>% 
	summarise(N=n()) %>% 
	ungroup() %>% 
	mutate(pct=round(100*N/sum(N),1)) %>% 
	print(n=Inf)


# By variant + boost status:
ct_dat_refined %>% 
	group_by(InfectionEvent) %>% 
	slice(1) %>% 
	filter(InfNum==2) %>%
	mutate(LineageBroad=case_when(
		LineageBroad=="BA.1"~"BA.1/BA.2", 
		LineageBroad=="BA.2"~"BA.1/BA.2", 
		LineageBroad=="BA.4"~"BA.4/BA.5", 
		LineageBroad=="BA.5"~"BA.4/BA.5",
		LineageBroad=="Other"~"Other/Unspecified",
		LineageBroad=="None"~"Other/Unspecified",
		TRUE~LineageBroad 
		)) %>% 
	mutate(LineageBroad=factor(LineageBroad, levels=c("Alpha","Delta","BA.1/BA.2","BA.4/BA.5","Other/Unspecified"))) %>% 
	mutate(BoosterStatus=case_when(is.na(BoosterStatus)~"Not Reported",TRUE~BoosterStatus)) %>% 
	mutate(BoosterStatus=factor(BoosterStatus, levels=c("Not Boosted","Boosted","Not Reported"))) %>% 
	group_by(LineageBroad, BoosterStatus) %>% 
	summarise(N=n()) %>% 
	ungroup() %>% 
	mutate(pct=round(100*N/sum(N),1)) %>% 
	print(n=Inf)


# By age group:
ct_dat_refined %>% 
	group_by(InfectionEvent) %>% 
	slice(1) %>% 
	filter(InfNum==2) %>%
	group_by(AgeGrp) %>% 
	summarise(N=n()) %>% 
	ungroup() %>% 
	mutate(pct=round(100*N/sum(N),1)) %>% 
	print(n=Inf)

