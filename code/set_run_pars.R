run_pars_list <- list(
	# 1) Measure raw differences between 1st and 2nd infections 
	list(excluded_rows=(ct_dat_refined %>% 
			filter(InPairedAnalysis==0) %>% 
			pull(RowID)),
		analysis_rows=list(
			(ct_dat_refined %>% 
				filter(InfNum==1) %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(InfNum==2) %>% 
				pull(RowID))
			), 
		adjustment_rows=list(list(
			(ct_dat_refined %>% 
				filter(AgeGrp=="[0,30)") %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(AgeGrp=="[30,50)" | is.na(AgeGrp)) %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(AgeGrp=="[50,100)") %>% 
				pull(RowID))
			)), # NULL
		analysis_names=c("First Infection","Second Infection"), 
		analysis_title="first vs. second infection", 
		adjustment_names=list(c("0-29","30-49","50+")), 
		adjustment_title=c("age group"), 
		tp_prior=c(0,2),
		dp_midpoint=20,
		wp_midpoint=5,
		wr_midpoint=12,
		sigma_prior=c(0,0.5),
		lambda=0.01,
		fpmean=1/log(10)
		), 
	# 2) Measure raw differences between variants: 
	list(excluded_rows=(ct_dat_refined %>% 
			filter(InPairedAnalysis==0) %>% 
			pull(RowID)),
		analysis_rows=list(
			(ct_dat_refined %>% 
				filter(LineageBroad%in%c("Other","None")) %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(LineageBroad=="Alpha") %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(LineageBroad=="Delta") %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(LineageBroad%in%c("BA.1","BA.2")) %>% 
				pull(RowID))
			), 
		adjustment_rows=list(list(
			(ct_dat_refined %>% 
				filter(AgeGrp=="[0,30)") %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(AgeGrp=="[30,50)" | is.na(AgeGrp)) %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(AgeGrp=="[50,100)") %>% 
				pull(RowID))
			)), # NULL
		analysis_names=c("Other/None","Alpha","Delta","BA.1/BA.2"), 
		analysis_title="variant", 
		adjustment_names=list(c("0-29","30-49","50+")), 
		adjustment_title=c("age group"), 
		tp_prior=c(0,2),
		dp_midpoint=20,
		wp_midpoint=5,
		wr_midpoint=12,
		sigma_prior=c(0,0.5),
		lambda=0.01,
		fpmean=1/log(10)
		), 
	# 3) Measure differences in omicron trajectories by prior variant: 
	list(excluded_rows=(ct_dat_refined %>% 
			filter(!(InPairedAnalysis==1 & InfNum==2)) %>% 
			pull(RowID)),
		analysis_rows=list(
			(ct_dat_refined %>% 
				filter(PrevLineageBroad%in%c("Other","None")) %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(PrevLineageBroad=="Alpha") %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(PrevLineageBroad=="Delta") %>% 
				pull(RowID))
			), 
		adjustment_rows=list(list(
			(ct_dat_refined %>% 
				filter(AgeGrp=="[0,30)") %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(AgeGrp=="[30,50)" | is.na(AgeGrp)) %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(AgeGrp=="[50,100)") %>% 
				pull(RowID))
			)), # NULL
		analysis_names=c("Other/None","Alpha","Delta"), 
		analysis_title="previous Lineage", 
		adjustment_names=list(c("0-29","30-49","50+")), 
		adjustment_title=c("age group"), 
		tp_prior=c(0,2),
		dp_midpoint=20,
		wp_midpoint=5,
		wr_midpoint=12,
		sigma_prior=c(0,0.5),
		lambda=0.01,
		fpmean=1/log(10)
		),
	# 4) Measure differences in 2nd omicron infections vs other omicrons: 
	list(excluded_rows=(ct_dat_refined %>% 
			filter(!(LineageBroad%in%c("BA.1","BA.2") & InfNum%in%c(1,2))) %>% 
			pull(RowID)),
		analysis_rows=list(
			(ct_dat_refined %>% 
				filter(InfNum==1) %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(InfNum==2) %>% 
				pull(RowID))
			), 
		adjustment_rows=list(list(
			(ct_dat_refined %>% 
				filter(AgeGrp=="[0,30)") %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(AgeGrp=="[30,50)" | is.na(AgeGrp)) %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(AgeGrp=="[50,100)") %>% 
				pull(RowID))
			)), # NULL
		analysis_names=c("First infection","Repeat infection"), 
		analysis_title="presence of prior infection (BA.1/BA.2 only)", 
		adjustment_names=list(c("0-29","30-49","50+")), 
		adjustment_title=c("age group"), 
		tp_prior=c(0,2),
		dp_midpoint=20,
		wp_midpoint=5,
		wr_midpoint=12,
		sigma_prior=c(0,0.5),
		lambda=0.01,
		fpmean=1/log(10)
		),
	# 5) Measure differences in 2nd omicron infections vs first omicron infections in boosted and unboosted individuals (not subset to the multiply-infected individuals): 
	list(excluded_rows=((ct_dat_refined %>% 
			filter(!(LineageBroad%in%c("BA.1","BA.2") & (
				BoosterStatus%in%c("Not Boosted","Boosted")) )) %>% 
			pull(RowID))),
		analysis_rows=list(
			(ct_dat_refined %>% 
				filter(InfNum==1 & BoosterStatus=="Not Boosted") %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(InfNum==1 & BoosterStatus=="Boosted") %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(InfNum==2 & BoosterStatus=="Not Boosted") %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(InfNum==2 & BoosterStatus=="Boosted") %>% 
				pull(RowID))
			), 
		adjustment_rows=list(list(
			(ct_dat_refined %>% 
				filter(AgeGrp=="[0,30)") %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(AgeGrp=="[30,50)" | is.na(AgeGrp)) %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(AgeGrp=="[50,100)") %>% 
				pull(RowID))
			)), # NULL
		analysis_names=c("First infection, unboosted","First infection, boosted","Repeat infection, unboosted","Repeat infection, boosted"), 
		analysis_title="infection order and booster status", 
		adjustment_names=list(c("0-29","30-49","50+")), 
		adjustment_title=c("age group"), 
		tp_prior=c(0,2),
		dp_midpoint=20,
		wp_midpoint=5,
		wr_midpoint=12,
		sigma_prior=c(0,0.5),
		lambda=0.01,
		fpmean=1/log(10)
		),
	# 6) Measure differences between 1st and 2nd infections, stratifying by vaccination status (first infection) or presence of an intervening vaccine dose (second infection) 
	list(excluded_rows=(ct_dat_refined %>% 
			filter(!(InPairedAnalysis==1 & VaccinationStatus%in%c("Not Vaccinated","Fully Vaccinated") & BoosterStatus%in%c("Not Boosted","Boosted"))) %>% 
			pull(RowID)),
		analysis_rows=list(
			(ct_dat_refined %>% 
				filter(InfNum==1 & VaccinationStatus=="Not Vaccinated") %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(InfNum==1 & VaccinationStatus=="Fully Vaccinated") %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(InterveningDose==0) %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(InterveningDose==1) %>% 
				pull(RowID))
			), 
		adjustment_rows=list(list(
			(ct_dat_refined %>% 
				filter(AgeGrp=="[0,30)") %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(AgeGrp=="[30,50)" | is.na(AgeGrp)) %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(AgeGrp=="[50,100)") %>% 
				pull(RowID))
			)), # NULL
		analysis_names=c("First Infection, unvaccinated","First infection, vaccinated","Second Infection, no intervening dose", "Second infection, intervening dose"), 
		analysis_title="first vs. second infection with vaccination status", 
		adjustment_names=list(c("0-29","30-49","50+")), 
		adjustment_title=c("age group"), 
		tp_prior=c(0,2),
		dp_midpoint=20,
		wp_midpoint=5,
		wr_midpoint=12,
		sigma_prior=c(0,0.5),
		lambda=0.01,
		fpmean=1/log(10)
		),
	# 7) Measure raw differences between 1st and 2nd infections, restricting to the people with two well-documented infections
	list(excluded_rows=(ct_dat_refined %>% 
			filter(WD2==0 | InfNum>2) %>% 
			pull(RowID)),
		analysis_rows=list(
			(ct_dat_refined %>% 
				filter(InfNum==1) %>% 
				pull(RowID)),
			(ct_dat_refined %>% 
				filter(InfNum==2) %>% 
				pull(RowID))
			), 
		adjustment_rows=list(
			# Adjust by age group:
			list(
				(ct_dat_refined %>% 
					filter(AgeGrp=="[0,30)") %>% 
					pull(RowID)),
				(ct_dat_refined %>% 
					filter(AgeGrp=="[30,50)" | is.na(AgeGrp)) %>% 
					pull(RowID)),
				(ct_dat_refined %>% 
					filter(AgeGrp=="[50,100)") %>% 
					pull(RowID))
				),
			# Adjust by variant: 
			list(
				(ct_dat_refined %>% 
					filter(LineageBroad%in%c("Other","None")) %>% 
					pull(RowID)),
				(ct_dat_refined %>% 
					filter(LineageBroad=="Alpha") %>% 
					pull(RowID)),
				(ct_dat_refined %>% 
					filter(LineageBroad=="Delta") %>% 
					pull(RowID)),
				(ct_dat_refined %>% 
					filter(LineageBroad%in%c("BA.1","BA.2")) %>% 
					pull(RowID)),
				(ct_dat_refined %>% 
					filter(LineageBroad%in%c("BA.4","BA.5")) %>% 
					pull(RowID))
				),
			# Adjust by vaccination status: 
			list(
				(ct_dat_refined %>% 
					filter(VaccinationStatus=="Not Vaccinated" & BoosterStatus=="Not Boosted") %>% 
					pull(RowID)),
				(ct_dat_refined %>% 
					filter(VaccinationStatus=="Not Vaccinated" & BoosterStatus=="Not Reported") %>% 
					pull(RowID)),
				(ct_dat_refined %>% 
					filter(VaccinationStatus=="Fully Vaccinated" & BoosterStatus=="Not Boosted") %>% 
					pull(RowID)),
				(ct_dat_refined %>% 
					filter(VaccinationStatus=="Fully Vaccinated" & BoosterStatus=="Boosted") %>% 
					pull(RowID)),
				(ct_dat_refined %>% 
					filter(VaccinationStatus=="Fully Vaccinated" & BoosterStatus=="Not Reported") %>% 
					pull(RowID)),
				(ct_dat_refined %>% 
					filter(VaccinationStatus=="Not Reported" | is.na(VaccinationStatus) | is.na(BoosterStatus)) %>% 
					pull(RowID))
				)
				), # NULL
		analysis_names=c("First Infection","Second Infection"), 
		analysis_title="first vs. second infection", 
		adjustment_names=list(
			c("0-29","30-49","50+"),
			c("Other/None","Alpha","Delta","BA.1/BA.2","BA.4/BA.5"),
			c("Unvaccinated/Unboosted","Unvaccinated/Unknonwn booster","Vaccinated/Unboosted","Vaccinated/Boosted","Vaccinated/Unknown booster","Not reported")), 
		adjustment_title=c("age group","variant","vax status"), 
		tp_prior=c(0,2),
		dp_midpoint=20,
		wp_midpoint=5,
		wr_midpoint=12,
		sigma_prior=c(0,0.5),
		lambda=0.01,
		fpmean=1/log(10)
		)
	# 8) Measure differences in second infection kinetics stratified by the first infection lineage, in people with two well-documented infections. 
	# 9) Measure differences in 2nd infections by vaccination status, in people with two well-documented infections 
	)

