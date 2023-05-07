library(tidyverse) 

log_wr_mean_df <- tibble(log_wr_mean=fitlist$log_wr_mean) %>% 
	mutate(iteration=1:n())

log_wradj_df <- as_tibble(fitlist$log_wradj,.name_repair="unique") %>% 
	mutate(iteration=1:n()) %>% 
	pivot_longer(-iteration) %>% 
	mutate(name=as.numeric(substr(name,4,1000))) %>% 
	rename(category=name, log_wradj=value)

log_wr_sd_df <- tibble(log_wr_sd=fitlist$log_wr_sd) %>% 
	mutate(iteration=1:n())

wr_raw_df <- as_tibble(fitlist$wr_raw,.name_repair="unique") %>% 
	mutate(iteration=1:n()) %>% 
	pivot_longer(-iteration) %>% 
	mutate(name=as.numeric(substr(name,4,1000))) %>% 
	rename(id_clean=name, wr_raw=value)

temp <- params_indiv %>% 
	left_join(log_wr_mean_df, by="iteration") %>% 
	left_join(log_wradj_df, by=c("iteration","category")) %>% 
	left_join(log_wr_sd_df, by="iteration") %>% 
	left_join(wr_raw_df, by=c("iteration","id_clean"))

temp2 <- temp %>% 
	mutate(wr0=exp(log_wr_mean + log_wradj + log_wr_sd*wr_raw)*(run_pars$wr_midpoint))

# for each iteration: calculate the spearman correlation between first-infection and second-infection onset times, for the people with two well-documented infections. 

temp3 <- temp2 %>% 
	filter(category%in%c(3,4)) %>% 
	left_join(
		ct_dat_refined %>% 
			group_by(InfectionEvent) %>% 
			slice(1) %>% 
			select(PersonID, InfectionEvent, InfNum),
		by=c("id"="InfectionEvent")) %>% 
	split(.$iteration) %>% 
	map(~ select(., PersonID, InfNum, wr0)) %>% 
	map(~ arrange(., PersonID, InfNum)) %>% 
	map(~ mutate(., InfNum=case_when(InfNum==1~"first",InfNum==2~"second",TRUE~NA_character_))) %>% 
	map(~ pivot_wider(., names_from=InfNum, values_from=wr0)) %>% 
	map(~ filter(., !is.na(first) & !is.na(second))) %>% 
	map(~ cor(.$first, .$second, method="spearman"))

temp4 <- temp3 %>% 	
	map(~ tibble(spcor=.)) %>% 
	bind_rows(.id="iteration") 

fig_spcor <- temp4 %>% 
	ggplot(aes(x=spcor)) + 
		geom_histogram(aes(y=..density..), size=0.2, fill="white", col="black") + 
		geom_density(size=0.75, adjust=2) + 
		theme_classic() 


