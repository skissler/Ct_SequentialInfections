library(tidyverse) 

# =============================================================================
# Calculate correlations
# =============================================================================

# Peak: -----------------------------------------------------------------------
log_dp_mean_df <- tibble(log_dp_mean=fitlist$log_dp_mean) %>% 
	mutate(iteration=1:n())

log_dpadj_df <- as_tibble(fitlist$log_dpadj,.name_repair="unique") %>% 
	mutate(iteration=1:n()) %>% 
	pivot_longer(-iteration) %>% 
	mutate(name=as.numeric(substr(name,4,1000))) %>% 
	rename(category=name, log_dpadj=value)

log_dp_sd_df <- tibble(log_dp_sd=fitlist$log_dp_sd) %>% 
	mutate(iteration=1:n())

dp_raw_df <- as_tibble(fitlist$dp_raw,.name_repair="unique") %>% 
	mutate(iteration=1:n()) %>% 
	pivot_longer(-iteration) %>% 
	mutate(name=as.numeric(substr(name,4,1000))) %>% 
	rename(id_clean=name, dp_raw=value)

dp_cor_df <- params_indiv %>% 
	left_join(log_dp_mean_df, by="iteration") %>% 
	left_join(log_dpadj_df, by=c("iteration","category")) %>% 
	left_join(log_dp_sd_df, by="iteration") %>% 
	left_join(dp_raw_df, by=c("iteration","id_clean")) %>% 
	mutate(dp0=exp(log_dp_mean + log_dpadj + log_dp_sd*dp_raw)*(run_pars$dp_midpoint)) %>% 
	filter(category%in%c(3,4)) %>% 
	left_join(
		ct_dat_refined %>% 
			group_by(InfectionEvent) %>% 
			slice(1) %>% 
			select(PersonID, InfectionEvent, InfNum),
		by=c("id"="InfectionEvent")) %>% 
	split(.$iteration) %>% 
	map(~ select(., PersonID, InfNum, dp0)) %>% 
	map(~ arrange(., PersonID, InfNum)) %>% 
	map(~ mutate(., InfNum=case_when(InfNum==1~"first",InfNum==2~"second",TRUE~NA_character_))) %>% 
	map(~ pivot_wider(., names_from=InfNum, values_from=dp0)) %>% 
	map(~ filter(., !is.na(first) & !is.na(second))) %>% 
	map(~ c(cor(.$first, .$second, method="pearson"),cor(.$first, .$second, method="spearman"))) %>% 
	map(~ tibble(pcor=.[1],spcor=.[2])) %>% 
	bind_rows(.id="iteration") 

# Proliferation time: ---------------------------------------------------------
log_wp_mean_df <- tibble(log_wp_mean=fitlist$log_wp_mean) %>% 
	mutate(iteration=1:n())

log_wpadj_df <- as_tibble(fitlist$log_wpadj,.name_repair="unique") %>% 
	mutate(iteration=1:n()) %>% 
	pivot_longer(-iteration) %>% 
	mutate(name=as.numeric(substr(name,4,1000))) %>% 
	rename(category=name, log_wpadj=value)

log_wp_sd_df <- tibble(log_wp_sd=fitlist$log_wp_sd) %>% 
	mutate(iteration=1:n())

wp_raw_df <- as_tibble(fitlist$wp_raw,.name_repair="unique") %>% 
	mutate(iteration=1:n()) %>% 
	pivot_longer(-iteration) %>% 
	mutate(name=as.numeric(substr(name,4,1000))) %>% 
	rename(id_clean=name, wp_raw=value)

wp_cor_df <- params_indiv %>% 
	left_join(log_wp_mean_df, by="iteration") %>% 
	left_join(log_wpadj_df, by=c("iteration","category")) %>% 
	left_join(log_wp_sd_df, by="iteration") %>% 
	left_join(wp_raw_df, by=c("iteration","id_clean")) %>% 
	mutate(wp0=exp(log_wp_mean + log_wpadj + log_wp_sd*wp_raw)*(run_pars$wp_midpoint)) %>% 
	filter(category%in%c(3,4)) %>% 
	left_join(
		ct_dat_refined %>% 
			group_by(InfectionEvent) %>% 
			slice(1) %>% 
			select(PersonID, InfectionEvent, InfNum),
		by=c("id"="InfectionEvent")) %>% 
	split(.$iteration) %>% 
	map(~ select(., PersonID, InfNum, wp0)) %>% 
	map(~ arrange(., PersonID, InfNum)) %>% 
	map(~ mutate(., InfNum=case_when(InfNum==1~"first",InfNum==2~"second",TRUE~NA_character_))) %>% 
	map(~ pivot_wider(., names_from=InfNum, values_from=wp0)) %>% 
	map(~ filter(., !is.na(first) & !is.na(second))) %>% 
	map(~ c(cor(.$first, .$second, method="pearson"),cor(.$first, .$second, method="spearman"))) %>% 
	map(~ tibble(pcor=.[1],spcor=.[2])) %>% 
	bind_rows(.id="iteration") 

# Clearance time: -------------------------------------------------------------
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

wr_cor_df <- params_indiv %>% 
	left_join(log_wr_mean_df, by="iteration") %>% 
	left_join(log_wradj_df, by=c("iteration","category")) %>% 
	left_join(log_wr_sd_df, by="iteration") %>% 
	left_join(wr_raw_df, by=c("iteration","id_clean")) %>% 
	mutate(wr0=exp(log_wr_mean + log_wradj + log_wr_sd*wr_raw)*(run_pars$wr_midpoint)) %>% 
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
	map(~ c(cor(.$first, .$second, method="pearson"),cor(.$first, .$second, method="spearman"))) %>% 
	map(~ tibble(pcor=.[1],spcor=.[2])) %>% 
	bind_rows(.id="iteration") 

# =============================================================================
# Plot
# =============================================================================

fig_spcor_dp <- dp_cor_df %>% 
	ggplot(aes(x=spcor)) + 
		geom_histogram(aes(y=..density..), size=0.2, fill="white", col="black",binwidth=0.05) + 
		geom_density(size=0.75, adjust=2) + 
		scale_x_continuous(limits=c(-0.5, 0.75)) + 
		theme_classic() + 
		labs(x="Spearman correlation", y="Density", title="Peak viral concentration")

fig_spcor_wp <- wp_cor_df %>% 
	ggplot(aes(x=spcor)) + 
		geom_histogram(aes(y=..density..), size=0.2, fill="white", col="black",binwidth=0.05) + 
		geom_density(size=0.75, adjust=2) + 
		scale_x_continuous(limits=c(-0.5, 0.75)) + 
		theme_classic() + 
		labs(x="Spearman correlation", y="Density", title="Proliferation time")

fig_spcor_wr <- wr_cor_df %>% 
	ggplot(aes(x=spcor)) + 
		geom_histogram(aes(y=..density..), size=0.2, fill="white", col="black",binwidth=0.05) + 
		geom_density(size=0.75, adjust=2) + 
		scale_x_continuous(limits=c(-0.5, 0.75)) + 
		theme_classic() + 
		labs(x="Spearman correlation", y="Density", title="Clearance time")


# =============================================================================
# Summaries
# =============================================================================

spcor_summary <- bind_rows(
	mutate(dp_cor_df, var="dp"),
	mutate(wp_cor_df, var="wp"),
	mutate(wr_cor_df, var="wr")) %>% 
	group_by(var) %>% 
	summarise(mean=mean(spcor), lwr=quantile(spcor, 0.025), upr=quantile(spcor, 0.975)) %>% 
	mutate(string=paste0(round(mean,2)," (",round(lwr,2),", ",round(upr,2),")"))

pcor_summary <- bind_rows(
	mutate(dp_cor_df, var="dp"),
	mutate(wp_cor_df, var="wp"),
	mutate(wr_cor_df, var="wr")) %>% 
	group_by(var) %>% 
	summarise(mean=mean(pcor), lwr=quantile(pcor, 0.025), upr=quantile(pcor, 0.975)) %>% 
	mutate(string=paste0(round(mean,2)," (",round(lwr,2),", ",round(upr,2),")"))

# =============================================================================
# Testing 
# =============================================================================

par0df <- params_indiv %>% 
	left_join(log_dp_mean_df, by="iteration") %>% 
	left_join(log_dpadj_df, by=c("iteration","category")) %>% 
	left_join(log_dp_sd_df, by="iteration") %>% 
	left_join(dp_raw_df, by=c("iteration","id_clean")) %>% 
	left_join(log_wp_mean_df, by="iteration") %>% 
	left_join(log_wpadj_df, by=c("iteration","category")) %>% 
	left_join(log_wp_sd_df, by="iteration") %>% 
	left_join(wp_raw_df, by=c("iteration","id_clean")) %>% 
	left_join(log_wr_mean_df, by="iteration") %>% 
	left_join(log_wradj_df, by=c("iteration","category")) %>% 
	left_join(log_wr_sd_df, by="iteration") %>% 
	left_join(wr_raw_df, by=c("iteration","id_clean")) %>% 
	mutate(dp0=exp(log_dp_mean + log_dpadj + log_dp_sd*dp_raw)*(run_pars$dp_midpoint)) %>% 
	mutate(wp0=exp(log_wp_mean + log_wpadj + log_wp_sd*wp_raw)*(run_pars$wp_midpoint)) %>% 
	mutate(wr0=exp(log_wr_mean + log_wradj + log_wr_sd*wr_raw)*(run_pars$wr_midpoint)) %>% 
	filter(category%in%c(3,4)) %>% 
	left_join(
		ct_dat_refined %>% 
			group_by(InfectionEvent) %>% 
			slice(1) %>% 
			select(PersonID, InfectionEvent, InfNum),
		by=c("id"="InfectionEvent")) %>% 
	select(iteration, id_clean, id, category, PersonID, dp0, wp0, wr0)

# temp2 <- par0df %>% 
# 	group_by(PersonID, id) %>% 
# 	summarise(category=first(category), dp0=mean(dp0), wp0=mean(wp0), wr0=mean(wr0)) %>% 
# 	group_by(category) %>% 
# 	arrange(dp0) %>% 
# 	mutate(dprank=1:n()) %>% 
# 	arrange(wp0) %>% 
# 	mutate(wprank=1:n()) %>% 
# 	arrange(wr0) %>% 
# 	mutate(wrrank=1:n()) %>% 
# 	arrange(PersonID, category) %>% 
# 	group_by(PersonID) %>% 
# 	mutate(dpassocrank=lag(dprank)) %>% 
# 	mutate(wpassocrank=lag(wprank)) %>% 
# 	mutate(wrassocrank=lag(wrrank)) %>% 
# 	arrange(PersonID, desc(category)) %>% 
# 	group_by(PersonID) %>% 
# 	mutate(dpassocrank=case_when(is.na(dpassocrank)~lag(dprank), TRUE~dpassocrank)) %>% 
# 	mutate(wpassocrank=case_when(is.na(wpassocrank)~lag(wprank), TRUE~wpassocrank)) %>% 
# 	mutate(wrassocrank=case_when(is.na(wrassocrank)~lag(wrrank), TRUE~wrassocrank)) 

# fig_dpreg <- temp2 %>% 
# 	ggplot(aes(x=dprank, y=dpassocrank)) + 
# 		geom_point() + 
# 		geom_smooth(method="lm", col="black") +
# 		theme_classic() 


# fig_wpreg <- temp2 %>% 
# 	ggplot(aes(x=wprank, y=wpassocrank)) + 
# 		geom_point() + 
# 		geom_smooth(method="lm", col="black") +
# 		theme_classic() 

# fig_wrreg <- temp2 %>% 
# 	ggplot(aes(x=wrrank, y=wrassocrank)) + 
# 		geom_point() + 
# 		geom_smooth(method="lm", col="black") +
# 		theme_classic() 

# fig_allreg <- temp2 %>% 
# 	select(PersonID, category, dp0, wp0, wr0) %>% 
# 	pivot_longer(c("dp0","wp0","wr0")) %>% 
# 	pivot_wider(names_from=category, values_from=value) %>% 
# 	ggplot(aes(x=`3`,y=`4`)) + 
# 		geom_point() + 
# 		theme_classic() + 
# 		geom_smooth(method="lm", col="black") + 
# 		facet_wrap(~name, scales="free") 

corfig <- par0df %>% 
	select(iteration, PersonID, category, dp0, wp0, wr0) %>% 
	mutate(`Peak viral load`=40-dp0) %>% 
	mutate(`Proliferation time`=wp0) %>% 
	mutate(`Clearance time`=wr0) %>% 
	select(iteration,PersonID, category, `Peak viral load`, `Proliferation time`, `Clearance time`) %>% 
	pivot_longer(c("Peak viral load","Proliferation time","Clearance time")) %>% 
	pivot_wider(names_from=category, values_from=value) %>% 
	arrange(PersonID, iteration) %>% 
	filter(iteration %in% sample(1:2000, 200)) %>% 
	mutate(units=case_when(
		name=="Peak viral load"~"Ct", 
		name=="Proliferation time"~"days",
		name=="Clearance time"~"days")) %>% 
	mutate(name=factor(name, levels=c("Peak viral load","Proliferation time", "Clearance time"))) %>% 
	split(.$name) %>% 
	map(~ (ggplot(data=., aes(x=(`3`), y=(`4`))) + 
			geom_point(alpha=0.01, size=0.25) + 
			geom_density_2d(adjust=2, bins=4, size=0.5, col="black", alpha=0.6) + 
			geom_line(stat="smooth", method="lm", col="black", size=0.4, lty="dashed") + 
			ifelse(.$units[1]=="Ct",list(scale_x_reverse()), list()) + 
			ifelse(.$units[1]=="Ct",list(scale_y_reverse()), list()) + 
			labs(title=.$name, 
				x=paste0(.$name, ", first infection (",.$units[1],")"), 
				y=paste0(.$name, ", second infection (",.$units[1],")")) + 
			theme_classic() + 
			theme(text=element_text(size=9))))

# for(indexA in 1:length(corfig)){
# 	ggsave(corfig[[indexA]], file=paste0("figures/",paste0("corfig",indexA,".pdf")), width=2.5, height=2.5, dpi=600, units="in")
# 	ggsave(corfig[[indexA]], file=paste0("figures/",paste0("corfig",indexA,".png")), width=2.5, height=2.5, dpi=600, units="in")
# }


