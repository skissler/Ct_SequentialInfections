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
	map(~ cor(.$first, .$second, method="spearman")) %>% 
	map(~ tibble(spcor=.)) %>% 
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
	map(~ cor(.$first, .$second, method="spearman")) %>% 
	map(~ tibble(spcor=.)) %>% 
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
	map(~ cor(.$first, .$second, method="spearman")) %>% 
	map(~ tibble(spcor=.)) %>% 
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

# =============================================================================
# Testing 
# =============================================================================

# wr
# temp %>% 
# 	bind_rows(.id="iteration") %>% 
# 	ggplot(aes(x=(first),y=(second))) + 
# 	geom_point(size=0.01, alpha=0.05) + 
# 	geom_line(stat="smooth",method="lm")

# # dp 
# temp2 %>% 
# 	bind_rows(.id="iteration") %>% 
# 	ggplot(aes(x=(first),y=(second))) + 
# 	geom_point(size=0.01, alpha=0.05) + 
# 	geom_line(stat="smooth",method="lm")






