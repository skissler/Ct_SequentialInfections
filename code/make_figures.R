# =============================================================================
# Import and key definitions
# =============================================================================

library(tidyverse) 
library(beeswarm) 
library(ggbeeswarm) 

fitlist <- extract(ct_fit) 
fitlist_chain <- extract(ct_fit, pars=c("log_dpadj[2]","log_wpadj[2]"), permuted=FALSE) 

categorylabs <- run_pars$analysis_names
categorylabsdf <- tibble(label=categorylabs, id=1:length(categorylabs)) %>% 
		mutate(label=factor(label,levels=categorylabs))
categorytitle <- run_pars$analysis_title
categorycolors <- c("#000000","#4053d3","#ddb310","#b51d14","#00beff","#fb49b0","#00b25d","#cacaca")[1:length(run_pars$analysis_rows)]
categorytags <- c("A)","B)","C)","D)","E)","F)","G)","H)","I)","J)","K)","L)","M)","N)","O)","P)")[1:length(run_pars$analysis_rows)]

adjustmentlabs <- run_pars$adjustment_names
adjustmentlabsdf <- tibble(label=adjustmentlabs, id=1:length(adjustmentlabs)) %>% 
		mutate(label=factor(label,levels=adjustmentlabs))
adjustmenttitle <- run_pars$adjustment_title
adjustmentcolors <- c("#000000","#4053d3","#ddb310","#b51d14","#00beff","#fb49b0","#00b25d","#cacaca")[1:length(run_pars$adjustment_rows)]
adjustmenttags <- c("A)","B)","C)","D)","E)","F)","G)","H)","I)","J)","K)","L)","M)","N)","O)","P)")[1:length(run_pars$adjustment_rows)]

idmap <- indiv_data %>% 
	group_by(id) %>% 
	slice(1) %>% 
	select(id, id_clean) 

params_indiv <- get_wide_output(fitlist, c("tp","dp","wp","wr")) %>% 
	left_join((indiv_data %>% 
			group_by(id_clean) %>% 
			slice(1) %>% 
			select(id_clean, category, adjustment=adj)),
		by=c("id"="id_clean")) %>% 
	rename(id_clean=id) %>% 
	left_join(idmap, by="id_clean")

meanvals_indiv <- params_indiv %>% 
	group_by(id) %>% 
	summarise(tp=mean(tp),dp=mean(dp),wp=mean(wp),wr=mean(wr),category=first(category), adjustment=first(adjustment)) %>% 
	left_join(
		(ct_dat_refined %>% 
			group_by(InfectionEvent) %>% 
			slice(1) %>% 
			ungroup() %>% 
			select(id=InfectionEvent, PersonID)),
		by="id"
		)

summaryvals_indiv <- params_indiv %>% 
	group_by(id) %>% 
	summarise(
		tp_mean=mean(tp),
		tp_lwr=quantile(tp,0.025),
		tp_upr=quantile(tp,0.975),
		dp_mean=mean(dp),
		dp_lwr=quantile(dp,0.025),
		dp_upr=quantile(dp,0.975),
		wp_mean=mean(wp),
		wp_lwr=quantile(wp,0.025),
		wp_upr=quantile(wp,0.975),
		wr_mean=mean(wr),
		wr_lwr=quantile(wr,0.025),
		wr_upr=quantile(wr,0.975),
		category=first(category),
		adjustment=first(adjustment)) %>% 
	rename(id_clean=id) %>% 
	left_join((indiv_data %>% 
			select(id,id_clean) %>% 
			group_by(id) %>% 
			slice(1)),by="id_clean") %>% 
	left_join((ct_dat_refined %>% 
			group_by(InfectionEvent) %>% 
			slice(1) %>% 
			select(InfectionEvent) %>% 
			rename(id=InfectionEvent)),
		by="id")


# =============================================================================
# Chain plots
# =============================================================================

chainplot_dpadj <- plot_chains(fitlist_chain, "log_dpadj[2]")
chainplot_wpadj <- plot_chains(fitlist_chain, "log_wpadj[2]")

# =============================================================================
# Individual trajectories
# =============================================================================

# Overall
id_sample <- sample(unique(params_indiv$id), min(length(unique(indiv_data$id)), 100))
iter_sample <- sort(sample(1:max(params_indiv$iteration),50))
fig_indivfits <- indiv_data %>% 
	filter(id %in% id_sample) %>% 
	ggplot() + 
		geom_point(aes(x=t,y=y), size=0.5)  +
		geom_segment(data=(filter(params_indiv,id%in%id_sample & iteration%in%iter_sample)),
			aes(x=tp-wp, xend=tp, y=40, yend=40-dp),
			size=0.1, alpha=0.2) + 
		geom_segment(data=(filter(params_indiv,id%in%id_sample & iteration%in%iter_sample)),
			aes(x=tp, xend=tp+wr, y=40-dp, yend=40),
			size=0.1, alpha=0.2) + 
		theme_minimal() + 
		scale_y_reverse() + 
		facet_wrap(~id)

fig_indivfits_mean <- indiv_data %>% 
	filter(id %in% id_sample) %>% 
	ggplot() + 
		geom_point(aes(x=t,y=y), size=0.5)  +
		geom_segment(data=(filter(meanvals_indiv,id%in%id_sample)),
			aes(x=tp-wp, xend=tp, y=40, yend=40-dp),
			size=0.5, alpha=1) + 
		geom_segment(data=(filter(meanvals_indiv,id%in%id_sample)),
			aes(x=tp, xend=tp+wr, y=40-dp, yend=40),
			size=0.5, alpha=1) + 
		theme_minimal() + 
		scale_y_reverse() + 
		facet_wrap(~id)

# By category
idlist <- indiv_data %>% 
	group_by(id) %>% 
	slice(1) %>% 
	select(id, id_clean, category) %>% 
	split(.$category) %>% 
	map(~ ungroup(.)) %>% 
	map(~ slice_sample(., n=min(100,nrow(.)), replace=FALSE)) %>% 
	map(~ arrange(., id))
iter_sample <- sort(sample(1:max(params_indiv$iteration),50))

fig_indivfits_cat <- indiv_data %>% 
	split(.$category) %>% 
	imap(~ filter(.x, id%in%idlist[[.y]]$id)) %>% 
	map(~ select(., id, t, y, category)) %>% 
	imap(~ ggplot(.x) + 
			geom_point(aes(x=t,y=y), size=0.5) + 
			geom_segment(data=(filter(params_indiv,id%in%(idlist[[.y]]$id) & iteration%in%iter_sample)),
				aes(x=tp-wp, xend=tp, y=40, yend=40-dp),
				size=0.1, alpha=0.2) + 
			geom_segment(data=(filter(params_indiv,id%in%(idlist[[.y]]$id) & iteration%in%iter_sample)),
				aes(x=tp, xend=tp+wr, y=40-dp, yend=40),
				size=0.1, alpha=0.2) + 
			theme_minimal() + 
			theme(text=element_text(size=10), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
			scale_y_reverse() + 
			facet_wrap(~id)
		)

# =============================================================================
# Posterior distributions for the adjustments
# =============================================================================

fig_dpadjhist <- get_long_output(fitlist, c("log_dpadj")) %>% 
	filter(id>1) %>% 
	left_join(categorylabsdf, by="id") %>% 
	mutate(value=exp(value)) %>% 
	ggplot() + 
		geom_histogram(aes(x=value, fill=label, y=..density..), alpha=0.4, position="identity", bins=50) + 
		geom_density(aes(x=value, col=label), adjust=2) + 
		geom_vline(aes(xintercept=1)) + 
		scale_color_manual(values=categorycolors[-1]) + 
		scale_fill_manual(values=categorycolors[-1]) + 
		theme_classic() + 
		theme(text=element_text(size=9), legend.title=element_blank()) + 
		labs(title="dp adjustment factor", subtitle=paste0("by ",categorytitle))

fig_wpadjhist <- get_long_output(fitlist, c("log_wpadj")) %>% 
	filter(id>1) %>% 
	left_join(categorylabsdf, by="id") %>% 
	mutate(value=exp(value)) %>% 
	ggplot() + 
		geom_histogram(aes(x=value, fill=label, y=..density..), alpha=0.4, position="identity", bins=50) + 
		geom_density(aes(x=value, col=label), adjust=2) + 
		geom_vline(aes(xintercept=1)) + 
		scale_color_manual(values=categorycolors[-1]) + 
		scale_fill_manual(values=categorycolors[-1]) + 
		theme_classic() + 
		theme(text=element_text(size=9), legend.title=element_blank()) + 
		labs(title="wp adjustment factor", subtitle=paste0("by ",categorytitle))

fig_wradjhist <- get_long_output(fitlist, c("log_wradj")) %>% 
	filter(id>1) %>% 
	left_join(categorylabsdf, by="id") %>% 
	mutate(value=exp(value)) %>% 
	ggplot() + 
		geom_histogram(aes(x=value, fill=label, y=..density..), alpha=0.4, position="identity", bins=50) + 
		geom_density(aes(x=value, col=label), adjust=2) + 
		geom_vline(aes(xintercept=1)) + 
		scale_color_manual(values=categorycolors[-1]) + 
		scale_fill_manual(values=categorycolors[-1]) + 
		theme_classic() + 
		theme(text=element_text(size=9), legend.title=element_blank()) + 
		labs(title="wr adjustment factor", subtitle=paste0("by ",categorytitle))

# =============================================================================
# Posterior distributions for the parameters themselves
# =============================================================================

postdf_dp <- get_long_output(fitlist,c("log_dp_mean")) %>% 
	rename(log_mean=value) %>% 
	select(-name, -id) %>% 
	left_join(
		(get_long_output(fitlist,c("log_dpadj")) %>% 
		rename(log_adj=value) %>% 
		select(-name)),
		by="iteration") %>% 
	select(iteration, id, log_mean, log_adj) %>% 
	mutate(post_dp=exp(log_mean+log_adj)*(run_pars$dp_midpoint)) %>% 
	select(iteration, id, post_dp)

postdf_wp <- get_long_output(fitlist,c("log_wp_mean")) %>% 
	rename(log_mean=value) %>% 
	select(-name, -id) %>% 
	left_join(
		(get_long_output(fitlist,c("log_wpadj")) %>% 
		rename(log_adj=value) %>% 
		select(-name)),
		by="iteration") %>% 
	select(iteration, id, log_mean, log_adj) %>% 
	mutate(post_wp=exp(log_mean+log_adj)*(run_pars$wp_midpoint)) %>% 
	select(iteration, id, post_wp)

postdf_wr <- get_long_output(fitlist,c("log_wr_mean")) %>% 
	rename(log_mean=value) %>% 
	select(-name, -id) %>% 
	left_join(
		(get_long_output(fitlist,c("log_wradj")) %>% 
		rename(log_adj=value) %>% 
		select(-name)),
		by="iteration") %>% 
	select(iteration, id, log_mean, log_adj) %>% 
	mutate(post_wr=exp(log_mean+log_adj)*(run_pars$wr_midpoint)) %>% 
	select(iteration, id, post_wr)


postdf_dp_adjust <- get_long_output(fitlist,c("log_dp_mean")) %>% 
	rename(log_mean=value) %>% 
	select(-name, -id) %>% 
	left_join(
		(get_long_output(fitlist,c("log_dpadj_adjust")) %>% 
		rename(log_adj=value) %>% 
		select(-name)),
		by="iteration") %>% 
	select(iteration, id, log_mean, log_adj) %>% 
	mutate(post_dp=exp(log_mean+log_adj)*(run_pars$dp_midpoint)) %>% 
	select(iteration, id, post_dp)

postdf_wp_adjust <- get_long_output(fitlist,c("log_wp_mean")) %>% 
	rename(log_mean=value) %>% 
	select(-name, -id) %>% 
	left_join(
		(get_long_output(fitlist,c("log_wpadj_adjust")) %>% 
		rename(log_adj=value) %>% 
		select(-name)),
		by="iteration") %>% 
	select(iteration, id, log_mean, log_adj) %>% 
	mutate(post_wp=exp(log_mean+log_adj)*(run_pars$wp_midpoint)) %>% 
	select(iteration, id, post_wp)

postdf_wr_adjust <- get_long_output(fitlist,c("log_wr_mean")) %>% 
	rename(log_mean=value) %>% 
	select(-name, -id) %>% 
	left_join(
		(get_long_output(fitlist,c("log_wradj_adjust")) %>% 
		rename(log_adj=value) %>% 
		select(-name)),
		by="iteration") %>% 
	select(iteration, id, log_mean, log_adj) %>% 
	mutate(post_wr=exp(log_mean+log_adj)*(run_pars$wr_midpoint)) %>% 
	select(iteration, id, post_wr)


postdf_rp <- inner_join(postdf_dp, postdf_wp, by=c("iteration","id")) %>% 
	mutate(post_rp=post_dp/post_wp) %>% 
	select(iteration, id, post_rp)

postdf_rr <- inner_join(postdf_dp, postdf_wr, by=c("iteration","id")) %>% 
	mutate(post_rr=post_dp/post_wr) %>% 
	select(iteration, id, post_rr)

postdf_sm <- inner_join(postdf_wp, postdf_wr, by=c("iteration","id")) %>% 
	mutate(post_sm=post_wp/post_wr) %>% 
	select(iteration, id, post_sm)


fig_dphist <- postdf_dp %>% 
	left_join(categorylabsdf, by="id") %>% 
	ggplot() + 
		geom_histogram(aes(x=global_pars[["lod"]]-post_dp, fill=label, y=..density..), alpha=0.4, position="identity", bins=50) + 
		geom_density(aes(x=global_pars[["lod"]]-post_dp, col=label), adjust=2) +
		scale_x_reverse(sec.axis=sec_axis(~convert_Ct_logGEML(.), name="log10 RNA copies per ml")) + 
		scale_color_manual(values=categorycolors) + 
		scale_fill_manual(values=categorycolors) + 
		theme_classic() + 
		theme(text=element_text(size=9), legend.title=element_blank()) + 
		labs(title=paste0("Peak Ct by ",categorytitle), x="Ct", y="Density")
fig_dphist_nolegend <- fig_dphist + theme(legend.position="none", plot.title=element_blank())

fig_wphist <- postdf_wp %>% 
	left_join(categorylabsdf, by="id") %>% 
	ggplot() + 
		geom_histogram(aes(x=post_wp, fill=label, y=..density..), alpha=0.4, position="identity", bins=50) + 
		geom_density(aes(x=post_wp, col=label), adjust=2) +
		scale_color_manual(values=categorycolors) + 
		scale_fill_manual(values=categorycolors) + 
		theme_classic() + 
		theme(text=element_text(size=9), legend.title=element_blank()) + 
		labs(title=paste0("Proliferation time by ",categorytitle), x="Proliferation time (days)", y="Density")

fig_wrhist <- postdf_wr %>% 
	left_join(categorylabsdf, by="id") %>% 
	ggplot() + 
		geom_histogram(aes(x=post_wr, fill=label, y=..density..), alpha=0.4, position="identity", bins=50) + 
		geom_density(aes(x=post_wr, col=label), adjust=2) +
		scale_color_manual(values=categorycolors) + 
		scale_fill_manual(values=categorycolors) + 
		theme_classic() + 
		theme(text=element_text(size=9), legend.title=element_blank()) + 
		labs(title=paste0("Clearance time by ",categorytitle), x="Clearance time (days)", y="Density")

fig_dphist_adjust <- postdf_dp_adjust %>% 
	left_join(adjustmentlabsdf, by="id") %>% 
	ggplot() + 
		geom_histogram(aes(x=global_pars[["lod"]]-post_dp, fill=label, y=..density..), alpha=0.4, position="identity", bins=50) + 
		geom_density(aes(x=global_pars[["lod"]]-post_dp, col=label), adjust=2) +
		scale_x_reverse(sec.axis=sec_axis(~convert_Ct_logGEML(.), name="log10 RNA copies per ml")) + 
		scale_color_manual(values=adjustmentcolors) + 
		scale_fill_manual(values=adjustmentcolors) + 
		theme_classic() + 
		theme(text=element_text(size=9), legend.title=element_blank()) + 
		labs(title=paste0("Peak Ct by ",adjustmenttitle), x="Ct", y="Density")

fig_wphist_adjust <- postdf_wp_adjust %>% 
	left_join(adjustmentlabsdf, by="id") %>% 
	ggplot() + 
		geom_histogram(aes(x=post_wp, fill=label, y=..density..), alpha=0.4, position="identity", bins=50) + 
		geom_density(aes(x=post_wp, col=label), adjust=2) +
		scale_color_manual(values=adjustmentcolors) + 
		scale_fill_manual(values=adjustmentcolors) + 
		theme_classic() + 
		theme(text=element_text(size=9), legend.title=element_blank()) + 
		labs(title=paste0("Proliferation time by ",adjustmenttitle), x="Proliferation time (days)", y="Density")

fig_wrhist_adjust <- postdf_wr_adjust %>% 
	left_join(adjustmentlabsdf, by="id") %>% 
	ggplot() + 
		geom_histogram(aes(x=post_wr, fill=label, y=..density..), alpha=0.4, position="identity", bins=50) + 
		geom_density(aes(x=post_wr, col=label), adjust=2) +
		scale_color_manual(values=adjustmentcolors) + 
		scale_fill_manual(values=adjustmentcolors) + 
		theme_classic() + 
		theme(text=element_text(size=9), legend.title=element_blank()) + 
		labs(title=paste0("Clearance time by ",adjustmenttitle), x="Clearance time (days)", y="Density")

fig_rphist <- postdf_rp %>% 
	left_join(categorylabsdf, by="id") %>% 
	ggplot() + 
		geom_histogram(aes(x=post_rp, fill=label, y=..density..), alpha=0.4, position="identity", bins=50) + 
		geom_density(aes(x=post_rp, col=label), adjust=2) +
		scale_color_manual(values=categorycolors) + 
		scale_fill_manual(values=categorycolors) + 
		theme_classic() + 
		theme(text=element_text(size=9), legend.title=element_blank()) + 
		labs(title=paste0("Proliferation rate by ",categorytitle), x="Proliferation rate (Ct/day)", y="Density")

fig_rrhist <- postdf_rr %>% 
	left_join(categorylabsdf, by="id") %>% 
	ggplot() + 
		geom_histogram(aes(x=post_rr, fill=label, y=..density..), alpha=0.4, position="identity", bins=50) + 
		geom_density(aes(x=post_rr, col=label), adjust=2) +
		scale_color_manual(values=categorycolors) + 
		scale_fill_manual(values=categorycolors) + 
		theme_classic() + 
		theme(text=element_text(size=9), legend.title=element_blank()) + 
		labs(title=paste0("Clearance rate by ",categorytitle), x="Clearance rate (Ct/day)", y="Density")

# =============================================================================
# Trajectory summaries 
# =============================================================================

tpdf <- get_long_output(fitlist, c("tp")) %>% 
	group_by(id) %>% 
	summarise(tp=mean(value)) %>%
	rename(id_clean=id)

postdf_overall <- postdf_dp %>% 
	left_join(postdf_wp, by=c("iteration","id")) %>% 
	left_join(postdf_wr, by=c("iteration","id")) 
postdf_overall_summary <- postdf_overall %>% 
	group_by(id) %>% 
	summarise(post_dp_mean=mean(post_dp), 
		post_dp_lwr=quantile(post_dp,0.025), 
		post_dp_upr=quantile(post_dp,0.975), 
		post_wp_mean=mean(post_wp), 
		post_wp_lwr=quantile(post_wp,0.025), 
		post_wp_upr=quantile(post_wp,0.975),
		post_wr_mean=mean(post_wr),
		post_wr_lwr=quantile(post_wr,0.025), 
		post_wr_upr=quantile(post_wr,0.975))

boundinterp <- function(val,xstart,xend,ystart,yend){
	out <- (yend-ystart)/(xend-xstart)*(val-xstart)+ystart
	return(out)
}

boundvalsup <- seq(from=-10,to=0,by=0.02)
boundvalsdown <- seq(from=0,to=20,by=0.02)

boundsup <- lapply(min(postdf_overall$id):max(postdf_overall$id),
	function(y){
		xstart <- -filter(postdf_overall,id==y)$post_wp
		xend <- rep(0,length(xstart))
		ystart <- rep(0, length(xstart))
		yend <- filter(postdf_overall,id==y)$post_dp
		bounds <- (lapply(boundvalsup, boundinterp, xstart, xend, ystart, yend) %>%
			map(~ tibble(lwr=quantile(.,0.025), mean=mean(.), upr=quantile(.,0.975))) %>%
			bind_rows() %>% 
			mutate(t=boundvalsup))
		return(bounds)
	}
	)

boundsdown <- lapply(min(postdf_overall$id):max(postdf_overall$id),
	function(y){
		xend <- filter(postdf_overall,id==y)$post_wr
		xstart <- rep(0,length(xend))
		ystart <- filter(postdf_overall,id==y)$post_dp
		yend <- rep(0, length(xend))
		bounds <- (lapply(boundvalsdown, boundinterp, xstart, xend, ystart, yend) %>%
			map(~ tibble(lwr=quantile(.,0.025), mean=mean(.), upr=quantile(.,0.975))) %>%
			bind_rows() %>% 
			mutate(t=boundvalsdown))
		return(bounds)
	}
	)

fig_viztrajectories <- indiv_data %>% 
	select(id_clean, t, y, category) %>% 
	filter(y<40) %>% 
	left_join(tpdf, by="id_clean") %>%  
	mutate(t=t-tp) %>% 
	split(.$category) %>% 
	imap(~ ggplot(.x, aes(x=t, y=y)) + 
		geom_point(alpha=0.1, size=0.5, col=categorycolors[as.numeric(.y)]) + 
		coord_cartesian(ylim=c(40,10), xlim=c(-12,25), expand=FALSE) + 
		scale_y_reverse(sec.axis=sec_axis(~convert_Ct_logGEML(.), name=expression(log[10]~RNA~copies/ml))) + 
		labs(x="Days since peak viral load (estimated)", y="Ct", tag=categorytags[as.numeric(.y)]) + 
		theme_classic() + 
		theme(text=element_text(size=9))) 

# for(indexA in 1:max(postdf_overall$id)){
for(indexA in 1:length(fig_viztrajectories)){
	fig_viztrajectories[[indexA]] <- fig_viztrajectories[[indexA]] + 
		geom_ribbon(data=boundsup[[indexA]], aes(x=t, y=mean, ymin=global_pars[["lod"]]-lwr, ymax=global_pars[["lod"]]-upr), alpha=0.5, fill=categorycolors[indexA]) + 
		geom_line(data=boundsup[[indexA]], aes(x=t, y=global_pars[["lod"]]-lwr),size=0.1, col=categorycolors[indexA]) +
		geom_line(data=boundsup[[indexA]], aes(x=t, y=global_pars[["lod"]]-upr),size=0.1, col=categorycolors[indexA]) +
		geom_line(data=boundsup[[indexA]], aes(x=t, y=global_pars[["lod"]]-mean), col=categorycolors[indexA]) + 
		geom_ribbon(data=boundsdown[[indexA]], aes(x=t, y=mean, ymin=global_pars[["lod"]]-lwr, ymax=global_pars[["lod"]]-upr), alpha=0.5, fill=categorycolors[indexA]) + 
		geom_line(data=boundsdown[[indexA]], aes(x=t, y=global_pars[["lod"]]-lwr),size=0.1, col=categorycolors[indexA]) +
		geom_line(data=boundsdown[[indexA]], aes(x=t, y=global_pars[["lod"]]-upr),size=0.1, col=categorycolors[indexA]) +
		geom_line(data=boundsdown[[indexA]], aes(x=t, y=global_pars[["lod"]]-mean), col=categorycolors[indexA])
}

fig_viztrajectories_grey <- indiv_data %>% 
	select(id_clean, t, y, category) %>% 
	filter(y<40) %>% 
	left_join(tpdf, by="id_clean") %>%  
	mutate(t=t-tp) %>% 
	split(.$category) %>% 
	imap(~ ggplot(.x, aes(x=t, y=y)) + 
		geom_point(alpha=0.8, size=0.5, col="grey") + 
		coord_cartesian(ylim=c(40,10), xlim=c(-12,25), expand=FALSE) + 
		scale_y_reverse(sec.axis=sec_axis(~convert_Ct_logGEML(.), name=expression(log[10]~RNA~copies/ml))) + 
		labs(x="Days since peak viral load (estimated)", y="Ct", tag=categorytags[as.numeric(.y)]) + 
		theme_classic() + 
		theme(text=element_text(size=9))) 

# for(indexA in 1:max(postdf_overall$id)){
for(indexA in 1:length(fig_viztrajectories_grey)){
	fig_viztrajectories_grey[[indexA]] <- fig_viztrajectories_grey[[indexA]] + 
		geom_ribbon(data=boundsup[[indexA]], aes(x=t, y=mean, ymin=global_pars[["lod"]]-lwr, ymax=global_pars[["lod"]]-upr), alpha=0.5, fill="black") + 
		geom_line(data=boundsup[[indexA]], aes(x=t, y=global_pars[["lod"]]-lwr),size=0.1, col="black") +
		geom_line(data=boundsup[[indexA]], aes(x=t, y=global_pars[["lod"]]-upr),size=0.1, col="black") +
		geom_line(data=boundsup[[indexA]], aes(x=t, y=global_pars[["lod"]]-mean), col="black") + 
		geom_ribbon(data=boundsdown[[indexA]], aes(x=t, y=mean, ymin=global_pars[["lod"]]-lwr, ymax=global_pars[["lod"]]-upr), alpha=0.5, fill="black") + 
		geom_line(data=boundsdown[[indexA]], aes(x=t, y=global_pars[["lod"]]-lwr),size=0.1, col="black") +
		geom_line(data=boundsdown[[indexA]], aes(x=t, y=global_pars[["lod"]]-upr),size=0.1, col="black") +
		geom_line(data=boundsdown[[indexA]], aes(x=t, y=global_pars[["lod"]]-mean), col="black")
}

varmap <- ct_dat_refined %>% 
	group_by(InfectionEvent) %>% 
	slice(1) %>% 
	ungroup() %>% 
	select(InfectionEvent, LineageBroad) %>% 
	rename(id=InfectionEvent) 

fig_viztrajectories_varcol <- indiv_data %>% 
	left_join(varmap) %>% 
	select(id_clean, t, y, category, LineageBroad) %>% 
	filter(y<40) %>% 
	left_join(tpdf, by="id_clean") %>%  
	mutate(t=t-tp) %>% 
	split(.$category) %>% 
	imap(~ ggplot(.x, aes(x=t, y=y)) + 
		geom_point(aes(col=LineageBroad), alpha=0.5, size=0.5) +
		scale_color_manual(values=c("Other"="Black","None"="Black","BA.1"="Magenta","BA.2"="Magenta","BA.3"="Orange","BA.4"="Orange","BA.5"="Orange","Delta"="Red","Alpha"="Blue"), guide="none") +  
		coord_cartesian(ylim=c(40,10), xlim=c(-12,25), expand=FALSE) + 
		scale_y_reverse(sec.axis=sec_axis(~convert_Ct_logGEML(.), name=expression(log[10]~RNA~copies/ml))) + 
		labs(x="Days since peak viral load (estimated)", y="Ct", tag=categorytags[as.numeric(.y)]) + 
		theme_classic() + 
		theme(text=element_text(size=9))) 

# for(indexA in 1:max(postdf_overall$id)){
for(indexA in 1:length(fig_viztrajectories_varcol)){
	fig_viztrajectories_varcol[[indexA]] <- fig_viztrajectories_varcol[[indexA]] + 
		geom_ribbon(data=boundsup[[indexA]], aes(x=t, y=mean, ymin=global_pars[["lod"]]-lwr, ymax=global_pars[["lod"]]-upr), alpha=0.5, fill="black") + 
		geom_line(data=boundsup[[indexA]], aes(x=t, y=global_pars[["lod"]]-lwr),size=0.1, col="black") +
		geom_line(data=boundsup[[indexA]], aes(x=t, y=global_pars[["lod"]]-upr),size=0.1, col="black") +
		geom_line(data=boundsup[[indexA]], aes(x=t, y=global_pars[["lod"]]-mean), col="black") + 
		geom_ribbon(data=boundsdown[[indexA]], aes(x=t, y=mean, ymin=global_pars[["lod"]]-lwr, ymax=global_pars[["lod"]]-upr), alpha=0.5, fill="black") + 
		geom_line(data=boundsdown[[indexA]], aes(x=t, y=global_pars[["lod"]]-lwr),size=0.1, col="black") +
		geom_line(data=boundsdown[[indexA]], aes(x=t, y=global_pars[["lod"]]-upr),size=0.1, col="black") +
		geom_line(data=boundsdown[[indexA]], aes(x=t, y=global_pars[["lod"]]-mean), col="black")
}

# =============================================================================
# Whiskers: 
# =============================================================================

pointsize <- 0.8
alphabee <- 0.2
whiskersize <- 0.3
whiskerwidth <- 0.1
fig_dp_whiskers <- meanvals_indiv %>% 
	select(dp, id=category) %>% 
	ggplot(aes(x=factor(id), col=factor(id), y=global_pars[["lod"]]-dp)) + 
		geom_beeswarm(size=pointsize, alpha=alphabee, cex=1.2) + 
		geom_segment(data=postdf_overall_summary, aes(x=id, xend=id, y=global_pars[["lod"]]-post_dp_lwr, yend=global_pars[["lod"]]-post_dp_upr), col="black", size=whiskersize) +  
		geom_segment(data=postdf_overall_summary, aes(x=id-whiskerwidth, xend=id+whiskerwidth, y=global_pars[["lod"]]-post_dp_lwr, yend=global_pars[["lod"]]-post_dp_lwr), col="black", size=whiskersize) +  
		geom_segment(data=postdf_overall_summary, aes(x=id-whiskerwidth, xend=id+whiskerwidth, y=global_pars[["lod"]]-post_dp_upr, yend=global_pars[["lod"]]-post_dp_upr), col="black", size=whiskersize) +  
		geom_segment(data=postdf_overall_summary, aes(x=id-1.2*whiskerwidth, xend=id+1.2*whiskerwidth, y=global_pars[["lod"]]-post_dp_mean, yend=global_pars[["lod"]]-post_dp_mean), col="black", size=whiskersize) +
		scale_color_manual(values=categorycolors,guide="none") + 
		scale_x_discrete(labels=categorylabs) + 
		scale_y_reverse(sec.axis=sec_axis(~convert_Ct_logGEML(.), name="log10 RNA copies per ml")) + 
		# scale_y_reverse() + 
		theme_classic() + 
		theme(
			text=element_text(size=9),
			axis.text.x=element_text(angle=30,hjust=1)) + 
		labs(x="", y="Peak Ct")


fig_wp_whiskers <- meanvals_indiv %>% 
	select(wp, id=category) %>% 
	ggplot(aes(x=factor(id), col=factor(id), y=wp)) + 
		geom_beeswarm(size=pointsize, alpha=alphabee, cex=0.8) + 
		geom_segment(data=postdf_overall_summary, aes(x=id, xend=id, y=post_wp_lwr, yend=post_wp_upr), col="black", size=whiskersize) +  
		geom_segment(data=postdf_overall_summary, aes(x=id-whiskerwidth, xend=id+whiskerwidth, y=post_wp_lwr, yend=post_wp_lwr), col="black", size=whiskersize) +  
		geom_segment(data=postdf_overall_summary, aes(x=id-whiskerwidth, xend=id+whiskerwidth, y=post_wp_upr, yend=post_wp_upr), col="black", size=whiskersize) +  
		geom_segment(data=postdf_overall_summary, aes(x=id-1.2*whiskerwidth, xend=id+1.2*whiskerwidth, y=post_wp_mean, yend=post_wp_mean), col="black", size=whiskersize) +  
		scale_color_manual(values=categorycolors,guide="none") + 
		scale_x_discrete(labels=categorylabs) + 
		# scale_y_reverse() + 
		theme_classic() + 
		theme(
			text=element_text(size=9),
			axis.text.x=element_text(angle=30,hjust=1)) + 
		labs(x="", y="Proliferation time (days)")


fig_wr_whiskers <- meanvals_indiv %>% 
	select(wr, id=category) %>% 
	ggplot(aes(x=factor(id), col=factor(id), y=wr)) + 
		geom_beeswarm(size=pointsize, alpha=alphabee, cex=1.0) + 
		geom_segment(data=postdf_overall_summary, aes(x=id, xend=id, y=post_wr_lwr, yend=post_wr_upr), col="black", size=whiskersize) +  
		geom_segment(data=postdf_overall_summary, aes(x=id-whiskerwidth, xend=id+whiskerwidth, y=post_wr_lwr, yend=post_wr_lwr), col="black", size=whiskersize) +  
		geom_segment(data=postdf_overall_summary, aes(x=id-whiskerwidth, xend=id+whiskerwidth, y=post_wr_upr, yend=post_wr_upr), col="black", size=whiskersize) +  
		geom_segment(data=postdf_overall_summary, aes(x=id-1.2*whiskerwidth, xend=id+1.2*whiskerwidth, y=post_wr_mean, yend=post_wr_mean), col="black", size=whiskersize) +  
		scale_color_manual(values=categorycolors,guide="none") + 
		scale_x_discrete(labels=categorylabs) + 
		# scale_y_reverse() + 
		theme_classic() + 
		theme(
			text=element_text(size=9),
			axis.text.x=element_text(angle=30,hjust=1)) + 
		labs(x="", y="Clearance time (days)")


# =============================================================================
# Summarise fits
# =============================================================================

summdf_geml <- postdf_dp %>% 
	group_by(id) %>% 
	summarise(mean=round(convert_Ct_logGEML(global_pars[["lod"]]-mean(post_dp)),1), lwr=round(convert_Ct_logGEML(global_pars[["lod"]]-quantile(post_dp,0.025)),1), upr=round(convert_Ct_logGEML(global_pars[["lod"]]-quantile(post_dp,0.975)),1)) %>% 
	mutate(var="Peak GEML") %>% 
	left_join(tibble(name=categorylabs, id=1:length(categorylabs)), by="id") %>% 
	select(var,name,mean,lwr,upr) %>% 
	mutate(string=paste0(mean," (",lwr,", ",upr,")"))

summdf_ct <- postdf_dp %>% 
	group_by(id) %>% 
	summarise(mean=round(global_pars[["lod"]]-mean(post_dp),1), lwr=round(global_pars[["lod"]]-quantile(post_dp,0.975),1), upr=round(global_pars[["lod"]]-quantile(post_dp,0.025),1)) %>% 
	mutate(var="Peak Ct") %>% 
	left_join(tibble(name=categorylabs, id=1:length(categorylabs)), by="id") %>% 
	select(var,name,mean,lwr,upr) %>% 
	mutate(string=paste0(mean," (",lwr,", ",upr,")"))

summdf_wp <- postdf_wp %>% 
	group_by(id) %>% 
	summarise(mean=round(mean(post_wp),1), lwr=round(quantile(post_wp,0.025),1), upr=round(quantile(post_wp,0.975),1)) %>% 
	mutate(var="Proliferation time") %>% 
	left_join(tibble(name=categorylabs, id=1:length(categorylabs)), by="id") %>% 
	select(var,name,mean,lwr,upr) %>% 
	mutate(string=paste0(mean," (",lwr,", ",upr,")"))

summdf_wr <- postdf_wr %>% 
	group_by(id) %>% 
	summarise(mean=round(mean(post_wr),1), lwr=round(quantile(post_wr,0.025),1), upr=round(quantile(post_wr,0.975),1)) %>% 
	mutate(var="Clearance time") %>% 
	left_join(tibble(name=categorylabs, id=1:length(categorylabs)), by="id") %>% 
	select(var,name,mean,lwr,upr) %>% 
	mutate(string=paste0(mean," (",lwr,", ",upr,")"))

summdf_rp <- postdf_rp %>% 
	group_by(id) %>% 
	summarise(mean=round(mean(post_rp),1), lwr=round(quantile(post_rp,0.025),1), upr=round(quantile(post_rp,0.975),1)) %>% 
	mutate(var="Proliferation rate (Ct/day)") %>% 
	left_join(tibble(name=categorylabs, id=1:length(categorylabs)), by="id") %>% 
	select(var,name,mean,lwr,upr) %>% 
	mutate(string=paste0(mean," (",lwr,", ",upr,")"))

summdf_rr <- postdf_rr %>% 
	group_by(id) %>% 
	summarise(mean=round(mean(post_rr),1), lwr=round(quantile(post_rr,0.025),1), upr=round(quantile(post_rr,0.975),1)) %>% 
	mutate(var="Clearance rate (Ct/day)") %>% 
	left_join(tibble(name=categorylabs, id=1:length(categorylabs)), by="id") %>% 
	select(var,name,mean,lwr,upr) %>% 
	mutate(string=paste0(mean," (",lwr,", ",upr,")"))

summdf_sm <- postdf_sm %>% 
	group_by(id) %>% 
	summarise(mean=round(mean(post_sm),1), lwr=round(quantile(post_sm,0.025),1), upr=round(quantile(post_sm,0.975),1)) %>% 
	mutate(var="Symmetry (proliferation/clearance)") %>% 
	left_join(tibble(name=categorylabs, id=1:length(categorylabs)), by="id") %>% 
	select(var,name,mean,lwr,upr) %>% 
	mutate(string=paste0(mean," (",lwr,", ",upr,")"))

summdf_overall <- bind_rows(
	summdf_ct,
	summdf_geml,
	summdf_wp,
	summdf_wr,
	summdf_rp,
	summdf_rr,
	summdf_sm)

