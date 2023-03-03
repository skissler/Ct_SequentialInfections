savedir <- paste0("figures/")
stdwidth <- 3.2
stdheight <- 3.2*10/16
stddpi <- 600

# =============================================================================
# Set up data frames for the analysis 
# =============================================================================

# Extract the mean trajectory values for those with well-documented repeat infections: 
meanvals_indiv_repinf <- meanvals_indiv %>% 
	arrange(id) %>% 
	group_by(PersonID) %>% 
	mutate(ninf=n()) %>% 
	filter(ninf==2) %>% 
	ungroup() %>% 
	mutate(infnum=case_when(category<4~1,TRUE~2)) %>% 
	select(PersonID, id, dp, wp, wr, category, infnum)

# Generate a wide table with the normalized order statistics for each paired infection within its category (Other/primary, Alpha/primary, Delta/primary, Omicron/secondary): 
repinf_order_df <- meanvals_indiv_repinf %>% 
	split(.$category) %>% 
	map(~ arrange(., dp)) %>% 
	map(~ mutate(., dporder=1:n()/n())) %>% 
	map(~ arrange(., wp)) %>% 
	map(~ mutate(., wporder=1:n()/n())) %>% 
	map(~ arrange(., wr)) %>% 
	map(~ mutate(., wrorder=1:n()/n())) %>% 
	map(~ select(., PersonID, dporder, wporder, wrorder)) %>% 
	bind_rows(.id="category") %>% 
	mutate(infnum=case_when(category<4~1,TRUE~2)) %>% 
	split(.$infnum) %>% 
	map(~ select(., -infnum)) %>% 
	(function(x){
				left_join(x[[1]], x[[2]], by="PersonID", suffix=c(".1",".2"))
			})

# For a sensitivity analysis: generate a second wide table with the BA.1/Ba.2 order statistics stratified by the lineage of the first infection: 
repinf_order_strat_df <- meanvals_indiv_repinf %>% 
	arrange(id) %>% 
	group_by(PersonID) %>% 
	mutate(prevcategory=lag(category)) %>% 
	mutate(category_new = case_when(
		category==1~1,
		category==2~2,
		category==3~3,
		category==4 & prevcategory==1~4,
		category==4 & prevcategory==2~5,
		category==4 & prevcategory==3~6
		)) %>% 
	select(-prevcategory) %>% 
	ungroup() %>% 
	split(.$category_new) %>% 
	map(~ arrange(., dp)) %>% 
	map(~ mutate(., dporder=1:n()/n())) %>% 
	map(~ arrange(., wp)) %>% 
	map(~ mutate(., wporder=1:n()/n())) %>% 
	map(~ arrange(., wr)) %>% 
	map(~ mutate(., wrorder=1:n()/n())) %>% 
	map(~ select(., PersonID, dporder, wporder, wrorder)) %>% 
	bind_rows(.id="category_new") %>% 
	mutate(infnum=case_when(category_new<4~1,TRUE~2)) %>% 
	split(.$infnum) %>% 
	map(~ select(., -infnum)) %>% 
	(function(x){
				left_join(x[[1]], x[[2]], by="PersonID", suffix=c(".1",".2"))
			})

# =============================================================================
# Plot the normalized order statistics 
# =============================================================================

# Plot the second vs first infection normalized order statistics: 
fig_repinf_order_dp <- repinf_order_df %>% 
	ggplot(aes(x=dporder.1, y=dporder.2)) + 
		geom_point() + 
		geom_smooth(method="lm", col="black", size=0.75) + 
		theme_classic() +
		theme(text=element_text(size=9)) + 
		labs(
			x="Relative peak viral load,\nfirst infection", 
			y="Relative peak viral load,\nsecond infection")

fig_repinf_order_wp <- repinf_order_df %>% 
	ggplot(aes(x=wporder.1, y=wporder.2)) + 
		geom_point() + 
		geom_smooth(method="lm", col="black", size=0.75) + 
		theme_classic() +
		theme(text=element_text(size=9)) + 
		labs(
			x="Relative proliferation time,\nfirst infection", 
			y="Relative proliferation time,\nsecond infection")

fig_repinf_order_wr <- repinf_order_df %>% 
	ggplot(aes(x=wrorder.1, y=wrorder.2)) + 
		geom_point() + 
		geom_smooth(method="lm", col="black", size=0.75) + 
		theme_classic() +
		theme(text=element_text(size=9)) + 
		labs(
			x="Relative clearance time,\nfirst infection", 
			y="Relative clearance time,\nsecond infection")


ggsave(fig_repinf_order_dp, file=paste0(savedir,"repinf_order_dp.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_repinf_order_wp, file=paste0(savedir,"repinf_order_wp.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_repinf_order_wr, file=paste0(savedir,"repinf_order_wr.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)

ggsave(fig_repinf_order_dp, file=paste0(savedir,"repinf_order_dp.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_repinf_order_wp, file=paste0(savedir,"repinf_order_wp.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_repinf_order_wr, file=paste0(savedir,"repinf_order_wr.png"), width=stdwidth, height=stdheight, dpi=stddpi)


# Try a different approach, including the rank more explicitly: 

repinf_order_df %>% 
	mutate(lwidth=1-abs(dporder.1-dporder.2)) %>% 
	mutate(lwidth=(exp(lwidth)-1)/(exp(1)-1)) %>% 
	ggplot() + 
		geom_point(aes(x=dporder.1, y=1),size=0.75) + 
		geom_point(aes(x=dporder.2, y=0),size=0.75) + 
		geom_segment(aes(x=dporder.1, xend=dporder.2, y=1, yend=0, size=lwidth, alpha=lwidth)) + 
		scale_size_continuous(range=c(0.1,0.5)) + 
		scale_alpha_continuous(range=c(0.2,0.8)) + 
		theme_classic()

# Same thing, but with the stratification: 

fig_repinf_order_strat_dp <- repinf_order_strat_df %>% 
	ggplot(aes(x=dporder.1, y=dporder.2, col=factor(category_new.1))) + 
		geom_point() + 
		geom_smooth(method="lm") + 
		scale_color_manual(values=c("black","blue","red"), labels=c("Other/None","Alpha","Delta")) + 
		theme_classic() + 
		theme(text=element_text(size=9), legend.position="bottom", strip.text.x=element_blank(),axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + 
		facet_wrap(~factor(category_new.1)) + 
		labs(
			x="Relative peak viral load,\nfirst infection", 
			y="Relative peak viral load,\nsecond infection",
			col="First infection lineage")

fig_repinf_order_strat_wp <- repinf_order_strat_df %>% 
	ggplot(aes(x=wporder.1, y=wporder.2, col=factor(category_new.1))) + 
		geom_point() + 
		geom_smooth(method="lm") + 
		scale_color_manual(values=c("black","blue","red"), labels=c("Other/None","Alpha","Delta")) + 
		theme_classic() + 
		theme(text=element_text(size=9), legend.position="bottom", strip.text.x=element_blank(),axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + 
		facet_wrap(~factor(category_new.1)) + 
		labs(
			x="Relative proliferation time,\nfirst infection", 
			y="Relative proliferation time,\nsecond infection",
			col="First infection lineage")

fig_repinf_order_strat_wr <- repinf_order_strat_df %>% 
	ggplot(aes(x=wrorder.1, y=wrorder.2, col=factor(category_new.1))) + 
		geom_point() + 
		geom_smooth(method="lm") + 
		scale_color_manual(values=c("black","blue","red"), labels=c("Other/None","Alpha","Delta")) + 
		theme_classic() + 
		theme(text=element_text(size=9), legend.position="bottom", strip.text.x=element_blank(),axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + 
		facet_wrap(~factor(category_new.1)) + 
		labs(
			x="Relative clearance time,\nfirst infection", 
			y="Relative clearance time,\nsecond infection",
			col="First infection lineage")

ggsave(fig_repinf_order_strat_dp, file=paste0(savedir,"repinf_order_strat_dp.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_repinf_order_strat_wp, file=paste0(savedir,"repinf_order_strat_wp.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_repinf_order_strat_wr, file=paste0(savedir,"repinf_order_strat_wr.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)

ggsave(fig_repinf_order_strat_dp, file=paste0(savedir,"repinf_order_strat_dp.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_repinf_order_strat_wp, file=paste0(savedir,"repinf_order_strat_wp.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_repinf_order_strat_wr, file=paste0(savedir,"repinf_order_strat_wr.png"), width=stdwidth, height=stdheight, dpi=stddpi)

# =============================================================================
# Test the correlations
# =============================================================================

# Overall: 
cordf <- repinf_order_df %>% 
	ungroup() %>% 
	summarise(
		dpcor=cor(dporder.1,dporder.2,method="spearman"),
		wpcor=cor(wporder.1,wporder.2,method="spearman"),
		wrcor=cor(wrorder.1,wrorder.2,method="spearman")
		)

corpermdf <- list(repinf_order_df) %>% 
	map(~ list(
		dpcorperm=unlist(lapply(1:10000, 
			function(x){
				return(cor(sample(.$dporder.1),sample(.$dporder.2),method="spearman"))})),
		wpcorperm=unlist(lapply(1:10000, 
			function(x){
				return(cor(sample(.$wporder.1),sample(.$wporder.2),method="spearman"))})),
		wrcorperm=unlist(lapply(1:10000, 
			function(x){
				return(cor(sample(.$wrorder.1),sample(.$wrorder.2),method="spearman"))}))
		)) %>% 
	map(~ bind_rows(.))  %>% 
	bind_rows()

corsigdf <- mutate(corpermdf,category=1) %>% 
	left_join(mutate(cordf,category=1), by="category") %>% 
	mutate(dpgreater=case_when(dpcorperm>dpcor~1,TRUE~0)) %>% 
	mutate(wpgreater=case_when(wpcorperm>wpcor~1,TRUE~0)) %>% 
	mutate(wrgreater=case_when(wrcorperm>wrcor~1,TRUE~0)) %>% 
	group_by(category) %>% 
	summarise(
		pdp=sum(dpgreater)/n(),
		pwp=sum(wpgreater)/n(),
		pwr=sum(wrgreater)/n())

dpcorhist <- corpermdf %>% 
	ggplot(aes(x=dpcorperm)) + 
		geom_histogram(binwidth=0.02, alpha=0.6, fill="white",col="gray") + 
		geom_vline(data=cordf,aes(xintercept=dpcor), lty="dashed") + 
		theme_classic() + 
		labs(x="Permuted correlations, peak viral load",
			y="Count") + 
		theme(text=element_text(size=9))

wpcorhist <- corpermdf %>% 
	ggplot(aes(x=wrcorperm)) + 
		geom_histogram(binwidth=0.02, alpha=0.6, fill="white",col="gray") + 
		geom_vline(data=cordf,aes(xintercept=wpcor), lty="dashed") + 
		theme_classic() + 
		labs(x="Permuted correlations, proliferation time",
			y="Count") + 
		theme(text=element_text(size=9))

wrcorhist <- corpermdf %>% 
	ggplot(aes(x=wpcorperm)) + 
		geom_histogram(binwidth=0.02, alpha=0.6, fill="white",col="gray") + 
		geom_vline(data=cordf,aes(xintercept=wrcor), lty="dashed") + 
		theme_classic() + 
		labs(x="Permuted correlations, clearance time",
			y="Count") + 
		theme(text=element_text(size=9))

ggsave(dpcorhist, file=paste0(savedir,"dpcorhist.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(wpcorhist, file=paste0(savedir,"wpcorhist.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(wrcorhist, file=paste0(savedir,"wrcorhist.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)

ggsave(dpcorhist, file=paste0(savedir,"dpcorhist.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(wpcorhist, file=paste0(savedir,"wpcorhist.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(wrcorhist, file=paste0(savedir,"wrcorhist.png"), width=stdwidth, height=stdheight, dpi=stddpi)

# Together on one plot, since the background should be the same for dp, wp, and wr: 

allcorpermdf <- list(repinf_order_df) %>% 
	map(~ list(
		corperm=unlist(lapply(1:100000, 
			function(x){
				return(cor(sample(.$dporder.1),sample(.$dporder.2),method="spearman"))}))
		)) %>% 
	map(~ bind_rows(.))  %>% 
	bind_rows()

allcorhist <- ggplot() + 
	geom_density(data=allcorpermdf, aes(x=corperm), alpha=1, fill="white",col="darkgray",adjust=2, size=1) + 
	scale_x_continuous(breaks=seq(from=-1, to=1, by=0.25)) + 
	geom_vline(data=cordf,aes(xintercept=dpcor),lty="dashed",size=0.3,alpha=0.7) + 
	geom_vline(data=cordf,aes(xintercept=wpcor),lty="dashed",size=0.5,alpha=0.7) + 
	geom_vline(data=cordf,aes(xintercept=wrcor),lty="dashed",size=0.8,alpha=0.7) + 
	theme_classic() + 
	theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
	labs(x="Spearman correlation", y="Density")

ggsave(allcorhist, file=paste0(savedir,"allcorhist.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(allcorhist, file=paste0(savedir,"allcorhist.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)

# With BA.1/BA.2 infections stratified by the previous lineage: 

cordf_strat <- repinf_order_strat_df %>% 
	split(.$category_new.1) %>% 
	map(~ list(
		dpcor=cor(.$dporder.1,.$dporder.2,method="spearman"),
		wpcor=cor(.$wporder.1,.$wporder.2,method="spearman"),
		wrcor=cor(.$wrorder.1,.$wrorder.2,method="spearman"))) %>% 
	map(~ bind_rows(.)) %>% 
	bind_rows(.id="category_new")

corpermdf_strat <- repinf_order_strat_df %>% 
	split(.$category_new.1) %>% 
	map(~ list(
		dpcorperm=unlist(lapply(1:10000, 
			function(x){
				return(cor(sample(.$dporder.1),sample(.$dporder.2),method="spearman"))})),
		wpcorperm=unlist(lapply(1:10000, 
			function(x){
				return(cor(sample(.$wporder.1),sample(.$wporder.2),method="spearman"))})),
		wrcorperm=unlist(lapply(1:10000, 
			function(x){
				return(cor(sample(.$wrorder.1),sample(.$wrorder.2),method="spearman"))}))
		)) %>% 
	map(~ bind_rows(.)) %>% 
	bind_rows(.id="category_new")

corsigdf_strat <- corpermdf_strat %>% 
	left_join(cordf_strat, by="category_new") %>% 
	mutate(dpgreater=case_when(dpcorperm>dpcor~1,TRUE~0)) %>% 
	mutate(wpgreater=case_when(wpcorperm>wpcor~1,TRUE~0)) %>% 
	mutate(wrgreater=case_when(wrcorperm>wrcor~1,TRUE~0)) %>% 
	group_by(category_new) %>% 
	summarise(
		pdp=sum(dpgreater)/n(),
		pwp=sum(wpgreater)/n(),
		pwr=sum(wrgreater)/n())

dpcorhist_strat <- corpermdf_strat %>% 
	ggplot(aes(x=dpcorperm, fill=factor(category_new))) + 
		geom_histogram(binwidth=0.05, alpha=0.6) + 
		scale_fill_manual(values=c("black","blue","red"), labels=c("Other/None","Alpha","Delta")) + 
		facet_wrap(~factor(category_new)) + 
		geom_vline(data=cordf_strat,aes(xintercept=dpcor)) + 
		theme_classic() + 
		labs(x="Permuted correlations, peak viral load",
			y="Count",
			fill="First infection\nlineage") + 
		theme(text=element_text(size=9), strip.text.x=element_blank(),axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

wpcorhist_strat <- corpermdf_strat %>% 
	ggplot(aes(x=wpcorperm, fill=factor(category_new))) + 
		geom_histogram(binwidth=0.05, alpha=0.6) + 
		scale_fill_manual(values=c("black","blue","red"), labels=c("Other/None","Alpha","Delta")) + 
		facet_wrap(~factor(category_new)) + 
		geom_vline(data=cordf_strat,aes(xintercept=wpcor)) + 
		theme_classic() + 
		labs(x="Permuted correlations, peak viral load",
			y="Count",
			fill="First infection\nlineage") + 
		theme(text=element_text(size=9), strip.text.x=element_blank(),axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

wrcorhist_strat <- corpermdf_strat %>% 
	ggplot(aes(x=wrcorperm, fill=factor(category_new))) + 
		geom_histogram(binwidth=0.05, alpha=0.6) + 
		scale_fill_manual(values=c("black","blue","red"), labels=c("Other/None","Alpha","Delta")) + 
		facet_wrap(~factor(category_new)) + 
		geom_vline(data=cordf_strat,aes(xintercept=wrcor)) + 
		theme_classic() + 
		labs(x="Permuted correlations, peak viral load",
			y="Count",
			fill="First infection\nlineage") + 
		theme(text=element_text(size=9), strip.text.x=element_blank(),axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

ggsave(dpcorhist_strat, file=paste0(savedir,"dpcorhist_strat.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(wpcorhist_strat, file=paste0(savedir,"wpcorhist_strat.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(wrcorhist_strat, file=paste0(savedir,"wrcorhist_strat.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)

ggsave(dpcorhist_strat, file=paste0(savedir,"dpcorhist_strat.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(wpcorhist_strat, file=paste0(savedir,"wpcorhist_strat.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(wrcorhist_strat, file=paste0(savedir,"wrcorhist_strat.png"), width=stdwidth, height=stdheight, dpi=stddpi)

# =============================================================================
# More scatters
# =============================================================================


fig_dpscatter_overlap <- repinf_order_df %>% 
	ggplot(aes(x=dporder.1, y=dporder.2)) + 
		geom_point(aes(col=category.1),size=1, alpha=0.6) + 
		geom_line(aes(col=category.1), stat="smooth",method="lm", size=0.4, alpha=0.8, lty="dashed") + 
		stat_smooth(method="lm", col="black", size=0.8) + 
		scale_color_manual(values=c("1"="black","2"="blue","3"="red")) + 
		coord_fixed(ratio=1, xlim=c(0,1), ylim=c(0,1)) + 
		labs(x="Relative peak viral load,\ninfection 1", y="Relative peak viral load,\ninfection 2") +
		theme_classic() + 
		theme(legend.position="none", text=element_text(size=9))

ggsave(fig_dpscatter_overlap, file=paste0(savedir,"dpscatter_overlap.pdf"), width=3, height=3, dpi=stddpi)
ggsave(fig_dpscatter_overlap, file=paste0(savedir,"dpscatter_overlap.png"), width=3, height=3, dpi=stddpi)

fig_wpscatter_overlap <- repinf_order_df %>% 
	ggplot(aes(x=wporder.1, y=wporder.2)) + 
		geom_point(aes(col=category.1),size=1, alpha=0.6) + 
		geom_line(aes(col=category.1), stat="smooth",method="lm", size=0.4, alpha=0.8, lty="dashed") + 
		stat_smooth(method="lm", col="black", size=0.8) + 
		scale_color_manual(values=c("1"="black","2"="blue","3"="red")) + 
		coord_fixed(ratio=1, xlim=c(0,1), ylim=c(0,1)) + 
		labs(x="Relative proliferation time,\ninfection 1", y="Relative proliferation time,\ninfection 2") +
		theme_classic() + 
		theme(legend.position="none", text=element_text(size=9))

ggsave(fig_wpscatter_overlap, file=paste0(savedir,"wpscatter_overlap.pdf"), width=3, height=3, dpi=stddpi)
ggsave(fig_wpscatter_overlap, file=paste0(savedir,"wpscatter_overlap.png"), width=3, height=3, dpi=stddpi)

fig_wrscatter_overlap <- repinf_order_df %>% 
	ggplot(aes(x=wrorder.1, y=wrorder.2)) + 
		geom_point(aes(col=category.1), size=1, alpha=0.8) + 
		geom_line(aes(col=category.1), stat="smooth",method="lm", size=0.4, alpha=0.6, lty="dashed") + 
		stat_smooth(method="lm", col="black", size=0.8) + 
		scale_color_manual(values=c("1"="black","2"="blue","3"="red")) + 
		coord_fixed(ratio=1, xlim=c(0,1), ylim=c(0,1)) + 
		labs(x="Relative clearance time,\ninfection 1", y="Relative clearance time,\ninfection 2") +
		theme_classic() + 
		theme(legend.position="none", text=element_text(size=9))


ggsave(fig_wrscatter_overlap, file=paste0(savedir,"wrscatter_overlap.pdf"), width=3, height=3, dpi=stddpi)
ggsave(fig_wrscatter_overlap, file=paste0(savedir,"wrscatter_overlap.png"), width=3, height=3, dpi=stddpi)



