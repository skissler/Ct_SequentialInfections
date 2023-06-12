
pairedinfs_formatted <- ct_dat_refined %>% 
	filter(InPairedAnalysis==1) %>% 
	group_by(IQVIA_ID, FirstPosPCR) %>% 
	slice(1) %>% 
	select(IQVIA_ID, FirstPosPCR, LineageBroad, InfNum) %>% 
	mutate(linbin=case_when(
		LineageBroad%in%c("Other","None")~"Other/None",
		LineageBroad=="Alpha"~"Alpha",
		LineageBroad=="Delta"~"Delta",
		LineageBroad%in%c("BA.1","BA.2")~"BA.1/BA.2",
		TRUE~NA_character_
		)) %>% 
	(function(x){
		out <- x %>% 
			right_join(expand_grid(IQVIA_ID=unique(x$IQVIA_ID), InfNum=1:2),by=c("IQVIA_ID","InfNum")) %>% 
			mutate(FirstPosPCR=case_when(
				is.na(LineageBroad) & InfNum==1 ~ ymd("1950-01-01"),
				is.na(LineageBroad) & InfNum==2 ~ ymd("2050-01-01"),
				TRUE~FirstPosPCR
				)) %>% 
			mutate(Linetype=case_when(
				is.na(LineageBroad)  ~ 0,
				TRUE~1
				)) %>% 
			group_by(IQVIA_ID) %>% 
			mutate(Linetype=min(Linetype)) %>% 
			ungroup() %>% 
			mutate(Linetype=factor(Linetype)) %>% 
			replace_na(list(LineageBroad="Other/None",linbin="Other/None"))
		return(out)
		}) %>% 
	mutate(linbin=factor(linbin,levels=c("Other/None","Alpha","Delta","BA.1/BA.2"))) %>% 
	(function(x){

		yvaldf <- x %>% 
			group_by(IQVIA_ID) %>% 
			slice(1) %>% 
			ungroup() %>% 
			arrange((FirstPosPCR)) %>% 
			mutate(yval=(1:n())) %>% 
			select(IQVIA_ID, yval)

		out <- left_join(x, yvaldf, by="IQVIA_ID")

		return(out)

	})

fig_pairedinfs_linbins <- pairedinfs_formatted %>% 
	ggplot() + 
		theme_classic() + 
		scale_fill_manual(values=c("black","blue","red","magenta")) + 
		labs(x="First positive PCR", y="Count") + 
		geom_line(aes(x=FirstPosPCR, y=yval, group=IQVIA_ID, size=Linetype, alpha=Linetype)) + 
		scale_alpha_manual(values=c(0.1,0.5), guide="none") + 
		scale_size_manual(values=c(0.2,0.2), guide="none") + 
		scale_x_date(date_labels = "%b %Y") + 
		coord_cartesian(xlim = (c(ymd("2020-01-01"), ymd("2022-12-31")))) + 
		geom_point(data=pairedinfs_formatted, aes(x=FirstPosPCR, y=yval, col=linbin), size=0.8, alpha=0.5) + 
		scale_color_manual(values=c("black","blue","red","magenta")) + 
		scale_linetype_manual(values=c("dotted","solid")) + 
		theme(text=element_text(size=9), legend.title=element_blank())

# ggsave(fig_pairedinfs_linbins, file="figures/pairedinfs_linbins.pdf", width=figwidth, height=figheight, dpi=600)
# ggsave_legend(fig_pairedinfs_linbins, name="pairedinfs_linbins", width=figwidth, height=figheight, dpi=600)
