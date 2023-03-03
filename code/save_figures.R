savedir <- paste0("figures/run_pars_adj_",run_pars_index,"/")
stdwidth <- 3.2
stdheight <- 3.2*10/16
stddpi <- 600
striplegend <- list(theme(legend.position="none", plot.title=element_blank()))

ggsave(chainplot_dpadj, file=paste0(savedir,"chainplot_dpadj.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(chainplot_dpadj, file=paste0(savedir,"chainplot_dpadj.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(chainplot_wpadj, file=paste0(savedir,"chainplot_wpadj.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(chainplot_wpadj, file=paste0(savedir,"chainplot_wpadj.png"), width=stdwidth, height=stdheight, dpi=stddpi)

ggsave(fig_indivfits, file=paste0(savedir,"indivfits.pdf"), width=8, height=10)
ggsave(fig_indivfits, file=paste0(savedir,"indivfits.png"), width=8, height=10)

ggsave(fig_indivfits_mean, file=paste0(savedir,"indivfits_mean.pdf"), width=8, height=10)
ggsave(fig_indivfits_mean, file=paste0(savedir,"indivfits_mean.png"), width=8, height=10)


for(indexA in 1:length(fig_indivfits_cat)){
	ggsave(fig_indivfits_cat[[indexA]],file=paste0(savedir,"indivfits_cat_",indexA,".pdf"),width=2*stdwidth,height=2*stdwidth)
	ggsave(fig_indivfits_cat[[indexA]],file=paste0(savedir,"indivfits_cat_",indexA,".png"),width=2*stdwidth,height=2*stdwidth)
}

ggsave(fig_dpadjhist, file=paste0(savedir,"dpadjhist.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_dpadjhist, file=paste0(savedir,"dpadjhist.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_wpadjhist, file=paste0(savedir,"wpadjhist.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_wpadjhist, file=paste0(savedir,"wpadjhist.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_wradjhist, file=paste0(savedir,"wradjhist.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_wradjhist, file=paste0(savedir,"wradjhist.png"), width=stdwidth, height=stdheight, dpi=stddpi)

ggsave(fig_dphist, file=paste0(savedir,"dphist.pdf"), width=stdwidth, height=1.1*stdheight, dpi=stddpi)
ggsave(fig_dphist, file=paste0(savedir,"dphist.png"), width=stdwidth, height=1.1*stdheight, dpi=stddpi)
ggsave(fig_dphist + striplegend, file=paste0(savedir,"dphist_nolegend.pdf"), width=stdwidth, height=1.1*stdheight, dpi=stddpi)
ggsave(fig_dphist + striplegend, file=paste0(savedir,"dphist_nolegend.png"), width=stdwidth, height=1.1*stdheight, dpi=stddpi)

ggsave(fig_dphist_adjust, file=paste0(savedir,"dphist_adjust.pdf"), width=stdwidth, height=1.1*stdheight, dpi=stddpi)
ggsave(fig_dphist_adjust, file=paste0(savedir,"dphist_adjust.png"), width=stdwidth, height=1.1*stdheight, dpi=stddpi)

ggsave(fig_wphist, file=paste0(savedir,"wphist.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_wphist, file=paste0(savedir,"wphist.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_wphist+striplegend, file=paste0(savedir,"wphist_nolegend.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_wphist+striplegend, file=paste0(savedir,"wphist_nolegend.png"), width=stdwidth, height=stdheight, dpi=stddpi)

ggsave(fig_wphist_adjust, file=paste0(savedir,"wphist_adjust.pdf"), width=stdwidth, height=1.1*stdheight, dpi=stddpi)
ggsave(fig_wphist_adjust, file=paste0(savedir,"wphist_adjust.png"), width=stdwidth, height=1.1*stdheight, dpi=stddpi)

ggsave(fig_wrhist, file=paste0(savedir,"wrhist.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_wrhist, file=paste0(savedir,"wrhist.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_wrhist+striplegend, file=paste0(savedir,"wrhist_nolegend.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_wrhist+striplegend, file=paste0(savedir,"wrhist_nolegend.png"), width=stdwidth, height=stdheight, dpi=stddpi)

ggsave(fig_wrhist_adjust, file=paste0(savedir,"wrhist_adjust.pdf"), width=stdwidth, height=1.1*stdheight, dpi=stddpi)
ggsave(fig_wrhist_adjust, file=paste0(savedir,"wrhist_adjust.png"), width=stdwidth, height=1.1*stdheight, dpi=stddpi)

ggsave(fig_rphist, file=paste0(savedir,"rphist.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_rphist, file=paste0(savedir,"rphist.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_rphist+striplegend, file=paste0(savedir,"rphist_nolegend.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_rphist+striplegend, file=paste0(savedir,"rphist_nolegend.png"), width=stdwidth, height=stdheight, dpi=stddpi)

ggsave(fig_rrhist, file=paste0(savedir,"rrhist.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_rrhist, file=paste0(savedir,"rrhist.png"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_rrhist+striplegend, file=paste0(savedir,"rrhist_nolegend.pdf"), width=stdwidth, height=stdheight, dpi=stddpi)
ggsave(fig_rrhist+striplegend, file=paste0(savedir,"rrhist_nolegend.png"), width=stdwidth, height=stdheight, dpi=stddpi)


for(indexA in 1:length(fig_viztrajectories)){
	ggsave(fig_viztrajectories[[indexA]],file=paste0(savedir,"viztrajectories_",indexA,".pdf"),width=stdwidth,height=stdheight)
	ggsave(fig_viztrajectories[[indexA]],file=paste0(savedir,"viztrajectories_",indexA,".png"),width=stdwidth,height=stdheight)
}

for(indexA in 1:length(fig_viztrajectories_grey)){
	ggsave(fig_viztrajectories_grey[[indexA]],file=paste0(savedir,"viztrajectories_grey_",indexA,".pdf"),width=stdwidth,height=stdheight)
	ggsave(fig_viztrajectories_grey[[indexA]],file=paste0(savedir,"viztrajectories_grey_",indexA,".png"),width=stdwidth,height=stdheight)
}

for(indexA in 1:length(fig_viztrajectories_varcol)){
	ggsave(fig_viztrajectories_varcol[[indexA]],file=paste0(savedir,"viztrajectories_varcol_",indexA,".pdf"),width=stdwidth,height=stdheight)
	ggsave(fig_viztrajectories_varcol[[indexA]],file=paste0(savedir,"viztrajectories_varcol_",indexA,".png"),width=stdwidth,height=stdheight)
}

ggsave(fig_dp_whiskers, file=paste0(savedir,"dp_whiskers.pdf"), width=stdwidth, height=1.2*stdheight, dpi=stddpi)
ggsave(fig_dp_whiskers, file=paste0(savedir,"dp_whiskers.png"), width=stdwidth, height=1.2*stdheight, dpi=stddpi)

ggsave(fig_wp_whiskers, file=paste0(savedir,"wp_whiskers.pdf"), width=stdwidth, height=1.2*stdheight, dpi=stddpi)
ggsave(fig_wp_whiskers, file=paste0(savedir,"wp_whiskers.png"), width=stdwidth, height=1.2*stdheight, dpi=stddpi)

ggsave(fig_wr_whiskers, file=paste0(savedir,"wr_whiskers.pdf"), width=stdwidth, height=1.2*stdheight, dpi=stddpi)
ggsave(fig_wr_whiskers, file=paste0(savedir,"wr_whiskers.png"), width=stdwidth, height=1.2*stdheight, dpi=stddpi)
# }


write_csv(summdf_overall, file=paste0(savedir,"summdf.csv"))


