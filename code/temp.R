





# tempfig <- temp2 %>%
# 	ggplot() + 
# 		geom_segment(aes(x=-Inf, xend=-wp0, y=40, yend=40, col=assocrank), alpha=0.5, size=0.7) + 
# 		geom_segment(aes(x=-wp0, xend=0, y=40, yend=40-dp0, col=assocrank), alpha=0.5, size=0.7) +
# 		geom_segment(aes(x=0, xend=wr0, y=40-dp0, yend=40, col=assocrank), alpha=0.5, size=0.7) + 
# 		geom_segment(aes(x=wr0, xend=Inf, y=40, yend=40, col=assocrank), alpha=0.5, size=0.7) + 
# 		scale_y_reverse() + 
# 		scale_color_distiller(palette="RdBu") + 
# 		theme_classic() + 
# 		facet_wrap(~factor(category))






# temp2 <- temp %>% 
# 	select(iteration, category, PersonID, wr0) %>% 
# 	group_by(iteration, PersonID) %>% 
# 	pivot_wider(names_from=category, values_from=wr0) %>% 
# 	rename("infection1"=`3`) %>% 
# 	rename("infection2"=`4`) %>% 
# 	group_by(iteration) %>% 
# 	arrange(infection1) %>% 
# 	mutate(inf1order=1:n()) %>% 
# 	group_by(iteration) %>% 
# 	arrange(infection2) %>% 
# 	mutate(inf2order=1:n()) %>% 
# 	arrange(iteration, inf1order)


# temp2_upr <- temp2 %>% 
# 	group_by(inf1order) %>% 
# 	summarise(inf2order_mean=quantile(inf2order,0.8))


# temp2 %>% 
# 	group_by(inf1order, inf2order) %>% 
# 	summarise(N=n()) %>% 
# 	ggplot() + 
# 		geom_tile(aes(x=inf1order,y=inf2order,fill=N)) + 
# 		scale_fill_distiller(direction=1) + 
# 		theme_classic() 

# temp2 %>% 
# 	ggplot(aes(x=inf1order, y=inf2order)) + 
# 		geom_jitter(size=0.2, alpha=0.02, width=0.5) + 
# 		# geom_density_2d(adjust=2) + 
# 		# geom_line(data=temp2_mean, aes(x=inf1order, y=inf2order_mean)) + 
# 		theme_classic()  

# temp2 %>% 
# 	ggplot(aes(x=inf1order, y=inf2order)) + 
# 		geom_density_2d(bins=20) 


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






