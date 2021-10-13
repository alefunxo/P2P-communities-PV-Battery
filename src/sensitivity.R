setwd("/data/home/alejandropena/Psychology/src")




colnames(df)
str(df)
p_inj <- ggboxplot(df, x="size_comm",y="Demand_peak")
p_inj <- p_inj + facet_grid(PV_penetration_x~Batt_penetration_x,
                                      labeller = as_labeller(c("25"= "Battery penetration\n25%",
                                                               "50"= "Battery penetration\n50%",
                                                               "75"= "Battery penetration\n 75%")))

p_inj <- p_inj + theme_bw()
p_inj <- p_inj + labs(fill="Community size:")
p_inj <- p_inj + ylab('Power imports\n[kW.]')
p_inj <- p_inj + xlab('PV penetration [%]')

p_inj <- p_inj + theme(legend.text=element_text(size=14),
                                 legend.title = element_text(size=16,face="bold"))
p_inj <- p_inj + theme(axis.text=element_text(size=14),
                                 axis.title=element_text(size=14,face="bold"))
p_inj
unique(df$PV_penetration_x)
p_exp <- ggboxplot(df, x="size_comm",y="Inj_peak")
p_exp <- p_exp + facet_grid(PV_penetration_x~Batt_penetration_x,
                            labeller = as_labeller(c("25"= "Battery penetration\n25%",
                                                     "50"= "Battery penetration\n50%",
                                                     "75"= "Battery penetration\n 75%")))

p_exp <- p_exp + theme_bw()
p_exp <- p_exp + labs(fill="Community size:")
p_exp <- p_exp + ylab('Power exports\n[kW]')
p_exp <- p_exp + xlab('PV penetration [%]')

p_exp <- p_exp + theme(legend.text=element_text(size=14),
                           legend.title = element_text(size=16,face="bold"))
p_exp <- p_exp + theme(axis.text=element_text(size=14),
                           axis.title=element_text(size=14,face="bold"))
p_exp

length(df_mean$Imported)
