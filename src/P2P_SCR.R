library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggrepel)


library(gtools)

##########init#####################
#read the dataframe
setwd("/data/home/alejandropena/Psychology/src")
d<-read.table('R_plots_SC_P2P.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)

head(d)

p_bill <- ggboxplot(d, x= "Comm_P2P",y="bill")
p_bill <- p_bill + facet_wrap(~type, scales="free_y",ncol=1,
                              labeller = as_labeller(c("No"="No PV nor battery",
                                                       "PV"= "PV only",
                                                       "PV_batt"= "PV and battery")))
p_bill <- p_bill + stat_compare_means(label.x.npc = 0.85,label.y.npc = 0.95)
p_bill <- p_bill + theme_bw()
p_bill <- p_bill + ylab('Bill\n[€ p.a.]')
p_bill <- p_bill + xlab('Community type')
p_bill <- p_bill + scale_x_discrete(labels=c("SC" = "SC", "SC_P2P" = "SC with\nP2P pricing",
                            "P2P" = "P2P"))

p_bill <- p_bill + theme(legend.text=element_text(size=14),
                                 legend.title = element_text(size=16,face="bold"))
p_bill <- p_bill + theme(axis.text=element_text(size=14),
                                 axis.title=element_text(size=14,face="bold"))


p_scr <- ggboxplot(subset(d,d$type=='PV_batt'), x= "Comm_P2P",y="SCR")
p_scr <- p_scr + facet_wrap(~type, scales="free_y",ncol=1,
                            labeller = as_labeller(c("No"="No PV nor battery",
                                                     "PV"= "PV only",
                                                     "PV_batt"= "PV and battery")))
p_scr <- p_scr + stat_compare_means()
p_scr <- p_scr + theme_bw()
p_scr <- p_scr + ylab('SCR\n[%]')
p_scr <- p_scr + xlab('Community type')
p_scr <- p_scr + scale_x_discrete(labels=c("SC" = "SC", "SC_P2P" = "SC with\nP2P pricing",
                            "P2P" = "P2P"))

p_scr <- p_scr + theme(legend.text=element_text(size=14),
                                 legend.title = element_text(size=16,face="bold"))
p_scr <- p_scr + theme(axis.text=element_text(size=14),
                                 axis.title=element_text(size=14,face="bold"))


p_ssr <- ggboxplot(subset(d,d$type=='PV_batt'), x= "Comm_P2P",y="SSR")
p_ssr <- p_ssr + facet_wrap(~type, scales="free_y",ncol=1,
                            labeller = as_labeller(c("No"="No PV nor battery",
                                                     "PV"= "PV only",
                                                     "PV_batt"= "PV and battery")))
p_ssr <- p_ssr + stat_compare_means()
p_ssr <- p_ssr + theme_bw()
p_ssr <- p_ssr + ylab('Autarky\n[%]')
p_ssr <- p_ssr + xlab('Community type')
p_ssr <- p_ssr + scale_x_discrete(labels=c("SC" = "SC", "SC_P2P" = "SC with\nP2P pricing",
                            "P2P" = "P2P"))

p_ssr <- p_ssr + theme(legend.text=element_text(size=14),
                                 legend.title = element_text(size=16,face="bold"))
p_ssr <- p_ssr + theme(axis.text=element_text(size=14),
                                 axis.title=element_text(size=14,face="bold"))

p_bill_com <- ggboxplot(d, x= "Comm_P2P",y="Bill_comm")
p_bill_com <- p_bill_com + stat_compare_means()
p_bill_com <- p_bill_com + theme_bw()
p_bill_com <- p_bill_com + ylab('Community bill\n[€ p.a.]')
p_bill_com <- p_bill_com + xlab('Community type')
p_bill_com <- p_bill_com + scale_x_discrete(labels=c("SC" = "SC", "SC_P2P" = "SC with\nP2P pricing",
                                                   "P2P" = "P2P"))
p_bill_com <- p_bill_com + theme(legend.text=element_text(size=14),
                                 legend.title = element_text(size=16,face="bold"))
p_bill_com <- p_bill_com + theme(axis.text=element_text(size=14),
                                 axis.title=element_text(size=14,face="bold"))

p_scr_com <- ggboxplot(d, x= "Comm_P2P",y="SCR_comm")
p_scr_com <- p_scr_com + stat_compare_means()
p_scr_com <- p_scr_com + theme_bw()
p_scr_com <- p_scr_com + ylab('SCR\n[%]')
p_scr_com <- p_scr_com + xlab('Community type')
p_scr_com <- p_scr_com + scale_x_discrete(labels=c("SC" = "SC", "SC_P2P" = "SC with\nP2P pricing",
                                                   "P2P" = "P2P"))
p_scr_com <- p_scr_com + theme(legend.text=element_text(size=14),
                               legend.title = element_text(size=16,face="bold"))
p_scr_com <- p_scr_com + theme(axis.text=element_text(size=14),
                               axis.title=element_text(size=14,face="bold"))


p_ssr_com <- ggboxplot(d, x= "Comm_P2P",y="SSR_comm")
p_ssr_com <- p_ssr_com + stat_compare_means()
p_ssr_com <- p_ssr_com + theme_bw()
p_ssr_com <- p_ssr_com + ylab('Autarky\n[%]')
p_ssr_com <- p_ssr_com + xlab('Community type')
p_ssr_com <- p_ssr_com + scale_x_discrete(labels=c("SC" = "SC", "SC_P2P" = "SC with\nP2P pricing",
                              "P2P" = "P2P"))
p_ssr_com <- p_ssr_com + theme(legend.text=element_text(size=14),
                                 legend.title = element_text(size=16,face="bold"))
p_ssr_com <- p_ssr_com + theme(axis.text=element_text(size=14),
                                 axis.title=element_text(size=14,face="bold"))

lay <- rbind(c(2,1),
             c(2,1),
             c(3,1),
             c(3,1))
grid.arrange(p_bill,p_scr,p_ssr,layout_matrix = lay)
grid.arrange(p_bill_com,p_scr_com,p_ssr_com,nrow=1, ncol=3)

ggsave('../Img/comparison_ind.pdf',plot=grid.arrange(p_bill,p_scr,p_ssr,layout_matrix = lay),width=15, height=8.5,
       encoding = "ISOLatin9.enc")

ggsave('../Img/compariso_com.pdf',plot=grid.arrange(p_bill_com,p_scr_com,p_ssr_com,nrow=1, ncol=3),width=15, height=8.5,
       encoding = "ISOLatin9.enc")

