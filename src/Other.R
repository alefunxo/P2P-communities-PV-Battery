


###########################FIG 3B  trading patterns community###################################
d<-read.table('trading_strategies/Optim_trading.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)


d
scr_hh_sc <- median(subset(d,(d$Comm=='SC')&(d$index=='PV_batt'))$SCR_hh)
ssr_hh_sc <- median(subset(d,(d$Comm=='SC')&(d$index=='PV_batt'))$SSR_hh)
bill_hh_sc <- median(subset(d,(d$Comm=='SC')&(d$index=='PV_batt'))$bill_hh)

d<-subset(d,(d$Comm=='P2P')&(d$index=='PV_batt'))
d$trading<-ordered(d$trading, levels =  c("Low", "Normal", "High"))

d.m <- melt(select(d,-X), id.var = c("trading","index", "Comm"))
head(d.m)
unique(d.m$variable)

dummy1 <- data.frame("variable" = c("SCR_hh","SSR_hh","bill_hh"), Z = c(scr_hh_sc,ssr_hh_sc, bill_hh_sc))

p_hh <- ggboxplot(d.m, x= "trading",y="value",fill="trading")
p_hh <- p_hh + geom_hline(data=dummy1, aes(yintercept = Z),colour="red",linetype="longdash")
p_hh <- p_hh + facet_wrap(~variable, scales="free_y",ncol=1,
                          labeller=as_labeller(c("SCR_hh"="SC [%]","SSR_hh"="Autarky [%]","bill_hh"="Bill [€]")))
p_hh <- p_hh + stat_compare_means(label.x=1.5)

p_hh <- p_hh + scale_fill_discrete(name =  'Dark2')
p_hh <- p_hh + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")

p_hh <- p_hh + theme_bw()
p_hh <- p_hh + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_hh <- p_hh + xlab('Amount of trading in P2P community')
p_hh <- p_hh + theme(legend.position="bottom",legend.text=element_text(size=14),
                     legend.title = element_text(size=16,face="bold"))
p_hh <- p_hh + labs(subtitle = "A) Households with PV and battery",face="bold")
p_hh <- p_hh + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_hh <- p_hh + theme(axis.text=element_text(size=14),
                     axis.title=element_text(size=14,face="bold"))
p_hh <- p_hh + guides(colour=FALSE, fill=FALSE)
#p_hh
###########################FIG 3B2  trading patterns hh###################################

d2<-read.table('Opt_trading_diff.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
d2
#get only P2P PV_batt
d_<-subset(d2,(d2$index=='PV_batt'))
d_$trading<-ordered(d_$trading, levels =  c("Low", "Normal", "High"))
d_

d_.m <- melt(select(d_,-X), id.var = c("trading","index"))
head(d_.m)
unique(d_.m$variable)

ann_text <- data.frame(mpg = 15,wt = 5,lab = "Text",
                       variable = factor(c('SCR_hh','SSR_hh')))
ann_text2 <- data.frame(mpg = 15,wt = 5,lab = "Text",
                        variable = factor(c('bill_hh')))
p_hh2 <- ggboxplot(d_.m, x= "trading",y="value",fill="trading")
#p_hh2 <- p_hh2 + geom_hline(data=dummy1, aes(yintercept = Z),colour="red",linetype="longdash")
#p_hh2 <- p_hh2 + geom_text(data = ann_text,aes(x=2, 
#                             label="A positive difference implies that\n the Self-consumption community performs better than the P2P community", 
#                             y=2), colour="black",alpha=1)
#p_hh2 <- p_hh2 + geom_text(data = ann_text2,aes(x=2, 
#                                             label="A positive difference implies that\n the P2P community performs better than the Self-consumption community", 
#                                             y=0), colour="black",alpha=1)
p_hh2 <- p_hh2 + facet_wrap(~variable, scales="free_y",ncol=1,
                            strip.position = "left", 
                            labeller = as_labeller(c("SCR_hh"="Difference of SC [%]",
                                                     "SSR_hh"="Difference of Autarky [%]",
                                                     "bill_hh"="Difference of Bill [€]")))

p_hh2 <- p_hh2 + stat_compare_means(label.x=1)+ylab(NULL)

p_hh2 <- p_hh2 + scale_fill_discrete(name =  'Dark2')
p_hh2 <- p_hh2 + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")

p_hh2 <- p_hh2 + theme_bw()
p_hh2 <- p_hh2 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_hh2 <- p_hh2 + xlab('Trading in the P2P community')
p_hh2 <- p_hh2 + theme(legend.position="bottom",legend.text=element_text(size=14),
                       legend.title = element_text(size=16,face="bold"))

p_hh2 <- p_hh2 + labs(subtitle = "A) Households with PV and battery",face="bold")
p_hh2 <- p_hh2 + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_hh2 <- p_hh2 + theme(axis.text=element_text(size=14),
                       axis.title=element_text(size=14,face="bold"))
p_hh2 <- p_hh2 + guides(colour=FALSE, fill=FALSE)
p_hh2
###########################FIG 3B2 trading patterns community###################################
d2<-read.table('Optim_trading_comm.csv',sep=',',header=TRUE,stringsAsFactors = TRUE)
d2
scr_sc <- median(subset(d2,(d2$Comm=='SC'))$SCR)
ssr_sc <- median(subset(d2,(d2$Comm=='SC'))$SSR)
bill_sc <- median(subset(d2,(d2$Comm=='SC'))$bill)
dummy2 <- data.frame("variable" = c("SCR","SSR","bill"), Z = c(scr_sc,ssr_sc, bill_sc))

d2<-subset(d2,d2$Comm=='P2P')
d2$trading<-ordered(d2$trading, levels = c("Low", "Normal", "High"))
d2.m <- melt(select(d2,-X), id.var = c("trading","Comm"))


p_comm <- ggboxplot(d2.m, x= "trading",y="value",fill="trading")
p_comm <- p_comm + geom_hline(data=dummy2, aes(yintercept = Z),colour="red",linetype="longdash")
p_comm <- p_comm + facet_wrap(~variable, scales="free_y", ncol= 1,
                              labeller=as_labeller(c("SCR"="SC [%]","SSR"="Autarky [%]","bill"="Bill [Thousands €]")))
p_comm <- p_comm + stat_compare_means(label.x.npc=0.4,label.y.npc = 0.05)
p_comm <- p_comm + scale_fill_discrete(name =  'Dark2')
p_comm <- p_comm + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")
p_comm <- p_comm + theme_bw()
p_comm <- p_comm + scale_y_continuous(labels = addUnits)
p_comm <- p_comm + xlab('Trading in the P2P community')
p_comm <- p_comm + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_comm <- p_comm + theme(legend.position="bottom",legend.text=element_text(size=14),
                         legend.title = element_text(size=16,face="bold"))
p_comm2 <- p_comm2 + labs(subtitle = "B) Community level",face="bold")
p_comm <- p_comm + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_comm <- p_comm + theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=14,face="bold"))
p_comm <- p_comm + guides(colour=FALSE, fill=FALSE)

#p_comm


grid.arrange(p_hh,p_comm,nrow=1, ncol=2)


###########################FIG 3B2  trading patterns community annotated ###################################
d2_<-read.table('Optim_trading_comm_diff.csv',sep=',',header=TRUE,stringsAsFactors = TRUE)
d2_
d2_$trading<-ordered(d2_$trading, levels = c("Low", "Normal", "High"))
d2_.m <- melt(select(d2_,-X), id.var = c("trading","Comm"))

ann_text <- data.frame(mpg = 15,wt = 5,lab = "Text",
                       variable = factor(c('SCR')))
ann_text2 <- data.frame(mpg = 15,wt = 5,lab = "Text",
                        variable = factor(c('SSR')))
ann_text3 <- data.frame(mpg = 15,wt = 5,lab = "Text",
                        variable = factor(c('bill')))


p_comm2 <- ggboxplot(d2_.m, x= "trading",y="value",fill="trading")
#p_comm2 <- p_comm2 + geom_hline(data=dummy2, aes(yintercept = Z),colour="red",linetype="longdash")
p_comm2 <- p_comm2 + facet_wrap(~variable, scales="free_y", ncol= 1,
                                strip.position = "left", 
                                labeller = as_labeller(c("SCR"="Difference of SC [%]",
                                                         "SSR"="Difference of Autarky [%]",
                                                         "bill"="Difference of Bill [thousands of €]")))

#p_comm2 <- p_comm2 + geom_text(data = ann_text,aes(x=2, 
#                                             label="A positive difference implies that\n the Self-consumption community performs better than the P2P community", 
#                                             y=-4.5), colour="black",alpha=1)

#p_comm2 <- p_comm2 + geom_text(data = ann_text2,aes(x=2, 
#                                                 label="A positive difference implies that\n the Self-consumption community performs better than the P2P community", 
#                                                 y=-4.5), colour="black",alpha=1)
#p_comm2 <- p_comm2 + geom_text(data = ann_text3,aes(x=2, 
#                                              label="A positive difference implies that\n the P2P community performs better than the Self-consumption community", 
#                                              y=15000), colour="black",alpha=1)

p_comm2 <- p_comm2 + stat_compare_means(label.x.npc=0.4)
p_comm2 <- p_comm2 + scale_fill_discrete(name =  'Dark2')
p_comm2 <- p_comm2 + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")
p_comm2 <- p_comm2 + theme_bw()+ylab(NULL)
p_comm2 <- p_comm2 + scale_y_continuous(labels = addUnits)
p_comm2 <- p_comm2 + xlab('Trading in the P2P community')
p_comm2 <- p_comm2 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_comm2 <- p_comm2 + theme(legend.position="bottom",legend.text=element_text(size=14),
                           legend.title = element_text(size=16,face="bold"))
p_comm2 <- p_comm2 + labs(subtitle = "B) Community level",face="bold")
p_comm2 <- p_comm2 + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_comm2 <- p_comm2 + theme(axis.text=element_text(size=14),
                           axis.title=element_text(size=14,face="bold"))
p_comm2 <- p_comm2 + guides(colour=FALSE, fill=FALSE)

p_comm2


grid.arrange(p_hh2,p_comm2,nrow=1, ncol=2)

#ggsave('../Img/P2P_diff2.pdf',plot=grid.arrange(p_hh2,p_comm2,nrow=1, ncol=2),width=15, height=8.5,
encoding = "ISOLatin9.enc")

#ggsave('../Img/P2P_diff.pdf',plot=grid.arrange(p_hh,p_comm,nrow=1, ncol=2),width=15, height=8.5,
encoding = "ISOLatin9.enc")
###########################FIG ULF  trading patterns scattered###################################
a<-read.table('hh_50_50_DE_0.25_100_low.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
b<-read.table('hh_50_50_DE_0.25_100_high.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
c<-read.table('hh_50_50_DE_0.25_100_False.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
dim(a)
dim(b)
dim(c)
a$p2p<-'Low'
b$p2p<-'High'
c$p2p<-'Normal'
u<-rbind(rbind(a,b),c)
write.csv(u,"hh_50_50_DE_0.25_100_all.csv")



dim(u)
u<-subset(u,u$type=='PV_batt')
dim(u)
u$relative_diff<-u$diff_bill/(u$bill-u$diff_bill)#diff/sc_bill
u$sell_norm<-u$sell/max(u$sell)
u$sell
u$sell_norm
p_ulf <- ggboxplot(u, aes(x=sell_norm,y=diff_bill,color=p2p))
p_ulf <- p_ulf + geom_point()
p_ulf <- p_ulf + theme_bw()
p_ulf <- p_ulf + geom_hline(aes(yintercept = 0),colour="red",linetype="longdash")
p_ulf <- p_ulf + geom_text(aes(x=0.5, label="p2p_bill>SC_bill", y=90), colour="black")
p_ulf <- p_ulf + geom_text(aes(x=0.5, label="p2p_bill<SC_bill", y=-90), colour="black")

p_ulf <- p_ulf + xlab('Trading decile rank')
p_ulf <- p_ulf + ylab('Bill difference\n(P2P-SC)[€ p.a.]')
p_ulf <- p_ulf + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_ulf <- p_ulf + theme(legend.position="bottom",legend.text=element_text(size=14),
                       legend.title = element_text(size=16,face="bold"))
p_ulf <- p_ulf + labs(subtitle = "Households with PV and battery",face="bold")
p_ulf <- p_ulf + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_ulf <- p_ulf + theme(axis.text=element_text(size=14),
                       axis.title=element_text(size=14,face="bold"))

p_ulf


df_bill<-read.table('diff_bill.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
df_u<-read.table('../Output/aggregated_results.csv',sep=',',header=TRUE,stringsAsFactors = TRUE)
df_u_sc<-subset(df_u,(df_u$Comm=='SC')&(df_u$type=='PV_batt'))
df_u_p<-subset(df_u,(df_u$type=='PV_batt')&(df_u$Comm=='P2P'))

p_ulf2 <- ggboxplot(df_bill, x="cut",y="X0")
#p_ulf2 <- p_ulf2 + geom_point()
p_ulf2 <- p_ulf2 + theme_bw()
p_ulf2 <- p_ulf2 + geom_hline(aes(yintercept = 0),colour="red",linetype="longdash")
p_ulf2 <- p_ulf2 + geom_text(aes(x=2.5, label="p2p_bill>SC_bill", y=90), colour="black")
p_ulf2 <- p_ulf2 + geom_text(aes(x=2.5, label="p2p_bill<SC_bill", y=-90), colour="black")
#p_ulf2 <- p_ulf2 + xlab('Amount of trading in P2P community')
p_ulf2 <- p_ulf2 + ylab('Bill difference\n(P2P-SC)[€ p.a.]')
p_ulf2 <- p_ulf2 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_ulf2 <- p_ulf2 + theme(legend.position="bottom",legend.text=element_text(size=14),
                         legend.title = element_text(size=16,face="bold"))
p_ulf2 <- p_ulf2 + labs(subtitle = "Households with PV and battery",face="bold")
p_ulf2 <- p_ulf2 + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_ulf2 <- p_ulf2 + theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=14,face="bold"))
p_ulf2

p_ulf3 <- ggboxplot(df_u_p, x="cut",y="SCR_comm")

p_ulf3 <- p_ulf3 + theme_bw()
p_ulf3 <- p_ulf3 + scale_y_continuous(limits = c(40,60))
#p_ulf3 <- p_ulf3 + scale_y_continuous("SSR_comm", sec.axis = sec_axis(~ ., name = "Autarky\n[%]"))
#p_ulf3 <- p_ulf3 + xlab('Amount of trading in P2P community')
p_ulf3 <- p_ulf3 + ylab('SC\n[%]')
p_ulf3 <- p_ulf3 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_ulf3 <- p_ulf3 + theme(legend.position="bottom",legend.text=element_text(size=14),
                         legend.title = element_text(size=16,face="bold"))
#p_ulf3 <- p_ulf3 + labs(subtitle = "Households with PV and battery",face="bold")
p_ulf3 <- p_ulf3 + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_ulf3 <- p_ulf3 + theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=14,face="bold"))


df_u_p$SSR_comm
p_ulf4 <- ggboxplot(df_u_p, x="cut",y="SSR_comm")

p_ulf4 <- p_ulf4 + theme_bw()
p_ulf4 <- p_ulf4 + scale_y_continuous(limits = c(40,60))
#p_ulf4 <- p_ulf4 + scale_y_continuous("SSR_comm", sec.axis = sec_axis(~ ., name = "Autarky\n[%]"))
#p_ulf4 <- p_ulf4 + xlab('Amount of trading in P2P community')
p_ulf4 <- p_ulf4 + ylab('Autarky\n[%]')
p_ulf4 <- p_ulf4 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_ulf4 <- p_ulf4 + theme(legend.position="bottom",legend.text=element_text(size=14),
                         legend.title = element_text(size=16,face="bold"))
#p_ulf4 <- p_ulf4 + labs(subtitle = "Households with PV and battery",face="bold")
p_ulf4 <- p_ulf4 + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_ulf4 <- p_ulf4 + theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=14,face="bold"))
grid.newpage()
grid.draw(rbind(rbind(ggplotGrob(p_ulf2), ggplotGrob(p_ulf3), size = "last"), ggplotGrob(p_ulf4), size = "last"))

#ggsave('../Img/p_Ulf1.pdf',plot=grid.draw(rbind(rbind(ggplotGrob(p_ulf2),
ggplotGrob(p_ulf3), size = "last"), ggplotGrob(p_ulf4), size = "last"))
,
width=15, height=8.5,
encoding = "ISOLatin9.enc")
###################################FIG ULF Participation index########################################
df_PI<-read.table('deciles_PI.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)

p_PI <- ggboxplot(df_PI, x="cut",y="SC")

p_PI <- p_PI + theme_bw()

#p_PI <- p_PI + scale_y_continuous("SSR_comm", sec.axis = sec_axis(~ ., name = "Autarky\n[%]"))
p_PI <- p_PI + xlab('Trading deciles within the P2P community')
p_PI <- p_PI + ylab('Participation\nwillingness index[%]')
p_PI <- p_PI + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_PI <- p_PI + theme(legend.position="bottom",legend.text=element_text(size=14),
                     legend.title = element_text(size=16,face="bold"))
#p_PI <- p_PI + labs(subtitle = "Households with PV and battery",face="bold")
p_PI <- p_PI + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_PI <- p_PI + theme(axis.text=element_text(size=14),
                     axis.title=element_text(size=14,face="bold"))
p_PI

#ggsave('../Img/p_PI.pdf',plot=p_PI,
width=15, height=8.5,
encoding = "ISOLatin9.enc")
###################################FIG ULF ONLY MEDIAN########################################




df_bill<-read.table('diff_bill.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
df_u<-read.table('../Output/aggregated_results.csv',sep=',',header=TRUE,stringsAsFactors = TRUE)
df_u_sc<-subset(df_u,(df_u$Comm=='SC')&(df_u$type=='PV_batt'))
df_u_p<-subset(df_u,(df_u$type=='PV_batt')&(df_u$Comm=='P2P'))

df_bill_median<-df_bill%>%
  group_by(cut)%>% 
  summarise(Mean=mean(X0))
df_u_p_med<-df_u_p%>%
  group_by(cut)%>% 
  summarise(Mean_SCR=mean(SCR_comm),Mean_SSR=mean(SSR_comm),Mean_SSR_hh=mean(SSR),
            Mean_SCR_hh=mean(SCR_comm))
p_ulf2 <- ggplot(df_bill_median, aes(x=cut,y=Mean))
p_ulf2 <- p_ulf2 + geom_point()
p_ulf2 <- p_ulf2 + theme_bw()
p_ulf2 <- p_ulf2 + geom_hline(aes(yintercept = 0),colour="red",linetype="longdash")
p_ulf2 <- p_ulf2 + geom_text(aes(x=2.5, label="p2p_bill>SC_bill", y=90), colour="black")
p_ulf2 <- p_ulf2 + geom_text(aes(x=2.5, label="p2p_bill<SC_bill", y=-90), colour="black")
#p_ulf2 <- p_ulf2 + xlab('Amount of trading in P2P community')
p_ulf2 <- p_ulf2 + ylab('Bill difference\n(P2P-SC)[€ p.a.]')
p_ulf2 <- p_ulf2 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_ulf2 <- p_ulf2 + theme(legend.position="bottom",legend.text=element_text(size=14),
                         legend.title = element_text(size=16,face="bold"))
p_ulf2 <- p_ulf2 + labs(subtitle = "Households with PV and battery",face="bold")
p_ulf2 <- p_ulf2 + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_ulf2 <- p_ulf2 + theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=14,face="bold"))
p_ulf2

p_ulf3 <- ggplot(df_u_p_med, aes(x=cut,y=Mean_SCR))
p_ulf3 <- p_ulf3 + geom_point()
p_ulf3 <- p_ulf3 + theme_bw()
p_ulf3 <- p_ulf3 + scale_y_continuous(limits = c(40,60))
#p_ulf3 <- p_ulf3 + scale_y_continuous("SSR_comm", sec.axis = sec_axis(~ ., name = "Autarky\n[%]"))
#p_ulf3 <- p_ulf3 + xlab('Amount of trading in P2P community')
p_ulf3 <- p_ulf3 + ylab('SC\n[%]')
p_ulf3 <- p_ulf3 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_ulf3 <- p_ulf3 + theme(legend.position="bottom",legend.text=element_text(size=14),
                         legend.title = element_text(size=16,face="bold"))
#p_ulf3 <- p_ulf3 + labs(subtitle = "Households with PV and battery",face="bold")
p_ulf3 <- p_ulf3 + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_ulf3 <- p_ulf3 + theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=14,face="bold"))

p_ulf4 <- ggplot(df_u_p_med, aes(x=cut,y=Mean_SSR))
p_ulf4 <- p_ulf4 + geom_point()
p_ulf4 <- p_ulf4 + theme_bw()
p_ulf4 <- p_ulf4 + scale_y_continuous(limits = c(40,60))
#p_ulf4 <- p_ulf4 + scale_y_continuous("SSR_comm", sec.axis = sec_axis(~ ., name = "Autarky\n[%]"))
#p_ulf4 <- p_ulf4 + xlab('Amount of trading in P2P community')
p_ulf4 <- p_ulf4 + ylab('Autarky\n[%]')
p_ulf4 <- p_ulf4 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_ulf4 <- p_ulf4 + theme(legend.position="bottom",legend.text=element_text(size=14),
                         legend.title = element_text(size=16,face="bold"))
#p_ulf4 <- p_ulf4 + labs(subtitle = "Households with PV and battery",face="bold")
p_ulf4 <- p_ulf4 + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_ulf4 <- p_ulf4 + theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=14,face="bold"))
grid.newpage()
grid.draw(rbind(rbind(ggplotGrob(p_ulf2), ggplotGrob(p_ulf3), size = "last"), ggplotGrob(p_ulf4), size = "last"))

#ggsave('../Img/p_Ulf2.pdf',plot=grid.draw(rbind(rbind(ggplotGrob(p_ulf2), 
ggplotGrob(p_ulf3), size = "last"), ggplotGrob(p_ulf4), size = "last")),
width=15, height=8.5,
encoding = "ISOLatin9.enc")
df_u_p_med
cbind(df_bill_median,df_u_p_med)
###########################FIG AUX###################################
d<-read.table('Optim_trading.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)

d1<-subset(d,d$Comm=='P2P')
p_bill <- ggboxplot(d1, x= "trading",y="bill_hh",fill="trading")+facet_grid(~index)

p_bill <- p_bill + stat_compare_means(label.y=800)

p_bill <- p_bill + scale_colour_manual(values = alpha(c("#F8766D", "#00BFC4"),.3),
                                       labels=c('P2P','Self-consumption'))
p_bill <- p_bill + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")

p_bill <- p_bill + ylab('Bill\n[€ p.a.]')
p_bill <- p_bill + xlab('P2P trading')
p_bill <- p_bill + theme_bw()
p_bill <- p_bill + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))

p_bill <- p_bill + theme(legend.position="bottom",legend.text=element_text(size=14),
                         legend.title = element_text(size=16,face="bold"))
#p_bill <- p_bill + labs(subtitle = "C) Household level",face="bold")
p_bill <- p_bill + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_bill <- p_bill + theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=14,face="bold"))
p_bill <- p_bill + guides(colour=FALSE, fill=FALSE)


p_ssr <- ggboxplot(d1, x= "trading",y="SSR_hh",fill="trading")+facet_grid(~index)

p_ssr <- p_ssr + stat_compare_means()

p_ssr <- p_ssr + scale_colour_manual(values = alpha(c("#F8766D", "#00BFC4"),.3),
                                     labels=c('P2P','Self-consumption'))
p_ssr <- p_ssr + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")

p_ssr <- p_ssr + ylab('Autarky\n[%]')
p_ssr <- p_ssr + xlab('P2P trading')
p_ssr <- p_ssr + theme_bw()
p_ssr <- p_ssr + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))

p_ssr <- p_ssr + theme(legend.position="bottom",legend.text=element_text(size=14),
                       legend.title = element_text(size=16,face="bold"))
#p_ssr <- p_ssr + labs(subtitle = "C) Household level",face="bold")
p_ssr <- p_ssr + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_ssr <- p_ssr + theme(axis.text=element_text(size=14),
                       axis.title=element_text(size=14,face="bold"))
p_ssr <- p_ssr + guides(colour=FALSE, fill=FALSE)


p_scr <- ggboxplot(d1, x= "trading",y="SCR_hh",fill="trading")+facet_grid(~index)
p_scr <- p_scr + stat_compare_means()

p_scr <- p_scr + scale_colour_manual(values = alpha(c("#F8766D", "#00BFC4"),.3),
                                     labels=c('P2P','Self-consumption'))
p_scr <- p_scr + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")

p_scr <- p_scr + ylab('SCR\n[%]')
p_scr <- p_scr + xlab('P2P trading')
p_scr <- p_scr + theme_bw()
p_scr <- p_scr + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))

p_scr <- p_scr + theme(legend.position="bottom",legend.text=element_text(size=14),
                       legend.title = element_text(size=16,face="bold"))
#p_scr <- p_scr + labs(subtitle = "C) Household level",face="bold")
p_scr <- p_scr + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_scr <- p_scr + theme(axis.text=element_text(size=14),
                       axis.title=element_text(size=14,face="bold"))
p_scr <- p_scr + guides(colour=FALSE, fill=FALSE)
grid.arrange(p_scr,p_ssr,p_bill,p_bill2,nrow=2, ncol=2)


####################Other#######################
al=0.02
p_peak2 <- p_peak2 + geom_rect(aes(xmin = 0,xmax = 24,
                                   ymin = -Inf, ymax = Inf, fill = 'Monday'), alpha = al)
p_peak2 <- p_peak2 + scale_fill_brewer(palette = 'Pastel1', name = 'Day of the week')
p_peak2 <- p_peak2 + geom_rect(aes(xmin = 24,xmax = 48,
                                   ymin = -Inf, ymax = Inf, fill = 'Tuesday'), alpha = al)
p_peak2 <- p_peak2 + geom_rect(aes(xmin = 48,xmax = 72,
                                   ymin = -Inf, ymax = Inf, fill = 'Wednesday'), alpha = al)
p_peak2 <- p_peak2 + geom_rect(aes(xmin = 72,xmax = 96,
                                   ymin = -Inf, ymax = Inf, fill = 'Thursday'), alpha = al)
p_peak2 <- p_peak2 + geom_rect(aes(xmin = 96,xmax = 120,
                                   ymin = -Inf, ymax = Inf, fill = 'Friday'), alpha = al)
p_peak2 <- p_peak2 + geom_rect(aes(xmin = 120,xmax = 144,
                                   ymin = -Inf, ymax = Inf, fill = 'Saturday'), alpha = al)
p_peak2 <- p_peak2 + geom_rect(aes(xmin = 144,xmax = 168,
                                   ymin = -Inf, ymax = Inf, fill = 'Sunday'), alpha = al)
p_peak2 <- p_peak2 + scale_fill_brewer(palette = 'Pastel1', name = 'Day of the week')
p_peak2 <- p_peak2 +  annotate("rect",xmin = 0,xmax = 24,
                               ymin = -Inf, ymax = Inf, fill = 'red', alpha = .1)
p_peak2 <- p_peak2 + annotate("rect", xmin = 24,xmax = 48,
                              ymin = -Inf, ymax = Inf, fill = 'Tuesday', alpha = .1)
p_peak2 <- p_peak2 + annotate("rect", xmin = 48,xmax = 72,
                              ymin = -Inf, ymax = Inf, fill = 'Wednesday', alpha = .1)
p_peak2 <- p_peak2 + annotate("rect", xmin = 72,xmax = 96,
                              ymin = -Inf, ymax = Inf, fill = 'Thursday', alpha = .1)
p_peak2 <- p_peak2 + annotate("rect", xmin = 96,xmax = 120,
                              ymin = -Inf, ymax = Inf, fill = 'Friday', alpha = .1)
p_peak2 <- p_peak2 + annotate("rect", xmin = 120,xmax = 144,
                              ymin = -Inf, ymax = Inf, fill = 'Saturday', alpha = .1)
p_peak2 <- p_peak2 + annotate("rect", xmin = 144,xmax = 168,
                              ymin = -Inf, ymax = Inf, fill = 'Sunday', alpha = .1)

df_2 <- subset(df,df$index=='No')

p_pi <- ggplot(df_2, aes(y=PI))
p_pi <- p_pi + geom_boxplot(fill="#F8766D")
p_pi <- p_pi + ylab('Participation willingness\n index [p.u.]')
p_pi <- p_pi + xlab('P2P')
p_pi <- p_pi + ylim(0,1)

p_pi <- p_pi + theme_bw()
p_pi <- p_pi + theme(legend.position="bottom",legend.text=element_text(size=14),
                     legend.title = element_text(size=16,face="bold"))
p_pi <- p_pi + labs(subtitle = "E) Participation index",face="bold")
p_pi <- p_pi + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_pi <- p_pi + theme(axis.text=element_text(size=14),
                     axis.title=element_text(size=14,face="bold"))

summary(df_2$PI)
