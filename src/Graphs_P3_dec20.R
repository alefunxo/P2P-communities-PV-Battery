setwd("/data/home/alejandropena/Psychology/Output")

###################Load#####################################

library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(reshape2)
library(dplyr)

addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), ''),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}
##########init#####################
#read the dataframe
d<-read.table('df_core.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)

df<-read.table('final_dataset.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)

df_1 <- subset(df,df$index!='No')


############# FIG 1A hh SS/SC#########################
hjust1=-0.1
vjust1=-0.1
p_tss <- ggplot(df_1, aes(x=SCR_hh, y=SSR_hh,shape=factor(index), fill=factor(index),colour=factor(Comm)))
p_tss <- p_tss + geom_point(size=3)
p_tss <- p_tss + theme_bw()
p_tss <- p_tss + scale_shape_manual(values =c(16, 17),
                                    labels=c("Prosumer with\nPV","Prosumer with\nPV & Battery"))
p_tss <- p_tss + scale_colour_manual(values = alpha(c("#F8766D", "#00BFC4"),.3),
                                     labels=c('P2P','Self-consumption'))
p_tss <- p_tss + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")
p_tss <- p_tss + scale_y_continuous(limits=c(0,100),expand = c(0,0))
p_tss <- p_tss + scale_x_continuous(limits=c(0,100),expand = c(0,0))
p_tss <- p_tss + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
#p_tss <- p_tss + coord_cartesian(xlim=c(0,100))
p_tss <- p_tss + xlab('SC [%]')
p_tss <- p_tss + ylab('Autarky\n[%]')
p_tss <- p_tss + geom_abline(aes(intercept = 0,slope=1))
p_tss <- p_tss + scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) 

p_tss <- p_tss + theme(legend.position="bottom",legend.text=element_text(size=12),
                       legend.title = element_text(size=15,face="bold"))
p_tss <- p_tss + labs(subtitle = "B)",face="bold")
p_tss <- p_tss + theme(plot.subtitle = element_text(hjust = hjust1,vjust=vjust1,size=16,face="bold"))
p_tss <- p_tss + theme(axis.text=element_text(size=14),
                       axis.title=element_text(size=14))
p_tss <- p_tss + guides(fill=FALSE,colour=guide_legend(nrow=2,byrow=TRUE),shape=guide_legend(nrow=2,byrow=TRUE))
p_tss <- p_tss + geom_text(aes(x=10, label="Net producer", y=90), colour="black")
p_tss <- p_tss + geom_text(aes(x=90, label="Net consumer", y=10), colour="black")
#p_tss                    
#p_tss <- p_tss + facet_grid(~Comm)

############# FIG 1B Community SS/SC#########################
p_tss2 <- ggplot(df, aes(x=SCR, y=SSR,colour=factor(Comm)))#,colour=factor(Comm)))
p_tss2 <- p_tss2 + theme_bw()
p_tss2 <- p_tss2 + geom_point(size=3,shape=15)#aes(color = factor(index)))#,shape=factor(df$index)))
p_tss2 <- p_tss2 + labs(colour="Community:")
p_tss2 <- p_tss2 + scale_colour_manual(values = c("#F8766D", "#00BFC4"),
                                       labels=c('P2P','Self-\nconsumption'))
p_tss2 <- p_tss2 + scale_y_continuous(limits=c(0,100),expand = c(0,0))
p_tss2 <- p_tss2 + scale_x_continuous(limits=c(0,100),expand = c(0,0))
p_tss2 <- p_tss2 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_tss2 <- p_tss2 + coord_cartesian(xlim=c(0,100))
p_tss2 <- p_tss2 + xlab('SC [%]')
p_tss2 <- p_tss2 + ylab('Autarky\n[%]')
p_tss2 <- p_tss2 + geom_abline(aes(intercept = 0,slope=1))
p_tss2 <- p_tss2 + scale_fill_manual(values = c("#00AFBB", "#E7
                                                B800", "#FC4E07")) 
p_tss2 <- p_tss2 + theme(legend.position="top",legend.text=element_text(size=12),
                         legend.title = element_text(size=15,face="bold"))
p_tss2 <- p_tss2 + labs(subtitle = "D)",face="bold",position='top')
p_tss2 <- p_tss2 + geom_text(aes(x=10, label="Net producer", y=90), colour="black")
p_tss2 <- p_tss2 + geom_text(aes(x=90, label="Net consumer", y=10), colour="black")

p_tss2 <- p_tss2 + theme(plot.subtitle = element_text(hjust = hjust1,vjust=vjust1,size=16,face="bold"))
p_tss2 <- p_tss2 + theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=14))

a<-tapply(subset(df,df$Comm=='P2P')$bill_hh, subset(df,df$Comm=='P2P')$index, median)
b<-tapply(subset(df,df$Comm=='SC')$bill_hh, subset(df,df$Comm=='SC')$index, median)
a-b
(a-b)/b
a<-tapply(subset(df,df$Comm=='P2P')$bill, subset(df,df$Comm=='P2P')$index, median)
b<-tapply(subset(df,df$Comm=='SC')$bill, subset(df,df$Comm=='SC')$index, median)
b
a-b
############# FIG 1C Bill hh#########################
p_bill <- ggboxplot(df, x= "Comm",y="bill_hh",fill="Comm")+facet_grid(~index,
                                                                      labeller=as_labeller(c("No"="Consumer","PV"="Prosumer with PV",
                                                                                             "PV_batt"="Prosumer with PV and battery")))

p_bill <- p_bill + stat_compare_means(label.y=800)

p_bill <- p_bill + scale_x_discrete(labels=c('P2P','Self-\nconsumption'))

p_bill <- p_bill + scale_colour_manual(values = alpha(c("#F8766D", "#00BFC4"),.3),
                                       labels=c('P2P','Self-consumption'))
p_bill <- p_bill + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")

p_bill <- p_bill + ylab('Bill\n[€ p.a.]')
p_bill <- p_bill + xlab('Type of member')
p_bill <- p_bill + theme_bw()
p_bill <- p_bill + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))

p_bill <- p_bill + theme(legend.position="bottom",legend.text=element_text(size=12),
                         legend.title = element_text(size=15,face="bold"))
p_bill <- p_bill + labs(subtitle = "A)",face="bold")
p_bill <- p_bill + theme(plot.subtitle = element_text(hjust = hjust1,vjust=vjust1,size=16,face="bold"))
p_bill <- p_bill + theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=14,face="bold"))
p_bill <- p_bill + guides(colour=FALSE, fill=FALSE)
#p_bill

############# FIG 1D Bill Community#########################


p_bill2 <- ggboxplot(df, x= "Comm",y= "bill" ,fill="Comm")
p_bill2 <- p_bill2 + stat_compare_means(label.y=40000,label.x=1.35)
#p_bill2 <- p_bill2 + geom_boxplot()
p_bill2 <- p_bill2 + theme_bw()
p_bill2 <- p_bill2 + scale_x_discrete(labels=c("P2P","Self-consumption"))
p_bill2 <- p_bill2 + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")
p_bill2 <- p_bill2 + ylab('Bill\n[€ p.a.]')
p_bill2 <- p_bill2 + xlab('Type of community')
#p_bill2 <- p_bill2 + scale_y_continuous(labels = addUnits,limits = c(35000,95000))
p_bill2 <- p_bill2 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))

p_bill2 <- p_bill2 + theme(legend.position="bottom",legend.text=element_text(size=12),
                           legend.title = element_text(size=15,face="bold"))
p_bill2 <- p_bill2 + labs(subtitle = "C)",face="bold")
p_bill2 <- p_bill2+ theme(plot.subtitle = element_text(hjust = hjust1,vjust=vjust1,size=16,face="bold"))
                           
p_bill2 <- p_bill2 + theme(axis.text=element_text(size=14),
                           axis.title=element_text(size=14,face="bold"))
p_bill2 <- p_bill2 + guides(colour=FALSE, fill=FALSE)
#p_bill2

sub_title1 <- paste("Household level")
sub_title2 <- paste("Community level")
#top=textGrob("Title", gp=gpar(fontsize=15,font=8)))
a<-grid.arrange(p_bill,p_tss,ncol=2,top=textGrob(expression(bold("Household level")), gp=gpar(fontsize=18)))
b<-grid.arrange(p_bill2,p_tss2,ncol=2,top=textGrob(expression(bold("Community level")), gp=gpar(fontsize=18)))


grid.arrange(a,b, nrow=2)


ggsave('../Img/P3.pdf',plot=grid.arrange(a,b, nrow=2),width=15, height=8.5,
       encoding = "ISOLatin9.enc")


a<-tapply(subset(df,df$Comm=='P2P')$Demand_peak, subset(df,df$Comm=='P2P')$index, median)
b<-tapply(subset(df,df$Comm=='SC')$Demand_peak, subset(df,df$Comm=='SC')$index, median)
(a-b)/b
a<-tapply(subset(df,df$Comm=='P2P')$Inj_peak, subset(df,df$Comm=='P2P')$index, median)
b<-tapply(subset(df,df$Comm=='SC')$Inj_peak, subset(df,df$Comm=='SC')$index, median)
(a-b)/b


############# FIG 2C Peak-to-Peak seasonal effect#########################

df2<-read.table('final_peak.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
df3<-read.table('final_week.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
head(df2)

p_peak <- ggboxplot(df2, x = "Comm", y = "Power",fill= "Comm")+ facet_grid(~season)
#p_peak <- p_peak + geom_boxplot(aes(x=factor(Comm),y=Power,fill=factor(Comm)))+facet_grid(~season)
p_peak <- p_peak + stat_compare_means(label.y=0,size=2)
p_peak <- p_peak + theme_bw()
p_peak <- p_peak + scale_x_discrete(labels=c("P2P","Self-\nconsumption"))
p_peak <- p_peak + labs(fill="Community:")
p_peak <- p_peak + ylab('Peak-to-peak\n difference [kW]')
p_peak <- p_peak + xlab('Type of community')
p_peak <- p_peak + ylim(0,NA)
p_peak <- p_peak + labs(subtitle = "B) Peak-to-peak seasonal effect",face="bold")
p_peak <- p_peak + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))

p_peak <- p_peak + theme(legend.position="right",legend.text=element_text(size=14),
                         legend.title = element_text(size=16,face="bold"))
p_peak <- p_peak + theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=14,face="bold"))
p_peak <- p_peak + theme(plot.margin = unit(c(0.5,0.5,0,0), "cm"))

############# FIG 2D Average grid exchange 1 week #########################
df4 <- subset(df3,(df3$seed==167))
df5<-rbind(df4[c(144:168),],df4[c(1:144),])
min(df5$sc)-max(df5$sc)

rownames(df5) <- NULL
df5$Y<-as.numeric(rownames(df5))-1
library(scales)
cols = hue_pal()(12)
#show_col(hue_pal()(12))

al=1
p_peak2 <- ggplot(df5)
p_peak2 <- p_peak2 + theme_bw()
p_peak2 <- p_peak2 + geom_line(aes(x=Y,y=sc,colour="#F8766D"))
p_peak2 <- p_peak2 + geom_line(aes(x=Y,y=p2p,colour="#00BFC4"))
p_peak2 <- p_peak2 + scale_colour_manual(values = c("#F8766D", "#00BFC4"),
                                         labels=c("P2P","Self-consumption" ))
#Expand is the magic word!
p_peak2 <- p_peak2 + scale_x_continuous(breaks=seq(0,168,24),limits=c(0,172),expand = c(0, 0))
p_peak2 <- p_peak2 + scale_y_continuous(limits=c(-125,70),expand = c(0, 0))


p_peak2 <- p_peak2 + labs(colour="Community:")
p_peak2 <- p_peak2 + ylab('Power [kW]')
p_peak2 <- p_peak2 + xlab('Time [hours]')
p_peak2 <- p_peak2 + coord_cartesian(xlim=c(0,172))
p_peak2 <- p_peak2 + labs(subtitle = "A) Average grid exchange in one week",face="bold")
p_peak2 <- p_peak2 + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_peak2 <- p_peak2 + theme(legend.position="bottom",legend.text=element_text(size=14),
                           legend.title = element_text(size=16,face="bold"))
p_peak2 <- p_peak2 + theme(axis.text=element_text(size=14),
                           axis.title=element_text(size=14,face="bold"))
p_peak2 <- p_peak2 + theme(plot.margin = unit(c(0.5,0.5,0,0), "cm"))

p_peak2 <- p_peak2 + annotate('segment',x=62, xend=62,
                              y = -120, yend = 57.5,colour="blue",alpha=0.5,size=1)
p_peak2 <- p_peak2 + annotate('rect',xmin=168, xmax=172,
                              ymin = 0, ymax = Inf,fill=cols[5],alpha=0.5)
p_peak2 <- p_peak2 + annotate('rect',xmin=168, xmax=172,
                              ymin = -Inf, ymax = 0,fill=cols[10])#,alpha=0.5)
p_peak2 <- p_peak2 + geom_hline(yintercept = 0)
p_peak2 <- p_peak2 + geom_text(aes(x=169.5, label="Import", y=35), colour="black",alpha=0.8,
                               angle=90)
p_peak2 <- p_peak2 + geom_text(aes(x=169.5, label="Export", y=-65), colour="black",alpha=0.8,
                               angle=90)
p_peak2 <- p_peak2 + geom_text(aes(x=63.5, label="peak-to-peak\namplitude difference", y=0), colour="black",
                               angle=90)
#p_peak2

############# FIG 2A Community Import peak#########################

p_peak3 <- ggboxplot(df, x= "Comm",y="Demand_peak",fill="Comm")

p_peak3 <- p_peak3 + stat_compare_means(label.x=1.3,label.y=100)

p_peak3 <- p_peak3 + scale_x_discrete(labels=c('P2P','Self-\nconsumption'))

p_peak3 <- p_peak3 + scale_colour_manual(values = alpha(c("#F8766D", "#00BFC4"),.3),
                                         labels=c('P2P','Self-consumption'))
p_peak3 <- p_peak3 + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")

p_peak3 <- p_peak3 + ylab('Power\n[kW]')
p_peak3 <- p_peak3 + xlab('Type of community')
p_peak3 <- p_peak3 + theme_bw()
p_peak3 <- p_peak3 + scale_y_continuous(limits=c(100,300))
p_peak3 <- p_peak3 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))

p_peak3 <- p_peak3 + theme(legend.position="bottom",legend.text=element_text(size=14),
                           legend.title = element_text(size=16,face="bold"))
p_peak3 <- p_peak3 + labs(subtitle = "D) Maximum import peak",face="bold")
p_peak3 <- p_peak3 + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_peak3 <- p_peak3 + theme(axis.text=element_text(size=14),
                           axis.title=element_text(size=14,face="bold"))
p_peak3 <- p_peak3 + guides(colour=FALSE, fill=FALSE)
#p_peak3
############# FIG 2B Community export peak#########################

p_peak4 <- ggboxplot(df, x= "Comm",y="Inj_peak",fill="Comm")
p_peak4 <- p_peak4 + stat_compare_means(label.x=1.3,label.y=100)

p_peak4 <- p_peak4 + scale_x_discrete(labels=c('P2P','Self-\nconsumption'))

p_peak4 <- p_peak4 + scale_colour_manual(values = alpha(c("#F8766D", "#00BFC4"),.3),
                                         labels=c('P2P','Self-consumption'))
p_peak4 <- p_peak4 + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")

p_peak4 <- p_peak4 + ylab('Power\n[kW]')
p_peak4 <- p_peak4 + xlab('Type of community')
p_peak4 <- p_peak4 + theme_bw()
p_peak4 <- p_peak4 + scale_y_continuous(limits=c(100,300))
p_peak4 <- p_peak4 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))

p_peak4 <- p_peak4 + theme(legend.position="bottom",legend.text=element_text(size=14),
                           legend.title = element_text(size=16,face="bold"))
p_peak4 <- p_peak4 + labs(subtitle = "C) Maximum export peak",face="bold")
p_peak4 <- p_peak4 + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_peak4 <- p_peak4 + theme(axis.text=element_text(size=14),
                           axis.title=element_text(size=14,face="bold"))
p_peak4 <- p_peak4 + guides(colour=FALSE, fill=FALSE)
#p_peak4
lay <- rbind(c(1,1,1,1),c(2,2,3,4))
grid.arrange(p_peak2, p_peak,p_peak4,p_peak3, layout_matrix = lay)
ggsave('../Img/Power.pdf',plot=grid.arrange(p_peak2, p_peak,p_peak4,p_peak3, layout_matrix = lay),
       width=15, height=8.5,
       encoding = "ISOLatin9.enc")



###############################FIG 3A trading patterns hh######################################

h<-read.table('hist_selling.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
h$group<-ifelse(h$X13.0<mean(h$X13.0)-sd(h$X13.0),'Rarely',ifelse(h$X13.0>mean(h$X13.0)+sd(h$X13.0),'Intensively','Moderately'))
h$group<-as.factor(ordered(h$group, levels =  c("Rarely", "Moderately", "Intensively")))

h_hist <- ggplot(data=h, aes(X13.0,fill=group))
h_hist <- h_hist + geom_histogram(color="black", 
                                  binwidth=1) 
h_hist <- h_hist + theme_bw()
h_hist <- h_hist + scale_fill_discrete(name =  'Dark2')
h_hist <- h_hist + xlab('Amount of trading')
h_hist <- h_hist + ylab('Frequency')
h_hist <- h_hist + theme(legend.position="bottom",legend.text=element_text(size=14),
                         legend.title = element_text(size=16,face="bold"))
h_hist <- h_hist + labs(subtitle = "A) Trading decisions",
                        face="bold")
h_hist <- h_hist + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
h_hist <- h_hist + theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=14,face="bold"))
h_hist <- h_hist + guides(fill=guide_legend(title="Trading:"))
h_hist <- h_hist + scale_x_continuous(breaks=c(7.5,12,16.5), labels=c('\u03BC-\u03C3','\u03BC','\u03BC+\u03C3'))

shapiro.test(h$X13.0)
###############################FIG 3B trading patterns hh######################################

d<-read.table('optimal_trading_individual.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
colnames(df)



d<-subset(d,(d$Comm=='P2P')&(d$type=='PV_batt'))
d$trading<-ordered(d$trading, levels =  c("Rarely", "Moderate", "Intensive"))

d.m <- melt(select(d,-X), id.var = c("trading","type", "Comm"))
head(d.m)
unique(d.m$variable)

unique(d.m$type)
#dummy1 <- data.frame("variable" = c("SCR_hh","SSR_hh","bill_hh"), Z = c(scr_hh_sc,ssr_hh_sc, bill_hh_sc))
tapply(subset(d.m,d.m$variable=='SCR')$value, subset(d.m,d.m$variable=='SCR')$trading, summary)
tapply(subset(d.m,d.m$variable=='SSR')$value, subset(d.m,d.m$variable=='SSR')$trading, summary)
tapply(subset(d.m,d.m$variable=='bill')$value, subset(d.m,d.m$variable=='bill')$trading, summary)

median(subset(df,(df$Comm=='SC')&(df$index=='PV_batt'))$bill_hh)
tapply(subset(df,df$Comm=='SC')$SCR_hh, subset(df,df$Comm=='SC')$index, median)
tapply(subset(df,df$Comm=='SC')$SSR_hh, subset(df,df$Comm=='SC')$index, median)

subset(d.m,d.m$variable=='bill')

dummy2 <- data.frame(trading = c("Rarely","Moderate","Intensive"), 
                     Z = c(median(subset(d.m,(d.m$variable=='bill')&(d.m$trading=='Rarely'))$value),
                           median(subset(d.m,(d.m$variable=='bill')&(d.m$trading=='Moderate'))$value),
                           median(subset(d.m,(d.m$variable=='bill')&(d.m$trading=='Intensive'))$value)))

dummy3 <- data.frame(trading = c("Rarely","Moderate","Intensive"), 
                     Z = c(mean(subset(d.m,(d.m$variable=='bill')&(d.m$trading=='Rarely'))$value),
                           mean(subset(d.m,(d.m$variable=='bill')&(d.m$trading=='Moderate'))$value),
                           mean(subset(d.m,(d.m$variable=='bill')&(d.m$trading=='Intensive'))$value)))


b_hist <- ggplot(data=subset(d.m,d.m$variable=='bill'), aes(value))
b_hist <- b_hist + geom_histogram() 
b_hist <- b_hist + geom_vline(data=dummy2,aes(xintercept = Z),
                              colour="red",linetype="longdash")

b_hist <- b_hist + geom_vline(data=dummy3,aes(xintercept = Z),
                              colour="blue",linetype="longdash")
b_hist <- b_hist + facet_wrap(~trading, scales="free_y",ncol=1)

                              #xintercept=median(subset(d.m,d.m$variable=='SCR')$value),colour='red')
b_hist <- b_hist + theme_bw()
b_hist <- b_hist +  xlab('Annual bill [€]')


b_hist

tapply(subset(df,df$Comm=='SC')$bill_hh, subset(df,df$Comm=='SC')$index, summary)
colnames(df)
dummy2 <- data.frame(variable = c("SCR","SSR","bill"), 
                     Z = c(median(subset(df,(df$Comm=='SC')&(df$index=='PV_batt'))$SCR_hh),
                           median(subset(df,(df$Comm=='SC')&(df$index=='PV_batt'))$SSR_hh),
                           median(subset(df,(df$Comm=='SC')&(df$index=='PV_batt'))$bill_hh)))

p_hh <- ggboxplot(d.m, x= "trading",y="value",fill="trading")

p_hh <- p_hh + facet_wrap(~variable, scales="free_y",ncol=1,strip.position = "left", 
                          labeller=as_labeller(c("SCR"="SC [%]","SSR"="Autarky [%]","bill"="Bill [\u20AC]")))
#p_hh <- p_hh + stat_compare_means(label.x.npc=0.4,label.y.npc = 0.05)#label.x= -Inf, label.y = Inf, hjust = , vjust = 1)
p_hh <- p_hh + geom_hline(data=dummy2,aes(yintercept = Z),
                          colour="red",linetype="longdash")
p_hh <- p_hh + scale_fill_discrete(name =  'Dark2')
p_hh <- p_hh + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")

p_hh <- p_hh + theme_bw()
p_hh <- p_hh + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_hh <- p_hh + xlab('Trading in P2P community')
p_hh <- p_hh + theme(legend.position="bottom",legend.text=element_text(size=14),
                     legend.title = element_text(size=16,face="bold"))
p_hh <- p_hh + labs(subtitle = "B) Households with PV and battery",face="bold")
p_hh <- p_hh + theme(panel.spacing = unit(0.8, "lines"),plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_hh <- p_hh + theme(axis.text=element_text(size=14),
                     axis.title=element_text(size=14,face="bold"))
p_hh <- p_hh + guides(colour=FALSE, fill=FALSE)
#p_hh

###########################FIG 3B  trading patterns community###################################
d2<-read.table('optimal_trading_community.csv',sep=',',header=TRUE,stringsAsFactors = TRUE)
d2$trading<-ordered(d2$trading, levels =  c("Rarely", "Moderate", "Intensive"))
d2.m <- melt(select(d2,-X), id.var = c("trading","Comm"))
unique(d2.m$variable)
tapply(subset(d2.m,d2.m$variable=='SCR_comm')$value, subset(d2.m,d2.m$variable=='SCR_comm')$trading, mean)
tapply(subset(d2.m,d2.m$variable=='SSR_comm')$value, subset(d2.m,d2.m$variable=='SSR_comm')$trading, mean)
tapply(subset(d2.m,d2.m$variable=='Bill_comm')$value, subset(d2.m,d2.m$variable=='Bill_comm')$trading, mean)

dummy2 <- data.frame(variable = c("SCR_comm","SSR_comm","Bill_comm"), 
                     Z = c(median(subset(df,(df$Comm=='SC'))$SCR),
                           median(subset(df,(df$Comm=='SC'))$SSR),
                           median(subset(df,(df$Comm=='SC'))$bill)))

p_comm <- ggboxplot(d2.m, x= "trading",y="value",fill="trading")
p_comm <- p_comm + geom_hline(data=dummy2, aes(yintercept = Z),colour="red",linetype="longdash")
p_comm <- p_comm + facet_wrap(~variable, scales="free_y", ncol= 1,strip.position = "left", 
                              labeller=as_labeller(c("SCR_comm"="SC [%]","SSR_comm"="Autarky [%]","Bill_comm"="Bill [k\u20AC]")))
#p_comm <- p_comm + stat_compare_means(label.x.npc=0.4,label.y.npc = 0.05)
p_comm <- p_comm + scale_fill_discrete(name =  'Dark2')
p_comm <- p_comm + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")
p_comm <- p_comm + theme_bw()
p_comm <- p_comm + scale_y_continuous(labels = addUnits)
p_comm <- p_comm + xlab('Trading in the P2P community')

p_comm <- p_comm + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))

p_comm <- p_comm + theme(legend.position="bottom",legend.text=element_text(size=14),
                         legend.title = element_text(size=16,face="bold"))
p_comm <- p_comm + labs(subtitle = "C) Community level",face="bold")
p_comm <- p_comm + theme(panel.spacing = unit(0.8, "lines"),plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_comm <- p_comm + theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=14,face="bold"))
p_comm <- p_comm + guides(colour=FALSE, fill=FALSE)

#p_comm
#grid.arrange(p_hh,p_comm,nrow=1, ncol=2)
lay2 <- rbind(c(1,1,1,1),
              c(2,2,3,3),
              c(2,2,3,3))

grid.arrange(h_hist,p_hh,p_comm, layout_matrix = lay2)
(174-154)/174
ggsave('../Img/P2P_strategy_abs.pdf',grid.arrange(h_hist,p_hh,p_comm, layout_matrix = lay2),width=15, height=8.5,device=cairo_pdf)


#encoding = "ISOLatin9.enc")
#################Survey#####################
p_sur <- ggplot(d, aes(x=Price/100,y=Sell*100, group=sp,fill=factor(sp)))
p_sur <- p_sur + facet_grid(~SOC,labeller = as_labeller(c("30"= "SOC\n30%","60"= "SOC\n60%",
                                                          "90"= "SOC\n90%")))
p_sur <- p_sur + geom_bar(stat="identity",position="dodge")
p_sur <- p_sur + theme_bw()
p_sur <- p_sur + scale_x_continuous(breaks=seq(0.04,0.28,0.08),limits=c(0,0.32))
p_sur <- p_sur + scale_y_continuous(limits=c(0,100))
p_sur <- p_sur + labs(fill="Surplus time:")
p_sur <- p_sur + scale_fill_manual(values = c("#F8766D", "#00BFC4"),
                                   labels=c('Within 12h','In more\nthan 12h'))
p_sur <- p_sur + ylab('Decision to sell\n[%]')
p_sur <- p_sur + xlab('Price[€/kWh]')

p_sur <- p_sur + theme(legend.text=element_text(size=14),
                       legend.title = element_text(size=16,face="bold"))
p_sur <- p_sur + theme(axis.text=element_text(size=14),
                       axis.title=element_text(size=14,face="bold"))
ggsave('../Img/Selling.pdf',plot=p_sur,width=15, height=8.5,
       encoding = "ISOLatin9.enc")
################P2P vs SC with P2P #############

d<-read.table('sc_vs_p2p.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)


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


################Sensitivity##############################

df<-read.table('sensitivity.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)

df_mean<-read.table('sensitivity_means.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
head(df)
str(df_mean)
colnames(df_mean)
mean(df_mean$SCR)
p_scr_ind <- ggplot(subset(df_mean,df_mean$type!='No'), aes(x=PV_penetration_x,y=SCR, group=size_comm,fill=factor(size_comm)))
p_scr_ind <- p_scr_ind + geom_bar(stat="identity",position="dodge")
p_scr_ind <- p_scr_ind + facet_grid(type~Batt_penetration_x,
                                    labeller = as_labeller(c("25"= "Battery penetration\n25%",
                                                             "50"= "Battery penetration\n50%",
                                                             "75"= "Battery penetration\n 75%",
                                                             "PV"= "PV only",
                                                             "PV_batt"= "PV and\nbattery")))
p_scr_ind <- p_scr_ind + theme_bw()
p_scr_ind <- p_scr_ind + scale_y_continuous(limits=c(0,100))
p_scr_ind <- p_scr_ind + labs(fill="Community size:")
p_scr_ind <- p_scr_ind + ylab('SCR\n[%]')
p_scr_ind <- p_scr_ind + xlab('PV penetration [%]')

p_scr_ind <- p_scr_ind + theme(legend.text=element_text(size=14),
                               legend.title = element_text(size=16,face="bold"))
p_scr_ind <- p_scr_ind + theme(axis.text=element_text(size=14),
                               axis.title=element_text(size=14,face="bold"))

p_scr_com <- ggplot(df_mean, aes(x=PV_penetration_x,y=SCR_comm, group=size_comm,fill=factor(size_comm)))
p_scr_com <- p_scr_com + geom_bar(stat="identity",position="dodge")
p_scr_com <- p_scr_com + facet_grid(~Batt_penetration_x,
                                    labeller = as_labeller(c("25"= "Battery penetration\n25%",
                                                             "50"= "Battery penetration\n50%",
                                                             "75"= "Battery penetration\n 75%")))
p_scr_com <- p_scr_com + theme_bw()
p_scr_com <- p_scr_com + scale_y_continuous(limits=c(0,100))
p_scr_com <- p_scr_com + labs(fill="Community size:")
p_scr_com <- p_scr_com + ylab('Community SCR\n[%]')
p_scr_com <- p_scr_com + xlab('PV penetration [%]')

p_scr_com <- p_scr_com + theme(legend.text=element_text(size=14),
                               legend.title = element_text(size=16,face="bold"))
p_scr_com <- p_scr_com + theme(axis.text=element_text(size=14),
                               axis.title=element_text(size=14,face="bold"))


p_ss_ind <- ggplot(subset(df_mean,df_mean$type!='No'), aes(x=PV_penetration_x,y=SSR, group=size_comm,fill=factor(size_comm)))
p_ss_ind <- p_ss_ind + geom_bar(stat="identity",position="dodge")
p_ss_ind <- p_ss_ind + facet_grid(type~Batt_penetration_x,
                                  labeller = as_labeller(c("25"= "Battery penetration\n25%",
                                                           "50"= "Battery penetration\n50%",
                                                           "75"= "Battery penetration\n 75%",
                                                           "PV"= "PV only",
                                                           "PV_batt"= "PV and\nbattery")))
p_ss_ind <- p_ss_ind + theme_bw()
p_ss_ind <- p_ss_ind + scale_y_continuous(limits=c(0,100))
p_ss_ind <- p_ss_ind + labs(fill="Community size:")
p_ss_ind <- p_ss_ind + ylab('Autarky\n[%]')
p_ss_ind <- p_ss_ind + xlab('PV penetration [%]')

p_ss_ind <- p_ss_ind + theme(legend.text=element_text(size=14),
                             legend.title = element_text(size=16,face="bold"))
p_ss_ind <- p_ss_ind + theme(axis.text=element_text(size=14),
                             axis.title=element_text(size=14,face="bold"))

p_ss_com <- ggplot(df_mean, aes(x=PV_penetration_x,y=SSR_comm, group=size_comm,fill=factor(size_comm)))
p_ss_com <- p_ss_com + geom_bar(stat="identity",position="dodge")
p_ss_com <- p_ss_com + facet_grid(~Batt_penetration_x,
                                  labeller = as_labeller(c("25"= "Battery penetration\n25%",
                                                           "50"= "Battery penetration\n50%",
                                                           "75"= "Battery penetration\n 75%")))
p_ss_com <- p_ss_com + theme_bw()
p_ss_com <- p_ss_com + scale_y_continuous(limits=c(0,100))
p_ss_com <- p_ss_com + labs(fill="Community size:")
p_ss_com <- p_ss_com + ylab('Community Autarky\n[%]')
p_ss_com <- p_ss_com + xlab('PV penetration [%]')

p_ss_com <- p_ss_com + theme(legend.text=element_text(size=14),
                             legend.title = element_text(size=16,face="bold"))
p_ss_com <- p_ss_com + theme(axis.text=element_text(size=14),
                             axis.title=element_text(size=14,face="bold"))

p_bill_ind <- ggplot(df_mean, aes(x=PV_penetration_x,y=bill, group=size_comm,fill=factor(size_comm)))
p_bill_ind <- p_bill_ind + geom_bar(stat="identity",position="dodge")
p_bill_ind <- p_bill_ind + facet_grid(type~Batt_penetration_x,
                                      labeller = as_labeller(c("25"= "Battery penetration\n25%",
                                                               "50"= "Battery penetration\n50%",
                                                               "75"= "Battery penetration\n 75%",
                                                               "No"= "No PV\n nor bat.",
                                                               "PV"= "PV only",
                                                               "PV_batt"= "PV and\nbattery")))
p_bill_ind <- p_bill_ind + theme_bw()
p_bill_ind <- p_bill_ind + labs(fill="Community size:")
p_bill_ind <- p_bill_ind + ylab('Individual bill\n[€ p.a.]')
p_bill_ind <- p_bill_ind + xlab('PV penetration [%]')

p_bill_ind <- p_bill_ind + theme(legend.text=element_text(size=14),
                                 legend.title = element_text(size=16,face="bold"))
p_bill_ind <- p_bill_ind + theme(axis.text=element_text(size=14),
                                 axis.title=element_text(size=14,face="bold"))

p_bill_com <- ggplot(df_mean, aes(x=PV_penetration_x,y=Bill_comm, group=size_comm,fill=factor(size_comm)))
p_bill_com <- p_bill_com + geom_bar(stat="identity",position="dodge")
p_bill_com <- p_bill_com + facet_grid(~Batt_penetration_x,
                                      labeller = as_labeller(c("25"= "Battery penetration\n25%",
                                                               "50"= "Battery penetration\n50%",
                                                               "75"= "Battery penetration\n 75%")))

p_bill_com <- p_bill_com + theme_bw()
p_bill_com <- p_bill_com + labs(fill="Community size:")
p_bill_com <- p_bill_com + ylab('Community bill\n[€ p.a.]')
p_bill_com <- p_bill_com + xlab('PV penetration [%]')

p_bill_com <- p_bill_com + theme(legend.text=element_text(size=14),
                                 legend.title = element_text(size=16,face="bold"))
p_bill_com <- p_bill_com + theme(axis.text=element_text(size=14),
                                 axis.title=element_text(size=14,face="bold"))

lay <- rbind(c(1,1),
             c(2,2),
             c(3,3))
grid.arrange(p_bill_ind,p_scr_ind,p_ss_ind,layout_matrix = lay)
grid.arrange(p_bill_com,p_scr_com,p_ss_com,nrow=3, ncol=1)

ggsave('../Img/sensitivity_ind.pdf',plot=grid.arrange(p_bill_ind,p_scr_ind,p_ss_ind,nrow=3, ncol=1),width=15, height=8.5,
       encoding = "ISOLatin9.enc")

ggsave('../Img/sensitivity_com.pdf',plot=grid.arrange(p_bill_com,p_scr_com,p_ss_com,nrow=3, ncol=1),width=15, height=8.5,
       encoding = "ISOLatin9.enc")
#######################tests###################
df<-read.table('final_dataset.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
unique(df$index)
# Fig 1 A
shapiro.test(subset(df,df$Comm=='SC' & df$index=='PV_batt')$bill_hh)
shapiro.test(subset(df,df$Comm=='P2P' & df$index=='PV_batt')$bill_hh)

wilcox.test(subset(df,df$Comm=='SC' & df$index=='PV_batt')$bill_hh,
            subset(df,df$Comm=='P2P' & df$index=='PV_batt')$bill_hh,
            paired=FALSE)

shapiro.test(subset(df,df$Comm=='SC' & df$index=='PV')$bill_hh)
shapiro.test(subset(df,df$Comm=='P2P' & df$index=='PV')$bill_hh)

wilcox.test(subset(df,df$Comm=='SC' & df$index=='PV')$bill_hh,
            subset(df,df$Comm=='P2P' & df$index=='PV')$bill_hh,
            paired=FALSE)

shapiro.test(subset(df,df$Comm=='SC' & df$index=='No')$bill_hh)
shapiro.test(subset(df,df$Comm=='P2P' & df$index=='No')$bill_hh)

wilcox.test(subset(df,df$Comm=='SC' & df$index=='No')$bill_hh,
                     subset(df,df$Comm=='P2P' & df$index=='No')$bill_hh,
                     paired=FALSE)
# Fig 1 C
shapiro.test(subset(df,df$Comm=='SC')$bill)
shapiro.test(subset(df,df$Comm=='P2P')$bill)

wilcox.test(subset(df,df$Comm=='SC')$bill,
            subset(df,df$Comm=='P2P')$bill,
            paired=FALSE)


df2<-read.table('final_peak.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
df3<-read.table('final_week.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
# Fig 2 B
shapiro.test(sample(subset(df2,df2$Comm=='SC'& df2$season=='Winter')$Power,5000))
shapiro.test(sample(subset(df2,df2$Comm=='SC'& df2$season=='Fall')$Power,5000))
shapiro.test(sample(subset(df2,df2$Comm=='SC'& df2$season=='Summer')$Power,5000))
shapiro.test(sample(subset(df2,df2$Comm=='SC'& df2$season=='Spring')$Power,5000))

shapiro.test(sample(subset(df2,df2$Comm=='P2P'& df2$season=='Winter')$Power,5000))
shapiro.test(sample(subset(df2,df2$Comm=='P2P'& df2$season=='Fall')$Power,5000))
shapiro.test(sample(subset(df2,df2$Comm=='P2P'& df2$season=='Summer')$Power,5000))
shapiro.test(sample(subset(df2,df2$Comm=='P2P'& df2$season=='Spring')$Power,5000))

wilcox.test(subset(df2,df2$Comm=='SC' & df2$season=='Fall')$Power,
            subset(df2,df2$Comm=='P2P' & df2$season=='Fall')$Power,
            paired=FALSE)

wilcox.test(subset(df2,df2$Comm=='SC' & df2$season=='Winter')$Power,
            subset(df2,df2$Comm=='P2P' & df2$season=='Winter')$Power,
            paired=FALSE)

wilcox.test(subset(df2,df2$Comm=='SC' & df2$season=='Summer')$Power,
            subset(df2,df2$Comm=='P2P' & df2$season=='Summer')$Power,
            paired=FALSE)

wilcox.test(subset(df2,df2$Comm=='SC' & df2$season=='Spring')$Power,
            subset(df2,df2$Comm=='P2P' & df2$season=='Spring')$Power,
            paired=FALSE)
# Fig 2 C
shapiro.test(subset(df,df$Comm=='SC')$Demand_peak)
shapiro.test(subset(df,df$Comm=='P2P')$Demand_peak)

wilcox.test(subset(df,df$Comm=='SC')$Demand_peak,
            subset(df,df$Comm=='P2P')$Demand_peak,
            paired=FALSE)
# Fig 2 D
shapiro.test(subset(df,df$Comm=='SC')$Inj_peak)
shapiro.test(subset(df,df$Comm=='P2P')$Inj_peak)

wilcox.test(subset(df,df$Comm=='SC')$Inj_peak,
            subset(df,df$Comm=='P2P')$Inj_peak,
            paired=FALSE)


d<-read.table('optimal_trading_individual.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)

d<-subset(d,(d$Comm=='P2P')&(d$type=='PV_batt'))
d$trading<-ordered(d$trading, levels =  c("Rarely", "Moderate", "Intensive"))
d.m <- melt(select(d,-X), id.var = c("trading","type", "Comm"))

# fig 3 B
shapiro.test(subset(d.m,d.m$variable=='SCR' & d.m$trading=='Rarely')$value)
shapiro.test(subset(d.m,d.m$variable=='SCR' & d.m$trading=='Moderate')$value)
shapiro.test(subset(d.m,d.m$variable=='SCR' & d.m$trading=='Intensive')$value)

kruskal.test(subset(d.m,d.m$variable=='SCR')$value~
               subset(d.m,d.m$variable=='SCR')$trading)


shapiro.test(subset(d.m,d.m$variable=='SSR' & d.m$trading=='Rarely')$value)
shapiro.test(subset(d.m,d.m$variable=='SSR' & d.m$trading=='Moderate')$value)
shapiro.test(subset(d.m,d.m$variable=='SSR' & d.m$trading=='Intensive')$value)

kruskal.test(subset(d.m,d.m$variable=='SSR')$value~
               subset(d.m,d.m$variable=='SSR')$trading)

shapiro.test(subset(d.m,d.m$variable=='bill' & d.m$trading=='Rarely')$value)
shapiro.test(subset(d.m,d.m$variable=='bill' & d.m$trading=='Moderate')$value)
shapiro.test(subset(d.m,d.m$variable=='bill' & d.m$trading=='Intensive')$value)

kruskal.test(subset(d.m,d.m$variable=='bill')$value~
               subset(d.m,d.m$variable=='bill')$trading)

d2<-read.table('optimal_trading_community.csv',sep=',',header=TRUE,stringsAsFactors = TRUE)
d2$trading<-ordered(d2$trading, levels =  c("Rarely", "Moderate", "Intensive"))
d2.m <- melt(select(d2,-X), id.var = c("trading","Comm"))

#fig 3 C

shapiro.test(sample(subset(d2.m,d2.m$variable=='SCR_comm' &
                             d2.m$trading=='Rarely')$value,5000))
shapiro.test(sample(subset(d2.m,d2.m$variable=='SCR_comm' &
                             d2.m$trading=='Moderate')$value,5000))
shapiro.test(sample(subset(d2.m,d2.m$variable=='SCR_comm' &
                             d2.m$trading=='Intensive')$value,5000))


kruskal.test(subset(d2.m,d2.m$variable=='SCR_comm')$value~
               subset(d2.m,d2.m$variable=='SCR_comm')$trading)


shapiro.test(sample(subset(d2.m,d2.m$variable=='SSR_comm' & 
                      d2.m$trading=='Rarely')$value,5000))
shapiro.test(sample(subset(d2.m,d2.m$variable=='SSR_comm' & 
                      d2.m$trading=='Moderate')$value,5000))
shapiro.test(sample(subset(d2.m,d2.m$variable=='SSR_comm' & 
                      d2.m$trading=='Intensive')$value,5000))

kruskal.test(subset(d2.m,d2.m$variable=='SSR_comm')$value~
               subset(d2.m,d2.m$variable=='SSR_comm')$trading)

shapiro.test(sample(subset(d2.m,d2.m$variable=='Bill_comm' & 
                      d2.m$trading=='Rarely')$value,5000))
shapiro.test(sample(subset(d2.m,d2.m$variable=='Bill_comm' & 
                      d2.m$trading=='Moderate')$value,5000))
shapiro.test(sample(subset(d2.m,d2.m$variable=='Bill_comm' & 
                      d2.m$trading=='Intensive')$value,5000))

kruskal.test(subset(d2.m,d2.m$variable=='Bill_comm')$value~
               subset(d2.m,d2.m$variable=='Bill_comm')$trading)


h<-read.table('hist_selling.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
h$group<-ifelse(h$X13.0<mean(h$X13.0)-sd(h$X13.0),'Rarely',ifelse(h$X13.0>mean(h$X13.0)+sd(h$X13.0),'Intensively','Moderately'))
h$group<-as.factor(ordered(h$group, levels =  c("Rarely", "Moderately", "Intensively")))

summary(h$X13.0)
sd(h$X13.0)

summary(subset(df,df$Comm=='P2P')$PI)
sd(subset(df,df$Comm=='P2P')$PI)
