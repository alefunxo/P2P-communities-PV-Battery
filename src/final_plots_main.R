setwd("/data/home/alejandropena/Psychology/Output_NE_1/")

###################Load#####################################

library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(reshape2)
library(dplyr)
setwd("/data/home/alejandropena/Psychology/Output_NE_1/")


scale_override <- function(which, scale) {
  if(!is.numeric(which) || (length(which) != 1) || (which %% 1 != 0)) {
    stop("which must be an integer of length 1")
  }
  
  if(is.null(scale$aesthetics) || !any(c("x", "y") %in% scale$aesthetics)) {
    stop("scale must be an x or y position scale")
  }
  
  structure(list(which = which, scale = scale), class = "scale_override")
}
CustomFacetWrap <- ggproto(
  "CustomFacetWrap", FacetWrap,
  init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
    # make the initial x, y scales list
    scales <- ggproto_parent(FacetWrap, self)$init_scales(layout, x_scale, y_scale, params)
    
    if(is.null(params$scale_overrides)) return(scales)
    
    max_scale_x <- length(scales$x)
    max_scale_y <- length(scales$y)
    
    # ... do some modification of the scales$x and scales$y here based on params$scale_overrides
    for(scale_override in params$scale_overrides) {
      which <- scale_override$which
      scale <- scale_override$scale
      
      if("x" %in% scale$aesthetics) {
        if(!is.null(scales$x)) {
          if(which < 0 || which max_scale_x) stop("Invalid index of x scale: ", which)
          scales$x[[which]] <- scale$clone()
        }
      } else if("y" %in% scale$aesthetics) {
        if(!is.null(scales$y)) {
          if(which < 0 || which max_scale_y) stop("Invalid index of y scale: ", which)
          scales$y[[which]] <- scale$clone()
        }
      } else {
        stop("Invalid scale")
      }
    }
    
    # return scales
    scales
  }
)
facet_wrap_custom <- function(..., scale_overrides = NULL) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_wrap(...)
  
  # sanitize scale overrides
  if(inherits(scale_overrides, "scale_override")) {
    scale_overrides <- list(scale_overrides)
  } else if(!is.list(scale_overrides) || 
            !all(vapply(scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
    stop("scale_overrides must be a scale_override object or a list of scale_override objects")
  }
  
  facet_super$params$scale_overrides <- scale_overrides
  
  ggproto(NULL, CustomFacetWrap,
          shrink = facet_super$shrink,
          params = facet_super$params
  )
}

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
getwd()
#read the dataframe
d<-read.table('../Input/df_core.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
df<-read.table('final_dataset.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
df_1 <- subset(df,df$index=='PV_batt')
df2<-read.table('final_peak.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
df3<-read.table('final_week.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
h<-read.table('hist_selling.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
d<-read.table('optimal_trading_individual.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
d2<-read.table('optimal_trading_community.csv',sep=',',header=TRUE,stringsAsFactors = TRUE)
############# FIG 1#########################

hjust1=-0.3
vjust1=1
hjust2=-0.2
vjust2=1
size_1=10
size_2=14
size_3=3
############# FIG 1A Bill hh#########################
p_bill <- ggboxplot(df, x= "Comm",y="bill_hh",fill="Comm",lwd=0.5,outlier.size=0.3)+facet_grid(~index,
                        labeller=as_labeller(c("No"="Consumers","PV"="Prosumers with PV",
                        "PV_batt"="Prosumers with PV\nand battery")))
p_bill <- p_bill + stat_compare_means(label.x=0.9,label.y=-500,size=size_3-1.2)
p_bill <- p_bill + scale_x_discrete(labels=c('P2P\ntrading','SC'))
p_bill <- p_bill + scale_colour_manual(values = alpha(c("#F8766D", "#00BFC4"),.3),
                                       labels=c('P2P\ntrading','\nSelf-\nconsumption\nmaximization'))
p_bill <- p_bill + labs(fill="Type of prosumer:" ,colour="Strategy:")
p_bill <- p_bill + ylab('Bill [€ p.a.]')
p_bill <- p_bill + xlab('Strategy')
p_bill <- p_bill + theme_bw()
p_bill <- p_bill + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_bill <- p_bill + theme(legend.position="bottom",legend.text=element_text(size=size_1),
                         legend.title = element_text(size=size_2,face="bold"))
p_bill <- p_bill + labs(subtitle = "a)",face="bold")
p_bill <- p_bill + theme(plot.subtitle = element_text(hjust = hjust2+0.045,vjust=vjust2,size=size_2,face="bold"))
p_bill <- p_bill  + theme(axis.text=element_text(size=size_1),
                           axis.title=element_text(size=size_1,face="bold"),
                           axis.text.x = element_text(size = size_1),
                           axis.text.y = element_text(size = size_1),
                           strip.text.x=element_text(size=size_1-4),
                           panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p_bill <- p_bill + guides(colour=FALSE, fill=FALSE)
#p_bill
  ############# FIG 1B hh SS/SC#########################
p_tss <- ggplot(df_1, aes(x=SCR_hh, y=SSR_hh,fill=factor(index),colour=factor(Comm)))
p_tss <- p_tss + geom_point(size=3)
p_tss <- p_tss + theme_bw()
p_tss <- p_tss + scale_colour_manual(values = c("#F8766D", "#00BFC4"),labels=c('P2P trading',
                                                                               'Self-consumption\n maximization (SC)'))
p_tss <- p_tss + labs(fill="Strategy:" ,colour="Strategy:")
p_tss <- p_tss + scale_y_continuous(limits=c(0,100),expand = c(0,0))
p_tss <- p_tss + scale_x_continuous(limits=c(0,100),expand = c(0,0))
p_tss <- p_tss + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
#p_tss <- p_tss + coord_cartesian(xlim=c(0,100))
p_tss <- p_tss + xlab('Self-consumption [%]')
p_tss <- p_tss + ylab('Autarky [%]')
p_tss <- p_tss + geom_abline(aes(intercept = 0,slope=1))
p_tss <- p_tss + scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) 
p_tss <- p_tss + theme(legend.position="bottom",legend.text=element_text(size=size_1-2),
                       legend.title = element_text(size=size_1-2,face="bold"))
p_tss <- p_tss + labs(subtitle = "b)",face="bold")
p_tss <- p_tss + theme(plot.subtitle = element_text(hjust = hjust2,vjust=vjust2,size=size_2,face="bold"))
p_tss <- p_tss  + theme(axis.text=element_text(size=size_1),
                        axis.title=element_text(size=size_1,face="bold"),
                        axis.text.x = element_text(size = size_1),
                        axis.text.y = element_text(size = size_1),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p_tss <- p_tss + guides(fill=FALSE)
p_tss <- p_tss + geom_text(aes(x=15, label="Net producers\n(Prosumers with\nPV and battery)", y=80), colour="black",size=size_3-1)
p_tss <- p_tss + geom_text(aes(x=80, label="Net consumers", y=10), colour="black",size=size_3-1)
############ FIG 1C Bill Community#################
p_bill2 <- ggboxplot(df, x= "Comm",y= "bill" ,fill="Comm",outlier.size=0.3)
p_bill2 <- p_bill2 + stat_compare_means(label.y=40000,label.x=1.3,size=size_3)
#p_bill2 <- p_bill2 + geom_boxplot()
p_bill2 <- p_bill2 + theme_bw()
p_bill2 <- p_bill2 + scale_x_discrete(labels=c("P2P\n trading","SC"))
p_bill2 <- p_bill2 + ylab('Bill [€ p.a.]')
p_bill2 <- p_bill2 + xlab('Strategy')
#p_bill2 <- p_bill2 + scale_y_continuous(labels = addUnits,limits = c(35000,95000))
p_bill2 <- p_bill2 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_bill2 <- p_bill2 + theme(legend.position = "none")
p_bill2 <- p_bill2 + labs(subtitle = "c)",face="bold")
p_bill2 <- p_bill2+ theme(plot.subtitle = element_text(hjust = hjust2,vjust=vjust2,size=size_2,face="bold"))
p_bill2 <- p_bill2 + theme(axis.text=element_text(size=size_1),
                           axis.title=element_text(size=size_1,face="bold"),
                           axis.text.x = element_text(size = size_1),
                           axis.text.y = element_text(size = size_1),
                           panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p_bill2 <- p_bill2 + guides(colour=FALSE, fill=FALSE)
#p_bill2
############# FIG 1D Community SS/SC#########################
p_tss2 <- ggplot(df, aes(x=SCR, y=SSR,colour=factor(Comm)))#,colour=factor(Comm)))
p_tss2 <- p_tss2 + theme_bw()
p_tss2 <- p_tss2 + geom_point(size=3)#aes(color = factor(index)))#,shape=factor(df$index)))
p_tss2 <- p_tss2 + labs(colour="Strategy:")
p_tss2 <- p_tss2 + scale_colour_manual(values = c("#F8766D", "#00BFC4"),
                                       labels=c('P2P\n trading','Self-consumption\n maximization'))
p_tss2 <- p_tss2 + scale_y_continuous(limits=c(0,100),expand = c(0,0))
p_tss2 <- p_tss2 + scale_x_continuous(limits=c(0,100),expand = c(0,0))
p_tss2 <- p_tss2 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_tss2 <- p_tss2 + coord_cartesian(xlim=c(0,100))
p_tss2 <- p_tss2 + xlab('Self-consumption [%]')
p_tss2 <- p_tss2 + ylab('Autarky [%]')
p_tss2 <- p_tss2 + geom_abline(aes(intercept = 0,slope=1))
p_tss2 <- p_tss2 + scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) 
p_tss2 <- p_tss2 + theme(legend.position = "none")
p_tss2 <- p_tss2 + labs(subtitle = "d)",face="bold",position='top')
p_tss2 <- p_tss2 + geom_text(aes(x=15, label="Net producers\n(All households)", y=90), colour="black",size=size_3-1)
p_tss2 <- p_tss2 + geom_text(aes(x=80, label="Net consumers", y=10), colour="black",size=size_3-1)
p_tss2 <- p_tss2 + theme(plot.subtitle = element_text(hjust = hjust2,vjust=vjust2,size=size_2,face="bold"))
p_tss2 <- p_tss2  + theme(axis.text=element_text(size=size_1),
                          axis.title=element_text(size=size_1,face="bold"),
                          axis.text.x = element_text(size = size_1),
                          axis.text.y = element_text(size = size_1),
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())

sub_title1 <- paste("Individual level")
sub_title2 <- paste("Aggregated level")
#top=textGrob("Title", gp=gpar(fontsize=15,font=8)))
a<-grid.arrange(p_bill,p_tss,ncol=2,top=textGrob(expression(bold("Individual level")), gp=gpar(fontsize=size_2)))
b<-grid.arrange(p_bill2,p_tss2,ncol=2,top=textGrob(expression(bold("Aggregated level")), gp=gpar(fontsize=size_2)))
grid.arrange(a,b, nrow=2)
############Fig 1 save################
ggsave('../Img/Figure1.pdf',plot=grid.arrange(a,b, nrow=2),width = 180, height = 185, units = "mm",
       encoding = "ISOLatin9.enc")

############# FIG 2 #########################
df4 <- subset(df3,(df3$seed==167))
df5<-rbind(df4[c(144:168),],df4[c(1:144),])
rownames(df5) <- NULL
df5$Y<-as.numeric(rownames(df5))-1
library(scales)
cols = hue_pal()(12)
al=1
############# FIG 2A Average grid exchange 1 week #########################

p_peak2 <- ggplot(df5)
p_peak2 <- p_peak2 + theme_bw()
p_peak2 <- p_peak2 + geom_line(aes(x=Y,y=sc,colour="#F8766D"))
p_peak2 <- p_peak2 + geom_line(aes(x=Y,y=p2p,colour="#00BFC4"))
p_peak2 <- p_peak2 + scale_colour_manual(values = c("#F8766D", "#00BFC4"),
                                         labels=c("P2P trading","Self-consumption\nmaximization (SC)" ))
#Expand is the magic word!
p_peak2 <- p_peak2 + scale_x_continuous(breaks=seq(0,168,12),limits=c(0,172),expand = c(0, 0))
p_peak2 <- p_peak2 + scale_y_continuous(limits=c(-125,70),expand = c(0, 0))
p_peak2 <- p_peak2 + labs(colour="Strategy:")
p_peak2 <- p_peak2 + ylab('Power [kW]')
p_peak2 <- p_peak2 + xlab('Time [hours]')
p_peak2 <- p_peak2 + coord_cartesian(xlim=c(0,172))
p_peak2 <- p_peak2 + labs(subtitle = "a)",face="bold")
p_peak2 <- p_peak2 + theme(plot.subtitle = element_text(hjust = -0.085,vjust=0,size=size_2,face="bold"))
p_peak2 <- p_peak2 + theme(legend.position="bottom",legend.text=element_text(size=size_1),
                           legend.title = element_text(size=size_2,face="bold"))
p_peak2 <- p_peak2  + theme(axis.text=element_text(size=size_1),
                            axis.title=element_text(size=size_1,face="bold"),
                            axis.text.x = element_text(size = size_1),
                            axis.text.y = element_text(size = size_1),
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#p_peak2 <- p_peak2 + theme(plot.margin = unit(c(0.5,0.5,0,0), "cm"))
p_peak2 <- p_peak2 + annotate('segment',x=62, xend=62,arrow=arrow(length=unit(0.2,"cm"),ends = "both"),
                              y = -120, yend = 57.5,colour="blue",alpha=0.5,size=1)
p_peak2 <- p_peak2 + annotate('rect',xmin=168, xmax=172,
                              ymin = 0, ymax = Inf,fill=cols[5],alpha=0.5)
p_peak2 <- p_peak2 + annotate('rect',xmin=168, xmax=172,
                              ymin = -Inf, ymax = 0,fill=cols[10])#,alpha=0.5)
p_peak2 <- p_peak2 + geom_hline(yintercept = 0)

p_peak2 <- p_peak2 + geom_text(aes(x=169.5, label="Import", y=35), colour="black",alpha=0.8,
                               angle=-90,size=size_3)
p_peak2 <- p_peak2 + geom_text(aes(x=169.5, label="Export", y=-65), colour="black",alpha=0.8,
                               angle=-90,size=size_3)
p_peak2 <- p_peak2 + geom_text(aes(x=62, label="peak-to-peak\namplitude difference", y=0), colour="black",
                               angle=90,size=size_3)
#p_peak2
############# FIG 2B Peak-to-Peak seasonal effect#########################

p_peak <- ggboxplot(df2, x = "Comm", y = "Power",fill= "Comm",outlier.size=0.3)+ facet_grid(~season)
#p_peak <- p_peak + geom_boxplot(aes(x=factor(Comm),y=Power,fill=factor(Comm)))+facet_grid(~season)
p_peak <- p_peak + stat_compare_means(label.y=0,label.x=0.9,size=size_3-1.5)
p_peak <- p_peak + theme_bw()
p_peak <- p_peak + scale_x_discrete(labels=c("P2P \ntrading","SC"))
p_peak <- p_peak + labs(fill="Strategy:")
p_peak <- p_peak + ylab('Peak-to-peak\ndifference [kW]')
p_peak <- p_peak + xlab('Strategy')
p_peak <- p_peak + ylim(0,NA)
p_peak <- p_peak + labs(subtitle = "b)",face="bold")
p_peak <- p_peak + theme(plot.subtitle = element_text(hjust = hjust2,vjust=vjust2,size=size_2,face="bold"))
#p_peak <- p_peak + theme(plot.subtitle = element_text(hjust = 0.5,size=size_2,face="bold"))
p_peak <- p_peak + theme(legend.position="none")
p_peak <- p_peak   + theme(axis.text=element_text(size=size_1),
                           axis.title=element_text(size=size_1,face="bold"),
                           axis.text.x = element_text(size = size_1),
                           axis.text.y = element_text(size = size_1),
                           panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p_peak <- p_peak + theme(plot.margin = unit(c(0.5,0.5,0,0), "cm"))
##############Figure 2C########################
df_p<-select(df,c('Comm','Demand_peak','Inj_peak'))
library(tidyr)
df_p<-gather(df_p,key='Type',value='Power',-Comm)

p_peak5 <- ggboxplot(df_p, x= "Comm",y="Power",fill="Comm",outlier.size=0.3)+ facet_grid(~Type,
                                              labeller=as_labeller(c("Demand_peak"="Import peak","Inj_peak"="Export peak")))
p_peak5 <- p_peak5 + stat_compare_means(label.x=1,label.y=50,size=size_3-0.5)
p_peak5 <- p_peak5 + scale_x_discrete(labels=c('P2P\n trading','SC'))
p_peak5 <- p_peak5 + scale_colour_manual(values = alpha(c("#F8766D", "#00BFC4"),.3),
                                         labels=c('P2P trading','Self-consumption\n maximization'))
p_peak5 <- p_peak5 + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Strategy:")
p_peak5 <- p_peak5 + ylab('Power [kW]')
p_peak5 <- p_peak5 + xlab('Strategy')
p_peak5 <- p_peak5 + theme_bw()
p_peak5 <- p_peak5 + scale_y_continuous(limits=c(0,300))
p_peak5 <- p_peak5 + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_peak5 <- p_peak5 + theme(legend.position="",legend.text=element_text(size=size_2),
                           legend.title = element_text(size=size_2,face="bold"))
p_peak5 <- p_peak5 + labs(subtitle = "c)",face="bold")
p_peak5 <- p_peak5 + theme(plot.subtitle = element_text(hjust = hjust2,vjust=vjust2,size=size_2,face="bold"))
#p_peak5 <- p_peak5 + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))
p_peak5 <- p_peak5 + theme(axis.text=element_text(size=size_1),
                           axis.title=element_text(size=size_1,face="bold"),
                           axis.text.x = element_text(size = size_1),
                           axis.text.y = element_text(size = size_1),
                           strip.text.x = element_text(size = size_1),
                           panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#p_peak <- p_peak + theme(plot.margin = unit(c(0.5,0.5,0,0), "cm"))
p_peak5 <- p_peak5 + guides(colour=FALSE, fill=FALSE)
#p_peak5
lay2 <- rbind(c(1,1,1,1),c(2,2,3,3))
grid.arrange(p_peak2, p_peak,p_peak5, layout_matrix = lay2)

############Fig 2 save################
ggsave('../Img/Figure2.pdf',plot=grid.arrange(p_peak2,p_peak,p_peak5, layout_matrix = lay2),
       width = 180, height = 185, units = "mm",
       encoding = "ISOLatin9.enc")

############Fig 3################

h$group<-ifelse(h$X13.0<mean(h$X13.0)-sd(h$X13.0),'Rarely',ifelse(h$X13.0>mean(h$X13.0)+sd(h$X13.0),'Intensively','Moderately'))
h$group<-as.factor(ordered(h$group, levels =  c("Rarely", "Moderately", "Intensively")))
my_comparisons <- list( c("Rarely", "Moderate"), c("Moderate", "Intensive"), c("Rarely", "Intensive") )

###############################FIG 3A trading patterns hh######################################


h_hist <- ggplot(data=h, aes(X13.0,fill=group))
h_hist <- h_hist + geom_histogram(color="black", 
                                  binwidth=1) 
h_hist <- h_hist + theme_bw()
h_hist <- h_hist + scale_fill_discrete(name =  'Dark2')
h_hist <- h_hist + xlab('Amount of trading')
h_hist <- h_hist + ylab('Frequency')
h_hist <- h_hist + theme(legend.position="top",legend.text=element_text(size=size_1),
                         legend.title = element_text(size=size_1,face="bold"))
h_hist <- h_hist + labs(subtitle = "a) Trading decisions", face="bold")
h_hist <- h_hist + theme(plot.subtitle = element_text(hjust = 0.5,size=16,face="bold"))

h_hist <- h_hist + guides(fill=guide_legend(title="Trading:"))
h_hist <- h_hist + scale_fill_discrete(name = "Trading:", labels = c("Restrained", "Moderately", "Intensively"))

h_hist <- h_hist + scale_x_continuous(breaks=c(7.5,12,16.5), labels=c('\u03BC-\u03C3','\u03BC','\u03BC+\u03C3'))
h_hist <- h_hist + theme(axis.text=element_text(size=size_1),
                         axis.title=element_blank(),
                         axis.text.x = element_text(size = size_1),
                         axis.text.y = element_text(size = size_1),
                         strip.text.x = element_text(size = size_1),plot.margin = margin(0, 0, 0, 0, "cm"),
                         panel.grid.major = element_blank(), panel.grid.minor = element_blank())
h_hist
######################Fig 3B1##################################

colnames(df)

d<-subset(d,(d$Comm=='P2P')&(d$type=='PV_batt'))
d$trading<-ordered(d$trading, levels =  c("Rarely", "Moderate", "Intensive"))

d.m <- melt(select(d,-X), id.var = c("trading","type", "Comm"))
d.m_ssr <- subset(d.m,d.m$variable=='SSR')

dummy_ssr <- data.frame(variable = c("SSR"), 
                        Z = c(
                          median(subset(df,(df$Comm=='SC')&(df$index=='PV_batt'))$SSR_hh)))


p_hh_ssr <- ggboxplot(d.m_ssr, x= "trading",y="value",fill="trading",outlier.size=0.3)
#p_hh_ssr <- p_hh_ssr + stat_compare_means(comparisons = my_comparisons,label = "p.signif",symnum.args =symnum.args ) # Add pairwise comparisons p-value
p_hh_ssr <- p_hh_ssr + stat_compare_means(comparisons = my_comparisons,size=size_3-1)
#p_hh_ssr <- p_hh_ssr + stat_compare_means(label.x.npc=0.4,label.y.npc = 0.03)
p_hh_ssr <- p_hh_ssr + stat_compare_means(label.x=1.8,label.y = -10,size=size_3)
p_hh_ssr <- p_hh_ssr + geom_hline(data=dummy_ssr,aes(yintercept = Z),
                                  colour="red",linetype="longdash")
p_hh_ssr <- p_hh_ssr + scale_fill_discrete(name =  'Dark2')
p_hh_ssr <- p_hh_ssr + scale_y_continuous(breaks=c(0,25,50,75,100),limits=c(-11,135))

p_hh_ssr <- p_hh_ssr + theme_bw()
#p_hh_ssr <- p_hh_ssr + ylim(c(-11,130))
p_hh_ssr <- p_hh_ssr + xlab(NULL)
p_hh_ssr <- p_hh_ssr + ylab('Autarky [%]')
p_hh_ssr <- p_hh_ssr + xlab('Trading in the P2P community')

p_hh_ssr <- p_hh_ssr + scale_x_discrete(labels= c("Restrained","Moderate","Intensive"))
p_hh_ssr <- p_hh_ssr + theme(legend.position="")
p_hh_ssr <- p_hh_ssr + labs(subtitle = "b) Individual level",face="bold")
p_hh_ssr <- p_hh_ssr + theme(plot.subtitle = element_text(hjust = hjust2,vjust=vjust2,size=size_2,face="bold"))
p_hh_ssr <- p_hh_ssr + theme(axis.text=element_text(size=size_1),
                             axis.title=element_text(size=size_1,face="bold"),
                             axis.text.x = element_text(size = size_1),
                             axis.text.y = element_text(size = size_1),
                             #axis.text.x=element_blank(),
                             strip.text.x = element_text(size = size_1),plot.margin = margin(0.5, 0, -2, 0, "cm"),
                             panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#p_hh_ssr
######################Fig 3B2##################################

d.m_bill <- subset(d.m,d.m$variable=='bill')

dummy_bill <- data.frame(variable = c("bill"), 
                         Z = c(
                           median(subset(df,(df$Comm=='SC')&(df$index=='PV_batt'))$bill)))


p_hh_bill <- ggboxplot(d.m_bill, x= "trading",y="value",fill="trading",outlier.size=0.3)
#p_hh_bill <- p_hh_bill + stat_compare_means(comparisons = my_comparisons,label = "p.signif",symnum.args =symnum.args ) # Add pairwise comparisons p-value
p_hh_bill <- p_hh_bill + stat_compare_means(comparisons = my_comparisons,size=size_3-1,
                                            label="p.format",aes(label=paste("p = ",..p.format..)))
#p_hh_bill <- p_hh_bill + stat_compare_means(label.x.npc=0.4,label.y.npc = 0.03)
p_hh_bill <- p_hh_bill + stat_compare_means(label.x=1.8,label.y = -1500,size=size_3)
#p_hh_bill <- p_hh_bill + geom_hline(data=dummy_ssr,aes(yintercept = Z),
#                                    colour="red",linetype="longdash")
p_hh_bill <- p_hh_bill + scale_fill_discrete(name =  'Dark2')
p_hh_bill <- p_hh_bill + theme_bw()
p_hh_bill <- p_hh_bill + scale_y_continuous(breaks=c(-1000,0,1000,2000),limits=c(-1500,2800))

#p_hh_bill <- p_hh_bill + ylim(c(-1400,2700))
p_hh_bill <- p_hh_bill + xlab('Trading in P2P community')
p_hh_bill <- p_hh_bill + ylab('Bill [\u20AC]')
p_hh_bill <- p_hh_bill + scale_x_discrete(labels= c("Restrained","Moderate","Intensive"))
p_hh_bill <- p_hh_bill + theme(legend.position="")
p_hh_bill <- p_hh_bill + theme(axis.text=element_text(size=size_1),
                               axis.title=element_text(size=size_1,face="bold"),
                               axis.text.x = element_text(size = size_1),
                               axis.text.y = element_text(size = size_1),
                               strip.text.x = element_text(size = size_1),plot.margin = margin(-2, 0, 0, 0, "cm"),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#p_hh_bill


###############################FIG 3C1######################################
d2$trading<-ordered(d2$trading, levels =  c("Rarely", "Moderate", "Intensive"))
d2.m <- melt(select(d2,-X), id.var = c("trading","Comm"))
d2.m_ssr <- subset(d2.m,d2.m$variable=='SSR_comm')

dummy2 <- data.frame(variable = c("SSR_comm"), 
                     Z = c(
                       median(subset(df,(df$Comm=='SC'))$SSR)))

p_comm_ssr <- ggboxplot(d2.m_ssr, x= "trading",y="value",fill="trading",outlier.size=0.3)
p_comm_ssr <- p_comm_ssr + geom_hline(data=dummy2, aes(yintercept = Z),colour="red",linetype="longdash",outlier.size=2)

p_comm_ssr <- p_comm_ssr + stat_compare_means(comparisons = my_comparisons,size=size_3-1)
p_comm_ssr <- p_comm_ssr + stat_compare_means(label.x.npc=0.4,label.y.npc = 0.03,size=size_3)
p_comm_ssr <- p_comm_ssr + scale_fill_discrete(name =  'Dark2')
p_comm_ssr <- p_comm_ssr + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")
p_comm_ssr <- p_comm_ssr + theme_bw()
p_comm_ssr <- p_comm_ssr + scale_fill_discrete(name =  'Dark2')
p_comm_ssr <- p_comm_ssr + scale_y_continuous(breaks=c(45,48,51,54),limits=c(45,56.5))

#p_comm_ssr <- p_comm_ssr + ylim(c(45,56))
#p_comm_ssr <- p_comm_ssr + scale_y_continuous(labels = addUnits)
p_comm_ssr <- p_comm_ssr + xlab(NULL)
p_comm_ssr <- p_comm_ssr + ylab('Autarky [%]')
p_comm_ssr <- p_comm_ssr + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_comm_ssr <- p_comm_ssr + scale_x_discrete(labels= c("Restrained","Moderate","Intensive"))
p_comm_ssr <- p_comm_ssr + theme(legend.position="")
p_comm_ssr <- p_comm_ssr + xlab('Trading in the P2P community')

p_comm_ssr <- p_comm_ssr + labs(subtitle = "c) Aggregated level",face="bold")
p_comm_ssr <- p_comm_ssr + theme(plot.subtitle = element_text(hjust = hjust2,vjust=vjust2,size=size_2,face="bold"))
p_comm_ssr <- p_comm_ssr  + theme(axis.text=element_text(size=size_1),
                                  axis.title=element_text(size=size_1,face="bold"),
                                  axis.text.x = element_text(size = size_1),
                                  axis.text.y = element_text(size = size_1),
                                  strip.text.x = element_text(size = size_1),plot.margin = margin(0.5, 0, -2, 0, "cm"),
                                  panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#p_comm_ssr

###############################FIG 3C2######################################
d2.m_bill <- subset(d2.m,d2.m$variable=='Bill_comm')

dummy2 <- data.frame(variable = c("Bill_comm"), 
                     Z = c(
                       median(subset(df,(df$Comm=='SC'))$bill)))




p_comm_bill <- ggboxplot(d2.m_bill, x= "trading",y="value",fill="trading",outlier.size=0.3)
#p_comm_bill <- p_comm_bill + geom_hline(data=dummy2, aes(yintercept = Z),colour="red",linetype="longdash")
p_comm_bill <- p_comm_bill + stat_compare_means(label.x=1.8,label.y=53000,size=size_3)
p_comm_bill <- p_comm_bill + stat_compare_means(comparisons = my_comparisons,size=size_3-1,  aes(label = sprintf("p = ", ..p.format..)))

p_comm_bill <- p_comm_bill + scale_fill_discrete(name =  'Dark2')
p_comm_bill <- p_comm_bill + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")
p_comm_bill <- p_comm_bill + theme_bw()
p_comm_bill <- p_comm_bill + scale_fill_discrete(name =  'Dark2')
p_comm_bill <- p_comm_bill + scale_y_continuous(breaks=c(55000,60000,65000,70000,75000),limits=c(53000,76000))
#p_comm_bill <- p_comm_bill + ylim(c(53000,80000))

#p_comm_bill <- p_comm_bill + scale_y_continuous(labels = addUnits)
p_comm_bill <- p_comm_bill + xlab('Trading in the P2P community')
p_comm_bill <- p_comm_bill + ylab('Bill [\u20AC]')
p_comm_bill <- p_comm_bill + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_comm_bill <- p_comm_bill + scale_x_discrete(labels= c("Restrained","Moderate","Intensive"))

p_comm_bill <- p_comm_bill + theme(legend.position="")

p_comm_bill <- p_comm_bill  + theme(axis.text=element_text(size=size_1),
                                    axis.title=element_text(size=size_1,face="bold"),
                                    axis.text.x = element_text(size = size_1),
                                    axis.text.y = element_text(size = size_1),
                                    strip.text.x = element_text(size = size_1),plot.margin = margin(-2, 0, 0, 0, "cm"),
                                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#p_comm_bill




#ggarrange(p_hh_ssr,NULL,p_comm_ssr,
#          p_hh_bill,NULL,p_comm_bill,align='hv',widths = c(1, 0.02,1,1,0.02,1))

lay2 <- rbind(c(1,1),
              c(2,2),
              c(2,2))

grid.arrange(h_hist,
             ggarrange(p_hh_ssr,NULL,p_comm_ssr,p_hh_bill,NULL,p_comm_bill,align='hv',widths = c(1, 0.02, 1,1,0.02,1)),
             layout_matrix = lay2)

ggsave('../Img/Figure3.pdf',grid.arrange(h_hist,
                                         ggarrange(p_hh_ssr,NULL,p_comm_ssr,p_hh_bill,NULL,
                                                   p_comm_bill,align='hv',widths = c(1, 0.02, 1,1,0.02,1)), layout_matrix = lay2),
       width = 180, height = 185, units = "mm",
       device=cairo_pdf)
