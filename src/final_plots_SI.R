setwd("/data/home/alejandropena/Psychology/Output/")

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
#read the dataframe
df<-read.table('final_dataset.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
df_1 <- subset(df,df$index=='PV_batt')
d<-read.table('optimal_trading_individual.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
d2<-read.table('optimal_trading_community.csv',sep=',',header=TRUE,stringsAsFactors = TRUE)
df_np<-(read.csv('../Output/not_participating.csv'))
d_scp2p<-read.table('sc_vs_p2p.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
df_mean<-read.table('sensitivity_means.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)
############# FIGS #########################

hjust1=-0.3
vjust1=1
hjust2=-0.2
vjust2=1
size_1=12
size_2=16
size_3=3
###############################FIG 1 SI######################################
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

p_sur <- p_sur + theme(legend.text=element_text(size=16),
                       legend.title = element_text(size=16,face="bold"))
p_sur <- p_sur + theme(axis.text=element_text(size=16),
                       axis.title=element_text(size=16,face="bold"))
#ggsave('../Img/SIFigure1.pdf',plot=p_sur,width=15, height=8.5,
#       encoding = "ISOLatin9.enc")
#ggsave('../Img/Selling.pdf',plot=p_sur,width=15, height=8.5,
#       encoding = "ISOLatin9.enc")

###############################FIG 2A SI######################################
my_comparisons <- list( c("Rarely", "Moderate"), c("Moderate", "Intensive"), c("Rarely", "Intensive") )

colnames(df)

d<-subset(d,(d$Comm=='P2P')&(d$type=='PV_batt'))
d$trading<-ordered(d$trading, levels =  c("Rarely", "Moderate", "Intensive"))

d.m <- melt(select(d,-X), id.var = c("trading","type", "Comm"))

d.m_scr <- subset(d.m,d.m$variable=='SCR')

dummy2 <- data.frame(variable = c("SCR"), 
                     Z = c(
                       median(subset(df,(df$Comm=='SC'))$SCR)))


p_hh_scr <- ggboxplot(d.m_scr, x= "trading",y="value",fill="trading")
#p_hh_scr <- p_hh_scr + geom_hline(data=dummy2, aes(yintercept = Z),colour="red",linetype="longdash")

p_hh_scr <- p_hh_scr + stat_compare_means(comparisons = my_comparisons,size=size_3+1)
p_hh_scr <- p_hh_scr + stat_compare_means(label.x=1.8,label.y=0,size=size_3+1)
p_hh_scr <- p_hh_scr + scale_fill_discrete(name =  'Dark2')
p_hh_scr <- p_hh_scr + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")
p_hh_scr <- p_hh_scr + theme_bw()
p_hh_scr <- p_hh_scr + scale_fill_discrete(name =  'Dark2')
#p_hh_scr <- p_hh_scr + ylim(c(45,56))
#p_hh_scr <- p_hh_scr + scale_y_continuous(labels = addUnits)
p_hh_scr <- p_hh_scr + xlab('Trading in the P2P community')
p_hh_scr <- p_hh_scr + ylab('Self-consumption [%]')
p_hh_scr <- p_hh_scr + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_hh_scr <- p_hh_scr + scale_x_discrete(labels= c("Restrained","Moderate","Intensive"))
p_hh_scr <- p_hh_scr + labs(subtitle = "a) Individual level",face="bold")
p_hh_scr <- p_hh_scr + theme(plot.subtitle = element_text(hjust = hjust2+0.1,vjust=vjust2,size=size_2,face="bold"))
p_hh_scr <- p_hh_scr + theme(legend.position="")

p_hh_scr <- p_hh_scr  + theme(axis.text=element_text(size=size_1),
                              axis.title=element_text(size=size_1,face="bold"),
                              axis.text.x = element_text(size = size_1),
                              axis.text.y = element_text(size = size_1),
                              strip.text.x = element_text(size = size_1),#plot.margin = margin(0, 0, 0, 0, "cm"),
                              panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_hh_scr


###############################FIG 2B SI######################################
d2$trading<-ordered(d2$trading, levels =  c("Rarely", "Moderate", "Intensive"))
d2.m <- melt(select(d2,-X), id.var = c("trading","Comm"))
d2.m_scr <- subset(d2.m,d2.m$variable=='SCR_comm')

dummy2 <- data.frame(variable = c("SCR_comm"), 
                     Z = c(
                       median(subset(df,(df$Comm=='SC'))$SCR)))

p_comm_scr <- ggboxplot(d2.m_scr, x= "trading",y="value",fill="trading")
p_comm_scr <- p_comm_scr + geom_hline(data=dummy2, aes(yintercept = Z),colour="red",linetype="longdash")

p_comm_scr <- p_comm_scr + stat_compare_means(comparisons = my_comparisons,size=size_3+1)
p_comm_scr <- p_comm_scr + stat_compare_means(label.x.npc=0.4,label.y.npc = 0.03,size=size_3+1)
p_comm_scr <- p_comm_scr + scale_fill_discrete(name =  'Dark2')
p_comm_scr <- p_comm_scr + labs(shape="Type of prosumer:",fill="Type of prosumer:" ,colour="Community:")
p_comm_scr <- p_comm_scr + theme_bw()
p_comm_scr <- p_comm_scr + scale_fill_discrete(name =  'Dark2')
p_comm_scr <- p_comm_scr + xlab('Trading in the P2P community')

#p_comm_scr <- p_comm_scr + ylim(c(45,56))
#p_comm_scr <- p_comm_scr + scale_y_continuous(labels = addUnits)
p_comm_scr <- p_comm_scr + ylab('Self-consumption [%]')
p_comm_scr <- p_comm_scr + theme(plot.margin = unit(c(0.5,1,0,0), "cm"))
p_comm_scr <- p_comm_scr + scale_x_discrete(labels= c("Restrained","Moderate","Intensive"))
p_comm_scr <- p_comm_scr + theme(legend.position="")
p_comm_scr <- p_comm_scr + labs(subtitle = "b) Aggregated level",face="bold")
p_comm_scr <- p_comm_scr + theme(plot.subtitle = element_text(hjust = hjust2+0.1,vjust=vjust2,size=size_2,face="bold"))
p_comm_scr <- p_comm_scr  + theme(axis.text=element_text(size=size_1),
                                  axis.title=element_text(size=size_1,face="bold"),
                                  axis.text.x = element_text(size = size_1),
                                  axis.text.y = element_text(size = size_1),
                                  strip.text.x = element_text(size = size_1),#plot.margin = margin(0, 0, 0, 0, "cm"),
                                  panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_comm_scr
lay2 <- rbind(c(1,2))
grid.arrange(p_hh_scr,p_comm_scr, layout_matrix = lay2)

#ggsave('../Img/SIFigure2.pdf',grid.arrange(p_hh_scr,p_comm_scr, layout_matrix = lay2),width=15, height=8.5,device=cairo_pdf)
ggsave('../Img/P2P_strategy_SI.pdf',grid.arrange(p_hh_scr,p_comm_scr, layout_matrix = lay2),width=15, height=8.5,device=cairo_pdf)


###############################FIG 5A SI######################################

colnames(df_np)<-c('Reason','Frequency','Group')
df_np<-data.frame(df_np)

df_np$Reason <- factor(df_np$Reason, levels = df_np$Reason[order(df_np$Group,decreasing=TRUE)])
df_np$Frequency<-df_np$Frequency/75*100

p <- ggplot(df_np,aes(x=`Reason`,y=`Frequency`, fill=as.factor(`Group`)))
p <- p + theme_bw()
p <- p + geom_bar(alpha=0.6,stat='identity')
p <- p + coord_flip()
p <- p + scale_fill_hue(direction = -1, h.start=90)
p <- p + scale_fill_discrete(labels=c('Complexity','Selectivity',
                                      'Trust','Inequality','Independence','Other'))
p <- p + theme(legend.position="")
p <- p + labs(subtitle = "a)",face="bold")
p <- p + theme(plot.subtitle = element_text(hjust = hjust2,vjust=vjust2,size=size_2,face="bold"))

p <- p + theme(axis.text=element_text(size=size_1),
               axis.title=element_text(size=size_1,face="bold"),
               axis.text.x = element_text(size = size_1),
               axis.text.y = element_text(size = size_1),
               strip.text.x = element_text(size = size_1),#plot.margin = margin(0, 0, 0, 0, "cm"),
               panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p
###############################FIG 5B SI######################################

df2_np<-aggregate(df_np$Frequency, by=list(Category=df_np$Group), FUN=sum)
colnames(df2_np)<-c('Category','Frequency')
df2_np$Category<-c('Complexity','Selectivity','Supply satisfaction',
                'Lack of trust','Inequality','Independence','Other')
df2_np$Category<-factor(df2_np$Category,rev(c('Complexity','Selectivity','Supply satisfaction',
                                        'Lack of trust','Inequality','Independence','Other')))
p2 <- ggplot(df2_np,aes(x=`Category`,y=`Frequency`, fill=as.factor(`Category`)))
p2 <- p2 + theme_bw()
p2 <- p2 + geom_bar(alpha=0.6,stat='sum')
p2 <- p2 + coord_flip()
p2 <- p2 + theme(legend.position="")
p2 <- p2 + scale_fill_hue(direction = -1, h.start=90)
p2 <- p2 + labs(subtitle = "b)",face="bold")
p2 <- p2 + theme(plot.subtitle = element_text(hjust = hjust2,vjust=vjust2,size=size_2,face="bold"))

p2 <- p2 + theme(axis.text=element_text(size=size_1),
                 axis.title=element_text(size=size_1,face="bold"),
                 axis.text.x = element_text(size = size_1),
                 axis.text.y = element_text(size = size_1),
                 strip.text.x = element_text(size = size_1),#plot.margin = margin(0, 2, 0, 0, "cm"),
                 panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p2
df2_np

grid.arrange(p,p2,ncol=2)
#ggsave('../Img/Not_participating.pdf',plot=grid.arrange(p,p2,ncol=2),width=15, height=8.5,encoding = "ISOLatin9.enc")
#ggsave('../Img/SIFigure5.pdf',plot=grid.arrange(p,p2,ncol=2),width=15, height=8.5,encoding = "ISOLatin9.enc")

###############################FIG 14 SI######################################

unique(d_scp2p$Comm_SC)
unique(d_scp2p$Comm_P2P)
my_comparisons <- list( c("SC", "SC_P2P"), c("SC_P2P", "P2P"), c("P2P", "SC") )


unique(d_scp2p$Comm_P2P)

d_scp2p
p_bill <- ggboxplot(d_scp2p, x= "Comm_P2P",y="bill", fill="Comm_P2P")
p_bill <- p_bill + facet_wrap_custom(~type, scales="free_y",ncol=1,
                                     labeller = as_labeller(c("No"="No PV nor battery",
                                                              "PV"= "PV only",
                                                              "PV_batt"= "PV and battery")), scale_overrides = list(
                                                                scale_override(1, scale_y_continuous(limits=c(-1200,3500),expand = c(0,0))),
                                                                scale_override(2, scale_y_continuous(limits=c(-1200,3500),expand = c(0,0))),
                                                                scale_override(3, scale_y_continuous(limits=c(-1200,3500),expand = c(0,0)))
                                                              ))
#p_bill <- p_bill + stat_compare_means(label.x.npc = 0.85,label.y.npc = 0.95)
p_bill <- p_bill + theme_bw()
p_bill <- p_bill + ylab('Bill\n[€ p.a.]')
p_bill <- p_bill + xlab('Strategy')

p_bill <- p_bill + scale_x_discrete(labels=c("SC" = "Self-\nconsumption\nmaximization", "SC_P2P" = "Self-\nconsumption\n maximization with\nP2P pricing",
                                             "P2P" = "P2P trading"))
p_bill <- p_bill + scale_fill_discrete(name="Strategy:",labels=c("SC" = "Self-\nconsumption\nmaximization", "SC_P2P" = "Self-\nconsumption\nmaximization with\nP2P pricing",
                                                                 "P2P" = "P2P trading"))
p_bill <- p_bill + stat_compare_means(comparisons = my_comparisons,size=size_3)

p_bill <- p_bill + labs(subtitle = "c)" ,face="bold")

p_bill <- p_bill + theme(plot.subtitle = element_text(hjust = hjust2+0.2,vjust=vjust2,size=size_2,face="bold"))

p_bill <- p_bill + theme(axis.text=element_text(size=size_2),
                 axis.title=element_text(size=size_2,face="bold"),
                 axis.text.x = element_text(size = size_2),
                 axis.text.y = element_text(size = size_2),legend.position = "none",
                 strip.text.x = element_text(size = size_2),#plot.margin = margin(0, 2, 0, 0, "cm"),
                 panel.grid.major = element_blank(), panel.grid.minor = element_blank())


p_scr <- ggboxplot(subset(d_scp2p,d_scp2p$type=='PV_batt'), x= "Comm_P2P",y="SCR", fill="Comm_P2P")
p_scr <- p_scr + facet_wrap_custom(~type, scales="free_y",ncol=1,
                                   labeller = as_labeller(c("No"="No PV nor battery",
                                                            "PV"= "PV only",
                                                            "PV_batt"= "PV and battery")), scale_overrides = list(
                                                              scale_override(1, scale_y_continuous(limits=c(0,130),expand = c(0,0)))
                                                            ))
p_scr <- p_scr + stat_compare_means(comparisons = my_comparisons,size=size_3)
p_scr <- p_scr + theme_bw()
p_scr <- p_scr + ylab('Self-consumption\n[%]')
p_scr <- p_scr + xlab('Strategy')
p_scr <- p_scr + labs(subtitle = "a)" ,face="bold")

p_scr <- p_scr + scale_x_discrete(labels=c("SC" = "Self-\nconsumption\nmaximization", "SC_P2P" = "Self-\nconsumption\n maximization with\nP2P pricing",
                                           "P2P" = "P2P trading"))
p_scr <- p_scr + scale_fill_discrete(name="Strategy:",labels=c("SC" = "Self-\nconsumption\nmaximization", "SC_P2P" = "Self-\nconsumption\n maximization with\nP2P pricing",
                                                               "P2P" = "P2P trading"))
p_scr <- p_scr + theme(plot.subtitle = element_text(hjust = hjust2+0.2,vjust=vjust2,size=size_2,face="bold"))

p_scr <- p_scr + theme(axis.text=element_text(size=size_2),
                         axis.title=element_text(size=size_2,face="bold"),
                         axis.text.x = element_text(size = size_2),
                         axis.text.y = element_text(size = size_2),legend.position = "none",
                         strip.text.x = element_text(size = size_2),#plot.margin = margin(0, 2, 0, 0, "cm"),
                         panel.grid.major = element_blank(), panel.grid.minor = element_blank())


p_ssr <- ggboxplot(subset(d_scp2p,d_scp2p$type=='PV_batt'), x= "Comm_P2P",y="SSR", fill="Comm_P2P")
p_ssr <- p_ssr + facet_wrap_custom(~type, scales="free_y",ncol=1,
                                   labeller = as_labeller(c("No"="No PV nor battery",
                                                            "PV"= "PV only",
                                                            "PV_batt"= "PV and battery")), scale_overrides = list(
                                                              scale_override(1, scale_y_continuous(limits=c(0,150),expand = c(0,0)))
                                                            ))
p_ssr <- p_ssr + stat_compare_means(comparisons = my_comparisons,size=size_3)
p_ssr <- p_ssr + theme_bw()
p_ssr <- p_ssr + ylab('Autarky\n[%]')
p_ssr <- p_ssr + xlab('Strategy')

p_ssr <- p_ssr + scale_x_discrete(labels=c("SC" = "Self-\nconsumption\nmaximization", "SC_P2P" = "Self-\nconsumption\n maximization with\nP2P pricing",
                                           "P2P" = "P2P trading"))
p_ssr <- p_ssr + scale_fill_discrete(name="Strategy:",labels=c("SC" = "Self-\nconsumption\nmaximization", "SC_P2P" = "Self-\nconsumption\n maximization with\nP2P pricing",
                                                               "P2P" = "P2P trading"))
p_ssr <- p_ssr + labs(subtitle = "b)" ,face="bold")
p_ssr <- p_ssr + theme(plot.subtitle = element_text(hjust = hjust2+0.2,vjust=vjust2,size=size_2,face="bold"))

p_ssr <- p_ssr + theme(axis.text=element_text(size=size_2),
                       axis.title=element_text(size=size_2,face="bold"),
                       axis.text.x = element_text(size = size_2),
                       axis.text.y = element_text(size = size_2),legend.position = "none",
                       strip.text.x = element_text(size = size_2),#plot.margin = margin(0, 2, 0, 0, "cm"),
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank())


lay <- rbind(c(2,1),
             c(2,1),
             c(3,1),
             c(3,1))
grid.arrange(p_bill,p_scr,p_ssr,layout_matrix = lay)
ggsave('../Img/comparison_ind.pdf',plot=grid.arrange(p_bill,p_scr,p_ssr,layout_matrix = lay),width=15, height=8.5,
       encoding = "ISOLatin9.enc")
###############################FIG 15 SI######################################

p_bill_com <- ggboxplot(d_scp2p, x= "Comm_P2P",y="Bill_comm", fill="Comm_P2P")
p_bill_com <- p_bill_com + stat_compare_means(comparisons = my_comparisons,size=size_3)
p_bill_com <- p_bill_com + theme_bw()
p_bill_com <- p_bill_com + ylab('Community bill\n[€ p.a.]')
p_bill_com <- p_bill_com + xlab('Strategy')

p_bill_com <- p_bill_com + scale_x_discrete(labels=c("SC" = "Self-\nconsumption\nmaximization", "SC_P2P" = "Self-\nconsumption\n maximization with\nP2P pricing",
                                                     "P2P" = "P2P trading"))
p_bill_com <- p_bill_com + labs(subtitle = "a)" ,face="bold",size=16)
p_bill_com <- p_bill_com + theme(plot.subtitle = element_text(hjust = hjust2+0.2,vjust=vjust2,size=16,face="bold"))
p_bill_com <- p_bill_com + theme(axis.text=element_text(size=size_2),
                                 axis.title=element_text(size=size_2,face="bold"),
                                 axis.text.x = element_text(size = size_2),
                                 axis.text.y = element_text(size = size_2),legend.position = "none",
                                 strip.text.x = element_text(size = size_2),#plot.margin = margin(0, 2, 0, 0, "cm"),
                                 panel.grid.major = element_blank(), panel.grid.minor = element_blank())


p_bill_com <- p_bill_com + scale_fill_discrete(name="Strategy:",labels=c("SC" = "Self-\nconsumption\nmaximization", "SC_P2P" = "Self-\nconsumption\n maximization with\nP2P pricing",
                                                                         "P2P" = "P2P trading"))
p_scr_com <- ggboxplot(d_scp2p, x= "Comm_P2P",y="SCR_comm", fill="Comm_P2P")
p_scr_com <- p_scr_com + stat_compare_means(comparisons = my_comparisons,size=size_3)
p_scr_com <- p_scr_com + theme_bw()
p_scr_com <- p_scr_com + ylab('Self-consumption\n[%]')
p_scr_com <- p_scr_com + xlab('Strategy')

p_scr_com <- p_scr_com + scale_x_discrete(labels=c("SC" = "Self-\nconsumption\nmaximization", "SC_P2P" = "Self-\nconsumption\n maximization with\nP2P pricing",
                                                   "P2P" = "P2P trading"))
p_scr_com <- p_scr_com + labs(subtitle = "b)" ,face="bold",size=16)
p_scr_com <- p_scr_com + theme(plot.subtitle = element_text(hjust = hjust2+0.2,vjust=vjust2,size=16,face="bold"))
p_scr_com <- p_scr_com + theme(axis.text=element_text(size=size_2),
                               axis.title=element_text(size=size_2,face="bold"),
                               axis.text.x = element_text(size = size_2),
                               axis.text.y = element_text(size = size_2),legend.position = "none",
                               strip.text.x = element_text(size = size_2),#plot.margin = margin(0, 2, 0, 0, "cm"),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p_scr_com <- p_scr_com + scale_fill_discrete(name="Strategy:",labels=c("SC" = "Self-\nconsumption\nmaximization", "SC_P2P" = "Self-\nconsumption\n maximization with\nP2P pricing",
                                                                       "P2P" = "P2P trading"))

p_ssr_com <- ggboxplot(d_scp2p, x= "Comm_P2P",y="SSR_comm", fill="Comm_P2P")
p_ssr_com <- p_ssr_com + stat_compare_means(comparisons = my_comparisons,size=size_3)
p_ssr_com <- p_ssr_com + theme_bw()
p_ssr_com <- p_ssr_com + ylab('Autarky\n[%]')
p_ssr_com <- p_ssr_com + xlab('Strategy')
p_ssr_com <- p_ssr_com + scale_x_discrete(labels=c("SC" = "Self-\nconsumption\nmaximization", "SC_P2P" = "Self-\nconsumption\n maximization with\nP2P pricing",
                                                   "P2P" = "P2P trading"))
p_ssr_com <- p_ssr_com + labs(subtitle = "c)" ,face="bold",size=16)
p_ssr_com <- p_ssr_com + theme(plot.subtitle = element_text(hjust = hjust2+0.2,vjust=vjust2,size=16,face="bold"))
p_ssr_com <- p_ssr_com + theme(axis.text=element_text(size=size_2),
                               axis.title=element_text(size=size_2,face="bold"),
                               axis.text.x = element_text(size = size_2),
                               axis.text.y = element_text(size = size_2),legend.position = "none",
                               strip.text.x = element_text(size = size_2),#plot.margin = margin(0, 2, 0, 0, "cm"),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p_ssr_com <- p_ssr_com + scale_fill_discrete(name="Strategy:",labels=c("SC" = "Self-\nconsumption\nmaximization", "SC_P2P" = "Self-\nconsumption\n maximization with\nP2P pricing",
                                                                       "P2P" = "P2P trading"))

lay <- rbind(c(2,1),
             c(2,1),
             c(3,1),
             c(3,1))
grid.arrange(p_bill_com,p_scr_com,p_ssr_com,nrow=1, ncol=3)
ggsave('../Img/compariso_com.pdf',plot=grid.arrange(p_bill_com,p_scr_com,p_ssr_com,nrow=1, ncol=3),width=20, height=8.5,
       encoding = "ISOLatin9.enc")

################Fig 17 SI##############################

#df_s<-read.table('sensitivity.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)



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
p_scr_ind <- p_scr_ind + ylab('Self-consumption\n[%]')
p_scr_ind <- p_scr_ind + xlab(NULL)
p_scr_ind <- p_scr_ind + labs(subtitle = "b)" ,face="bold",size=16)
p_scr_ind <- p_scr_ind + theme(plot.subtitle = element_text(hjust = hjust2+0.2,vjust=vjust2,size=size_2,face="bold"))

p_scr_ind <- p_scr_ind + theme(axis.text=element_text(size=size_1),
                               axis.title=element_text(size=size_1,face="bold"),
                               axis.text.x = element_text(size = size_1),
                               axis.text.y = element_text(size = size_1),
                               strip.text.x = element_text(size = size_1),#plot.margin = margin(0, 2, 0, 0, "cm"),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank())



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
p_ss_ind <- p_ss_ind + labs(subtitle = "c)" ,face="bold",size=16)
p_ss_ind <- p_ss_ind + theme(plot.subtitle = element_text(hjust = hjust2+0.2,vjust=vjust2,size=size_2,face="bold"))
p_ss_ind <- p_ss_ind + theme(axis.text=element_text(size=size_1),
                             axis.title=element_text(size=size_1,face="bold"),
                             axis.text.x = element_text(size = size_1),
                             axis.text.y = element_text(size = size_1),
                             strip.text.x = element_text(size = size_1),#plot.margin = margin(0, 2, 0, 0, "cm"),
                             panel.grid.major = element_blank(), panel.grid.minor = element_blank())



p_bill_ind <- ggplot(df_mean, aes(x=PV_penetration_x,y=bill, group=size_comm,fill=factor(size_comm)))
p_bill_ind <- p_bill_ind + geom_bar(stat="identity",position="dodge")
p_bill_ind <- p_bill_ind + facet_grid(type~Batt_penetration_x,
                                      labeller = as_labeller(c("25"= "Battery penetration\n25%",
                                                               "50"= "Battery penetration\n50%",
                                                               "75"= "Battery penetration\n 75%",
                                                               "No"= "No PV\n no bat.",
                                                               "PV"= "PV\nonly",
                                                               "PV_batt"= "PV &\nbat.")))
p_bill_ind <- p_bill_ind + theme_bw()
p_bill_ind <- p_bill_ind + labs(fill="Community size:")
p_bill_ind <- p_bill_ind + ylab('Individual bill\n[€ p.a.]')
p_bill_ind <- p_bill_ind + xlab(NULL)
p_bill_ind <- p_bill_ind + labs(subtitle = "a)" ,face="bold",size=16)
p_bill_ind <- p_bill_ind + theme(plot.subtitle = element_text(hjust = hjust2+0.2,vjust=vjust2,size=size_2,face="bold"))
p_bill_ind <- p_bill_ind + theme(axis.text=element_text(size=size_1),
                                 axis.title=element_text(size=size_1,face="bold"),
                                 axis.text.x = element_text(size = size_1),
                                 axis.text.y = element_text(size = size_1),
                                 strip.text.x = element_text(size = size_1),#plot.margin = margin(0, 2, 0, 0, "cm"),
                                 panel.grid.major = element_blank(), panel.grid.minor = element_blank())



lay <- rbind(c(1),
             c(2),
             c(3))
grid.arrange(p_bill_ind,p_scr_ind,p_ss_ind,layout_matrix = lay)
#ggsave('../Img/SIFigure17.pdf',plot=grid.arrange(p_bill_ind,p_scr_ind,p_ss_ind,nrow=3, ncol=1),width=15, height=8.5,
#       encoding = "ISOLatin9.enc")

ggsave('../Img/sensitivity_ind.pdf',plot=grid.arrange(p_bill_ind,p_scr_ind,p_ss_ind,layout_matrix = lay),width=15, height=8.5,
       encoding = "ISOLatin9.enc")
################Fig 18 SI##############################

p_scr_com <- ggplot(df_mean, aes(x=PV_penetration_x,y=SCR_comm, group=size_comm,fill=factor(size_comm)))
p_scr_com <- p_scr_com + geom_bar(stat="identity",position="dodge")
p_scr_com <- p_scr_com + facet_grid(~Batt_penetration_x,
                                    labeller = as_labeller(c("25"= "Battery penetration\n25%",
                                                             "50"= "Battery penetration\n50%",
                                                             "75"= "Battery penetration\n 75%")))
p_scr_com <- p_scr_com + theme_bw()
p_scr_com <- p_scr_com + scale_y_continuous(limits=c(0,100))
p_scr_com <- p_scr_com + labs(fill="Community size:")
p_scr_com <- p_scr_com + ylab('Community\nSelf-consumption\n[%]')
p_scr_com <- p_scr_com + xlab(NULL)
p_scr_com <- p_scr_com + labs(subtitle = "b)" ,face="bold",size=16)
p_scr_com <- p_scr_com + theme(plot.subtitle = element_text(hjust = hjust2+0.2,vjust=vjust2,size=size_2,face="bold"))
p_scr_com <- p_scr_com + theme(axis.text=element_text(size=size_1),
                               axis.title=element_text(size=size_1,face="bold"),
                               axis.text.x = element_text(size = size_1),
                               axis.text.y = element_text(size = size_1),
                               strip.text.x = element_text(size = size_1),#plot.margin = margin(0, 2, 0, 0, "cm"),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_ss_com <- ggplot(df_mean, aes(x=PV_penetration_x,y=SSR_comm, group=size_comm,fill=factor(size_comm)))
p_ss_com <- p_ss_com + geom_bar(stat="identity",position="dodge")
p_ss_com <- p_ss_com + facet_grid(~Batt_penetration_x,
                                  labeller = as_labeller(c("25"= "Battery penetration\n25%",
                                                           "50"= "Battery penetration\n50%",
                                                           "75"= "Battery penetration\n 75%")))
p_ss_com <- p_ss_com + theme_bw()
p_ss_com <- p_ss_com + scale_y_continuous(limits=c(0,100))
p_ss_com <- p_ss_com + labs(fill="Community size:")
p_ss_com <- p_ss_com + ylab('Community\nAutarky\n[%]')
p_ss_com <- p_ss_com + xlab('PV penetration [%]')
p_ss_com <- p_ss_com + labs(subtitle = "c)" ,face="bold",size=16)
p_ss_com <- p_ss_com + theme(plot.subtitle = element_text(hjust = hjust2+0.2,vjust=vjust2,size=size_2,face="bold"))
p_ss_com <- p_ss_com + theme(axis.text=element_text(size=size_1),
                             axis.title=element_text(size=size_1,face="bold"),
                             axis.text.x = element_text(size = size_1),
                             axis.text.y = element_text(size = size_1),
                             strip.text.x = element_text(size = size_1),#plot.margin = margin(0, 2, 0, 0, "cm"),
                             panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_bill_com <- ggplot(df_mean, aes(x=PV_penetration_x,y=Bill_comm, group=size_comm,fill=factor(size_comm)))
p_bill_com <- p_bill_com + geom_bar(stat="identity",position="dodge")
p_bill_com <- p_bill_com + facet_grid(~Batt_penetration_x,
                                      labeller = as_labeller(c("25"= "Battery penetration\n25%",
                                                               "50"= "Battery penetration\n50%",
                                                               "75"= "Battery penetration\n 75%")))

p_bill_com <- p_bill_com + theme_bw()
p_bill_com <- p_bill_com + labs(fill="Community size:")
p_bill_com <- p_bill_com + ylab('Community bill\n[€ p.a.]')
p_bill_com <- p_bill_com + xlab(NULL)
p_bill_com <- p_bill_com + labs(subtitle = "a)" ,face="bold",size=16)
p_bill_com <- p_bill_com + theme(plot.subtitle = element_text(hjust = hjust2+0.2,vjust=vjust2,size=size_2,face="bold"))
p_bill_com <- p_bill_com + theme(axis.text=element_text(size=size_1),
                                 axis.title=element_text(size=size_1,face="bold"),
                                 axis.text.x = element_text(size = size_1),
                                 axis.text.y = element_text(size = size_1),
                                 strip.text.x = element_text(size = size_1),#plot.margin = margin(0, 2, 0, 0, "cm"),
                                 panel.grid.major = element_blank(), panel.grid.minor = element_blank())
lay <- rbind(c(1,1),
             c(2,2),
             c(3,3))
grid.arrange(p_bill_com,p_scr_com,p_ss_com,nrow=3, ncol=1)

#ggsave('../Img/SIFigure18.pdf',plot=grid.arrange(p_bill_com,p_scr_com,p_ss_com,nrow=3, ncol=1),width=15, height=8.5,
#       encoding = "ISOLatin9.enc")
ggsave('../Img/sensitivity_com.pdf',plot=grid.arrange(p_bill_com,p_scr_com,p_ss_com,nrow=3, ncol=1),width=15, height=8.5,
       encoding = "ISOLatin9.enc")


