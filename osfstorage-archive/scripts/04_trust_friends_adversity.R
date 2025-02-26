##mixed model for trustxFamily Experience predicting friendships
###after revisions this is reported under: In a next analysis step, we were interested in elucidating the relationship of trust, family adversity and the longitudinal development of peer relations....."

rm(list = ls())
require(DataCombine)
require(tidyr)
require(emmeans)
require(dplyr)
require(afex)
require(reshape)
require(ggplot2)
require(ggthemes)
require(interactions)

#############################################
# function to categorise continuous variable
#=============================================
asfactor_cat4 <- function(var){
  var_quant0 <- quantile(var, na.rm=T)[1]
  var_quant25 <- quantile(var, na.rm=T)[2]
  var_quant50 <- quantile(var, na.rm=T)[3]
  var_quant75 <- quantile(var, na.rm=T)[4]
  var_quant100 <-quantile(var, na.rm=T)[5]
  data <- NULL
  data$var <- var
  data$cat_var <- as.numeric()
  data$cat_var[data$var <= var_quant25] <- "low"
  data$cat_var[data$var > var_quant25 & data$var <= var_quant50] <- "medium low"
  data$cat_var[data$var > var_quant50 & data$var <= var_quant75] <- "medium high"
  data$cat_var[data$var > var_quant75] <- "high"
  data$cat_var <- factor(data$cat_var, levels=c("low","medium low","medium high","high"))
  return(data$cat_var)
}
##########



####################################
###grab and pre-organize data########
#####################################

setwd('C:/Users/reiter/Nextcloud/Trust_analysis/dataforupload/')

#friendships
cfq<-read.csv("cfq.csv")
colnames(cfq)[colnames(cfq)=="nspnID"] <- "ID"

#demodat
all.data.demo<-read.csv('all.data.demo.csv')
all.data.demo<-all.data.demo[,1:27] #only use demo data
colnames(all.data.demo)[colnames(all.data.demo)=="nspnID"] <- "ID"

###demo data: time elapsed between questionnaire 1,2,3
mean(all.data.demo$age_hqp2-all.data.demo$age_hqp1, na.rm=T)
sd(all.data.demo$age_hqp2-all.data.demo$age_hqp1, na.rm=T)
mean(all.data.demo$age_hqp3-all.data.demo$age_hqp1, na.rm=T)
sd(all.data.demo$age_hqp3-all.data.demo$age_hqp2, na.rm=T)

###early adversities
eadv<-read.csv(file="ELST_vanHar.csv")
colnames(eadv)[colnames(eadv)=="subject"]<-"ID"

###meanInvestments
meanInvest<-read.csv("meanInvestment.csv")


all.data<-merge(all.data.demo, cfq[,c("ID","friendships_hpq1", "friendships_hpq2", "friendships_hpq3")], by="ID", all=TRUE)
all.data<-merge(all.data, eadv[,c("ID", "ELST1")], by="ID", all=TRUE)
all.data<-merge(all.data, meanInvest[,c("ID"   ,             "meanInvestment",    "tr1investment",    
                                     "meanInvestment.fu", "tr1investment.fu",  "diffInvest" ) ], by="ID", all = TRUE)
dat_res_sh<-gather(all.data, time, friendships, c("friendships_hpq1", "friendships_hpq2", "friendships_hpq3"), factor_key=TRUE)

dat_res_sh$time.num[dat_res_sh$time=="friendships_hpq1"]<-1
dat_res_sh$time.num[dat_res_sh$time=="friendships_hpq2"]<-2
dat_res_sh$time.num[dat_res_sh$time=="friendships_hpq3"]<-3
dat_res_sh$time<-as.factor(dat_res_sh$time.num)
colnames(dat_res_sh)[colnames(dat_res_sh)=="dum3$ELST1"] <- "ELST1"


###rebuild df and add variables

dat_res_sh$age<-NULL
for (k in 1:dim(dat_res_sh)[1]) {
  dat_res_sh[k,'age']<-NA
  if (dat_res_sh[k,]$time==1) {
    dat_res_sh[k,'age']<-as.numeric(dat_res_sh[k,]$age_hqp1)
    dat_res_sh[k,'timediffqt']<-(dat_res_sh[k,]$age_hqp1)-(dat_res_sh[k,]$age.bl)
    
    } else if (dat_res_sh[k,]$time==2) {
      dat_res_sh[k,'age']<-as.numeric(dat_res_sh[k,]$age_hqp2)
      dat_res_sh[k,'timediffqt']<-(dat_res_sh[k,]$age_hqp2)-(dat_res_sh[k,]$age.fu)
    } else if (dat_res_sh[k,]$time==3) {
      dat_res_sh[k,'age']<-as.numeric(dat_res_sh[k,]$age_hqp3)
      dat_res_sh[k,'timediffqt']<-(dat_res_sh[k,]$age_hqp3)-(dat_res_sh[k,]$age.fu)
     } else if (dat_res_sh[k,]$time=="NA") {
      dat_res_sh[k,'age']<-NA
    
  }
}



dat_res_sh<-DropNA(dat_res_sh, Var = c(  "meanInvestment", "tr1investment", "friendships"))
dat_res_sh$ID<-factor(dat_res_sh$ID)



##age

for (i in levels(dat_res_sh$ID)){
  dat_res_sh$mean_subj_age[dat_res_sh$ID==i] <- mean(dat_res_sh$age[dat_res_sh$ID==i], na.rm=T)
}

dat_res_sh$age[is.na(dat_res_sh$age_hqp2)]<-dat_res_sh$mean_subj_age[is.na(dat_res_sh$age_hqp2)]
dat_res_sh$age[is.na(dat_res_sh$age_hqp3)]<-dat_res_sh$mean_subj_age[is.na(dat_res_sh$age_hqp3)]

dat_res_sh$mean_sample_age<- mean(dat_res_sh$age, na.rm=T)
dat_res_sh$longage <- dat_res_sh$age - dat_res_sh$mean_subj_age
dat_res_sh$crossage <- dat_res_sh$mean_subj_age -dat_res_sh$mean_sample_age

dat_res_sh$meanInvestment.c<-scale(dat_res_sh$meanInvestment,  center = TRUE, scale = FALSE)
dat_res_sh$tr1investment.c<-scale(dat_res_sh$tr1investment,  center = TRUE, scale = FALSE)


 ####Do friendships increase with measurement timepoint?
 afex_mo_0<- (mixed(friendships~time+sexFM +(1|ID), data=dat_res_sh, method="S"))
 nice(afex_mo_0)
 emmeans(afex_mo_0, pairwise~time)
##Figure 5a
 p1<- afex_plot( afex_mo_0, x = "time", trace = "time", 
                 mapping = c( "shape", "fill"),
                 data_geom = ggplot2::geom_boxplot, 
                 data_arg = list(width = 0.5))
             p1+ theme_bw() +
              #   jtools::theme_apa()+
              theme(
                plot.background   = element_blank()
                ,panel.grid.major = element_blank()
                ,panel.grid.minor = element_blank()
              )+
              theme(axis.line = element_line(color = 'black'))+
              theme(
                axis.title.x = element_text( size=16),
                axis.title.y = element_text( size=16),
                axis.text=element_text(size=12)
              )
            
##n for legend
unique(afex_mo_0$data$ID) 

#############################################
##Dependency of friends on T1 investment#####
#############################################

 ####MEAN
 afex_mo_1a<- (mixed(friendships~meanInvestment.c*longage+ meanInvestment.c*crossage+sexFM +(1|ID), data=dat_res_sh, method="S"))
 nice(afex_mo_1a)
 summary(afex_mo_1a)
 unique(afex_mo_1a$data$ID) 
 
 
 ###TRIAL 1####
afex_mo_1b<- (mixed(friendships~ tr1investment.c*longage*crossage+sexFM+(1|ID), data=dat_res_sh, method="S"))
 
 nice(afex_mo_1b)

 
 unique(afex_mo_1b$data$ID) 
 
 #for unstandardized estimates
 summary(afex_mo_1b)
# tr1investment.c  1, 757.77    5.23 *    .022
 
 ####only Trial 1 investment, not mean investment significantly predicts friendships over the 3 measurement time points
#--> ###check whether this is moderated by ELST


#####SENSITIVITY ANALYSIS: include IQ as a control, need to mean centre everything again###
dat_res_sh.c<-NULL
dat_res_sh.c<-DropNA(dat_res_sh, Var = c( "IQ.bl"))
dat_res_sh.c$IQ_cent<-scale(dat_res_sh.c$IQ.bl, center = TRUE, scale = FALSE)
dat_res_sh.c$meanInvestment.c<-scale(dat_res_sh.c$meanInvestment,  center = TRUE, scale = FALSE)
dat_res_sh.c$tr1investment.c<-scale(dat_res_sh.c$tr1investment,  center = TRUE, scale = FALSE)


##age

for (i in levels(dat_res_sh.c$ID)){
  dat_res_sh.c$mean_subj_age[dat_res_sh.c$ID==i] <- mean(dat_res_sh.c$age[dat_res_sh.c$ID==i], na.rm=T)
}
dat_res_sh.c$age[is.na(dat_res_sh.c$age_hqp3)]<-dat_res_sh.c$mean_subj_age[is.na(dat_res_sh.c$age_hqp3)]
dat_res_sh.c$age[is.na(dat_res_sh.c$age_hqp2)]<-dat_res_sh.c$mean_subj_age[is.na(dat_res_sh.c$age_hqp2)]
dat_res_sh.c$mean_sample_age<- mean(dat_res_sh.c$age, na.rm=T)
dat_res_sh.c$longage <- dat_res_sh.c$age - dat_res_sh.c$mean_subj_age
dat_res_sh.c$crossage <- dat_res_sh.c$mean_subj_age -dat_res_sh.c$mean_sample_age



##control for IQ

afex_mo_1bIQ<- (mixed(friendships~ tr1investment*longage*crossage+sexFM+IQ_cent+(1|ID), data=dat_res_sh.c, method="S"))
nice(afex_mo_1bIQ)
##still significant
# tr1investment  1, 753.26 4.57 *    .033

############################################
##Dependency of friends on T1 investment####
#******************************************#
##MODERATION BY FAMILY EXPERIENCES##########
#******************************************#
############################################

dat_res_sh_incltrauma <- NULL

##only those that have ELST data // mean-centre again
dat_res_sh_incltrauma<-DropNA(dat_res_sh, "ELST1", "friendships")
dat_res_sh_incltrauma$IQ_cent<-scale(dat_res_sh_incltrauma$IQ.bl, center = TRUE, scale = FALSE)
dat_res_sh_incltrauma$ELSIT_cent<-scale(dat_res_sh_incltrauma$ELST1, center = TRUE, scale = FALSE)
dat_res_sh_incltrauma$ID<-factor(dat_res_sh_incltrauma$ID)
dat_res_sh_incltrauma$meanInvestment.c<-scale(dat_res_sh_incltrauma$meanInvestment,  center = TRUE, scale = FALSE)
dat_res_sh_incltrauma$tr1investment.c<-scale(dat_res_sh_incltrauma$tr1investment,  center = TRUE, scale = FALSE)



for (i in levels(dat_res_sh_incltrauma$ID)){
  dat_res_sh_incltrauma$mean_subj_age[dat_res_sh_incltrauma$ID==i] <- mean(dat_res_sh_incltrauma$age[dat_res_sh_incltrauma$ID==i], na.rm=T)
}
dat_res_sh_incltrauma$age[is.na(dat_res_sh_incltrauma$age_hqp3)]<-dat_res_sh_incltrauma$mean_subj_age[is.na(dat_res_sh_incltrauma$age_hqp3)]
dat_res_sh_incltrauma$age[is.na(dat_res_sh_incltrauma$age_hqp2)]<-dat_res_sh_incltrauma$mean_subj_age[is.na(dat_res_sh_incltrauma$age_hqp2)]
dat_res_sh_incltrauma$mean_sample_age<- mean(dat_res_sh_incltrauma$age, na.rm=T)

dat_res_sh_incltrauma$longage <- dat_res_sh_incltrauma$age - dat_res_sh_incltrauma$mean_subj_age
dat_res_sh_incltrauma$crossage <- dat_res_sh_incltrauma$mean_subj_age - dat_res_sh_incltrauma$mean_sample_age

###TrIAL 1 ##moderation by ELST
afex_mo_1bELST<- (mixed(friendships~ tr1investment.c*longage*ELSIT_cent+ tr1investment.c*longage*crossage+sexFM + tr1investment.c*crossage*ELSIT_cent+(1|ID), data=dat_res_sh_incltrauma, method="S"))
tab1<-as.data.frame(nice(afex_mo_1bELST))
##for revision: add unstandardized estimates
summary(afex_mo_1bELST)
##n for legend
unique(afex_mo_1bELST$data$ID)
###longage:ELSIT_cent 1, 1050.70 13.63 ***   <.001
##tr1investment.c:longage:ELSIT_cent 1, 1050.70   6.77 **    .009

# ##MEAN Investment moderation by ELST
 afex_mo_1b<- (mixed(friendships~meanInvestment.c*longage*ELSIT_cent+longage*crossage*meanInvestment.c+longage*crossage*ELSIT_cent+ sexFM+(1|ID),dat_res_sh_incltrauma, method="S"))
 nice(afex_mo_1b)
###fails to reach significance
## meanInvestment.c:longage 1, 1049.69    3.32 +    .069

 
  ### SENSITIVITY ANALYSIS - holds when only done on participants who have longitudinal data on friendships?
 dat_multiple <- dat_res_sh_incltrauma[dat_res_sh_incltrauma$longage!=0,]
 afex_mo_1cm<- (mixed(friendships~meanInvestment*longage*ELSIT_cent+longage*crossage*ELSIT_cent+meanInvestment*crossage*ELSIT_cent +sexFM +(1|ID),dat_multiple, method="S"))
 nice(afex_mo_1cm)
 afex_mo_1bm<- (mixed(friendships~tr1investment*longage*ELSIT_cent+longage*crossage+tr1investment*crossage+sexFM +(1|ID), dat_multiple, method="S"))
 nice(afex_mo_1bm)

 ## tr1investment:longage:ELSIT_cent 1, 876.00   7.26 **    .007
##yes, effect is even stronger  

 
 ####PLOTTING STUFF
 
longplot1<-interact_plot(afex_mo_1bELST$full_model, pred = "longage", modx = "tr1investment.c", mod2="ELSIT_cent", interval=T, main.title = "Investment Behaviour", color.class = "Blues") #x.label = "Longitudinal Age", y.label = "Investment", legend.main="Cross-sectional Age" ) 
longplot1

#afex_mo_1b<- (mixed(friends~invest.investor.bl.c*longage*ELST1+longage*crossage+invest.investor.bl.c*crossage+sexFM +(1|ID), dat_res_sh_incltrauma, method="S"))
#nice(afex_mo_1b)

longplot2<-interact_plot(afex_mo_1bELST$full_model, pred = "longage", mod2 = "tr1investment", modx="ELSIT_cent", interval=T, main.title = "Investment Behaviour", color.class = "Blues") #x.label = "Longitudinal Age", y.label = "Investment", legend.main="Cross-sectional Age" ) 
longplot2

longplot3<-interact_plot(afex_mo_1c$full_model, pred = "longage", modx = "meanInvestment",mod2="ELSIT_cent", interval=T, main.title = "Investment Behaviour", color.class = "Blues") #x.label = "Longitudinal Age", y.label = "Investment", legend.main="Cross-sectional Age" ) 
longplot3

dat_res_sh_incltrauma$ELST_cat4<-asfactor_cat4(dat_res_sh_incltrauma$ELST1) 
dat_res_sh_incltrauma$tr1invest4<-asfactor_cat4(dat_res_sh_incltrauma$tr1investment) 
dat_res_sh_incltrauma$ELST_cat2<-NULL
dat_res_sh_incltrauma$ELST_cat2[dat_res_sh_incltrauma$ELST_cat4=="medium low"]<-" lower adversity"
dat_res_sh_incltrauma$ELST_cat2[dat_res_sh_incltrauma$ELST_cat4=="low"]<-" lower adversity"
dat_res_sh_incltrauma$ELST_cat2[dat_res_sh_incltrauma$ELST_cat4=="medium high"]<-"higher adversity"
dat_res_sh_incltrauma$ELST_cat2[dat_res_sh_incltrauma$ELST_cat4=="high"]<-"higher adversity"
dat_res_sh_incltrauma$tr1invest2<-NULL
dat_res_sh_incltrauma$tr1invest2[dat_res_sh_incltrauma$tr1invest4=="medium low"]<-"lower round 1 investment"
dat_res_sh_incltrauma$tr1invest2[dat_res_sh_incltrauma$tr1invest4=="low"]<-"lower round 1 investment"
dat_res_sh_incltrauma$tr1invest2[dat_res_sh_incltrauma$tr1invest4=="medium high"]<-"higher round 1 investment"
dat_res_sh_incltrauma$tr1invest2[dat_res_sh_incltrauma$tr1invest4=="high"]<-"higher round 1 investment"

####


plot_res_split_long3<- ggplot(dat_res_sh_incltrauma, aes(x=longage, y=friendships, group=ID)) + 
  geom_point(col="grey",size=1.5,alpha=.9)+
  geom_line(col="grey",alpha=.9)+ geom_smooth(aes(x=longage, y=friendships, colour=tr1invest2, group=NULL), method='lm', size=2)  + ylab("Friendships") + xlab("Longitudinal Development") + labs(color="A-priori Trust")+facet_grid(~ELST_cat2)+
scale_colour_manual(values=c("goldenrod", "chocolate3" ))

###Figure 5C
p<-plot_res_split_long3+theme_bw() + theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(
    plot.background   = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,strip.background = element_blank()
    ,strip.text = element_text(size=10) ##title of the facets
    ,axis.line = element_line(color = 'black')) +
  theme(
    axis.title.x = element_text( size=14),
    axis.title.y = element_text( size=14),
    axis.text=element_text(size=12)) 

p

##Figure 5B
warnings()##
pl2<-ggplot(dat_res_sh_incltrauma, aes(x=tr1investment, y=friendships)) +
  geom_point() +    # Use hollow circles
  geom_smooth(method=lm, color="darkseagreen", size=3)+# Add linear regression line 
  theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background   = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,strip.background = element_blank()
    ,strip.text = element_text(size=7) ##title of the facets
    ,axis.line = element_line(color = 'black'))+
  #  theme("Early Social Adversity") +
  xlab("A Priori Trust (T1)")+
  ylab("Friendships") +
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text( size=16),
    axis.title.y = element_text( size=16),
    axis.text=element_text(size=12))+
  facet_grid(~time)
pl2
