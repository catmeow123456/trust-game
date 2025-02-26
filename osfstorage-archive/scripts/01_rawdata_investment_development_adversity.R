##does developmental analyses on investment round.by-round, on trial 1, 

# as reported under "Behavioural Analyses: Longitudinal Development of Trust Behaviour "
#adds the early adversity factor derived as in van Harmelen to the analyses
#(as reported unter "Relationship between adolescent development of trust and self-reported family adversity ")

###makes Fig 1,2,3
### also reads out age range, long followup range, gender etc for methods


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
options(scipen = 999)

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


setwd('../data')
cfq<-read.csv("cfq.csv")
all.data.demo<-read.csv('all.data.demo.csv')
all.data.demo<-all.data.demo[,1:27] #only use demo data

###early adversities
eadv<-read.csv(file="ELST_vanHar.csv")

###round-by-round bhv
dat_bl.prl<-read.csv("BaseNSPNTrust.csv",  sep=";")

dat_bl<-merge(dat_bl.prl, eadv, by.x="ID", by.y="subject", all.x=T)

subexList <- all.data.demo$nspnID[all.data.demo$study=="Depression"]
subex <- "16261" ##duplicate subject
dat_bl<-subset(dat_bl, !(ID %in% subexList))
dat_bl<-subset(dat_bl, !(ID %in% subex))


dat_bl_investor<-dplyr::select(dat_bl, c("ID", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "ELST1"))

colnames(dat_bl_investor)[colnames(dat_bl_investor)=="X1"] <- "1"
colnames(dat_bl_investor)[colnames(dat_bl_investor)=="X2"] <- "2" 
colnames(dat_bl_investor)[colnames(dat_bl_investor)=="X3"] <- "3"
colnames(dat_bl_investor)[colnames(dat_bl_investor)=="X4"] <- "4"
colnames(dat_bl_investor)[colnames(dat_bl_investor)=="X5"] <- "5"
colnames(dat_bl_investor)[colnames(dat_bl_investor)=="X6"] <- "6"
colnames(dat_bl_investor)[colnames(dat_bl_investor)=="X7"] <- "7"
colnames(dat_bl_investor)[colnames(dat_bl_investor)=="X8"] <- "8"
colnames(dat_bl_investor)[colnames(dat_bl_investor)=="X9"] <- "9"
colnames(dat_bl_investor)[colnames(dat_bl_investor)=="X10"] <- "10"

dat_bl_investor<-merge(dat_bl_investor, all.data.demo, by.x="ID", by.y="nspnID",all.x=T)
all_data_bl<-subset(dat_bl_investor, dat_bl_investor$study!="Depression")
all_data_bl$meanInvestment<-(all_data_bl$`1`+all_data_bl$`2`+all_data_bl$`3`+all_data_bl$`4`+all_data_bl$`5`+all_data_bl$`6`+all_data_bl$`7`+all_data_bl$`8`+all_data_bl$`9`+all_data_bl$`10`)/10


#######################################################################
############# first, build long df  for investment (trust) analysis##
######################################################################

##investor data
dat_bl_inv_long <- gather(dat_bl_investor, trial, invest, "1":"10", factor_key=TRUE) # this wirtes trial by trial investment choices vertically
dat_bl_round1_data<-subset(dat_bl_inv_long, dat_bl_inv_long$trial=="1") ##trial 0 is the first investment, without any observed behaviour of trustee
dat_bl_inv_long$trial.n<-as.numeric(as.character(dat_bl_inv_long$trial)) 

###data orga baseline ends here

####################################
###get and add longitudinal data###
##################################

dat_fu<-read.csv("FollowUpNSPNTrust.csv",  sep=";")
dat_fu<-subset(dat_fu, !(ID %in% subexList))
dat_fu<-subset(dat_fu, !(ID %in% subex))

dat_fu_investor<-dat_fu[,1:11] #split up into investor and 

# investor's investment
colnames(dat_fu_investor)[colnames(dat_fu_investor)=="X1"] <- "1.fu"
colnames(dat_fu_investor)[colnames(dat_fu_investor)=="X2"] <- "2.fu"
colnames(dat_fu_investor)[colnames(dat_fu_investor)=="X3"] <- "3.fu"
colnames(dat_fu_investor)[colnames(dat_fu_investor)=="X4"] <- "4.fu"
colnames(dat_fu_investor)[colnames(dat_fu_investor)=="X5"] <- "5.fu"
colnames(dat_fu_investor)[colnames(dat_fu_investor)=="X6"] <- "6.fu"
colnames(dat_fu_investor)[colnames(dat_fu_investor)=="X7"] <- "7.fu"
colnames(dat_fu_investor)[colnames(dat_fu_investor)=="X8"] <- "8.fu"
colnames(dat_fu_investor)[colnames(dat_fu_investor)=="X9"] <- "9.fu"
colnames(dat_fu_investor)[colnames(dat_fu_investor)=="X10"] <-"10.fu"



##mean Investment Values
dat_fu_investor<-merge(dat_fu_investor, all.data.demo, by.x="ID", by.y="nspnID")
dat_fu_investor$meanInvestment.fu<-(dat_fu_investor$`1.fu`+ dat_fu_investor$`2.fu`+dat_fu_investor$`3.fu`+ dat_fu_investor$`4.fu`+ dat_fu_investor$`5.fu`+dat_fu_investor$`6.fu`+ dat_fu_investor$`7.fu`+ dat_fu_investor$`8.fu`+ dat_fu_investor$`9.fu`+dat_fu_investor$`10.fu`)/10

meanInvestmentcombined <- merge(x =all_data_bl[,c("ID","meanInvestment")],  y = dat_fu_investor[,c("ID","meanInvestment.fu")], by = "ID", all.x = TRUE) 
meanInvestmentcombined$diffInvest<-meanInvestmentcombined$meanInvestment.fu-meanInvestmentcombined$meanInvestment

###for Fig 1C
hist.bl<-ggplot(meanInvestmentcombined, aes(meanInvestment, fill = cut(meanInvestment, 100))) +
  geom_histogram(show.legend = FALSE) +
  scale_fill_discrete(h = c(240, 10), c = 120, l = 70) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 50))+
  theme_bw() +
  jtools::theme_apa()+
  #eliminates background, gridlines, and chart border
  theme(
    plot.background   = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )+
  theme(axis.line = element_line(color = 'black'))+
  theme(
    axis.title.x = element_text( size=16),
    axis.title.y = element_text( size=16),
    axis.text=element_text(size=12))+
  labs(x = "Mean Investment", y = "n") +
  ggtitle("Baseline")

hist.bl

hist.fu<-ggplot(meanInvestmentcombined, aes(meanInvestment.fu, fill = cut(meanInvestment.fu, 100))) +
  geom_histogram(show.legend = FALSE) +
  scale_fill_discrete(h = c(240, 10), c = 120, l = 70) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 50))+
  theme_bw() +
  jtools::theme_apa()+
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
  )+
  labs(x = "Mean Investment", y = "n") +
  ggtitle("Follow up")

hist.bl
hist.fu

##superimpose both: reproduce FIG 1C

meanInvestmentnoDropOut<-DropNA(meanInvestmentcombined, Var="meanInvestment.fu")

##ggplot
histggplotdf<-reshape(meanInvestmentnoDropOut, direction = "long", varying=2:3,  timevar="condition", idvar="ID")
histggplotdf$condition[histggplotdf$condition=="meanInvestment"]<- "Baseline"
histggplotdf$condition[histggplotdf$condition=="fu"]<- "Follow-up"
ggplot(histggplotdf, aes(x=meanInvestment, fill=condition)) + geom_histogram(alpha=0.2, position="identity")+
  theme_minimal() +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 50))+
  theme_bw() +
  jtools::theme_apa()+
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
  )+
  labs(x = "Mean Investment", y = "n")+
  scale_fill_discrete(type = c("aquamarine3", "darkslateblue" ))

#write.csv(meanInvestmentcombined, "meanInvestment_new.csv")

##demogrpahics fu###
range(dat_fu_investor$age.fu)
mean(dat_fu_investor$age.fu)
sd(dat_fu_investor$age.fu)
sum(dat_fu_investor$sexFM=="Female")
sum(dat_fu_investor$sexFM=="Male")

###time between task measurements
range(dat_fu_investor$age.fu-dat_fu_investor$age.bl)
mean(dat_fu_investor$age.fu-dat_fu_investor$age.bl)
sd(dat_fu_investor$age.fu-dat_fu_investor$age.bl)

###long format with respect to trial
###investor data
dat_fu_inv_long <- gather(dat_fu_investor, trial, invest, "1.fu":"10.fu", factor_key=TRUE)

### for first trial analysis
dat_fu_round1_data<-dplyr::select(dat_fu_investor, c("ID","1.fu"))

##exclude depression cohort
all.data.fu<-subset(dat_fu_inv_long , dat_fu_inv_long$study!="Depression")
colnames(all.data.fu)[colnames(all.data.fu)=="invest"] <- "invest.fu"


all.data.fu$trial.num[all.data.fu$trial=="1.fu"]<-1
all.data.fu$trial.num[all.data.fu$trial=="2.fu"]<-2
all.data.fu$trial.num[all.data.fu$trial=="3.fu"]<-3
all.data.fu$trial.num[all.data.fu$trial=="4.fu"]<-4
all.data.fu$trial.num[all.data.fu$trial=="5.fu"]<-5
all.data.fu$trial.num[all.data.fu$trial=="6.fu"]<-6
all.data.fu$trial.num[all.data.fu$trial=="7.fu"]<-7
all.data.fu$trial.num[all.data.fu$trial=="8.fu"]<-8
all.data.fu$trial.num[all.data.fu$trial=="9.fu"]<-9
all.data.fu$trial.num[all.data.fu$trial=="10.fu"]<-10
all.data.fu$trial<-all.data.fu$trial.num

###select relevant colums from fu file and merge
all.data.fu.sh<-dplyr::select(all.data.fu, c("ID", "trial", "invest.fu"))
#all.data<-merge(dat_bl_inv_long,all.data.fu.sh, by=c("ID", "trial") )#and merge baseline and fu data
all.data<-merge(dat_bl_inv_long,all.data.fu.sh, by=c("ID", "trial"), all.x=TRUE )#and merge baseline and fu data


###this now is in long format with respect to trials, but we also need it in long format with respect to time for investor's actions
##investor data
dat<-gather(all.data, time, invest.investor, c("invest", "invest.fu"), factor_key=TRUE)
dat$time.num[dat$time=="invest"]<-1
dat$time.num[dat$time=="invest.fu"]<-2
dat$time<-as.factor(dat$time.num)
colnames(dat)[colnames(dat)=="dum3$ELST1"] <- "ELST1"
datforplot<-DropNA(dat, Var = "age.fu")
  
p1 <- ggplot(data = dat[dat$time.num==1,], aes(x = trial, y = invest.investor, group=ID, colour=ID))
p1<-p1+ geom_line(alpha=0.02, size=0.02, colour="darkslateblue") + stat_summary(aes(group = 1), geom = "line", fun.y = mean , size = 1, colour="darkslateblue") + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 4, colour="darkslateblue")  + ylab ("Investments")+ggtitle ("Baseline")+theme(legend.position = "none") 
#p1+jtools::theme_apa()

p2 <- ggplot(data = dat[dat$time.num==2,], aes(x = trial, y = invest.investor, group=ID, colour=ID))
p2<-p2+ geom_line(alpha=0.02, size=0.02, colour="aquamarine3") + stat_summary(aes(group = 1), geom = "line", fun.y = mean , size = 1, colour="aquamarine3") + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 4, colour="aquamarine3")  + ylab ("Investments")+ggtitle ("Follow-up")+ theme(legend.position = "none") 


###this reproduces FIGURE 1B
ggplot(data=datforplot, aes(x = trial.n, y = invest.investor, group=ID, colour=ID))+
  jtools::theme_apa()+
   geom_line(data=datforplot[dat$time.num==1,] ,alpha=0.02, size=0.02, colour="aquamarine3") + stat_summary(data=dat[datforplot$time.num==1,],aes(group = 1, colour="aquamarine3"), geom = "line", fun.y = mean , size = 1) + stat_summary(data=datforplot[dat$time.num==1,],aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 1, colour="aquamarine3")  + ylab ("Investments")+
   geom_line(data=datforplot[dat$time.num==2,], alpha=0.02, size=0.02, colour="darkslateblue") + stat_summary(data=datforplot[dat$time.num==2,], aes(group = 1, colour="darkslateblue") , geom = "line", fun.y = mean , size = 1) + stat_summary(data=datforplot[dat$time.num==2,],aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 1, colour="darkslateblue")  + ylab ("Investments")+ggtitle ("Trial-by-trial Investments")+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), labels=c("1","2","3", "4", "5", "6", "7", "8", "9", "10"))+
  labs (x = "n") +
  theme_minimal() +
  theme_bw() +
  theme(
    plot.background   = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank())+
  theme(axis.line = element_line(color = 'black'))+
  theme(
    axis.title.x = element_text( size=16),
    axis.title.y = element_text( size=16),
    axis.text=element_text(size=12))+
   scale_color_manual( labels = c("baseline", "follow-up"), values=c("aquamarine3", "darkslateblue" ))+
   guides(color=guide_legend("")) 

###rebuild df

dat_res_sh<-dat

for (k in 1:dim(dat_res_sh)[1]) {
  dat_res_sh[k,'age']<-NA
  
  if (dat_res_sh[k,]$time==1) {
    dat_res_sh[k,'age']<-as.numeric(dat_res_sh[k,]$age.bl)

  } else if (dat_res_sh[k,]$time==2) {
    dat_res_sh[k,'age']<-as.numeric(dat_res_sh[k,]$age.fu)
  
  } else if (dat_res_sh[k,]$time=="NA") {
    dat_res_sh[k,'age']<-NA

    
  }
}



dat_res_sh<-DropNA(dat_res_sh, Var = c( "age.bl", "age", "sexFM", "age.fu", "invest.investor"))
dat_res_sh<-dat_res_sh[dat_res_sh$ID!=10629,] ##has only 1 measurement but age.fu
dat_res_sh$ID<-factor(dat_res_sh$ID)

##decompose age crosssectional/longitudinal
dat_res_sh$mean_sample_age<- mean(dat_res_sh$age, na.rm=T)

for (i in levels(dat_res_sh$ID)){
  dat_res_sh$mean_subj_age[dat_res_sh$ID==i] <- mean(dat_res_sh$age[dat_res_sh$ID==i], na.rm=T)
}
dat_res_sh$age[is.na(dat_res_sh$age.fu)]<-dat_res_sh$age.bl[is.na(dat_res_sh$age.fu)]
dat_res_sh$longage <- dat_res_sh$age - dat_res_sh$mean_subj_age
dat_res_sh$crossage <- dat_res_sh$mean_subj_age -dat_res_sh$mean_sample_age
dat_res_sh$trial<-factor(dat_res_sh$trial)





#########################################
##Investments throughout the whole game##
#########################################
#Behavioural Analyses: Longitudinal Development of Trust Behaviour 

###investments
afex_mo_1a<-mixed(invest.investor~longage*crossage+sexFM+trial+(1|ID), data=dat_res_sh, method="S")
nice(afex_mo_1a)
##get beta for manu revision
summary(afex_mo_1a$full_model)
tab2<-as.data.frame(nice(afex_mo_1a))

###for revision, include quadratic term of mean age as reviewer asked for quad effects
afex_mo_1b<-mixed(invest.investor~longage*crossage+I(crossage^2)+I(crossage^2):longage+sexFM+trial+(1|ID), data=dat_res_sh, method="S")
nice(afex_mo_1b)

##only trial one
dat_res_sh.tr1<-dat_res_sh[dat_res_sh$trial==1,]
afex_mo_1a.tr1<-mixed(invest.investor~longage*crossage+sexFM+(1|ID), data=dat_res_sh.tr1, method="S")
nice(afex_mo_1a.tr1)

##revision: add estimates as effect sizes
summary(afex_mo_1a)
summary(afex_mo_1a.tr1)
tab1<-as.data.frame(nice(afex_mo_1a.tr1))
tab1 ##S-Table 1

###add quadratic term as per reviewer's request
afex_mo_1b.tr1<-mixed(invest.investor~longage*crossage+I(crossage^2)+I(crossage^2):longage+sexFM+(1|ID), data=dat_res_sh.tr1, method="S")
nice(afex_mo_1b.tr1)

#####include baseline IQ as a control, need to mean centre everything again###
dat_res_sh.c<-NULL
dat_res_sh.c<-DropNA(dat_res_sh, Var = c( "IQ.bl"))
dat_res_sh.c$IQ_cent<-scale(dat_res_sh.c$IQ.bl, center = TRUE, scale = FALSE)
##age
dat_res_sh.c$mean_sample_age<- mean(dat_res_sh.c$age, na.rm=T)
for (i in levels(dat_res_sh.c$ID)){
  dat_res_sh.c$mean_subj_age[dat_res_sh.c$ID==i] <- mean(dat_res_sh.c$age[dat_res_sh.c$ID==i], na.rm=T)
}
dat_res_sh.c$age[is.na(dat_res_sh.c$age.fu)]<-dat_res_sh.c$age.bl[is.na(dat_res_sh.c$age.fu)]
dat_res_sh.c$longage <- dat_res_sh.c$age - dat_res_sh.c$mean_subj_age
dat_res_sh.c$crossage <- dat_res_sh.c$mean_subj_age-dat_res_sh.c$mean_sample_age


##control for IQ
afex_mo_1b<-mixed(invest.investor~longage*crossage+trial+IQ_cent+sexFM+(1|ID), data=dat_res_sh.c, method="S")
nice(afex_mo_1b)


dat_res_sh.c.tr1<-dat_res_sh.c[dat_res_sh.c$trial==1,]
afex_mo_1b.tr1<-mixed(invest.investor~longage*crossage+sexFM+IQ_cent+(1|ID), data=dat_res_sh.c.tr1, method="S")
nice(afex_mo_1b.tr1)

###make some plots for crosssectional and longitudinal effects 
require(jtools)
longplot<-interact_plot(afex_mo_1a$full_model, pred = "longage", modx = "crossage", interval=T, main.title = "Investment Behaviour", color.class = "Blues", x.label = "Longitudinal Age", y.label = "Investment", legend.main="Cross-sectional Age" ) 

longplot+
  theme_bw() +
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

dat_res_sh<-merge(dat_res_sh,meanInvestmentcombined, by="ID")


for (i in levels(dat_res_sh$ID)){
  dat_res_sh$mean.invest[dat_res_sh$ID==i&dat_res_sh$time==1] <- dat_res_sh$meanInvestment[dat_res_sh$ID==i&dat_res_sh$time==1]
  if (length (dat_res_sh$mean.invest[dat_res_sh$ID==i&dat_res_sh$time==3])+ length (dat_res_sh$meanInvestment[dat_res_sh$ID==i&dat_res_sh$time==1])  > 1) {
    dat_res_sh$mean.invest[dat_res_sh$ID==i&dat_res_sh$time==2] <- dat_res_sh$meanInvestment.fu[dat_res_sh$ID==i&dat_res_sh$time==2]
  }
}
dat_res_sh$crossage_cat4<-asfactor_cat4(dat_res_sh$crossage) 
datforplot<-dat_res_sh[dat_res_sh$trial==1,]


###change titles for the boxes: 
rlow<-(round(range(datforplot$mean_subj_age[datforplot$crossage_cat4=="low"])))
rmlow<-(round(range(datforplot$mean_subj_age[datforplot$crossage_cat4=="medium low"])))
rmhigh<-(round(range(datforplot$mean_subj_age[datforplot$crossage_cat4=="medium high"])))
rhigh<-(round(range(datforplot$mean_subj_age[datforplot$crossage_cat4=="high"])))

datforplot$crossage_cat4_num[datforplot$crossage_cat4=="low"]<-paste(toString(rlow[1]), '-', toString(rlow[2]), 'years', collapse = '')
datforplot$crossage_cat4_num[datforplot$crossage_cat4=="medium low"]<-paste(toString(rmlow[1]), '-', toString(rmlow[2]), 'years', collapse = '')
datforplot$crossage_cat4_num[datforplot$crossage_cat4=="medium high"]<-paste(toString(rmhigh[1]), '-', toString(rmhigh[2]), 'years', collapse = '')
datforplot$crossage_cat4_num[datforplot$crossage_cat4=="high"]<-paste(toString(rhigh[1]), '-', toString(rhigh[2]), 'years', collapse = '')

plot_res_split_long<- ggplot(datforplot, aes(x=longage, y=mean.invest, group=ID)) + geom_point(size=1,col="grey",colour=NULL) + geom_line(col="grey",colour=NULL)  +  theme(plot.title = element_text(size = 11, face = "bold")) + geom_smooth(aes(x=longage, y=mean.invest, colour=crossage_cat4_num, group=NULL), method='lm')+ theme_stata(scheme="s1mono") + scale_color_stata() + ylab("Mean Investment") + xlab("Longitudinal age (centered, years)") + labs(color="Mean Age") + facet_grid(~crossage_cat4_num) 
#plot_res_split<- ggplot(datforplot, aes(x=longage_cent, y=mean.invest, group=ID)) + geom_point(size=1,col="grey",colour=NULL) + geom_line(col="grey",colour=NULL)  +  theme(plot.title = element_text(size = 11, face = "bold")) + geom_smooth(aes(x=longage_cent, y=mean.invest, colour=ELST_cat4, group=NULL), method='lm')+ theme_stata(scheme="s1mono") + scale_color_stata() + ylab("investment") + xlab("Longitudinal age (centered, years)") + labs(color="Childhood Family Adversity") + facet_grid(~ELST_cat4) 
plot_res_split_long<-plot_res_split_long+theme_few()+theme( 
  axis.text=element_text(size=8),    axis.title.x = element_text( size=16),
  axis.title.y = element_text( size=16))


plot_res_split_long+
theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background   = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )+
  theme(axis.line = element_line(color = 'black'))+
  theme(
    # plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text( size=16),
    axis.title.y = element_text( size=16),
    axis.text=element_text(size=8)
  )

###this reproduces FIG 2
plot_res_split_long

###make a plot for sex effects (supplemental Fig)
###S-Figure 1 

afex_plot(afex_mo_1b, x = "sexFM")+
  labs(y = "Mean Investment", x = "")+
  theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background   = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )+
  theme(axis.line = element_line(color = 'black'))+
  theme(
    # plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text( size=16),
    axis.title.y = element_text( size=16),
    axis.text=element_text(size=8)
  )




##post-hoc control for IQ and poverty
#####include IQ as a control, need to mean centre everything again###
dat_res_sh.c<-NULL
dat_res_sh.c<-DropNA(dat_res_sh, Var = c( "povAft12.hpq1"))
dat_res_sh.c$IQ_cent<-scale(dat_res_sh.c$IQ.bl, center = TRUE, scale = FALSE)
dat_res_sh.c$pov_cent<-scale(dat_res_sh.c$povAft12.hpq1, center = TRUE, scale = FALSE)


##age
dat_res_sh.c$mean_sample_age<- mean(dat_res_sh.c$age, na.rm=T)
for (i in levels(dat_res_sh.c$ID)){
  dat_res_sh.c$mean_subj_age[dat_res_sh.c$ID==i] <- mean(dat_res_sh.c$age[dat_res_sh.c$ID==i], na.rm=T)
}
dat_res_sh.c$age[is.na(dat_res_sh.c$age.fu)]<-dat_res_sh.c$age.bl[is.na(dat_res_sh.c$age.fu)]
dat_res_sh.c$longage <- dat_res_sh.c$age - dat_res_sh.c$mean_subj_age
dat_res_sh.c$crossage <- dat_res_sh.c$mean_subj_age-dat_res_sh.c$mean_sample_age


##control for Poverty (Supplement)
##round-by-round
afex_mo_1c<-mixed(invest.investor~longage*crossage+pov_cent*IQ_cent+sexFM+trial+(1|ID), data=dat_res_sh.c, method="S")
nice(afex_mo_1c)

##trial 1
afex_mo_1d<-mixed(invest.investor~longage*crossage+pov_cent+sexFM+(1|ID), data=dat_res_sh.c[dat_res_sh.c$trial=="1",], method="S")
nice (afex_mo_1d)





##################################################
###family adversity related analysis########################
##need to mean-centre to effective sample#####
##################################################
###early life stress
dat_res_sh_incltrauma <- NULL
dat_res_sh_incltrauma<-DropNA(dat_res_sh, "ELST1") ##drop those that do not have the questionnaire data available
dat_res_sh_incltrauma$IQ_cent<-scale(dat_res_sh_incltrauma$IQ.bl, center = TRUE, scale = FALSE)
dat_res_sh_incltrauma$ELSIT_cent<-scale(dat_res_sh_incltrauma$ELST1, center = TRUE, scale = FALSE)
dat_res_sh_incltrauma$ID<-factor(dat_res_sh_incltrauma$ID)
dat_res_sh_incltrauma$mean_sample_age<- mean(dat_res_sh_incltrauma$age, na.rm=T)

for (i in levels(dat_res_sh_incltrauma$ID)){
  dat_res_sh_incltrauma$mean_subj_age[dat_res_sh_incltrauma$ID==i] <- mean(dat_res_sh_incltrauma$age[dat_res_sh_incltrauma$ID==i], na.rm=T)
}
dat_res_sh_incltrauma$age[is.na(dat_res_sh_incltrauma$age.fu)]<-dat_res_sh_incltrauma$mean_subj_age[is.na(dat_res_sh_incltrauma$age.fu)]
dat_res_sh_incltrauma$longage <- dat_res_sh_incltrauma$age - dat_res_sh_incltrauma$mean_subj_age
dat_res_sh_incltrauma$crossage <- dat_res_sh_incltrauma$mean_subj_age - dat_res_sh_incltrauma$mean_sample_age

###model with early adversity
afex_mo_1c<-mixed(invest.investor~longage*crossage*ELSIT_cent+trial+sexFM+(1|ID), data=dat_res_sh_incltrauma,  method="S")
nice(afex_mo_1c)
summary(afex_mo_1c)
tab3<-as.data.frame(nice(afex_mo_1c))

##only trial one
dat_res_sh_incltrauma.tr1<-dat_res_sh_incltrauma[dat_res_sh_incltrauma$trial==1,]
dat_res_sh_incltrauma.tr1.multiple<-dat_res_sh_incltrauma.tr1[dat_res_sh_incltrauma.tr1$longage!=0,]

afex_mo_1c.tr1<-mixed(invest.investor~longage*crossage*ELSIT_cent+sexFM+(1|ID), data=dat_res_sh_incltrauma.tr1, method="S")
nice(afex_mo_1c.tr1)
summary(afex_mo_1c.tr1)
##correct for IQ
afex_mo_1d<-mixed(invest.investor~longage*crossage*ELSIT_cent+trial+IQ_cent+sexFM+(1|ID), data=dat_res_sh_incltrauma,  method="S")
nice (afex_mo_1d)
#correct for IQ & poverty
afex_mo_1e<-mixed(invest.investor~longage*crossage*ELSIT_cent+trial+IQ_cent+povAft12.hpq1+sexFM+(1|ID), data=dat_res_sh_incltrauma,  method="S")

#correct for IQ & poverty
#afex_mo_1d<-mixed(invest.investor~longage*crossage*povAft12.hpq1+longage*crossage*ELSIT_cent+trial+IQ_cent+sexFM+(1|ID), data=dat_res_sh_incltrauma,  method="S")

##  longage:ELSIT_cent 1, 6597.00   9.17 **    .002
##  longage:povAft12.hpq1 1, 6597.00    2.86 +    .091

##trial 1 only
afex_mo_1d.tr1<-mixed(invest.investor~longage*crossage*ELSIT_cent+IQ_cent+sexFM+(1|ID), data=dat_res_sh_incltrauma.tr1, method="S")

nice(afex_mo_1d)
nice(afex_mo_1d.tr1)


###reviewer asks whether trauma effects hold after correcting for cfq


##trauma + peers analysis (revision: reviewer1 ########################
##need to mean-centre to effective sample#####
##################################################
###early life stress
dat_res_sh_traumfriends<-NULL
cfq_red<-cfq %>% select(c(nspnID,friendships_hpq1))
dat_res_sh_traumfriends<-merge(dat_res_sh_incltrauma, cfq_red, by.x="ID", by.y="nspnID",all.x=T)

dat_res_sh_traumfriends<-DropNA(dat_res_sh_traumfriends, "friendships_hpq1") ##drop those that do not have the questionnaire data available
dat_res_sh_traumfriends$ELSIT_cent<-scale(dat_res_sh_traumfriends$ELST1, center = TRUE, scale = FALSE)
dat_res_sh_traumfriends$friendships_hpq1_cent<-scale(dat_res_sh_traumfriends$friendships_hpq1, center = TRUE, scale = FALSE)
dat_res_sh_traumfriends$ID<-factor(dat_res_sh_traumfriends$ID)
dat_res_sh_traumfriends$mean_sample_age<- mean(dat_res_sh_traumfriends$age, na.rm=T)

for (i in levels(dat_res_sh_traumfriends$ID)){
  dat_res_sh_traumfriends$mean_subj_age[dat_res_sh_traumfriends$ID==i] <- mean(dat_res_sh_incltrauma$age[dat_res_sh_incltrauma$ID==i], na.rm=T)
}
dat_res_sh_traumfriends$age[is.na(dat_res_sh_traumfriends$age.fu)]<-dat_res_sh_traumfriends$mean_subj_age[is.na(dat_res_sh_traumfriends$age.fu)]
dat_res_sh_traumfriends$longage <- dat_res_sh_traumfriends$age -  dat_res_sh_traumfriends$mean_subj_age
dat_res_sh_traumfriends$crossage <- dat_res_sh_traumfriends$mean_subj_age - dat_res_sh_traumfriends$mean_sample_age
dat_res_sh_traumfriends$ELST_cat4<-asfactor_cat4 (dat_res_sh_traumfriends$ELSIT_cent)

afex_mo_1cinclfriendsa<-mixed(invest.investor~longage*crossage*ELSIT_cent+trial+sexFM+friendships_hpq1_cent+(1|ID), data=dat_res_sh_traumfriends,  method="S")
nice(afex_mo_1cinclfriendsa)

afex_mo_1cinclfriendsb<-mixed(invest.investor~longage*crossage*ELSIT_cent+trial+sexFM+friendships_hpq1_cent*longage+(1|ID), data=dat_res_sh_traumfriends,  method="S")
nice(afex_mo_1cinclfriendsb)
##longage:ELSIT_cent 1, 9600.00   7.93 **    .005

dat_res_sh_incltrauma$mean.invest<-NA
for (i in levels(dat_res_sh_incltrauma$ID)){
  dat_res_sh_incltrauma$mean.invest[dat_res_sh_incltrauma$ID==i&dat_res_sh_incltrauma$time==1] <- dat_res_sh_incltrauma$meanInvestment[dat_res_sh_incltrauma$ID==i&dat_res_sh_incltrauma$time==1]
  if (length (dat_res_sh_incltrauma$mean.invest[dat_res_sh_incltrauma$ID==i&dat_res_sh_incltrauma$time==3])+ length (dat_res_sh_incltrauma$meanInvestment[dat_res_sh_incltrauma$ID==i&dat_res_sh_incltrauma$time==1])  > 1) {
    dat_res_sh_incltrauma$mean.invest[dat_res_sh_incltrauma$ID==i&dat_res_sh_incltrauma$time==2] <- dat_res_sh_incltrauma$meanInvestment.fu[dat_res_sh_incltrauma$ID==i&dat_res_sh_incltrauma$time==2]
  }
}

##reproduce Fig 3 
dat_res_sh_incltrauma$ELST_cat4<-asfactor_cat4 (dat_res_sh_incltrauma$ELSIT_cent)
datforplot<-dat_res_sh_incltrauma[dat_res_sh_incltrauma$trial==1,]

plot_res_split<- ggplot(datforplot, aes(x=longage, y=mean.invest, group=ID)) + geom_point(size=1,col="grey",colour=NULL) + geom_line(col="grey",colour=NULL)  +  theme(plot.title = element_text(size = 11, face = "bold")) + geom_smooth(aes(x=longage, y=mean.invest, colour=ELST_cat4, group=NULL), method='lm')+ theme_stata(scheme="s1mono") + scale_color_stata() + ylab(" Mean Investment") + xlab("Longitudinal age (centered, years)") + labs(color="Childhood Family Adversity") + facet_grid(~ELST_cat4) 
plot_res_split<-plot_res_split+theme_few()+theme( 
  axis.text=element_text(size=6)
  )

plot_res_split

##end of analysis
###########################################################################################################################################

