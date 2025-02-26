##script does analyses reported on crosssectional and longitudinal development of risk aversion
###reported under "Computtaional Modeling "
###plots risk aversion development (not shown in manuscript)

rm(list = ls())

##load packages###
require(reshape)
library(jtools)
require(interactions)
library(lavaan)
library(ggplot2)
library(reshape2)
library(DataCombine)
require("plyr")
require(ordinal)
require(afex)

#############################################
# function to categorise continuous variable (for plotting)
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
  data$cat_var[data$var > var_quant25 & data$var <= var_quant50] <- "medium_low"
  data$cat_var[data$var > var_quant50 & data$var <= var_quant75] <- "medium_high"
  data$cat_var[data$var > var_quant75] <- "high"
  data$cat_var <- factor(data$cat_var, levels=c("low","medium_low","medium_high","high"))
  return(data$cat_var)
}
##########



####load data&data prep###

##age###
setwd('C:/Users/reiter/Nextcloud/Trust_analysis/dataforupload/')
all.data.demo<-read.csv('all.data.demo.csv')
all.data.demo<-all.data.demo[,1:27] #only use demo data

##modelling params###
dat_bl<-read.csv("BaseParametersNew.csv", sep=";")
dat_fu<-read.csv("RecallParametersNew.csv", sep=";")

colnames(dat_fu)[colnames(dat_fu)=="InvestorToM"] <- "InvestorToM_fu"
colnames(dat_fu)[colnames(dat_fu)=="InvestorPlan"] <- "InvestorPlan_fu"
colnames(dat_fu)[colnames(dat_fu)=="InvestorAversion"] <- "InvestorAversion_fu"
colnames(dat_fu)[colnames(dat_fu)=="InvestorGuilt"] <- "InvestorGuilt_fu"
colnames(dat_fu)[colnames(dat_fu)=="InvestorTemp"] <- "InvestorTemp_fu"
colnames(dat_fu)[colnames(dat_fu)=="InvestorIrritability"] <- "InvestorIrritability_fu"
colnames(dat_fu)[colnames(dat_fu)=="InvestorIrrBelief"] <- "InvestorIrrBelief_fu"




##now finally merge all together for a dataset, that does only contain complete cases for T1 and T2
all.data<-merge(dat_bl, dat_fu, by="ID", all=TRUE)
all.data<-merge(all.data, all.data.demo, by.x="ID", by.y="nspnID",all.x=T)
all.data.bl<-merge(dat_bl, all.data.demo, by.x="ID", by.y="nspnID", all.x=T)


###throw out duplicate participant
subex <- "16261"
all.data<-subset(all.data, !(ID %in% subex))
###kick out depression cohort
all.data<-subset(all.data, all.data$study!="Depression")
all.data.bl<-subset(all.data.bl, all.data.bl$study!="Depression")

#######################################################################
###start analyses: first, run some correlation with age at baseline####
########################################################################
###t1 so uses all.data.bl
cor.test(all.data.bl$age.bl, all.data.bl$InvestorAversion, method="spearman")


summary( lm(InvestorAversion~age.bl+sexFM, data = all.data.bl))
##control analysis
summary( lm(InvestorAversion~age.bl+sexFM+IQ.bl, data = all.data.bl))

###it also holds if corrected for IQ and sex


##check after correcting for other params in multiple regression model with all params as predictors
summary(lm(age.bl~InvestorAversion+InvestorIrritability+InvestorIrrBelief+InvestorTemp+InvestorPlan+InvestorToM+IQ.bl+sexFM, data=all.data.bl))

agemodelparams<-lm(age.bl~InvestorAversion+InvestorIrritability+InvestorIrrBelief+InvestorTemp+InvestorPlan+InvestorToM+IQ.bl+sexFM, data=all.data.bl)


plot_summs(agemodelparams)


################################################################
##the only one that survives bonf correction is risk aversion!##
############let's plot it########################################
################################################################



##categorise age group
all.data.bl$age.cat<- asfactor_cat4(all.data.bl$age.bl)

ggplot(all.data.bl, 
       aes(x = age.cat, 
           y = InvestorAversion)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .2, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 2) + 
  labs(title = "Social Risk Aversion by Age Group")



### interesting to look at the correlation with pay-out - is age-related decrease in social risk-aversion  adaptive?
##load pay-out data for baseline
keyraw<-read.csv("bslKeyTrust_201809131519.csv")
all.data.bl.c<-merge(all.data.bl, keyraw, by.y="pt_ID", by.x="ID")

cor.test(all.data.bl.c$InvestorAversion, all.data.bl.c$invTotAssets)




##plot it (S Figure 2)
aversion.gain.plot<-ggplot(all.data.bl.c, aes(y=invTotAssets, x=InvestorAversion)) +
  geom_point() +    # Use hollow circles
  geom_smooth(method=lm, color="darkseagreen", size=3)+# Add linear regression line 
  theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background   = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )+
  theme(axis.line = element_line(color = 'black'))+
  ylab("Total investor's winnings") +
  xlab("Social Risk Aversion") +
  theme(
    # plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text( size=16),
    axis.title.y = element_text( size=16),
    axis.text=element_text(size=12)
  )

aversion.gain.plot

###can we correct it for tendency to gamble as a more conventional risk-taking measure?
dat_bl.risk<-read.csv("C:/Users/reiter/Nextcloud/Trust_analysis/Economic_preferences_data_from_baseline/GamblingTask.csv")
all.data.bl.c<-merge(all.data.bl, dat_bl.risk, by.y="Subject.ID", by.x="ID")

cor.test(all.data.bl.c$InvestorAversion, all.data.bl.c$Beta0, method="spearman")

summary( lm(InvestorAversion~age.bl+sexFM+Beta0, data = all.data.bl.c))

##plot correlation of %gambling with social risk aversion 
###S-Fig 4
aversion.age.plot<-ggplot(all.data.bl.c, aes(x=age.bl, y=InvestorAversion)) +
  geom_point() +    # Use hollow circles
  geom_smooth(method=lm, color="darkseagreen", size=3)+# Add linear regression line 
  theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background   = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )+
  theme(axis.line = element_line(color = 'black'))+
  xlab("Age") +
  ylab("% Gambling (non-social Risk Task)") +
  theme(
    # plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text( size=16),
    axis.title.y = element_text( size=16),
    axis.text=element_text(size=12)
  )

aversion.age.plot


#############################################################
################longitudinal analysis #######################
#############################################################

keyraw_fu<-read.csv("MRITTdescriptives4AR.csv")
all.data<-merge(all.data, keyraw_fu, by.y="nspnID", by.x="ID")

cor.test(all.data$InvestorAversion_fu, all.data$invTotAssets.fu2)
##plot it
aversion.gain.plot.fu<-ggplot(all.data, aes(y=invTotAssets.fu2, x=InvestorAversion_fu)) +
  geom_point() +    # Use hollow circles
  geom_smooth(method=lm, color="darkseagreen", size=3)+# Add linear regression line 
 # geom_jitter()
  theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background   = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )+
  theme(axis.line = element_line(color = 'black'))+
  ylab("Total investor's winnings") +
  xlab("Social Risk Aversion") +
  theme(
    # plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text( size=16),
    axis.title.y = element_text( size=16),
    axis.text=element_text(size=12)
  )+
  ggtitle("Follow Up")
aversion.gain.plot.fu


all.data$COG_T1<-as.numeric(all.data$InvestorAversion)
all.data$COG_T2<-as.numeric(all.data$InvestorAversion_fu)


###all the remaining ones
all.data<-DropNA(all.data, c( "COG_T1"), message = TRUE)

##################################################
##########end of data organization################
##################################################

###################################
###pre-processing for mixed model##
###################################

dat<-NULL
dat$paramt1<-all.data$COG_T1
dat$paramt2<-all.data$COG_T2
dat$subject<-all.data$ID
dat$age.bl<-all.data$age.bl
dat$age.fu<-all.data$age.fu
dat$sex<-factor(all.data$sexFM)
dat$IQ<-all.data$IQ.bl
dat<-as.data.frame(dat)


####melt into long format
dat_res <- reshape::melt(dat, id.vars=c(names(dat[,3:length(dat)])), variable_name = "COG_measure")

dat_res$time<-NA
dat_res$time[dat_res$COG_measure=="paramt1"]<-1
dat_res$time[dat_res$COG_measure=="paramt2"]<-2
dat_res$time<-factor(dat_res$time)

rm(k)
for (k in 1:dim(dat_res)[1]) {
  dat_res[k,'age']<-NA
  
  if (dat_res[k,]$time==1) {
    dat_res[k,'age']<-as.numeric(dat_res[k,]$age.bl)
    
  } else if (dat_res[k,]$time==2) {
    dat_res[k,'age']<-as.numeric(dat_res[k,]$age.fu)
    
  } else if (dat_res[k,]$time=="NA") {
    dat_res[k,'age']<-NA
    
  }
}

rm(k)


###mean-centre all continous vars
dat_res_sh<-DropNA(dat_res, c("age", "sex", "value", "age.bl", "age.fu")) ##get rid of all the params that are used as predcitors for mean-centering 
dat_res_sh$IQ_cent<-scale(dat_res_sh$IQ, center = TRUE, scale = FALSE)
dat_res_sh$subject<-as.factor(dat_res_sh$subject)
##age
dat_res_sh$mean_sample_age<- mean(dat_res_sh$age, na.rm=T)
for (i in levels(dat_res_sh$subject)){
  dat_res_sh$mean_subj_age[dat_res_sh$subject==i] <- mean(dat_res_sh$age[dat_res_sh$subject==i], na.rm=T)
}
dat_res_sh$age[is.na(dat_res_sh$age.fu)]<-dat_res_sh$age.bl[is.na(dat_res_sh$age.fu)]
dat_res_sh$longage <- dat_res_sh$age - dat_res_sh$mean_subj_age
dat_res_sh$crossage <- dat_res_sh$mean_subj_age-dat_res_sh$mean_sample_age


##########################
########stats##############
###########################

###run linear mixed effect models###

afex_mo_1<-mixed(value~longage*crossage+sex+(1|subject), data=dat_res_sh)
nice(afex_mo_1)
##get estimates for revision
summary(afex_mo_1)

pl2<-ggplot(dat_res_sh, aes(x=longage, y=value)) +
  geom_point() +    # Use hollow circles
  geom_smooth(method=lm, color="darkseagreen", size=3)+# Add linear regression line 
  geom_jitter()+
  theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background   = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )+
  theme(axis.line = element_line(color = 'black'))+
  xlab("Longitudinal Age") +
  ylab("Risk Aversion") +
  theme(
    # plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text( size=16),
    axis.title.y = element_text( size=16),
    axis.text=element_text(size=12)
  )

pl2



dat_res_sh$crossage_cat4<-asfactor_cat4(dat_res_sh$crossage)

plot_res_split_long<- ggplot(dat_res_sh, aes(x=longage, y=value, group=subject)) + 
  geom_point() + geom_line(col="grey",colour=NULL) + 
  theme(plot.title = element_text(size = 11, face = "bold")) + geom_smooth(aes(x=longage, y=value, group=NULL), method='lm', color="darkseagreen", size=3)   +
theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background   = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )+
  theme(axis.line = element_line(color = 'black'))+
  xlab("Longitudinal Age") +
  ylab("Risk Aversion") +
  theme(
    # plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text( size=16),
    axis.title.y = element_text( size=16),
    axis.text=element_text(size=12)
  )

plot_res_split_long<-plot_res_split_long+theme_few()+theme( 
  axis.text=element_text(size=6)
)
plot_res_split_long




###control analysis: 
#take into account that data is ordinal

dat_res_sh$value.o<-factor(dat_res_sh$value, ordered=TRUE)

clmm.max <- clmm2(value.o ~ longage*crossage+sex+(1|subject),data=dat_res_sh)
summary(clmm.max)
##reproduces results of lin mixed model


