################################################################################################
##script does analyses reported on crosssectional and longitudinal development on Irritability##
###it analyses their relationship with early adversity+plots it############
###########################################################################
rm(list = ls())

##load packages###
require(reshape)
library(jtools)
library(ggplot2)
library(reshape2)
library(DataCombine)
require(plyr)
require(ordinal)
require(afex)
require (ggplot2)
require (ggpubr)
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




####load data###

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
colnames(dat_fu)[colnames(dat_fu)=="IrrInd"] <- "IrrInd_fu"

eadv<-read.csv("ELST_vanHar.csv")



##now  merge all together for a dataset, that does only contain complete cases for T1 and T2
all.data<-merge(dat_bl, dat_fu, by="ID", all.x=TRUE) # try out all.x true
all.data<-merge(all.data, all.data.demo, by.x="ID", by.y="nspnID",all.x=T)
all.data<-merge(all.data, eadv, by.x="ID", by.y="subject", all.x=T)
all.data.bl<-merge(dat_bl, all.data.demo, by.x="ID", by.y="nspnID", all.x=T)


###throw out duplicate participant
subex <- "16261"
all.data<-subset(all.data, !(ID %in% subex))
###kick out depressed participants
all.data<-subset(all.data, all.data$study!="Depression")





#############################################################
######longitudinal analysis ###
#############################################################

all.data$COG_T1<-all.data$InvestorIrritability
all.data$COG_T2<-all.data$InvestorIrritability_fu

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
dat$ELST<-all.data$ELST1
dat$IrrInd<-as.factor(all.data$IrrInd)
dat<-as.data.frame(dat)


####melt into long format
dat_res <- reshape::melt(dat, id.vars=c(names(dat[,3:length(dat)])), variable_name = "COG_measure")

dat_res$time<-NA
dat_res$time[dat_res$COG_measure=="paramt1"]<-1
dat_res$time[dat_res$COG_measure=="paramt2"]<-2
dat_res$time<-factor(dat_res$time)
dat_res$subject<-factor(dat_res$subject)
dat_res$age.fu[dat_res$subject=="10629"]<-NA ##no modelling data for follow-up, so set age.fu to NA

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


##centre all continous vars
dat_res_sh<-DropNA(dat_res, c("age", "sex", "value", "age.bl", "age.fu")) ##get rid of all the params that are used as predcitors for mean-centering 

dat_res_sh$IQ_cent<-scale(dat_res_sh$IQ, center = TRUE, scale = FALSE)
dat_res_sh$mean_sample_age<- mean(dat_res_sh$age, na.rm=T)

for (i in levels(dat_res_sh$subject)){
  dat_res_sh$mean_subj_age[dat_res_sh$subject==i] <- mean(dat_res_sh$age[dat_res_sh$subject==i], na.rm=T)
}
dat_res_sh$age[is.na(dat_res_sh$age.fu)]<-dat_res_sh$age.bl[is.na(dat_res_sh$age.fu)]

dat_res_sh$longage <- dat_res_sh$age - dat_res_sh$mean_subj_age
dat_res_sh$crossage <- dat_res_sh$mean_subj_age -dat_res_sh$mean_sample_age

##########################
########stats##############
###########################

###run linear mixed effect models###
dat_res_sh<-droplevels(dat_res_sh)
afex_mo_1<-mixed(value~longage*crossage+sex+(1|subject), data=dat_res_sh)
nice(afex_mo_1)
###no effect of age or development per se

pl2<-ggplot(dat_res_sh, aes(x=longage, y=value)) +
  geom_smooth(method=lm, color="darkseagreen", size=3)+# Add linear regression line 
  geom_point() +    # Use hollow circles
  theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background   = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )+
  theme(axis.line = element_line(color = 'black'))+
  xlab("Longitudinal Age") +
  ylab("Irritability") +
  theme(
    # plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text( size=16),
    axis.title.y = element_text( size=16),
    axis.text=element_text(size=12)
  )




###take into account that data is ordinal, run ordinal model

dat_res_sh$value.o<-factor(dat_res_sh$value, ordered=TRUE)
dat_res_sh_complete<-dat_res_sh[dat_res_sh$longage!=0,]
dat_res_sh$ID<-as.numeric(dat_res_sh$subject)
  
clmm.max <- clmm(value.o ~ longage*crossage+(1|ID),link="cloglog",data=dat_res_sh )
summary(clmm.max)
##reproduces results of lmm

##############################
###family adversity analysis##
##############################
dat_res_traum<-DropNA(dat_res_sh, c("ELST")) ##get rid of all the params that are used as predcitors for mean-centering 
dat_res_traum$IQ_cent<-scale(dat_res_traum$IQ, center = TRUE, scale = FALSE)
dat_res_traum$mean_sample_age<- mean(dat_res_traum$age, na.rm=T)

for (i in levels(dat_res_traum$subject)){
  dat_res_traum$mean_subj_age[dat_res_traum$subject==i] <- mean(dat_res_traum$age[dat_res_traum$subject==i], na.rm=T)
}
dat_res_traum$age[is.na(dat_res_traum$age.fu)]<-dat_res_traum$age.bl[is.na(dat_res_traum$age.fu)]

dat_res_traum$longage <- dat_res_traum$age - dat_res_traum$mean_subj_age
dat_res_traum$crossage <- dat_res_traum$mean_subj_age -dat_res_traum$mean_sample_age


dat_res_traum$ELST_cent<-scale(dat_res_traum$ELST, center = TRUE, scale = FALSE)

##########################
########stats##############
###########################
#####early lifes stress/parenting analysis
###this is reported 
###metric analysis using afex
dat_res_traum<-droplevels(dat_res_traum)
afex_mo_1_traum<-mixed(value~longage*crossage*ELST_cent+sex+(1|subject), data=dat_res_traum, method = "S")
nice(afex_mo_1_traum)
###3                  ELST_cent 1, 506.00 7.92 **    .005
summary(afex_mo_1_traum)
### ELST_cent 1, 506.00 7.92 **    .005

###########################################
#########some controlanalyses##############
############################################

##control /sensitivity analysis: only those who have experienced unfair actions at all
all.data.indicator.subset <- dat_res_traum[dat_res_traum$IrrInd==1,]

afex_mo_1_traum.b<-mixed(value~longage*crossage*ELST_cent+sex+(1|subject), data=all.data.indicator.subset, method = "S")
nice(afex_mo_1_traum.b)
##effect is significant, too

####control /sensitivity analysis: dichotomize Irritablility 0 and > 0
dat_res_sh$value.bivar<-factor(dat_res_sh$value>0)
afex_mo_1_traum_binom<-mixed(value.bivar~longage*crossage*ELST+sex+(1|subject), data=dat_res_sh, family="binomial", method = "LRT")
nice(afex_mo_1_traum_binom)
##significant, too


########################################

###categorize for plots
dat_res_traum$ELSTcat<-asfactor_cat4 (dat_res_traum$ELST)
dat_res_traum$ELSTcat2<-dat_res_traum$ELSTcat

dat_res_traum$ELSTcat2[dat_res_traum$ELSTcat=="medium low"]<-"low"
dat_res_traum$ELSTcat2[dat_res_traum$ELSTcat=="medium high"]<-"high"



###########################################################################
#########Plot for ELST ~ Irritability Main Effect#####
###########################################################################

afex_mo_1_traumforplot<-mixed(value~longage*crossage*ELSTcat+sex+(1|subject), data=dat_res_traum, method = "S")
nice(afex_mo_1_traumforplot)

afex_plot(afex_mo_1_traumforplot, "ELSTcat", id = "subject",
          dodge = 0.7,
          data_geom = ggplot2::geom_violin,
          mapping = c("shape", "fill"),
          data_arg = list(width = 0.7)) +
  ylab("Irritability")+
  ggplot2::scale_shape_manual(values = c(24, 17, 4, 8)) +
  ggplot2::labs(title = "ID")+
    jtools::theme_apa()


averageirri<-afex_plot(afex_mo_1_traumforplot, x = "ELSTcat", trace = "ELSTcat", dodge = 0.7,
          #error_ci=TRUE,
                       mapping = c("shape", "fill"),
          data_geom = geom_boxplot,
          data_arg = list(
            width = 0.5,
            jitter.width = 0,
            jitter.height = 10,
            outlier.intersect = TRUE),
          point_arg = list(size = 2.5),
          line_arg = list(linetype = 0),
          error_arg = list(size = 1.5, width = 0))+ theme(legend.position = "none")+
  ylab("Irritability")+ xlab ("Family Adversity")+ ggtitle("Irritability as a Function of Family Adversity") +
  jtools::theme_apa(legend.use.title = FALSE) + labs(trace = "Degree of Family Adversity")
  
averageirri

###info wrt n for legend
unique(afex_mo_1_traumforplot$data$subject)


