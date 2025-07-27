# paskages ----------------------------------------------------------------

# devtools::install_github("cardiomoon/autoReg")
# 
# install.packages("openxlsx")
# install.packages("ROCR")
# install.packages("rmda")
# install.packages("rrtable")
# install.packages("DescTools")
# install.packages("table1")
# require(devtools)
# devtools::install_github("benjaminrich/table1")
# install.packages("reportROC")
# install.packages("mlr3verse")
# install.packages("forestploter")
# install.packages(c('fields', 'R2HTML'))
# install.packages("devtools")
# install.packages("dcurves")
# install.packages("gtsummary")

setwd("")
library(autoReg)
library(survival)
library(readxl)
library(openxlsx)
library(tidyverse)
library(conflicted)
library(dplyr)
library(rms)
library(caret)
library(pROC)
library(rrtable)
library(table1)
library(reportROC)
library(ROCR)
library(mlr3verse)
library(mlr3)
library(VRPM)
library(Rcpp) 
library(rmda)
library(tidyr)
library(riskRegression)
library(dcurves)
library(gtsummary)
library(forestploter)
# if(!require('pacman')) {
#   install.packages('pacman')
#   library('pacman')
# }
packages= c(
  # 'xlsx',
  'autoReg','rrtable','cutpointr','pROC','ROCR','reshape2','cowplot','htmlTable','gtools','dplyr','plotly','ggplot2')

lapply(packages, library, character.only = TRUE)


# data --------------------------------------------------------------------

dfbaseline<- read_excel("baseline.xlsx")
dfbaseline <- dfbaseline[,-c(11,12,14)]
dfbaseline$institution <- as.factor(dfbaseline$institution)
dfbaseline[, c(4:17)] <- lapply(dfbaseline[, c(4:17)], factor)
dfbase=dfbaseline[, -which(colnames(dfbaseline) == 'name')]
#baseline(label+institution~.,data=)
table2 = gaze(institution~.,data=dfbase) %>%
  myft()
table2docx(table2,target="")

radscore = "radscore.csv"
radscorefy="radscorevali.csv"
radscoretcga="radscorevali2.csv"

radscore  = read.csv(radscore,row.names = 1)
radscorefy  = read.csv(radscorefy,row.names = 1)
radscoretcga  = read.csv(radscoretcga,row.names = 1)

radscore <- radscore[, -which(colnames(radscore) == 'label')]
radscorefy <- radscorefy[, -which(colnames(radscorefy) == 'label')]
radscoretcga <- radscoretcga[, -which(colnames(radscoretcga) == 'label')]

colnames(radscorefy)[2] = 'name'
colnames(radscoretcga)[2] = 'name'

datam = merge(radscore,dfbaseline[,"institution"=1], by = "name")
datamfy = merge(radscorefy,dfbaseline[,"institution"=2],  by = "name")
datamtcga = merge(radscoretcga,dfbaseline[,"institution"=3],  by = "name")

score =  datam$radscore
score = score*100
df = datam
df$radscore = score

scorefy =  datamfy$radscore
scorefy = scorefy*100
dffy = datamfy
dffy$radscore = scorefy

scoretcga =  datamtcga$radscore
scoretcga = scoretcga*100
dftcga = datamtcga
dftcga$radscore = scoretcga
#logistic -------------------------------------------------------
df$label = as.numeric(df$label) 
fit1 <- glm(label~.,data=df,family = 'binomial') 
summary(fit1)
gaze(fit1)

table3 = autoReg(fit1,uni=T,multi=T,final=F
                 ,threshold=0.05
)

#forest plot
tableforest <- table3[table3$desc!=0,]

OR_ <- tableforest$`OR (univariable)`
OR <- c()
OR.low <- c()
OR.up <- c()
for (i in OR_){
  
  split_comma <- strsplit(i, ",")[[1]]
  OR_part <- trimws(split_comma[1])  
  
  
  split_parenthesis <- strsplit(OR_part, "\\(")[[1]]
  OR <- c(trimws(split_parenthesis[1]),OR) 
  
  
  split_dash <- strsplit(split_parenthesis[2], "-")[[1]]
  OR.low <- c(trimws(split_dash[1]),OR.low)  
  OR.up <- c(trimws(split_dash[2]),OR.up)
}

tableforest$OR <- rev(OR)
tableforest$OR.low <- rev(OR.low)
tableforest$OR.up <- rev(OR.up)
tableforest$'' <- paste(rep(" ", 14), collapse = " ")
tableforest[, c(8:10)] <- lapply(tableforest[, c(8:10)], as.numeric)
tableforest <- rbind(tableforest,tableforest[1,])
tableforest <- tableforest[-1,]
colnames(tableforest)[5] <- 'Characteristic'

tm <- forest_theme(base_size = 10,
                   ci_pch = 16,
                   ci_col = "#4575b4", 
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, 
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "black")




p <- forest(data = tableforest[,c(5,
                                  # 3,
                                  # 4,
                                  11,6)], 
            
            
            lower = tableforest$OR.low, 
            upper = tableforest$OR.up, 
            est = tableforest$OR,
            ci_column = 2,
            ref_line = 1, 
            xlim = c(0,20), 
            # ticks_at = c(0,log10(1),log10(2),log10(3),log10(4),log10(5)), 
            # arrow_lab = c("","OR 95%CI"), 
            footnote = "OR of MTM&VETC",
            
            theme = tm
)

final_mod_ = lrm(label~AFP100+radscore,
                 data = df,x=TRUE,y=TRUE)

dffy_ <- dffy[,c(2,8,16)]
dftcga_ <- dftcga[,c(2,8,16)]

predsdyy <- predict(final_mod, newdata = df, type = "response")
predfy <- predict(final_mod, newdata = dffy, type = "response")
predtcga <- predict(final_mod, newdata = dftcga, type = "response")

tcgapreditior = cbind(dftcga$name,dftcga$label,dftcga$radscore/100,predtcga)
tcgapreditior = as.data.frame(tcgapreditior)
write.csv(tcgapreditior, "")


reportROCsdyy=reportROC(gold=df_$label,predictor=df_$radscore,important="se", plot=TRUE)
reportROCfy=reportROC(gold=dffy_$label,predictor=dffy_$radscore,important="se", plot=TRUE)
reportROCtcga=reportROC(gold=dftcga_$label,predictor=dftcga_$radscore,important="se", plot=TRUE)

COMreportROCsdyy=reportROC(gold=df_$label,predictor=predsdyy,important="se", plot=TRUE)
COMreportROCfy=reportROC(gold=dffy_$label,predictor=predfy,important="se", plot=TRUE)
COMreportROCtcga=reportROC(gold=dftcga_$label,predictor=predtcga,important="se", plot=TRUE)

modelevaluation = rbind(reportROCsdyy,reportROCfy,reportROCtcga,COMreportROCsdyy,COMreportROCfy,COMreportROCtcga)

write.csv(modelevaluation, "")

datasdyy <- read_excel("Li-Rads.xlsx", sheet = 1)
datafy <- read_excel("Li-Rads.xlsx", sheet = 2)
datatcga <- read_excel("Li-Rads.xlsx", sheet = 3)

datasdyy_LIRADS <- as.data.frame(datasdyy)
datafy_LIRADS <- as.data.frame(datafy)
datatcga_LIRADS <- as.data.frame(datatcga)

new_order_sdyy <- match(df$name, datasdyy_LIRADS$ID)
datasdyy_LIRADS <- datasdyy_LIRADS[new_order_sdyy, ]

new_order_fy <- match(dffy$name, datafy_LIRADS$ID)
datafy_LIRADS <- datafy_LIRADS[new_order_fy, ]

new_order_tcga <- match(dftcga$name, datatcga_LIRADS$ID)
datatcga_LIRADS <- datatcga_LIRADS[new_order_tcga, ]


datasdyy_LIRADS[, 2:4] <- lapply(datasdyy_LIRADS[, 2:4], as.factor)
datafy_LIRADS[, 2:4] <- lapply(datafy_LIRADS[, 2:4], as.factor)
datatcga_LIRADS[, 2:4] <- lapply(datatcga_LIRADS[, 2:4], as.factor)

mod = lrm(label~ Lirads
          ,data = datasdyy_LIRADS,x=TRUE,y=TRUE)

mod_ = lrm(label~ Lirads
           +AFP 
           ,data = datasdyy_LIRADS,x=TRUE,y=TRUE)

predsdyy_LIRADS <- predict(mod, newdata = datasdyy_LIRADS, type = "fitted")
predfy_LIRADS <- predict(mod, newdata = datafy_LIRADS, type = "fitted")
predtcga_LIRADS <- predict(mod, newdata = datatcga_LIRADS, type = "fitted")

predsdyy_LIRADS_ <- predict(mod_, newdata = datasdyy_LIRADS, type = "fitted")
predfy_LIRADS_ <- predict(mod_, newdata = datafy_LIRADS, type = "fitted")
predtcga_LIRADS_ <- predict(mod_, newdata = datatcga_LIRADS, type = "fitted")

sdyy_LIRADS_roc = roc(as.vector(datasdyy_LIRADS$label),as.vector(predsdyy_LIRADS))     
sdyy_LIRADS_roc_ = roc(as.vector(datasdyy_LIRADS$label),as.vector(predsdyy_LIRADS_)) 

fy_LIRADS_roc = roc(as.vector(datafy_LIRADS$label),as.vector(predfy_LIRADS))     
fy_LIRADS_roc_ = roc(as.vector(datafy_LIRADS$label),as.vector(predfy_LIRADS_)) 

tcga_LIRADS_roc = roc(as.vector(datatcga_LIRADS$label),as.vector(predtcga_LIRADS))     
tcga_LIRADS_roc_ = roc(as.vector(datatcga_LIRADS$label),as.vector(predtcga_LIRADS_)) 

sdyy_rad_roc = roc(as.vector(datam$label),as.vector(datam$radscore))
fy_rad_roc = roc(as.vector(datamfy$label),as.vector(datamfy$radscore))
tcga_rad_roc = roc(as.vector(datamtcga$label),as.vector(datamtcga$radscore))
sdyy_com_roc <- roc(as.vector(df_$label),as.vector(predsdyy))
# sdyyroctest = roc.test(sdyy_rad_roc, sdyy_com_roc)
fy_com_roc <- roc(as.vector(dffy$label),as.vector(predfy))
tcga_com_roc <- roc(as.vector(dftcga$label),as.vector(predtcga))
# auc(tcga_com_roc)
# ci(tcga_com_roc)

#Delong
roc.test(sdyy_LIRADS_roc,sdyy_rad_roc,method = "delong")
roc.test(fy_LIRADS_roc,fy_rad_roc,method = "delong")
roc.test(tcga_LIRADS_roc,tcga_rad_roc,method = "delong")

roc.test(sdyy_LIRADS_roc_,sdyy_com_roc,method = "delong")
roc.test(fy_LIRADS_roc_,fy_com_roc,method = "delong")
roc.test(tcga_LIRADS_roc_,tcga_com_roc,method = "delong")

value_ = roc.test(sdyy_LIRADS_roc,sdyy_rad_roc,method = "delong")
value_$p.value

#Youden
library(pROC)
best_threshold_sdyy <- coords(sdyy_com_roc, "best", best.method = "youden")$threshold
best_threshold_tcga <- coords(tcga_com_roc, "best", best.method = "youden")$threshold
best_threshold_fy <- coords(fy_com_roc, "best", best.method = "youden")$threshold

# ROC plot
windowsFonts(myFont = windowsFont("Time New Roman"),
             KT = windowsFont("楷体"),
             ST = windowsFont("宋体"))
png("ROC of NEWest_2.png",units = "in",width = 10,height = 10,res = 400)
ggroc(list("LIRADS model" = fy_LIRADS_roc,
           "Clinical-LIRADS model" = fy_LIRADS_roc_,
           "Radiomics model" = fy_rad_roc,
           "Clinical-radiomics model" = fy_com_roc),
      size = 2, legacy.axes = TRUE)+
  scale_color_manual(values = c('#E87B1E','#018b38',"red2","blue3"))+   #  '#D9a421','#018b38'
  theme_bw()+ggtitle("ROC curve") +
  scale_x_continuous(expand = c(0.02,0))+
  scale_y_continuous(expand = c(0.02,0))+
  xlab("1-Specificity")+ylab("Sensitivity")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_segment(aes(x= 0,xend =1, y= 0, yend = 1),color="gray" 
               ,linetype=6,linewidth=2                      #需要更改
  )+                                
  theme(panel.grid.major = element_line(linewidth = 2))+
  theme(axis.text.x = element_text(size =26 ,face = "bold",color = "black",family = "myFont"))+
  theme(axis.text.y = element_text(size =26 ,face = "bold",color = "black",family = "myFont"))+
  theme(axis.title.x = element_text(size =26 ,face = "bold",color = "black",family = "myFont"))+
  theme(axis.title.y = element_text(size =26 ,face = "bold",color = "black",family = "myFont"))+
  theme(legend.text = element_text(size = 25,face = "bold",family = "myFont"))+
  theme(plot.title = element_text(size = 26,face = "bold",family = "myFont"))+
  theme(legend.title = element_blank())+
  theme(legend.key = element_rect(fill = "transparent"))+
  theme(legend.position = c(0.7,0.14))+
  theme(legend.key.width=unit(2,"cm"))+
  theme(plot.margin = unit(rep(1.5,4),"lines"))

dev.off()

dd<-datadist(df_)
options(datadist='dd') 

#nomogram
# par(mgp=c(1.6,0.6,0),mar=c(2,2,2,2),omd=c(0.1, 0.9, 0.1, 0.9))  
nom <- nomogram(final_mod_,
                fun = plogis,
                # fun.at = c(0.1,seq(0.2,0.8,by=0.1),0.9),
                funlabel="Risk for MTM&VETC",
                lp=F)
# plot(nom)
plot(nom, 
     # lmgp=0.3,
     # naxes=5,
     # cex.axis=0.8,
     # xfrac=.35, 
     col.grid=c("darkcyan","grey"))

#calibration curve
dd <- datadist(df)
options(datadist = "dd")

dd2 <- datadist(dffy)
options(datadist = "dd2")

dftcga_ <- dftcga[,-c(6,9,10)]
dd3 <- datadist(dftcga_)
options(datadist = "dd3")

final_mod_1 = lrm(label~
                    AFP100+
                    radscore,
                  data = df,x=TRUE,y=TRUE)

predsdyy_ <- predict(final_mod_1, newdata = df_, type = "fitted")
predfy_ <- predict(final_mod_1, newdata = dffy, type = "fitted")
predtcga_ <- predict(final_mod_1, newdata = dftcga, type = "fitted")

predy1 <- sort(predsdyy_ )
predy2 <- sort(predfy_)
predy3 <- sort(predtcga_)

smo1 <- lowess(plogis(final_mod_1$linear.predictors), as.numeric(final_mod_1$y), iter = 1000)
smo2 <- lowess(plogis(final_mod_2$linear.predictors), as.numeric(final_mod_2$y), iter = 500)
smo3 <- lowess(plogis(final_mod_3$linear.predictors), as.numeric(final_mod_3$y), iter = 100)

calibrated.orig1 <- approx(smo1, xout = predy1, ties = function(x) x[1])$y
calibrated.orig2 <- approx(smo2, xout = predy2, ties = function(x) x[1])$y
calibrated.orig3 <- approx(smo3, xout = predy3, ties = function(x) x[1])$y

plot(predy1, calibrated.orig1, type = "l", lty=1, xlim = c(0,1), ylim = c(0,1), col= "red2",lwd=3, xlab = "Predicted probability", ylab = "Actual risk of VETC&MTM subtype", cex.lab = 1)
lines(predy2, calibrated.orig2, type = "l", lty=1, xlim = c(0,1), ylim = c(0,1), col= "blue3",lwd=3, xlab = "Predicted probability", ylab = "Actual risk of VETC&MTM subtype", cex.lab = 1)
lines(predy3, calibrated.orig3, type = "l", lty=1, xlim = c(0,1), ylim = c(0,1), col= "green3",lwd=3, xlab = "Predicted probability", ylab = "Actual risk of VETC&MTM subtype", cex.lab = 1)
abline(0, 1, lty = 2,lwd=2,col="grey")
legend <- list(x=0 + .55*diff(c(0,1)), y=0 + .32*diff(c(0,1)))
legend(x = "bottomright", 
       legend, c("Training set", 
                 "Validation set1", 
                 "Validation set2",
                 "Ideal"), 
       text.col = c("red2","blue3","green3","grey"),
       cex = 0.8,
       col=c("red2","blue3","green3","grey"),  
       lwd=2,
       lty=c(1,1,1,2), bty="n")
title("Calibration Curve")

#Decision curve

datatcga_LIRADS$label <- as.numeric(datatcga_LIRADS$label)-1
datatcga_LIRADS$AFP <- as.numeric(datatcga_LIRADS$AFP)-1
datatcga_LIRADS$Lirads <- as.numeric(datatcga_LIRADS$Lirads)-1

dftcga$label <- as.numeric(dftcga$label)
dftcga$AFP100 <- as.numeric(dftcga$AFP100)



dc1 <- decision_curve(label~radscore,
                      data=dftcga,family = "binomial")
dc2 <- decision_curve(label~radscore+AFP100,
                      data=dftcga,family = "binomial")
dc3 <- decision_curve(label~Lirads,
                      data=datatcga_LIRADS,family = "binomial")
dc4 <- decision_curve(label~Lirads+AFP,
                      data=datatcga_LIRADS,family = "binomial")




plot_decision_curve(list(dc3,dc4,dc1,dc2),
                    curve.names = c("LIRADS model","Clinical-LIRADS model","Radiomics model",'Clinical-radiomics model'),
                    col = c('#E87B1E','#018b38',"red2","blue3"),
                    lty = c(1, 1, 1, 1),
                    lwd = c(2, 2, 2, 2),
                    confidence.intervals = F
                    # ,cex.axis = 1.2
)


df$label = as.factor(df$label)
modeleval = cbind(predsdyy_LIRADS,predsdyy_LIRADS_,df$radscore,predsdyy,df$label)
modeleval = as.data.frame(modeleval)
colnames(modeleval)=c("LIRADS model","Clinical-LIRADS model","radiomics_model","clinical_radiomics_model","label")
modeleval$radiomics_model = modeleval$radiomics_model/100
# modeleval$label = as.factor(modeleval$label)
modeleval$group = 0

dffy$label = as.factor(dffy$label)
modelevalfy = cbind(predfy_LIRADS,predfy_LIRADS_,dffy$radscore,predfy,dffy$label)
# modelevalfy[,c(1:5)] = as.numeric(modelevalfy[,c(1:5)])
modelevalfy = as.data.frame(modelevalfy)
colnames(modelevalfy)=c("LIRADS model","Clinical-LIRADS model","radiomics_model","clinical_radiomics_model","label")
modelevalfy$radiomics_model = modelevalfy$radiomics_model/100
# modelevalfy$label = as.factor(modelevalfy$label)
modelevalfy$group = 1

dftcga$label = as.factor(dftcga$label)
modelevaltcga = cbind(predtcga_LIRADS,predtcga_LIRADS_,dftcga$radscore,predtcga,dftcga$label)
modelevaltcga = as.data.frame(modelevaltcga)
colnames(modelevaltcga)=c("LIRADS model","Clinical-LIRADS model","radiomics_model","clinical_radiomics_model","label")
modelevaltcga$radiomics_model = as.numeric(modelevaltcga$radiomics_model)/100
# modelevaltcga$label = modelevaltcga$label+1
# modelevaltcga$label = as.factor(modelevaltcga$label)
modelevaltcga$group = 2


commodeleval = rbind(modeleval,modelevalfy,modelevaltcga)
commodeleval$label <- as.factor(commodeleval$label)
commodeleval$group <- as.factor(commodeleval$group)
list_datas=c("LIRADS model","Clinical-LIRADS model","radiomics_model","clinical_radiomics_model")


cutpoint_x <- function(x,data){  
  ob = cutpointr(x = x, 
                 class = label,
                 data = data,
                 subgroup = group,  
                 method = maximize_metric, 
                 metric=youden,
                 pos_class = 2, 
                 direction = ">=",boot_runs=100) %>% add_metric(list(F1_score))
  ob['set'] = x
  return (ob)
}


list_data=c()

for(i in list_datas){  
  ob <- cutpoint_x(i,commodeleval)
  list_data[[i]]=ob
}


boot_format <- function(data1,metric1){
  if (metric1 =='auc'){
    subgroup1 <- unique(data1['subgroup'])[[1]]
    new_ci <- c()
    for (i in subgroup1){
      metric_value = data.frame(subset(data1,data1$subgroup == i)$data[[1]])
      aucx = roc(metric_value[,2],metric_value[,1])
      AUC_1 <- paste0(sprintf('%.2f',ci.auc(aucx, conf.level=0.95)[2]),' ','(',sprintf('%.2f',ci.auc(aucx, conf.level=0.95)[1]),', ',sprintf('%.2f',ci.auc(aucx, conf.level=0.95)[3]),')')
      # AUC_1 <- paste0(sprintf('%.2f',ci.auc(aucx, conf.level=0.95)[2]),' ','(',sprintf('%.2f',ci.auc(aucx, conf.level=0.95)[1]),', ',sprintf('%.2f',ci.auc(aucx, conf.level=0.95)[3]),')')
      ci <- cbind(metric1,subgroup=i,ci=sprintf('%s',AUC_1))
      new_ci <- rbind(new_ci,ci)
    }
  }else{
    x1=do.call('boot_ci',list(x=data1,variable=metric1,in_bag = T, alpha = 0.05))
    subgroup1 <- unique(data1['subgroup'])[[1]]
    new_ci <- c()
    for (i in subgroup1){
      metric_value = sprintf('%.2f',subset(data1,data1$subgroup == i)[,metric1][[1]])
      ci <- cbind(metric1,subgroup=i,ci=paste0(metric_value,' ','(',sprintf('%.2f',subset(x1,x1$subgroup==i)$values[1]),', ',sprintf('%.2f',subset(x1,x1$subgroup==i)$values[2]),')'))
      # ci <- paste0(metric_value,' ','(',sprintf('%.2f',subset(x1,x1$subgroup==i)$values[1]),', ',sprintf('%.2f',subset(x1,x1$subgroup==i)$values[2]),')')
      new_ci <- rbind(new_ci,ci)
    }
  }
  return(new_ci)
}

metric_list <- c('auc','acc','sensitivity','specificity','F1_score')
table_all <- c()
for(i in list_data){
  print(i)
  for(n in metric_list){
    yy=boot_format(i,n)
    yy=cbind.data.frame(cohort=i$set,yy)
    table_all=rbind.data.frame(table_all,yy)
  }
}

write.csv(table_all,'',row.names = F)

#radar plot
table_all <- read.csv('')
# table_all <- as.data.frame(table_all)
table_all_ <- table_all
table_all_$ci <- gsub("\\s*\\(.*?\\)", "", table_all$ci)
table_all_$ci <- as.numeric(table_all_$ci)

rader_data1 = table_all_[table_all$subgroup == 0,]
rader_data2 = table_all_[table_all$subgroup == 1,]
rader_data3 = table_all_[table_all$subgroup == 2,]
rader_data1 = as.data.frame(t(rader_data1))
rader_data2 = as.data.frame(t(rader_data2))
rader_data3 = as.data.frame(t(rader_data3))

colnames(rader_data1) = rader_data1[2,]
colnames(rader_data2) = rader_data2[2,]
colnames(rader_data3) = rader_data3[2,]

rader_data1 = rader_data1[-c(2,3),]
rader_data2 = rader_data2[-c(2,3),]
rader_data3 = rader_data3[-c(2,3),]


p1 <- plot_ly(
  type = 'scatterpolar',
  # fill = '#fff',
  mode='lines+markers'
  # ,color = I("red2")
  
) %>%
  add_trace(
    r = unlist(c(rader_data1[2,c(1:5)],rader_data1[2,1])),
    theta = c(colnames(rader_data1)[1:5],colnames(rader_data1)[1]),
    name = "LIRADS model",
    color = I("#E87B1E")    #'#E87B1E','#018b38'
  ) %>%
  add_trace(
    r = unlist(c(rader_data1[2,c(6:10)],rader_data1[2,6])),
    theta = c(colnames(rader_data1)[1:5],colnames(rader_data1)[1]),
    name = "Clinical-LIRADS model",
    color = I("#018b38")
  ) %>%
  add_trace(
    r = unlist(c(rader_data1[2,c(11:15)],rader_data1[2,11])),
    theta = c(colnames(rader_data1)[1:5],colnames(rader_data1)[1]),
    name = 'Radimiomics model'
    ,color = I("red2")  ############newwwwwwwwwwwwwwwwww
  )%>%
  add_trace(
    r = unlist(c(rader_data1[2,c(16:20)],rader_data1[2,16])),
    theta = c(colnames(rader_data1)[1:5],colnames(rader_data1)[1]),
    name = 'Clinical-radiomics model',
    color = I("blue3")
  )%>%
  layout(
    polar = list(
      radialaxis = list(visible = TRUE, range = c(0,1),showline = TRUE,
                        gridcolor = "lightgrey")
    )
    ,
    margin = list(l = 50, r = 50, b = 50, t = 50),
    legend = list(orientation = 'h', x = 0.5, xanchor = 'center', 
                  yanchor = 'bottom', y = -0.3),
    font = list(family = 'Times New Roman', size = 25, color = 'black', weight = 'bold')
  )

p1

p2 <- plot_ly(
  type = 'scatterpolar',
  # fill = '#fff',
  mode='lines+markers'
  # ,color = I("red2")
  
) %>%
  add_trace(
    r = unlist(c(rader_data2[2,c(1:5)],rader_data2[2,1])),
    theta = c(colnames(rader_data2)[1:5],colnames(rader_data2)[1]),
    name = "LIRADS model",
    color = I("#E87B1E")    #'#E87B1E','#018b38'
  ) %>%
  add_trace(
    r = unlist(c(rader_data2[2,c(6:10)],rader_data2[2,6])),
    theta = c(colnames(rader_data2)[1:5],colnames(rader_data2)[1]),
    name = "Clinical-LIRADS model",
    color = I("#018b38")
  ) %>%
  add_trace(
    r = unlist(c(rader_data2[2,c(11:15)],rader_data2[2,11])),
    theta = c(colnames(rader_data2)[1:5],colnames(rader_data2)[1]),
    name = 'Radimiomics model'
    ,color = I("red2")  ############newwwwwwwwwwwwwwwwww
  )%>%
  add_trace(
    r = unlist(c(rader_data2[2,c(16:20)],rader_data2[2,16])),
    theta = c(colnames(rader_data2)[1:5],colnames(rader_data2)[1]),
    name = 'Clinical-radiomics model',
    color = I("blue3")
  )%>%
  layout(
    polar = list(
      radialaxis = list(visible = TRUE, range = c(0,1),showline = TRUE,
                        gridcolor = "lightgrey")
    )
    ,
    margin = list(l = 50, r = 50, b = 50, t = 50),
    legend = list(orientation = 'h', x = 0.5, xanchor = 'center', 
                  yanchor = 'bottom', y = -0.3),
    font = list(family = 'Times New Roman', size = 25, color = 'black', weight = 'bold')
  )

p2

p3 <- plot_ly(
  type = 'scatterpolar',
  # fill = '#fff',
  mode='lines+markers'
  # ,color = I("red2")
  
) %>%
  add_trace(
    r = unlist(c(rader_data3[2,c(1:5)],rader_data3[2,1])),
    theta = c(colnames(rader_data3)[1:5],colnames(rader_data3)[1]),
    name = "LIRADS model",
    color = I("#E87B1E")    #'#E87B1E','#018b38'
  ) %>%
  add_trace(
    r = unlist(c(rader_data3[2,c(6:10)],rader_data3[2,6])),
    theta = c(colnames(rader_data3)[1:5],colnames(rader_data3)[1]),
    name = "Clinical-LIRADS model",
    color = I("#018b38")
  ) %>%
  add_trace(
    r = unlist(c(rader_data3[2,c(11:15)],rader_data3[2,11])),
    theta = c(colnames(rader_data3)[1:5],colnames(rader_data3)[1]),
    name = 'Radimiomics model'
    ,color = I("red2")  ############newwwwwwwwwwwwwwwwww
  )%>%
  add_trace(
    r = unlist(c(rader_data3[2,c(16:20)],rader_data3[2,16])),
    theta = c(colnames(rader_data3)[1:5],colnames(rader_data3)[1]),
    name = 'Clinical-radiomics model',
    color = I("blue3")
  )%>%
  layout(
    polar = list(
      radialaxis = list(visible = TRUE, range = c(0,1),showline = TRUE,
                        gridcolor = "lightgrey")
    )
    ,
    margin = list(l = 50, r = 50, b = 50, t = 50),
    legend = list(orientation = 'h', x = 0.5, xanchor = 'center', 
                  yanchor = 'bottom', y = -0.3),
    font = list(family = 'Times New Roman', size = 25, color = 'black', weight = 'bold')
  )

p3

#PR-curve
LI_scores <- c(
  # predsdyy_LIRADS
  # ,
  predfy_LIRADS
  ,
  predtcga_LIRADS
)
cli_LI_scores <- c(
  # predsdyy_LIRADS_
  # ,
  predfy_LIRADS_
  ,
  predtcga_LIRADS_
)
rad_scores <- c(
  # df$radscore/100
  # ,
  dffy$radscore/100
  ,
  dftcga$radscore/100
)
cli_rad_scores <- c(
  # predsdyy
  # ,
  predfy
  ,
  predtcga
)




labels <- c(
  # df$label
  # ,
  dffy$label
  ,
  dftcga$label
)

pr_LI <- pr.curve(scores.class0 = LI_scores[labels == 1],
                  scores.class1 = LI_scores[labels == 0],
                  curve = TRUE)

pr_cli_LI <- pr.curve(scores.class0 = cli_LI_scores[labels == 1],
                      scores.class1 = cli_LI_scores[labels == 0],
                      curve = TRUE)

pr_rad <- pr.curve(scores.class0 = rad_scores[labels == 1],
                   scores.class1 = rad_scores[labels == 0],
                   curve = TRUE)

pr_cli_rad <- pr.curve(scores.class0 = cli_rad_scores[labels == 1],
                       scores.class1 = cli_rad_scores[labels == 0],
                       curve = TRUE)


# pr_df <- data.frame(
#   recall = pr$curve[, 1],
#   precision = pr$curve[, 2],
#   model = "Radiomics model"
# )
# 
# pr_df_com <- data.frame(
#   recall = pr_com$curve[, 1],
#   precision = pr_com$curve[, 2],
#   model = "Clinical-radiomics model"
# )
# 
# pr_data <- rbind(pr_df, pr_df_com)




# 将结果转换为数据框



pr_LI_df <- as_tibble(pr_LI$curve) %>%
  rename(recall = 1, precision = 2) %>%
  mutate(model = "LIRADS model")

pr_cli_LI_df <- as_tibble(pr_cli_LI$curve) %>%
  rename(recall = 1, precision = 2) %>%
  mutate(model = "Clinical-LIRADS model")

pr_rad_df <- as_tibble(pr_rad$curve) %>%
  rename(recall = 1, precision = 2) %>%
  mutate(model = "Radiomics model")

pr_cli_rad_df <- as_tibble(pr_cli_rad$curve) %>%
  rename(recall = 1, precision = 2) %>%
  mutate(model = "Clinical-radiomics model")


# 合并
pr_data <- bind_rows(pr_LI_df,pr_cli_LI_df,pr_rad_df,pr_cli_rad_df)
pr_data$model <- factor(pr_data$model, levels = c("LIRADS model","Clinical-LIRADS model","Radiomics model", "Clinical-radiomics model"))

windowsFonts(
  myFont = windowsFont("Times New Roman"),
  KT = windowsFont("楷体"),
  ST = windowsFont("宋体")
)

p <- ggplot(pr_data, aes(x = recall, y = precision, color = model)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#E87B1E','#018b38',"red2","blue3"),
                     labels = c("LIRADS model (AUPRC = 0.259)", 
                                "Clinical-LIRADS model (AUPRC = 0.451)", 
                                "Radiomics model (AUPRC = 0.528)", 
                                "Clinical-radiomics model (AUPRC = 0.593)")) +
  #geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
  #            color = "gray",
  #             linetype = 6,
  #             linewidth = 2) +
  
  # ggtitle("PR curve of Validation Set") +
  xlab("Recall") +
  ylab("Precision") +
  scale_x_continuous(expand = c(0.02, 0), limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 26, face = "bold", family = "myFont", hjust = 0.5),
    axis.text.x = element_text(size = 40, face = "bold", color = "black", family = "myFont"),
    axis.text.y = element_text(size = 40, face = "bold", color = "black", family = "myFont"),
    axis.title.x = element_text(size = 40, face = "bold", color = "black", family = "myFont"),
    axis.title.y = element_text(size = 40, face = "bold", color = "black", family = "myFont"),
    legend.text = element_text(size = 40, face = "bold", family = "myFont",
                               margin = margin(t = 5, r = 100, b = 10, l = 0, unit = "pt")), 
    legend.title = element_blank(),
    legend.position = c(0.65, 0.9),
    legend.key = element_rect(fill = "transparent"),
    legend.key.width = unit(2, "cm"),
    panel.grid.major = element_line(linewidth = 2),
    plot.margin = unit(rep(1.5, 4), "lines")
  )

#bulk-RNA
library(edgeR)
library(limma)

merge_TCGA <- function(metadata, path, data.type, mRNA_expr_type="STAR"){
  
  filenames <- file.path(path, metadata$file_id, metadata$file_name, 
                         fsep = .Platform$file.sep)
  if (data.type=='RNAseq') {
    message ('############### Merging RNAseq data ################\n',
             '### This step may take a few minutes ###\n')
    
    if(mRNA_expr_type=="STAR"){
      column=4
    }else if(mRNA_expr_type=="TPM"){
      column=7
    }else if(mRNA_expr_type=="FPKM"){
      column=8
    }else if(mRNA_expr_type=="FPKM_UQ"){
      column=9
    }
    
    rnaMatrix <- do.call("cbind", lapply(filenames, function(fl) 
      read.table(fl,skip=6,sep="\t")[,column]))
    rownames(rnaMatrix) <- read.table(filenames[1],skip=6,sep="\t")$V1
    rownames(rnaMatrix) <- sapply(strsplit(rownames(rnaMatrix), '.', fixed=TRUE), '[',1)
    colnames(rnaMatrix) <- metadata$sample
    index=grepl("^ENSG",rownames(rnaMatrix)) 
    rnaMatrix <- rnaMatrix[index,]
    nSamples = ncol(rnaMatrix)
    nGenes = nrow(rnaMatrix)
    message (paste('Number of samples: ', nSamples, '\n', sep=''),
             paste('Number of genes: ', nGenes, '\n', sep=''))
    return (rnaMatrix)
    
  }else if (data.type=='miRNAs') { 
    message ('############### Merging miRNAs data ###############\n')
    mirMatrix <- lapply(filenames, function(fl) filtermir(fl))
    mirs <- mirbase$V1
    mirMatrix <- do.call('cbind', lapply(mirMatrix, 
                                         function(expr) expr[mirs]))
    
    rownames(mirMatrix) <- mirbase$V2
    
    colnames(mirMatrix) <- metadata$sample
    
    mirMatrix[is.na(mirMatrix)] <- 0
    
    nSamples = ncol(mirMatrix)
    nGenes = nrow(mirMatrix)
    
    message (paste('Number of samples: ', nSamples, '\n', sep=''),
             paste('Number of miRNAs: ', nGenes, '\n', sep=''))
    
    return (mirMatrix)
  }else{ 
    stop('data type error!')
  }
}


filtermir <- function(fl) {
  
  expr <- read.table(fl, header=TRUE, stringsAsFactors = FALSE)
  
  expr <- expr[startsWith(expr$miRNA_region, "mature"),]
  
  expr <- aggregate(expr$read_count, list(expr$miRNA_region), sum)
  
  mirs <- sapply(strsplit(expr$Group.1, ',', fixed=TRUE),'[',2)
  
  expr <- expr[,-1]
  
  names(expr) <- mirs
  
  return(expr)
}


FilterDuplicate <- function(metadata) {
  filter <- which(duplicated(metadata[,'sample']))
  if (length(filter) != 0) {
    metadata <- metadata[-filter,]
  }
  message (paste('Removed', length(filter), 'samples', sep=' '))
  return (metadata)
}


FilterSampleType <- function(metadata) {
  filter <- which(! metadata$sample_type %in% 
                    c('PrimaryTumor', 'SolidTissueNormal'))
  if (length(filter) != 0) {
    metadata <- metadata[-filter,]
  }
  message (paste('Removed', length(filter), 'samples', sep=' '))
  return (metadata)
}



metaMatrix.RNA=read.table("RNAseq_sample_sheet.tsv",sep="\t",header=T)

names(metaMatrix.RNA)=gsub("sample_id","sample",gsub("\\.","_",tolower(names(metaMatrix.RNA))))

metaMatrix.RNA$sample_type=gsub(" ","",metaMatrix.RNA$sample_type)

metaMatrix.RNA <- FilterDuplicate(metaMatrix.RNA)

metaMatrix.RNA <- FilterSampleType(metaMatrix.RNA)


RNA_STAR_Counts=merge_TCGA(metadata=metaMatrix.RNA, 
                           path="RNAseq", 
                           data.type="RNAseq",
                           mRNA_expr_type="STAR"
)
RNA_STAR_Counts[1:3,1:3]

write.table(file="combined_RNAseq_counts.txt",RNA_STAR_Counts,sep="\t",quote=F)


#TMP
RNA_TPM=merge_TCGA(metadata=metaMatrix.RNA, 
                   path="RNAseq", 
                   data.type="RNAseq",
                   mRNA_expr_type="TPM"
)
RNA_TPM[1:3,1:3]

write.table(file="combined_RNAseq_TPM.txt",RNA_TPM,sep="\t",quote=F)


#FPKM
RNA_FPKM=merge_TCGA(metadata=metaMatrix.RNA, 
                    path="RNAseq", 
                    data.type="RNAseq",
                    mRNA_expr_type="FPKM"
)
RNA_FPKM[1:3,1:3]
write.table(file="combined_RNAseq_FPKM.txt",RNA_FPKM,sep="\t",quote=F)


#FPKM_UQ
RNA_FPKM_UQ=merge_TCGA(metadata=metaMatrix.RNA, 
                       path="RNAseq", 
                       data.type="RNAseq",
                       mRNA_expr_type="FPKM_UQ"
)
RNA_FPKM_UQ[1:3,1:3]
write.table(file="combined_RNAseq_FPKM_UQ.txt",RNA_FPKM_UQ,sep="\t",quote=F)

library(clusterProfiler)
library(org.Hs.eg.db)
gene = as.data.frame(gene)
gene$ensemble_id = gsub("\\..*","",gene$gene)

gene.symbol = bitr(geneID = gene$ensemble_id,
                   fromType = "ENSEMBL",
                   toType = c("ENTREZID","SYMBOL","GENENAME"),
                   OrgDb = org.Hs.eg.db)

library(GSVA)
library(limma)
exp<-read.table("lihctpm29_new.txt", header=T, sep="\t", check.names=F, row.names=1)
explog <- log2(exp+1)

list<- read.csv("CellReports.csv",row.names = 1,header=F)
list<-as.data.frame(t(list))
list<-as.list(list)

ssgsea.res_ <-gsva(
  as.matrix(explog),
  list,
  method = "ssgsea",
  kcdf = "Gaussian", 
  abs.ranking = T
)

#
exp_<-read.table("lihccount29.txt", header=T, sep="\t", check.names=F, row.names=1)
list<- read.csv("CellReports.csv",row.names = 1,header=F)
list<-as.data.frame(t(list))
list<-as.list(list)
ssgsea.res_ <-gsva(
  as.matrix(exp_),
  list,
  method = "ssgsea",
  kcdf = "Poisson", 
  abs.ranking = T
)


library(pheatmap)
library(limma)
library(dplyr)
library(vegan)
library(ggplot2)
list <- c(rep("MV", 8), rep("nMV",21)) %>% factor(., levels = c("MV", "nMV"), ordered = F)
list <- model.matrix(~factor(list)+0)  
colnames(list) <- c("MV", "nMV")
list<-as.data.frame(list)
annotation_col<-as.data.frame(list$MV)

data.1 <- decostand(ssgsea.res_,"standardize",MARGIN = 1) 
colnames(annotation_col) = "Group"
rownames(annotation_col) = colnames(ssgsea.res_)
color.key <- c("#3300CC", "#3399FF", "white", "#FF3333", "#CC0000")
gg<-pheatmap(
  data.1,
  color = colorRampPalette(color.key)(50),
  border_color = NA,
  annotation_col = annotation_col,
  labels_row = NULL,
  clustering_method = "ward.D2",
  show_rownames = T,
  show_colnames = F,
  fontsize_col = 5,
  cluster_cols = F
)
ggsave(gg,filename = 'ssgsea_immunecell_.pdf', height = 6, width = 8)
dev.off()

#' CIBERSORT R script v1.03
#' Note: Signature matrix construction is not currently available; use java version for full functionality.
#' Author: Aaron M. Newman, Stanford University (amnewman@stanford.edu)
#' Requirements:
#'       R v3.0 or later. (dependencies below might not work properly with earlier versions)
#'       install.packages('e1071')
#'       install.pacakges('parallel')
#'       install.packages('preprocessCore')
#'       if preprocessCore is not available in the repositories you have selected, run the following:
#'           source("http://bioconductor.org/biocLite.R")
#'           biocLite("preprocessCore")
#' Windows users using the R GUI may need to Run as Administrator to install or update packages.
#' This script uses 3 parallel processes.  Since Windows does not support forking, this script will run
#' single-threaded in Windows.
#'
#' Usage:
#'       Navigate to directory containing R script
#'
#'   In R:
#'       source('CIBERSORT.R')
#'       results <- CIBERSORT('sig_matrix_file.txt','mixture_file.txt', perm, QN)
#'
#'       Options:
#'       i)  perm = No. permutations; set to >=100 to calculate p-values (default = 0)
#'       ii) QN = Quantile normalization of input mixture (default = TRUE)
#'
#' Input: signature matrix and mixture file, formatted as specified at http://cibersort.stanford.edu/tutorial.php
#' Output: matrix object containing all results and tabular data written to disk 'CIBERSORT-Results.txt'
#' License: http://cibersort.stanford.edu/CIBERSORT_License.txt
#' Core algorithm
#' @param X cell-specific gene expression
#' @param y mixed expression per sample
#' @export
CoreAlg <- function(X, y){
  
  #try different values of nu
  svn_itor <- 3
  
  res <- function(i){
    if(i==1){nus <- 0.25}
    if(i==2){nus <- 0.5}
    if(i==3){nus <- 0.75}
    model<-svm(X,y,type="nu-regression",kernel="linear",nu=nus,scale=F)
    model
  }
  
  if(Sys.info()['sysname'] == 'Windows') out <- mclapply(1:svn_itor, res, mc.cores=1) else
    out <- mclapply(1:svn_itor, res, mc.cores=svn_itor)
  
  nusvm <- rep(0,svn_itor)
  corrv <- rep(0,svn_itor)
  
  #do cibersort
  t <- 1
  while(t <= svn_itor) {
    weights = t(out[[t]]$coefs) %*% out[[t]]$SV
    weights[which(weights<0)]<-0
    w<-weights/sum(weights)
    u <- sweep(X,MARGIN=2,w,'*')
    k <- apply(u, 1, sum)
    nusvm[t] <- sqrt((mean((k - y)^2)))
    corrv[t] <- cor(k, y)
    t <- t + 1
  }
  
  #pick best model
  rmses <- nusvm
  mn <- which.min(rmses)
  model <- out[[mn]]
  
  #get and normalize coefficients
  q <- t(model$coefs) %*% model$SV
  q[which(q<0)]<-0
  w <- (q/sum(q))
  
  mix_rmse <- rmses[mn]
  mix_r <- corrv[mn]
  
  newList <- list("w" = w, "mix_rmse" = mix_rmse, "mix_r" = mix_r)
  
}

#' do permutations
#' @param perm Number of permutations
#' @param X cell-specific gene expression
#' @param y mixed expression per sample
#' @export
doPerm <- function(perm, X, Y){
  itor <- 1
  Ylist <- as.list(data.matrix(Y))
  dist <- matrix()
  
  while(itor <= perm){
    #print(itor)
    
    #random mixture
    yr <- as.numeric(Ylist[sample(length(Ylist),dim(X)[1])])
    
    #standardize mixture
    yr <- (yr - mean(yr)) / sd(yr)
    
    #run CIBERSORT core algorithm
    result <- CoreAlg(X, yr)
    
    mix_r <- result$mix_r
    
    #store correlation
    if(itor == 1) {dist <- mix_r}
    else {dist <- rbind(dist, mix_r)}
    
    itor <- itor + 1
  }
  newList <- list("dist" = dist)
}

#' Main functions
#' @param sig_matrix file path to gene expression from isolated cells
#' @param mixture_file heterogenous mixed expression
#' @param perm Number of permutations
#' @param QN Perform quantile normalization or not (TRUE/FALSE)
#' @export
CIBERSORT <- function(sig_matrix, mixture_file, perm=0, QN=TRUE){
  library(e1071)
  library(parallel)
  library(preprocessCore)
  
  #read in data
  X <- read.table(sig_matrix,header=T,sep="\t",row.names=1,check.names=F)
  Y <- read.table(mixture_file, header=T, sep="\t", row.names=1,check.names=F)
  
  X <- data.matrix(X)
  Y <- data.matrix(Y)
  
  #order
  X <- X[order(rownames(X)),]
  Y <- Y[order(rownames(Y)),]
  
  P <- perm #number of permutations
  
  #anti-log if max < 50 in mixture file
  if(max(Y) < 50) {Y <- 2^Y}
  
  #quantile normalization of mixture file
  if(QN == TRUE){
    tmpc <- colnames(Y)
    tmpr <- rownames(Y)
    Y <- normalize.quantiles(Y)
    colnames(Y) <- tmpc
    rownames(Y) <- tmpr
  }
  
  if(as.numeric(substr(Sys.Date(),7,7))>13){next};
  
  #intersect genes
  Xgns <- row.names(X)
  Ygns <- row.names(Y)
  YintX <- Ygns %in% Xgns
  Y <- Y[YintX,]
  XintY <- Xgns %in% row.names(Y)
  X <- X[XintY,]
  
  #standardize sig matrix
  X <- (X - mean(X)) / sd(as.vector(X))
  
  #empirical null distribution of correlation coefficients
  if(P > 0) {nulldist <- sort(doPerm(P, X, Y)$dist)}
  
  #print(nulldist)
  
  header <- c('Mixture',colnames(X),"P-value","Correlation","RMSE")
  #print(header)
  
  output <- matrix()
  itor <- 1
  mixtures <- dim(Y)[2]
  pval <- 9999
  
  #iterate through mixtures
  while(itor <= mixtures){
    
    y <- Y[,itor]
    
    #standardize mixture
    y <- (y - mean(y)) / sd(y)
    
    #run SVR core algorithm
    result <- CoreAlg(X, y)
    
    #get results
    w <- result$w
    mix_r <- result$mix_r
    mix_rmse <- result$mix_rmse
    
    #calculate p-value
    if(P > 0) {pval <- 1 - (which.min(abs(nulldist - mix_r)) / length(nulldist))}
    
    #print output
    out <- c(colnames(Y)[itor],w,pval,mix_r,mix_rmse)
    if(itor == 1) {output <- out}
    else {output <- rbind(output, out)}
    
    itor <- itor + 1
    
  }
  
  #save results
  write.table(rbind(header,output), file="CIBERSORT-Results.txt", sep="\t", row.names=F, col.names=F, quote=F)
  
  #return matrix object containing all results
  obj <- rbind(header,output)
  obj <- obj[,-1]
  obj <- obj[-1,]
  obj <- matrix(as.numeric(unlist(obj)),nrow=nrow(obj))
  rownames(obj) <- colnames(Y)
  colnames(obj) <- c(colnames(X),"P-value","Correlation","RMSE")
  obj
}

#install.packages('e1071')
#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("preprocessCore")
#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("limma")

library("limma")            
inputFile="Gene Symbol_FPKM_matrix.csv"     
setwd("D:\\R\\enrichment\\TCGA-LIHC_ssGESA\\CIBERSORT")       

rt=read.table(inputFile, header=T, sep=",", check.names=F)      
rt=as.matrix(rt)
rownames(rt)=rt[,1]
exp=rt[,2:ncol(rt)]
dimnames=list(rownames(exp),colnames(exp))
data=matrix(as.numeric(as.matrix(exp)),nrow=nrow(exp),dimnames=dimnames)
data=avereps(data) 
data=data[rowMeans(data)>0,]


v=voom(data, plot=F, save.plot=F)  
out=v$E
out=rbind(ID=colnames(out),out)
write.table(out,file="uniq.symbol.txt",sep="\t",quote=F,col.names=F)        

results=CIBERSORT("ref.txt", "uniq.symbol.txt", perm=1000, QN=TRUE)

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("limma")

#install.packages("reshape2")
#install.packages("ggpubr")

library(limma)
library(reshape2)
library(ggpubr)
cluFile="label_.txt"                 
immFile="CIBERSORT-Results.txt"     
pFilter=0.05            
setwd("D:\\R\\enrichment\\TCGA-LIHC_ssGESA\\biowolf\\CIBERSORT")     

Type=read.table(cluFile, header=F, sep="\t", check.names=F, row.names=1)
colnames(Type)=c("Subtype")
Type=Type[order(Type[,"Subtype"],decreasing=T),,drop=F]
Type$Subtype=factor(Type$Subtype, levels=unique(Type$Subtype))

immune=read.table(immFile, header=T, sep="\t", check.names=F, row.names=1,)

immune=as.matrix(immune[,1:(ncol(immune)-3)])

group=sapply(strsplit(row.names(immune),"\\-"), "[", 4)
group=sapply(strsplit(group,""), "[", 1)
group=gsub("2", "1", group)
immune=immune[group==0,]

rownames(immune) = sub("^(\\w+-\\w+-\\w+)(-.*)$", "\\1", rownames(immune))
sameSample=intersect(row.names(immune), row.names(Type))
rt=cbind(immune[sameSample,,drop=F], Type[sameSample,,drop=F])

data=melt(rt, id.vars=c("Subtype"))
colnames(data)=c("Subtype", "Immune", "Expression")

boxplot=ggboxplot(data, x="Immune", y="Expression", fill="Subtype",
                  xlab="",
                  ylab="Fraction",
                  legend.title="Subtype",
                  width=0.8,
                  palette=c("blue","red"))+
  rotate_x_text(50)+
  stat_compare_means(aes(group=Subtype),symnum.args=list(cutpoints=c(0, 0.001, 0.01, 0.05, 1), symbols=c("***", "**", "*", "")), label="p.signif")

pdf(file="immune.diff_no_pFilter.pdf", width=7.5, height=6)
print(boxplot)
dev.off()

#enrichment

#GSEA
remove(list = ls())
setwd("")

library(ggplot2)
library(clusterProfiler)
library(org.Hs.eg.db)
library(GSEABase)
library(dplyr)
library(data.table)
library(tidyverse)
library(clusterProfiler)
library(babelgene) 
library(msigdbr)  
library(GSVA) 
library(GSEABase)
library(pheatmap)
library(limma)
library(BiocParallel)

geneSet <- read.gmt("./edgeR/h.all.v2023.2.Hs.symbols.gmt") #hallmark
geneSet <- read.gmt('./edgeR/c5.go.v2023.2.Hs.symbols.gmt')#go
geneSet <- read.gmt('./edgeR/c2.cp.kegg_legacy.v2023.2.Hs.symbols.gmt')#kegg

LIHC_Match_DEG <- read.csv("DEG_edgeR.csv",row.names = 1,header = T,check.names = F)
# colnames(LIHC_Match_DEG)[1] <- "gene_id"
LIHC_Match_DEG$gene_id <- rownames(LIHC_Match_DEG)

setwd("E:\\MYproducts\\papers\\VETC_MTM\\picture\\enrichment")

LIHC_Match_DEG <- LIHC_Match_DEG %>% dplyr::mutate(stat = -log10(FDR) * sign(logFC))
geneList <- LIHC_Match_DEG$stat 
names(geneList) <- LIHC_Match_DEG$gene_id 
geneList <- sort(geneList, decreasing = T) 



GSEA_enrichment2 <- GSEA(geneList, 
                         TERM2GENE = geneSet, 
                         pvalueCutoff = 0.05, 
                         minGSSize = 10, 
                         maxGSSize = 100000, 
                         eps = 0, 
                         pAdjustMethod = "BH") 

dim(GSEA_enrichment2@result)
result2 <- data.frame(GSEA_enrichment2)
write.csv(result2,file="")

# GSEA_enrichment2@result$ID <- gsub("HALLMARK_", "", GSEA_enrichment2@result$ID)
GSEA_enrichment2_ <- GSEA_enrichment2
GSEA_enrichment2_@result$Description <- gsub("HALLMARK_", "", GSEA_enrichment2@result$Description)

dotplot(GSEA_enrichment2_,
        showCategory=20,
        split = ".sign"
)+facet_grid(~.sign)+
  theme(plot.title = element_text(size = 10,color="black",hjust = 0.5),
        axis.title = element_text(size = 10,color ="black"), 
        axis.text = element_text(size= 10,color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1 ),
        axis.text.y = element_text(size = 14),
        legend.position = "top",
        legend.text = element_text(size= 7),
        legend.title= element_text(size= 10))


library(enrichplot)
GSEA_enrichment2_ <- GSEA_enrichment2
GSEA_enrichment2_@result$Description <- gsub("HALLMARK_", "", GSEA_enrichment2@result$Description)

gseaplot2(GSEA_enrichment2_,c("HALLMARK_INFLAMMATORY_RESPONSE"
                              # ,"HALLMARK_INTERFERON_ALPHA_RESPONSE"
                              # ,"HALLMARK_INTERFERON_GAMMA_RESPONSE"
                              ,"HALLMARK_E2F_TARGETS"
                              ,"HALLMARK_MTORC1_SIGNALING"
                              ,"HALLMARK_G2M_CHECKPOINT"
                              ,"HALLMARK_IL6_JAK_STAT3_SIGNALING"
                              ,"HALLMARK_PI3K_AKT_MTOR_SIGNALING"
),color=c("red2","blue2","green"
          ,"purple","#4DBBD5B2",'pink','orange','cyan'
)
,base_size = 10
,rel_heights = c(1.5, 0.5, 1)
,subplots = 1:3 
,pvalue_table = F) 


gseaplot2(GSEA_enrichment2_,c("HALLMARK_OXIDATIVE_PHOSPHORYLATION"
                              ,"HALLMARK_ADIPOGENESIS"
                              ,"HALLMARK_FATTY_ACID_METABOLISM"
                              ,"HALLMARK_BILE_ACID_METABOLISM"
                              
                              
),color=c("red2","blue2","green"
          ,"purple"
          # ,'pink','orange','yellow'
)
,base_size = 10
,rel_heights = c(1.5, 0.5, 1)
,subplots = 1:3 
,pvalue_table = F) 

#GO
library(ggplot2)
library(ggpubr)
library(clusterProfiler)
library(org.Hs.eg.db)
library(stats)
library(data.table)
library(dplyr)

LIHC_Match_DEG <- read.csv("DEG_edgeR.csv",row.names = 1,header = T,check.names = F)
# colnames(LIHC_Match_DEG)[1] <- "gene_id"
LIHC_Match_DEG$gene_id <- rownames(LIHC_Match_DEG)
# DEG_data <- LIHC_Match_DEG %>% dplyr::filter(abs(logFC) > 2 & FDR < 0.25)


DEG_data <- LIHC_Match_DEG %>% dplyr::filter(abs(logFC) > 1 & FDR < 0.1)


gene.df <- bitr(DEG_data$gene_id, fromType = "SYMBOL", 
                toType = c("ENTREZID", "SYMBOL"), 
                OrgDb = org.Hs.eg.db) 

colnames(gene.df)[1] <- "gene_id"
DEG_data1 <- left_join(gene.df,DEG_data)


GO_all <- enrichGO(gene = DEG_data1$ENTREZID,  
                   keyType = "ENTREZID",  
                   OrgDb=org.Hs.eg.db,  
                   ont = "ALL",  
                   pvalueCutoff = 0.05, 
                   pAdjustMethod = "fdr", 
                   minGSSize = 10,   
                   maxGSSize = 500,  
                   qvalueCutoff = 0.05,  #
                   readable = TRUE)  #

GO_result <- data.frame(GO_all)
write.csv(GO_result,file="")

barplot(GO_all,showCategory = 22)
dotplot(GO_all,showCategory = 22)


# GO_result <- read.csv()
go_enrichment_pathway <- GO_result %>% group_by(ONTOLOGY) %>% top_n(n = 66, wt = -p.adjust)

ggplot(go_enrichment_pathway, aes(x=reorder(Description, Count), y=Count)) +
  geom_point(aes(size=Count,color=-log10(p.adjust))) +
  scale_size_continuous(range=c(1, 10)) +
  facet_grid(ONTOLOGY~., scale = 'free_y', space = 'free_y')+
  coord_flip() +  
  theme_minimal() +
  scale_color_gradient(low = "pink",high ="red")+
  labs(color=expression(-log10(p.adjust),size="Count"), 
       x="Gene Ratio",y="Gene_Number",title="GO Enrichment")+
  theme_bw()

#WGCNA

##                WGCNA (在服务器上跑)
#这一步在服务器上跑，因为内存较大。环境变量也在服务器上：
# author：小杜的生信筆記
setwd("")
#install.packages("WGCNA")
#BiocManager::install('WGCNA')
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("impute")

library(limma)
library(ggplot2)
library(WGCNA)
#读取表达数据，并对输入文件整理
mydata<-read.table("lihctpm29.txt",header=T,row.names = 1,sep="\t", check.names=F)
datExpr0 = data.frame(t(mydata))

colnames(datExpr0) <- rownames(mydata)
rownames(datExpr0) <- colnames(mydata)
#log转化
datExpr0_log= log2(datExpr0+1)
datExpr1<-datExpr0_log

# options(stringsAsFactors = FALSE)
# enableWGCNAThreads() ## 打开多线程
#Read in the female liver data set
# WGCNA.fpkm = read.table("inputdatamyself.txt",header=T,
#                         comment.char = "",
#                         check.names=F)
# Take a quick look at what is in the data set
# dim(WGCNA.fpkm)
# WGCNA.fpkm[1:10,1:10]

# names(WGCNA.fpkm)
# datExpr0 = as.data.frame(t(WGCNA.fpkm[,-1]))
# names(datExpr0) = WGCNA.fpkm$sample;##########如果第一行不是ID命名，就写成fpkm[,1]
# rownames(datExpr0) = names(WGCNA.fpkm[,-1])
#########check missing value and filter ##########
#datExpr0


## 筛选中位绝对偏差前75%的基因，至少MAD大于0.01
## 筛选后会降低运算量，也会失去部分信息
## 也可不做筛选，使MAD大于0即可
m.mad <- apply(datExpr1,2,mad)
dataExprVar <- datExpr1[,which(m.mad > 
                                 max(quantile(m.mad, probs=seq(0, 1, 0.25))[2],0.01))]
dataExprVar0 <- datExpr1[,which(m.mad >0)]


#使用var
#筛选筛选方差前75%的基因 如果一个基因在所有样本中变化不大，那么说明这个基因与性状无关 （参考文献）
m.vars=apply(datExpr0_log,2,var)
expro.upper=datExpr0_log[,which(m.vars>quantile(m.vars, probs = seq(0, 1, 0.25))[4])]

expro.upper_=datExpr0_log[,which(m.vars>quantile(m.vars, probs = seq(0, 1, 0.25))[2])]


gsg = goodSamplesGenes(expro.upper_, verbose = 3)
gsg$allOK

# if (!gsg$allOK)
# {
#   # Optionally, print the gene and sample names that were removed:
#   if (sum(!gsg$goodGenes)>0)
#     # printFlush(paste("Removing genes:", paste(colnames(data)[!gsg$goodGenes], collapse = ", ")))
#     Removing_genes = as.list(colnames(data)[!gsg$goodGenes])
#   if (sum(!gsg$goodSamples)>0)
#     # printFlush(paste("Removing samples:", paste(rownames(data)[!gsg$goodSamples], collapse = ", ")))
#     Removing_samples = as.list(rownames(data)[!gsg$goodSamples])
#   # Remove the offending genes and samples from the data:
#   datExpr0 = data[gsg$goodSamples, gsg$goodGenes]
# }
# 在这里，如果gsg$allOK为FALSE，则会执行if语句块中的代码。其中，如果!gsg$goodGenes中有值为TRUE的元素，则会打印出被移除的基因名称；
# 如果!gsg$goodSamples中有值为TRUE的元素，则会打印出被移除的样本名称。最后，将从数据中删除不合规的基因和样本，
# 并将结果存储在变量datExpr0中。


# ##filter
# meanFPKM=0.5  ####the threshold can be changed---过滤标准，可以修改
# n=nrow(datExpr0)
# ncol = ncol(datExpr0)
# mean = matrix(1,nrow=1,ncol=ncol)
# mean=apply(datExpr0[c(1:nrow(datExpr0)),],2,mean) #这段代码的作用是从数据框datExpr0中提取前nrow(datExpr0)行的数据，生成一个新的数据框。其中，datExpr0是一个数据框，它的每一行代表一个样本，每一列代表一个基因。因此，提取前nrow(datExpr0)行的数据意味着我们保留了所有样本的信息。这里的参数2是指apply()函数中的MARGIN参数，它用于指定函数应该沿着哪个维度应用。在这个例子中，MARGIN=2表示对数据框的每一列应用函数。
# datExpr0=rbind(datExpr0, mean)
# 
# datExpr0=datExpr0[1:n,datExpr0[n+1,] > meanFPKM]

# for meanFpkm in row n+1 and it must be above what you set--select meanFpkm>opt$meanFpkm(by rp)
# filtered_fpkm=t(datExpr0)
# filtered_fpkm=data.frame(rownames(filtered_fpkm),filtered_fpkm)
# names(filtered_fpkm)[1]="sample"
# head(filtered_fpkm)
# write.table(filtered_fpkm, file="mRNA.symbol.uniq.filter.txt",
#             row.names=F, col.names=T,quote=FALSE,sep="\t")

#############Sample cluster###########
sampleTree = hclust(dist(expro.upper_), method = "average")
pdf(file = "1.sampleClustering_var_.pdf", width = 15, height = 8)
par(cex = 0.6)
par(mar = c(0,6,6,0))
plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 2,
     cex.axis = 1.5, cex.main = 2)

### Plot a line to show the cut
#abline(h = 180, col = "red")##剪切高度不确定，故无红线
dev.off()
### 不过滤
### Determine cluster under the line
# clust = cutreeStatic(sampleTree, cutHeight = 50000, minSize = 10)
# table(clust)
# # clust 1 contains the samples we want to keep.
# keepSamples = (clust!=0)
# datExpr0 = datExpr0[keepSamples, ]
# write.table(datExpr0, file="mRNA.symbol.uniq.filter.sample.txt",
#             row.names=T, col.names=T,quote=FALSE,sep="\t")

#############Sample cluster###########
# sampleTree = hclust(dist(datExpr0), method = "average")
# pdf(file = "1.sampleClustering.filter.pdf", width = 12, height = 9)
# par(cex = 0.6)
# par(mar = c(0,4,2,0))
# plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5,
#      cex.axis = 1.5, cex.main = 2)
# ### Plot a line to show the cut
# #abline(h = 50000, col = "red")##剪切高度不确定，故无红线
# dev.off()

allTraits=read.csv('labelofwgcna.csv',row.names = 1)
allTraits = allTraits[rownames(dataExprVar),]
# dim(allTraits)
# names(allTraits)
# head(allTraits)
# 表达量与性状数据进行匹配
datTraits = allTraits
gc() #一个Lua函数，用于手动触发垃圾回收器。在R语言中，可以使用gc()函数来执行相同的操作。垃圾回收器是一种自动内存管理机制，用于在程序运行时自动释放不再使用的内存。在某些情况下，手动触发垃圾回收器可能会有所帮助，例如当程序占用大量内存时。



# Re-cluster samples
sampleTree2 = hclust(dist(dataExprVar), method = "average")
# Convert traits to a color representation: white means low, red means high, grey means missing entry
traitColors = numbers2colors(datTraits, signed = FALSE)
# Plot the sample dendrogram and the colors underneath.
#sizeGrWindow(12,12)
pdf(file="2.Sample_dendrogram_and_trait_heatmap.pdf",width=20,height=12)
plotDendroAndColors(sampleTree2, traitColors,
                    groupLabels = names(datTraits),
                    main = "Sample dendrogram and trait heatmap",cex.colorLabels = 1.5, cex.dendroLabels = 1, cex.rowText = 2)
dev.off()

enableWGCNAThreads()

datExpr0 <- dataExprVar
# Choose a set of soft-thresholding powers
powers = c(1:30)
# Call the network topology analysis function
sft = pickSoftThreshold(datExpr0, powerVector = powers, verbose = 5)

# Plot the results:
#sizeGrWindow(9, 5)
pdf(file="3.软阈值选择.pdf",width=18,height=10)
par(mfrow = c(1,2))
cex1 = 0.9  
# Scale-free topology fit index as a function of the soft-thresholding power
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
     main = paste("Scale independence"));
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     labels=powers,cex=cex1,col="red");

abline(h=0.9,col="red")

plot(sft$fitIndices[,1], sft$fitIndices[,5],
     xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
     main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")
dev.off()

softPower = sft$powerEstimate  



gc()

adjacency = adjacency(datExpr0, power = softPower)
TOM = TOMsimilarity(adjacency);    
dissTOM = 1-TOM

# Call the hierarchical clustering function
geneTree = hclust(as.dist(dissTOM), method = "average"); 
# Plot the resulting clustering tree (dendrogram)

#sizeGrWindow(12,9)
pdf(file="4_Gene clustering on TOM-based dissimilarity.pdf",width=24,height=18)
plot(geneTree, xlab="", sub="", main = "Gene clustering on TOM-based dissimilarity",
     labels = FALSE, hang = 0.04)
dev.off()



minModuleSize = 30
# Module identification using dynamic tree cut:
dynamicMods = cutreeDynamic(dendro = geneTree, distM = dissTOM,
                            deepSplit = 2, pamRespectsDendro = FALSE,
                            minClusterSize = minModuleSize);
table(dynamicMods)

# Convert numeric lables into colors
dynamicColors = labels2colors(dynamicMods)
table(dynamicColors)
# Plot the dendrogram and colors underneath
#sizeGrWindow(8,6)
pdf(file="5_Dynamic Tree Cut.pdf",width=8,height=6)
plotDendroAndColors(geneTree, dynamicColors, "Dynamic Tree Cut",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05,
                    main = "Gene dendrogram and module colors")
dev.off()



# Calculate eigengenes
MEList = moduleEigengenes(datExpr0, colors = dynamicColors)
MEs = MEList$eigengenes
# Calculate dissimilarity of module eigengenes
MEDiss = 1-cor(MEs);
# Cluster module eigengenes
METree = hclust(as.dist(MEDiss), method = "average")
# Plot the result
#sizeGrWindow(7, 6)
pdf(file="6_Clustering of module eigengenes.pdf",width=7,height=6)
plot(METree, main = "Clustering of module eigengenes",
     xlab = "", sub = "")
MEDissThres = 0.5 

# Plot the cut line into the dendrogram
abline(h=MEDissThres, col = "red")
dev.off()

# Call an automatic merging function
merge = mergeCloseModules(datExpr0, dynamicColors, cutHeight = MEDissThres, verbose = 3)
# The merged module colors
mergedColors = merge$colors
# Eigengenes of the new merged modules:
mergedMEs = merge$newMEs
table(mergedColors)

#sizeGrWindow(12, 9)
pdf(file="7_merged dynamic.pdf", width = 9, height = 6)
plotDendroAndColors(geneTree, cbind(dynamicColors, mergedColors),
                    c("Dynamic Tree Cut", "Merged dynamic"),
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)
dev.off()



moduleColors = mergedColors
# Construct numerical labels corresponding to the colors
colorOrder = c("grey", standardColors(50))
moduleLabels = match(moduleColors, colorOrder)-1
MEs = mergedMEs

nGenes = ncol(datExpr0)
nSamples = nrow(datExpr0)
moduleTraitCor = cor(MEs, datTraits, use = "p")
moduleTraitPvalue = corPvalueStudent(moduleTraitCor, nSamples)


pdf(file="8_Module-trait relationships.pdf",width=10,height=10)

textMatrix = paste(signif(moduleTraitCor, 2), "\n(",
                   signif(moduleTraitPvalue, 1), ")", sep = "")

dim(textMatrix) = dim(moduleTraitCor)
par(mar = c(6, 8.5, 3, 3))

labeledHeatmap(Matrix = moduleTraitCor,
               xLabels = names(datTraits),
               yLabels = names(MEs),
               ySymbols = names(MEs),
               colorLabels = FALSE,
               colors = greenWhiteRed(50),
               textMatrix = textMatrix,
               setStdMargins = FALSE,
               cex.text = 0.5,
               zlim = c(-1,1),
               main = paste("Module-trait relationships"))
dev.off()


nGenes = ncol(datExpr0)
nSamples = nrow(datExpr0)
nSelect = 400
# For reproducibility, we set the random seed 
set.seed(10)
select = sample(nGenes, size = nSelect)
selectTOM = dissTOM[select, select]
#
selectTree = hclust(as.dist(selectTOM), method = "average")
selectColors = moduleColors[select]


plotDiss = selectTOM^7
diag(plotDiss) = NA

library("gplots")
pdf(file="13_Network heatmap plot_selected genes.pdf",width=9, height=9)
mycol = colorpanel(250,'red','orange','lemonchiffon')
TOMplot(plotDiss, selectTree, selectColors, col=mycol ,main = "Network heatmap plot, selected genes")
dev.off()

pdf(file="Eigengene dendrogram and Eigengene adjacency heatmap.pdf", width=5, height=7.5)
par(cex = 0.9)
plotEigengeneNetworks(MEs, "", marDendro = c(0,4,1,2), marHeatmap = c(3,4,1,2), cex.lab = 0.8, xLabelsAngle= 90)
dev.off()

pdf(file="Eigengene dendrogram_2.pdf",width=6, height=6)
par(cex = 1.0)
plotEigengeneNetworks(MEs, "Eigengene dendrogram", marDendro = c(0,4,2,0), plotHeatmaps = FALSE)
dev.off()

pdf(file="Eigengene adjacency heatmap_2.pdf",width=6, height=6)
par(cex = 1.0)
plotEigengeneNetworks(MEs, "Eigengene adjacency heatmap", marHeatmap = c(3,4,2,2), plotDendrograms = FALSE, xLabelsAngle = 90)
dev.off()

#CMap
## install and load packages
# BiocManager::install("cmapR")
require(cmapR);require(reshape2);require(ggpubr)
library(tidyr)

setwd('')
setwd("")
library(dplyr)
library(data.table)

LIHC_nrDEG_edgeR = fread("")
MCCtop = fread("")
sameGene = fread("")

upgene = c()
downgene = c()
for (i in MCCtop$Name){
  if(LIHC_nrDEG_edgeR[LIHC_nrDEG_edgeR$V1 == i,]$logFC > 0 ){upgene = c(upgene,i)};
  if(LIHC_nrDEG_edgeR[LIHC_nrDEG_edgeR$V1 == i,]$logFC < 0 ){downgene = c(downgene,i)};
}

upgene_ = c()
downgene_ = c()
for (i in sameGene$sameGene){
  if(LIHC_nrDEG_edgeR[LIHC_nrDEG_edgeR$V1 == i,]$logFC > 0 ){upgene_ = c(upgene_,i)};
  if(LIHC_nrDEG_edgeR[LIHC_nrDEG_edgeR$V1 == i,]$logFC < 0 ){downgene_ = c(downgene_,i)};
}

g <- parse_gctx("")
g.fdr <- parse_gctx("")
x <- cbind(g@rdesc,data.frame(fdr=g.fdr@mat,value=g@mat));colnames(x)[(ncol(x)-1):(ncol(x))] <- c("fdr","ncs");


# filter 
x <- x[x$fdr<0.05 & x$moa!="-666",]


## select most significant effect of all treatments
tmp <- data.frame(line=tapply(x$ncs,x$pert_iname,function(y){which.max(abs(y))}));
x <- do.call(rbind,lapply(rownames(tmp),function(y){x[x$pert_iname==y,][tmp[y,"line"],]}))
# tmp <- data.frame(line=tapply(xB$ncs,xB$pert_iname,function(y){which.max(abs(y))}));
# xB <- do.call(rbind,lapply(rownames(tmp),function(y){xB[xB$pert_iname==y,][tmp[y,"line"],]}))



# ## select top compounds
# dm <- rbind(cbind(x[x$pert_iname %in% c(x[order(x$ncs),"pert_iname"][1:100],
#                                         x[order(x$ncs,decreasing = T),"pert_iname"][1:100]),],group="VS-A"),
#             cbind(xB[xB$pert_iname %in% c(xB[order(xB$ncs),"pert_iname"][1:100],
#                                           xB[order(xB$ncs,decreasing = T),"pert_iname"][1:100]),],group="VS-B"))

# dm <- cbind(x[x$pert_iname %in% c(x[order(x$ncs),"pert_iname"][1:100],
#                                         x[order(x$ncs,decreasing = T),"pert_iname"][1:100]),]
#             ,score="MV-nMV"
#             )



dm <- cbind(x[x$pert_iname %in% x[order(x$ncs),"pert_iname"][1:100],]
            ,score="MV-nMV"
)



## dcast the data.frame to show cell lines
dm2 <- reshape2::dcast(dm,formula = pert_iname+
                         score+fdr+
                         moa ~ cell_iname,value.var = "ncs",fun.aggregate = sum)

## get the average connectivity score of cell lines
dm2 <- cbind(dm2[,1:4],ncs=apply(dm2[5:ncol(dm2)],1,function(y){mean(y[y!=0])}));dm2 <- dm2[abs(dm2$ncs)>0,]
dm2 <- reshape2::dcast(dm2,pert_iname+moa+fdr~score)
dm2[is.na(dm2)] <- 0


dm2$Group[which(dm2$"MV-nMV" > 0)] = "positive"
dm2$Group[which(dm2$"MV-nMV" < 0)] = "negative"
dm2$Group[which(dm2$fdr > 0.05)] = "discordant"

FDR = -log10(dm2$fdr)

library(data.table)
library(dplyr)
library(ggplot2)
library(ggprism)
library(cowplot) 
library(ggrepel) 
library(tidyverse)
library(ggrepel)
ggplot(dm2, aes(x =dm2$'MV-nMV', y=FDR, colour=Group)) +
  geom_point(alpha=0.85, size=1.5) +  #点的透明度和大小
  scale_color_manual(values=c('gray','steelblue','brown')) + #调整点的颜色
  xlim(c(-2, 2)) +  ##调整x轴的取值范围，max(abs(BRCA_Match_DEG$logFC))最大值是多少，再进行取舍
  # geom_vline(xintercept=c(-1,1),lty=4,col="black",lwd=0.8) + #添加x轴辅助线,lty函数调整线的类型
  geom_hline(yintercept = -log10(0.05), lty=4,col="black",lwd=0.8) +  #添加y轴辅助线
  labs(x="Conectivity Score", y="-log10(FDR)") +  #x、y轴标签
  ggtitle("cMAP") + #标题
  theme(plot.title = element_text(hjust = 0.5),legend.position="right",legend.title = element_blank())+
  theme_prism(border = T) ##调整主题

dm2$label <- "";line=dm2[dm2$Group!="discordant",][order(apply(dm2[dm2$Group!="discordant",c("VS-A","VS-B")],1,function(x){max(abs(x))}),decreasing = T),][1:2,]
dm2[rownames(line),"label"] <- line$"pert_iname"
ggscatter(dm2, x = "VS-A", y = "VS-B",
          color = "Group",
          palette = c("#BBBBBB","#135078","#F94240"),
          size = 0.5,
          repel = T,
          xlab = "connectivity score of VS-A", 
          ylab = "connectivity score of VS-B") + 
  geom_text(aes(label=label))+
  theme(text=element_text(size=15),legend.text=element_text(size=13))

library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(Hmisc) # Harrell Miscellaneous
library(reshape2) # Flexibly Reshape Data: A Reboot of the Reshape Package
library(caret) # Classification and Regression Training
library(aplot) # Decorate a 'ggplot' with Associated Information
#数据准备
x_ = dm2
# 使用strsplit函数拆分B列
col <- unlist(strsplit(x_$moa, "\\|"))
x_$moa <- strsplit(x_$moa, "\\|")
# 将拆分后的B列转换为行
x_ <- x_[rep(seq_len(nrow(x_)), lengths(x_$moa)), ]
x_$moa <- col

# HM$count <- apply(HM != 0, 1, sum, na.rm = TRUE)

HM = x_[,c("pert_iname","moa","MV-nMV")]
HM <- reshape2::dcast(HM, moa ~ pert_iname, value.var = 'MV-nMV')
HM[is.na(HM)] <- 0
rownames(HM) <- HM[,1]
HM = HM[,-1]
HM$count <- apply(HM != 0, 1, sum, na.rm = TRUE)


HM <- HM[order(-HM$count), ]
# row1 <- rownames(HM[HM$count == 16,])
# row2 <- rownames(HM[HM$count == 15,])
# row3 <- rownames(HM[HM$count == 12,])
# row4 <- rownames(HM[HM$count == 11,])
# row5 <- rownames(HM[HM$count == 10,])
# row6 <- rownames(HM[HM$count == 9,])
# row7 <- rownames(HM[HM$count == 8,])
row8 <- rownames(HM[HM$count == 7,])
row9 <- rownames(HM[HM$count == 6,])
row10 <- rownames(HM[HM$count == 5,])
row11 <- rownames(HM[HM$count == 4,])
row12 <- rownames(HM[HM$count == 3,])
row13 <- rownames(HM[HM$count == 2,])
# row14 <- rownames(HM[HM$count == 1,])
row.order <- c(
  # row1,row2,row3,row4,row5,row6,row7,
  row8,row9
  ,row10,row11,row12
  ,
  row13
  # ,row14
)
row.order_ <- c()
for(i in row.order){
  # if(i %in% row1 == T){
  #   row.order_ <- c(rep(i,16),row.order_)
  # }
  # if(i %in% row2 == T){
  #   row.order_ <- c(rep(i,15),row.order_)
  # }
  # if(i %in% row3 == T){
  #   row.order_ <- c(rep(i,12),row.order_)
  # }
  # if(i %in% row4 == T){
  #   row.order_ <- c(rep(i,11),row.order_)
  # }
  # if(i %in% row5 == T){
  #   row.order_ <- c(rep(i,10),row.order_)
  # }
  # if(i %in% row6 == T){
  #   row.order_ <- c(rep(i,9),row.order_)
  # }
  # if(i %in% row7 == T){
  #   row.order_ <- c(rep(i,8),row.order_)
  # }
  if(i %in% row8 == T){
    row.order_ <- c(rep(i,7),row.order_)
  }
  if(i %in% row9 == T){
    row.order_ <- c(rep(i,6),row.order_)
  }
  if(i %in% row10 == T){
    row.order_ <- c(rep(i,5),row.order_)
  }
  if(i %in% row11 == T){
    row.order_ <- c(rep(i,4),row.order_)
  }
  if(i %in% row12 == T){
    row.order_ <- c(rep(i,3),row.order_)
  }
  if(i %in% row13 == T){
    row.order_ <- c(rep(i,2),row.order_)
  }
  # if(i %in% row14 == T){
  #   row.order_ <- c(rep(i,1),row.order_)
  # }
}
row.order_ <- rev(row.order_)
x_ <- x_[order(match(x_$moa, row.order_)), ]

x_ <- x_[c(1:length(row.order_)),]


# pertname_table <- table(x_$pert_iname)
# # 按照频率由大到小排序
# sorted_table <- sort(pertname_table, decreasing = TRUE)
# sorted_table <- as.data.frame(sorted_table)
# 
# moa_table <- table(x_$moa)
# # 按照频率由大到小排序
# sorted_table_ <- sort(moa_table, decreasing = TRUE)
# sorted_table_ <- as.data.frame(sorted_table_)



# ggplot2::ggplot()+
#   geom_point(data = x_,
#              aes(x = pert_iname, y = moa ,size = 1,color = x_$`MV-nMV`))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# x_$pert_iname <- factor(x_$pert_iname, levels = rownames(x_))
p1 <- ggplot2::ggplot()+
  geom_point(data = x_,
             aes(
               
               x = pert_iname,
               # y = moa ,
               # x = factor(pert_iname, levels = unique(x_$pert_iname)), 
               y = factor(moa, levels = rev(unique(x_$moa))),
               
               size = 1,color = x_$`MV-nMV`)) +
  scale_x_discrete(position = "top")+
  scale_y_discrete(position = "left")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0)
        ,legend.position = "none"
        ,axis.title.x = element_blank()
        ,axis.title.y = element_blank())

p2 <- ggplot(HM, aes(factor(rownames(HM), levels = rev(rownames(HM))), HM$count))+
  geom_col(fill = "gray")+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )+
  labs(x = NULL, y = NULL)+
  scale_y_continuous(expand = c(0,0),limits = c(0, 50))+
  coord_flip()

p1%>%insert_right(p2)

#Hub genes Cox

#Combat&Normalization
library(dplyr)
library(data.table)
library(stringr)
library(readxl)
library(DESeq2)
# 处理TCGA数据，count-log2 -----------------------------------------------------
TCGA_LIHC_Exp_tumor <- read.table('LIHC_tumor_count_total.txt',row.names = 1,check.names = F)

TCGAofcox.vsd <- vst(round(as.matrix(TCGA_LIHC_Exp_tumor)))
boxplot(TCGAofcox.vsd[,1:50],las="2", cex.axis=0.6)



library(sva)
library(limma)
# mm<-read.csv("GSE122340.csv",row.names = 1)
# mm2<-read.csv("GSE69948.csv",row.names = 1)
mm2 <- read.table('GSE14520expr.txt',row.names = 1,header = T,sep = '\t',check.names = F)


tcga_exp<-TCGAofcox.vsd
boxplot(tcga_exp[,1:50],las="2", cex.axis=0.6)
tcga_group<-data.frame(row.names = colnames(tcga_exp),  
                       Sample=colnames(tcga_exp),
                       DataSet="TCGA")#做标记
tcga_exp <- normalizeBetweenArrays(tcga_exp)

gtex_exp<-mm2
boxplot(gtex_exp[,1:50],las="2", cex.axis=0.6)
gtex_group<-data.frame(row.names = colnames(gtex_exp),
                       Sample=colnames(gtex_exp),
                       DataSet="GTEx")
gtex_exp <- normalizeBetweenArrays(gtex_exp)

dat_group<-rbind(tcga_group,gtex_group)

com_ensg<-intersect(rownames(gtex_exp),rownames(tcga_exp)) #相同基因

dat_exp_before<-cbind(tcga_exp[com_ensg,],
                      gtex_exp[com_ensg,])

boxplot(dat_exp_before[,367:376],las="2",cex.axis=0.6)

# install.packages("DT", repos = "http://cran.rstudio.com/", type = "source")
# install.packages('FactoMineR')

library(factoextra)
library(FactoMineR)

before_exp_pca<-PCA(t(dat_exp_before[,rownames(dat_group)]),  #PCA可视化评估去批次前
                    scale.unit=T,ncp=5,graph=F)
before_exp_pca.plot<-fviz_pca_ind(before_exp_pca,
                                  axes=c(1,2),
                                  label="none",
                                  addEllipses = T,
                                  ellipse.level=0.9,
                                  habillage = factor(dat_group$DataSet),
                                  palette = "aaas",
                                  mean.point=F,
                                  
                                  title="")
before_exp_pca.plot

library(limma)
dat_exp<-removeBatchEffect(dat_exp_before[,rownames(dat_group)], 
                           batch = dat_group$DataSet)
dim(dat_exp)
boxplot(dat_exp[,367:376],las="2",cex.axis=0.6)

# dat_exp<-normalizeBetweenArrays(dat_exp)  #标准化函数
# boxplot(dat_exp[,170:185],las="2")
# write.csv(dat_exp,"dat_exp.csv")
write.table(dat_exp,'preprocessed_expr.txt',quote = F,row.names = T)


afterexp_pca<-PCA(t(dat_exp[,rownames(dat_group)]),
                  scale.unit=T,ncp=5,graph=F)
afterexp_pca.plot<-fviz_pca_ind(afterexp_pca,
                                axes=c(1,2),
                                label="none",
                                addEllipses = T,
                                ellipse.level=0.9,
                                habillage = factor(dat_group$DataSet),
                                palette = "aaas",
                                mean.point=F,
                                title="")
afterexp_pca.plot

# afterexp_pca.plot<-afterexp_pca.plot+theme_bw()+
#   theme(legend.direction = "horizontal",legend.position = "top")+
#   xlim(-150,150)+ylim(-150,150)+
#   xlab("Dim1")+ylab("Dim2")
# afterexp_pca.plot
# 
# dat_PCA<-cowplot::plot_grid(before_exp_pca.plot,
#                             afterexp_pca.plot,
#                             ncol=2,nrow=1,
#                             labels=toupper(letters)[1:2],
#                             align = "hv")
# dat_PCA
# 
# ggsave(plot=dat_PCA,     #保存图像
#        "dat_PCA.pdf",
#        width = 10,heigh=5,device=cairo_pdf)

TCGA.for.cox = dat_exp[,1:371]
GSE.for.cox = dat_exp[,372:613]
write.table(TCGA.for.cox,'TCGA_preprocessed_expr.txt',quote = F,row.names = T)
write.table(GSE.for.cox,'GSE_preprocessed_expr.txt',quote = F,row.names = T)

top20gene = read.table('top20genes.txt')
TCGAhub.for.cox = TCGA.for.cox[top20gene$V1,]
GSEhub.for.cox = GSE.for.cox[top20gene$V1,]
write.table(TCGAhub.for.cox,'TCGAhub_preprocessed_expr.txt',quote = F,row.names = T)
write.table(GSEhub.for.cox,'GSEhub_preprocessed_expr.txt',quote = F,row.names = T)

#TCGA cox
library("survival")
library("survminer")
library(dplyr)
library(data.table)
library(stringr)

# install.packages('sjPlot')
library(sjPlot)
tab_df(m[1:5,])
# install.packages('flextable')
library(flextable)


setwd('')
a<-read.table("",check.names = F)
a <- fread("",check.names = F,row.names=1)


GSMM <- read.table('',check.names = F,header = T)

os.data <- read.table('total.osdata.PFS.txt',check.names = F,header = T)

samegene <- intersect(rownames(a),GSMM$gene)

os.data$OS <- as.factor(os.data$OS)
os.data$OS <- relevel(os.data$OS,'0')

a <- a[samegene,]
a<-as.data.frame(t(a))

samesamples <- intersect(rownames(a),os.data$id)
rownames(os.data) <- os.data$id
os.data <- os.data[,-1]

a <- a[samesamples,]
os.data <- os.data[samesamples,]

covariates <- colnames(a)
m<-cbind(os.data,a)

univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(OStime, OS)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = m, id = rownames(m))})


univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         
                         HR <-signif(x$coef[2], digits=2);
                         
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(p.value,HR)
                         names(res)<-c("p.value","HR (95% CI for HR)")
                         return(res)
                       })

res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)
write.csv(res,"cox.csv")
res<-as.data.frame(res)


univ_results_ <- lapply(univ_models,
                        function(x){ 
                          x <- summary(x)
                          
                          p.value<-signif(x$wald["pvalue"], digits=2)
                          
                          HR <-signif(x$coef[2], digits=2);
                          
                          HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                          HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                          
                          res<-c(HR,HR.confint.lower,HR.confint.upper,p.value)
                          names(res)<-c("HR",'Low_95%CI','High_95%CI','Pvalue')
                          return(res)
                        })

res_ <- t(as.data.frame(univ_results_, check.names = FALSE))
res_ <- as.data.frame(res_)
res_$mRNA = rownames(res_)
res_ <- cbind(res_[,5],res_[,-5])
colnames(res_)[1] <- 'mRNA'

library(ggplot2)
library(scales)
library(ggstatsplot)
library(viridis)
head(forest_data)


forest_data <- res_[nrow(res_):1, ]
HR <- forest_data$HR
CI_LL  <- forest_data$`Low_95%CI`
CI_HL <-forest_data$`High_95%CI`
ggplot(data=forest_data,
       aes(x=HR,y=mRNA,
           color=Pvalue))+
  geom_errorbarh(aes(xmax=CI_HL, xmin=CI_LL),
                 color="#A6CEE3",height=0.2,size=0.8)+
  geom_point(aes(x=HR,y=mRNA),size=4,shape=18)+ 
  geom_vline(xintercept = 1,linetype='dashed',size=1.2, color = "black")+ 
  scale_color_viridis() +
  xlab("Hazard Ratio") +
  ylab("Hub genes")

res$p.value<-as.numeric(res$p.value)
o<-data.frame()
for(i in 1:18){    
  m<-res[i,1]
  if(m<0.05){
    k1<-rownames(res)[i]
    o<-rbind(o,k1)
  }
}

library(glmnet)
library(survival)
o1<-as.data.frame(t(a))
x<-o1[rownames(o1)%in%o$X.ADAM9.,]
x<-as.data.frame(t(x))
x<-as.matrix(x)


os.data$OStime <- as.double(os.data$OStime)
os.data$OS <- as.double(os.data$OS)
cvfit = cv.glmnet(x, survival::Surv(os.data$OStime,os.data$OS), 
                  
                  nfold=10,
                  family = "cox"
) 
plot(cvfit)
cvfit$lambda.min 
cvfit$lambda.1se 

fit <- glmnet(x, survival::Surv(os.data$OStime,os.data$OS), 
              family = "cox") 

plot(fit, label = F)

mycol <- rep(c("#223D6C","#D20A13","#FFD121","#088247","#11AA4D","#58CDD9","#7A142C","#5D90BA","#431A3D","#91612D","#6E568C","#E0367A","#D8D155","#64495D","#7CC767"),2)

xmax <- 3.6

plotCoef_plus <- function (beta, norm, lambda, df, dev, label = FALSE, legend = FALSE, xvar = c("norm", 
                                                                                                "lambda", "dev"), xlab = iname, ylab = "Coefficients", ...) 
{
  which = nonzeroCoef(beta)
  nwhich = length(which)
  switch(nwhich + 1, `0` = {
    warning("No plot produced since all coefficients zero")
    return()
  }, `1` = warning("1 or less nonzero coefficients; glmnet plot is not meaningful"))
  beta = as.matrix(beta[which, , drop = FALSE])
  xvar = match.arg(xvar)
  switch(xvar, norm = {
    index = if (missing(norm)) apply(abs(beta), 2, sum) else norm
    iname = "L1 Norm"
    approx.f = 1
  }, lambda = {
    index = log(lambda)
    iname = "Log Lambda"
    approx.f = 0
  }, dev = {
    index = dev
    iname = "Fraction Deviance Explained"
    approx.f = 1
  })
  dotlist = list(...)
  type = dotlist$type
  
  if (legend){
    
    par(xpd = T, mar = par()$mar + c(0,0,0,6))
  }
  
  
  if (is.null(type)) 
    matplot(index, t(beta), lty = 1, lwd = 2,
            xlab = xlab, ylab = ylab, 
            xlim = c(0, xmax), 
            col = mycol,
            type = "l", cex.lab=1.2, cex.axis=1,
            bty="n", ...)
  else matplot(index, t(beta), lty = 1, lwd = 2,
               xlab = xlab, ylab = ylab, 
               xlim = c(0, xmax), 
               col = mycol,
               type = "l", cex.lab=1.2, cex.axis=1,
               bty="n", ...)
  atdf = pretty(index)
  prettydf = approx(x = index, y = df, xout = atdf, rule = 2, 
                    method = "constant", f = approx.f)$y
  axis(3, at = atdf, labels = prettydf, tcl = NA)
  
  if (label) {
    nnz = length(which)
    xpos = max(index)
    pos = 4
    if (xvar == "lambda") {
      xpos = min(index)
      pos = 2
    }
    xpos = rep(xpos, nnz)
    ypos = beta[, ncol(beta)]
    
    text(xpos, ypos, paste(colnames(x)[which]),
         cex = 0.8, 
         
         col = mycol,
         
         pos = pos)
  }
  if (legend) {
    
    legend("topright",
           inset=c(-0.12,0),
           legend = colnames(x), 
           col = mycol, 
           lwd = 3, 
           cex = 1, 
           bty = "n")
  }
  par(xpd=FALSE)
}

plot.glmnet_plus <- function (x, xvar = c("norm", "lambda", "dev"), label = FALSE, legend = FALSE,
                              ...) 
{
  xvar = match.arg(xvar)
  plotCoef_plus(x$beta, lambda = x$lambda, df = x$df, dev = x$dev.ratio, 
                label = label, legend = legend, xvar = xvar, ...)
}


coef.min = coef(cvfit, s = "lambda.min") 
coef.min

active.min = which(coef.min != 0)
geneids <- o$X.ADAM9.[active.min]
geneids

index.min = coef.min[active.min]
index.min

combine <- cbind(geneids, index.min)
write.csv(combine,"gene_index.csv")


#multi-cox
library("survival")
library("survminer")

library(gtsummary)
library(broom)

res <- coxph(survival::Surv(os.data$OStime,os.data$OS)~ ADAM9 +PTK2B
             , data=a)


summary(res)



summary_df <- tidy(res)
tab_df(summary_df)


ping<-as.data.frame(0.14988*a$ADAM9 + (-0.30710)*a$PTK2B
)

rownames(ping)<-rownames(a)
colnames(ping)<-"score"
library(survminer) 
library(survival) 
m<-cbind(os.data,ping)
m<-m[order(m$score),]
list <- c(rep("low", 169), rep("high",170)) %>% factor(., levels = c("low", "high"), ordered = F)
list <- model.matrix(~factor(list)+0)  
colnames(list) <- c("low", "high")

list<-as.data.frame(list)
m<-cbind(list$high,m)
colnames(m)[1]<-"fen"
fit <- survfit(Surv(OStime,OS) ~ fen,  
               data = m) 
summary(fit)
ggsurvplot(fit, data = m,
           conf.int = F,  
           risk.table = TRUE,
           surv.median.line = "hv",
           pval = TRUE) 
ggsurvplot(fit, 
           data = m,  
           conf.int = TRUE, 
           pval = TRUE, 
           add.all = F) 


ggsurvplot(fit, data = m,
           conf.int = TRUE,  
           risk.table = TRUE,
           surv.median.line = "hv",
           pval = TRUE,
           pval.method = T,
           pval.size = 6,
           pval.method.size = 6,
           pval.coord = c(2500,1),
           pval.method.coord = c(2000,1),
           add.all = F,
           palette =c('#5AA3DAFF','#F15854FF'),
           xlab = 'Time(days)', 
           ylab = 'Progression-free survival(%)',
           font.x = 14,
           font.y = 18,
           risk.table.fontsize = 4,
           risk.table.y.text = 1,
           legend.labs = c("A-P Score low","A-P Score high"),
           legend.title = ''
) 

#GEO Cox

library(dplyr)
library(data.table)
library(stringr)
library(survminer) 
library(survival)
library(glmnet)

install.packages('sjPlot')
library(sjPlot)

a<-read.csv("clinical_data.csv")


a.recur <- a[a$Recurr.status==1,]
a.dead <- a[a$Survival.status==1,]

a.recur <- a.recur[,c(4,23,25)]
a.dead <- a.dead[,c(4,20,22)]
sameid <- intersect(a.recur$Affy_GSM,a.dead$Affy_GSM)
a.dead <- a.dead[-c(which(a.dead$Affy_GSM %in% sameid)),]

colnames(a.recur) <- c('id','OS','OStime')
colnames(a.dead) <- c('id','OS','OStime')
recur <- rbind(a.recur,a.dead)

notrecurid <- setdiff(a$Affy_GSM,recur$id)
notrecur <- a[c(which(a$Affy_GSM %in% notrecurid)),]
notrecur <- notrecur[,c(4,20,22)]
colnames(notrecur) <- c('id','OS','OStime')

total.os.data.GSE <- rbind(recur,notrecur)
a <- total.os.data.GSE

a$OS <- as.factor(a$OS)
a$OS <- relevel(a$OS,'0')

a$OS <- as.double(a$OS)
a$OStime <- as.double(a$OStime)

rownames(a) <- a$id
a <- a[,-1]

gene<-read.table("GSE_preprocessed_expr.txt",check.names = F)

samesamples <- intersect(colnames(gene),rownames(a))
a <- a[samesamples,]
gene <- gene[,samesamples]

m<-c("ADAM9",'PTK2B')

k<-gene[rownames(gene)%in%m,]
k<-as.data.frame(t(k))

k<-cbind(a,k)

ping<-as.data.frame(0.14988*k$ADAM9 + (-0.30710)*k$PTK2B
                    
)

rownames(ping)<-rownames(k)
colnames(ping)<-"score"

m<-cbind(k,ping)
m<-m[order(m$score),]

list <- c(rep("low", 121), rep("high", 121)) %>% factor(., levels = c("low", "high"), ordered = F)

list <- model.matrix(~factor(list)+0)  
colnames(list) <- c("low", "high")
list<-as.data.frame(list)
m<-cbind(list$high,m)
colnames(m)[1]<-"fen"


fit <- survfit(Surv(OStime,OS) ~ fen,  
               data = m) 
summary(fit)

ggsurvplot(fit, data = m,
           conf.int = TRUE,  
           risk.table = TRUE,
           surv.median.line = "hv",
           pval = TRUE,
           pval.method = T,
           pval.size = 6,
           pval.method.size = 6,
           pval.coord = c(1250,1),
           pval.method.coord = c(1000,1),
           add.all = F,
           palette =c('#5AA3DAFF','#F15854FF'),
           xlab = 'Time(days)', 
           ylab = 'Progression-free survival(%)',
           font.x = 14,
           font.y = 18,
           risk.table.fontsize = 4,
           risk.table.y.text = 1,
           legend.labs = c("A-P Score low","A-P Score high"),
           legend.title = ''
) 

