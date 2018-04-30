rm(list = ls())


library(pastecs)
library(gridExtra)
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(PerformanceAnalytics)
library(VIM)
library(DMwR)
library(ROCR)




df <- fread("C:/Users/u0117439/Desktop/MSc Statistics/Advanced Analytics in Bussiness/Assignments/Assignment 2/hmeq.csv",
              sep=",",header = T)

df<-df %>%  mutate_if(is.character, as.factor) 

head(df)


########################## Exploratory analysis ###############################

table(df$BAD)

summary(df[,c("BAD","LOAN","MORTDUE","VALUE","YOJ","DEROG",
                "DELINQ","CLAGE","NINQ","CLNO","DEBTINC")])


options(digits=1)
options(scipen=999)
stat.desc(df) 

xtabs(~BAD + REASON, data = df)
xtabs(~BAD + JOB, data = df)


plot1<-ggplot(df, aes(x=factor(BAD), y=LOAN))+
  geom_boxplot()+
  labs(x="BAD")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))
plot2<-ggplot(df, aes(x=factor(BAD), y=VALUE))+
  geom_boxplot()+
  labs(x="BAD")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))
plot3<-ggplot(df, aes(x=factor(BAD), y=MORTDUE))+
  geom_boxplot()+
  labs(x="BAD")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))
grid.arrange(plot1, plot2, plot3, ncol=3)

dfmelt <- melt(df, measure.vars=c(2:4)) 

ggplot(dfmelt, aes(x=factor(BAD), y=value,fill=variable))+
  geom_boxplot()+
  facet_grid(.~variable)+
  labs(x="X (binned)")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))

dfmelt2 <- melt(df, measure.vars=c(7:9,11,12)) 
ggplot(dfmelt2, aes(x=factor(BAD), y=value,fill=variable))+
  geom_boxplot()+
  facet_grid(.~variable)+
  labs(x="X (binned)")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))


#Correlation of continuous variables
chart.Correlation(df[,c("LOAN","MORTDUE","VALUE","YOJ","CLAGE","DEBTINC")], histogram=TRUE, pch=19)


############################# Missing data ########################

#Visual representation

aggr_plot <- aggr(df, col=c('navyblue','red'), sortVars=TRUE, numbers=TRUE, labels=names(df), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))

#Create new category "Unknown" for REASON and JOB
levels <- levels(df$REASON)
levels[1] <- "Unknown"
df$REASON <- factor(df$REASON, levels = levels)
df$REASON[is.na(df$REASON)] <- "Unknown"
df[df$REASON=="",]$REASON<-"Unknown"

levels <- levels(df$JOB)
levels[1] <- "Unknown"
df$JOB <- factor(df$JOB, levels = levels)
df$JOB[is.na(df$JOB)] <- "Unknown"
df[df$JOB=="",]$JOB<-"Unknown"


#Split dataset 
#70-30 split
set.seed(123); index = sample(1:nrow(df), size = floor(nrow(df)*0.70))
df_tr = df[index,]
df_ts = df[-index,]


#knn imputation 
#df_clean<-knnImputation(df_tr, k = 10, scale = T, 
#                          meth = "weighAvg",distData = NULL)
#round
#df_clean<-as.data.table(df_clean)
#cols <- c("LOAN","MORTDUE","VALUE","DEROG","DELINQ","NINQ","CLNO")
#df_clean[,(cols) := round(.SD,0), .SDcols=cols]
#df_clean


#median imputation of training set
num_cols <- names(df_tr)[sapply(df_tr, is.numeric)]
for(col in num_cols) {
  set(df_tr, i = which(is.na(df_tr[[col]])), j = col, value = median(df_tr[[col]], na.rm=TRUE))
}

summary(df_tr[,c("BAD","LOAN","MORTDUE","VALUE","YOJ","DEROG",
                      "DELINQ","CLAGE","NINQ","CLNO","DEBTINC")])

#median imputation of test set (with training median)
num_cols_test <- names(df_ts)[sapply(df_ts, is.numeric)]
for(col in num_cols_test) {
  set(df_ts, i = which(is.na(df_ts[[col]])), j = col, value = median(df_tr[[col]], na.rm=TRUE))
}


###################################### Outliers ###############################



###################################### Logistic regression ###############################

fit<-glm(BAD~.,data = df_tr,family="binomial")
summary(fit)


#ROC curve
p <- predict(fit, newdata=df_ts, type="response")
pr <- prediction(p, df_ts$BAD)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf);abline(a=0,b=1)

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

