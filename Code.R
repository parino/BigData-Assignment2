rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(VIM)

hmeq <- fread("C:/Users/u0117439/Desktop/MSc Statistics/Advanced Analytics in Bussiness/Assignments/Assignment 2/hmeq.csv",
              sep=",",header = T)

hmeq<-hmeq %>%  mutate_if(is.character, as.factor) 

head(hmeq)


########################## Exploratory analysis ###############################

table(hmeq$BAD)

summary(hmeq[,c("BAD","LOAN","MORTDUE","VALUE","YOJ","DEROG",
                "DELINQ","CLAGE","NINQ","CLNO","DEBTINC")])

xtabs(~BAD + REASON, data = hmeq)
xtabs(~BAD + JOB, data = hmeq)


dfmelt <- melt(hmeq, measure.vars=c(2:4)) 

ggplot(dfmelt, aes(x=" ", y=value,fill=variable))+
  geom_boxplot()+
  facet_grid(.~variable)+
  labs(x="X (binned)")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))

dfmelt2 <- melt(hmeq, measure.vars=c(7:9,11,12)) 
ggplot(dfmelt2, aes(x=" ", y=value,fill=variable))+
  geom_boxplot()+
  facet_grid(.~variable)+
  labs(x="X (binned)")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))




############################# Missing data ########################

#Visual representation

aggr_plot <- aggr(hmeq, col=c('navyblue','red'), sortVars=TRUE, numbers=TRUE, labels=names(hmeq), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))

#Create new category "Unknown"
levels <- levels(hmeq$REASON)
levels[1] <- "Unknown"
hmeq$REASON <- factor(hmeq$REASON, levels = levels)
hmeq$REASON[is.na(hmeq$REASON)] <- "Unknown"
hmeq[hmeq$REASON=="",]$REASON<-"Unknown"

levels <- levels(hmeq$JOB)
levels[1] <- "Unknown"
hmeq$JOB <- factor(hmeq$JOB, levels = levels)
hmeq$JOB[is.na(hmeq$JOB)] <- "Unknown"
hmeq[hmeq$JOB=="",]$JOB<-"Unknown"


#knn imputation 
hmeq_clean<-knnImputation(hmeq, k = 10, scale = T, 
                            meth = "weighAvg",distData = NULL)
summary(hmeq_clean[,c("BAD","LOAN","MORTDUE","VALUE","YOJ","DEROG",
                "DELINQ","CLAGE","NINQ","CLNO","DEBTINC")])

#round
hmeq_clean<-as.data.table(hmeq_clean)
cols <- c("LOAN","MORTDUE","VALUE","DEROG","DELINQ","NINQ","CLNO")
hmeq_clean[,(cols) := round(.SD,0), .SDcols=cols]
hmeq_clean


###################################### Logistic regression ###############################

# index
set.seed(123); index = sample(1:nrow(hmeq_clean), size = floor(nrow(hmeq_clean)*0.75))
df_tr = hmeq_clean[index,]
df_ts = hmeq_clean[-index,]


fit<-glm(BAD~.,data = df_tr,family="binomial")
summary(fit)

library(ROCR)

#ROC curve
p <- predict(fit, newdata=df_ts, type="response")
pr <- prediction(p, df_ts$BAD)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf);abline(a=0,b=1)

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
