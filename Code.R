rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(VIM)
library(DMwR)

df <- fread("C:/Users/u0117439/Desktop/MSc Statistics/Advanced Analytics in Bussiness/Assignments/Assignment 2/hmeq.csv",
              sep=",",header = T)

df<-df %>%  mutate_if(is.character, as.factor) 

head(df)


########################## Exploratory analysis ###############################

table(df$BAD)

summary(df[,c("BAD","LOAN","MORTDUE","VALUE","YOJ","DEROG",
                "DELINQ","CLAGE","NINQ","CLNO","DEBTINC")])

xtabs(~BAD + REASON, data = df)
xtabs(~BAD + JOB, data = df)


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



############################# Missing data ########################

#Visual representation

aggr_plot <- aggr(df, col=c('navyblue','red'), sortVars=TRUE, numbers=TRUE, labels=names(df), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))

#Create new category "Unknown"
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

# index
set.seed(123); index = sample(1:nrow(df_clean), size = floor(nrow(df_clean)*0.80))
df_tr = df_clean[index,]
df_ts = df_clean[-index,]

#knn imputation 
df_clean<-knnImputation(df_tr, k = 10, scale = T, 
                          meth = "weighAvg",distData = NULL)
summary(df_clean[,c("BAD","LOAN","MORTDUE","VALUE","YOJ","DEROG",
                      "DELINQ","CLAGE","NINQ","CLNO","DEBTINC")])

#round
df_clean<-as.data.table(df_clean)
cols <- c("LOAN","MORTDUE","VALUE","DEROG","DELINQ","NINQ","CLNO")
df_clean[,(cols) := round(.SD,0), .SDcols=cols]
df_clean


###################################### Logistic regression ###############################




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

