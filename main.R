rm(list=ls())
library(RSQLite)
library(randomForest)

setwd("~/Dropbox/data/kaggle/practicefusion/mycode")

makeconfusion <- function(y,pr){
  prediction <- ifelse( pr> 0.50, TRUE, FALSE)
  confusion  <- table(y,prediction)
  confusion  <- cbind(confusion, c(1 - confusion[1,1]/sum(confusion[1,]), 1 - confusion[2,2]/(sum(confusion[2,]))))
  confusion  <- as.data.frame(confusion)
  names(confusion) <- c('FALSE', 'TRUE', 'class.error')
  confusion
}
createSplits<-function(dataset,prop=.7,seed=42){
  set.seed(seed)
  nobs <- nrow(dataset) 
  trainidx <- sample(nrow(dataset), prop*nobs) 
  testidx <- sample(setdiff(seq_len(nrow(dataset)), trainidx), (1-prop)*nobs) 
  d.train <- dataset[trainidx,]
  d.test <- dataset[testidx,] 
  output <- list(train=d.train,test=d.test)
  return (output)
}



n <- dbDriver("SQLite")
con <- dbConnect(n, dbname="compData.db")

sql = "select dmIndicator, avg(2014-YearOfBirth) as age, gender, avg(BMI) as BMI, avg(weight) as Weight, avg(height) as Height
, avg(systolicBP) as SystBP, avg(DiastolicBP) as DiastBP
, avg(RespiratoryRate) as RespRate, PatientGUID
from training_patientTranscript 
where 1=1 and BMI < 100 and bmi > 0
group by PatientGuid"
pdata <- dbGetQuery(con, sql)

# data work
pdata[pdata$Gender == "F",]$Gender <- 0
pdata[pdata$Gender == "M",]$Gender <- 1
pdata$Gender <- as.factor(pdata$Gender)
pdata$dmIndicator <- as.factor(pdata$dmIndicator)
#simple impute RespRate where = 0
impute <- function (a, a.impute){
  ifelse ((a==0), a.impute, a)
}
imp <- lm(RespRate ~ ., data=pdata[,-c(ncol(pdata))], subset=pdata$RespRate>0)
pred.imp <- predict (imp, pdata) 
pdata$RespRate <- impute (pdata$RespRate, pred.imp)

#plot(pdata$BMI,pdata$age)
attach(pdata)

# 1) split random 20% undiagnosed out of total pool of undiagnosed
set.seed(42)
allun <- pdata[which(dmIndicator==0),]
sun_idx <- sample(nrow(allun),nrow(pdata)*.2,replace=FALSE)
unsam <- allun[sun_idx,]
# remove our undiag patients from main data
rundata <- pdata[!(PatientGuid %in% unsam$PatientGuid),]
#remove the PGuids from each dataset, dont need anymore
rundata$PatientGuid <- NULL
unsam$PatientGuid <- NULL

# 2) split remaining ~8000 into train and test
d <- createSplits(rundata)
train <- d$train
test <- d$test

# 3) build model.glm logit , and remove weight and height, correlated with BMI
model.glm <- glm(dmIndicator ~ ., data=train[,-c(5,6)], family="binomial")
pr = predict(model.glm,test,type="response")  
conf.glm <- makeconfusion(test$dmIndicator,pr)
overall.error.glm <- (conf.glm[2,1]+conf.glm[1,2])/nrow(test)
diab.error.glm <- conf.glm[2,3]
conf.glm
overall.error.glm
diab.error.glm

# 4) build model.rf randomForest
model.rf <- randomForest(y=as.factor(train$dmIndicator), x=train[,-1],ntree=1000, mtry=2,
                       importance=TRUE,
                       na.action=na.roughfix,
                       replace=FALSE
)
pr <- predict(model.rf,test,type="prob")
conf.rf <- makeconfusion(test$dmIndicator,pr[,2])
diab.error.rf <- conf.rf[2,3]
overall.error.rf <- (conf.rf[2,1]+conf.rf[1,2])/nrow(test)
conf.rf
overall.error.rf
diab.error.rf



# 5) pick best by diab class error
if (diab.error.glm > diab.error.rf){
  print ("RF won")
  winmodel = model.rf
  prtype = "prob"
  winprederror = diab.error.rf
} else { 
  print ("GLM won")
  winmodel = model.glm
  prtype = "response"
  winprederror = diab.error.glm
}
# 6) score our undiagnosed dataset and get predicted probabilities
probs2 <- ifelse(predict(winmodel,unsam,type=prtype)[,2]>= 0.50, TRUE, FALSE)
summary(probs2)
# 7) calculate proportion above .5 as likely undiagnosed. 
pred_diab <- sum(probs2)/length(probs2)
print(paste("Predicted proportion having diabetes:",round(pred_diab,3)))
#adjust for prediction error of model
print(paste("Adjusted proportion for model error:",round(pred_diab*(1-winprederror),3)))

