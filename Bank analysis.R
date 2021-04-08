library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(GGally)
library(gplots)
library(MASS)
library(tidyverse)
library(car)
library(epitools)
library(glmnet)
library(reshape2)
library(caret)
library(ROCR)
library(randomForest)
setwd("C:/Users/wrf0/Documents/SMU Data Science Program/Applied Statistics/Project 2")
bank <- read.csv("Bank_Full.csv",header=T)
head(bank)
str(bank)

bank$y = factor(bank$y,levels=c('no','yes')) #last level is the success
bank$job=as.factor(bank$job)
bank$marital=as.factor(bank$marital)
bank$education=as.factor(bank$education)
bank$default=as.factor(bank$default)
bank$housing=as.factor(bank$housing)
bank$loan=as.factor(bank$loan)
bank$contact=as.factor(bank$contact)
bank$month=as.factor(bank$month)
bank$poutcome=as.factor(bank$poutcome)

attach(bank)
summary(bank)
prop.table(table(y,marital),2)
plot(y~marital,col=c("red","blue"))

t(aggregate(balance~y,data=bank,summary))
plot(balance~y,col=c("red","blue"))

A

prop.table(table(y,default),2)
plot(y~default,col=c("red","blue")) #Looks good in separation

prop.table(table(y,housing),2)
plot(y~housing,col=c("red","blue")) #Looks good in separation

prop.table(table(y,loan),2)
plot(y~loan,col=c("red","blue")) #Looks good in separation

prop.table(table(y,contact),2)
plot(y~contact,col=c("red","blue"))  #Looks good in separation

prop.table(table(y,month),2)
plot(y~month,col=c("red","blue"))  #possible weak one

prop.table(table(y,poutcome),2)
plot(y~poutcome,col=c("red","blue")) #Looks good in separation

t(aggregate(duration~y,data=bank,summary))
plot(duration~y,col=c("red","blue"))  #Looks good in separation

t(aggregate(campaign~y,data=bank,summary)) 
plot(campaign~y,col=c("red","blue"))   #No clear separation

t(aggregate(age~y,data=bank,summary))
plot(age~y,col=c("red","blue"))   #No clear separation

t(aggregate(day~y,data=bank,summary))
plot(day~y,col=c("red","blue")) #No clear separation

t(aggregate(pdays~y,data=bank,summary))
plot(pdays~y,col=c("red","blue"))  #No clear separation

t(aggregate(previous~y,data=bank,summary))
plot(previous~y,col=c("red","blue"))  #No clear separation

prop.table(table(y,job),2)
plot(y~job,col=c("red","blue"))  #will use job for a try

#Let's look at the heatmap by using all continuous variables.
cormat=round(cor(bank[,c('age','balance','day','duration','campaign','pdays','previous')]),2)
melted_cormat <- melt(cormat)
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
upper_tri
melted_cormat <- melt(upper_tri, na.rm = TRUE)

reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

print(ggheatmap)
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
#Looked at the correlation plot, we would like to drop either pdays or previous variables.Since these two variables 
#don't show correlation to y, we will not include them in our model.

#Let's take a look at PCA
pc.result<-prcomp(bank[,c('age','balance','day','duration','campaign','pdays','previous')],scale.=TRUE)
pc.scores<-pc.result$x
pc.scores<-data.frame(pc.scores)
pc.scores$y<-bank$y

ggplot(data = pc.scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col=y), size=1)+
  ggtitle("PC1 VS PC2")

ggplot(data = pc.scores, aes(x = PC3, y = PC2)) +
  geom_point(aes(col=y), size=1)+
  ggtitle("PC3 VS PC2")

ggplot(data = pc.scores, aes(x = PC3, y = PC1)) +
  geom_point(aes(col=y), size=1)+
  ggtitle("PC3 VS PC1")

ggplot(data = pc.scores, aes(x = PC3, y = PC4)) +
  geom_point(aes(col=y), size=1)+
  ggtitle("PC3 VS PC4")

pc.result<-prcomp(bank[,c('balance','duration')],scale.=TRUE)
pc.scores<-pc.result$x
pc.scores<-data.frame(pc.scores)
pc.scores$y<-bank$y

ggplot(data = pc.scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col=y), size=1)+
  ggtitle("PC1 VS PC2")


#No clear separation between PCAs, so by using only continuous variables will not provide good prediction result. 

#Check multiconllinearity between categorical variables.
#Check housing & loan, default & loan, default & housing.

prop.table(table(housing,loan),2)
plot(housing~loan,col=c("purple","green"))
table(housing,loan)

prop.table(table(default,loan),2)
plot(default~loan,col=c("purple","green"))
table(default,loan)

prop.table(table(housing,default),2)
plot(housing~default,col=c("purple","green"))
table(housing,default)
#Hard to tell, we will run VIF after we build the model

#Object 1
#Make a 80-20 split
newbank = bank[,c('job','marital','education','default','balance','housing','loan','contact','month','duration','poutcome','y')]
dim(newbank)
set.seed(1234)
index<-sample(1:nrow(newbank),round(.8*nrow(newbank)),replace=FALSE)
test<-newbank[-index,]
train<-newbank[index,]

renfeng.full.log = glm(y~.,family='binomial',data=newbank)
renfeng.step.log<-renfeng.full.log %>% stepAIC(trace=FALSE)
coef(renfeng.full.log)
coef(renfeng.step.log)
vif(renfeng.step.log)  
vif(renfeng.full.log)  

#LASSO model
dat.train.x <- model.matrix(y~job+marital+education+default+balance+housing+loan+contact+month+duration+poutcome-1,train)
dat.train.y<-train[,c('y')]
cvfit <- cv.glmnet(dat.train.x, dat.train.y, family = "binomial", type.measure = "class", nlambda = 1000)
plot(cvfit)
coef(cvfit, s = "lambda.min")
print(cvfit)

cvfit$cvm[which(cvfit$lambda==cvfit$lambda.min)]

cvfit$lambda.min

finalmodel<-glmnet(dat.train.x, dat.train.y, family = "binomial",lambda=cvfit$lambda.min)
coef(finalmodel)
#We will use the predictors in final model to build a new model to do the hypothesis test.
newmodel = glm(y~job+month+marital+education+balance+housing+loan+contact+poutcome+duration,family='binomial',data=newbank)
summary(newmodel)
confint.default(newmodel,level = 0.95)
exp(cbind("Odds ratio" = coef(newmodel), confint.default(newmodel, level = 0.95)))
#tmp_coeffs <- coef(cvfit, s = "lambda.min")
#data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
vif(newmodel)

dat.test.x<-model.matrix(y~job+marital+education+balance+default+housing+loan+contact+month+duration+poutcome-1,test)
fit.pred.lasso <- predict(finalmodel, newx = dat.test.x, type = "response")
fit.pred.step<-predict(renfeng.step.log,newdata=test,type="response")

cutoff<-0.5
class.lasso<-factor(ifelse(fit.pred.lasso>cutoff,"yes","no"),levels=c("no","yes"))
class.step<-factor(ifelse(fit.pred.step>cutoff,"yes","no"),levels=c("no","yes"))
confusionMatrix(class.lasso,test$y)
confusionMatrix(class.step,test$y)

results.lasso<-prediction(fit.pred.lasso, test$y,label.ordering=c("no","yes"))
roc.lasso = performance(results.lasso, measure = "tpr", x.measure = "fpr")
plot(roc.lasso,colorize = TRUE)
abline(a=0, b= 1)

results.step<-prediction(fit.pred.step, test$y,label.ordering=c("no","yes"))
roc.step = performance(results.step, measure = "tpr", x.measure = "fpr")
plot(roc.step,colorize = TRUE)
abline(a=0, b= 1)

lasso.auc.train <- performance(results.lasso, measure = "auc")
lasso.auc.train <- lasso.auc.train@y.values
step.auc.train <- performance(results.step, measure = "auc")
step.auc.train <- step.auc.train@y.values
plot(roc.lasso,colorize = TRUE)
plot(roc.step,col="orange", add = TRUE,colorize = TRUE)
legend("bottomright",legend=c("Lasso","Stepwise"),col=c("black","orange"),lty=1,lwd=1)
abline(a=0, b= 1)  #Ref line indicating poor performance
text(x = .40, y = .6,paste("LASSO.AUC = ", round(lasso.auc.train[[1]],3), sep = ""))
text(x = .40, y = .7,paste("STEP.AUC = ", round(step.auc.train[[1]],3), sep = ""))

#Changed the cutoff value to 0.09656392 based on ROC curve
cutoff<-0.09656392
class.lasso<-factor(ifelse(fit.pred.lasso>cutoff,"yes","no"),levels=c("no","yes"))
class.step<-factor(ifelse(fit.pred.step>cutoff,"yes","no"),levels=c("no","yes"))
confusionMatrix(class.lasso,test$y)
confusionMatrix(class.step,test$y)

#Object 2
#We will use job+month+marital+education+balance+housing+loan+contact+poutcome+duration plus interaction items
#and quadratic items to make model more complicated.
#poutcome:duration, balance:loan, balance:housing, job:education
train.x <- model.matrix(y~job+marital+education+balance+housing+loan+contact+month+duration+poutcome
                            +poutcome:duration+balance:loan+balance:housing+job:education-1,train)
train.y<-train[,c('y')]
cvfit.interaction <- cv.glmnet(train.x, train.y, family = "binomial", type.measure = "class", nlambda = 1000)
plot(cvfit.interaction)
coef(cvfit.interaction, s = "lambda.min")
print(cvfit.interaction)

cvfit.interaction$cvm[which(cvfit.interaction$lambda==cvfit.interaction$lambda.min)]

cvfit.interaction$lambda.min

interaction.model<-glmnet(train.x, train.y, family = "binomial",lambda=cvfit.interaction$lambda.min)
coef(interaction.model)

#job, marital,education,balance,housing,loan,contact,month,duration,poutcome,poutcome:duration,job:education were left in the model
test.x<-model.matrix(y~job+marital+education+balance+housing+loan+contact+month+duration+poutcome+
                       poutcome:duration+job:education+balance:loan+balance:housing-1,test)
fit.interaction.lasso <- predict(interaction.model, newx = test.x, type = "response")

cutoff<-0.5
interaction.lasso<-factor(ifelse(fit.interaction.lasso>cutoff,"yes","no"),levels=c("no","yes"))

confusionMatrix(interaction.lasso,test$y)


results.interaction<-prediction(fit.interaction.lasso, test$y,label.ordering=c("no","yes"))
roc.interaction = performance(results.interaction, measure = "tpr", x.measure = "fpr")
plot(roc.interaction,colorize = TRUE)
abline(a=0, b= 1)

interaction.auc.train <- performance(results.interaction, measure = "auc")
interaction.auc.train <- interaction.auc.train@y.values

plot(roc.interaction,colorize = TRUE)

legend("bottomright",legend=c("Interaction"),col=c("black"),lty=1,lwd=1)
abline(a=0, b= 1) 
text(x = .40, y = .6,paste("LASSO.AUC = ", round(interaction.auc.train[[1]],3), sep = ""))

#The model with interaction items is not as well as the model in object 1, let's try different model.
train.x.2 <- model.matrix(y~job+marital+education+balance+housing+loan+contact+month+duration+poutcome
                          +marital:housing+contact:job+I(balance^2)+I(duration^2)-1,train)
train.y<-train[,c('y')]
cvfit.interaction.2 <- cv.glmnet(train.x.2, train.y, family = "binomial", type.measure = "class", nlambda = 1000)
plot(cvfit.interaction.2)
coef(cvfit.interaction.2, s = "lambda.min")
print(cvfit.interaction.2)

cvfit.interaction.2$cvm[which(cvfit.interaction.2$lambda==cvfit.interaction.2$lambda.min)]

cvfit.interaction.2$lambda.min

interaction.model.2<-glmnet(train.x.2, train.y, family = "binomial",lambda=cvfit.interaction.2$lambda.min)
coef(interaction.model.2)


test.x.2<-model.matrix(y~job+marital+education+balance+housing+loan+contact+month+duration+poutcome
                       +marital:housing+contact:job+I(balance^2)+I(duration^2)-1,test)
fit.interaction.lasso.2 <- predict(interaction.model.2, newx = test.x.2, type = "response")

cutoff<-0.5
interaction.lasso.2<-factor(ifelse(fit.interaction.lasso.2>cutoff,"yes","no"),levels=c("no","yes"))

confusionMatrix(interaction.lasso.2,test$y)


results.interaction.2<-prediction(fit.interaction.lasso.2, test$y,label.ordering=c("no","yes"))
roc.interaction.2 = performance(results.interaction.2, measure = "tpr", x.measure = "fpr")
plot(roc.interaction.2,colorize = TRUE)
abline(a=0, b= 1)

interaction.auc.train.2 <- performance(results.interaction.2, measure = "auc")
interaction.auc.train.2 <- interaction.auc.train.2@y.values

plot(roc.interaction.2,colorize = TRUE)

legend("bottomright",legend=c("Interaction"),col=c("black"),lty=1,lwd=1)
abline(a=0, b= 1) 
text(x = .40, y = .6,paste("LASSO.AUC = ", round(interaction.auc.train.2[[1]],3), sep = ""))

cutoff<-0.1083344
interaction.lasso.2<-factor(ifelse(fit.interaction.lasso.2>cutoff,"yes","no"),levels=c("no","yes"))
confusionMatrix(interaction.lasso.2,test$y)

#Let's find the optimal cutoff point for the model we use in object 1 and 2
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.interaction.2, results.interaction.2)) #Model 2 in object 2

print(opt.cut(roc.interaction, results.interaction))  #Model 1 in object 2

print(opt.cut(roc.lasso, results.lasso)) #LASSO model in object 1

print(opt.cut(roc.step, results.step))  #Stepwise model in object 1

#LDA or QDA with continuous variables.
bank.numeric = bank[,c('age','balance','day','duration','campaign','pdays','previous','y')]
head(bank.numeric)  #balance and duration are the only continuous variables we used in previous models.

#Let's fit balance and duration first
plot(bank.numeric[,c('balance','duration')],col=bank.numeric$y,main='LDA') #There is no clear separation.

lda.model = lda(y~balance+duration,data=train)
lda.predict=predict(lda.model,newdata=test)$class
confusionMatrix(lda.predict,test$y)  #Sensitivity is pretty low.

#Let's try to fit all continuous variables.
set.seed(1234)
index.lda<-sample(1:nrow(bank),round(.8*nrow(bank)),replace=FALSE)
test.lda<-bank[-index.lda,]
train.lda<-bank[index.lda,]
lda.model.full = lda(y~age+balance+day+duration+campaign+pdays+previous,data=train.lda)
lda.predict.full=predict(lda.model.full,newdata=test.lda)$class
confusionMatrix(lda.predict.full,test.lda$y)  #Sensitivity is pretty low.

#Let's try QDA
qda.model.full = qda(y~age+balance+day+duration+campaign+pdays+previous,data=train.lda)
qda.predict.full=predict(qda.model.full,newdata=test.lda)$class
confusionMatrix(qda.predict.full,test.lda$y)  #Best in three models

qda.model = qda(y~balance+duration,data=train.lda)
qda.predict=predict(qda.model,newdata=test.lda)$class
confusionMatrix(qda.predict,test.lda$y)  #not well predicted, similar as LDA

#I used full QDA model to create ROC plot.
qda.predict.full.1=predict(qda.model.full,newdata=test.lda)
preds.QDA<- qda.predict.full.1$posterior
preds.QDA <- as.data.frame(preds.QDA)
results.QDA<-prediction(preds.QDA[,2], test.lda$y)
roc.QDA = performance(results.QDA, measure = "tpr", x.measure = "fpr")
auc.QDA <- performance(results.QDA, measure = "auc")
auc.QDA <- auc.QDA@y.values
plot(roc.QDA,colorize = TRUE)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.QDA[[1]],3), sep = ""))

#Random Forest model, we still use newbank dataset after we completed the EDA.
model.rf=randomForest(y~.,data=train,mtry=7,ntree=1000,importance=T)
pred.rf=predict(model.rf,newdata=test,type='prob')
results.rf<-prediction(pred.rf[,2], test$y)
roc.rf = performance(results.rf, measure = "tpr", x.measure = "fpr")
auc.rf <- performance(results.rf, measure = "auc")
auc.rf <- auc.rf@y.values
plot(roc.rf,colorize = TRUE)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.rf[[1]],3), sep = ""))
confusionMatrix(predict(model.rf,newdata=test),test$y) 




print(opt.cut(roc.rf, results.rf))

#set cutoff=0.141,mtry=7
model.rf=randomForest(y~.,data=train,mtry=7,ntree=1000,importance=T,cutoff=c(.859,0.141))
pred.rf=predict(model.rf,newdata=test,type='prob')
results.rf<-prediction(pred.rf[,2], test$y)
roc.rf = performance(results.rf, measure = "tpr", x.measure = "fpr")
auc.rf <- performance(results.rf, measure = "auc")
auc.rf <- auc.rf@y.values
plot(roc.rf,colorize = TRUE)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.rf[[1]],3), sep = ""))
confusionMatrix(predict(model.rf,newdata=test),test$y) 
varImpPlot(model.rf)


#Use caret package to do cross validation. Don't need to run the code below
set.seed(1234)

trControl <- trainControl(method = "cv", number = 10, search = "grid")

#Search best mtry
tuneGrid <- expand.grid(.mtry = c(3: 11))
rf_mtry <- train(y~.,
                 data = train,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 ntree = 100)
print(rf_mtry)
max(rf_mtry$results$Accuracy)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#Search best maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 15)) {
  rf_maxnode <- train(y~.,
                      data = train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      maxnodes = maxnodes,
                      ntree = 100)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(15: 25)) {
  rf_maxnode <- train(y~.,
                      data = train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      maxnodes = maxnodes,
                      ntree = 100)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)
#search the best ntrees, when maxnodes=15, it gave the best result
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500,1000)) {
  rf_maxtrees <- train(y~.,
                       data = train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       maxnodes = 15,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)
