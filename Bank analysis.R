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

prop.table(table(y,marital),2)
plot(y~marital,col=c("red","blue"))

t(aggregate(balance~y,data=bank,summary))
plot(balance~y,col=c("red","blue"))

prop.table(table(y,education),2)
plot(y~education,col=c("red","blue"))

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

#Make a 80-20 split
newbank = bank[,c('job','marital','education','default','balance','housing','loan','contact','month','duration','poutcome','y')]
dim(newbank)
set.seed(1234)
index<-sample(1:nrow(newbank),round(.8*nrow(newbank)),replace=FALSE)
test<-newbank[-index,]
train<-newbank[index,]

