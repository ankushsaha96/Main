rm(list = ls())
install.packages("dplyr")
install.packages("usdm")
install.packages("pROC")
library(dplyr)
library(ggplot2)
library(corrgram)
library(scales)
library(C50)
library(caret)
library(usdm)
library(class)
library(pROC)
#getwd()
setwd("Enter your Path")

############################################loading the Data#############################################
x = read.csv("train.csv", header = T)
xt = x
#x = xt

############################################Preparing Data#############################################
row.names(x) = x$ID_code
x = x[,-1]
target = x$target
x = x[,-1]
x = cbind(x,target)
x$target = as.factor(x$target)
############################################Data Pre-Processing#############################################

#####Missing Value Analysis#####

# mval = data.frame(apply(x,2,function(y){sum(is.na(y))}))
# mval$collumns = row.names(mval)
# names(mval)[1] = "missing percentage"
# mval$`missing percentage` = (mval$`missing percentage`*100)/nrow(x)
# mval = mval[,c(2,1)]
# mval = mval[order(-mval$`missing percentage`),]

#No missing value found

#####Outlier Analysis#####
numer = sapply(x, is.numeric)
numer = x[,numer]
cnames = colnames(numer)
#cnames = cnames[1:200]
# for (i in 1:14)
# {
#    assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i])), data = subset(x))+
#             stat_boxplot(geom = "errorbar", width = 0.5) +
#             geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
#                          outlier.size=1, notch=FALSE) +
#             theme(legend.position="top")+
#             labs(y=cnames[i],x="target")+
#             ggtitle(paste("Box plot of responded for",cnames[i])))
# }
# gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,gn7,ncol=7)
# gridExtra::grid.arrange(gn8,gn9,gn10,gn11,gn12,gn13,gn14,ncol=7)


for (i in cnames) {
  print(i)
  val = x[,i][x[,i] %in% boxplot.stats(x[,i])$out]
  x = x[which(!x[,i] %in% val),]
}

#####Feature Selection#####

#corellation test
#  corrgram(x[5000:15000,1:50], order = F,
#           upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
# corrgram(x[5000:15000,30:80], order = F,
#           upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#  corrgram(x[5000:15000,50:100], order = F,
#           upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#  corrgram(x[5000:15000,70:120], order = F,
#           upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#  corrgram(x[5000:15000,100:150], order = F,
#           upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#  corrgram(x[5000:15000,120:170], order = F,
#           upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#  corrgram(x[5000:15000,150:200], order = F,
#           upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
# 

#####Feature Scaling#####

# ggplot(x,aes_string(x=x$var_69))+
#   geom_histogram(fill="cyan", colour = "black") + geom_density() +
#   scale_y_continuous(breaks=pretty_breaks(n=10)) +
#   scale_x_continuous(breaks=pretty_breaks(n=10))+
#   theme_bw() + xlab("var") + ylab("Frequency") + ggtitle("hist") +
#   theme(text=element_text(size=20))



for(i in cnames){
  print(i)
  x[,i] = (x[,i] - min(x[,i]))/
    (max(x[,i] - min(x[,i])))
}
#####Feature Sampling#####

set.seed(1234)
train_index = createDataPartition(x$target, p= 0.70, list = FALSE)
train = x[train_index,]
test = x[-train_index,]


############################################Developing Model#############################################

#####Logistic regression Model#####

#vifcor(x[,-201],th =0.9)
lr_model = glm(target~., data = train, family = "binomial")

lr_predict = predict(lr_model, test[,1:200], type= "response")
lr_predict = ifelse(lr_predict > 0.5, 2, 1)
a= as.numeric(test$target)
b =as.numeric(lr_predict) 
#summary(lr_predict)
#summary(lr_model)

#####model testing#####
#AUC
roc_obj = roc(test$target,lr_predict)
auc(roc_obj)


confusionMatrixlr = table(a,b)
confusionMatrix(confusionMatrixlr)

Mape = function(y, z){
  mean(abs((y - z)/y))*100
}


Mape(a,b)
#MAPE = 4.697%
#Accuracy = 91.69%
#FNR = 3792/(3792+46820) = 7.49%
#FPR = 571/(571+1338) = 29.91%
#AUC = 0.8592
#Precision = 1338/(571+1338) = 70.08%
#Recall = 1338/(1338+3792) = 26.08%


#####Decision tree classification(C5.0 model)#####
# c50 = C5.0(target ~.,train,trails = 100, rules = TRUE)
# c50_predict = predict(c50, test[,-201],type = "class")
# summary(c50)
# 
# b = as.numeric(c50_predict)
# 
#####model testing#####
# roc_obj = roc(a,b)
# auc(roc_obj)
# 
# confusionMatrix_c50 = table(test$target,c50_predict)
# confusionMatrix(confusionMatrix_c50)
# 
# 
# Mape(a,b)
# #AUC = .5286
# #MAPE = 5.6187%
# #accuracy = 89.83%
# #FNR = 4776/(46828+4776) = 9.25%
# #FPR = 563/(563+354) = 61.4%
# #Precision = 354/(563+354) = 38.6%
# #Recall = 354/(354+4776) = 6.9%


#####KNN#####

# KNN_predict = knn(train[,1:200], test[,1:200], train$target, k=3)
# 
# b = as.numeric(KNN_predict)
# 
#####model testing#####
# roc_obj = roc(a,b)
# auc(roc_obj)
# 
# Mape(a,b)
# confusionMatrix_KNN = table(test$target,KNN_predict)
# confusionMatrix(confusionMatrix_KNN)
# #auc = .5027
# #accuracy = 88.57%
# #MAPE = 6.730641%
# #FNR = 4932/(4932+46322) = 9.6%
# #FPR = 1069/(198+1069) = 84.37%
# #RECALL = 198/(198+4932) = 3.85%
# #Precision = 198/(1069+198) = 15.62%


#####################Applying on Test data#####################

y = read.csv("test.csv", header = TRUE)
row.names(y)= y$ID_code
y=y[,-1]
backup = y
#View(y)

#####scaling test data######
for(i in cnames){
  print(i)
  y[,i] = (y[,i] - min(y[,i]))/
    (max(y[,i] - min(y[,i])))
}


target_test = predict(lr_model,y, type = "response")
target_test = ifelse(target_test > 0.5,1,0)
#summary(target_test)
#y = backup
ID_code = row.names(y)
y = cbind(ID_code,target_test,y)
write.csv(y, "predict_R.csv", row.names = FALSE)