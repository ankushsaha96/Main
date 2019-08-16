rm(list = ls())
library(ggplot2)
library(corrgram)
library(caret)
library(DMwR)
library(rpart)
install.packages('randomForest')
library(randomForest)

#####setting the path#####
setwd("C:/Users/Ankush Saha/Downloads")

############################################loading the Data#############################################
x =read.csv("day.csv")

#####Data pre prosessing#####
row.names(x) = x$instant
x = x[,c(-1,-2)]
cnames = colnames(x[,c(1:11)])
x$season = as.factor(x$season)
x$yr = as.factor(x$yr)
x$mnth = as.factor(x$mnth)
x$holiday = as.factor(x$holiday)
x$weekday = as.factor(x$weekday)
x$workingday = as.factor(x$workingday)
x$weathersit = as.factor(x$weathersit)

#####backup of pre prosessed data#####
backup = x



#####Missing Value Analysis#####

mval = data.frame(apply(x,2,function(y){sum(is.na(y))}))
mval$collumns = row.names(mval)
names(mval)[1] = "missing percentage"
mval$`missing percentage` = (mval$`missing percentage`*100)/nrow(x)
mval = mval[,c(2,1)]
mval = mval[order(-mval$`missing percentage`),]

#####Outlier Analysis#####
numer = sapply(x[,-14], is.numeric)
numeric = x[,numer]
cnames = colnames(numeric)

for (i in 1:6)
{
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i])), data = subset(x))+
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="top")+
            labs(y=cnames[i],x="CNT")+
            ggtitle(paste("Box plot of responded for",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,ncol=6)

#####removing outliers#####
for (i in cnames) {
  print(i)
  val = x[,i][x[,i] %in% boxplot.stats(x[,i])$out]
  x = x[which(!x[,i] %in% val),]
}

#####Feature Selection#####

#####corellation test#####
corrgram(x, order = F,
          upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#####chi-square test#####
Factor_index = sapply(x, is.factor)
factor_data = x[,Factor_index]

for (i in 1:4) {
  for (j in 1:4) {
    print(c(names(factor_data[i]), names(factor_data[j])))
    print(chisq.test(table(factor_data[,i],factor_data[,j])))
  }
  
}

#####removing corellated dependent variables#####
x = subset(x, select = -c(season,weekday,workingday,weathersit,temp,registered))

#####Feature Scaling#####

x$casual = (x$casual - min(x$casual))/(max(x$casual) - min(x$casual))



#####Feature Sampling#####

set.seed(1234)
train_index = createDataPartition(x$cnt, p= 0.70, list = FALSE)
train = x[train_index,]
test = x[-train_index,]

############################################Developing Model#############################################

#####Linear regression Model#####

lm_model = lm(cnt ~. , data = train)
predicted = predict(lm_model, test[,-8])
summary(lm_model)
#####model testing#####
regr.eval(test[,8], predicted, stats = c('mae','mape'))


#####Decision tree#####
fit = rpart(cnt ~ ., data = train, method = 'anova')
predicted2 = predict(fit, test[,-8])
#####model testing#####
regr.eval(test[,8], predicted2, stats = c('mae','mape'))


########Random Forest#########
m1 = randomForest(  formula = cnt ~ ., data= train)
predicted3 = predict(m1, test[,-8])
#####model testing#####
regr.eval(test[,8], predicted3, stats = c('mae','mape'))


#####KNN#####
predicted4 = knnregTrain(train, test, train$cnt,  k = 5, use.all = TRUE)
#####model testing#####
regr.eval(test$cnt, predicted4, stats = c('mae','mape'))



############################################Test case#############################################

sampleInput = data.frame(yr = 1,mnth = 12,holiday = 0,atemp = 0.474 ,hum = .627 , windspeed = .19,casual = 713,cnt = 0)


#####pre prosessing test sample#####
sampleInput$yr = as.factor(sampleInput$yr)
sampleInput$mnth = as.factor(sampleInput$mnth)
sampleInput$holiday = as.factor(sampleInput$holiday)


#####scaling test sample#####
sampleInput$casual = (sampleInput$casual - min(backup$casual))/(max(backup$casual) - min(backup$casual))


#####predicting output#####
sampleInput$cnt = knnregTrain(train, sampleInput, train$cnt,  k = 5, use.all = TRUE)
sampleInput$cnt