rm(list=ls(all=T))
setwd("D:/Gaikwad/Data/Projects R/Practice/bike rental analysis final/Bike renting project")
getwd()
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','fastDummies')

lapply(x, require, character.only = TRUE)
rm(x)

bike = read.csv("day.csv", header = T, na.strings = c(" ", "", "NA"))

bike_train=bike
######Exploratory Data Analysis##################

bike_train$season=as.factor(bike_train$season)
bike_train$mnth=as.factor(bike_train$mnth)
bike_train$yr=as.factor(bike_train$yr)
bike_train$holiday=as.factor(bike_train$holiday)
bike_train$weekday=as.factor(bike_train$weekday)
bike_train$workingday=as.factor(bike_train$workingday)
bike_train$weathersit=as.factor(bike_train$weathersit)
bike_train=subset(bike_train,select = -c(instant,casual,registered))


#change data format
d1=unique(bike_train$dteday)
df=data.frame(d1)
bike_train$dteday=as.Date(df$d1,format="%Y-%m-%d")
df$d1=as.Date(df$d1,format="%Y-%m-%d")
bike_train$dteday=format(as.Date(df$d1,format="%Y-%m-%d"), "%d")
bike_train$dteday=as.factor(bike_train$dteday)

str(bike_train)
###Missing Values Analysis###############################################

# Checking for missing value
missing_val = data.frame(apply(bike_train,2,function(x){sum(is.na(x))}))

##############Outlier Analysis##########

#BoxPlots - Distribution and Outlier Check
numeric_index = sapply(bike_train,is.numeric) #selecting only numeric

numeric_data = bike_train[,numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
  {
     assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(bike_train))+ 
              stat_boxplot(geom = "errorbar", width = 0.5) +
              geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                           outlier.size=1, notch=FALSE) +
              theme(legend.position="bottom")+
              labs(y=cnames[i],x="cnt")+
              ggtitle(paste("Box plot of count for",cnames[i])))
   }

gridExtra::grid.arrange(gn1,gn2,ncol=3)
gridExtra::grid.arrange(gn3,gn4,ncol=2)


#loop for removing outliers
Bike_new = bike_train      #saving the data
for (i in cnames) {
  print(i)
  val = bike_train[,i][bike_train[,i] %in% boxplot.stats(bike_train[,i])$out]
  print(length(val))
  bike_train = bike_train[which(! bike_train[,i] %in% val),] 
}

#####################Feature Selection#################


## Correlation Plot 
corrgram(Bike_new[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")



## Dimension Reduction####

bike_train = subset(bike_train,select = -c(atemp))


#######Model Development####################################################


###########################################
#divide data into test and train
rmExcept("bike_train")
set.seed(1234)
train.index = createDataPartition(bike_train$cnt , p = .80, list = FALSE)
train = bike_train[train.index,]
test = bike_train[-train.index,]

###########Decision tree regression  #################
fit = rpart(cnt ~ ., data = train, method = "anova")
predictions_DT = predict(fit, test[,-12])

#############Random Forest Model##########################
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 200)
predictions_RF = predict(RF_model, test[,-12])
plot(RF_model)
################Linear Regression#################

#converting multilevel categorical variable into binary dummy variable
cnames= c("dteday","season","mnth","weekday","weathersit")
data_lr=bike_train[,cnames]
cnt=data.frame(bike_train$cnt)
names(cnt)[1]="cnt"
data_lr <- fastDummies::dummy_cols(data_lr)
data_lr= subset(data_lr,select = -c(dteday,season,mnth,weekday,weathersit))
d3 = cbind(data_lr,bike_train)
d3= subset(d3,select = -c(dteday,season,mnth,weekday,weathersit,cnt))
data_lr=cbind(d3,cnt)


#dividind data into test and train
train_index = sample(1:nrow(data_lr), 0.8 * nrow(data_lr))
train_lr = data_lr[train_index,]
test_lr = data_lr[-train_index,]

#Linear regression model making
lm_model = lm(cnt ~., data = train_lr)
predictions_LR = predict(lm_model,test_lr[,-64])


#summary(lm_model)

#################evaluating MApe value###############


MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}
MAPE(test[,12], predictions_DT)

MAPE(test[,12], predictions_RF)

MAPE(test_lr[,64],  predictions_LR)



##########extacting predicted values output from Random forest model######################
results <- data.frame(test, pred_cnt = predictions_RF)

write.csv(results, file = 'RF output R .csv', row.names = FALSE, quote=FALSE)

############################################################################################









