#remove all objects stored
rm(list = ls())

#set working directory
setwd("D:/edwisor_project-2/R files")


#Load libraries
x = c("ggplot2","ggcorrplot", "corrgram","xlsx","VIM","forecast","caret")

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

#load data
data_original <- read.xlsx("Absenteeism_at_work_Project.xls",sheetIndex = 1,stringsAsFactors = FALSE)

#use copy of coriginal data
data <- data_original


#convert to required data types
#data$Day.of.the.week = factor(data$Day.of.the.week, levels = c(2,3,4,5,6), labels = c("monday","tuesday","wednesday","thursday","friday"))
#data$Seasons=factor(data$Seasons,levels = c(1,2,3,4), labels = c("summer","autumn","winter","spring"))                              
#data$Education=factor(data$Education,levels = c(1,2,3,4), labels = c("high school","graduate","postgraduate","master & doctor")) 
data$Social.drinker = as.factor(data$Social.drinker)
data$Social.smoker = as.factor(data$Social.smoker)
data$Reason.for.absence = as.factor(data$Reason.for.absence)
data$Disciplinary.failure = as.factor(data$Disciplinary.failure)
data$Day.of.the.week = as.factor(data$Day.of.the.week)
data$Seasons=as.factor(data$Seasons)
data$Education=as.factor(data$Education)
data$Month.of.absence=as.factor(data$Month.of.absence)
#------------------------------------------DATA PRE-PROCESSING----------------------------------------------
library(dplyr)
#Create the mode function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
databy_id <- as.data.frame(summarize(group_by(data, ID),
                       Transportation.expense = getmode(Transportation.expense),
                       Distance.from.Residence.to.Work = getmode(Distance.from.Residence.to.Work),
                       Service.time = getmode(Service.time),
                       Age = getmode(Age),
                       Education =getmode(Education),
                       Son = getmode(Son),
                       Social.drinker = getmode(Social.drinker),
                       Social.smoker = getmode(Social.smoker),
                       Pet = getmode(Pet),
                       Weight = getmode(Weight),
                       Height = getmode(Height),
                       Body.mass.index = getmode(Body.mass.index)))
                       

setwd("D:/edwisor_project-2/R files/univariate analysis plots")
#1.univariate analysis

png(file = "ID.plot.png")
ID.plot <- barplot(table(data$ID),ylim = c(0,150), col="grey",ylab = "Frequency",xlab = "Id")
text(ID.plot,table(data$ID),labels = table(data$ID),pos = 3)
dev.off()


png(file = "Reason.for.absence.plot.png")
Reason.for.absence.plot <- barplot(table(data$Reason.for.absence),ylim = c(0,180), col="grey",ylab = "Frequency",xlab = "reason for absence")
text(Reason.for.absence.plot,table(data$Reason.for.absence),labels = table(data$Reason.for.absence),pos = 3)
dev.off() 


png(file = "Month.of.absence.plot.png")
Month.of.absence.plot <- barplot(table(data$Month.of.absence),ylim = c(0,150), col="grey",ylab = "Frequency",xlab = "Months")
text(Month.of.absence.plot,table(data$Month.of.absence),labels = table(data$Month.of.absence),pos = 3)
dev.off() 


png(file = "barplot_seasons.png")
season_plot <- barplot(table(data$Seasons),ylim = c(0,250), col="grey",ylab = "Frequency",xlab = "Seasons")
text(season_plot,table(data$Seasons),labels = table(data$Seasons),pos = 3)
dev.off() 


png(file = "Disciplinary.failure.plot.png")
Disciplinary.failure.plot <- barplot(table(data$Disciplinary.failure),ylim = c(0, 800),col="grey",ylab = "Frequency",xlab = "Disciplinary failure")
text(Disciplinary.failure.plot,table(data$Disciplinary.failure),labels = table(data$Disciplinary.failure),pos = 3)
dev.off() 


png(file = "education.plot.png")
education.plot <- barplot(table(databy_id$Education),ylim = c(0,40), col="grey",ylab = "Frequency",xlab = "level of education")
text(education.plot,table(databy_id$Education),labels = table(databy_id$Education),pos = 3)
dev.off()


png(file = "Social.drinker.plot.png")
Social.drinker.plot <- barplot(table(databy_id$Social.drinker),ylim = c(0,30), col="grey",ylab = "Frequency",xlab = "social drinker or not")
text(Social.drinker.plot,table(databy_id$Social.drinker),labels = table(databy_id$Social.drinker),pos = 3)
dev.off()


png(file = "Social.smoker.plot.png")
Social.smoker.plot <- barplot(table(databy_id$Social.smoker),ylim = c(0,30), col="grey",ylab = "Frequency",xlab = "social smoker or not")
text(Social.smoker.plot,table(databy_id$Social.smoker),labels = table(databy_id$Social.smoker),pos = 3)
dev.off()


png(file = "Transportation.expense.png")
hist(databy_id$Transportation.expense,
     xlab = "Transportation.expense",
     col = "grey",
     border = "black" )
dev.off()


png(file = "Distance.from.Residence.to.Work.png")
hist(databy_id$Distance.from.Residence.to.Work,
     xlab = "Distance.from.Residence.to.Work",
     col = "grey",
     border = "black",
     breaks = seq(0,60,10))
dev.off()


png(file = "service_time.png")
hist(databy_id$Service.time,
     xlab = "service_time",
     col = "grey",
     border = "black")
dev.off()


png(file = "age.png")
hist(databy_id$Age,
     xlab = "age",
     col = "grey",
     border = "black")
dev.off()


png(file = "Work.load.Average.day.png")
hist(data$Work.load.Average.day.,
     xlab = "Work.load.Average.day.",
     col = "grey",
     border = "black",
     xlim = c(150000,400000))
#abline(v = mean(data$Work.load.Average.day., na.rm = TRUE),lwd = 2, lty = 1, col = "red"  )
#text(x = 320000, y = 150, 
#     labels = paste("Mean = ", round(mean(data$Work.load.Average.day., na.rm = TRUE),2), sep = ""), col="red" )

#abline(v = median(data$Work.load.Average.day.,na.rm = TRUE), lwd = 2, lty = 3, col = "blue")
#text(x = 200000, y = 150, 
#     labels = paste("Median = ", round(median(data$Work.load.Average.day., na.rm = TRUE),2), sep = ""), col="blue" )
dev.off()


png(file = "Hit.target.png")
hist(data$Hit.target,
     xlab = "Hit.target",
     col = "grey",
     border = "black",
     breaks = seq(80,100,2))
dev.off()


png(file = "son.png")
hist(databy_id$Son,
     xlab = "son",
     col = "grey",
     border = "black",
     breaks = seq(0,5,1))
dev.off()


png(file = "pet.png")
hist(databy_id$Pet,
     xlab = "pet",
     col = "grey",
     border = "black",
     breaks = seq(0,10,1))
dev.off()


png(file = "bmi.png")
hist(databy_id$Body.mass.index,
     xlab = "body mass index",
     col = "grey",
     border = "black",
     breaks = seq(17,39,2),
     xaxt = "n")
axis(1, at=seq(17,39, by=2))
dev.off()


png(file = "Absenteeism.time.in.hours.png")
hist(data$Absenteeism.time.in.hours,
     xlab = "Absenteeism.time.in.hours",
     col = "grey",
     border = "black",
     breaks = seq(0,120,2),
     xaxt = "n")
axis(1, at=seq(0,120, by=2))
dev.off()


#remove objects not to be used further
rm(ID.plot,Month.of.absence.plot,Disciplinary.failure.plot,education.plot,Reason.for.absence.plot,
   season_plot,Social.smoker.plot,Social.drinker.plot)


#----------------------------------------------------------
setwd("D:/edwisor_project-2/R files/bivariate analysis plots")
#2.Bivariate analysis

#boxplots of categorical vs continous variables
png(file = "absenteeism hrs vs Day.of.the.week.png",res = 100)
boxplot(log1p(data$Absenteeism.time.in.hours)~data$Day.of.the.week,xlab="Day.of.the.week",ylab="log1p(data$Absenteeism.time.in.hours)",col = "grey" )
dev.off()

png(file = "absenteeism hrs vs Reason.for.absence.png",res = 100,width = 2000,height = 1000)
boxplot(log1p(data$Absenteeism.time.in.hours)~data$Reason.for.absence,xlab="Reason.for.absence",ylab="log1p(data$Absenteeism.time.in.hours)",col = "grey" )
dev.off()

png(file = "absenteeism hrs vs Seasons.png",res = 100)
boxplot(log1p(data$Absenteeism.time.in.hours)~data$Seasons,xlab="Seasons",ylab="log1p(data$Absenteeism.time.in.hours)",col = "grey" )
dev.off()

png(file = "absenteeism hrs vs Disciplinary.failure.png",res = 100)
boxplot(log1p(data$Absenteeism.time.in.hours)~data$Disciplinary.failure,xlab="Disciplinary.failure",ylab="log1p(data$Absenteeism.time.in.hours)",col = "grey" )
dev.off()

png(file = "absenteeism hrs vs Education.png",res = 100)
boxplot(log1p(data$Absenteeism.time.in.hours)~data$Education,xlab="Education",ylab="log1p(data$Absenteeism.time.in.hours)",col = "grey" )
dev.off()

png(file = "absenteeism hrs vs Social.drinker.png",res = 100)
boxplot(log1p(data$Absenteeism.time.in.hours)~data$Social.drinker,xlab="Social.drinker",ylab="log1p(data$Absenteeism.time.in.hours)",col = "grey" )
dev.off()

png(file = "absenteeism hrs vs Social.smoker.png",res = 100)
boxplot(log1p(data$Absenteeism.time.in.hours)~data$Social.smoker,xlab="Social.smoker",ylab="log1p(data$Absenteeism.time.in.hours)",col = "grey" )
dev.off()

png(file = "absenteeism hrs vs month",res = 100)
boxplot(log1p(data$Absenteeism.time.in.hours)~data$Month.of.absence,xlab="months",ylab="log1p(data$Absenteeism.time.in.hours)",col = "grey" )
dev.off()

png(file = "absenteeism hrs vs ID.png",res = 100)
boxplot(log1p(data$Absenteeism.time.in.hours)~data$ID,xlab="ID",ylab="log1p(data$Absenteeism.time.in.hours)",col = "grey" )
dev.off()



#line plot of continous vs continous variables
png(file = "Absenteeism.time.in.hours vs Transportation.expense.png",res = 100)
ggplot(data, aes(x = Transportation.expense, y = log1p(Absenteeism.time.in.hours), color = Transportation.expense)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  xlab("Transportation.expense") + 
  ylab("Absenteeism.time.in.hours") +
  ggtitle("Absenteeism.time.in.hours vs Transportation.expense") +
  theme_classic()
dev.off()

png(file = "Absenteeism.time.in.hours vs Distance.from.Residence.to.Work.png",res = 100)
ggplot(data, aes(x = Distance.from.Residence.to.Work, y = log1p(Absenteeism.time.in.hours), color = Distance.from.Residence.to.Work)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  xlab("Distance.from.Residence.to.Work") + 
  ylab("Absenteeism.time.in.hours") +
  ggtitle("Absenteeism.time.in.hours vs Distance.from.Residence.to.Work") +
  theme_classic()
dev.off()

png(file = "Absenteeism.time.in.hours vs Service.time.png",res = 100)
ggplot(data, aes(x = Service.time, y = log1p(Absenteeism.time.in.hours), color = Service.time)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  xlab("Service.time") + 
  ylab("Absenteeism.time.in.hours") +
  ggtitle("Absenteeism.time.in.hours vs Service.time") +
  theme_classic()
dev.off()

png(file = "Absenteeism.time.in.hours vs Age.png",res = 100)
ggplot(data, aes(x = Age, y = log1p(Absenteeism.time.in.hours), color = Age)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  xlab("Age") + 
  ylab("Absenteeism.time.in.hours") +
  ggtitle("Absenteeism.time.in.hours vs Age") +
  theme_classic()
dev.off()

png(file = "Absenteeism.time.in.hours vs Work.load.Average.day.png",res = 100)
ggplot(data, aes(x = Work.load.Average.day., y = log1p(Absenteeism.time.in.hours), color = Work.load.Average.day.)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  xlab("Work.load.Average.day") + 
  ylab("Absenteeism.time.in.hours") +
  ggtitle("Absenteeism.time.in.hours vs Work.load.Average.day") +
  theme_classic()
dev.off()

png(file = "Absenteeism.time.in.hours vs Hit.target.png",res = 100)
ggplot(data, aes(x = Hit.target, y = log1p(Absenteeism.time.in.hours), color = Hit.target)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  xlab("Hit.target") + 
  ylab("Absenteeism.time.in.hours") +
  ggtitle("Absenteeism.time.in.hours vs Hit.target") +
  theme_classic()
dev.off()

png(file = "Absenteeism.time.in.hours vs Son.png",res = 100)
ggplot(data, aes(x = Son, y = log1p(Absenteeism.time.in.hours), color = Son)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  xlab("Son") + 
  ylab("Absenteeism.time.in.hours") +
  ggtitle("Absenteeism.time.in.hours vs Son") +
  theme_classic()
dev.off()

png(file = "Absenteeism.time.in.hours vs Pet.png",res = 100)
ggplot(data, aes(x = Pet, y = log1p(Absenteeism.time.in.hours), color = Pet)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  xlab("Pet") + 
  ylab("Absenteeism.time.in.hours") +
  ggtitle("Absenteeism.time.in.hours vs Pet") +
  theme_classic()
dev.off()

png(file = "Absenteeism.time.in.hours vs weight.png",res = 100)
ggplot(data, aes(x = Weight, y = log1p(Absenteeism.time.in.hours), color = Weight)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  xlab("weight") + 
  ylab("Absenteeism.time.in.hours") +
  ggtitle("Absenteeism.time.in.hours vs weight") +
  theme_classic()
dev.off()

png(file = "Absenteeism.time.in.hours vs height.png",res = 100)
ggplot(data, aes(x = Height, y = log1p(Absenteeism.time.in.hours), color = Height)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  xlab("height") + 
  ylab("Absenteeism.time.in.hours") +
  ggtitle("Absenteeism.time.in.hours vs height") +
  theme_classic()
dev.off()

png(file = "Absenteeism.time.in.hours vs Body.mass.index.png",res = 100)
ggplot(data, aes(x = Body.mass.index, y = log1p(Absenteeism.time.in.hours), color = Body.mass.index)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  xlab("Body.mass.index") + 
  ylab("Absenteeism.time.in.hours") +
  ggtitle("Absenteeism.time.in.hours vs Body.mass.index") +
  theme_classic()
dev.off()



#----------------------------------------------------------
setwd("D:/edwisor_project-2/R files/")

#3.missing values analysis
missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

#remove rows containing 0 month values
data <- data[!is.na(data$Month.of.absence),]
data <- data[!data$Month.of.absence == 0,]


data <- hotdeck(data,variable = c("Education","Social.drinker","Social.smoker","Pet","Body.mass.index","Height",
                                  "Transportation.expense","Son","Distance.from.Residence.to.Work",
                                  "Service.time","Age","Weight","Work.load.Average.day.","Hit.target","Reason.for.absence"),
                ord_var = "ID")

write.xlsx(data,"hotdeck_data.xlsx",row.names = FALSE)
data <- data[,1:21]

#imputing disciplinary variable values
#reason for logic
temp <- data.frame(table(data$Disciplinary.failure,data$Reason.for.absence))
png(file = "stacked_chart",res = 100)
ggplot(data=temp, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat="identity")
dev.off()
rm(temp)

data$Disciplinary.failure[is.na(data$Disciplinary.failure) & data$Reason.for.absence == 0] <- 1
data$Disciplinary.failure[is.na(data$Disciplinary.failure) & data$Reason.for.absence != 0] <- 0

#----------------------------------------------------------

#4. outliers analysis

png(file = "boxplot.Transportation.expense.png")
boxplot(databy_id$Transportation.expense,
        xlab = "Transportation.expense",
        ylab = "frequency", main = "Boxplot for Transportation.expense")
dev.off() 



png(file = "boxplot.Distance.from.Residence.to.Work.png")
boxplot(databy_id$Distance.from.Residence.to.Work,
        xlab = "Distance.from.Residence.to.Work",
        ylab = "frequency", main = "Boxplot for Distance.from.Residence.to.Work")
dev.off() 


png(file = "boxplot.Service.time.png")
boxplot(databy_id$Service.time,
        xlab = "Service.time",
        ylab = "frequency", main = "Boxplot for Service.time")
dev.off() 

png(file = "boxplot.Age.png")
boxplot(databy_id$Age,
        xlab = "Age",
        ylab = "frequency", main = "Boxplot for Age")
dev.off() 

png(file = "boxplot.Work.load.Average.day.png")
boxplot(data$Work.load.Average.day.,
        xlab = "Work.load.Average.day.",
        ylab = "frequency", main = "Boxplot for Work.load.Average.day")
dev.off() 

png(file = "boxplot.Hit.target.png")
boxplot(data$Hit.target,
        xlab = "Hit.target",
        ylab = "frequency", main = "Boxplot for Hit.target")
dev.off() 

png(file = "boxplot.Son.png")
boxplot(databy_id$Son,
        xlab = "Son",
        ylab = "frequency", main = "Boxplot for Son")
dev.off() 

png(file = "boxplot.Pet.png")
boxplot(databy_id$Pet,
        xlab = "Pet",
        ylab = "frequency", main = "Boxplot for Pet")
dev.off() 


png(file = "boxplot.Weight.png")
boxplot(databy_id$Weight,
        xlab = "Weight",
        ylab = "frequency", main = "Boxplot for Weight")
dev.off() 

png(file = "boxplot.Height.png")
boxplot(databy_id$Height,
        xlab = "Height",
        ylab = "frequency", main = "Boxplot for Height")
dev.off() 

png(file = "boxplot.Body.mass.index.png")
boxplot(databy_id$Body.mass.index,
        xlab = "Body.mass.index",
        ylab = "frequency", main = "Boxplot for Body.mass.index")
dev.off() 



#save numeric names
cnames <-  c("Pet","Service.time","Height")

for (i in cnames) {
  q25 <- quantile(databy_id[,i],probs = 0.25)
  q75 <- quantile(databy_id[,i],probs = 0.75)
  iqr = q75 - q25
  min = q25 - (iqr*1.5)
  max = q75 + (iqr*1.5)
  data <- data[!data[,i] < min,]
  data <- data[!data[,i] > max,]
  
}

#--------------------------------------------------------------------

#5.Feature Engineering

#correlation analysis
numeric_index <- sapply(data,is.numeric) #selecting only numeric
numeric_data <- data[,numeric_index]


png(file = "correlation_matrix.png",width = 1000,height = 1000)
ggcorrplot(cor(numeric_data),method = "square",type = "full", lab = TRUE)
dev.off()


#check normality
qqnorm(data$Transportation.expense)
qqnorm(data$Distance.from.Residence.to.Work)
qqnorm(data$Service.time)
qqnorm(data$Age)
qqnorm(data$Work.load.Average.day.)
qqnorm(data$Hit.target)
qqnorm(data$Weight)
qqnorm(data$Height)
qqnorm(data$Body.mass.index)
qqnorm(data$Absenteeism.time.in.hours)

#normalization of continous variables
cnames <- c("Transportation.expense","Distance.from.Residence.to.Work","Service.time","Age","Work.load.Average.day.",
            "Hit.target","Weight","Height","Body.mass.index")

for (i in cnames) {
  data[,i] <- (data[,i] - min(data[,i])) / 
              (max(data[,i]) - min(data[,i]))               
  
}

#-------------------------------------------------------------------
#removing data points having NA in dependent variable
na_data <- data[is.na(data$Absenteeism.time.in.hours),]
data <- data[!is.na(data$Absenteeism.time.in.hours),]

#evaluation metric definition
#evaluation - 1.Root Mean Squared Error Loss 
RMSE = function(yhat,y_tru ){
  sqrt(mean((yhat - y_tru)^2))
}

#-------------------------------------MODEL1------------------------------
#sampling
set.seed(220)
sample_index <- sample(nrow(data), nrow(data)*0.10,replace = FALSE)
test_data <- data[sample_index,]
train_data <- data[-sample_index,]


#Multiple linear regression model-1
lm_model1 <- lm(log1p(Absenteeism.time.in.hours)~., data = train_data)
lm_model1.step <- step(lm_model1)
summary(lm_model1.step)

prediction_model1 <- predict(lm_model1.step,test_data[1:20])
RMSE(round(expm1(prediction_model1)),test_data[,21])

#-------------------------------------MODEL2------------------------------
#sampling
set.seed(200)
#library(caret)
sample_index <- createDataPartition(data$Reason.for.absence, p = .9, list = FALSE)
train_data <- data[sample_index,]
test_data  <- data[-sample_index,]


lm_model2 <- lm(log1p(Absenteeism.time.in.hours) ~., data = train_data)
lm_model2.step <- step(lm_model2)
summary(lm_model2.step)

prediction_model2 <- predict(lm_model2.step,test_data[1:20])
RMSE(round(expm1(prediction_model2)),test_data[,21]) 

plot(test_data$Absenteeism.time.in.hours, main = "Linear Model", ylab = "Test Set of absenteeism in hrs.", pch = 20)
points(prediction_model2, col = "red", pch = 20)

#-------------------------------------------------------------------

#using model_ 2 to find missing values in target variable
na_data$Absenteeism.time.in.hours <- round(expm1(predict(lm_model2.step,na_data[1:20])))

#combine data
final_data <- rbind(data,na_data)
#--------------------------TIME SERIES ANALYSIS-----------------------------------------

final_data <- final_data[order(final_data$ID,final_data$Month.of.absence),]


databy_month <- summarize(group_by(data, Month.of.absence),
                       Absenteeism.time = sum(Absenteeism.time.in.hours))


ts_data = ts(data = databy_month$Absenteeism.time,frequency = 12)
fit <- tslm(ts_data ~ trend +season)
plot(forecast(fit, h=12))

#--------------------------end-----------------------------------------



