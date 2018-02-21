#Set the working directory
getwd()
setwd("C:\\Users\\ni258624\\Documents\\R\\Linear Regression")

# Read the train and test data
train_data = read.csv("pisa2009train.csv")
test_data = read.csv("pisa2009test.csv")

# Find the no. of rows in train and test
nrow(train_data) # 3663 rows
nrow(test_data) # 1570 rows

#Merge the train and test to perform exploratory data analysis.
df = merge(train_data, test_data, all=TRUE)
View(df)

str(df)

# Convert all categorical Variable to factor's
df$male = as.factor(df$male)
df$preschool = as.factor(df$preschool)
df$expectBachelors = as.factor(df$expectBachelors)
df$motherHS = as.factor(df$motherHS)
df$motherBachelors = as.factor(df$motherBachelors)
df$motherWork = as.factor(df$motherWork)
df$fatherHS = as.factor(df$fatherHS)
df$fatherBachelors = as.factor(df$fatherBachelors)
df$fatherWork = as.factor(df$fatherWork)
df$selfBornUS = as.factor(df$selfBornUS)
df$motherBornUS = as.factor(df$motherBornUS)
df$fatherBornUS = as.factor(df$fatherBornUS)
df$englishAtHome = as.factor(df$englishAtHome)
df$computerForSchoolwork = as.factor(df$computerForSchoolwork)
df$read30MinsADay = as.factor(df$read30MinsADay)
df$schoolHasLibrary = as.factor(df$schoolHasLibrary)
df$publicSchool = as.factor(df$publicSchool)
df$urban = as.factor(df$urban)


summary(df)

# Handle the Null's in the data
df$raceeth[is.na(df$raceeth)] = 'White'
df$preschool[is.na(df$preschool)] = '1'
df$expectBachelors[is.na(df$expectBachelors)] = '1'
df$motherHS[is.na(df$motherHS)] = '1'
df$motherBachelors[is.na(df$motherBachelors)] = '0'
df$motherWork[is.na(df$motherWork)] = '1'
df$fatherHS[is.na(df$fatherHS)] = '1'
df$fatherBachelors[is.na(df$fatherBachelors)] = '0'
df$selfBornUS[is.na(df$selfBornUS)] = '1'
df$motherBornUS[is.na(df$motherBornUS)] = '1'
df$fatherBornUS[is.na(df$fatherBornUS)] = '1'
df$englishAtHome[is.na(df$englishAtHome)] = '1'
df$computerForSchoolwork[is.na(df$computerForSchoolwork)] = '1'
df$fatherWork[is.na(df$fatherWork)] = '1'
df$read30MinsADay[is.na(df$read30MinsADay)] = '0'
df$minutesPerWeekEnglish[is.na(df$minutesPerWeekEnglish)] = median(df$minutesPerWeekEnglish, na.rm ='T')
df$studentsInEnglish[is.na(df$studentsInEnglish)] = median(df$studentsInEnglish, na.rm ='T')
df$schoolHasLibrary[is.na(df$schoolHasLibrary)] = '1'
df$schoolSize[is.na(df$schoolSize)] = median(df$schoolSize, na.rm ='T')


# Find outliers

#1) 
boxplot(df$minutesPerWeekEnglish)
q1 = quantile(df$minutesPerWeekEnglish, .25)
q3 = quantile(df$minutesPerWeekEnglish, .75)
q = IQR(df$minutesPerWeekEnglish)
upper = q3 +1.5*q 
lower = q1 -1.5*q  

df$minutesPerWeekEnglish= sapply(df$minutesPerWeekEnglish,function(x){ifelse(x > 387.5,387.5, x)} )
df$minutesPerWeekEnglish= sapply(df$minutesPerWeekEnglish,function(x){ifelse(x < 127.5,127.5, x)} )


#2)
boxplot(df$studentsInEnglish)
q1 = quantile(df$studentsInEnglish, .25)
q3 = quantile(df$studentsInEnglish, .75)
q = IQR(df$studentsInEnglish)
upper = q3 +1.5*q 
lower = q1 -1.5*q  

df$studentsInEnglish= sapply(df$studentsInEnglish,function(x){ifelse(x > 45,45, x)} )
df$studentsInEnglish= sapply(df$studentsInEnglish,function(x){ifelse(x < 5,5, x)} )

#3)
boxplot(df$schoolSize)
q1 = quantile(df$schoolSize, .25)
q3 = quantile(df$schoolSize, .75)
q = IQR(df$schoolSize)
upper = q3 +1.5*q 
lower = q1 -1.5*q  

df$schoolSize= sapply(df$schoolSize,function(x){ifelse(x > 3599.5,3599.5, x)} )

summary(df)

#Split Train and Test data

full_data = df
tr_dt = (full_data[1:3663,])
ts_dt = (full_data[3664:nrow(full_data),])
#Train data
df_train = tr_dt
nrow(df_train)

#Test data
df_test = ts_dt
nrow(df_test)

# newdf= head(full_data[(full_data$raceeth == 'American Indian/Alaska Native'),])
# View(newdf)


# Build Linear Reg Model

train_reg = lm(readingScore~. -motherWork -fatherHS -studentsInEnglish -fatherWork -selfBornUS 
               -fatherBornUS -schoolHasLibrary -raceeth  -preschool -motherBornUS -motherHS,data = df_train)

summary(train_reg)


train_pred = predict(train_reg,df_train)
test_pred = predict(train_reg,df_test)

table(test_pred, df_test$readingScore)


#4.1
predTest=predict(train_reg,newdata=df_test)
summary(predTest)

#4.2
SSE=(sum(df_test$readingScore - predTest)^2)

RMSE=(SSE/nrow(df_test))^0.5
RMSE

#4.3
baseline_pred=mean(df_train$readingScore)
baseline_pred
SST=(sum(df_test$readingScore - baseline_pred)^2)

#4.4
R2=1-(SSE/SST)
R2

#MAPE
mape_train = sum(abs(df_train$readingScore-train_pred)/df_train$readingScore)/length(df_train$readingScore)*100

mape_test = sum(abs(df_test$readingScore-test_pred)/df_test$readingScore)/length(df_test$readingScore)*100



