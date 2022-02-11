library(naniar)
library(ggplot2)
library(dplyr)
library(caTools)
heart_data <- read.csv('E:\\RHeartDisease\\heart.csv', fileEncoding = "UTF-8-BOM")

head(heart_data)

colnames(heart_data)

str(heart_data)

summary(heart_data)

sapply(heart_data, sd)


######## Data Preparation ########

## getting a copy of the dataframe
heart<- heart_data 

## Changing to categorical variables
#sex column 
heart_data$sex[heart_data$sex == 0] = "female"
heart_data$sex[heart_data$sex == 1] = "male"

#cp 
heart_data$cp[heart_data$cp==0] = "typical angina"
heart_data$cp[heart_data$cp==1] = "atypical angina"
heart_data$cp[heart_data$cp==2] = "non-anginal pain"
heart_data$cp[heart_data$cp==3] = "asymptotic"

#fbs
heart_data$fbs[heart_data$fbs==0] = "false"
heart_data$fbs[heart_data$fbs==1] = "true"

#exang
heart_data$exang[heart_data$exang == 1] = "yes"
heart_data$exang[heart_data$exang == 0] = "no"

#restecg
heart_data$restecg[heart_data$restecg==0] = "Nothing to note"
heart_data$restecg[heart_data$restecg==1] = "ST-T Wave abnormality"
heart_data$restecg[heart_data$restecg==2] = " Definite left ventricular hypertrophy"

#slope
heart_data$slope[heart_data$slope == 0] = "upsloping"
heart_data$slope[heart_data$slope == 1] = "flat"
heart_data$slope[heart_data$slope == 2] = "downsloping"

#thal
heart_data$thal[heart_data$thal == 1] = "normal"
heart_data$thal[heart_data$thal == 2] = "fixed defect"
heart_data$thal[heart_data$thal == 3] = "reversible defect"

head(heart_data, n = 6)

##counting the total number of missing values
n_miss(heart_data) 
  # No missing values in the dataset

## converting some variables to factors
cols <- c('sex', 'cp', 'fbs', 'restecg', 'exang', 'slope', 'ca', 'thal', 'target')
for (col in cols) {
  heart_data[, col] <- as.factor(heart_data[, col])
}

## removing duplicate rows
heart_data <- distinct(heart_data)
dim(heart_data) 
  # One observation was dropped

## checking the levels of variables in cols
lapply(heart_data, levels) 
  # ca column has 5 levels and it should range from 0-3
  # thal also has value of 0 which is an outliner
  # We will drop them from the dataset
heart_data <- heart_data %>% filter(ca != 4 & thal != 0) %>% droplevels()
dim(heart_data) # 5 outliners deleted

levels(heart_data$ca)
levels(heart_data$thal)



######## Descriptive Statistics Techniques ########

round(prop.table(table(heart_data$target)),2)
  # 54% of people have heart disease and 46% dont


## proportion by sex 
round(prop.table(table(heart_data$sex)),2)
  # 32% Female and 68% Male


table(heart_data$cp, heart_data$sex)
  # There are more male than female who have cp


heart_rate <- heart_data %>% group_by(target) %>% count(cp)
heart_rate
  # Most of the ppl who have chest pain excluding "typical angina" suffer from heart disease

#Age distribution of population
ggplot(heart_data, aes(x=age)) + geom_histogram(bins =50) + 
  labs(x="Age", y="Density", title="Age distribution of population")
# The majority of people lies in the age of 40 to 70 ahs more chance of heart disease


# Slope of the Peak Exercise ST Segment(Slope)
ggplot(heart_data, aes(x= slope, fill=target)) + 
  geom_bar(position="dodge") + 
  labs(x="Slope", y="Count", title = "Analysis types of Slope")
  # For "Downsloping" the no heart disease is of 34 than the heart disease patient far above 100. 
  # For "Flat" the no heart disease is far above 90 than the heart disease patient is of 45.
  # For "Upsloping" the no heart disease is of below 20 than the heart disease patient is of below 15.


# Number of Major Vessels (ca)
mosaicplot(table(heart_data$target, heart_data$ca),
           col=c("#754869", "coral", "skyblue", "#423f42", "#ed18c6"), las=1, main="Heart Disease for Major Vessels")
  #The majority  of the people having heart disease have no major vessel.


# ST Depression Induced by Exercise Relative to Rest(oldpeak)
ggplot(heart_data, aes(x=oldpeak, fill=target)) + geom_boxplot() + labs(x="St Depression", y="Heart Disease State" , title="ST Depression Induced by Exercise vs Haert Disease")


## type of depression change with Gender
ggplot(heart_data, aes(x=oldpeak, fill=sex)) + geom_boxplot()



## Resting Blood Sugars separated by heart disease state
ggplot(heart_data, aes(x=trestbps, fill= target)) + geom_boxplot() +
  labs(y="Heart Disease State", x="Resting Blood Pressure", title="Resting Blood Pressure by Heart Condition")
  # The plot above shows an interquartile range of resting blood sugar is slightly higher for
  # the no heart disease plot. But the medians of both the box plot looks the same


## Heart Rate vs heart condition
ggplot(heart_data, aes(x=thalach, fill=target)) + geom_boxplot() + labs(x="Max Heart Rate", y="Heart Disease")


## Age vs Max rate for Heart Disease
ggplot(heart_data, aes(x=age, y=thalach, color=target, size=factor(thalach))) + geom_point(alpha= 0.3) +
  labs(x="Age", y="Maximum Heart Rate", title="Maximum Heart Rate for Heart Disease by Age",color = "Heart Disease State") + guides(size="none")
  # it shows, that when heart rate goes up most of are heart disease patient than no disease.


## Age vs. Bloop pressure for the Heart Disease
ggplot(heart_data, aes(x=age, y=trestbps, color=target, size=factor(trestbps))) + geom_point(alpha=0.3) +
  labs(x="Age", y="Resting Bloop Pressure", title="Age vs Bloop pressure for the heart Disease", color= "Heart Disease State") + guides(size="none")
  # When the blood pressure/sugar lies between 100-150 than the major number heart diseases patient found.
  # As you in the chart, the more bigger blue dot means no heart disease at 180 blood pressure/sugar.
  # Same time, the bigger red dot means heart disease at 150-180 blood presuure/sugar.


## Age vs. type of depression for the Heart Disease
ggplot(heart_data, aes(x=age, y=oldpeak, color=target, size=factor(oldpeak))) + geom_point(alpha = 0.4) + 
  labs(x="AGE", y="OLDPEAK", title = "Age vs. Type of Depression for Heart Disease", color = "Heart Disease State") + guides(size="none")
  # Major number heart disease patient lies in age group between 45 to 60.




###### Hypothesis testing with respect to one or several variables ######

shapiro.test(heart_data$trestbps) 
  # p-value = 1.913e-06 < .05 => trestbps is not normally distributed => Mann-Whitney-Wilcoxon test
  wilcox.test(heart_data$trestbps~heart_data$target)
  # Null hypothesis: Distribution functions of trestbps and target are equal
  # p-value = 0.02918 < .05 => nonidentical populations
  
shapiro.test(heart_data$age) 
  # p-value = 0.006045 < .05 => age is not normally distributed => Mann-Whitney-Wilcoxon test
  wilcox.test(heart_data$age~heart_data$target)
  # Null hypothesis: Distribution functions of age and target are equal
  # p-value = 4.314e-05 < .05 => nonidentical populations
  
shapiro.test(heart_data$chol) 
  # p-value = 8.986e-09 < .05 => chol is not normally distributed => Mann-Whitney-Wilcoxon test
  wilcox.test(heart_data$chol~heart_data$target)
  # Null hypothesis: Distribution functions of chol and target are equal
  # p-value = 0.05563 > .05 => identical populations
  
  wilcox.test(heart_data$chol~heart_data$fbs)
  # Null hypothesis: Distribution functions of chol and fasting blood sugar are equal
  # p-value = 0.7446 > .05 => identical populations
  
# Kruskal-Wallis Test
kruskal.test(trestbps ~ cp, data = heart_data)
  # Null hypothesis: Distribution functions of trestbps and type of chest pain are equal
  # p-value = 0.05642 > 0.5 => identical populations


kruskal.test(age ~ cp, data = heart_data)
  # Null hypothesis: Distribution functions of age and type of chest pain are equal
  # p-value = 0.007865 < .05 => Nonidentical populations

kruskal.test(chol ~ ca, data = heart_data)
# Null hypothesis: Distribution functions of cholestoral and no. of major vessel colored by flourosopy are equal
# p-value = 0.1077 > .05 => Identical populations

kruskal.test(trestbps ~ ca, data = heart_data)
# Null hypothesis: Distribution functions of resting blood pressure and no. of major vessel colored by flourosopy are equal
# p-value = 0.1815 > .05 => Identical populations




###### Creating a predictive model ######

split <- sample.split(heart_data, SplitRatio = 0.65)
heart_data_train <- subset(heart_data, split == "TRUE")
heart_data_test <- subset(heart_data, split == "FALSE")

model <- glm(target ~ age + sex + cp + trestbps + 
               chol + fbs + restecg + thalach + exang + oldpeak + slope + 
               ca + thal, data= heart_data_train, family = 'binomial')
summary(model)


# Prediction on training dataset 
res_pred_train <- predict(model, heart_data_train, type= "response")

confusion_matrix_train <- table(Actual_target = heart_data_train$target, Predicted_target = res_pred_train > 0.6)
confusion_matrix_train

round(((confusion_matrix_train[[1,1]] + confusion_matrix_train[[2,2]])/ sum(confusion_matrix_train)), 3)


# Prediction on testing dataset  
res_predict_test <- predict(model, heart_data_test, type = "response")

# Confusion Matrix for the test dataset
confusion_matrix_test <- table(Actual_target = heart_data_test$target, Predicted_target = res_predict_test > 0.6)
confusion_matrix_test

# Calculate model accuracy for test data set
round(((confusion_matrix_test[[1,1]] + confusion_matrix_test[[2,2]])/ sum(confusion_matrix_test)), 3)
