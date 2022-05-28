# Clear Environment
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Clear plots & console
if(!is.null(dev.list())) dev.off()
cat("\014") 

# Install required packages
library(ggplot2)
library(lattice)
library(caret)
library(dplyr)

# Read data
filename = "../data/insurance.csv"
data <- read.csv(file=filename, sep=",", header = TRUE)


data$sex <- NULL
data$region <- NULL
colnames(data) = c("age","bmi","children","smoker","charges")
data$smoker[data$smoker == "yes"] <- 0
data$smoker[data$smoker == "no"] <- 1
data$smoker <- as.numeric(data$smoker)
data$bmi <- as.integer(data$bmi)
data$charges <- as.integer(data$charges)


for (col_name in colnames(data)) {
  if (col_name != "charges") {
    scatter.smooth(x=data$charges, y=data[[col_name]], main=col_name, col="lightgreen")
  }
}
par(mfrow = c(1,3), mar=c(5.1,4.1,4.1,2.1))


print("Correlation between each attribute and Score: A low correlation (-0.2 < x < 0.2)", quote=FALSE)

for (col_name in colnames(data)) {
  print(paste0(col_name, ": ", cor(data$charges, data[[col_name]])), quote=FALSE)
}

training <- 0.8
training_partition <- createDataPartition(y = data$charges, p = training, list = FALSE)

best <- NULL
ratio_error <- 10000

training_data <- data[training_partition,]
test_data     <- data[-training_partition,]

for(i in 1:10){
  lmodel <- lm(formula = charges ~., data = training_data)
  
  prediction <- predict(lmodel,test_data)
  
  mavg_error <- mean(abs(prediction - test_data$charges))
  if(mavg_error < ratio_error){
    best <- lmodel
    ratio_error <- mavg_error
  }
  lmodel <- best
  mavg_error <- ratio_error
  print(paste0("Mean average error: ", mavg_error))
  print(lmodel)
}


df_nosmoker <- filter(data,data$smoker==1)
df_nosmoker$smoker <- 0
print(df_nosmoker)
df_nosmoker$prediction = predict(lmodel,df_nosmoker)
inc_if_smoke <- mean(df_nosmoker$prediction) - mean(df_nosmoker$charges) 
print(inc_if_smoke)

df_smoker <- filter(data,data$smoker==0)
df_smoker$smoker <- 1
print(df_smoker)
df_smoker$prediction = predict(lmodel,df_smoker)
dec_if_nosmoke <- mean(df_smoker$charges)-mean(df_smoker$prediction)
print(dec_if_nosmoke)

data$age <- data$age + 5 
data$costs <- predict(lmodel, newdata = data)
data$costs_difference <- data$costs - data$charges
data <- data[order(-data$costs_difference),]
most_costs <- head(data,3)
print(most_costs)

summary(lmodel)

