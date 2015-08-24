main <- rm(list = ls(all =T))
setwd("/media/prasad/Edu/Machine_learning/Spam")
library(infotheo)
library(C50)
library(caret)
library(e1071)
library(rpart)

data <- read.table("Spam.txt", header = F)
str(data)
summary(data)

newColNames <- c("word_freq_make", "word_freq_address", "word_freq_all", "word_freq_3d", 
                 "word_freq_our", "word_freq_over", "word_freq_remove", "word_freq_internet", 
                 "word_freq_order", "word_freq_mail", "word_freq_receive", "word_freq_will", 
                 "word_freq_people", "word_freq_report", "word_freq_addresses", "word_freq_free", 
                 "word_freq_business", "word_freq_email", "word_freq_you", "word_freq_credit", 
                 "word_freq_your", "word_freq_font", "word_freq_000", "word_freq_money", 
                 "word_freq_hp", "word_freq_hpl", "word_freq_george", "word_freq_650", "word_freq_lab", 
                 "word_freq_labs", "word_freq_telnet", "word_freq_857", "word_freq_data", 
                 "word_freq_415", "word_freq_85", "word_freq_technology", "word_freq_1999", 
                 "word_freq_parts", "word_freq_pm", "word_freq_direct", "word_freq_cs", "word_freq_meeting", 
                 "word_freq_original", "word_freq_project", "word_freq_re", "word_freq_edu", 
                 "word_freq_table", "word_freq_conference", "char_freq_ch;", "char_freq_ch(", 
                 "char_freq_ch[", "char_freq_ch!", "char_freq_ch$", "char_freq_ch#", "capital_run_length_average", 
                 "capital_run_length_longest", "capital_run_length_total", "spam")
names(data) <- newColNames

#EDA 

str(data)
## converting spam colum as factor
data$spam <- as.factor(data$spam)
str(data)
table(data$spam)
sum(is.na(data)) #checking for NA values.

summary(data)

set.seed(1234)
index <- 1:nrow(data)
trainIndex <- sample(index, trunc(length(index) * 0.666666666666667))
data.train <- data[trainIndex, ]
data.test <- data[-trainIndex,]
print(paste0("Percentage: ", round((nrow(data.train)/nrow(data)) * 100, 2), " %"))

resTable <- table(data.train$spam)
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)  # increase y-axis margin.
# plot <- plot(data.train$spam, col = CUSTOM_COLORS_PLOT(6), main = "Email vs. Spam (Training Data Set)", 
#              ylim = c(0, max(resTable) + 100), ylab = "Examples Number")
# text(x = plot, y = resTable + 50, labels = resTable, cex = 0.75)
# 


model.rpart <- rpart(spam ~ ., method = "class", data = data.train)

printcp(model.rpart)
plot(model.rpart, uniform = TRUE, main = "Classification (RPART). Classification Tree for SPAM")
text(model.rpart, all = TRUE, cex = 0.75)

draw.tree(model.rpart, cex = 0.5, nodeinfo = TRUE, col = gray(0:8/8))
prediction.rpart <- predict(model.rpart, newdata = data.test, type = "class")
table(`Actual Class` = data.test$spam, `Predicted Class` = prediction.rpart)


error.rate.rpart <- sum(data.test$spam != prediction.rpart)/nrow(data.test)
print(paste0("Accuary (Precision): ", 1 - error.rate.rpart))


###
model.svm <- svm(spam ~ ., method = "class", data = data.train)
