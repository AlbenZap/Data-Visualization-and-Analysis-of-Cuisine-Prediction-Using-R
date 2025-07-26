recipes <- read.csv("C:/Users/Alben/Desktop/Projects/Data Visualization and Analysis of Cuisine Prediction/recipes.csv")
view(recipes)
library(ggplot2) 
library(plyr) 
library(dplyr) 
library(tidyverse) 
library(DataExplorer)
dim(recipes)
glimpse(recipes)
summary(recipes)
summary(recipes$butter)
sum(is.na(recipes))
recipes <- recipes %>% rename(cuisine = country)
recipes$cuisine = tolower(recipes$cuisine)
count <- table(recipes$cuisine)
cuisines_indices <- count>50
cuisines_indices_FALSE <- cuisines_indices[!cuisines_indices %in% TRUE]
new_recipes <- subset(recipes, recipes$cuisine != "austrian" & recipes$cuisine != "bangladesh" & recipes$cuisine != "belgian" & recipes$cuisine != "dutch" & recipes$cuisine != "east-african" & recipes$cuisine != "indonesian" & recipes$cuisine != "iranian" & recipes$cuisine != "lebanese" & recipes$cuisine != "malaysian" & recipes$cuisine != "pakistani" & recipes$cuisine != "philippine" & recipes$cuisine != "south-african" & recipes$cuisine != "swiss" & recipes$cuisine != "turkish" & recipes$cuisine != "west-african")
new_recipes[new_recipes=="Yes"]<-1
new_recipes[new_recipes=="No"]<-0
glimpse(full)
summary(full$butter)
plot_missing(full)
mean <- full %>% group_by(cuisine) %>% summarise_all("mean")
mean <- mean[2:384] * 100
boxplot(cream ~ butter, data = edit_recipes, xlab = "Butter", ylab = "Cream", main = "Cream ~ Butter")
plot_correlation(full[1:10], type = 'continuous','cuisine')
plot_bar(edit_recipes$cuisine)
plot_bar(edit_recipes)
length(unique(recipes$country))
recipes %>% group_by(country) %>% 
	summarise(ALMOND_max  = max(almond, na.rm = T),
			 ALMOND_min  = min(almond, na.rm = T),
			 count   = n())  
sort(table(recipes$country), decreasing = TRUE)
full %>% count(cuisine, butter)
ggplot(data = edit_recipes, mapping = aes(x = rice)) + geom_freqpoly(mapping = aes(colour = cuisine), binwidth = 500)
ggplot(data = edit_recipes) +geom_bar(mapping = aes(x = cuisine))
barplot(mean_full$butter,names.arg=mean_full$cuisine,xlab="Cuisine",ylab="Butter",col="blue",main="Chart",border="red")
boxplot(mean_full$butter ~ mean_full$cream, data = mean_full, xlab = "Cream",ylab = "Butter", main = "Ingredients Data")
hist(mean_full$butter,xlab = "Butter",col = "green",border = "red", xlim = c(0,80), ylim = c(0,5),  breaks = 10)
plot(mean_full$butter,type = "o", col = "red", xlab = "Cuisine", ylab = "Butter", main = "Line Chart")
lines(mean_full$cream, type = "o", col = "blue")
pie(mean_full$butter, mean_full$cuisine, main = "Pie Chart", col = rainbow(length(mean_full$butter)))
pairs(~mean_full$bell_pepper+mean_full$black_pepper+mean_full$cardamom+mean_full$cayenne,data = mean_full, main = "Scatterplot")

//Decision Tree
library(rpart)
library(rpart.plot)
create_train_test <- function(data, size = 0.8, train = TRUE) {
+     n_row = nrow(data)
+     total_row = size * n_row
+     train_sample <- 1: total_row
+     if (train == TRUE) {
+         return (data[train_sample, ])
+     } else {
+         return (data[-train_sample, ])
+     }
+ }
train <- create_train_test(asian, 0.8, train = TRUE)
test <- create_train_test(asian, 0.8, train = FALSE)
prop.table(table(test$cuisine))
fit <- rpart(cuisine~., data = train, method = 'class')
rpart.plot(fit)
predict_unseen <-predict(fit, test, type = 'class')
table_mat <- table(test$cuisine, predict_unseen)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

//SVM
training[["cuisine_label "]] = factor(training[["cuisine_label "]])
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_Linear <- train(cuisine_label ~., data = training, method = "svmLinear",
        trControl=trctrl,
                                preProcess = c("center", "scale"),
        tuneLength = 10)
svm_Linear
test_pred <- predict(svm_Linear, newdata = testing)
test_pred
confusionMatrix(table(test_pred, testing$cuisine_label))
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(cuisine_label ~., data = training, method = "svmLinear",
       trControl=trctrl,
                                           preProcess = c("center", "scale"),
                                           tuneGrid = grid,
                                           tuneLength = 10)
       svm_Linear_Grid
       plot(svm_Linear_Grid)
test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
confusionMatrix(table(test_pred_grid, testing$cuisine_label))

//KNN
set.seed(123)
dat.d <- sample(1:nrow(asian_l),size=nrow(asian_l)*0.7,replace = FALSE) #random selection of 70% data.
train.cuisine <- asian_l[dat.d,] # 70% training data
test.cuisine <- asian_l[-dat.d,] # remaining 30% test data
train.cuisine_labels <- asian_l[dat.d,384]
test.cuisine_labels <- asian_l[-dat.d,384]
knn.26 <- knn(train= train.cuisine, test= test.cuisine, cl=train.cuisine_labels, k=26)
knn.27 <- knn(train= train.cuisine, test= test.cuisine, cl=train.cuisine_labels, k=27)
ACC.26 <- 100 * sum(test.cuisine_labels == knn.26)/NROW(test.cuisine_labels)
ACC.27 <- 100 * sum(test.cuisine_labels == knn.27)/NROW(test.cuisine_labels)
table(knn.26 ,test.cuisine_labels)
table(knn.27 ,test.cuisine_labels)
i=1
k.optm=1
for (i in 1:28){
 knn.mod <- knn(train=train.cuisine, test=test.cuisine, cl=train.cuisine_labels, k=i)
 k.optm[i] <- 100 * sum(test.cuisine_labels == knn.mod)/NROW(test.cuisine_labels)
 k=i
  	 cat(k,'=',k.optm[i],'
')
 }
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")
