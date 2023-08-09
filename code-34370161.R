set.seed(34370161)
diabetes <- read.csv("diabetes.csv")

# question1 ---------------------------------------------------------------
# boxplot between diabetes and other variables
library("ggplot2")
library(gridExtra)
p1 <- ggplot(diabetes, aes(x = Age, fill = Diabetes)) +
  geom_boxplot()
p2 <-ggplot(diabetes, aes(x = BMI, fill = Diabetes)) +
  geom_boxplot()
p3 <-ggplot(diabetes, aes(x = Glucose, fill = Diabetes)) +
  geom_boxplot()
p4 <-ggplot(diabetes, aes(x = Pressure, fill = Diabetes)) +
  geom_boxplot()
p5 <-ggplot(diabetes, aes(x = Pregnant, fill = Diabetes)) +
  geom_boxplot()
grid.arrange(p1, p2,p3,p4,p5, ncol = 2)

# histogram to see distribution
p6 <- ggplot(diabetes, aes(x = Age, fill = Diabetes)) +
  geom_histogram()
p7 <- ggplot(diabetes, aes(x = BMI, fill = Diabetes)) +
  geom_histogram()
p8 <- ggplot(diabetes, aes(x = Glucose, fill = Diabetes)) +
  geom_histogram()
p9 <- ggplot(diabetes, aes(x = Pressure, fill = Diabetes)) +
  geom_histogram()
p10 <- ggplot(diabetes, aes(x = Pregnant, fill = Diabetes)) +
  geom_histogram()
grid.arrange(p6,p7,p8,p9,p10, ncol = 2)
# summarize all variables in each diabetes group
library(tidyr)
library(dplyr)
diabetes_summary <- diabetes %>%
  group_by(Diabetes) %>%
  summarise_all(list( mean = mean, sd = sd))
diabetes_summary

# Q2 the naive bayes classifier -------------------------------------------
# convert Diabetes to binary
diabetes$Diabetes <-  ifelse(diabetes$Diabetes == "neg", 0, 1)
# Split the dataset into training and test sets
train <- diabetes[1:500,]
test <- diabetes[501:724,]
# the naive most frequent category in the training set
most_freq <- names(which.max(table(train$Diabetes)))
# Apply the naive classifier to the test set
naive_pred <- rep(most_freq, nrow(test))
# compute the error rate
naive_error_rate <- mean(naive_pred != test$Diabetes)
naive_error_rate


# Q3 logistic regression --------------------------------------------------
set.seed(34370161)
# fit a logistic regression model on the training set
log_model <- glm(Diabetes ~ ., data=train,family = "binomial")
summary(log_model)
# apply the model to the test set
log_pred <- predict(log_model,test, type = "response")
# compute error rate
log_error_rate <- mean((log_pred>= 0.5) != test$Diabetes)
log_error_rate


# Q4 The Naive Bayes classifier -------------------------------------------

set.seed(34370161)
library(e1071)
# use the train set to fit a naive bayes model
nb_fit <- naiveBayes( Diabetes ~ . ,data=train)
nb_fit
# apply the classifier to the test set and return the predictions
nb_pred <- predict(nb_fit,test)
# compute error rate
nb_error_rate <- mean(nb_pred != test$Diabetes)
nb_error_rate


# Q5 The KNN classifier, with K = 1 to 10 ---------------------------------

set.seed(34370161)
library (class)
# set the train set and test set without variable Diabetes
train.X <- train[,-2]
test.X <- test[,-2]
# create k 
k <- 1:10
# Create an empty set to hold error rate
knn_error_rates <- NULL
# compute error rate 
for (i in k) {
  knn_pred <- knn(train.X, test.X, train$Diabetes, k[i])
  knn_error_rates[i] <- mean(knn_pred != test$Diabetes)
}
knn_error_rates
# the k with the smallest error rate
optimal_k <- which.min(knn_error_rates)
optimal_k
# the smallest error rate
min_knn_er <- min(knn_error_rates)
min_knn_er


# Q6 The classifier by logistic LASSO regression, with λ = 0.01, 0 --------
set.seed(34370161)
library(glmnet)
# create lambda
lambda <- c(0.01, 0.02, 0.03, 0.04)
# Create an empty set to hold error rate
lasso_error_rates <- vector()
# transform to matrix
train.X <- as.matrix(train.X)
test.X <- as.matrix(test.X)
# compute error rate 
for (i in lambda) {
  lasso_model <- glmnet(train.X, train$Diabetes, alpha = 1, lambda = i)
  lasso_pred <- predict(lasso_model, test.X, type="response")
  lasso_error_rates[[as.character(i)]]  <- sum(round(lasso_pred) != test$Diabetes) / length(test$Diabetes)
}
lasso_error_rates
# the lambda with the smallest error rate
optimal_lambda <- names(which.min(lasso_error_rates))
optimal_lambda
# the smallest error rate
min_lasso_er <- min(lasso_error_rates)
min_lasso_er


# Q7 The classifier by a pruned classification tree -----------------------
set.seed(34370161)
library(tree)
# fit a classification tree on the train set
tree_model <- tree(as.factor(Diabetes) ~., train)
# evaluate tree's performance on the test data
tree_pred <- predict(tree_model, test, type = "class")
# compute the error rate
tree_error_rate <- mean(tree_pred != test$Diabetes)
tree_error_rate
# perform cross-validation to determine the optimal level of tree complexity
cv_diabetes <- cv.tree(tree_model, FUN = prune.misclass)
cv_diabetes
# plot the size and dev under different terminal nodes
par(mfrow=c(1,2))
plot(cv_diabetes$size , cv_diabetes$dev, type = "b")
plot(cv_diabetes$k, cv_diabetes$dev, type = "b")
# prune the tree to attain the two-node tree
prune_model <- prune.misclass(tree_model, best=2)
# predict on the test data set under the pruned tree
pruned_pred <- predict(prune_model, test, type="class")
pruned_error_rate <- mean(pruned_pred != test$Diabetes)
pruned_error_rate

# Q8 The classifier by random forest --------------------------------------
library (randomForest)
set.seed(34370161)
# train on the randomforest classifier
rf_model <- randomForest(as.factor(Diabetes) ~ ., data=train, type="classification")
# predict on the test set
rf_pred <- predict(rf_model, newdata = test)
# calculate error rate
rf_error_rate <- mean(rf_pred != test$Diabetes)
rf_error_rate


# Q9 The classifier by boosted trees  ---------------------------------------------------
set.seed(34370161)
library(gbm)
# fit boosted regression trees
boost_model <- gbm(Diabetes ~ ., data = train,
                   distribution = "bernoulli", 
                   n.trees = 100)
summary(boost_model)
# predict on the test data
btree_pred <- predict(boost_model, newdata = test, type = "response", n.trees = 100)
# error rate
btree_error_rate <- mean((btree_pred > 0.5) != test$Diabetes)
btree_error_rate

# Q10 The support vector classifier ---------------------------------------
set.seed(34370161)
# train a support vector machine classifier
sv_model <- svm(Diabetes~., train, kernel="linear")
# predict on the test set
sv_pred <- predict(sv_model, test)
# compute error rate
sv_error_rate <- mean((sv_pred > 0.5) != test$Diabetes)
sv_error_rate

# Q11 The support vector classifier ---------------------------------------

set.seed(34370161)
# perform cross-validation to select the best gamma and cost
svm_tune <- tune(svm, Diabetes~., 
                 data = train,kernel = "radial",
                 ranges = list (
                   cost = c(0.1, 1, 10, 100, 1000),
                   gamma = c(0.5, 1, 2, 3, 4)))
summary(svm_tune)
best_params <- svm_tune$best.parameters
best_params # best performance is cost=1 and gamma=1
# predict on the test data using the best svm model
svm_pred <- predict(svm_tune$best.model, test)
# compute error rate
svm_error_rate <- mean((svm_pred > 0.5) != test$Diabetes)
svm_error_rate


# Q12 The classifier by principal components logistic regression --------------
set.seed(34370161)
# calculate the proportion of variation explained by each principal component
# perform PCA 
pca_out <-  prcomp(train[,-2], scale = TRUE)
summary(pca_out)# proportion of variance according to summary
summary(pca_out)$importance["Proportion of Variance",]
loadings(pca_out)
# or compute the variance using sdev
pr.var <- pca_out$sdev^2
pr.var
# then compute proportion of variance
pve <- pr.var / sum (pr.var)
pve
# Principal components regression (PCR) 
#install.packages("pls")
library (pls)
# perform PCR on the training data
pcr.fit <- pcr(Diabetes ~ ., data=train ,
                scale = TRUE , 
               validation = "CV",
               family= "binomial")
summary (pcr.fit)
# plot the cross-validation scores
par(mfrow=c(1,1))
validationplot (pcr.fit , val.type = "MSEP")# the lowest cross-validation error occurs when M = 3
# evaluate the testing set performance
pcr_pred <- predict(pcr.fit ,test, ncomp = 3, type = "response")
# error rate
pcr_error_rate <- mean((pcr_pred> 0.5) != test$Diabetes)
pcr_error_rate

# Q14 A mixed classifier --------------------------------------------------
# optimal mixing
# install.packages("caret")
library(caret)
set.seed(34370161)
# create 5-fold cross-validation object
ctrl <- trainControl(method = "cv", number = 5)
# define the models to be trained
model1 <- gbm(Diabetes ~ ., data = train,
              distribution = "bernoulli", n.trees = 100, cv.folds = 10)
model2 <- pcr(Diabetes ~ ., data=train, scale=TRUE, validation="CV", family="binomial", cv.folds=10)

# calculate predicted values for each fold of cross-validation
pred1 <- predict(model1, newdata = test, type = "response", n.trees = 100)
# plot the cross-validation scores for PCA
validationplot (model2 , val.type = "MSEP")# the lowest cross-validation error occurs when M = 3
# evaluate the testing set performance
pred2 <- predict(model2 ,test, ncomp = 3, type = "response")

# calculate E[ˆe1(x)ˆe2(x)] using cross-validation results
ef <- mean((test$Diabetes-pred1) * (test$Diabetes-pred2))

# calculate optimal value of "w" using formula
w <- (var(test$Diabetes-pred2)-ef) / (var(test$Diabetes-pred1) + var(test$Diabetes-pred2) - 2 * ef)
w
# calculate new classifier using weighted average of estimated labels
y_hat <- w * pred1 + (1 - w) * pred2
error_rate <- mean((y_hat > 0.5) != test$Diabetes)
error_rate

# Q15 Estimate the out-of-sample error rates ------------------------------
set.seed(34370161)
# Train the three classifiers on all the data
model1_os <- gbm(Diabetes ~ ., data=diabetes, distribution="bernoulli", n.trees=100)
model2_os <- pcr(Diabetes ~ ., data=diabetes, scale=TRUE, validation="CV", family="binomial")

# Compute the out-of-sample error rates for each classifier using cross-validation
pred1_error<- rep(NA, 5)
pred2_error<- rep(NA, 5)
mix_error <- rep(NA, 5)
for (i in 1:5) {
  folds <- createFolds(diabetes$Diabetes, k=5, list=TRUE)
  train_index <- unlist(folds[-i])
  test_index <- folds[[i]]
  train_data <- diabetes[train_index,]
  test_data <- diabetes[test_index,]
  pred1 <- predict(model1_os, newdata=test_data, type="response")
  pred2 <- predict(model2_os, newdata=test_data, type="response")
  pred_mix <- w * pred1 + (1 - w) * pred2
  pred1_error[i] <- mean((pred1 > 0.5) != test_data$Diabetes)
  pred2_error[i] <- mean((pred2 > 0.5) != test_data$Diabetes)
  mix_error[i] <- mean((pred_mix > 0.5) != test_data$Diabetes)
}
# Calculate the out of sample error rate
mean_pred1_error <- mean(pred1_error)
mean_pred1_error
mean_pred2_error <- mean(pred2_error)
mean_pred2_error
mean_mix_error <- mean(mix_error)
mean_mix_error


# Q16 Clustering kmeans ----------------------------------------------------------
# load package
library(cluster)
library(dplyr)
library(NbClust)
# divide Diabetes into positive and negative
diabetes_pos <- diabetes %>% filter(Diabetes == 1) 
diabetes_neg <- diabetes %>% filter(Diabetes == 0)

# scale the variable
scale_pos <- scale(diabetes_pos[,-2],center = FALSE)
scale_neg <- scale(diabetes_neg[,-2],center = FALSE)
#calculate k to determine the best k
set.seed(34370161)
sse_pos <- c()
sse_neg <- c()
for (k in 1:20) {
  kmeans_pos <- kmeans(scale_pos, k, nstart = 50)
  kmeans_neg <- kmeans(scale_neg, k, nstart = 50)
  sse_pos[k] <- kmeans_pos$tot.withinss
  sse_neg[k] <- kmeans_neg$tot.withinss
}
# plot SSE
sse_df <- data.frame(k = 1:20, SSE = c(sse_pos, sse_neg), Type = rep(c("Positive", "Negative"),each=20))
ggplot(sse_df, aes(x = k, y = SSE, color = Type)) + 
  geom_line() + 
  geom_point() + 
  ggtitle("Elbow Method to Find Optimal K") 
# determine the best number of clusters
nc_pos <- NbClust(scale_pos, distance = "euclidean", 
                  min.nc = 2, max.nc = 15, method = "kmeans")
par(mfrow=c(1,1))
barplot(table(nc_pos$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria (positive)")
nc_neg <- NbClust(scale_neg, distance = "euclidean", 
                  min.nc = 2, max.nc = 15, method = "kmeans")
barplot(table(nc_neg$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria (negative)")

# perform k-means clustering and print output of kmeans
kmeans_pos <- kmeans(scale_pos, 2, nstart = 25)
kmeans_pos
kmeans_neg <- kmeans(scale_neg, 2, nstart = 25)
kmeans_neg

#plot results of final k-means model
library(factoextra)
fviz_cluster(kmeans_pos, data = scale_pos,
             geom = "point", palette = "jco", ggtheme = theme_classic())
fviz_cluster(kmeans_neg, data = scale_neg,
             geom = "point", palette = "jco", ggtheme = theme_classic())

# add clustering results into the data set
pos_final <- data.frame(diabetes_pos, cluster=kmeans_pos$cluster)
neg_final <- data.frame(diabetes_neg, cluster=kmeans_neg$cluster)
pos_final %>%
  group_by(cluster) %>%
  summarize(across(Age:Pregnant, mean))
neg_final %>%
  group_by(cluster) %>%
  summarize(across(Age:Pregnant, mean))


# Q16 Clustering Hierarchical Clustering -------------------------------------------------
## hierarchical cluster analysis for positive patients
set.seed(34370161)
hc_pos <- hclust(dist(scale_pos), method = "complete")
# install.packages("NbClust")
# determine the best number of clusters
library(NbClust)
nc_pos <- NbClust(scale_pos, distance="euclidean", 
              min.nc=2, max.nc=15, method="average")
par(mfrow = c(1, 1))
barplot(table(nc_pos$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria (positive)")
# plot the dendrograms
par(mfrow = c(1, 1))
par(cex=0.4)
plot(hc_pos,
     xlab = "", sub = "" )
# Split into 2 clusters
clus_pos <- cutree(hc_pos, 2)
rect.hclust(hc_pos, k=2, border="red")
# Visualize the result in a scatter plot
library(factoextra)
fviz_cluster(list(data = scale_pos, cluster = clus_pos))
# computes the median value within each cluster
pos_mean <- aggregate(diabetes_pos[,-2],by=list(cluster=clus_pos),mean)
pos_mean

## hierarchical cluster analysis for negative patients
hc_neg <- hclust(dist(scale_neg), method = "complete")
# determine the best number of clusters
nc_neg <- NbClust(noneg, distance="euclidean", 
              min.nc=2, max.nc=15, method="average")
par(mfrow = c(1, 1))
barplot(table(nc_neg$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria (negative)")
# plot the dendrograms
par(mfrow = c(1, 1))
par(cex=0.4)
plot(hc_neg,
     xlab = "", sub = "")
# Split into 2 clusters
clus_neg <- cutree(hc_neg, 2)
rect.hclust(hc_neg, k=2, border="red")
# Visualize the result in a scatter plot
library(factoextra)
fviz_cluster(list(data = scale_neg, cluster = clus_neg))
# computes the median value within each cluster
neg_mean <- aggregate(diabetes_neg[,-2],by=list(cluster=clus_neg),mean)
neg_mean
