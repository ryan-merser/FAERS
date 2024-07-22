

library(readr)
library(MASS)

merged_data_18 = read_csv("merged_data_18Q4.csv")
merged_data_20 = read_csv("merged_data_20Q4.csv")

#Row 2056 was removed since it was the only row in the dataset where rechal = "U"
merged_data_18 = merged_data_18[-2056,] 

set.seed(123)

ind <- sample(2, nrow(merged_data_18),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- merged_data_18[ind==1,]
testing <- merged_data_18[ind==2,]

ind2 <- sample(2, nrow(merged_data_20),
               replace = TRUE,
               prob = c(0.8, 0.2))
training2 <- merged_data_20[ind2==1,]
testing2 <- merged_data_20[ind2==2,]

#The following variables needed to be removed because of problems encountered: 
#age, age_cod,  wt, wt_cod,  reporter_country,  occr_country,  role_cod, route, outc_cod


linear <- lda(rechal~  sex + to_mfr + occp_cod, training)
linear

p1 <- predict(linear, testing)$class
tab1 <- table(Predicted = p1, Actual = testing$rechal)
sum(diag(tab1))/sum(tab1)

#The linear discriminant model for rechal in 2018 quarter training data has 80.94% prediction accuracy
#in corresponding testing data
linear2 <- lda(dechal~ sex + to_mfr + occp_cod, training)
linear2

p2 <- predict(linear2, testing)$class
tab2 <- table(Predicted = p2, Actual = testing$dechal)
sum(diag(tab2))/sum(tab2)

#The linear discriminant model for dechal in 2018 quarter training data has 47.36% prediction accuracy
#in corresponding testing data

linear3 <- lda(rechal~sex + to_mfr + occp_cod, training2)
linear3

p3 <- predict(linear3, testing2)$class
tab3 <- table(Predicted = p3, Actual = testing2$rechal)
sum(diag(tab3))/sum(tab3)

#The linear discriminant model for rechal in 2020 quarter training data has 85.02% prediction accuracy
#in corresponding testing data

linear4 <- lda(dechal~ sex + to_mfr + occp_cod, training2)
linear4

p4 <- predict(linear4, testing2)$class
tab4 <- table(Predicted = p4, Actual = testing2$dechal)
sum(diag(tab4))/sum(tab4)

#The linear discriminant model for dechal in 2020 quarter training data has 58.37% prediction accuracy
#in corresponding testing data

quadratic <- qda(rechal~ sex + to_mfr + occp_cod, training)
quadratic

predicted <- predict(quadratic, testing)
mean(predicted$class==testing$rechal)

#The quadratic discriminant model for rechal in 2018 quarter training data has 55.28% prediction accuracy
#in corresponding testing data

quadratic2 <- qda(dechal~sex + to_mfr + occp_cod, training)
quadratic2

predicted2 <- predict(quadratic2, testing)
mean(predicted2$class==testing$dechal)

#The quadratic discriminant model for dechal in 2018 quarter training data has 47.36% prediction accuracy
#in corresponding testing data

quadratic3 <- qda(rechal~sex + to_mfr + occp_cod, training2)
quadratic3

predicted3 <- predict(quadratic3, testing2)
mean(predicted3$class==testing2$rechal)

#The quadratic discriminant model for rechal in 2018 quarter training data has 85.02% prediction accuracy
#in corresponding testing data

quadratic4 <- qda(dechal~ sex + to_mfr + occp_cod, training2)
quadratic4

predicted4 <- predict(quadratic4, testing2)
mean(predicted4$class==testing2$dechal)

#The quadratic discriminant model for dechal in 2020 quarter training data has 58.37% prediction accuracy
#in corresponding testing data