htru = read.csv("HTRU_2.csv")

index = sample(nrow(htru), 0.3*nrow(htru))
train = htru[-index,]
test = htru [index,]

library(randomForest)

rf = randomForest(X0 ~ ., data = train)

p = predict(rf, newdata = test, type = "response")>0.5

errors = sum(test$X0!=p)
errors/nrow(test)

lm = glm(X0 ~ ., data = train, family = binomial("logit"))
p = predict(lm, newdata = test, type = "response")>0.5

errors = sum(test$X0!=p)
errors/nrow(test)

library(e1071)

svm = svm(X0 ~ ., data = train)
p = predict(svm, newdata = test, type = "response")>0.5

errors = sum(test$X0!=p)
errors/nrow(test)

library(glmnet)



mt = data.matrix(train[,1:8])

lm = cv.glmnet(mt, train$X0)

lasso = glmnet(mt, train$X0, lambda = lm$lambda.min)

p = predict(lasso, newx = data.matrix(test[,1:8]), type="response")[,1]>0.5

errors = sum(test$X0!=p)
errors/nrow(test)
