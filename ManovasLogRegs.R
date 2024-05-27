library(mosaic)
library(effsize)
library(Stat2Data)

allAnova = read.csv(file=file.choose())


#Comparing means of groups ----

#Use the four-step process to see if there is evidence of a difference between these two groups


#choose: (decide to use equal variance (or not) t-test)
boxplot(P.wins~as.factor(Different.clusters), data=allAnova)
favstats(P.wins~as.factor(Different.clusters), data=allAnova)

### 47 of the 80 teams ended up in different clusters, 33 stayed in the same (each season clustered separately)
### Different cluster (changed play style in postseason) mean of 5.7 wins, sd of 5.2, and median of 3 
### Same cluster (maintained play style in postseason) mean of 4.7 wins, sd of 4.2, median of 4 
### Also, we see equal (ish) variance 

#fit: run the t-test
plot(P.wins~as.factor(Different.clusters),data=allAnova,ylab = "Playoff Wins", xlab="Postseason play style compared to regular season")
t.test(P.wins~as.factor(Different.clusters), data=allAnova, var.equal=T)

### Our p-value is very large (0.3588), and our 95% confidence interval for the difference in means contains 0

### Considering H_0 of m1 = m2 and H_a: m1 =! m2:
### With a t of 0.92 on 78 df, with a corresponding p value of 0.36 > alpha = 0.05,
### We do not have statistically significant evidence that there is a difference in means in the two groups. 
### We fail to reject the null hypothesis

### No stat. sig. evidence that the means are different. 

#we are a bit skeptical about conditions (especially independence of observation) but will not focus on them here, 
#especially because there is so clearly no evidence of differences in the two means. 




### MANOVA


#read in data
forMANLR=read.csv(file=file.choose())
View(forMANLR)

###EDA

#look at potentially relevant box plots
boxplot(Bench.FGA~Type, data = forMANLR, ylab="Bench field goals attempted")
boxplot(X3PA~Type, data = forMANLR,ylab="Three point goals attempted")
boxplot(FTA~Type, data = forMANLR)

###MANOVA

#to make MANOVA code easier, we define our dependent variables and our independent variable
depvs <- cbind(forMANLR$FGA,forMANLR$X3PA,forMANLR$X2PA,forMANLR$FTA,forMANLR$ORB,forMANLR$DRB,forMANLR$AST,
               forMANLR$STL,forMANLR$BLK,forMANLR$TOV,forMANLR$PF,forMANLR$Age,forMANLR$Pace,forMANLR$FTr,forMANLR$Dist.,
               forMANLR$X.FG..2P,forMANLR$X.FG..0.3,forMANLR$X.FG..3.10,forMANLR$X.FG..10.16,forMANLR$X.FG..16.3P,
               forMANLR$X.FG..Ast.d..2P,forMANLR$X.FG.AST.d..3P,forMANLR$X.FGA.Dunk,forMANLR$X.3PA.Corner,
               forMANLR$Bench.MIN,forMANLR$Bench.FGA)
indepv <- forMANLR$Type

#MANOVA test
our_manova <- manova(depvs ~ indepv, data = forMANLR)

#Summary of output
summary(our_manova)


###Logistic regression

# Separately create simple logistic regression model for each variable, and evaluate by k-means clustering

# FGA
cv_model_FGA <- train(as.factor(Type) ~ FGA, 
                      data = forMANLR,
                      method = "glm",
                      family = "binomial",
                      trControl = trainControl(method = "cv", number = number))

pred_class_FGA <- predict(cv_model_FGA, forMANLR)

confusionMatrix(
  data = relevel(pred_class_FGA, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# X3PA
cv_model_X3PA <- train(as.factor(Type) ~ X3PA, 
                       data = forMANLR,
                       method = "glm",
                       family = "binomial",
                       trControl = trainControl(method = "cv", number = number))

pred_class_X3PA <- predict(cv_model_X3PA, forMANLR)

confusionMatrix(
  data = relevel(pred_class_X3PA, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# X2PA
cv_model_X2PA <- train(as.factor(Type) ~ X2PA, 
                       data = forMANLR,
                       method = "glm",
                       family = "binomial",
                       trControl = trainControl(method = "cv", number = number))

pred_class_X2PA <- predict(cv_model_X2PA, forMANLR)

confusionMatrix(
  data = relevel(pred_class_X2PA, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# FTA
cv_model_FTA <- train(as.factor(Type) ~ FTA, 
                      data = forMANLR,
                      method = "glm",
                      family = "binomial",
                      trControl = trainControl(method = "cv", number = number))

pred_class_FTA <- predict(cv_model_FTA, forMANLR)

confusionMatrix(
  data = relevel(pred_class_FTA, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# ORB
cv_model_ORB <- train(as.factor(Type) ~ ORB, 
                      data = forMANLR,
                      method = "glm",
                      family = "binomial",
                      trControl = trainControl(method = "cv", number = number))

pred_class_ORB <- predict(cv_model_ORB, forMANLR)

confusionMatrix(
  data = relevel(pred_class_ORB, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# DRB
cv_model_DRB <- train(as.factor(Type) ~ DRB, 
                      data = forMANLR,
                      method = "glm",
                      family = "binomial",
                      trControl = trainControl(method = "cv", number = number))

pred_class_DRB <- predict(cv_model_DRB, forMANLR)

confusionMatrix(
  data = relevel(pred_class_DRB, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# AST
cv_model_AST <- train(as.factor(Type) ~ AST, 
                      data = forMANLR,
                      method = "glm",
                      family = "binomial",
                      trControl = trainControl(method = "cv", number = number))

pred_class_AST <- predict(cv_model_AST, forMANLR)

confusionMatrix(
  data = relevel(pred_class_AST, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# STL
cv_model_STL <- train(as.factor(Type) ~ STL, 
                      data = forMANLR,
                      method = "glm",
                      family = "binomial",
                      trControl = trainControl(method = "cv", number = number))

pred_class_STL <- predict(cv_model_STL, forMANLR)

confusionMatrix(
  data = relevel(pred_class_STL, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# BLK
cv_model_BLK <- train(as.factor(Type) ~ BLK, 
                      data = forMANLR,
                      method = "glm",
                      family = "binomial",
                      trControl = trainControl(method = "cv", number = number))

pred_class_BLK <- predict(cv_model_BLK, forMANLR)

confusionMatrix(
  data = relevel(pred_class_BLK, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# TOV
cv_model_TOV <- train(as.factor(Type) ~ TOV, 
                      data = forMANLR,
                      method = "glm",
                      family = "binomial",
                      trControl = trainControl(method = "cv", number = number))

pred_class_TOV <- predict(cv_model_TOV, forMANLR)

confusionMatrix(
  data = relevel(pred_class_TOV, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# PF
cv_model_PF <- train(as.factor(Type) ~ PF, 
                     data = forMANLR,
                     method = "glm",
                     family = "binomial",
                     trControl = trainControl(method = "cv", number = number))

pred_class_PF <- predict(cv_model_PF, forMANLR)

confusionMatrix(
  data = relevel(pred_class_PF, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# Age
cv_model_Age <- train(as.factor(Type) ~ Age, 
                      data = forMANLR,
                      method = "glm",
                      family = "binomial",
                      trControl = trainControl(method = "cv", number = number))

pred_class_Age <- predict(cv_model_Age, forMANLR)

confusionMatrix(
  data = relevel(pred_class_Age, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# Pace
cv_model_Pace <- train(as.factor(Type) ~ Pace, 
                       data = forMANLR,
                       method = "glm",
                       family = "binomial",
                       trControl = trainControl(method = "cv", number = number))

pred_class_Pace <- predict(cv_model_Pace, forMANLR)

confusionMatrix(
  data = relevel(pred_class_Pace, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# FTr
cv_model_FTr <- train(as.factor(Type) ~ FTr, 
                      data = forMANLR,
                      method = "glm",
                      family = "binomial",
                      trControl = trainControl(method = "cv", number = number))

pred_class_FTr <- predict(cv_model_FTr, forMANLR)

confusionMatrix(
  data = relevel(pred_class_FTr, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# Dist.
cv_model_Dist <- train(as.factor(Type) ~ Dist., 
                       data = forMANLR,
                       method = "glm",
                       family = "binomial",
                       trControl = trainControl(method = "cv", number = number))

pred_class_Dist <- predict(cv_model_Dist, forMANLR)

confusionMatrix(
  data = relevel(pred_class_Dist, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# X.FG..2P
cv_model_XFG_2P <- train(as.factor(Type) ~ X.FG..2P, 
                         data = forMANLR,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = number))

pred_class_XFG_2P <- predict(cv_model_XFG_2P, forMANLR)

confusionMatrix(
  data = relevel(pred_class_XFG_2P, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# X.FG..0.3
cv_model_XFG_0.3 <- train(as.factor(Type) ~ X.FG..0.3, 
                          data = forMANLR,
                          method = "glm",
                          family = "binomial",
                          trControl = trainControl(method = "cv", number = number))

pred_class_XFG_0.3 <- predict(cv_model_XFG_0.3, forMANLR)

confusionMatrix(
  data = relevel(pred_class_XFG_0.3, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# X.FG..3.10
cv_model_XFG_3.10 <- train(as.factor(Type) ~ X.FG..3.10, 
                           data = forMANLR,
                           method = "glm",
                           family = "binomial",
                           trControl = trainControl(method = "cv", number = number))

pred_class_XFG_3.10 <- predict(cv_model_XFG_3.10, forMANLR)

confusionMatrix(
  data = relevel(pred_class_XFG_3.10, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# X.FG..10.16
cv_model_XFG_10.16 <- train(as.factor(Type) ~ X.FG..10.16, 
                            data = forMANLR,
                            method = "glm",
                            family = "binomial",
                            trControl = trainControl(method = "cv", number = number))

pred_class_XFG_10.16 <- predict(cv_model_XFG_10.16, forMANLR)

confusionMatrix(
  data = relevel(pred_class_XFG_10.16, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# X.FG..16.3P
cv_model_XFG_16.3P <- train(as.factor(Type) ~ X.FG..16.3P, 
                            data = forMANLR,
                            method = "glm",
                            family = "binomial",
                            trControl = trainControl(method = "cv", number = number))

pred_class_XFG_16.3P <- predict(cv_model_XFG_16.3P, forMANLR)

confusionMatrix(
  data = relevel(pred_class_XFG_16.3P, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# X.FG..Ast.d..2P
cv_model_XFG_Ast.d.2P <- train(as.factor(Type) ~ X.FG..Ast.d..2P, 
                               data = forMANLR,
                               method = "glm",
                               family = "binomial",
                               trControl = trainControl(method = "cv", number = number))

pred_class_XFG_Ast.d.2P <- predict(cv_model_XFG_Ast.d.2P, forMANLR)

confusionMatrix(
  data = relevel(pred_class_XFG_Ast.d.2P, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# X.FG.AST.d..3P
cv_model_XFG_AST.d.3P <- train(as.factor(Type) ~ X.FG.AST.d..3P, 
                               data = forMANLR,
                               method = "glm",
                               family = "binomial",
                               trControl = trainControl(method = "cv", number = number))

pred_class_XFG_AST.d.3P <- predict(cv_model_XFG_AST.d.3P, forMANLR)

confusionMatrix(
  data = relevel(pred_class_XFG_AST.d.3P, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

Bench.MIN,  Bench.FGA


# X.FGA.Dunk
cv_modelX.FGA.Dunk <- train(as.factor(Type) ~ X.FGA.Dunk, 
                            data = forMANLR,
                            method = "glm",
                            family = "binomial",
                            trControl = trainControl(method = "cv", number = number))

pred_classX.FGA.Dunk <- predict(cv_modelX.FGA.Dunk, forMANLR)

confusionMatrix(
  data = relevel(pred_classX.FGA.Dunk, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# X.FGA.Dunk
cv_modelX.3PA.Corner <- train(as.factor(Type) ~ X.3PA.Corner, 
                              data = forMANLR,
                              method = "glm",
                              family = "binomial",
                              trControl = trainControl(method = "cv", number = number))

pred_classX.3PA.Corner <- predict(cv_modelX.3PA.Corner, forMANLR)

confusionMatrix(
  data = relevel(pred_classX.3PA.Corner, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# Bench.MIN
cv_modelBench.MIN <- train(as.factor(Type) ~ Bench.MIN, 
                           data = forMANLR,
                           method = "glm",
                           family = "binomial",
                           trControl = trainControl(method = "cv", number = number))

pred_classBench.MIN <- predict(cv_modelBench.MIN, forMANLR)

confusionMatrix(
  data = relevel(pred_classBench.MIN, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# Bench.FGA
cv_modelBench.FGA <- train(as.factor(Type) ~ Bench.FGA, 
                           data = forMANLR,
                           method = "glm",
                           family = "binomial",
                           trControl = trainControl(method = "cv", number = number))

pred_classBench.FGA <- predict(cv_modelBench.FGA, forMANLR)

confusionMatrix(
  data = relevel(pred_classBench.FGA, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# X.FG..2P
cv_modelX.FG..2P <- train(as.factor(Type) ~ X.FG..2P, 
                          data = forMANLR,
                          method = "glm",
                          family = "binomial",
                          trControl = trainControl(method = "cv", number = number))

pred_classX.FG..2P <- predict(cv_modelX.FG..2P, forMANLR)

confusionMatrix(
  data = relevel(pred_classX.FG..2P, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)

# TRB
cv_model_TRB <- train(as.factor(Type) ~ TRB, 
                      data = forMANLR,
                      method = "glm",
                      family = "binomial",
                      trControl = trainControl(method = "cv", number = number))

pred_class_TRB <- predict(cv_model_TRB, forMANLR)

confusionMatrix(
  data = relevel(pred_class_TRB, ref = "Post"),
  reference = relevel(as.factor(forMANLR$Type), ref = "Post")
)





