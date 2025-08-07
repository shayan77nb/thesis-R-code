#######Approach1:------> my data#######

######training_testing_mydata####
set.seed(402)
sample1 = createDataPartition(mydata$pcf, p = 0.8, list = FALSE)
train_data1 = mydata[sample1,]
test_data1 = mydata[-sample1,]

#####Fit_Logistic_Regression####
mylr1 = glm(pcf ~ ., data = train_data1
            , family = "binomial")
summary(mylr1)


mylr1_power2 = glm(as.numeric(pcf) ~ .+age^2+bmi^2+
                     depression^2+anxiety^2+met^2+hospital_date^2
                   , data = train_data11 
                   , family = "binomial")
stargazer(mylr1_power2 , mylr1 , title="Results", type = "text"
          , align=TRUE)
#####Interreptation of mylr1####
mylr1$model %>%
  select(cfs) %>%
  cbind(probability = mylr1$fitted.values,.)-> dataviz_dataset
dataviz_dataset %>%
  ggplot(aes(x = cfs , y = probability)) +
  geom_point()

#####Validation of mylr1####
predicted_lr1 = predict(mylr1 , test_data1 , type = "response")
predicted_classes1 = ifelse(predicted_lr1 > 0.2 , 1 , 0)
observation1 = test_data1$pcf
cm_lr1 = confusionMatrix(as.factor(observation1), 
                         as.factor(predicted_classes1))
accuracy_lr1 = cm_lr1$overall['Accuracy']
precision_lr1 = cm_lr1$byClass['Pos Pred Value']
recall_lr1 = cm_lr1$byClass['Sensitivity']
f1_score_lr1 = cm_lr1$byClass['F1']
roc_score_lr1 = roc(observation1 , predicted_classes1)
plot(roc_score_lr1 , main = "ROC -- Logistic Regression")
test_roc_lr1 = roc(observation1 ~ predicted_classes1, 
                   plot = TRUE, print.auc = TRUE)
#####Fit_Support_vector_machine1####
mysvm1 = svm(pcf ~ . , 
             data = train_data1 , type = "C" , kernel = "linear")
mysvm1

#####The importance of variables in mysvm1####
weight1 = t(mysvm1$coefs) %*% mysvm1$SV
sd(weight1)
weight1 %>% abs %>% as.data.frame() %>% sort(decreasing = TRUE)

#####Validation of mysvm1####
predicted_svm1 = predict(mysvm1 , na.omit(test_data1) , type = "response")
observation1 = na.omit(test_data1)$pcf
table(as.factor(predicted_svm1))
table(as.factor(observation1))

cm_svm1 = confusionMatrix(as.factor(predicted_svm1),
                          as.factor(observation1))
cm_svm1
test_roc_svm1 = roc(observation1 ~ as.numeric( predicted_svm1),plot=T,
                    print.auc = TRUE)

#######Fit_Desicion_TREE#####
mydt1 = rpart(pcf ~ . , method = "class"
              ,data = train_data1 , parms=list(split="information"))
printcp(mydt)
summary(mydt)
######Validation_DT#####
predicted_dt1 = predict(mydt1 , test_data1 , type = "class")
observation1 = test_data1$pcf
cm_dt1 = confusionMatrix(as.factor(observation1),
                        as.factor(predicted_dt1))
test_roc_dt1 = roc(observation1 ~ as.numeric( predicted_dt1),plot=T,
                    print.auc = TRUE)
#####Fit_Random_Forest####
set.seed(402)
myrf1 = randomForest(pcf ~ . , data = na.omit(train_data1) , 
                     importance = TRUE)
plot(myrf1)
varImpPlot(myrf1)
#####Validation_RF####
predicted_rf1 = predict(myrf1 , test_data1 , type = "class")
observation1 = test_data1$pcf
cm_rf1 = confusionMatrix(as.factor(observation1),
                        as.factor(predicted_rf1))
test_roc_rf1 = roc(observation1 ~ as.numeric( predicted_rf1),plot=T,
                   print.auc = TRUE)
#####Acuracy_four_models#####
acuracy_lr1 = cm_lr1$overall['Accuracy']
acuracy_svm1 = cm_svm1$overall['Accuracy']
acuracy_dt1 = cm_dt1$overall['Accuracy']
acuracy_rf1 = cm_rf1$overall['Accuracy']

acuracy_models1 = data.frame(model = c("logistic",
                                      "SVM",
                                       "decision_tree",
                                       "random_forest"),
                             acuracy = c(acuracy_lr1 , acuracy_svm1
                                         ,acuracy_dt1 , acuracy_rf1))
ggplot(data = acuracy_models1, aes(x = acuracy,y = model, label =
                           round(acuracy,2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")
#####Presicion of Four models####
precision_lr1 = cm_lr1$byClass['Pos Pred Value']
precision_svm1 = cm_svm1$byClass['Pos Pred Value']
precision_dt1 = cm_dt1$byClass['Pos Pred Value']
precision_rf1 = cm_rf1$byClass['Pos Pred Value']

precision_models1 = data.frame(model = c("logistic",
                                        "support_vector_machine",
                                        "decision_tree",
                                        "random_forest"),
                              precision = c(precision_lr1 ,precision_svm1,
                                            precision_dt1 , precision_rf1))
ggplot(data =precision_models1, aes(x = precision , y = model, label =
                           round(precision , 2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")

#####Recall of four models####
recall_lr1 = cm_lr1$byClass['Sensitivity']
recall_svm1 = cm_svm1$byClass['Sensitivity']
recall_dt1 = cm_dt1$byClass['Sensitivity']
recall_rf1 = cm_rf1$byClass['Sensitivity']

recall_models1 = data.frame(model = c("logistic",
                                     "support_vector_machine",
                                     "decision_tree",
                                     "random_forest"),
                           recall = c(recall_lr1 , recall_svm1,
                                      recall_dt1 , recall_rf1))

ggplot(data = recall_models1, aes(x = recall , y = model, label =
                           round(recall , 2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")

#####F1 of four models####
f1_score_lr1 = cm_lr1$byClass['F1']
f1_score_svm1 = cm_svm1$byClass['F1']
f1_score_dt1 = cm_dt1$byClass['F1']
f1_score_rf1 = cm_rf1$byClass['F1']

f1_models1 = data.frame(model = c("logistic",
                                 "support_vector_machine",
                                 "decision_tree",
                                 "random_forest"),
                       f1_score= c(f1_score_lr1 , f1_score_svm1,
                                   f1_score_dt1 , f1_score_rf1))

ggplot(data = data4, aes(x = f1_score , y = model, label =
                           round(f1_score , 2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")

#####Specificity_four_models#####
Specificity_lr1 = cm_lr1$byClass['Specificity']
Specificity_svm1 = cm_svm1$byClass['Specificity']
Specificity_dt1 = cm_dt1$byClass['Specificity']
Specificity_rf1 = cm_rf1$byClass['Specificity']

Specificity_models1 = data.frame(model = c("logistic",
                                       "SVM",
                                       "decision_tree",
                                       "random_forest"),
                             acuracy = c(Specificity_lr1 , Specificity_svm1
                                         ,Specificity_dt1 , Specificity_rf1))
ggplot(data = acuracy_models1, aes(x = acuracy,y = model, label =
                                     round(acuracy,2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")
#####Presicion of Four models####
precision_lr1 = cm_lr1$byClass['Pos Pred Value']
precision_svm1 = cm_svm1$byClass['Pos Pred Value']
precision_dt1 = cm_dt1$byClass['Pos Pred Value']
precision_rf1 = cm_rf1$byClass['Pos Pred Value']

precision_models1 = data.frame(model = c("logistic",
                                         "support_vector_machine",
                                         "decision_tree",
                                         "random_forest"),
                               precision = c(precision_lr1 ,"NA",
                                             precision_dt1 , precision_rf1))
ggplot(data =precision_models1, aes(x = precision , y = model, label =
                                      round(precision , 2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")