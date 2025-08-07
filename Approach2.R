##########Approach2:ROS-----> Random Ower Sample#######
######training_testing_mydata####
set.seed(402)
mydata2 = upSample(x=mydata[,-1],y=mydata$pcf,yname="pcf")
sample1 = createDataPartition(mydata2$pcf, p = 0.8, list = FALSE)
train_data2 = mydata2[sample1,]
test_data2 = mydata2[-sample1,]

#####Fit_Logistic_Regression####
mylr2 = glm(pcf ~ ., data = train_data2
            , family = "binomial")
summary(mylr2)


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

#####Validation of mylr2####
predicted_lr2 = predict(mylr2 ,test_data2 , type = "response")
predicted_classes2 = ifelse(predicted_lr2 > 0.5 , 1 , 0)
observation2 = test_data2$pcf
cm_lr2 = confusionMatrix(as.factor(observation2), 
                         as.factor(predicted_classes2))
test_roc_lr2 = roc(observation2 ~ as.numeric( predicted_lr2),plot=T,
                   print.auc = TRUE)
accuracy_lr2 = cm_lr2$overall['Accuracy']
precision_lr2 = cm_lr2$byClass['Pos Pred Value']
recall_lr2 = cm_lr2$byClass['Sensitivity']
f1_score_lr2 = cm_lr2$byClass['F1']
roc_score_lr2 = roc(observation2 , predicted_classes2)
plot(roc_score_lr2 , main = "ROC -- Logistic Regression")
#####Fit_Support_vector_machine2####
mysvm2 = svm(pcf ~ . ,data = train_data2 , type ="C" 
            , kernel = "linear")
mysvm2

#####The importance of variables in mysvm2####
weight2 = t(mysvm2$coefs) %*% mysvm2$SV
sd(weight2)
weight2 %>% abs %>% as.data.frame() %>% sort(decreasing = TRUE)

#####Validation of mysvm2####
predicted_svm2 = predict(mysvm2 , na.omit(test_data2) , type = "response")
observation2 = na.omit(test_data2)$pcf

cm_svm2 = confusionMatrix(as.factor(predicted_svm2),
                          as.factor(observation2))
cm_svm2
roc_score_svm2 = roc(observation2 , as.numeric(predicted_svm2))
plot(roc_score_svm2 , main = "ROC -- SVM")
#######Fit_Desicion_TREE#####
mydt2 = rpart(pcf ~ . , method = "class"
              ,data = train_data2 , parms=list(split="information"))
printcp(mydt)
summary(mydt)
######Validation_DT#####
predicted_dt2 = predict(mydt2 , test_data2 , type = "class")
observation2 = test_data2$pcf
cm_dt2 = confusionMatrix(as.factor(observation2),
                         as.factor(predicted_dt2))
cm_dt2
test_roc_dt2 = roc(observation2 ~ as.numeric( predicted_dt2),plot=T,
                   print.auc = TRUE)
#####Fit_Random_Forest####
set.seed(402)
myrf2 = randomForest(pcf ~ . , data = na.omit(train_data2) , 
                     importance = TRUE)
plot(myrf2)
varImpPlot(myrf2)
#####Validation_RF####
predicted_rf2 = predict(myrf2 , test_data2 , type = "class")
observation2 = test_data2$pcf
cm_rf2 = confusionMatrix(as.factor(observation2),
                         as.factor(predicted_rf2))
roc_rf2 = roc(observation2 , as.numeric( predicted_rf2))
plot.roc(roc_rf2,main="Random Forest ---> RUS")
#####Acuracy_four_models#####
acuracy_lr2 = cm_lr2$overall['Accuracy']
acuracy_svm2 = cm_svm2$overall['Accuracy']
acuracy_dt2 = cm_dt2$overall['Accuracy']
acuracy_rf2 = cm_rf2$overall['Accuracy']

acuracy_models2 = data.frame(model = c("logistic",
                                       "Support_vector_machine",
                                       "decision_tree",
                                       "random_forest"),
                             acuracy = c(acuracy_lr2 , acuracy_svm2
                                         ,acuracy_dt2 , acuracy_rf2))
ggplot(data = acuracy_models2, aes(x = acuracy,y = model, label =
                                     round(acuracy,2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")
#####Presicion of Four models####
precision_lr2 = cm_lr2$byClass['Pos Pred Value']
precision_svm2 = cm_svm2$byClass['Pos Pred Value']
precision_dt2 = cm_dt2$byClass['Pos Pred Value']
precision_rf2 = cm_rf2$byClass['Pos Pred Value']

precision_models2 = data.frame(model = c("logistic",
                                         "support_vector_machine",
                                         "decision_tree",
                                         "random_forest"),
                               precision = c(precision_lr2 ,precision_svm2,
                                             precision_dt2 , precision_rf2))
ggplot(data =precision_models2, aes(x = precision , y = model, label =
                                      round(precision , 2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")

#####Recall of four models####
recall_lr2 = cm_lr2$byClass['Sensitivity']
recall_svm2 = cm_svm2$byClass['Sensitivity']
recall_dt2 = cm_dt2$byClass['Sensitivity']
recall_rf2 = cm_rf2$byClass['Sensitivity']

recall_models2 = data.frame(model = c("logistic",
                                      "support_vector_machine",
                                      "decision_tree",
                                      "random_forest"),
                            recall = c(recall_lr2 , recall_svm2,
                                       recall_dt2 , recall_rf2))

ggplot(data = recall_models2, aes(x = recall , y = model, label =
                                    round(recall , 2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")

#####F1 of four models####
f1_score_lr2 = cm_lr2$byClass['F1']
f1_score_svm2 = cm_svm2$byClass['F1']
f1_score_dt2 = cm_dt2$byClass['F1']
f1_score_rf2 = cm_rf2$byClass['F1']

f1_models2 = data.frame(model = c("logistic",
                                  "support_vector_machine",
                                  "decision_tree",
                                  "random_forest"),
                        f1_score= c(f1_score_lr2 , f1_score_svm2,
                                    f1_score_dt2 , f1_score_rf2))

ggplot(data = f1_models2, aes(x = f1_score , y = model, label =
                           round(f1_score , 2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")

#####Specificity_four_models#####
Specificity_lr2 = cm_lr2$byClass['Specificity']
Specificity_svm2 = cm_svm2$byClass['Specificity']
Specificity_dt2 = cm_dt2$byClass['Specificity']
Specificity_rf2 = cm_rf2$byClass['Specificity']

Specificity_models2 = data.frame(model = c("logistic",
                                           "SVM",
                                           "decision_tree",
                                           "random_forest"),
                                 Specificity = c(Specificity_lr2 , Specificity_svm2
                                             ,Specificity_dt2 , Specificity_rf2))
