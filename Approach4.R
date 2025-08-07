##########Approach4:-----> SMOTE#######
######training_testing_mydata####
set.seed(402)
mydata4 = SMOTE(pcf~. , data = mydata)
sample4 = createDataPartition(mydata4$pcf, p = 0.8, list = FALSE)
train_data4 = mydata4[sample4,]
test_data4 = mydata4[-sample4,]

#####Fit_Logistic_Regression####
mylr4 = glm(pcf ~ ., data = train_data4
            , family = "binomial")
summary(mylr4)


mylr1_power4 = glm(as.numeric(pcf) ~ .+age^2+bmi^2+
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
predicted_lr4 = predict(mylr4 ,test_data4 , type = "response")
predicted_classes4 = ifelse(predicted_lr4 > 0.5 , 1 , 0)
observation4 = test_data4$pcf
cm_lr4 = confusionMatrix(as.factor(observation4), 
                         as.factor(predicted_classes4))
accuracy_lr4 = cm_lr4$overall['Accuracy']
precision_lr4 = cm_lr4$byClass['Pos Pred Value']
recall_lr4 = cm_lr4$byClass['Sensitivity']
f1_score_lr4 = cm_lr4$byClass['F1']
roc_score_lr4 = roc(observation4 , predicted_classes4)
plot(roc_score_lr4 , main = "ROC -- Logistic Regression")
#####Fit_Support_vector_machine4####
mysvm4 = svm(pcf ~ . ,data = train_data4 , type ="C" 
             , kernel = "linear")
mysvm4

#####The importance of variables in mysvm4####
weight4 = t(mysvm4$coefs) %*% mysvm4$SV
sd(weight4)
weight4 %>% abs %>% as.data.frame() %>% sort(decreasing = TRUE)

#####Validation of mysvm4####
predicted_svm4 = predict(mysvm4 , na.omit(test_data4) , type = "response")
observation4 = na.omit(test_data4)$pcf

cm_svm4 = confusionMatrix(as.factor(predicted_svm4),
                          as.factor(observation4))
cm_svm4
roc_score_svm4 = roc(observation4 , as.numeric(predicted_svm4))
plot(roc_score_svm4 , main = "ROC -- Logistic Regression")
#######Fit_Desicion_TREE#####
mydt4 = rpart(pcf ~ . , method = "class"
              ,data = train_data4 , parms=list(split="information"))
printcp(mydt)
summary(mydt)
######Validation_DT#####
predicted_dt4 = predict(mydt4 , test_data4 , type = "class")
observation4 = test_data4$pcf
cm_dt4 = confusionMatrix(as.factor(observation4),
                         as.factor(predicted_dt4))
cm_dt4
test_roc_dt4 = roc(observation4 ~ as.numeric( predicted_dt4),plot=T,
                   print.auc = TRUE)
#####Fit_Random_Forest####
set.seed(402)
myrf4 = randomForest(pcf ~ . , data = na.omit(train_data4) , 
                     importance = TRUE)
plot(myrf4)
varImpPlot(myrf4)
#####Validation_RF####
predicted_rf4 = predict(myrf4 , test_data4 , type = "class")
observation4 = test_data4$pcf
cm_rf4 = confusionMatrix(as.factor(observation4),
                         as.factor(predicted_rf4))
test_roc_rf4 = roc(observation4 ~ as.numeric( predicted_rf4),plot=T,
                   print.auc = TRUE)
#####Acuracy_four_models#####
acuracy_lr4 = cm_lr4$overall['Accuracy']
acuracy_svm4 = cm_svm4$overall['Accuracy']
acuracy_dt4 = cm_dt4$overall['Accuracy']
acuracy_rf4 = cm_rf4$overall['Accuracy']

acuracy_models4 = data.frame(model = c("logistic",
                                       "Support_vector_machine",
                                       "decision_tree",
                                       "random_forest"),
                             acuracy = c(acuracy_lr4 , acuracy_svm4
                                         ,acuracy_dt4 , acuracy_rf4))
ggplot(data = acuracy_models2, aes(x = acuracy,y = model, label =
                                     round(acuracy,2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")
#####Presicion of Four models####
precision_lr4 = cm_lr4$byClass['Pos Pred Value']
precision_svm4 = cm_svm4$byClass['Pos Pred Value']
precision_dt4 = cm_dt4$byClass['Pos Pred Value']
precision_rf4 = cm_rf4$byClass['Pos Pred Value']

precision_models4 = data.frame(model = c("logistic",
                                         "support_vector_machine",
                                         "decision_tree",
                                         "random_forest"),
                               precision = c(precision_lr4 ,precision_svm4,
                                             precision_dt4 , precision_rf4))
ggplot(data =precision_models4, aes(x = precision , y = model, label =
                                      round(precision , 2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")

#####Recall of four models####
recall_lr4 = cm_lr4$byClass['Sensitivity']
recall_svm4 = cm_svm4$byClass['Sensitivity']
recall_dt4 = cm_dt4$byClass['Sensitivity']
recall_rf4 = cm_rf4$byClass['Sensitivity']

recall_models4 = data.frame(model = c("logistic",
                                      "support_vector_machine",
                                      "decision_tree",
                                      "random_forest"),
                            recall = c(recall_lr4 , recall_svm4,
                                       recall_dt4 , recall_rf4))

ggplot(data = recall_models4, aes(x = recall , y = model, label =
                                    round(recall , 2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")

#####F1 of four models####
f1_score_lr4 = cm_lr4$byClass['F1']
f1_score_svm4 = cm_svm4$byClass['F1']
f1_score_dt4 = cm_dt4$byClass['F1']
f1_score_rf4 = cm_rf4$byClass['F1']

f1_models4 = data.frame(model = c("logistic",
                                  "support_vector_machine",
                                  "decision_tree",
                                  "random_forest"),
                        f1_score= c(f1_score_lr4 , f1_score_svm4,
                                    f1_score_dt4 , f1_score_rf4))

ggplot(data = f1_models4, aes(x = f1_score , y = model, label =
                                round(f1_score , 2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")

#####Specificity_four_models#####
Specificity_lr4 = cm_lr4$byClass['Specificity']
Specificity_svm4 = cm_svm4$byClass['Specificity']
Specificity_dt4 = cm_dt4$byClass['Specificity']
Specificity_rf4 = cm_rf4$byClass['Specificity']

Specificity_models4 = data.frame(model = c("logistic",
                                           "SVM",
                                           "decision_tree",
                                           "random_forest"),
                                 Specificity = c(Specificity_lr4 , Specificity_svm4
                                                 ,Specificity_dt4 , Specificity_rf4))

roc_rf4 = roc(observation4 , as.numeric( predicted_rf4))
plot.roc(roc_rf4,main="Random Forest ---> SMOTE")
s