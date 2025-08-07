##########Approach3:RUS-----> Random Under Sample#######
######training_testing_mydata####
set.seed(402)
mydata3 = downSample(x=mydata[,-1] , y=mydata$pcf , yname="pcf")
sample3 = createDataPartition(mydata3$pcf, p = 0.8, list = FALSE)
train_data3 = mydata3[sample3,]
test_data3 = mydata3[-sample3,]

#####Fit_Logistic_Regression####
mylr3 = glm(pcf ~ ., data = train_data3
            , family = "binomial")
summary(mylr3)


mylr1_power3 = glm(as.numeric(pcf) ~ .+age^2+bmi^2+
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

#####Validation of mylr3####
predicted_lr3 = predict(mylr3 ,test_data3 , type = "response")
predicted_classes3 = ifelse(predicted_lr3 > 0.5 , 1 , 0)
observation3 = test_data3$pcf
cm_lr3 = confusionMatrix(as.factor(observation3), 
                         as.factor(predicted_classes3))
accuracy_lr3 = cm_lr3$overall['Accuracy']
precision_lr3 = cm_lr3$byClass['Pos Pred Value']
recall_lr3 = cm_lr3$byClass['Sensitivity']
f1_score_lr3 = cm_lr3$byClass['F1']
roc_score_lr3 = roc(observation3 , predicted_classes3)
plot(roc_score_lr3 , main = "ROC -- Logistic Regression")
#####Fit_Support_vector_machine3####
mysvm3 = svm(pcf ~ . ,data = train_data3 , type ="C" 
             , kernel = "linear")
mysvm3

#####The importance of variables in mysvm3####
weight3 = t(mysvm3$coefs) %*% mysvm3$SV
sd(weight3)
weight3 %>% abs %>% as.data.frame() %>% sort(decreasing = TRUE)

#####Validation of mysvm3####
predicted_svm3 = predict(mysvm3 , na.omit(test_data3) , type = "response")
observation3 = na.omit(test_data3)$pcf

cm_svm3 = confusionMatrix(as.factor(predicted_svm3),
                          as.factor(observation3))
cm_svm3
roc_score_svm3 = roc(observation3 , as.numeric(predicted_svm3))
plot(roc_score_svm3 , main = "ROC -- Logistic Regression")
#######Fit_Desicion_TREE#####
mydt3 = rpart(pcf ~ . , method = "class"
              ,data = train_data3 , parms=list(split="information"))
printcp(mydt)
summary(mydt)
######Validation_DT#####
predicted_dt3 = predict(mydt3 , test_data3 , type = "class")
observation3 = test_data3$pcf
cm_dt3 = confusionMatrix(as.factor(observation3),
                         as.factor(predicted_dt3))
cm_dt3
cccccc
#####Fit_Random_Forest####
set.seed(402)
myrf3 = randomForest(pcf ~ . , data = na.omit(train_data3) , 
                     importance = TRUE)
plot(myrf3)
varImpPlot(myrf3)
#####Validation_RF####
predicted_rf3 = predict(myrf3 , test_data3 , type = "class")
observation3 = test_data3$pcf
cm_rf3 = confusionMatrix(as.factor(observation3),
                         as.factor(predicted_rf3))
test_roc_rf3 = roc(observation3 ~ as.numeric( predicted_rf3),plot=T,
                   print.auc = TRUE)
#####Acuracy_four_models#####
acuracy_lr3 = cm_lr3$overall['Accuracy']
acuracy_svm3 = cm_svm3$overall['Accuracy']
acuracy_dt3 = cm_dt3$overall['Accuracy']
acuracy_rf3 = cm_rf3$overall['Accuracy']

acuracy_models3 = data.frame(model = c("logistic",
                                       "Support_vector_machine",
                                       "decision_tree",
                                       "random_forest"),
                             acuracy = c(acuracy_lr3 , acuracy_svm3
                                         ,acuracy_dt3 , acuracy_rf3))
ggplot(data = acuracy_models2, aes(x = acuracy,y = model, label =
                                     round(acuracy,2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")
#####Presicion of Four models####
precision_lr3 = cm_lr3$byClass['Pos Pred Value']
precision_svm3 = cm_svm3$byClass['Pos Pred Value']
precision_dt3 = cm_dt3$byClass['Pos Pred Value']
precision_rf3 = cm_rf3$byClass['Pos Pred Value']

precision_models3 = data.frame(model = c("logistic",
                                         "support_vector_machine",
                                         "decision_tree",
                                         "random_forest"),
                               precision = c(precision_lr3 ,precision_svm3,
                                             precision_dt3 , precision_rf3))
ggplot(data =precision_models3, aes(x = precision , y = model, label =
                                      round(precision , 2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")

#####Recall of four models####
recall_lr3 = cm_lr3$byClass['Sensitivity']
recall_svm3 = cm_svm3$byClass['Sensitivity']
recall_dt3 = cm_dt3$byClass['Sensitivity']
recall_rf3 = cm_rf3$byClass['Sensitivity']

recall_models3 = data.frame(model = c("logistic",
                                      "support_vector_machine",
                                      "decision_tree",
                                      "random_forest"),
                            recall = c(recall_lr3 , recall_svm3,
                                       recall_dt3 , recall_rf3))

ggplot(data = recall_models3, aes(x = recall , y = model, label =
                                    round(recall , 2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")

#####F1 of four models####
f1_score_lr3 = cm_lr3$byClass['F1']
f1_score_svm3 = cm_svm3$byClass['F1']
f1_score_dt3 = cm_dt3$byClass['F1']
f1_score_rf3 = cm_rf3$byClass['F1']

f1_models3 = data.frame(model = c("logistic",
                                  "support_vector_machine",
                                  "decision_tree",
                                  "random_forest"),
                        f1_score= c(f1_score_lr3 , f1_score_svm3,
                                    f1_score_dt3 , f1_score_rf3))

ggplot(data = f1_models3, aes(x = f1_score , y = model, label =
                                round(f1_score , 2) ))+
  geom_bar(stat = 'identity')+
  geom_text(size = 5 , position=position_dodge(width=0.9),
            hjust=1.5,colour="white")

#####Specificity_four_models#####
Specificity_lr3 = cm_lr3$byClass['Specificity']
Specificity_svm3 = cm_svm3$byClass['Specificity']
Specificity_dt3 = cm_dt3$byClass['Specificity']
Specificity_rf3 = cm_rf3$byClass['Specificity']

Specificity_models3 = data.frame(model = c("logistic",
                                           "SVM",
                                           "decision_tree",
                                           "random_forest"),
                                 Specificity = c(Specificity_lr3 , Specificity_svm3
                                                 ,Specificity_dt3 , Specificity_rf3))


roc_rf3 = roc(observation3 , as.numeric( predicted_rf3))
plot.roc(roc_rf3,main="Random Forest ---> RUS")
