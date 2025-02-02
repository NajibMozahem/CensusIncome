## install libraries
packages <- c("tidyverse", "dplyr", "caret", "randomForest")

lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }  
})

##read the data
the_data <- read_csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"),
                     col_names = FALSE)

colnames(the_data) <- c("age", "workclass", "fnlwgt", "education",
                        "education_num", "maritial_status", "occupation", "relationship", "race",
                        "sex", "capital_gain", "capital_loss",
                        "hours_per_week", "native_country", "income")

##convert characters to factors
the_data <- the_data %>% mutate(across(where(is.character), as.factor))

##some ? values in country
unique(the_data$native_country)

##plot bar graphs that help visually the results
ggplot(the_data) + 
  geom_bar(aes(maritial_status, fill = income), 
           position = "fill") + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.5))

ggplot(the_data) + 
  geom_bar(aes(workclass, fill = income), 
           position = "fill") + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.5))

ggplot(the_data) + 
  geom_bar(aes(education, fill = income), 
           position = "fill") + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.5))

ggplot(the_data) + 
  geom_bar(aes(occupation, fill = income), 
           position = "fill") + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.5))

ggplot(the_data) + 
  geom_bar(aes(race, fill = income), 
           position = "fill") + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.5))

ggplot(the_data) + 
  geom_bar(aes(relationship, fill = income), 
           position = "fill") + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.5))

ggplot(the_data) + 
  geom_bar(aes(sex, fill = income), 
           position = "fill") + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.5))

##there are too many countries. Better to manually produce
## plots for countries with highest ratios of >50K and 
##countries with lowest ratios of >50K
the_data %>% group_by(native_country) %>% 
  mutate(ratio = sum(income == ">50K")/n()) %>% 
  filter(row_number() == 1) %>% ungroup() %>% 
  select(native_country, ratio) %>% slice_max(ratio, n = 10) %>% 
  ggplot() + 
  geom_bar(aes(reorder(native_country, desc(ratio)), ratio), 
           stat = "identity", fill = "green") + 
  theme(axis.text.x = element_text(angle = 90, size = 8)) + 
  xlab("Native country") + 
  ylab("Ratio with income>50K") + coord_flip()

the_data %>% group_by(native_country) %>% 
  mutate(ratio = sum(income == ">50K")/n()) %>% 
  filter(row_number() == 1) %>% ungroup() %>% 
  select(native_country, ratio) %>% slice_min(ratio, n=10) %>% 
  ggplot() + 
  geom_bar(aes(reorder(native_country, desc(ratio)), ratio), 
           stat = "identity", fill = "red") + 
  theme(axis.text.x = element_text(angle = 90, size = 8)) + 
  xlab("Native country") + 
  ylab("Ratio with income<=50K") + coord_flip()

##calculate chi-square statistics for categorical variables
chi <- lapply(the_data[, sapply(the_data, is.factor) & colnames(the_data) != "income"], function(x) {
  chisq.test(the_data[, 15], x)
})
chi <- do.call(rbind, chi)[, c(1,3)]
chi <- as.data.frame(chi)
chi$p.value <- as.numeric(chi$p.value)
chi %>% arrange(p.value)
##looking at the order, we see that education, marital status,
## occupation, and sex are among the most significant

##we next look at the numeric variables
## look at differences in averages for numeric variables 
## for both groups
compare_avgs <- lapply(the_data[, sapply(the_data, is.numeric)], function(x) {
  data <- cbind(x, the_data[, 15])
  head(data)
  data %>% group_by(income) %>% summarise(avg = mean(x, na.rm = TRUE))
})
compare_avgs <- do.call(rbind, compare_avgs)
compare_avgs <- data.frame(compare_avgs)
compare_avgs$avg <- format(compare_avgs$avg, scientific = FALSE)
compare_avgs
## we see that people who make >50K are older, more 
## educated, and work more
##now calculate ttest statistics for these numeric variables
ttest <- lapply(the_data[, sapply(the_data, is.numeric)], function(x) {
  t.test(x ~ the_data$income)
})
ttest <- do.call(rbind, ttest)[, c(1, 3)]
ttest <- as.data.frame(ttest)
ttest$p.value <- as.numeric(ttest$p.value)
ttest %>% arrange(p.value)
## looking at the order we see that age, education, and hours
##worked have the highest level of significance

#logistic regression

set.seed(1982, sample.kind = "Rounding")

test_index = createDataPartition(the_data$income, p = 0.5, list = FALSE)
train_set <- the_data[-test_index, ]
test_set <- the_data[test_index, ]


model_log <- glm(income ~ ., data = train_set, 
                 family = "binomial")
yhat_log <- predict(model_log, train_set, type = "response")

prob <- seq(0.5, 0.7, 0.02)
results <- lapply(prob, function(x) {
  pred <- ifelse(yhat_log > x, ">50K", "<=50K")
  pred <- factor(pred, levels=levels(train_set$income))
  conf <- confusionMatrix(pred, train_set$income)
  accuracy <- conf$overall["Accuracy"]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  tibble(Accuracy = accuracy, Sensitivity = sensitivity, 
         Specificity = specificity)
    
})
results <- do.call(rbind, results)
results <- cbind(results, prob)
results %>% pivot_longer(1:3, names_to = "measure") %>% 
  ggplot() + geom_line(aes(prob, value, color = measure))

model_log <- train(income ~ ., data = train_set,
                   method = "glm", family = "binomial",
                   trControl = trainControl(method = "cv",
                                            number = 5))
predict_log <- predict(model_log, test_set)
results_log <- confusionMatrix(predict_log, test_set$income)

model_knn <- train(income ~ ., data = train_set,
                   method = "knn",
                   trControl = trainControl(method = "cv",
                                            number = 5),
                   tuneGrid = expand.grid(k = seq(2, 50, 1)))
ggplot(model_knn, highlight = TRUE)
predict_knn <- predict(model_knn, test_set)
results_knn <- confusionMatrix(predict_knn, test_set$income)

model_rf <- train(income ~ ., data = train_set,
                   method = "rf",
                   trControl = trainControl(method = "cv",
                                            number = 5),
                  tuneGrid = expand.grid(.mtry = c(1:15)))
ggplot(model_rf, highlight = TRUE)
predict_rf <- predict(model_rf, test_set)
results_rf <- confusionMatrix(predict_rf, test_set$income)

##ensemble
ensemble_data <- data.frame(Logistic = predict_log, 
                            Knn = predict_knn, 
                            Randomforest = predict_rf)
ensemble_data$vote <- rowSums(ensemble_data == ">50K")
ensemble_data$ensemble <- ifelse(ensemble_data$vote > 1, ">50K", "<=50K")
ensemble_data$ensemble <- factor(ensemble_data$ensemble, levels=levels(test_set$income))
results_ensemble <- confusionMatrix(ensemble_data$ensemble, test_set$income)

results <- data.frame(Model = "Logistic", 
                      Accuracy = results_log$overall["Accuracy"],
                      Sensitivity = results_log$byClass[1],
                      Specificity = results_log$byClass[2])
results <- results %>% add_row(Model = "KNN",  
                               Accuracy = results_knn$overall["Accuracy"],
                               Sensitivity = results_knn$byClass[1],
                               Specificity = results_knn$byClass[2])
results <- results %>% add_row(Model = "Random Forest",  
                               Accuracy = results_rf$overall["Accuracy"],
                               Sensitivity = results_rf$byClass[1],
                               Specificity = results_rf$byClass[2])
results <- results %>% add_row(Model = "Ensemble",  
                               Accuracy = results_ensemble$overall["Accuracy"],
                               Sensitivity = results_ensemble$byClass[1],
                               Specificity = results_ensemble$byClass[2])
results
## seems the random forest is the best overall
## get the best tune mtry to use in randomForest() function
## so that we can get the variable importance
mtry_best <- model_rf$bestTune$mtry

model_randomForest <- randomForest(income ~ ., data = test_set,
                                   mtry = mtry_best)
as.data.frame(importance(model_randomForest)) %>% 
  rownames_to_column("Variable") %>% 
  ggplot() + geom_bar(aes(reorder(Variable, -MeanDecreaseGini), 
                          MeanDecreaseGini), 
                      stat = "identity", fill = "blue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8)) + 
  xlab("Variables")

fourfoldplot(results_rf$table,
             color = c("#CC6666", "#99CC99"),
             conf.level = 0,
             main = "Random Forest Confusion Matrix")
