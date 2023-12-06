### Load the data
```{r echo=TRUE , warning= FALSE , message= FALSE}
library(tidyverse)
library(gridExtra)
library(readxl)
churn <- read_xlsx("Telco_customer_churn.xlsx")
```

### Exploratory data analysis
```{r}
str(churn)
```

The churn dataset has 7043 observations and 33 variables

### check missing values
```{r}
sapply(churn, function(x) sum(is.na(x)))
```

##Remove missing values
```{r}
churn = churn %>% select(-`Churn Reason`)
churn <- na.omit(churn)
```

```{r}
churn<- churn %>% select(-c(CustomerID,Count,Country,State,City,`Zip Code`,`Lat Long`,Latitude,Longitude,`Churn Label`))
```

In churn value 1 represents yes the customer churned and 0 represents No

```{r}
churn <- churn %>% mutate_if(is.character , as.factor)
str(churn)
```

```{r}
churn$`Churn Value` = as.factor(churn$`Churn Value`)
ggplot(churn, aes(x=`Churn Value`,fill=`Churn Value`))+ geom_bar() + geom_text(aes(y = ..count..-200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', 
                                                                               position = position_dodge(.1), size = 3)+ ylab("Count")
```

```{r}
churn %>% 
  group_by(`Tenure Months`, `Churn Value`) %>% 
  dplyr::summarise(Number = n()) %>% 
  ggplot(aes(`Tenure Months`, Number)) +
  geom_line(aes(col = `Churn Value`)) +
  labs(x = "Tenure (month)",
       y = "Number of Customer",
       title = "Churn Based on Tenure") +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  theme_minimal()
```

```{r}
churn_score = churn %>% 
  group_by(`Churn Value`) %>% 
  summarise(Churn_Score = mean(`Churn Score`))

ggplot(churn_score, aes(x = `Churn Value`, y = Churn_Score, fill = `Churn Value`)) +
  geom_bar(stat = "identity") +  # Use geom_bar for bar plot
  geom_text(aes(label = round(Churn_Score, 2)),  # Use round to limit decimal places
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Average Churn Score by Churn Value",
       x = "Churn Value",
       y = "Average Churn Score") +
  theme_minimal()
```
Churn Score of 82.51043 has a high likelihood of churning (Churn Value = 1), while a customer with a Churn Score of 50.10149 is less likely to churn (Churn Value = 0).

```{r}
cltv_score = churn %>% 
  group_by(`Churn Value`) %>% 
  summarise(CLTV_Score = mean(`CLTV`))

ggplot(cltv_score, aes(x = `Churn Value`, y = CLTV_Score, fill = `Churn Value`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(CLTV_Score, 2)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Average CLTV by Churn Value",
       x = "Churn Value",
       y = "Average CLTV") +
  theme_minimal()
```

```{r}
a<-ggplot(churn, aes(x=Gender,fill=`Churn Value`))+ geom_bar() + geom_text(aes(y = ..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', 
                                                                           position = position_dodge(.1),  size = 3)+ ylab("Count")


b<-ggplot(churn, aes(x=`Senior Citizen`,fill=`Churn Value`))+ geom_bar() + geom_text(aes(y = ..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), 
                                                                                     stat = 'count', position = position_dodge(0.1), size = 3)+ ylab("Count")



c<-ggplot(churn, aes(x=Partner,fill=`Churn Value`))+ geom_bar() + geom_text(aes(y = ..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', 
                                                                            position = position_dodge(.1), size = 3)+ ylab("Count")


d<-ggplot(churn, aes(x=Dependents,fill=`Churn Value`))+ geom_bar() + geom_text(aes(y = ..count.. -200,label = paste0(round(prop.table(..count..),4) * 100, '%')), 
                                                                               stat = 'count',position = position_dodge(.1), size = 3) + ylab("Count")

grid.arrange(a,b,c,d,ncol=2)

```

```{r}
grid.arrange(ggplot(churn, aes(x=`Internet Service`,fill=`Churn Value`))+ geom_bar() + geom_text(aes(y = ..count.. -200,label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1),size = 3)+ ylab("Count"),
             
             ggplot(churn, aes(x=`Online Security`,fill=`Churn Value`))+ geom_bar() + geom_text(aes(y = ..count.. -200,label = paste0(round(prop.table(..count..),4) * 100, '%')),stat = 'count',position = position_dodge(0.2),size = 3)+ ylab("Count"),
             
             ggplot(churn, aes(x=`Online Backup`,fill=`Churn Value`))+ geom_bar() + geom_text(aes(y = ..count.. -200,label = paste0(round(prop.table(..count..),4) * 100, '%')),stat = 'count',position = position_dodge(0.2),size = 3)+ ylab("Count"),
             
             ggplot(churn, aes(x=`Device Protection`,fill=`Churn Value`))+ geom_bar() + geom_text(aes(y = ..count.. -200,label = paste0(round(prop.table(..count..),4) * 100, '%')),stat = 'count',position = position_dodge(0.2),size = 3)+ ylab("Count"),
             
             ggplot(churn, aes(x=`Phone Service`,fill=`Churn Value`))+ geom_bar() + geom_text(aes(y = ..count.. -200,label = paste0(round(prop.table(..count..),4) * 100, '%')),stat = 'count',position = position_dodge(0.2),size = 3)+ ylab("Count"),
             
             ggplot(churn, aes(x=`Multiple Lines`,fill=`Churn Value`))+ geom_bar() + geom_text(aes(y = ..count.. -200,label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count',position = position_dodge(0.2), size = 3)+ ylab("Count"),
             
             ggplot(churn, aes(x=`Tech Support`,fill=`Churn Value`))+ geom_bar() + geom_text(aes(y = ..count.. -200,label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(0.2),size = 3)+ ylab("Count"),
             
             ggplot(churn, aes(x=`Streaming TV`,fill=`Churn Value`))+ geom_bar() + geom_text(aes(y = ..count.. -200,label = paste0(round(prop.table(..count..),4) * 100, '%')),stat = 'count',position = position_dodge(0.2), size = 3)+ ylab("Count"),
             
             ggplot(churn, aes(x=`Streaming Movies`,fill=`Churn Value`))+ geom_bar() + geom_text(aes(y = ..count.. -200,label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count',position = position_dodge(0.2),size = 3)+ ylab("Count"))
```

### Statistical tests
```{r}
str(churn)
chisq.test(churn$`Churn Value`, churn$Gender)
chisq.test(churn$`Churn Value`, churn$`Senior Citizen`)
chisq.test(churn$`Churn Value`, churn$Partner)
chisq.test(churn$`Churn Value`, churn$Dependents)
chisq.test(churn$`Churn Value`, churn$`Phone Service`)
chisq.test(churn$`Churn Value`, churn$`Multiple Lines`)
chisq.test(churn$`Churn Value`, churn$`Internet Service`)
chisq.test(churn$`Churn Value`, churn$`Online Security`)
chisq.test(churn$`Churn Value`, churn$`Online Backup`)
chisq.test(churn$`Churn Value`, churn$`Device Protection`)
chisq.test(churn$`Churn Value`, churn$`Tech Support`)
chisq.test(churn$`Churn Value`, churn$`Streaming TV`)
chisq.test(churn$`Churn Value`, churn$`Streaming Movies`)
chisq.test(churn$`Churn Value`, churn$`Paperless Billing`)
chisq.test(churn$`Churn Value`, churn$`Payment Method` )
chisq.test(churn$`Churn Value`, churn$Contract)

t.test(`Tenure Months`~`Churn Value`,data=churn)
t.test(`Total Charges`~`Churn Value`,data=churn)
t.test(`Monthly Charges`~`Churn Value`,data=churn)
t.test(`Churn Score`~`Churn Value`,data=churn)
t.test(CLTV~`Churn Value`,data=churn)
```

```{r}
library(GGally)
ggcorr(churn, label = T)
```

```{r}
churn <- churn %>% select(-c(`Total Charges`,Gender,`Phone Service`))
churn$`Churn Value` = as.factor(ifelse(churn$`Churn Value` == 0 , "No","Yes"))
library(caret)
split_train_test <- createDataPartition(churn$`Churn Value`,p=0.7,list=FALSE)
set.seed(1234)
dtrain<- churn[split_train_test,]
dtest<- churn[-split_train_test,]


mod <- glm(`Churn Value` ~., data = dtrain,
              family=binomial(link='logit'))
summary(mod)
```

```{r}
churn <- churn %>%
  mutate(
    `Multiple Lines` = as.factor(case_when(
      `Multiple Lines` == "No phone service" ~ "No",
      TRUE ~ as.character(`Multiple Lines`)
    )),
    `Online Security` = as.factor(case_when(
      `Online Security` == "No internet service" ~ "No",
      TRUE ~ as.character(`Online Security`)
    )),
    `Online Backup` = as.factor(case_when(
      `Online Backup` == "No internet service" ~ "No",
      TRUE ~ as.character(`Online Backup`)
    )),
    `Device Protection` = as.factor(case_when(
      `Device Protection` == "No internet service" ~ "No",
      TRUE ~ as.character(`Device Protection`)
    )),
    `Tech Support` = as.factor(case_when(
      `Tech Support` == "No internet service" ~ "No",
      TRUE ~ as.character(`Tech Support`)
    )),
    `Streaming TV` = as.factor(case_when(
      `Streaming TV` == "No internet service" ~ "No",
      TRUE ~ as.character(`Streaming TV`)
    )),
    `Streaming Movies` = as.factor(case_when(
      `Streaming Movies` == "No internet service" ~ "No",
      TRUE ~ as.character(`Streaming Movies`)
    ))
  )


```


```{r}
split_train_test <- createDataPartition(churn$`Churn Value`,p=0.7,list=FALSE)
set.seed(1234)
dtrain<- churn[split_train_test,]
dtest<- churn[-split_train_test,]


mod_log <- glm(`Churn Value` ~., data = dtrain,
           family=binomial(link='logit'))
summary(mod_log)
```
#Confusion matrix
```{r}
lr_prob1 <- predict(mod_log, dtest,type = "response")
lr_pred1 <- ifelse(lr_prob1 > 0.6,"Yes","No")
cm_log = confusionMatrix(as.factor(lr_pred1), reference = dtest$`Churn Value`)
```

#Gradient Boosted Tree
```{r}
set.seed(1)

gbm <- train(`Churn Value`~., data = dtrain, method = "gbm",metric = "ROC", trControl = trainControl("cv", number = 10,sampling = "smote",summaryFunction = twoClassSummary,classProbs = TRUE),  preProc="nzv")
predictions <- predict(gbm,dtest)
cm_gbm = confusionMatrix(predictions,dtest$`Churn Value`)
```
### Results
```{r}
data.frame(
  Model = c("Logistic Regression", "GBM"),
  Accuracy = c(0.9137, 0.935),
  Sensitivity = c(0.9664, 0.9444),
  Specificity = c(0.7679, 0.9089),
  Kappa = c(0.7684, 0.8367)
)
```
