library(tidyverse)

getwd()

setwd("C:\\Users\\User\\OneDrive\\Desktop\\Courses\\RP\\SDBA\\Post-Diploma Certificate in Advanced Business Analytics\\PAML\\C3439C_Coursework-2")

getwd()

df <- read_csv("CW2_Telco_Churn_Data_cleaned.csv")

df_new <- df %>% mutate_if(sapply(., is.character),
                           as.factor)

df_new <- as.data.frame(df_new)

df_new <- df_new %>% select(-1)

as.data.frame(table(df_new$Churn))
#   Var1 Freq
# 1   No 5174
# 2  Yes 1869

# install.packages("DMwR_0.4.1.tar.gz", repos=NULL, 
#                  type="source")

library(caTools)

split <- sample.split(df_new$Churn, SplitRatio = 0.7)

train <- subset(df_new, split == TRUE)
test <- subset(df_new, split == FALSE)

as.data.frame(table(train$Churn))
#   Var1 Freq
# 1   No 3622
# 2  Yes 1308

library(funModeling)
df_status(df_new)

library(DMwR)

## Smote : Synthetic Minority Oversampling Technique 
# To Handle Class Imbalancy In Binary Classification
balanced.data <- SMOTE(Churn ~., 
                       train, perc.over = 100, k = 5)

as.data.frame(table(balanced.data$Churn))
#   Var1 Freq
# 1   No 2616
# 2  Yes 2616

library(randomForest)  
library(e1071)  


rf = randomForest(Churn~.,  
                  ntree = 100,
                  data = balanced.data)
plot(rf) 

library(caret)
varImp(rf)

## Important variables according to the model
varImpPlot(rf,  
           sort = T,
           n.var=25,
           main="Variable Importance")

predicted.response <- predict(rf, test)


confusionMatrix(data=predicted.response,  
                reference=test$Churn)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   No  Yes
# No  1159  155
# Yes  393  406
# 
# Accuracy : 0.7407          
# 95% CI : (0.7214, 0.7592)
# No Information Rate : 0.7345          
# P-Value [Acc > NIR] : 0.2698          
# 
# Kappa : 0.4144          
# 
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.7468          
#             Specificity : 0.7237          
#          Pos Pred Value : 0.8820          
#          Neg Pred Value : 0.5081          
#              Prevalence : 0.7345          
#          Detection Rate : 0.5485          
#    Detection Prevalence : 0.6219          
#       Balanced Accuracy : 0.7352          
#                                           
#        'Positive' Class : No       