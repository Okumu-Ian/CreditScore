#import data
german <- read.table(file.choose(),header=F)

#cleaning of data
#removing column with unwanted variables
german = german[,c(-6,-11,-12,-14,-16,-17)]

#title the columns
colnames(german) <- c("checking_account_status","loan_duration_months","credit_rating","loan_purpose","loan_amount","employment_status","loan_payment_ration","marital_gender"
                      ,"other_debtors","age","housing_status","dependents","has_telephone","is_foreign_worker","cost_matrix")

str(german)

#convert values to text.
german[,"checking_account_status"] <- sapply(german[,"checking_account_status"], as.character)

str(german)
#changing checking account status
for(i in 1:1000){
  
  value_to_change = german[i,"checking_account_status"]
  if(value_to_change == "A11"){
    german[i,"checking_account_status"] = "1"
  }else if(value_to_change == "A12"){
    german[i,"checking_account_status"] = "2"
  }else if(value_to_change == "A13"){
    german[i,"checking_account_status"] = "3"
  }else if(value_to_change == "A14"){
    german[i,"checking_account_status"] = "0"
  }
  
}

#convert to 
german[,"checking_account_status"] <- sapply(german[,"checking_account_status"], as.factor)
str(german)

#visualize the checking account status using a barplot
checking_account_status.barplot <- barplot(table(german[,1]))

#convert values to text.
german[,"credit_rating"] <- sapply(german[,"credit_rating"], as.character)
str(german)

#changing credit rating
for(i in 1:1000){
  
  value_to_change = german[i,"credit_rating"]
  if(value_to_change == "A30"){
    german[i,"credit_rating"] = "0"
  }else if(value_to_change == "A31"){
    german[i,"credit_rating"] = "1"
  }else if(value_to_change == "A32"){
    german[i,"credit_rating"] = "2"
  }else if(value_to_change == "A33"){
    german[i,"credit_rating"] = "3"
  }else {german [i,"credit_rating"]="4"}
  
}

#convert to 
german[,"credit_rating"] <- sapply(german[,"credit_rating"], as.factor)
str(german)

#visualize the credit rating using a barplot
credit_rating.barplot <- barplot(table(german[,3]))

#convert values to text.
german[,"loan_purpose"] <- sapply(german[,"loan_purpose"], as.character)
str(german)

#changing loan_purpose
for(i in 1:1000){
  
  value_to_change = german[i,"loan_purpose"]
  if(value_to_change == "A40"){
    german[i,"loan_purpose"] = "car_new"
  }else if(value_to_change == "A41"){
    german[i,"loan_purpose"] = "car_used"
  }else if(value_to_change == "A42"){
    german[i,"loan_purpose"] = "furniture_equipment"
  }else if(value_to_change == "A43"){
    german[i,"loan_purpose"] = "radio_television"
  }else if (value_to_change =="A44"){
    german[i,"loan_purpose"]="domestic_appliances"
  }else if (value_to_change == "A45"){
    german[i,"loan_purpose"] = "repairs"
  }else if (value_to_change == "A46"){
    german[i,"loan_purpose"] = "education"
  }else if (value_to_change == "A47"){
    german[i,"loan_purpose"] = "vacation_does_not_exist"
  }else if (value_to_change == "A48"){
    german[i,"loan_purpose"] = "retaining"
  }else if (value_to_change == "A49"){
    german[i,"loan_purpose"] = "business"
  }else if (value_to_change == "A410"){
    german[i,"loan_purpose"] = "others"
  }
  }


#convert to 
german[,"loan_purpose"] <- sapply(german[,"loan_purpose"], as.factor)
str(german)

#visualize the loan_purpose using a barplot
loan_purpose.barplot <- barplot(table(german[,4]))

#convert values to text.
german[,"employment_status"] <- sapply(german[,"employment_status"], as.character)
str(german)

#changing employment_status
for(i in 1:1000){
  
  value_to_change = german[i,"employment_status"]
  if(value_to_change == "A71"){
    german[i,"employment_status"] = "0"
  }else if(value_to_change == "A72"){
    german[i,"employment_status"] = "1"
  }else if(value_to_change == "A73"){
    german[i,"employment_status"] = "2"
  }else if(value_to_change == "A74"){
    german[i,"employment_status"] = "3"
  }else if (value_to_change =="A75"){
    german[i,"employment_status"]="4"
}
}


#convert to 
german[,"employment_status"] <- sapply(german[,"employment_status"], as.factor)
str(german)

#visualize the employment_status using a barplot
employment_status.barplot <- barplot(table(german[,6]))



#convert values to text.
german[,"marital_gender"] <- sapply(german[,"marital_gender"], as.character)
str(german)

#changing marital_gender
for(i in 1:1000){
  
  value_to_change = german[i,"marital_gender"]
  if(value_to_change == "A91"){
    german[i,"marital_gender"] = "male_divorced_separated"
  }else if(value_to_change == "A92"){
    german[i,"marital_gender"] = "female_divorced_separated_married"
  }else if(value_to_change == "A93"){
    german[i,"marital_gender"] = "male_single"
  }else if(value_to_change == "A94"){
    german[i,"marital_gender"] = "male_married_widowed"
  }else if (value_to_change =="A95"){
    german[i,"marital_gender"]="female_single"
  }
}


#convert values to text
german[,"marital_gender"] <- sapply(german[,"marital_gender"], as.factor)
str(german)

#visualize the marital_gender using a barplot
marital_gender.barplot <- barplot(table(german[,8]))

#convert values to text.
german[,"other_debtors"] <- sapply(german[,"other_debtors"], as.character)
str(german)

#changing other_debtors
for(i in 1:1000){
  
  value_to_change = german[i,"other_debtors"]
  if(value_to_change == "A101"){
    german[i,"other_debtors"] = "none"
  }else if(value_to_change == "A102"){
    german[i,"other_debtors"] = "co_applicant"
  }else if(value_to_change == "A103"){
    german[i,"other_debtors"] = "guarantor"
  }
}


#convert values to text
german[,"other_debtors"] <- sapply(german[,"other_debtors"], as.factor)
str(german)

#visualize the other debtors using a barplot
other_debtors.barplot <- barplot(table(german[,9]))

#convert values to text.
german[,"housing_status"] <- sapply(german[,"housing_status"], as.character)
str(german)

#changing housing_status
for(i in 1:1000){
  
  value_to_change = german[i,"housing_status"]
  if(value_to_change == "A151"){
    german[i,"housing_status"] = "rent"
  }else if(value_to_change == "A152"){
    german[i,"housing_status"] = "own"
  }else if(value_to_change == "A153"){
    german[i,"housing_status"] = "for_free"
  }
}


#convert to 
german[,"housing_status"] <- sapply(german[,"housing_status"], as.factor)
str(german)

#visualize the housing_status using a barplot
housing_status.barplot <- barplot(table(german[,11]))


#convert values to text.
german[,"has_telephone"] <- sapply(german[,"has_telephone"], as.character)
str(german)

#changing has_telephone
for(i in 1:1000){
  
  value_to_change = german[i,"has_telephone"]
  if(value_to_change == "A191"){
    german[i,"has_telephone"] = "no"
  }else if(value_to_change == "A192"){
    german[i,"has_telephone"] = "yes"
  }
}


#convert to 
german[,"has_telephone"] <- sapply(german[,"has_telephone"], as.factor)
str(german)

#visualize the has_telephone using a barplot
has_telephone.barplot <- barplot(table(german[,13]))


#convert values to text.
german[,"is_foreign_worker"] <- sapply(german[,"is_foreign_worker"], as.character)
str(german)

#changing is_foreign_worker
for(i in 1:1000){
  
  value_to_change = german[i,"is_foreign_worker"]
  if(value_to_change == "A201"){
    german[i,"is_foreign_worker"] = "yes"
  }else if(value_to_change == "A202"){
    german[i,"is_foreign_worker"] = "no"
  }
}


#convert to 
german[,"is_foreign_worker"] <- sapply(german[,"is_foreign_worker"], as.factor)
str(german)

#visualize the is_foreign_worker using a barplot
foreign_worker.barplot <- barplot(table(german[,14]))

#summary of measures of dispersion of the german dataset
german.summary <- summary(german)

#Visualization of the data.
loan.duration.hist <- hist(german[,"loan_duration_months"])

#splitting values for training and testing
#list of(100 random) numbers from 1 -> 1000
test_list <- sample(1:1000, 100, replace = FALSE)

#create a test sample dataset
test_german <- german[test_list,]

#create train data
train_german <- german[-test_list,]



#create a function that trains multinomial logistic regression using nnet
train.multinom.function <- function(dataset.train, dataset.test, dataset.relevel){

  #building a multinomial logistic regression model
  #set the baseline for regression
  dataset.train$credit_rating <- relevel(dataset.train$credit_rating, ref = dataset.relevel)
  
  #create a list with information -> model details
    
  #load the nnet package
  require(nnet)
  
  #fit the multinomial logistic regression model
  multinom.fit <- multinom(credit_rating ~ ., data = dataset.train)
  #summary of model
  model.summary <- summary(multinom.fit)
  
  model_list_details <- list("Model_Summary" = model.summary)
  
  #check the exponentials of the coefficients
  model.coefficients <- exp(coef(multinom.fit))
  model_list_details[["coefficient_values"]] <- model.coefficients
  
  #probabilities for better understanding
  model.probabilities.head <- head(probability.table <- fitted(multinom.fit))
  model_list_details[["prob_head"]] <- model.probabilities.head
  
  #predicting values for our train set
  dataset.train$predicted <- predict(multinom.fit, newdata = dataset.train, "class")
  
  #building a classification table
  ctable <- table(dataset.train$credit_rating, dataset.train$predicted)
  model_list_details[["train_classification_table"]] <- ctable
  
  # Calculating accuracy - sum of diagonal elements divided by total obs
  train.accuracy <- round((sum(diag(ctable))/sum(ctable))*100,2)
  model_list_details[["train_accuracy"]] <- train.accuracy
  
  #test with unseen data
  # Predicting the values for train dataset
  dataset.test$predict <- predict(multinom.fit, newdata = dataset.test, "class")
  
  # Building classification table
  ctable <- table(dataset.test$credit_rating, dataset.test$predict)
  
  
  #rearrange columns
  temp_col_name_0 <- colnames(ctable)[colnames(ctable) == 0]
  temp_col_name_1 <- colnames(ctable)[colnames(ctable) == 1]
  temp_col_name_2 <- colnames(ctable)[colnames(ctable) == 2]
  temp_col_name_3 <- colnames(ctable)[colnames(ctable) == 3]
  temp_col_name_4 <- colnames(ctable)[colnames(ctable) == 4]
  
  temp_col_0 <- ctable[,c("0")]
  temp_col_1 <- ctable[,c("1")]
  temp_col_2 <- ctable[,c("2")]
  temp_col_3 <- ctable[,c("3")]
  temp_col_4 <- ctable[,c("4")]
  
  ctable[,1] <- temp_col_4
  ctable[,2] <- temp_col_2
  ctable[,3] <- temp_col_3
  ctable[,4] <- temp_col_0
  ctable[,5] <- temp_col_1
  
  colnames(ctable) <- c(4,2,3,0,1)
  model_list_details[["test_classification_table"]] <- ctable
  
  # Calculating accuracy - sum of diagonal elements divided by total observation
  test.accuracy <- round((sum(diag(ctable))/sum(ctable))*100,2)
  model_list_details[["test_accuracy"]] <- test.accuracy
  
  return(model_list_details)
  
}

values <- train.multinom.function(dataset.train = train_german, dataset.test = test_german, dataset.relevel = "0")
