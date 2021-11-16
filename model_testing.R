library(tidyverse)
library(glmnet)
library(boot)
library(randomForest)
library(gbm)

#Read the data
data <- read.csv("historic_property_data.csv")

#Data Cleaning
#Check which variables have NA value for more than 10,000 rows
variables1 <- data %>% 
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))
print(variables1)

#Run linear regression on all variables in the data (except variables found in previous part) 
#to test the significance of the variables
data[is.na(data)] <- 0
set.seed(1)
model <- lm(sale_price~1+meta_class+meta_town_code+meta_nbhd+meta_certified_est_bldg
            +meta_certified_est_land+factor(meta_deed_type)
            +char_hd_sf+char_age+factor(char_ext_wall)
            +factor(char_roof_cnst)+char_rooms+char_beds+factor(char_bsmt)
            +factor(char_bsmt_fin)+factor(char_heat)+factor(char_oheat)+factor(char_air)
            +char_frpl+factor(char_attic_type)+char_fbath+char_hbath
            +factor(char_cnst_qlty)+factor(char_site)+factor(char_gar1_size)
            +factor(char_gar1_cnst)+factor(char_gar1_att)+factor(char_gar1_area)
            +char_ot_impr+char_bldg_sf+factor(char_repair_cnd)
            +factor(char_use)+factor(char_type_resd)+factor(geo_property_city)
            +factor(geo_property_zip)+geo_tract_pop+geo_white_perc+geo_black_perc
            +geo_asian_perc+geo_his_perc+geo_other_perc+factor(geo_fips)
            +factor(geo_municipality),data=data)

f <- summary(model)
variables <- names(f$coeff[-1,4])
variables_list = c()
factor_variable_list = c()
numeric_variable_list = c()

for (i in variables) {
  if (substr(i,1,1)=="f"){
    for (j in 6:nchar(i)){
      if (substr(i,j,j)==")"){
        if (!(substr(i,8,j-1) %in% variables_list)){
          variables_list <- append(variables_list,substr(i,8,j-1))
          factor_variable_list <- append(factor_variable_list,substr(i,8,j-1))
        }
      }
    }
  }else{
    variables_list<-append(variables_list,i)
    numeric_variable_list <- append(numeric_variable_list,i)
  }
}

#Check the scatter plot of numeric variable with sale_price
for (j in numeric_variable_list){
    print(ggplot(data,aes(x=get(j),y=sale_price))+
    geom_point()+
    ggtitle(j))
}
#no variable seems to have linear relationship with sale_price

#Check the scatter plot of numeric variable with log sale_price
for (j in numeric_variable_list){
  print(ggplot(data,aes(x=get(j),y=log(sale_price)))+
          geom_point()+
          geom_smooth(method="lm")+
          ggtitle(j))
}
#geo_white_perc seems to have relationship from the plot

#Check the scatter plot of log numeric variable with log sale_price
for (j in numeric_variable_list){
  print(ggplot(data,aes(x=log(get(j)),y=log(sale_price)))+
          geom_point()+
          geom_smooth(method="lm")+
          ggtitle(j))
}
#log(geo_white_perc),log(char_bldg_sf),log(meta_certified_est_land),log(meta_certified_est_bldg) all seem to have relationship with log(sale_price)

#check
for (k in factor_variable_list){
  print(data %>% 
        group_by(get(k)) %>% 
        ggplot(aes(group=get(k),y=log(sale_price)))+
        geom_boxplot() +
        ggtitle(k))
}
#char_roof_cnst,char_bsmt,char_oheat,char_air,char_type_resd seem to have relationship with log(sale_price)

#Run simple regression for these variables
temp_data <- data %>% 
  filter(complete.cases(geo_white_perc,char_bldg_sf,meta_certified_est_land,
                        meta_certified_est_bldg,char_roof_cnst,char_bsmt,char_oheat,char_air,char_type_resd))

#As numeric variables include 0, we have to add a very small number in order to make a log calculation for regression
sum(temp_data$meta_certified_est_bldg==0)
sum(temp_data$char_bldg_sf==0)
sum(temp_data$meta_certified_est_bldg==0)
temp_data$char_bldg_sf[temp_data$char_bldg_sf==0]<-0.0000000000001
temp_data$meta_certified_est_land[temp_data$meta_certified_est_land==0]<-0.0000000000001
temp_data$meta_certified_est_bldg[temp_data$meta_certified_est_bldg==0]<-0.0000000000001

temp_data <- temp_data %>% 
  mutate(log_char_bldg_sf=log(char_bldg_sf),log_meta_certified_est_land=log(meta_certified_est_land),
         log_meta_certified_est_bldg=log(meta_certified_est_bldg),factor_char_roof_cnst=factor(char_roof_cnst),
         factor_char_bsmt=factor(char_bsmt),factor_char_oheat=factor(char_oheat),factor_char_air=factor(char_air),
         factor_char_type_resd=factor(char_type_resd))


set.seed(1)
n_rows <- nrow(temp_data)
train <- sample(n_rows, n_rows*4/5)
train_data <- temp_data[train,]
test_data <- temp_data[-train,]

lm_model1 <-lm(log(sale_price) ~ 1 + geo_white_perc + log_char_bldg_sf
                    +log_meta_certified_est_land+log_meta_certified_est_bldg
                    + factor_char_roof_cnst + factor_char_bsmt + factor_char_oheat
                    + factor_char_air + factor_char_type_resd,data=train_data)

summary(lm_model1)

lm_model1_y_hat <- predict(lm_model1,newdata=test_data)
lm_model1_y_hat <- exp(lm_model1_y_hat)
print(lm_model1_y_hat)
lm_model1_mse <- mean((test_data$sale_price-lm_model1_y_hat)^2)


lm_model2 <-lm(log_sale_price ~ 1 + geo_white_perc + log_char_bldg_sf
               +log_meta_certified_est_land+log_meta_certified_est_bldg
               + factor_char_roof_cnst + factor_char_bsmt + factor_char_oheat 
               + factor_char_air ,data=train_data)

summary(lm_model2)

lm_model2_y_hat <- predict(lm_model2,newdata=test_data)
lm_model2_y_hat <- exp(lm_model2_y_hat)
lm_model2_mse <- mean((test_data$sale_price-lm_model2_y_hat)^2)


x <- model.matrix(log(sale_price) ~ 1 + geo_white_perc + log_char_bldg_sf
                  +log_meta_certified_est_land+log_meta_certified_est_bldg
                  + factor_char_roof_cnst + factor_char_bsmt + factor_char_oheat
                  + factor_char_air + factor_char_type_resd,data=train_data)
y <- log(train_data$sale_price)
lasso_model1 <- cv.glmnet(x=x, y=y)
coef(lasso_model1, s = "lambda.min")

newx <- model.matrix(log(sale_price) ~ 1 + geo_white_perc + log_char_bldg_sf
                     +log_meta_certified_est_land+log_meta_certified_est_bldg
                     + factor_char_roof_cnst + factor_char_bsmt + factor_char_oheat
                     + factor_char_air + factor_char_type_resd,data=test_data)
lasso1_y_hat <- predict(lasso_model1, newx = newx, s = "lambda.min")
lasso1_y_hat <- exp(lasso1_y_hat)
lasso1_mse <- mean((test_data$sale_price - lasso1_y_hat)^2)
lasso1_mse
best_model

#Select only those variables that have p-values less than 0.05
f <- summary(model)
selected <- f$coeff[-1,4]<0.05
variables <- names(f$coeff[-1,4])[selected==TRUE]
variables_list = c()
factor_variable_list = c()
numeric_variable_list = c()

for (i in variables) {
  if (substr(i,1,1)=="f"){
    for (j in 6:nchar(i)){
      if (substr(i,j,j)==")"){
        if (!(substr(i,8,j-1) %in% variables_list)){
          variables_list <- append(variables_list,substr(i,8,j-1))
          factor_variable_list <- append(factor_variable_list,substr(i,8,j-1))
        }
      }
    }
  }else{
    variables_list<-append(variables_list,i)
    numeric_variable_list <- append(numeric_variable_list,i)
  }
}
factor_variable_list
#group the variables with less than 100 observations in geo_property_city to "OTHERS"
city <- data %>% 
  group_by(geo_property_city) %>%
  summarize(n=n())

need_changed<-city$`geo_property_city`[city$n<100]

data$geo_property_city[data$geo_property_city %in% need_changed] <- "OTHERS"

#group the variables with less than 100 observations in geo_property_zip to "OTHERS"
zip <- data %>% 
  group_by(geo_property_zip) %>%
  summarize(n=n())

zip_need_change <- zip$`geo_property_zip`[zip$n<100]
data$geo_property_zip[data$geo_property_zip %in% zip_need_change] <- "OTHERS"

#group the variables with less than 100 observations in geo_fips to "OTHERS"
fips <- data %>% 
  group_by(geo_fips) %>%
  summarize(n=n())

fips_need_change <- fips$`geo_fips`[fips$n<100]
data$geo_fips[data$geo_fips %in% fips_need_change] <- "OTHERS"

#turn those factor variables into factor type
data[,factor_variable_list] <- lapply(data[,factor_variable_list],factor)


pairs(data[numeric_variable_list])

data[-factor_variable_list]

#Run stepwise simple regression on these variables and test the MSE
set.seed(1)
temp_potential_predictor <- variables_list
best_model_overall <-"sale_price ~ 1"
model_constant <- glm(formula(best_model_overall),data=data)
best_model_mse <- cv.glm(data=data,model_constant,K=5)$delta[1]
a <-"sale_price ~ 1"
mark <- 0
while (length(temp_potential_predictor)!=0){
  mse <- 99999999999999999999999999
  for (j in (1:length(temp_potential_predictor))){
    a <- best_model_overall
    a <- paste(a,"+",temp_potential_predictor[j])
    temp_model <- glm(formula(a),data=data)
    temp_mse <- cv.glm(data,temp_model,K=5)$delta[1]
    if (temp_mse<mse){
      best_model <- a
      mse <- temp_mse
      mark <- j
    }
  }
  temp_potential_predictor <- temp_potential_predictor[-mark]
  print("The best model of this step is ")
  print(best_model)
  
  if (mse<best_model_mse){
    best_model_overall <- best_model
    best_model_mse <- mse
  }
}

print("The overall best model is ")
print(best_model_overall)

lm_model <- temp_model
lm_mse <- temp_mse

#Run lasso regression model in the variables list
formula1 <- "sale_price ~ 1+"    

formula1 <- paste(formula1,paste(variables_list,collapse=" + "))
formula1 <- formula(formula1)

set.seed(1)
n_rows <- nrow(data)
train <- sample(n_rows, n_rows*4/5)
train_data <- data[train,]
test_data <- data[-train,]
x <- model.matrix(formula1,data=train_data)
y <- train_data$sale_price

lasso_model <- cv.glmnet(x=x, y=y)
coef(lasso_model, s = "lambda.min")

newx <- model.matrix(formula1,data=test_data)
lasso_y_hat <- predict(lasso_model, newx = newx, s = "lambda.min")
lasso_mse <- mean((test_data$sale_price - lasso_y_hat)^2)

#Run Random Forest Model in the variables list
train_data$geo_property_city <- train_data$geo_property_city %>% 
  fct_lump_n(52)

train_data$geo_property_zip <- train_data$geo_property_zip %>% 
  fct_lump_n(52)

train_data$geo_fips <- train_data$geo_fips %>% 
  fct_lump_n(52)
set.seed(1)
rf_model <- randomForest(formula1, data = train_data, mtry=7, importance=TRUE,ntree=300)
rf_y_hat <- predict(rf_model,newdata = test_data)
rf_mse <- mean((test_data$sale_price - rf_y_hat)^2)

#Run Boosting in the variables list
set.seed(1)
boosting_model <- gbm(formula1,data=train_data,distribution = "gaussian", n.trees = 5000, interaction.depth = 5)
boosting_y_hat <- predict(boosting_model,newdata=test_data)
boosting_mse <- mean((test_data$sale_price - boosting_y_hat)^2)

