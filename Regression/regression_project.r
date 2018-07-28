#Importing the libraries
library('dplyr')
library('tidyr')
library('openxlsx')
dataset=read.xlsx('13 Model Data on Revenue.xlsx')
str(dataset)
statistic=summary(dataset)
write.csv(statistic,'summary.csv')
# To find negative value for orders which is not valid input. 
select(dataset,Revenue,Orders,Sales)%>%filter(Orders<0)
select(dataset,Revenue,Orders,Sales)%>%filter(Orders==0)
# Orders ccant be negative, so replacing invalid value to sales value
dataset$Orders=ifelse(dataset$Orders<=0,dataset$Sales,dataset$Orders)
# To check is theres any invalid values in Orders
nrow(filter(dataset,Orders<0))
# To check presence of invalid in sales
nrow(select(dataset,Revenue,Orders,Sales)%>%filter(Sales<0))
select(dataset,Revenue,Orders,Sales)%>%filter(Sales<0 & Orders<0)
# Replacing the negative value of both sales and Orders in same rows with zero
dataset$Orders=ifelse((dataset$Orders<0 & dataset$Sales<0),0,dataset$Orders)
dataset$Sales=ifelse((dataset$Orders==0 & dataset$Sales<0),0,dataset$Sales)
(select(dataset,Revenue,Orders,Sales)%>%filter(Sales<0))
# if the difference between Orders and Sales is negative then it is invalid, always sales should be less tahn Orders
# So replace Sales with orders,
nrow(select(dataset,Revenue,Orders,Sales)%>%filter(dataset$Orders-dataset$Sales<0))
dataset$Sales=ifelse((dataset$Orders-dataset$Sales)<0,dataset$Orders,dataset$Sales)
nrow(select(dataset,Revenue,Orders,Sales)%>%filter(dataset$Orders-dataset$Sales<0))
#summary statistic after handling the invalid values
statistic_Valid=summary(dataset)
write.csv(statistic_Valid,'summary_valid.csv')

#To identify the outliers
new1=dataset%>%group_by(Customer,Product.Name)%>%summarise(Q1=quantile(Sales,probs=0.05),Q2=quantile(Sales,probs=0.10),Q3=quantile(Sales,probs=0.25),Q4=quantile(Sales,probs=0.50),Q5=quantile(Sales,probs=0.75),Q6=quantile(Sales,probs=0.90),Q7=quantile(Sales,probs=0.95),Q8=quantile(Sales,probs=0.99))%>%group_by(Customer)%>%summarise(sum_of_Q1=sum(Q1),sum_of_Q2=sum(Q2),sum_of_Q3=sum(Q3),sum_of_Q4=sum(Q4),sum_of_Q5=sum(Q5),sum_of_Q6=sum(Q6),sum_of_Q7=sum(Q7),sum_of_Q8=sum(Q8))
new2=dataset%>%group_by(Customer,Product.Name)%>%summarise(Q1=quantile(Orders,probs=0.05),Q2=quantile(Orders,probs=0.10),Q3=quantile(Orders,probs=0.25),Q4=quantile(Orders,probs=0.50),Q5=quantile(Orders,probs=0.75),Q6=quantile(Orders,probs=0.90),Q7=quantile(Orders,probs=0.95),Q8=quantile(Orders,probs=0.99))
write.csv(new1,'quantile_sales.csv')
write.csv(new2,'quantile_orders.csv')
filter(new1,Customer=='Business')
filter(new1,Customer=='Government')
filter(new1,Customer=='Other')
filter(new1,Customer=='Small Business')
table(new1$Customer)
filter(dataset,(Customer=='Business' & Product.Name=='Other'))
class(dataset$Year)

#Derived variable: difference
dataset$difference=dataset$Orders-dataset$Sales

#to include seasonality including previous year sales data.
#To create previous year sales, derive quarter variable from Time
dataset$Year=as.numeric(as.character(dataset$Year))
#incrementing the year by one when merging two table based on year, 
#the incremented year sales value ie 2015 actually that is 2014 sales,
#2015 sales is merged with 2014 sales ie previous sales
dataset$year=dataset$Year+1
dataset$Quarter=(strsplit(dataset$Time,"Q"))%>%sapply(tail,1)
dataset$Quarter=as.numeric(as.character(dataset$Quarter))
names(dataset)
dataset1=dataset[-2]
dataset=dataset[-10]
names(dataset)
names(dataset1)
dataset_PYD=inner_join(dataset,dataset1,by=c("Year"="year","Quarter","Customer","Product.Name","Location"))
summary(dataset_PYD)
names(dataset_PYD)
dataset_PYD=dataset_PYD[,c(-6,-11,-12,-14)]
names(dataset_PYD)
old_names=c("Time.x","Sales.x","Orders.x","difference.x","Sales.y","difference.y")
new_names=c("Time","Sales","Orders","Difference","Prev_year_Sales","Prev_year_difference")
dataset_PYD=dataset_PYD%>%rename_at(vars(old_names),~new_names)
summary_derived_var=summary(dataset_PYD)
write.csv(summary_derived_var,'summary_derived_var.csv')
#previous quarter sales, because sales depends on recent previous values.
names(dataset1)
dataset_PYD$quarter=dataset_PYD$Quarter+1
dataset_PYD$year=ifelse(((dataset_PYD$quarter>4)),dataset_PYD$Year+1,dataset_PYD$Year)
#replace(dataset1$year,dataset1$quarter>4,dataset1$year+1)
dataset_PYD$quarter=ifelse(dataset_PYD$quarter>4,1,dataset_PYD$quarter)
names(dataset)
names(dataset_PYD)
dataset_PYD1=dataset_PYD[,c(-2,-9)]
dataset_PYD=dataset_PYD[c(-12,-13)]
names(dataset_PYD)
names(dataset_PYD1)
dataset_pQD=left_join(dataset_PYD,dataset_PYD1,by=c("Year"="year","Quarter"="quarter","Customer","Product.Name","Location"))
names(dataset_pQD)
dataset_pQD=dataset_pQD[,c(-12,-14,-16,-17)]
names(dataset_pQD)
old_names=c("Time.x","Sales.x","Orders.x","Sales.y","Difference.y")
new_names=c("Time","Sales","Orders",'prev_quart_sales',"prev_quart_diff")
dataset_pQD=dataset_pQD%>%rename_at(vars(old_names),~new_names)
names(dataset_pQD)

#derived varable : prev year sales average
colSums(is.na(dataset_pQD))
new=summarise_at(group_by(dataset,Year,Customer,Product.Name),vars(Sales,difference),funs(mean),na.rm=TRUE)
names(new)
dataset_pQD1=left_join(dataset_pQD,new,by=c("Year","Customer","Product.Name"))
names(dataset_pQD1)

dataset_pQD1$Year=as.factor(dataset_pQD1$Year)

summary(dataset_pQD1)
old_names=c("Sales.x",'Sales.y',"difference")
new_names=c("Sales","Prev_year_avg_sales","prev_year_avg_diff")
dataset_pQD1=dataset_pQD1%>%rename_at(vars(old_names),~new_names)
dataset_pQD1$prev_quart_sales[is.na(dataset_pQD1$prev_quart_sales)]=dataset_pQD1$Prev_year_avg_sales[is.na(dataset_pQD1$prev_quart_sales)]
dataset_pQD1$prev_quart_diff[is.na(dataset_pQD1$prev_quart_diff)]=dataset_pQD1$prev_year_avg_diff[is.na(dataset_pQD1$prev_quart_diff)]
summary=summary(dataset_pQD1)
sample_pdq=head(dataset_pQD1)
write.csv(sample_pdq,'sample_pdq.csv')
write.csv(summary,'summary_derived.csv')
#Creating dummy variable
for (t in unique(dataset_pQD1$Customer))
  {dataset_pQD1[paste("Customer",t,"")]<-ifelse(dataset_pQD1$Customer==t,1,0)
  
}
for (t in unique(dataset_pQD1$Product.Name)){
  dataset_pQD1[paste("product",t,"_")]<-ifelse(dataset_pQD1$Product.Name==t,1,0)
}

names(dataset_pQD1)

dataset_pQD1=dataset_pQD1[c(-1,-3,-4,-5,-18,-32)]
sample_data=head(dataset_pQD1)
write.csv(sample_data,'Sampledata.csv')
names(dataset_pQD1)
  ncol(dataset_pQD1)
# checking the presence of multicolinearity


#diving the dataset into validation and development sample
X=dataset_pQD1[-4]

#splitting dataset for developing and validating the model
set.seed(0)
d=sort(sample(nrow(X),nrow(X)*.7))
train=X[d,]
test=X[-d,]
Ind_Var=colnames(train)[3:ncol(train)]
final <- matrix(NA,2,5)
for (i in (3:ncol(train))){
  model=lm(Sales~ train[,i], data=train)
  S1<-summary.lm(model)$coefficients
  category <- as.data.frame(rep((Ind_Var)[i-2],nrow(S1)))
  summary <- cbind(category,S1)
  names(summary)[1]=paste("Variable")
  final <- rbind(as.matrix(summary),final)
  
}
write.csv(final,file="Bivariate.csv")


#tO SELECT THOSE SIGNIFICANT VARIABLE IN THe TRAIN DATA SET ACCESSING 
#THE CSV FILE AND USE THE VARIABLE AND P-VALUE COLUMN AND FILTER VARIABLE
#BASED ON P-VALUE SELECT THOSE COLUMNS FROM THE TRAIN DATA FRAME.
bivariate_result=read.csv('Bivariate.csv')
head(bivariate_result)
bivariate_Result=select(bivariate_result,X,Variable,Pr...t..)%>%filter((bivariate_result$X=='train[, i]')& (bivariate_result$Pr...t..<=0.05))
nrow(bivariate_Result)
sig_var=bivariate_Result[2]
sig_var$Variable
names(train)
train1=train[c(-1,-12,-16,-18,-19,-24)]
names(train1)

# ----Correlation Matrix-------

cor_matrix=cor(train1)
write.csv(cor_matrix,"cor.csv")

#-------vif-----
install.packages('car')
library('car')
str(train1)
#step 1 vif with all independnt variables and find vif value for all variable
model_vif=lm(Sales ~ . , data=train1)
s=vif(model_vif)
write.csv(s,'vif1.csv')

# step 2 after eliminating Prev_year_avg_sales
names(train1)
train1=train1[-8]
model_vif=lm(Sales ~ . , data=train1)
s=vif(model_vif)
write.csv(s,'vif2.csv')

# step 3 after eliminating orders
names(train1)
train1=train1[-2]
model_vif=lm(Sales ~ . , data=train1)
s=vif(model_vif)
write.csv(s,'vif3.csv')

# step 4 after eliminating prev_year_sales.x
train1=train1[-3]
names(train1)
model_vif=lm(Sales ~ . , data=train1)
s=vif(model_vif)
write.csv(s,'vif4.csv')

# step 5 after eliminating prev_year_avg_diff
train1=train1[-6]
names(train1)
model_vif=lm(Sales ~ . , data=train1)
s=vif(model_vif)
write.csv(s,'vif5.csv')

# step 6 after eliminating prev_year_difference
train1=train1[-3]
names(train1)
model_vif=lm(Sales ~ . , data=train1)
s=vif(model_vif)
write.csv(s,'vif6.csv')


#step wise selection
install.packages("stats")
library(stats)
model_test=lm(Sales ~ .,data=train1)
model_step=step(model_test)
s=summary(model_step)
write.csv(s$coefficients,file="stepwise.csv")

#Model building and validation
names(train1)
f_train=train1[,c( -6,-7,-(9:14))]
names(f_train)
f_test=select(test, "Quarter","prev_quart_sales","prev_quart_diff","Customer Regular ","product Software _")
names(f_test)
model=lm(Sales ~ . ,f_train)
s=summary(model)
write.csv(s$coefficients,'s.csv')
fit=fitted(model)
out=data.frame(f_train,fit)
write.csv(out,'fitted.csv')
predicted=predict(model,f_test)
out=data.frame(test1,predicted)
write.csv(out,'predicted.csv')


#Developing model on whole data set
names(X)
names(f_train)
x=select(X, "Quarter","prev_quart_sales","prev_quart_diff","Customer Regular ","product Software _", "Sales" )
final_model=lm(Sales ~ .,data=x)
s=summary(final_model)
write.csv(s$coefficients,'final_coefficient.csv')


#score dataset to derived the variable required to predict
score=read.xlsx('13 Score.xlsx')
data_2017=select(dataset,"Year","Customer","Product.Name","Sales",   
                 "Orders","difference","Quarter" )%>%filter(Year==2017)
data_2017$quarter=data_2017$Quarter+1
names(data_2017)
data_2017=data_2017[-7]
score$Quarter=(strsplit(score$Time,"Q"))%>%sapply(tail,1)
score$Year=(strsplit(score$Time,"Q"))%>%sapply(head,1)
names(score)
score=score[c(-1,-2,-5,-6)]
str(score)
str(data_2017)
score$Quarter=as.numeric(score$Quarter)
score$Year=as.numeric(score$Year)
derived_score=left_join(score,data_2017,by=c("Year","Quarter"="quarter","Customer","Product.Name"))
names(derived_score)
old_names=c("Sales","difference")
new_names=c("prev_quart_sales","prev_quart_diff")
derived_score=derived_score%>%rename_at(vars(old_names),~new_names)
names(derived_score)
derived_score=derived_score[-7]

#Removing other other category in both product and customer
#derived_score=filter(derived_score,derived_score$Customer!='Other' & derived_score$Product.Name!='Other')
summary(derived_score)
#g=derived_score%>%summarise_at(c('prev_quart_sales','prev_quart_diff'),mean,na.rm=TRUE)

for (t in unique(derived_score$Customer))
{derived_score[paste("Customer",t,"")]<-ifelse(derived_score$Customer==t,1,0)

}
for (t in unique(derived_score$Product.Name)){
  derived_score[paste("product",t,"_")]<-ifelse(derived_score$Product.Name==t,1,0)
}
names(derived_score)
derived_score1=select(derived_score,"Quarter","prev_quart_sales","prev_quart_diff","Customer Regular ","product Software _","Orders.x")

#dropping order for prediction to match the no of variables of model
derived_score2=derived_score1[-6]
derived_score2_Q1=filter(derived_score2,Quarter==2)
summary(derived_score2_Q1)

#replace Na with mean value
derived_score2_Q1[is.na(derived_score2_Q1[,"prev_quart_sales"]), "prev_quart_sales"] <- mean(derived_score2_Q1[,"prev_quart_sales"], na.rm = TRUE)
derived_score2_Q1[is.na(derived_score2_Q1[,"prev_quart_diff"]), "prev_quart_diff"] <- mean(derived_score2_Q1[,"prev_quart_diff"], na.rm = TRUE)
summary(derived_score2_Q1)
str(derived_score2_Q1)

#predicting 2017 q2 
predicted=predict(final_model,derived_score2_Q1)
out=data.frame(derived_score2_Q1,predicted)
write.csv(out,'predicted_17_Q2.csv')

#-------------preparing data- 2017,Q3-------------
#preparing the data to predict to predict 2017 Q3
names(score)
score_Q2=filter(score,Quarter==2)
score_Q2$Sales=predicted
#replacing invalid sales with order value
nrow(filter(score_Q2,Orders<0))
score_Q2$Orders=ifelse(score_Q2$Orders<0,score_Q2$Sales,score_Q2$Orders)
score_Q2$Sales=ifelse(score_Q2$Sales>score_Q2$Orders,score_Q2$Orders,score_Q2$Sales)
score_Q2$Sales=ifelse(score_Q2$Sales<0,0,score_Q2$Sales)
filter(score_Q2,Sales<0)

score_Q2$difference=score_Q2$Orders-score_Q2$Sales
filter(score_Q2,difference<0)

score_Q2$quarter=score_Q2$Quarter+1
score_Q3=filter(score,Quarter==3)
derv_score_Q3=left_join(score_Q3,score_Q2,by=c("Year","Quarter"="quarter","Customer","Product.Name"))
names(derv_score_Q3)
old_names=c("Sales","difference")
new_names=c("prev_quart_sales","prev_quart_diff")
derv_score_Q3=derv_score_Q3%>%rename_at(vars(old_names),~new_names)
derv_score_Q3=derv_score_Q3[c(-6,-7,-5)]
for (t in unique(derv_score_Q3$Customer))
{derv_score_Q3[paste("Customer",t,"")]<-ifelse(derv_score_Q3$Customer==t,1,0)

}
for (t in unique(derv_score_Q3$Product.Name)){
  derv_score_Q3[paste("product",t,"_")]<-ifelse(derv_score_Q3$Product.Name==t,1,0)
}
names(derv_score_Q3)
derv_score_Q3_final=select(derv_score_Q3,"Quarter","prev_quart_sales","prev_quart_diff","Customer Regular ","product Software _")
summary(derv_score_Q3_final)

#predicting the 2017 quarter 3
predicted=predict(final_model,derv_score_Q3_final)
out=data.frame(derv_score_Q3_final,predicted)
write.csv(out,'predicted_17_Q3.csv')
