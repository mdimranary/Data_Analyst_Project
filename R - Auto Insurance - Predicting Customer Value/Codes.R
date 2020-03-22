library(boot) 	
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)



setwd("C:\\Users\\Imran\\Desktop\\Ivy classes\\R\\Project")

data <- read.csv("Fn-UseC_-Marketing-Customer-Value-Analysis.csv")
str(data)
summary(data)


data$Number.of.Open.Complaints  <- as.factor(data$Number.of.Open.Complaints)

data$Number.of.Policies  <- as.factor(data$Number.of.Policies)



data1 <- data[,-(1)]
data <- data1
boxplot(data2$Customer.Lifetime.Value)
boxplot(data3$Income)

head(data)

quantile(data$Customer.Lifetime.Value, c(0,0.05,0.10,0.25,0.5,0.75,0.91,0.93,0.98,0.995,0.997,1))

quantile(data$Income, c(0,0.05,0.10,0.25,0.5,0.75,0.91,0.93,0.98,0.995,0.997,1))

data3 <- data[data$Income > 0,]

nrow(data3)

data2 <- data[data$Customer.Lifetime.Value  < 17000,]

nrow(data)- nrow(data2)

data <- data2

boxplot(data$Income)

sapply(data, function(x) sum(is.na(x)))


fit <- lm(Customer.Lifetime.Value  ~ State + Response + Coverage  + Education  
+ Effective.To.Date  + EmploymentStatus + Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto + Months.Since.Last.Claim + Months.Since.Policy.Inception +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type + Policy
+ Renew.Offer.Type + Sales.Channel + Total.Claim.Amount + Vehicle.Class + Vehicle.Size , data = data)

summary(fit)


fit <- lm(Customer.Lifetime.Value  ~ State + Response + Coverage  + Education  
+ Effective.To.Date  + EmploymentStatus + Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto + Months.Since.Last.Claim + Months.Since.Policy.Inception +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type
+ Renew.Offer.Type + Sales.Channel + Total.Claim.Amount + Vehicle.Class + Vehicle.Size , data = data)

summary(fit)

fit <- lm(Customer.Lifetime.Value  ~ State + Response + Coverage  + Education  
+ I(Effective.To.Date =="Effective.To.Date02 December 2011")+I(Effective.To.Date =="Effective.To.Date02 September 2011") + I(Effective.To.Date =="Effective.To.Date1/19/11 ") + I(Effective.To.Date =="Effective.To.Date1/27/11") + EmploymentStatus + Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto + Months.Since.Last.Claim + Months.Since.Policy.Inception +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type
+ Renew.Offer.Type + Sales.Channel + Total.Claim.Amount + Vehicle.Class + Vehicle.Size , data = data)

summary(fit)


fit <- lm(Customer.Lifetime.Value  ~ State + Response + Coverage  + Education  
 + EmploymentStatus + Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto + Months.Since.Last.Claim + Months.Since.Policy.Inception +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type
+ Renew.Offer.Type + Sales.Channel + Total.Claim.Amount + Vehicle.Class + Vehicle.Size , data = data)

summary(fit)

fit <- lm(Customer.Lifetime.Value  ~ Response + Coverage  + Education  
 + EmploymentStatus + Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto + Months.Since.Last.Claim + Months.Since.Policy.Inception +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type
+ Renew.Offer.Type + Sales.Channel + Total.Claim.Amount + Vehicle.Class + Vehicle.Size , data = data)

summary(fit)


fit <- lm(Customer.Lifetime.Value  ~ Response + Coverage  + Education  
 + EmploymentStatus + Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto + Months.Since.Last.Claim + Months.Since.Policy.Inception +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type
+ Renew.Offer.Type + Sales.Channel + Total.Claim.Amount + Vehicle.Class + Vehicle.Size , data = data)

summary(fit)

fit <- lm(Customer.Lifetime.Value  ~ Coverage  + Education  
 + EmploymentStatus + Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto + Months.Since.Last.Claim + Months.Since.Policy.Inception +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type
+ Renew.Offer.Type + Sales.Channel + Total.Claim.Amount + Vehicle.Class + Vehicle.Size , data = data)

summary(fit)


fit <- lm(Customer.Lifetime.Value  ~ Coverage  + I(Education == "EducationDoctor")+  I(Education == "EducationMaster")+
 + EmploymentStatus + Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto + Months.Since.Last.Claim + Months.Since.Policy.Inception +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type
+ Renew.Offer.Type + Sales.Channel + Total.Claim.Amount + Vehicle.Class + Vehicle.Size , data = data)

summary(fit)


fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
 + EmploymentStatus + Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto + Months.Since.Last.Claim + Months.Since.Policy.Inception +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type
+ Renew.Offer.Type + Sales.Channel + Total.Claim.Amount + Vehicle.Class + Vehicle.Size , data = data)

summary(fit)

fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
  I(EmploymentStatus =="EmploymentStatusEmployed ") + Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto + Months.Since.Last.Claim + Months.Since.Policy.Inception +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type
+ Renew.Offer.Type + Sales.Channel + Total.Claim.Amount + Vehicle.Class + Vehicle.Size , data = data)

summary(fit)

fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
   Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto + Months.Since.Last.Claim + Months.Since.Policy.Inception +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type
+ Renew.Offer.Type + Sales.Channel + Total.Claim.Amount + Vehicle.Class + Vehicle.Size , data = data)

summary(fit)

fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
   Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto + Months.Since.Last.Claim + Months.Since.Policy.Inception +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type
+ Renew.Offer.Type + Sales.Channel + Total.Claim.Amount + Vehicle.Class , data = data)

summary(fit)

fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
   Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto + Months.Since.Last.Claim + Months.Since.Policy.Inception +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type
+ Renew.Offer.Type + Total.Claim.Amount + Vehicle.Class , data = data)

summary(fit)

fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
   Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto + Months.Since.Policy.Inception +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type
+ Renew.Offer.Type + Total.Claim.Amount + Vehicle.Class , data = data)

summary(fit)

fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
   Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto +
Number.of.Open.Complaints + Number.of.Policies + Policy.Type
+ Renew.Offer.Type + Total.Claim.Amount + Vehicle.Class , data = data)

summary(fit)


fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
   Gender + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto +
Number.of.Open.Complaints + Number.of.Policies
+ I(Renew.Offer.Type=="Renew.Offer.TypeOffer2") + Total.Claim.Amount + Vehicle.Class , data = data)

summary(fit)

fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
   I(Gender=="Marital.StatusSingle") + Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto +
Number.of.Open.Complaints + Number.of.Policies
 + Total.Claim.Amount + Vehicle.Class , data = data)

summary(fit)


fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
   Income + Location.Code + 
Marital.Status + Monthly.Premium.Auto +
Number.of.Open.Complaints + Number.of.Policies
 + Total.Claim.Amount + Vehicle.Class , data = data)

summary(fit)

fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
   Income + I(Location.Code=="Location.CodeUrban") + 
Marital.Status + Monthly.Premium.Auto +
Number.of.Open.Complaints + Number.of.Policies
 + Total.Claim.Amount + Vehicle.Class , data = data)

summary(fit)

fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
   Income + I(Marital.Status =="Marital.StatusSingle") + Monthly.Premium.Auto +
Number.of.Open.Complaints + Number.of.Policies
 + Total.Claim.Amount + Vehicle.Class , data = data)

summary(fit)

fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
   Income  + Monthly.Premium.Auto +
I(Number.of.Open.Complaints=="Number.of.Open.Complaints2") +I(Number.of.Open.Complaints=="Number.of.Open.Complaints3")+I(Number.of.Open.Complaints=="Number.of.Open.Complaints4")+I(Number.of.Open.Complaints=="Number.of.Open.Complaints5") + Number.of.Policies
 + Total.Claim.Amount + Vehicle.Class , data = data)

summary(fit)


fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
   Income  + Monthly.Premium.Auto +
  Number.of.Policies
 + Total.Claim.Amount + Vehicle.Class , data = data)

summary(fit)


vif(fit)

fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
   Income  +Number.of.Policies
 + Total.Claim.Amount + Vehicle.Class , data = data)

summary(fit)

fit <- lm(Customer.Lifetime.Value  ~ Coverage  +
   Income  +Number.of.Policies
 + Vehicle.Class , data = data)

summary(fit)

fitted(fit)

data$pred <- fitted(fit)
head(data)

attach(data)
(sum((abs(Customer.Lifetime.Value-pred))/Customer.Lifetime.Value))/nrow(data)

Mape  - 0.13

r2 <- .875

resids <- fit$residuals


dwt(fit)
 0.55 <- p value is more than .05 so there is no serial co relation validated

bptest(fit)
p value is less than .05 then it is heteroscedasticity violated

resids <- fit$residuals
ad.test(resids)
pvalue is less than .05 so the data is not normally distrubeted assumption is violated


data$resids <- fit$residuals

write.csv(data,"pred.csv")





















































































































































































