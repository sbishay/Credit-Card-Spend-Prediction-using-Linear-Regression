########## Linear Regression Case Study #########

rm(list = ls())

setwd("C:\\Users\\subha\\Desktop\\DA\\R\\Linear Case Study")


# Importing the data file

library(readxl)

LR = read_excel(choose.files(), sheet = 1)
View(LR)

# Understanding the data

str(LR)
dim(LR)
summary(LR)


# Exploratory Analysis:

#Distribution of spends
par("mar")
par(mar=c(1,1,1,1))
hist(LR$cardspent, col = "skyblue")
hist(LR$card2spent, col = "skyblue")


#Creating a new Derived Variable "TotalCreditSpend" which is a summation of primary and secondary spend
#Droping "cardspent"(Primary card), "card2spend"(Secondary card) and carditems,card2items as they are depend on spending

LR$total_spend = LR$cardspent + LR$card2spent

#dropping the redudant variables

LR1 = subset(LR,select = -c(cardspent,card2spent,custid,birthmonth,carditems,card2items))

#Checking age distribution

hist(LR$age, col = "skyblue")

###Segregating Categorical and Numerical Data:
#Splitting Categorical Data with Continuos Data

var_num= subset(LR1,select = -c(region,townsize,gender,agecat,edcat,jobcat,union,
                                empcat,retire,inccat,default,jobsat,marital,spousedcat,
                                homeown,hometype,address,addresscat,cars,carown,cartype,
                                carcatvalue,carbought,carbuy,commute,commutecat,commutecar,
                                commutemotorcycle,commutecarpool,commutebus,commuterail,
                                commutepublic,commutebike,commutewalk,commutenonmotor,
                                telecommute,reason,polview,polparty,polcontrib,vote,card,
                                cardtype,cardbenefit,cardfee,cardtenure,cardtenurecat,card2,
                                card2type,card2benefit,card2fee,card2tenure,card2tenurecat,
                                active,bfast,churn,tollfree,equip,callcard,wireless,multline,
                                voice,pager,internet,callid,callwait,forward,confer,ebill,
                                owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,owngame,ownfax,
                                news,response_01,response_02,response_03))


var_num= as.data.frame(lapply(var_num,as.numeric))
str(var_num)

var_cat= subset(LR1,select = c(region,townsize,gender,agecat,edcat,jobcat,union,
                               empcat,retire,inccat,default,jobsat,marital,spousedcat,
                               homeown,hometype,address,addresscat,cars,carown,cartype,
                               carcatvalue,carbought,carbuy,commute,commutecat,commutecar,
                               commutemotorcycle,commutecarpool,commutebus,commuterail,
                               commutepublic,commutebike,commutewalk,commutenonmotor,
                               telecommute,reason,polview,polparty,polcontrib,vote,card,
                               cardtype,cardbenefit,cardfee,cardtenure,cardtenurecat,card2,
                               card2type,card2benefit,card2fee,card2tenure,card2tenurecat,
                               active,bfast,churn,tollfree,equip,callcard,wireless,multline,
                               voice,pager,internet,callid,callwait,forward,confer,ebill,
                               owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,owngame,ownfax,
                               news,response_01,response_02,response_03))

var_cat= as.data.frame(lapply(var_cat,as.factor))
str(var_cat)

lr2= cbind.data.frame(var_num,var_cat)
summary(lr2)

num_var= sapply(lr2,is.numeric)
Other_var= !sapply(lr2,is.numeric)

# Descriptive statistics
#For Numeric

mystats_num = function(x) {
  nmiss=sum(is.na(x))
  c = class(x)
  a = x[!is.na(x)]
  m = mean(a,na.rm = T)
  med=median(a,na.rm = T)
  n = length(a)
  s = sd(a,na.rm = T)
  min = min(a,na.rm = T)
  q1=quantile(a,0.25,na.rm = T)
  q2=quantile(a,0.5,na.rm = T)
  q3=quantile(a,0.75,na.rm = T)
  p99=quantile(a,0.99,na.rm = T)
  max = max(a,na.rm = T)
  UC = m+3*s
  LC = m-3*s
  outlier_flag= max>1.5*(p99)
  return(c(class=c,n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m,median=med, stdev=s,min = min,
           q1=q1,q2=q2,q3=q3,p99=p99,max=max, UC=UC, LC=LC ))
}

#For Categorical
mystats_cat=function(x){
  Var_Type=class(x)
  n=length(x)
  nmiss=sum(is.na(x))
  return( c(Var_Type=Var_Type, n=n,nmiss=nmiss))
}




diag_stats=  t(data.frame(apply(lr2[,num_var],2,FUN = mystats_num)))
write.csv(diag_stats,"DiagStat.csv")
#View(diag_stats)

diag_stats2=  t(data.frame(apply(lr2[,Other_var],2,FUN = mystats_cat)))
write.csv(diag_stats2,"DiagStat2.csv")
#View(diag_stats2)



#####Data Preparation

#Missing Values

miss_num= as.data.frame(sort(apply(lr2[,num_var], 2,function(col)sum(is.na(col))/length(col)*100)),descending=TRUE)
miss_num

miss_cat= as.data.frame(sort(apply(lr2[,Other_var], 2,function(col)sum(is.na(col))/length(col)*100)),descending=TRUE)
miss_cat

# Removing the Numerical variables which tend to have higher percentage of missing values
lr2$lntollmon=NULL
lr2$lntollten=NULL
lr2$lnequipmon=NULL
lr2$lnequipten=NULL
lr2$lncardmon=NULL
lr2$lncardten=NULL
lr2$lnwiremon=NULL
lr2$lnwireten=NULL

summary(lr2)

num_var1= sapply(lr2,is.numeric)
Other_var1= !sapply(lr2,is.numeric)

### Missing Value Treatment
#Mean value treatment for Numerical variable

lr2[,num_var1] = apply(lr2[,num_var1], 2, function(x){x = replace(x, is.na(x), mean(x, na.rm=TRUE))})

#Value with maximum frequency treatment
lr2[,Other_var1] = apply(lr2[,Other_var1], 2, function(x){x = replace(x, is.na(x), which.max(prop.table(table(x))))})

# Outlier capping with p95 and p5

outlier_treat = function(x){
  UC1 = quantile(x, p=0.95,na.rm=T)
  LC1 = quantile(x, p=0.05,na.rm=T)
  x[x>UC1]=UC1
  x[x<LC1]=LC1
  
  return(x)
}

lr2[,num_var1] = data.frame(apply(lr2[,num_var1], 2, FUN=outlier_treat))

# Distribution of the Dependent Variable

hist(lr2$total_spend, col = "skyblue")

lr2$ln_total_spend=log(lr2$total_spend)
hist(lr2$ln_total_spend, col= "skyblue")

#lr2$total_spend=NULL

num_var2= sapply(lr2,is.numeric)
Other_var2= !sapply(lr2,is.numeric)

corrm= cor(lr2[,num_var2])
View(corrm)

write.csv(corrm, file = "corrm1.csv")


# Categorical Variable Reduction using ANOVA
# 95% confidence level

#performing the anova test and selecting variables with p value < 0.05 (5%)

fit=aov(ln_total_spend~ region+townsize+gender+agecat+edcat+jobcat+union+empcat+retire+inccat+default+jobsat+marital+spousedcat+homeown+hometype+address+addresscat+cars+carown+cartype+carcatvalue+carbought+carbuy+commute+commutecat+commutecar+commutemotorcycle+commutecarpool+commutebus+commuterail+commutepublic+commutebike+commutewalk+commutenonmotor+telecommute+reason+polview+polparty+polcontrib+vote+card+cardtype+cardbenefit+cardfee+cardtenure+cardtenurecat+card2+card2type+card2benefit+card2fee+card2tenure+card2tenurecat+active+bfast+churn+tollfree+equip+callcard+wireless+multline+voice+pager+internet+callid+callwait+forward+confer+ebill+owntv+ownvcr+owndvd+owncd+ownpda+ownpc+ownipod+owngame+ownfax+news+response_01+response_02+response_03,
        data = lr2) 

summary(fit)

library(MASS)
step= stepAIC(fit,direction = "both")
ls(step)
step$anova

# The significant categorical variables we got after performing ANOVA test and stepwise regresion:
#region,gender,edcat,retire,inccat,default,carown,reason,card,card2,pager,internet,ownvcr,owndvd, response_03,cardtenurecat

fin_cat= subset(lr2,select = c(region,gender,edcat,retire,inccat,default,carown,reason ,card,card2,pager,internet,ownvcr,owndvd,response_03,cardtenurecat))

# #Final Model:
# ln_total_spend ~ region + gender + edcat + retire + inccat + 
#   default + carown + reason + card + card2 + pager + internet + 
#   ownvcr + owndvd + response_03 + cardtenurecat


# Numerical Variable Reduction using ANOVA

z= subset(lr2,select = -c(region,townsize,gender,agecat,edcat,jobcat,union,
                          empcat,retire,inccat,default,jobsat,marital,spousedcat,
                          homeown,hometype,address,addresscat,cars,carown,cartype,
                          carcatvalue,carbought,carbuy,commute,commutecat,commutecar,
                          commutemotorcycle,commutecarpool,commutebus,commuterail,
                          commutepublic,commutebike,commutewalk,commutenonmotor,
                          telecommute,reason,polview,polparty,polcontrib,vote,card,
                          cardtype,cardbenefit,cardfee,cardtenure,cardtenurecat,card2,
                          card2type,card2benefit,card2fee,card2tenure,card2tenurecat,
                          active,bfast,churn,tollfree,equip,callcard,wireless,multline,
                          voice,pager,internet,callid,callwait,forward,confer,ebill,
                          owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,owngame,ownfax,
                          news,response_01,response_02,response_03))
zz=lm(ln_total_spend~. ,data = z)
summary(zz)

step= stepAIC(zz,direction = "both")
ls(step)
step$anova

# Final Model:
#   ln_total_spend ~ age + lninc + debtinc + lncreddebt + othdebt + 
#   pets_birds + carvalue + cardmon + cardten

fin_num= subset(lr2,select = c(ln_total_spend, age,lninc,debtinc,lncreddebt, othdebt,pets_birds,carvalue,cardmon,cardten))


#Converting categorical variable into factor

fin_cat= as.data.frame(lapply(fin_cat,as.factor))


#Exporting final cleaned dataset(ready for modelling)
finaldata= cbind.data.frame(fin_num,fin_cat,total_spend=lr2$total_spend)
write.csv(finaldata, file="finaldata.csv")


#  Final Model Building
# _ _ _ _ _ _ _ _ _ _ _ _ _ _
#  Splitting the dataset into "Developement" and "validation" datasets

set.seed(222)
ind= sample(2,nrow(finaldata),replace = TRUE,prob = c(0.7,0.3))
dev= finaldata[ind==1,]
val= finaldata[ind==2,]

#  Building model on dev dataset

fit2= lm(ln_total_spend ~ .,data = dev)
summary(fit2)

library(car)
vif(fit2)


library(MASS)
step= stepAIC(fit2,direction="both")
ls(step)
step$anova

fit3= lm(ln_total_spend ~ lninc + debtinc + lncreddebt + othdebt + carvalue + 
           cardmon + cardten + gender + retire + default + reason + 
           card + card2 + ownvcr + cardtenurecat,
         data = dev)

summary(fit3)

final_model2= lm(ln_total_spend ~ lninc + debtinc+lncreddebt+othdebt+cardmon+cardten+gender + retire + card + card2,data = dev2)
summary(final_model2)
######################################################################################

#  Predicting the Total spend in dev and val datasets:
#     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

pred1= exp(predict(final_model2,newdata = dev))
dev= cbind(dev,pred_spend=pred1)

pred2= exp(predict(final_model2,newdata = val))
val= cbind(val,pred_spend=pred2)

#  Decile Analysis:
#  _ _ _ _ _ _ _ _ _ _ _ 

dec_loc= quantile(dev$pred_spend,probs = seq(0.1,0.9,by=0.1))
dev$decile= findInterval(dev$pred_spend,c(-Inf,dec_loc,+Inf))

library(sqldf)

dev_decile= sqldf("select decile,count(decile) as Count,
                   avg(total_spend) as Actual_spend,
                   avg(pred_spend) as Predicted_spend
                   from dev
                   group by decile
                   order by decile desc")
write.csv(dev_decile,"dev_decile.csv")

dec_loc= quantile(val$pred_spend,probs = seq(0.1,0.9,by=0.1))
val$decile= findInterval(val$pred_spend,c(-Inf,dec_loc,Inf))

val_decile= sqldf("select decile,count(decile) as Count,
                   avg(total_spend) as Actual_spend,
                   avg(pred_spend) as Predicted_spend
                   from val
                   group by decile
                   order by decile desc")
write.csv(val_decile,"val_decile.csv")

library(caret)
fn=as.data.frame(varImp(final_model))
write.csv(fn,"var_imp.csv")
