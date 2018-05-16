###################################################################################
# Libraries
###################################################################################
require(corrplot)
require(glmnet)
require(MASS)
require(qcc)
require(aod)
require(plotly)
require(ResourceSelection)

###################################################################################
#Import Data
###################################################################################

myopia <- read.csv("C:/Users/georg/Dropbox/Business Analytics/Winter-Quarter/Statistics_2/Assignment_1/myopia.csv", sep=";")

###########################
#Delete Columns ID
###########################

myopia$ID<-NULL


###########################
#cREATE A NEW VARIABLE
###########################

#Mum Myopic
table1<-table(myopia$MYOPIC, myopia$MOMMY)
prop_table<-prop.table(table1,2)
table_Mummy<-round(prop_table,2)
row.names(table_Mummy)<-c("Myopic","No-Myopic")
colnames(table_Mummy)<-c("No","Yes")

#Dad Myopic
table1<-table(myopia$MYOPIC, myopia$DADMY)
prop_table<-prop.table(table1,2)
table_Dadmy<-round(prop_table,2)
row.names(table_Dadmy)<-c("Myopic","No-Myopic")
colnames(table_Dadmy)<-c("No","Yes")


#Comment: We observe that if your parents are myopic then you have VERY High changes to be myopic.
#So, we create one variable for both parents

#Create one variable for both parents
myopia$PARENTMY<-rep(0,nrow(myopia))
for(i in 1:nrow(myopia))
{
  if (myopia$DADMY[i]==1 | myopia$MOMMY[i]==1) myopia$PARENTMY[i]<-1
}

#Now we don't want the MOM & DAD variables anymore
#If we keep them with PARENTMY we will have high multicollinearity
myopia$MOMMY<-NULL
myopia$DADMY<-NULL

###################################################################################
#Numerical variables
###################################################################################

myopia<-transform(myopia, STUDYYEAR = as.numeric(STUDYYEAR))
myopia<-transform(myopia, AGE = as.numeric(AGE))
myopia<-transform(myopia, SPORTHR = as.numeric(SPORTHR))
myopia<-transform(myopia, READHR = as.numeric(READHR))
myopia<-transform(myopia, COMPHR = as.numeric(COMPHR))
myopia<-transform(myopia, STUDYHR = as.numeric(STUDYHR))
myopia<-transform(myopia, TVHR = as.numeric(TVHR))

###################################################################################
#Factor variables
###################################################################################

myopia<-transform(myopia, GENDER = factor(GENDER))
myopia<-transform(myopia, PARENTMY = factor(PARENTMY))
myopia<-transform(myopia, MYOPIC = factor(MYOPIC))

#################
#STRUCTURE
#################

str(myopia)
summary(myopia)

sum(myopia$MYOPIC==1)
mean(myopia[(myopia$MYOPIC==1),]$SPHEQ)
min(myopia[(myopia$MYOPIC==1),]$SPHEQ)

###################################################################################
#Create numeric variable dataset
###################################################################################

index <- sapply(myopia, class) == "numeric"
myopianum <- myopia[,index]

###################################################################################
#Create factor variable dataset
###################################################################################

index <- sapply(myopia, class) == "factor"
myopiafact <- myopia[,index]

###################################################################################
#Visual Analysis for numerical variables
###################################################################################

par(mfrow=c(4,3))
for (i in 1:nrow(myopianum)){
  hist(myopianum[,i],col="salmon", main=names(myopianum)[i])
}

###################################################################################
#Visual Analysis for factors
###################################################################################

#Genre Table
#Count the number of the men and woman
sum(myopia$GENDER==1) #302
sum(myopia$GENDER==0) #316


table1<-table(myopia$MYOPIC, myopia$GENDER)
prop_table<-prop.table(table1,2)
table_genre<-round(prop_table,2)
row.names(table_genre)<-c("Myopic","No-Myopic")
colnames(table_genre)<-c("Man","Woman")

#X-square test
prop.test(table1,2) #p>0.05, so no significant difference


#Parents Myopic
table1<-table(myopia$MYOPIC, myopia$PARENTMY)
prop_table<-prop.table(table1,1)
table_Parents<-round(prop_table,2)
row.names(table_Parents)<-c("Myopic","No-Myopic")
colnames(table_Parents)<-c("No","Yes")

#With Plotly
p1<-plot_ly(x=~c("Males","Females"),y = ~table_genre[1,]*100, type = 'bar', name = 'Gender')%>% layout(yaxis =list(title="Percentage of Myopic(%)"))
p2<-plot_ly(x=~c("No Myopic Parents","At least one Myopic Parent"),y = ~table_Parents[1,]*100, type = 'bar', name = 'Parents')
subplot(p1, p2, shareY = TRUE)


###################################################################################
#Pairs of numerical variables
###################################################################################

cor(myopianum)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(myopianum), method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)



###################################################################################
#MYOPIA on each numerical variable
###################################################################################

p1<-plot_ly(x=myopia$MYOPIC, y = ~myopianum[,1], color = ~cut, type = "box",name = names(myopianum)[1])%>% layout(xaxis =list(title=names(myopia)[1]))
p2<-plot_ly(x=myopia$MYOPIC, y = ~myopianum[,2], color = ~cut, type = "box",name = names(myopianum)[2])%>% layout(xaxis =list(title=names(myopia)[1]))
p3<-plot_ly(x=myopia$MYOPIC, y = ~myopianum[,3], color = ~cut, type = "box",name = names(myopianum)[3])%>% layout(xaxis =list(title=names(myopia)[1]))
p4<-plot_ly(x=myopia$MYOPIC, y = ~myopianum[,4], color = ~cut, type = "box",name = names(myopianum)[4])%>% layout(xaxis =list(title=names(myopia)[1]))
p5<-plot_ly(x=myopia$MYOPIC, y = ~myopianum[,5], color = ~cut, type = "box",name = names(myopianum)[5])%>% layout(xaxis =list(title=names(myopia)[1]))
p6<-plot_ly(x=myopia$MYOPIC, y = ~myopianum[,6], color = ~cut, type = "box",name = names(myopianum)[6])%>% layout(xaxis =list(title=names(myopia)[1]))

subplot(p1, p2,p3,p4,p5,p6,nrows = 2, shareX = TRUE)

###################################################################################
#Loggistic Model
###################################################################################

myopic_Logistic<-glm(MYOPIC~.,data=myopia, family = "binomial")
summary(myopic_Logistic)

#Comments:We also checked to change the family of the distribution with Poisson and Negative binomial
#but the AIC value and the ratio of residual deviance/degrees of freedom wasn't significant change

#Collinearity check
round(vif(myopic_Logistic),1)#Using VIF 
vif(step(myopic_Logistic, direction = "both"))


################################
#NULL MODEL
################################

myopic_Null<-glm(MYOPIC~1,data=myopia, family = "binomial")
summary(myopic_Null)

#STEPWISE FOR VARIABLE SELECTION
################################

#AIC Criterion
step(myopic_Logistic, direction = "both")
myopic_step<-glm(MYOPIC~ACD+STUDYHR+GENDER+SPORTHR+PARENTMY+SPHEQ,data=myopia, family = "binomial")
summary(myopic_step)

#BIC Criterion
step(myopic_Logistic, direction = "both", k=log(618))
summary(glm(MYOPIC~SPORTHR+PARENTMY+SPHEQ,data=myopia, family = "binomial"))

###################################
#Test for an overall effect of rank
###################################

#For Gender
wald.test(b = coef(myopic_Logistic), Sigma = vcov(myopic_Logistic), Terms = 2)

#For Parent
wald.test(b = coef(myopic_Logistic), Sigma = vcov(myopic_Logistic), Terms = 6)

###################################################################################
#Lasso for variable selection
###################################################################################

myopic_mat<- model.matrix(MYOPIC~.,myopia)[,-1]
lambdas<- 10^seq(8,-4,length=200)
myopic_model_lasso<-glmnet(myopic_mat, myopia$MYOPIC,alpha=1, lambda=lambdas,family = "binomial")


##############################
#Find the best value for Lamda
##############################

lasso1 <- cv.glmnet(myopic_mat, myopia$MYOPIC,alpha=1,lambda=lambdas, family="binomial")

#Comments:Now we want to find the minimum lamda value.

# Results
par(mfrow=c(1,2))
plot(lasso1)
plot(lasso1$glmnet.fit, xvar="lambda", label=TRUE)

#log(lasso1$lambda.min)
log(lasso1$lambda.1se)
coef(lasso1, s=lasso1$lambda.1se)

#Model after Lasso
myopic_Lasso<-glm(MYOPIC~SPHEQ+PARENTMY,data=myopia, family = "binomial")
summary(myopic_Lasso)

##############################
#2nd Interactions
##############################

myopic_2nd<-glm(MYOPIC~.^2,data=myopia, family = "binomial")
step(myopic_2nd)
summary(myopic_2nd)
#We observe that we have significant interactions between AGE:SPHEQ, AGE:SPORTHR, AL:LT,ACD:LT, LT:READHR, VCD:READHR 
#we created other model these interaction
myopic_2nd<-glm(MYOPIC~.+AGE*SPHEQ+AGE*SPORTHR+AL*LT+ACD*LT+LT*READHR+VCD*READHR,data=myopia, family = "binomial")
step(myopic_2nd)
summary(myopic_2nd)

###################################################################################
#Models Evaluation
###################################################################################

#MLE estimator
logLik(myopic_Logistic)
logLik(myopic_Lasso)

#Count Overdipresion
qcc.overdispersion.test(myopia$MYOPIC,size=1:618,type="binomial")

#Goodness of fit
#p+1<g
hoslem.test(myopic_step$y, fitted(myopic_step), g=11)
