#####STATS SCRIPT#####
## Includes:
## -Basic Models
##     -OLS Regression
## -Variable Selection
##     -Best Subset Selection
##     -Forward Selection
##     -Backward Selection
## -Model Selection Tools
##     -Validation
##     -Cross-validation
##     -Ridge
##     -Lasso
#######################

###Regression Test###
baseball<-read.csv("C://Users//Ciprianna Dudding//Documents//Sentinel Development//Baseball.csv",header=TRUE,blank.lines.skip=TRUE)
dim(baseball)
names(baseball)
plot(baseball$Batting_Avg,baseball$Salary)
plot(baseball$RBI,baseball$Salary)
attach(baseball)
plot(Home_Runs,Salary)
boxplot(Salary,AE)
pairs(baseball)
summary(baseball)
bas_all<-lm(Salary ~ ., data=baseball)
summary(bas_all)
cor(baseball)
corr<-cor(baseball)
highcor<-corr>.65
highcor


###Variable Selection###
library(leaps)

##Best Subset Selection###
#  Identifies the best model that contains a given number of predictors
#  "Best" = lowest RSS; examines 2^P models, so can be computationally intensive
bss<-regsubsets(Salary~.,baseball) #Will conduct variable selection based on up
#  to an 8 variable model, interpreted by line as the number of variables to include
#  in the model, from 1 variable, up to 8, indicated with a star in each line.
#  For example, line 2 indicates that the two starred variables are the best to 
#  include in a two-variable model.
summary(bss)
bssall<-regsubsets(Salary~.,baseball,nvmax=16) #Increases the max number of varialbes
#  to 16, since that's how many we have to work with (although we won't use them!)
summary(bssall)
bssall.summary<-summary(bssall)
names(bssall.summary) #shows all of the parameters derived in the model selection
#  Probably want to focus on Rsq, AdjR2, BIC, RSS, etc.
#  This helps determine what size of model you should be using
bssall.summary$rss
bssall.summary$adjr2 #This shows that not much value is added as we add more vars
#  Simpler model is always better, so may select ~6 variabls at max.
par(mfrow=c(2,2))
plot(bssall.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(bssall.summary$adjr2,xlab="Number of Variables",ylab="Adjusted R2",type="l")
#  Plots confirm that around 6 variables is where the added benefits drop off
which.max(bssall.summary$adjr2)
points(11,bssall.summary$adjr2[11],col="red",cex=2,pch=20)
which.min(bssall.summary$rss) #Not very helpful because all vars reduce RSS
points(16,bssall.summary$rss[16],col="red",cex=2,pch=20)
which.min(bssall.summary$bic)
plot(bssall.summary$bic,xlab="Number of Variables",ylab='BIC',type="l")
points(6,bssall.summary$bic[6],col="red",cex=2,pch=20)
which.min(bssall.summary$cp)
plot(bssall.summary$cp,xlab="Number of Variables",ylab="Cp",type="l")
points(9,bssall.summary$cp[9],col="red",cex=2,pch=20)
dev.off() #since filled the 2x2 graph space, have to reset it
#  Graphs are another way of representing the best number of variables to select
#  Based on this variable selection method, we can re-run the regression with 
#  the top 6 variables identified.
bas_bss<-lm(Salary~RBI+FAE+AE+Strike.outs+Stolen_Bases+Home_Runs)
summary(bas_bss)
#  Leads to a decent model, high R2, significant variables, and good interperability

##Forward and Backward Stepwise Selection##
bas_f<-regsubsets(Salary~.,data=baseball,nvmax=16,method="forward")
summary(bas_f)
#  Forward selects predictors one at a time to an empty model
#  Added variable is the one that gives the greatest additional improvement from
#  Where the model currently is
bas_f.summary<-summary(bas_f)
names(bas_f.summary)
bas_b<-regsubsets(Salary~.,data=baseball,nvmax=16,method="backward")
summary(bas_b)
#  Backward starts will all predictors and then removes them one by one, by 
#  removing the least useful predictor in each iteration until the model is empty
bas_b.summary<-summary(bas_b)
#  Plot forward
par(mfrow=c(2,2))
plot(bas_f.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(bas_f.summary$adjr2,xlab="Number of Variables",ylab="Adjusted R2",type="l")
plot(bas_f.summary$cp,xlab="Number of Variables",ylab="Cp",type="l")
plot(bas_f.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
which.max(bas_f.summary$adjr2)
points(11,bas_f.summary$adjr2[11],col="red",cex=2,pch=20)
which.min(bas_f.summary$cp)
points(8,bas_f.summary$cp[8],col="red",cex=2,pch=20)
which.min(bas_f.summary$bic)
points(6,bas_f.summary$bic[6],col="red",cex=2,pch=20)
dev.off()
#  FSS is again showing around 6 variables that add value
#  Plot Backward selection
par(mfrow=c(2,2))
plot(bas_b.summary$rss,xlab="Number of Variables", ylab="RSS",type="l")
plot(bas_b.summary$adjr2,xlab="Number of variables",ylab="Adjusted R2",type="l")
plot(bas_b.summary$cp,xlab="Number of Variables",ylab="Cp", type="l")
plot(bas_b.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
which.max(bas_b.summary$adjr2)
points(11,bas_b.summary$adjr2[11],col="red",cex=2,pch=20)
which.min(bas_b.summary$cp)
points(8,bas_b.summary$cp[8],col="red",cex=2,pch=20)
which.min(bas_b.summary$bic)
points(6,bas_b.summary$bic[6],col="red",cex=2,pch=20)
dev.off()
#  This variable selection tool shows similar results to forward and best selection
#  We can then run the models based on the forward/backward selections
#  and compare them to each other and the BSS variable selection tool
bas_for<-lm(Salary~RBI+FAE+AE+Strike.outs+Stolen_Bases+Home_Runs)
#  Forward selection produces the same model
bas_back<-lm(Salary~RBI+FAE+AE+Strike.outs+Stolen_Bases+Home_Runs)
#  As does the backward selection
coef(bssall,6) #Lists the coefs of the 6-var version of the BSS model
coef(bas_f,6) #and forward
coef(bas_b,6) #and backward
#  Three different variable selection tools give us the same results...Good!

###Model Selection Tools###
##Validation##
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(baseball),rep=TRUE) #randomly selects a test set
test=(!train)
bas_bss<-regsubsets(Salary~.,data=baseball[train,],nvmax=16)
test.mat<-model.matrix(Salary~.,data=baseball[train,])
val.errors<-rep(NA,16)
  for (i in 1:16){
  coefi<-coef(bas_bss,id=i)
  pred<-test.mat[,names(coefi)]%*%coefi
  val.errors[i]<-mean((baseball$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors) #this shows us that the smallest RSS is with 1 variable
coef(bas_bss,1)
##Creates a function doing the above steps
predict<-function(object,newdata,id,...){
  form<-as.formula(object$call[[2]])
  mat<-model.matrix(form,newdata)
  coefi<-coef(object,id=id)
  xvars<-names(coefi)
  mat[,xvars]%*%coefi
}
bas_bsstest<-regsubsets(Salary~.,data=baseball,nvmax=16)
coef(bas_bsstest,1)

##Cross-Validation##
k=10
set.seed(1)
folds<-sample(1:k,nrow(baseball),replace=TRUE) #Divides up the ten folds
cv.errors<-matrix(NA,k,16,dimnames=list(NULL,paste(1:16))) #creates a matrix to 
#  store the results of the cv test
for (j in 1:k){
  best.fit<-regsubsets(Salary~.,data=baseball[folds!=j,],nvmax=16)
  for (i in 1:16){
    pred<-predict(best.fit,baseball[folds==j,],id=i)
    cv.errors[j,i]=mean((baseball$Salary[folds==j]-pred)^2)
  }
}
##The above gives us a 10x16 matrix, of which the (i,j) element corresponds to
#  the MSE for the ith cross-validation fold for the best j-variable model
mean.cv.errors<-apply(cv.errors,2,mean) #averages over cols to get cv error for
#  each j-variable model
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type="b") #shows 7 varibles has the lowest error
reg.best<-regsubsets(Salary~.,data=baseball,nvmax=16) #against all data now
coef(reg.best,7) #gives coefs of the selected 7 variables
bas_cv<-lm(Salary~Home_Runs+RBI+Strike.outs+Stolen_Bases+FAE+FA+AE)
summary(bas_cv)
#Performs slightly better than that selected by BSS/Forward/Backward, but similar

##Ridge Regression##
library(glmnet)
#  package does not use y~x syntax, and instead takes an x matrix and a y vector
x<-model.matrix(Salary~.,baseball)[,-1] #creates variable matrix and also 
#  transforms qualitative variables into dummy variables
y<-baseball$Salary
grid<-10^seq(10,-2,length=100) #sets the full range of possible lambda values
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) #if alpha is 0, ridge; if 1, lasso
#  glmnet automatically standardizes values to be on the same scale
#  to turn that off, uses argument "standardize=FALSE"
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
predict(ridge.mod,s=50,type="coefficients")[1:17,]
set.seed(1)
train<-sample(1:nrow(x),nrow(x)/2)
test<-y[test]
ridge.mod<-glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred<-predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
