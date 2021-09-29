#Iimporting all necessary libraries
library(ggplot2)
library(stargazer)
library(ggplot2)
library(MASS)
library(Hmisc)

#Importing the data
lalondedata <- read.delim("/lalondedata.txt", stringsAsFactors = FALSE, sep = ",", dec=",")
lalondedata

#Working on data cleaning and manipulation 
names(lalondedata)
class(lalondedata$treat)
class(lalondedata$age)
class(lalondedata$educ)
class(lalondedata$black)
class(lalondedata$hispan)
class(lalondedata$married)
class(lalondedata$nodegree)
class(lalondedata$re74)
class(lalondedata$re75)
class(lalondedata$re78)

#Labelling and recoding variables

#creating factor variable for treat
lalondedata$treat_fac <- ""
lalondedata[which(lalondedata$treat==1), 'treat_fac'] <- "jobtrain"
lalondedata[which(lalondedata$treat==0), 'treat_fac'] <- "nojobtrain"
lalondedata$treat_fac <- as.factor(lalondedata$treat_fac)
table(lalondedata$treat_fac)
table(lalondedata$treat)


#black factor
lalondedata$black_fac <- ""
lalondedata[which(lalondedata$black==1), 'black_fac'] <- "black"
lalondedata[which(lalondedata$black==0), 'black_fac'] <- "notblack"
lalondedata$black_fac <- as.factor(lalondedata$black_fac)
table(lalondedata$black_fac)
table(lalondedata$black)

#hispan factor
lalondedata$hispan_fac <- ""
lalondedata[which(lalondedata$hispan==1), 'hispan_fac'] <- "hispan"
lalondedata[which(lalondedata$hispan==0), 'hispan_fac'] <- "nothispan"
lalondedata$hispan_fac <- as.factor(lalondedata$hispan_fac)
table(lalondedata$hispan_fac)
table(lalondedata$hispan)

#married as factor
lalondedata$married_fac <- ""
lalondedata[which(lalondedata$married==1), 'married_fac'] <- "married"
lalondedata[which(lalondedata$married==0), 'married_fac'] <- "notmarried"
lalondedata$married_fac <- as.factor(lalondedata$married_fac)
table(lalondedata$married_fac)
table(lalondedata$married)

#nodegree as factor
lalondedata$nodegree_fac <- ""
lalondedata[which(lalondedata$nodegree==1), 'nodegree_fac'] <- "nodgree"
lalondedata[which(lalondedata$nodegree==0), 'nodegree_fac'] <- "degree"
lalondedata$nodegree_fac <- as.factor(lalondedata$nodegree_fac)
table(lalondedata$nodegree_fac)
table(lalondedata$nodegree)

#Looking at income (PARTI)
#real annual earnings in 1978 (response)
summary(lalondedata$re78)


#Centering the predictor continuous variables
#age 
lalondedata$agec <- c(scale(lalondedata$age,scale=F))
summary(lalondedata$agec)
#real annual earnings in 1974
lalondedata$re74c <- c(scale(lalondedata$re74,scale=F))
summary(lalondedata$re74c)
#real annual earnings in 1975
lalondedata$re75c <- c(scale(lalondedata$re75,scale=F))
summary(lalondedata$re75c)


################################Exploratory data analysis (EDA) for PARTI##################################
#summarize the variable of annual income 1978 again
dim(lalondedata)
summary(lalondedata$re78)

#Creating our response variable which is the difference in 78 income and 74income
#we choose income in 1974 because this is when baseline incomes were collected
lalondedata$change_income <- (lalondedata$re78 - lalondedata$re74)
summary(lalondedata$change_income)

#histogram of outcome (change in income)
#We have different histograms below(run the whole stuff and check the plots)
hist(lalondedata$change_income)
x <- lalondedata$change_income 
h<-hist(x, breaks=10, col="red", xlab="Real annual earnings in 1978", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
#With sigmoid curve over lay 
myhist <- hist(lalondedata$change_income)
multiplier <- myhist$counts / myhist$density
mydensity <- density(lalondedata$change_income)
mydensity$y <- mydensity$y * multiplier[1]
plot(myhist)
lines(mydensity)
myhist <- hist(lalondedata$change_income)


#Plots looking at preliminary relationships with response

#continuous predictors

#age
plot(lalondedata$agec, lalondedata$change_income)

#real annual earnings in 1974
plot(lalondedata$re74c, lalondedata$re78)   #looking at relationship with re78 (this may make more sense) 
plot(lalondedata$re74c, lalondedata$change_income) #looking at relationship with change in income

#real annual earnings in 1975 (not so mush required but worth checking, would be an alternative for baseline income)
plot(lalondedata$re75c, lalondedata$re78)
plot(lalondedata$re75c, lalondedata$change_income)

##just a quick look at the relationship between 1974 and 1975 incomes
rcorr(lalondedata$re74c, lalondedata$re75c, type="pearson") # kind of high correlation here
rcorr(lalondedata$re74c, lalondedata$re78, type="pearson")
rcorr(lalondedata$re75c, lalondedata$re78, type="pearson")

#educ
plot(lalondedata$educ, lalondedata$change_income)


#Now factor predictors

#treat
plot(lalondedata$treat_fac, lalondedata$change_income)  #you may want to add labels to x and y axis and titles, had no time

#black
plot(lalondedata$black_fac, lalondedata$change_income)

#hispan
plot(lalondedata$hispan_fac, lalondedata$change_income)

#married
plot(lalondedata$married_fac, lalondedata$change_income)

#nodgree
plot(lalondedata$nodegree_fac, lalondedata$change_income)


#Assessing interaction effects i.e association between change in income and treat across different variables

#factor variables

#treat and educ (dont consider this, nicer graph for it in continuous group of interactions) 
ggplot(lalondedata,aes(x=treat_fac, y=change_income, fill=treat_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Change in income (1974-1978) vs job training by education",x="job training",y="Change in income (1974-1978)") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ educ,ncol=4)
#doesnt look good, try another graph

#treat and black 
ggplot(lalondedata,aes(x=treat_fac, y=change_income, fill=treat_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Change in income (1974-1978) vs job training by education",x="job training",y="Change in income (1974-1978)") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ black_fac,ncol=4)

#treat and hispan_fac 
ggplot(lalondedata,aes(x=treat_fac, y=change_income, fill=treat_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Change in income (1974-1978) vs job training by hispan_fac",x="job training",y="Change in income (1974-1978)") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ hispan_fac,ncol=4)

#treat and hispan_fac 
ggplot(lalondedata,aes(x=treat_fac, y=change_income, fill=treat_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Change in income (1974-1978) vs job training by married_fac",x="job training",y="Change in income (1974-1978)") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ married_fac,ncol=4)

#treat and nodgree 
ggplot(lalondedata,aes(x=treat_fac, y=change_income, fill=treat_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Change in income (1974-1978) vs job training by nodegree_fac",x="black",y="Change in income (1974-1978)") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ nodegree_fac,ncol=4)

#CONTINOUS VARIABLE INTERACTIONS
#age and treat (Please take this graph, shows high possibility of interaction)
ggplot(lalondedata,aes(x=agec, y=change_income)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Change in income (1974-1978) vs age centered by job training”",x="job training",y="Change in income (1974-1978)") +
  facet_wrap( ~ treat_fac,ncol=4)

#educ and treat
ggplot(lalondedata,aes(x=educ, y=change_income)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Change in income (1974-1978) vs education by job training”",x="job training",y="Change in income (1974-1978)") +
  facet_wrap( ~ treat_fac,ncol=4)



#########################STATISTICAL MODELLING FOLKS :)#########################################################
#Let us start by fitting a model with all parameters regardless and see the general behaviour of model with this outcome
#recall that we dont have to put re74 and re75 in model because its taken care of in outcome
#nice to see that our treatment group could be telling us something
FullModel <- lm(change_income ~ treat_fac+agec+educ+black_fac+hispan_fac+married_fac+nodegree_fac, data=lalondedata)
summary(FullModel)

#Check these assumptions real quick
#independence 
ggplot(lalondedata,aes(x=agec, y=FullModel$residual)) + 
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs age",x="age (centered)",y="Residuals")

ggplot(lalondedata,aes(x=educ, y=FullModel$residual)) + 
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs educ",x="educ (centered)",y="Residuals")

#looks linear only sparse data
# assumption2and3. independence and equal variance (residuals and fitted values)
plot(FullModel, which=1,col=c("blue4")) # doesnt look randomly disparsed, variance may be fine though
# assumption4. Normality (qq plots)
plot(FullModel, which=2,col=c("blue4")) # can we improve this normality
#checking outliers
plot(FullModel, which=5,col=c("blue4"))    # that point 132 may worry us


#Can we try and log transform to improve normality, hmmm. lets try out!!

###################log transforming the change in annual income
#log transformation
lalondedata$change_income_log<- log(lalondedata$change_income)  # generates with -inf
summary(lalondedata$change_income_log)   #warning of nans, doesnt seem to be going well, look at this output

#smoothing the outcome to work on -infs and try log transforming again
lalondedata$change_incomenew <- (lalondedata$change_income + 0.00001)  
lalondedata$change_incomenew_log<- log(lalondedata$change_incomenew)  # generates with -inf
summary(lalondedata$change_incomenew_log)   # still have a problem of nans, but lets check histogram
hist(lalondedata$change_incomenew_log) # this is a complete disaster, we cant move on from here, clearly log transforming cant work out

#But just looking at command for NA, not required though
#lalondedata[is.na(lalondedata) | lalondedata == "NaN"] <- NA
#summary(lalondedata$change_incomenew_log)
#hist(lalondedata$change_incomenew_log)

############################What about squaring our response, can we see an improvement in outcome??
#Raising to the power2 and perform flat model with all predictors and see
FullModel4 <- lm(change_income^2 ~ treat_fac+agec+educ+black_fac+hispan_fac+married_fac+nodegree_fac, data=lalondedata)
summary(FullModel4)   # model not well behaved it seems just looking at r2 and errors

#Nevertheless, Checking assumptions
##linearity
ggplot(lalondedata,aes(x=agec, y=FullModel4$residual)) + 
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs age",x="age (centered)",y="Residuals")   # cant use this

ggplot(lalondedata,aes(x=educ, y=FullModel4$residual)) + 
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs educ",x="educ (centered)",y="Residuals") # cant use this

# assumption2and3. independence and equal variance (residuals and fitted values)
plot(FullModel4, which=1,col=c("blue4"))   ## violated

# assumption4. Normality (qq plots)  # normality looks somehow
plot(FullModel4, which=2,col=c("blue4"))

#Generally most assumptions have been violated, we cant use a squared outcome



#NOW LET US BRING BACK OUR FIRST FULL MODEL FROM ABOVE AND TRY TO IMPROVE IT THROUGH MODEL SELECTION 
#AND CHECKING ASSUMPTIONS

###############OUR FULL NORMAL MODEL WITH VARIABLE SELECTION 
#First, Full plane model with forward selection using AIC
NullModel1 <- lm(change_income ~ 1 + treat_fac ,data=lalondedata)
FullModel5 <- lm(change_income ~treat_fac + agec+educ+black_fac+hispan_fac+married_fac+nodegree_fac, data=lalondedata)
Model_forward <- step(NullModel1, scope = formula(FullModel5),direction="forward",trace=0) # Remove the trace=0 option if you want to function to print the entire process
Model_forward$call
summary(Model_forward)    # Not bad, Let us keep this model to compare with interaction model later on 

#Full model with interactions, AIC and forward selection
NullModel1 <- lm(change_income ~ 1 + treat_fac ,data=lalondedata)
FullModel_int1 <- lm(change_income ~treat_fac + agec+educ+black_fac+hispan_fac+married_fac+nodegree_fac+
                      treat_fac:agec+treat_fac:educ+treat_fac:black_fac+treat_fac:married_fac+treat_fac:nodegree_fac, data=lalondedata)
Model_forward_int1 <- step(NullModel1, scope = formula(FullModel_int1),direction="forward",trace=0) # Remove the trace=0 option if you want to function to print the entire process
Model_forward_int1$call
summary(Model_forward_int1) #Looks good, kept age and treat interaction, this seems to align with EDA, remember!

#Try stepwise and see if it gives different results
#Step wise selection
FullModel_int2 <- lm(change_income ~treat_fac+ agec+educ+black_fac+hispan_fac+married_fac+nodegree_fac+
                       treat_fac:agec+treat_fac:educ+treat_fac:black_fac+treat_fac:married_fac+treat_fac:nodegree_fac, data=lalondedata)
Model_forward_int2 <- step(NullModel1, scope = formula(FullModel_int2),direction="both",trace=0) # Remove the trace=0 option if you want to function to print the entire process
Model_forward_int2$call
summary(Model_forward_int2) # Nice, turns out results same as that of AIC forward selection  

#Stepwise model and AIC with forward selection arrive at same answer, we choose one of them to compare with model without interaction

#checking if we should keep the model with age interaction terms or not
anova(Model_forward, Model_forward_int1) # pvalue = 0.003472, we take model with interaction

#Also checking if the model with all demographic interactions is relevant vs with only age interaction; with treat
FullModel_int2 <- lm(change_income ~treat_fac+ agec+educ+black_fac+hispan_fac+married_fac+nodegree_fac+
                       treat_fac:agec+treat_fac:educ+treat_fac:black_fac+treat_fac:married_fac+treat_fac:nodegree_fac, data=lalondedata)
f_test <- anova(FullModel_int2, Model_forward_int2, test ='F')
f_test

#looks like interaction term is useful, model with age interaction as our final model
summary(Model_forward_int1)    ## our final model

#Checking assumptions of final model
##linearity
ggplot(lalondedata,aes(x=agec, y=Model_forward_int1$residual)) + 
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs age",x="age (centered)",y="Residuals")   # cant use this

# assumption2and3. independence and equal variance (residuals and fitted values)
plot(Model_forward_int1, which=1,col=c("blue4"))  
# assumption4. Normality (qq plots) 
plot(Model_forward_int1, which=2,col=c("blue4"))
#checking outliers
plot(Model_forward_int1, which=5,col=c("blue4"))  ##not good

#Generally they look good but, could still worry about independence and normality 
#Plus we have that outlier at point 132, could it be causing some violation of 
#independence and normality??
#We have no issue with leverage points


##############LAST STRATEGY: remove the 132 outlier and fit again the interaction model and check assumptions
#We remove 132 because it seems to be the furthest point away from the 3 standard deviations away from the mean
lalondedata_new <- lalondedata[-c(132),]
dim(lalondedata)
dim(lalondedata_new)


#Let us do the AIC with formward selection but without the 132 point and see
#full model with interactions
NullModel1 <- lm(change_income ~ 1 + relevel(treat_fac, ref = "nojobtrain") ,data=lalondedata_new)
FullModel_int6 <- lm(change_income ~relevel(treat_fac, ref = "nojobtrain") + agec+educ+black_fac+hispan_fac+relevel(married_fac, ref = "notmarried")+nodegree_fac+
                       treat_fac:agec+treat_fac:educ+treat_fac:black_fac+treat_fac:married_fac+treat_fac:nodegree_fac, data=lalondedata_new)
Model_forward_int6 <- step(NullModel1, scope = formula(FullModel_int6),direction="forward",trace=0) # Remove the trace=0 option if you want to function to print the entire process
Model_forward_int6$call
summary(Model_forward_int6)  # gives same model, but lets check assumptions
confint(Model_forward_int6,level=0.95)

#Model Assumptions
#Linearity (Nice!, more dispersed randomly)
ggplot(lalondedata_new,aes(x=agec, y=Model_forward_int6$residual)) + 
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs age",x="age (centered)",y="Residuals")

# assumption2and3. independence and equal variance (residuals and fitted values)
plot(Model_forward_int6, which=1,col=c("blue4"))  # Happy with independence too now and variance
#sparse data affecting equal variance and independence assumption
# assumption4. Normality (qq plots)
plot(Model_forward_int6, which=2,col=c("blue4")) # this could be best we go with normality for this analysis
#checking outliers
plot(Model_forward_int6, which=5,col=c("blue4")) # looks okay


#checking multicollinearity
library(car)
vif(Model_forward_int6)   # vif all well below 10, so well behaved  


#########So our final model is 
summary(Model_forward_int6)
confint(Model_forward_int6,level=0.95)


####################################END OF ANALYSIS PART1################################################



















