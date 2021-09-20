library(readxl)
library(tidyverse)
library(regclass) #For VIF's
library(dplyr) #For graphs
library(olsrr) #For AIC steps 
library(MASS) #For robust regression
library(glmnet) #For ridge/lasso regression

#####Upload data and get things setup
divorce_all <- read_excel("C:/Users/eberl/OneDrive/Desktop/Semester II/Regression/Project/divorce.xlsx")
divorce = divorce_all[-c(2:4)]

#Shortening variables
dr <- divorce$`Divorce Rate`
income <- divorce$`Household Income`
wc <- divorce$`Wedding Cost`
religious <- divorce$`Percent that Consider Religion Important`
degree <- divorce$`Percent with a Bachelor's Degree`
pr <- divorce$`Poverty Rate`
unmarried <- divorce$`Percent of Babies Born to Unmarried Mothers`
tbr <- divorce$`Teen Birth Rate`
age <- divorce$`Median Age 2018`
mr <- divorce$`Marriage Rate`

#Making categorical variables factor
divorce$Region = as.factor(divorce$Region)                 
divorce$State = as.factor(divorce$State)
divorce$`Minimum Marriage Age` = as.factor(divorce$`Minimum Marriage Age`)
divorce$`Election Result` = as.factor(divorce$`Election Result`)

region <- divorce$Region
min_age <- divorce$`Minimum Marriage Age`
result <- divorce$`Election Result`

#####Data exploration

#DR by Categorical 
plot(divorce$`Minimum Marriage Age`,divorce$`Divorce Rate`, xlab = 'Minimum Marriage Age', ylab = "Divorce Rate")
plot(divorce$`Election Result`,divorce$`Divorce Rate`, xlab = 'Election Result', ylab = "Divorce Rate")
plot(divorce$Region,divorce$`Divorce Rate`, xlab = 'Region', ylab = "Divorce Rate")
summary(divorce$`Minimum Marriage Age`)

#Some Plots 
plot(divorce$`Divorce Rate`, main = 'Divorce Rate', ylab = 'Rate per 1,000')
plot(divorce$`Wedding Cost`, divorce$`Divorce Rate`, xlab="Bachelor's Degree", ylab='Divorce Rate')
plot(divorce$`Percent with a Bachelor's Degree`, divorce$`Divorce Rate`)
plot(divorce$`Poverty Rate`, divorce$`Divorce Rate`)
plot(divorce$`Teen Birth Rate`, divorce$`Divorce Rate`)
plot(divorce$`Percent that Consider Religion Important`, divorce$`Divorce Rate`, xlab='Proportion that Considers Religion Important', ylab='Divorce Rate', main="Religion")
plot(divorce$`Wedding Cost`, divorce$`Divorce Rate`, xlab = 'Average Wedding Cost', ylab = "Divorce Rate", main="Wedding Cost")
plot(divorce$`Household Income`, divorce$`Divorce Rate`)
plot(divorce$`Percent with a Bachelor's Degree`, divorce$`Wedding Cost`)
plot(divorce$`Percent with a Bachelor's Degree`, divorce$`Divorce Rate`, xlab="Bachelor's Degree", ylab='Divorce Rate', main='Degree')

#Scatterplot matrix
pairs(cbind(dr,age,degree,income,pr,religious,tbr,unmarried,wc, mr)) #Scatterplots
## degree, income, tbr, pr are linear with a lot of other vars

#Looking closer at correlations

cor(income, pr)
cor(wc, income) #very high
cor(pr, tbr) #very high
cor(tbr,religious) #very high
plot(tbr,religious)
plot(religious, unmarried)
plot(income,unmarried)
plot(income,religious)
plot(dr,wc)
cor(religious, degree)
cor(unmarried, degree)

# Average divorce by categorical variables
### region
region<-as.factor(divorce$Region)
tapply(divorce$`Divorce Rate`, divorce$Region, summary)
summary(region)

ggplot(divorce, aes(x=Region, y=`Divorce Rate`)) + 
  geom_boxplot(aes(fill = Region), show.legend = FALSE)

aggregate(divorce$`Divorce Rate`,         # Median by group
          list(divorce$`Region`),
          median, na.rm=TRUE)
plot(divorce$`Divorce Rate`, divorce$`Percent that Consider Religion Important`)
### 2016 election results
ggplot(divorce, aes(x=`Election Result`, y=`Divorce Rate`)) + 
  geom_boxplot(aes(fill = `Election Result`), show.legend = FALSE)

aggregate(divorce$`Divorce Rate`,         # Median by group
          list(divorce$`Election Result`),
          median, na.rm=TRUE)

### Income by region
ggplot(divorce, aes(x=`Region`, y=`Household Income`)) + 
  geom_boxplot(aes(fill = `Region`), show.legend = FALSE)

aggregate(divorce$`Household Income`,         # Median by group
          list(divorce$`Region`),
          median, na.rm=TRUE)









#####The beginning of modeling

##### A Model with Numerical Variables 
#Using AIC.
out = lm(dr~age+degree+income+pr+religious+tbr+unmarried+wc+mr)
ols_step_forward_aic(out, details=TRUE) #mr, degree, unmarried
              #age, tbr, income ->  R^2 = .4885
ols_step_backward_aic(out) #age, degree, income, tbr, unmarried, mr
ols_step_both_aic(out) # same as forward

#Amazingly, they all give the same model
out1=lm(dr~mr+degree+unmarried+tbr+age+income)
summary(out1) #R^2 = .4885
anova(out1)
VIF(out1) #ok, degree and income could be better

#Without TBR (not sig)
out2=lm(dr~ mr+degree+unmarried+age+income)
summary(out2)
VIF(out2)

#Without Income (not sig and high VIF)
out3=lm(dr~ mr+degree+unmarried+age)
summary(out3)
VIF(out3)

#Without age (not sig)
out4=lm(dr~mr+degree+unmarried) #Adj R^2 = 0.3857 
summary(out4)
VIF(out4)

#Now let's add interaction terms - do they help? - one does
out5=lm(dr~degree+unmarried+mr+degree:unmarried)
summary(out5)
VIF(out5)
cor(unmarried, degree)

out5=lm(dr~degree+unmarried+mr+degree:mr) #Adj R^ = .4531, unmarried is no longer sig
summary(out5)
VIF(out5)

out5=lm(dr~degree+unmarried+mr+unmarried:mr) #Adj R^ = .4845, all sig!
summary(out5)
VIF(out5)

out5=lm(dr~degree+unmarried+mr+degree:mr+unmarried:mr) #Not as good
summary(out5)
VIF(out5)
cor(unmarried, degree)




###### Add Categorical variables
out = lm(dr~degree+unmarried+mr+unmarried:mr + region)#Add region first
summary(out) #Looking good! We're up to Adj R^2 = .6078
anova(out)

out = lm(dr~degree+unmarried+mr+unmarried:mr + min_age)#min_age is not significant
summary(out) 
anova(out)

out = lm(dr~degree+unmarried+mr+unmarried:mr + result)#Result is not significant
summary(out) 
anova(out)

#Look at categorical variables together and with interaction terms
out=lm(dr~degree+unmarried+mr+unmarried:mr + region + region:degree)  
summary(out) #Adj R^2 = .6446
anova(out)
VIF(out)

out=lm(dr~degree+unmarried+mr + region + region:degree) #without other interaction
summary(out) #Adj R^2 = .6532
anova(out)
VIF(out)

out=lm(dr~degree+unmarried+mr+unmarried:mr + region + region:mr) 
summary(out) #Adj R^2 = .7066
anova(out)
VIF(out)

out=lm(dr~degree+unmarried+mr + region + region:mr) #without other interaction (not sig)
summary(out) #Adj R^2 = .713
anova(out)
VIF(out)

out=lm(dr~degree+unmarried+mr+unmarried:mr + region + region:unmarried) 
summary(out) #Adj R^2 = .6401
anova(out)
VIF(out)

out=lm(dr~degree+unmarried+mr + region + region:unmarried) 
summary(out) #Adj R^2 = .6492
anova(out)
VIF(out)



###### Is final model a good fit?
out=lm(dr~degree+unmarried+mr + region + region:mr) #without other interaction (not sig)
par(mfrow=c(2,2))
plot(out)
VIF(out) ## MR and MR:region have very high VIF's
par(mfrow=c(1,1))

#Does centering help?
mr_c=mr-mean(mr)
out=lm(dr~degree+unmarried+mr_c + region + region:mr_c) #without other interaction (not sig)
VIF(out) #VIF for mr_c is still very high, though lower for the interaction

#It doesn't, so we revert to this

######Final Model (Best)
out=lm(dr~degree + unmarried + mr + region )
summary(out) #Adj R^2 = .6018
anova(out) # All very significant 
VIF(out) #Looks good
pairs(cbind(dr,degree,unmarried,mr)) #Unmarried and degree might have slight corr
cor(unmarried, degree) # not too too bad
par(mfrow=c(2,2))
plot(out)
par(mfrow=c(1,1))

#Interesting relationship?
plot(unmarried, dr, ylab = 'Divorce Rate', xlab = "% to Unmarried Mothers")
text(dr ~unmarried, labels=rownames(divorce),data=divorce, cex=0.9, font=2)
#21 (MA), 13 (IL), and 44 (UT) look like big players, debatably 18 (LA) and 24 (MS)


#Look at PRESS residuals - Look good
rstandard(out,type='predictive') 
summary(rstandard(out,type='predictive'))



##### States with missing divorce rates: 
  ##California, Hawaii, Indiana, Minnesota, New Mexico

#California = 0.062969 - .077706*.35 - .049695*0.372 + .073723*0.057 + 0.008314(1)
#Hawaii = 0.062969 - .077706*0.3363 - .049695*0.384 + .073723*0.142 + .008314(1)
#Indiana = 0.062969 - .077706*0.2694 - .049695*0.435 + .073723*0.062
#Minnesota = 0.062969 - .077706*0.3725 - .049695*0.320 + .073723*0.051
#New_Mexico = 0.062969 - .077706*0.2767 - .049695*0.515 + .073723*0.060 + 0.006928(1)

#CA = 0.0298
#HI = 0.0365
#IN = 0.0250
#MN = 0.0219
#NM = 0.0272

cor(religious, dr)


##### Do I get the same model with stepwise_p?
out = lm(dr~age+degree+income+pr+religious+tbr+unmarried+wc+mr)
ols_step_forward_p(out,penter=0.05, details= TRUE) #mr, degree, unmarried
ols_step_backward_p(out,prem=0.05) #age, tbr, unmarried, mr - a little lower C(p) but an extra var
ols_step_both_p(out,penter=0.15,prem=0.15) #mr, degree, unmarried  and all are in 
                                           #model if we change penter/prem to .1 or .05 too

out = lm(dr~mr+tbr+unmarried+age) #higher adjusted R^2, but not when we add region
summary(out)
anova(out)
VIF(out)

out = lm(dr~mr+degree+unmarried) #What we got above, so I will stick with it
summary(out)

#All influence measures - note 13 (IL), 28 (NV), 44 (UT)
influence.measures(out) 





#####Do we get the same model without Nevada?
small <- divorce %>% 
filter(State != 'Nevada') 

#Making categorical variables factor
small$Region = as.factor(small$Region)                 
small$State = as.factor(small$State)
## Should age be a factor? - can try both
small$`Minimum Marriage Age` = as.factor(small$`Minimum Marriage Age`)
small$`Election Result` = as.factor(small$`Election Result`)

#Shortening variables
sdr <- small$`Divorce Rate`
sincome <- small$`Household Income`
swc <- small$`Wedding Cost`
sreligious <- small$`Percent that Consider Religion Important`
sdegree <- small$`Percent with a Bachelor's Degree`
spr <- small$`Poverty Rate`
sunmarried <- small$`Percent of Babies Born to Unmarried Mothers`
stbr <- small$`Teen Birth Rate`
sage <- small$`Median Age 2018`
smr <- small$`Marriage Rate`



out = lm(sdr~sage+sdegree+sincome+spr+sreligious+stbr+sunmarried+swc+smr)
ols_step_forward_p(out,penter=0.05) #smr, sdegree, sincome
ols_step_backward_p(out,prem=0.05) #sdegree, sincome, smr
ols_step_both_p(out,penter=0.15,prem=0.15) #smr, sdegree, sincome

#ALL SAME MODEL!! Put those in a model
out = lm(sdr~smr + sdegree + sincome)
summary(out) #Adj R^2 = .647
VIF(out) #boarderline, but all under 5
cor(sdegree, sincome) #that's pretty high

#Maybe an interaction term will help
out = lm(sdr~smr + sdegree + sincome + sdegree:sincome)
summary(out) #Adj R^2 = .638
VIF(out) #Definitely not helpful!!

#Perhaps centering 
sincome_c=sincome-mean(sincome)
out=lm(sdr~smr + sdegree + sincome_c + sdegree:sincome_c)
summary(out)
VIF(out) #Not helpful

#Perhaps centering without interaction
sincome_c=sincome-mean(sincome)
out=lm(sdr~smr + sdegree + sincome_c ) #without other interaction (not sig)
summary(out)
VIF(out) #Not helpful, same as before


##### Adding categorical variables to the "small" model (built without Nevada)
out = lm(sdr~smr + sdegree + sincome + small$Region)
summary(out) #Adj R^2 = .6503
VIF(out) #All look great
plot(out)

#I ran this with region interacting with each numeric variable. The Adj R^2
  #goes up, but the GVIF's (right column) are too high
out = lm(sdr~smr + sdegree + sincome + small$Region + small$Region:sincome)
summary(out) 
VIF(out) #All look great

#However, region is not significant
out = lm(sdr~smr + sdegree + sincome + small$Region)
anova(out)

##### Final Model (chosen without Nevada)
out = lm(sdr~smr + sdegree + sincome)
summary(out) #Adj R^2 = .647
anova(out)
VIF(out) #All look good, under 10 and even 5, though degree and income are almost
plot(out) #All plots look good, residuals vs leverage is a little noteworthy
          #since residuals shrink as leverage increases. All are within cook's
          #distance it looks ike (maybe 13? (Illinois))


#Predictions 

#California = 7.937e-03 + 4.597e-01*0.057 -9.871e-02*0.3500 + 3.809e-07*80440
#Hawaii = 7.937e-03 + 4.597e-01*0.142 -9.871e-02*0.3363 + 3.809e-07*83102
#Indiana = 7.937e-03 + 4.597e-01*0.062 -9.871e-02*0.2694 + 3.809e-07*57603
#Minnesota = 7.937e-03 + 4.597e-01*0.051 -9.871e-02*0.3725 + 3.809e-07*74593
#New_Mexico = 7.937e-03 + 4.597e-01*0.060 -9.871e-02*0.2767 + 3.809e-07*51945

#CA = 0.030231
#HI = 0.07167178
#IN = 0.03178691
#MN = 0.0230247
#NM = 0.02799179




#With NV, (Marriage Rate, Unmarried, Degree, Region)
#Without NV, (Marriage Rate, Income, Degree, Region)


#####Robust regression
out = rlm(dr~degree + unmarried + mr + region )
summary(out)
plot(out) #Not super great, and definitely not better. There's still a cook's
          #distance outlier, it's just a new one. If I remove another and 
          #another I'll have the same problem for about 6 or more points
          #The residual std error does decrease by a little.

VIF(out)  #Good

#California = 0.0587 + -0.0690*.35 -0.0446*0.372 + 0.0685*0.057 + 0.0081
#Hawaii = 0.0587 + -0.0690*0.3363 -0.0446*.384 + 0.0685*.142 + 0.0081
#Indiana = 0.0587 + -0.0690*0.2694 -0.0446*0.435 + 0.0685*0.062 
#Minnesota = 0.0587 + -0.0690*.3725 -0.0446*.32 + 0.0685*.051 
#New_Mexico = 0.0587 + -0.0690*0.2767 -0.0446*.515 + 0.0685*.06 + 0.0078

#CA = .0300
#HI = .0362
#IN = .0250
#MN = .0222
#NM = .0285



#####Ridge/Lasso regression
nonmiss <- divorce %>% 
  filter(!is.na(divorce$`Divorce Rate`) )

nmd <- nonmiss$`Percent with a Bachelor's Degree`
nmu <- nonmiss$`Percent of Babies Born to Unmarried Mothers`
nmmr <- nonmiss$`Marriage Rate`
nmr <- as.factor(nonmiss$Region)
xmat = cbind(nmd, nmu, nmmr, nmr)

lasso=glmnet(xmat,nonmiss$`Divorce Rate`,alpha=1) #Lasso
plot(lasso)

ridge=glmnet(xmat,nonmiss$`Divorce Rate`,alpha=0) #ridge
plot(ridge)

#####The highest Adj. R^2 I found was .713, but the model
      #had strong multicolinearity
out=lm(dr~degree+unmarried+mr + region + region:mr) #without other interaction (not sig)
summary(out) #Adj R^2 = .713
anova(out)
VIF(out)





#####FINAL MODEL
out=lm(dr~degree + unmarried +mr + region )
summary(out) #Adj R^2 = .6018
