


#libraries######################################################################
install.packages("missMDA")
install.packages("naniar")
install.packages("VIM")
install.packages("ggplot2")
install.packages("moments")
install.packages("magrittr")
install.packages("pastecs")
install.packages("BBmisc")
install.packages("rmarkdown")
install.packages("ggpubr")

library(naniar)
library(VIM)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(pastecs) 
library(moments)
library(tidyverse)
library(BBmisc)
library(tidyr)
library(purrr)
library(magrittr)
library(mice)
library(missMDA)
library(knitr)

#Loading Dataset 

df1 <- read.csv("Life_Expectancy_Data1.csv", header = T)
head(df1) #An overview of the data
dim(df1) #Check for number of rows and column 

#In order to make them more descriptive, column names were renamed.
df2 <- df1 %>% rename(Life.Exp = SP.DYN.LE00.IN,  
                      Adj.NNIncome = NY.ADJ.NNTY.KD.ZG,
                      Adj.NNIncome.P = NY.ADJ.NNTY.PC.KD.ZG,
                      Age.to.14= SH.HIV.INCD.14,
                      Dropout.Children = SE.PRM.UNER,
                      Pry.Graduate = SE.PRM.CUAT.ZS,
                      BSc.Graduate = SE.TER.CUAT.BA.ZS,
                      Infant = SP.DYN.IMRT.IN,
                      Pry.Complete= SE.PRM.CMPT.ZS,
                      Adult.lite = SE.ADT.LITR.ZS,
                      Real.int.rate = FR.INR.RINR,
                      Pop.growth = SP.POP.GROW,
                      Pop_density = EN.POP.DNST,
                      Total.pop = SP.POP.TOTL,
                      HealthExp.PPP = SH.XPD.CHEX.PC.CD,
                      HealthExp.GDP = SH.XPD.CHEX.GD.ZS,
                      Unemployment = SL.UEM.TOTL.NE.ZS,
                      Gdp.Grwth = NY.GDP.MKTP.KD.ZG,
                      Gdp.PPP = NY.GDP.PCAP.CD,
                      Birth.Rate = SP.DYN.CBRT.IN,
                      Energy.Consump = EG.FEC.RNEW.ZS,
                      Adults.Hiv.15.19 = SH.HIV.INCD,
                      Ppl.SafeWater = SH.H2O.SMDW.ZS,
                      Poverty.Headcount = SI.POV.LMIC,
                      Comp.Edu = SE.COM.DURS)
head(df2) #Verifying the column names have been changed
dim(df2)
summary(df2) #Summary of data
##The descriptive statistics before the missing values were taken into account
stat.desc(df2, norm = TRUE) 


#Number of missing values in data
#Counting number of missing values in each column
na_count <- sapply(df2, function(x) sum(length(which(is.na(x)))))
na_count

#Observation:
#There are 266 rows and 26 columns with the last column (26) having empty rows
#from row 219, therefore  we need to drop the rows



#Dropping country, country code and continent
df <-  df2[-c(1,2,3)] 
head(df)

#Visualization
#Histogram Plot of predictor variables
df %>% keep(is.numeric) %>%   #plot of histograms for all variables
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(color= "blue", fill = "blue")




gg_miss_upset(df, color = 'blue') #Relationship of missing values within the columns
vis_miss(df) #Distribution of missing values in data
#gg_miss_case(df) 
gg_miss_var(df, show_pct = TRUE) #Percentage of missing values in each column
gg_miss_which(df) #Column without Missing variable is the black one
ggplot(df) + aes(x = Life.Exp) + geom_histogram(binwidth = 10, color= "blue", fill = "blue" )

#ggboxplot
ggplot(df, aes(x=Adj.NNIncome, fill=Life.Exp)) + geom_boxplot(notch=TRUE)

#Scatter Plots of missing values in re;ation to Life expectancy
ggplot(df, aes(x = Adj.NNIncome, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = Adj.NNIncome.P, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x= Age.to.14, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = df$Dropout.Children, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = df$Pry.Graduate, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = df$BSc.Graduate, y= Life.Exp)) + geom_miss_point() #negative correlation
ggplot(df, aes(x = df$Infant, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = df$Pry.Complete, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = df$Adult.lite, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = df$Real.int.rate, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = df$Pop.growth, y= Life.Exp)) + geom_miss_point() #obs
ggplot(df, aes(x = df$Pop_density, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = df$Total.pop, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = df$HealthExp.PPP, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = df$HealthExp.GDP, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = df$Unemployment, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = df$Gdp.Grwth, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = df$Gdp.PPP, y= Life.Exp)) + geom_miss_point()
ggplot(df, aes(x = df$Birth.Rate, y= Life.Exp)) + geom_miss_point()

#Treating missing values###########################
#############################
##Percentage of missing values in data
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(df, 2, p)
md.pairs(df)
md.pattern(df, plot = T)

clean_data <- mice(df, seed = 23884, method = "cart", m=20)
head(clean_data)

# This shows the number of missing values is now zero
n_miss(clean_data)
complete(clean_data)

# To have a better understanding of the data, we can plot it

clean_data$imp
stripplot(clean_data, pch = 20, cex = 1.2)

rmarkdown::render("MA317.R")


###########################################

install.packages("mice")
install.packages("dplyr")
install.packages("corrarray")
install.packages("corrplot")
install.packages("faraway")
install.packages("corrarray", dependencies=TRUE)
install.packages("stringi", dependencies=TRUE)
library(mice)
library(dplyr)
library(corrarray)
library(corrplot)
library(faraway)

data<-read.csv("Life_Expectancy_Data1.csv", header = T)

data_rename<-data
data_rename <- data %>% rename(Life.Exp = SP.DYN.LE00.IN,  #renaming columns
                               ElecAccess = EG.ELC.ACCS.ZS,
                               GovtInc = NY.ADJ.NNTY.KD.ZG,
                               GovtIncPP = NY.ADJ.NNTY.PC.KD.ZG,
                               HIVto14 = SH.HIV.INCD.14,
                               OutofSch1 = SE.PRM.UNER,
                               PryGrad = SE.PRM.CUAT.ZS,
                               Degree = SE.TER.CUAT.BA.ZS,
                               Infant = SP.DYN.IMRT.IN,
                               PryGrad1 = SE.PRM.CMPT.ZS,
                               Read = SE.ADT.LITR.ZS,
                               Int = FR.INR.RINR,
                               PopRate = SP.POP.GROW,
                               PopSqmt = EN.POP.DNST,
                               PopSum = SP.POP.TOTL,
                               Healthpp = SH.XPD.CHEX.PC.CD,
                               Health = SH.XPD.CHEX.GD.ZS,
                               Jobless = SL.UEM.TOTL.NE.ZS,
                               GDP = NY.GDP.MKTP.KD.ZG,
                               GDPpp = NY.GDP.PCAP.CD,
                               Births = SP.DYN.CBRT.IN,
                               NewEnergy = EG.FEC.RNEW.ZS,
                               HIVAdult = SH.HIV.INCD,
                               SafeH2O = SH.H2O.SMDW.ZS,
                               Poverty = SI.POV.LMIC,
                               SchYrs = SE.COM.DURS)




# Finding % Missing values per column

p <- function(x) {sum(is.na(x))/length(x)*100}
apply(data_rename, 2, p)

# Drop columns > 50% but keep HIV_age 14 (58%). Dropping 5 columns
#Pry.Graduate, BSc Graduate, Adult.lite, Energy.consumption and Poverty.Headcount

new_df<-data_rename
new_df<-data_rename %>% select(-10, -11, -14, -25, -28)

#Check columns renamed
#str(new_df)

#Impute Missing Values

imputations <- mice(new_df, seed = 23884, method = "cart", m=10)

#Check first imputed dataset
#print(imputations)

#Get each imputed datasets sequentially for multicollinearity experiments.
#The next line of code gets the second imputed dataset

new_df2<-complete(imputations, 2)
head(new_df2)

#Get correlation matrix

X<-new_df2[ ,-1:-3]
new_df2.corr<-cor(X)

#To express matrix values to two decimals
#new_df2.corr

round(new_df2.corr,2)

#corrplot.mixed(new_df2.corr, lower.col = "black", number.cex = .1)
#Lab4 code illegible and Untidy correlogram
#Using new code

#Plot Correlogram

corrplot.mixed(new_df2.corr, lower = "number", upper = "circle",tl.pos = c("lt"))
round(new_df2.corr,1)

# Regression of Unrestricted 'full' model with all 20 predictor variables

model1 <- lm(Life.Exp ~ ElecAccess + GovtInc + GovtIncPP + HIVto14 + OutofSch1 + Infant + PryGrad1 + Int + PopRate + PopSqmt + PopSum + Healthpp + Health + Jobless + GDP + GDPpp + Births +
               HIVAdult + SafeH2O + SchYrs, data=new_df2)
summary(model1)

#Regression of Restricted model with test columns dropped

model2 <- lm(Life.Exp ~ ElecAccess + GovtIncPP  + OutofSch1 + Infant + PryGrad1 + Int + PopRate + PopSqmt + PopSum + Healthpp + Health + Jobless + GDP + GDPpp + Births +
               HIVAdult + SafeH2O + SchYrs, data=new_df2)
summary(model2)

#Checking suitability of restricted model using F-test

anova(model2,model1)

#Get VIF values
X = new_df2[c(-1:-3,-6,-8)]
vif(X)

# Executing conclusion reached after multicoll experiments.
#Dropping "GovtInc" and "HIV14"

new_colsdf2<-new_df2
new_colsdf2<-new_df2 %>% select(-6,-8)

#Checking that columns dropped"
#str(new_colsdf2)

#Save this imputed dataset for use in pooling and features selection, after multicol processing is completed on the other imputed datasets.

#save(new_colsdf2, file =  "C:/Users/Inthiran Moodley/OneDrive/Documents/Essex University/Academic/2021/MA317_Expt Data/Assignment 2_MA317_Group/Multicor_imp_data/new_colsdf2.Rdata", header = T)


#####################################################################
###########Model Selection Methods################

new_df_final<- complete(imputations)  #Data set after the data cleaning.
head(new_df_final)
new_df_final2<- new_df_final
new_df_final2<- new_df_final %>% select(-6,-8) #Removed the correlated variables.
#########Majority Method############

imputations$imp #This save all the 10 imputed data sets.
#Checking on the imputed values might be helpful in diagnosing.
stripplot(imputations, pch = 100, cex = 2)
xyplot(imputations,Life.Exp~Infant | .imp, pch=20, cex=1.4)
#From the above plot, we found that the imputed values follows  
model.fit <- with(imputations, lm(Life.Exp ~ Infant + Births))
summary(model.fit)

# If our interest was to check differences in the mean values of Life.Exp 
# and Infant, a t-test would be performed and so within the context of 
# imputation the following command is needed:

model.fit2 <- with(imputations, t.test(Life.Exp,Infant))
model.fit2

pooled.model<-pool(model.fit)

summary(pooled.model)

pool.r.squared(model.fit)

feature.selection1 <- expression(null.model1 <- lm(Life.Exp ~ 1),
                                 model2 <- step(null.model1, scope = ~ ElecAccess+GovtIncPP+OutofSch1+Infant+PryGrad1+Int+PopRate+PopSqmt+PopSum+Healthpp+Health+Jobless+GDP+GDPpp+Births+HIVAdult+SafeH2O+SchYrs))
summary(feature.selection1)
#Using the with function evaluate the above expression
step.fit <- with(imputations, feature.selection1)
#One way to conclude which is the best model in the case of multiple imputation is the majority method.
#According to this method, the variables which appear at least in half of the imputed models are selected
# in the final model. To extract the number of times each variable appears in the m imputed datasets the
#following code can be used:
step.fit.models <- lapply(step.fit$analyses, formula)
step.fit.features <- lapply(step.fit.models, terms)
feature.frequency <- unlist(lapply(step.fit.features, labels))
feature.frequency
sort(table(feature.frequency),decreasing=TRUE)
#According to the Majority method of model selection, the variables which appear at least in half of the imputed models are selected in the final model.
#From the above analysis we found that variables Births, Healthpp, Infant, PopRate, HIVAdult, ElecAccess,PopSqmt, SafeH2O, Health & Jobless appears 10, 10, 10, 10, 9, 8, 6, 6, 5, 5 times respectively in the imputed models. 
#We are selecting the variables Births, Healthpp, Infant, PopRate, HIVAdult, ElecAccess,PopSqmt, SafeH2O which appears more than half of the imputed models for the final model. 

best.model1<-lm(Life.Exp~Births+Healthpp+Infant+PopRate+HIVAdult+ElecAccess+SafeH2O,data=new_df_final2)
summary(best.model1)
#From the summary we can find that all the selected variables are significant predictor variables.

#We can evaluate other model selection methods.

########## Mallow's Cp and AIC###################################

#Model for estimating Life Expectancy against all other variables
full.model<-lm(Life.Exp ~ ElecAccess+GovtIncPP+OutofSch1+Infant+PryGrad1+Int+PopRate+PopSqmt+PopSum+Healthpp+Health+Jobless+GDP+GDPpp+Births+HIVAdult+SafeH2O+SchYrs,data=new_df_final2)
summary(full.model)
#From the above estimate we found that other than intercept, the predictor variables Infant, PopRate, Healthpp, Births, HIVAdult are found as significant predictor variables. 
# So we can estimate another model with these predictor variables.
reduced.model<-lm(Life.Exp ~ Infant+PopRate+Healthpp+Births+HIVAdult,data=new_df_final2)
summary(reduced.model)

#In the reduced model F:statistic value has increased and the p-value is same in both the models. 
#We can compare the two model by using anova.
anova(reduced.model,full.model)
#From this we found that p-value is more. This says that the model with all the variables are actually better.
#Hence failed the hypothesis that the model with significant predictor values is good.
#This shows we need to investigate more models.
anova(full.model)
#From this we can found that 'EleAccess' has the very high variance. 
#Also found that the more variability comes from the following predictor variables. EleAccess, Infant, Healthpp and Births.

#Another method for choose between full model and reduced model is by calculating AIC and Mallow's Cp values.
AIC(full.model)
AIC(reduced.model)
#Here AIC of Reduced model is 969.6793, and AIC of Full model is 982.5669. We can say that the model with less AIC value is better.
#That means the reduced model, the one with significant predictor variables is better.

#We can determine whether the reduced model is a viable fit by calculating the Mallow's Cp value.
ols_mallows_cp(reduced.model,full.model)
#Mallows Cp value is 5.333112 
#If the given value is close to, or smaller than, the number of predictor variables (5+1) in the submodel then it is an acceptable model.
#Here it is smaller than number of predictor variables (5+1). That means the reduced model is acceptable.

#We must consider every single model and find which of those are good models,rather than randomly trying.
#By using 'leaps' command we can consider each sub model and calculate their respective Mallow's Cp value
#For this we have to start with full model with slight change.
full.model.cp<-lm(Life.Exp ~ ElecAccess+GovtIncPP+OutofSch1+Infant+PryGrad1+Int+PopRate+PopSqmt+PopSum+Healthpp+Health+Jobless+GDP+GDPpp+Births+HIVAdult+SafeH2O+SchYrs,data=new_df_final2,x=TRUE)
x<-full.model.cp$x
y<-new_df_final2[,-c(1,2,3)]$Life.Exp
all.models1<-leaps(x,y,int=FALSE,strictly.compatible = FALSE,method="Cp")
all.models1
#To find out which is the best model according Cp value we have to find out the model with smallest Cp value.
min.cp<-all.models1$Cp==min(all.models1$Cp) #Find out the smallest CP value
min(all.models1$Cp) #Gives the minimum Cp value
min.cp<-all.models1$which[min.cp, ] #Find outs the model which has smallest Cp value.
min.cp
#From this Mallow's Cp selection method we can find out that better model is the one with follwoing variables.
#ElecAccess, Infant, PopRate, Healthpp, GDPpp, Births, HIVAdult, SafeH2O
better.model.cp1<-(lm(Life.Exp~ElecAccess+Infant+PopRate+Healthpp+GDPpp+Births+HIVAdult+SafeH2O,data = new_df_final2))
summary(better.model.cp1)

#####Wrapper Variable selection methods#######

#Wrapper variable selection methods: Backward feature selection & Forward feature selection

#### Backward Feature selection #####

full.model<-lm(Life.Exp ~.,data=new_df_final2[,-c(1,2,3)])
back.step1<-step(full.model,method='backward')
summary(back.step1)
# From this method find that the least AIC obtained by estimating the model with below mentioned parameters.That is the best model.
#AIC=347.29
# Hence from this we can conclude that the model with below mentioned predictor variables is better one.
# Life.Exp ~ ElecAccess + Infant + PopRate + Healthpp + GDPpp + Births + HIVAdult + SafeH2O
best.model.backward<-lm(Life.Exp ~ ElecAccess + Infant + PopRate + Healthpp + GDPpp + Births + HIVAdult + SafeH2O,data=new_df_final2)
summary(best.model.backward)

####Forward feature selection ########

#For this we need to start with a null model
null.model<-lm(Life.Exp ~1,data=new_df_final2[,-c(1,2,3)])
summary(null.model)
forward.step1<-step(null.model,method='forward',scope=~ElecAccess+GovtIncPP+OutofSch1+Infant+PryGrad1+Int+PopRate+PopSqmt+PopSum+Healthpp+Health+Jobless+GDP+GDPpp+Births+HIVAdult+SafeH2O+SchYrs)
# From this method find that the least AIC obtained by estimating the model with below mentioned parameters. That is the best model.
#AIC=347.29
# Hence from this we can conclude that the model with below mentioned predictor variables is better one.
#Life.Exp ~ Infant + Healthpp + Births + PopRate + HIVAdult + SafeH2O + ElecAccess + GDPpp
best.model.forward<-lm(Life.Exp ~ Infant + Healthpp + Births + PopRate + HIVAdult + SafeH2O + ElecAccess + GDPpp,data=new_df_final2)
summary(best.model.forward)
#Backward selection model and forward selection model gives the same results.



#####################################################################
#################One-way Anova################


#installing and loading relevant packages

install.packages("ggplot2")
install.packages("ggpubr")
library(ggplot2)
library(ggpubr)
library(dplyr)

#We want to see if there are disparities in life expectancy between the six continents using 
#one-way ANOVA. We should check for some summary statistics like group means, 
#explore the dataset using graphical representations of data, 
#and check for missing values before utilising the one-way ANOVA model.

#grouping the countries by continent wise
data_anova <- new_df_final[,c(3,4)] %>% group_by(Continent) %>% 
  dplyr::summarize(AveragelifeExp=mean(Life.Exp)) 

#converting the numeric values to the factor using as.factor function
data_anova$Continent<-as.factor(data_anova$Continent) 

#calculating mean of Life expectancy
mean(data_anova$AveragelifeExp) 


#plotting boxplot for continent vs Life expectancy
ggboxplot(new_df_final, x = "Continent", y = "Life.Exp") 

#calculating each group means i.e each continent's means
group.means<-tapply(data_anova$AveragelifeExp,data_anova$Continent, mean) 

#printing group means
group.means 

#adjusting the indent in plots
par(mar=c(1,1,1,1))

#plotting boxplot for comparing the life expectancy of all continents
boxplot(new_df_final$Life.Exp~new_df_final$Continent,main='Comparing the life expectancy of six continents', 
        xlab='Continent', col="light gray",las = 2,cex.axis=0.75, ylab = "Life Expectancy") 

#calculating one-way anova using aov inbuilt funtion 
anova1way<-aov(Life.Exp~as.factor(Continent),data=new_df_final) 

#The F-value and the p-value in the last columns of the table are the values we need to check.
#Pr(>F). We can see that at the 5% significance level, we are able to reject H0. 

#printing anova's summary
summary(anova1way) 

#Display structure
str(new_df_final) 


#heading
cat("Bonferroni post-hoc test","\n") 

########Bonferroni post-hoc test#########

pairwise.t.test(new_df_final$Life.Exp, new_df_final$Continent, p.adj = "bonferroni") 

#heading
cat("\n","Tukey post-hoc test","\n") 


#######Tukey post-hoc test ##########

#We can see from the R extract output (columns "diff" and "p adj") that continents have 
#considerable variances in life expectancy from one another. The confidence interval 
#produced from the Tukey post-hoc test can also be visualised as follows:
  
#By charting the "tukey" object in R, the results acquired above can be displayed. 
#Significant differences are those that do not exceed a value of zero.


new_df_final$Continent<-as.factor(new_df_final$Continent) 

#adjusting the indent in plots
par(mar=c(6,7,2,1))


tukey.continent<-TukeyHSD(anova1way) 

plot(tukey.continent,cex.axis=0.8,srt=30,las=2) 


#producing the Residuals Normality

residuals1<-anova1way$residuals 

#adjusting the indent in plots
par(mar=c(4,4,1,1)) 

#par(mfrow=c(1,2)) 

#plotting histogram for the residuals
hist(residuals1, main="Standardised residuals-histogram",xlab="Standardised residuals") 

#normality plot for the residuals
qqnorm(residuals1,pch=19) 

#drawing line on the normality plot
qqline(residuals1) 

########shapiro test for the residuals found###########
shapiro.test(residuals1) 

