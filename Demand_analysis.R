#  Demand_analysis.R
################################################################################################
library(s20x)
library(car)
#read the dataset from an existing .csv file
df <- read.csv("C:/Users/Mitch/Documents/R/Data/SISfake.csv",header=T)
#list the name of each variable (data column) and the first six rows of the dataset
head(df)
# basic statistics of the variables
summary(df)
################################################################################################
par(mfrow = c(1,2))

# boxplot to check if there are outliers
boxplot(df$sales,horizontal = TRUE, xlab = 'sales')
# histogram to explore the data distribution shape
hist(df$sales,main=' ',xlab= 'sales',prob=T)
lines(density(df$sales),lty= 'dashed',lwd=2.5,col='red')

# Note there appeas to be an outlier at 400 sales
# LOF (density) approach
library(DMwR)
myvars <- c("sales", "price_LED", "price_CFL_Stand", "price_CFL_Stand")
newdata <- df[myvars]

outlier.scores <- lofactor(newdata, k=5)
plot(density(outlier.scores))

# pick top 5 as outliers
outliers <- order(outlier.scores, decreasing=T)[1:5]
# who are outliers
print(outliers)
df[21,]
df[5,]
#Visualize outliers
n <- nrow(df)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(df), cex=.8, xlabs=labels)
################################################################################################
#divide the dataset into two sub dataset by ad_type
sales_ad_none = subset(df,ad_type==0)
sales_ad_promo = subset(df,ad_type==1)

#calculate the mean of sales with different ad_type
mean(sales_ad_none$sales)
mean(sales_ad_promo$sales)

par(mfrow = c(1,2))
# histogram to explore the data distribution shapes
hist(sales_ad_none$sales,main=' ',xlab='sales with no promortion',prob=T)
lines(density(sales_ad_nature$sales),lty='dashed',lwd=2.5,col='red')

hist(sales_ad_promo$sales,main=' ',xlab='sales with in store promotion',prob=T)
lines(density(sales_ad_family$sales),lty='dashed',lwd=2.5,col='red')
#  Shapiro-Wilk test (https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test)
shapiro.test(sales_ad_none$sales)
shapiro.test(sales_ad_promo$sales)

t.test(sales_ad_none$sales,sales_ad_promo$sales)
################################################################################################
#correlation coefficients in pairs
pairs(df,col='blue',pch=20)
pairs20x(df)

################################################################################################
#Model specifications
################################################################################################
require(plm)        # load panel data package

# convert the data set into a pdata.frame by identifying the
# individual ("store") and time ("month") variables in our data

bulb.pd = pdata.frame(df, index = c("store", "month"),
                      drop.index = TRUE, row.names = TRUE)


#A Pooled OLS Regression
reg1.pool <-  plm( log(sales) ~ log(price_LED) + ad_type + retail_DIY + log(price_CFL_Stand) + 
                     log(price_CFL_Spec) , data=bulb.pd, model = "pooling")
summary(reg1.pool)

#The Breusch-Pagan test for Heteroskedasticity
require(lmtest)
bptest(reg1.pool)

# plotting the residuals vs. other key model metrics


###################################################
# Fixed Effects Regression

reg2.fe <-  plm( log(sales) ~ log(price_LED) + ad_type + retail_DIY + log(price_CFL_Stand) + 
                     log(price_CFL_Spec) , data=bulb.pd, model = "within")
summary(reg2.fe)
# CoMPARE MODELS
pFtest(reg2.fe,reg1.pool)

###################################################
#Random Effects Estimator

reg3.re <-  plm( log(sales) ~ log(price_LED) + ad_type + retail_DIY + log(price_CFL_Stand) + 
                   log(price_CFL_Spec) , data=bulb.pd, model = "random")
summary(reg3.re)

# The Hausman test 
phtest(reg2.fe,reg3.re)
