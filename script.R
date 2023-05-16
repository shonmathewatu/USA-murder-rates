# Practical 9

# Q1
states <- as.data.frame(state.x77)
str(states)

# Renaming Life Exp and HS Grad variables as 
# these will cause possible issues when referring to
# them since they contain a space.
colnames(states)[colnames(states) == "Life_Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS_Grad"] <- "HS_Grad"

# Q3a
# Examine initial linearity between variables in the dataset
library(psych)
pairs.panels(states,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# Examine linearity in more detail
scatter.smooth(x = states$Population,
               y = states$Murder,
               xlab = "Population (,000)",
               ylab = "Murder %", main = "Correlation of murder ~ population")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(states$Murder, states$Population)

scatter.smooth(x = states$Illiteracy,
               y = states$Murder,
               main = "Correlation of Murder ~ Illiteracy",
               xlab = "Illiteracy %",
               ylab = "Murder %")

# Examining correlation between murder and illiteracy
cor(states$Murder, states$Illiteracy)

# This is a better correlation value between both variables.
# Lets examine murder and frost variables for correlation.
scatter.smooth(x = states$Frost,
               y = states$Murder,
               main = "Correlation of Murder ~ Frost",
               xlab = "Frost",
               ylab = "Murder %")
cor(states$Murder, states$Frost)

# Examining the other variables
paste("Correlation for Murder and Frost: ", cor(states$Murder, states$Frost))
paste("Correlation for Murder and Illiteracy: ", cor(states$Murder, states$Illiteracy))
paste("Correlation for Murder and Population: ", cor(states$Murder, states$Population))
paste("Correlation for Murder and HS Grad: ", cor(states$Murder, states$HS_Grad))
paste("Correlation for Murder and Income: ", cor(states$Murder, states$Income))
paste("Correlation for Murder and Life Exp: ", cor(states$Murder, states$Life_Exp))
paste("Correlation for Murder and Area: ", cor(states$Murder, states$Area))

# It appears that the variable Area has a vary low correlation with Murder. 
# Therefore I am going to remove it from the dataset. 
# Alternatively we can choose to exclude these independent variables when
# we are constructing the MLR model.

# Q3b
# Check for outliers
opar <- par(no.readonly = TRUE)
par(mfrow = c(4, 2)) # divide graph area in 3 rows by 2 columns
attach(states)
boxplot(Murder,
        main = "Murder",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Murder)$out)) # box plot for 'murder'
boxplot(Population,
        main = "Population",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Population)$out)) # box plot for 'Population'
boxplot(states$HS_Grad,
        main = "Graduation",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$HS_Grad)$out)) # box plot for 'HS Grad'
boxplot(Illiteracy,
        main = "Illiteracy",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Illiteracy)$out)) # box plot for 'HS Grad'
boxplot(Income,
        main = "Income",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Income)$out)) # box plot for 'HS Grad'
boxplot(Frost,
        main = "Frost",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Frost)$out)) # box plot for 'HS Grad'
boxplot(states$Life_Exp,
        main = "Life Exp",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$Life_Exp)$out)) # box plot for 'HS␣Grad'
detach(states)
par(opar)

# Both the population and Income variables contain outliers.
# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(states$Population)$out # outlier values.
paste("Population outliers: ", paste(outlier_values, collapse=", "))


# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(states$Income)$out # outlier values.
paste("Income outliers: ", paste(outlier_values, collapse=", "))

#removing the outliers 
# USA murder rates 3b pdf
#"Population outliers:  21198, 11197, 18076, 11860, 12237"
# "Income outliers:  6315"
states <- subset(states, states$Population != 21198
                 & states$Population != 11197 
                 & states$Population != 18076
                 & states$Population != 11860
                 & states$Population != 12237)
states <- subset(states, states$Income != 6315 )

states

#USA murder rates 3.c pdf


install.packages("e1071")
library(e1071)

#opar <- par(no.readonly = TRUE)
#Show 4 rows x 2 cols of charts
#par(mfrow = c(4,2))
#par(opar)
plot(density(states$Population),
     main = "Density plot for population",
     sub = paste("Skewness : ", round(e1071::skewness(states$Population), 2)))

#to fill the area inside with color                 
polygon(density(states$Population), col = "red")

shapiro.test(states$Population)

states


install.packages("MASS")
library(MASS)
box_cox_transform <- boxcox(states$Murder ~ states$Population)

lambda <- box_cox_transform$x[which.max(box_cox_transform$y)]
lambda
# lambda value 0.4242424
normalised_population <- ((states$Population^lambda-1)/lambda)
hist(normalised_population)
shapiro.test(normalised_population)
#p-value = 0.1835 - so normally distributed (if p value < 0.05 it is not normally distributed)



#income
plot(density(states$Income),
     main = "Density plot for income",
     sub = paste("Skewness : ", round(e1071::skewness(states$Income), 2)))

#to fill the area inside with color                 
polygon(density(states$Income), col = "red")

shapiro.test(states$Income)


#illitracy
plot(density(states$Illiteracy),
     main = "Density plot for illitracy",
     sub = paste("Skewness : ", round(e1071::skewness(states$Illiteracy), 2)))

#to fill the area inside with color                 
polygon(density(states$Illiteracy), col = "red")

shapiro.test(states$Illiteracy)
# not normallly distributed

#life expectancy
plot(density(states$`Life Exp`),
     main = "Density plot for life exp",
     sub = paste("Skewness : ", round(e1071::skewness(states$`Life Exp`), 2)))

#to fill the area inside with color                 
polygon(density(states$`Life Exp`), col = "red")

shapiro.test(states$`Life Exp`)
# normally distributed

#murder
plot(density(states$Murder),
     main = "Density plot for Murder",
     sub = paste("Skewness : ", round(e1071::skewness(states$Murder), 2)))

#to fill the area inside with color                 
polygon(density(states$Murder), col = "red")

shapiro.test(states$Murder)
# normally distributed

#HS grad
plot(density(states$`HS Grad`),
     main = "Density plot for HS Grad",
     sub = paste("Skewness : ", round(e1071::skewness(states$`HS Grad`), 2)))

#to fill the area inside with color                 
polygon(density(states$`HS Grad`), col = "red")

shapiro.test(states$`HS Grad`)
#not normally distributed

#Frost
plot(density(states$Frost),
     main = "Density plot for frost",
     sub = paste("Skewness : ", round(e1071::skewness(states$Frost), 2)))

#to fill the area inside with color                 
polygon(density(states$Frost), col = "red")

shapiro.test(states$Frost)
#normally distributed

#area
plot(density(states$Area),
     main = "Density plot for area",
     sub = paste("Skewness : ", round(e1071::skewness(states$Area), 2)))

#to fill the area inside with color                 
polygon(density(states$Area), col = "red")

shapiro.test(states$Area)
#normally distributed


box_cox_transform <- boxcox(states$Murder ~ states$Population)

lambda <- box_cox_transform$x[which.max(box_cox_transform$y)]
lambda
# lambda value 0.4242424
normalised_illitracy <- ((states$Population^lambda-1)/lambda)
hist(normalised_illitracy)
shapiro.test(normalised_illitracy)
#p-value = 0.1835 - so normally distributed (if p value < 0.05 it is not normally distributed)



