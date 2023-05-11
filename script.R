install.packages("state.x77")

#preperocessing
str(state.x77)
class(state.x77)

#converting matrix array to df

states <- as.data.frame(state.x77)


str(states)

#murders will be the dependet variable
help("state.x77")

pairs(states)


attach(states)


scatter.smooth(x = Population,
               y = Murder,
               main = "Murder vs Population",
               xlab = "Population",
               ylab = "Murder")

cor(Murder,Population)

# -0.2< x < 0.2 then most of ther esponse os the var
# s not predicted by the predictor variable

boxplot(Population,
        main = "poplulation",
        sub = paste("outlier rows:", boxplot.stats(Population))

        
        
        
