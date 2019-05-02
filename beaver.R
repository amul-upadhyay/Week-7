
#Beaver dataset is avaialble in Rstudio
# The data contists of 4 beavers body temperature every 10 mins of the day
# We want to examine the difference in average body temp during periods of activity to evaluate wheater 
# body temp is affected by activity

str(beaver2)

# Null Hypotheses
# H0---> Temperature Does not affect beaver's activities


# Null Hypotheses
# H0---> Temperature Does not affect beaver's activities

# Active should be factor
# Temp is numerical

# Using trasform function change activ to factor

tansformed_beaver_data <- transform(beaver2, activ = factor(activ, labels = c("no", "yes")))

tansformed_beaver_data


# Before selecting appropriate test we need to check wheater data is distributed or not
# See Notes on blackboard for more info

library("lattice")


# The Histogram uses one sided formula so we dont specify anything on the left  side of ~
# and on the right side we specify which variable is in the histogram


histogram(~temp | activ, data = tansformed_beaver_data)

# Quantile-quantile plot allows us to examine if dat is distributed normally
# Compare the quantiles of both samples
# We use the square brackets to select the cases we want


with(tansformed_beaver_data, qqplot(temp[activ == "yes"],
                                    temp[activ == "no"],
                                    main = "Comparing 2 samples",
                                    xlab = "Active temp = yes",
                                    ylab = "Active temp = no"
))


# Using qqplot check for normality
# qqnorm function plots the sample against a normal distribution

with(tansformed_beaver_data, {
  qqnorm(temp[activ == "no"],
         main = "Inactive data")
  qqline(temp[activ == "no"])
})


# Formal test of normality
# Provided through the shapiro-wilks test


normality_test <- shapiro.test(tansformed_beaver_data$temp)

normality_test$p.value


# The p value tells us the chances that the sample comes from a normal distribution
# In this example, p-value is clearly lower than 0.05
# so it is not normally distributed


# We can also check the normality of each of the variables
# Using the tapply function
with(tansformed_beaver_data, tapply(temp, activ, shapiro.test))






