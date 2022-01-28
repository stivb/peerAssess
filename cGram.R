#library(GGally)

# Create data 
data <- data.frame( var1 = 1:100 + rnorm(100,sd=20), v2 = 1:100 + rnorm(100,sd=27), v3 = rep(1, 100) + rnorm(100, sd = 1)) 
data$v4 = data$var1 ** 2 
data$v5 = -(data$var1 ** 2) 

# Check correlation between variables
cor(data) 

# Nice visualization of correlations
ggcorr(data, method = c("everything", "pearson"))