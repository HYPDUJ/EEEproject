# EEE Project
#  Meta-analysis

install.packages("meta")
library(meta)

#Change your the dataset path
data1 <- read.csv("~/M2-ERNA/Spring Term/Empirical Environmental Econ/Meta-analysis/RESULTS PAPER CLIMATE AND HEALTH - R dataset.csv")
View(data1)

# estimates normalization
as.data.frame(data1)

beta_norm <-vector()

beta_norm <- data1$beta*data1$mean.mortality.rate.per.1.10.5 # Gives the average number of extra deaths due to temperature changes

data1$beta_norm <- beta_norm



#standard error normalization

sd_norm <- vector()

sd_norm <- data1$standard.error..beta.*data1$mean.mortality.rate.per.1.10.5

data1$sd_norm <- sd_norm

# Computation of weights

sumwyd <- sum(1/data1$sd_norm, na.rm = T)
data1$wyd <- (1/data1$sd_norm)/sumwyd

