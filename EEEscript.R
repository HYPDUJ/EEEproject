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

se_norm <- vector()

se_norm <- data1$standard.error..beta.*data1$mean.mortality.rate.per.1.10.5

data1$se_norm <- se_norm

# Computation of weights

sumwyd <- sum(1/data1$se_norm, na.rm = T)

data1$wyd <- (1/data1$se_norm)/sumwyd

# Computation of confidence intervals

upper.95 <-vector()

upper.95 <- data1$beta_norm + 2*data1$se_norm

data1$upper.95 <- upper.95


lower.95 <-vector()

lower.95 <- data1$beta_norm - 2*data1$se_norm

data1$lower.95 <- lower.95

