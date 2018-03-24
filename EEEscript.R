# EEE Project
#  Meta-analysis

#install.packages("meta")
#library(meta)
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)

#Change your the dataset path
data1 <- read.csv("~/M2-ERNA/Spring Term/Empirical Environmental Econ/Meta-analysis/RESULTS PAPER CLIMATE AND HEALTH - R dataset.csv")
View(data1)

# estimates normalization
as.data.frame(data1)
data1[5,5] = 0.1 # if I dont have a value for all lines plot 1 doesn't work
data1[6,5] = 0.1
data1[7,5] = 0.1
data1[1,5] = 0.1
data1[1,4] = 0.1
data1[1,3] = 10

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


  data1 <- data1[,c(1,2,3,4,5,8,9,11,12,10,6,7)] # reordering the dataset


  #############################
  ###   GRAPH 1 ###

ggplot() + 
    geom_errorbar(data=data1, mapping=aes(x=data1$paper.id, ymin=data1$lower.95, ymax=data1$upper.95) + 
    geom_point(data=data1, mapping=aes(x=data1$paper.id, y=data1$beta_norm), size=4, shape=21, fill="white")) 
  
  
  
  
  #B<-B + theme_economist()