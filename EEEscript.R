# EEE Project
#  Meta-analysis

#install.packages("meta")
#library(meta)
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)

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

sumsigmaB_ys <- sum(1/data1$se_norm, na.rm = T)

data1$wyd <- (1/data1$se_norm)/sumsigmaB_ys

# Computation of confidence intervals

upper.95 <-vector()

upper.95 <- data1$beta_norm + 2*data1$se_norm

data1$upper.95 <- upper.95


lower.95 <-vector()

lower.95 <- data1$beta_norm - 2*data1$se_norm

data1$lower.95 <- lower.95


  data1 <- data1[,c(1,2,3,4,5,8,9,11,12,10,6,7)] # reordering the dataset



  
#computation of the weighted sum estimate
beta_norm_bar <- weighted.mean(data1$beta_norm,data1$wyd, na.rm = T) #weighted sum of coef estimates
se_norm_bar <- sqrt(sum(data1$se_norm^2, na.rm = T)) #average se_norm

upper.95 <- beta_norm_bar + 2*se_norm_bar
lower.95 <- beta_norm_bar - 2*se_norm_bar

df <- data.frame("Bbar",NA,NA,NA,NA,beta_norm_bar,se_norm_bar,upper.95,lower.95,NA,NA,NA)
names(df) <- names(data1)

data1 <- rbind(data1,df)
  

  #############################
  ###   GRAPH 1 ###
  
data1 <- data1[order(beta_norm),] #sorting betas in increasing order

A <- ggplot() + 
    geom_errorbar(data=data1, mapping=aes(x=data1$paper.id, ymin=data1$lower.95, ymax=data1$upper.95), width=0.3, size=0.5, color="blue") + 
    geom_point(data=data1, mapping=aes(x=data1$paper.id, y=data1$beta_norm), size=3, shape=21, fill="blue") +
    geom_errorbar(data=data1, mapping=aes(x="Bbar", ymin=data1[which(data1$paper.id=="Bbar"),9], ymax=data1[which(data1$paper.id=="Bbar"),8]), width=0.4, size=0.5, color="red")  +
    geom_point(data=data1, mapping=aes(x="Bbar", y=data1[which(data1$paper.id=="Bbar"),6]), size=4, shape=21, fill="red") 

A <- A + theme_economist()  
A  