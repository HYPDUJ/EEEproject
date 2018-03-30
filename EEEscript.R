# EEE Project
#  Meta-analysis

#install.packages("meta")
library(meta)
library(gplots)
library(snowfall)
library(ggplot2)
library(ggthemes)

#Change your dataset path
setwd("C:/Users/victo/OneDrive/Documents/TSE MASTER 2/semestre 2/EMPIRICAL ENVIRONMENTAL ECONNOMICS")
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


data1 <- data1[,c(1,2,3,4,5,8,9,11,12,13,10,6,7)] # reordering the dataset




#computation of the weighted sum estimate
beta_norm_bar <- weighted.mean(data1$beta_norm,data1$wyd, na.rm = T) #weighted sum of coef estimates
se_norm_bar <- sqrt(sum(data1$se_norm^2, na.rm = T)/length(data1$se_norm)) #average se_norm

upper.95 <- beta_norm_bar + 2*se_norm_bar
lower.95 <- beta_norm_bar - 2*se_norm_bar

df <- data.frame(NA,"Bbar",NA,NA,NA,NA,beta_norm_bar,NA,upper.95,lower.95,NA,NA,NA)
names(df) <- names(data1)

data1 <- rbind(data1,df)


#############################
###   GRAPH 1 ###


A <- ggplot() + 
  geom_errorbar(data=data1, mapping=aes(x=data1$Author.s.initials, ymin=data1$lower.95, ymax=data1$upper.95), width=0.3, size=0.5, color="blue") + 
  geom_point(data=data1, mapping=aes(x=data1$Author.s.initials, y=data1$beta_norm), size=3, shape=21, fill="blue") +
  geom_errorbar(data=data1, mapping=aes(x="Bbar", ymin=data1[which(data1$paper.id=="Bbar"),10], ymax=data1[which(data1$paper.id=="Bbar"),9]), width=0.4, size=0.5, color="red")  +
  geom_point(data=data1, mapping=aes(x="Bbar", y=data1[which(data1$paper.id=="Bbar"),7]), size=4, shape=21, fill="red") +
  labs(title="Forest Plot", subtitle="Effect of one extra degree-day on mortality", x="", y="Average number of deceased per 100 000 p.")
A <- A + theme_economist()
A  

###   GRAPH 2 ###

#computation of the weighted sum estimate (without outlier)
data2 <- data1[data1$paper.id!="Bbar",]
newdata<-data2[data1$Author.s.initials!="D", ] # Outlier

beta_norm_bar <- weighted.mean(newdata$beta_norm,newdata$wyd, na.rm = T) #weighted sum of coef estimates
se_norm_bar <- sqrt(sum((newdata$se_norm*newdata$wyd)^2, na.rm = T)/length(newdata$se_norm)) #average se_norm

upper.95 <- beta_norm_bar + 2*se_norm_bar
lower.95 <- beta_norm_bar - 2*se_norm_bar

df2 <- data.frame(NA,"Bbar",NA,NA,NA,NA,beta_norm_bar,NA,upper.95,lower.95,NA,NA,NA)
names(df2) <- names(newdata)

newdata <- rbind(newdata,df2)

View(newdata)
B <- ggplot() + 
  geom_errorbar(data=newdata, mapping=aes(x=newdata$Author.s.initials, ymin=newdata$lower.95, ymax=newdata$upper.95), width=0.3, size=0.5, color="blue") + 
  geom_point(data=newdata, mapping=aes(x=newdata$Author.s.initials, y=newdata$beta_norm), size=3, shape=21, fill="blue") +
  geom_errorbar(data=newdata, mapping=aes(x="Bbar", ymin=newdata[which(newdata$paper.id=="Bbar"),10], ymax=newdata[which(newdata$paper.id=="Bbar"),9]), width=0.4, size=0.5, color="red")  +
  geom_point(data=newdata, mapping=aes(x="Bbar", y=newdata[which(newdata$paper.id=="Bbar"),7]), size=4, shape=21, fill="red") +
  labs(title="Forest Plot", subtitle="Effect of one extra degree-day on mortality", x="", y="Average number of deceased per 100 000 p.", caption="(Taking out the outlier, 'Bbar' corresponds to the weighted sum of est.)")

B<-B+theme_economist()
B 


###   funnel plot ### 
#install.packages(metafor)
library(metafor)


### fit fixed-effects model
res <- rma(data2$beta_norm,1/data2$se_norm , data=data2, measure="OR", method="FE")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

funnel(data2$beta_norm,1/data2$se_norm ,main="Inverse Standard Error")
