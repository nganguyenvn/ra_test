library(foreign)
library(plm)
data <- read.csv("C:/Users/LongNga/Downloads/ra_test_data.csv")
View(data)

data$did=data$trt*data$post
did_model <- lm(y ~ trt + post + did,data=data)
summary(did_model)

install.packages("ggplot2")
library(ggplot2)
residuals_df <- data.frame(Residuals=did_model$residuals,FV=did_model$fitted.values)
ggplot(residuals_df,aes(x=FV, y=Residuals))+
  geom_point()+
  xlab("Fitted Values")+
  ylab("Residuals")

ggplot(residuals_df, aes(sample=Residuals))+
  stat_qq()+
  stat_qq_line()+
  xlab("Theoretical Quantiles")+
  ylab("Standardized REsiduals")

scale <- data.frame(Sqrt=sqrt(abs(did_model$residuals)),FV=did_model$fitted.values)

ggplot(scale,aes(x=FV,y=Sqrt))+
  geom_point()+
  xlab("Fitted Values")
  ylab("Square Root of Standardized Residuals")
  
res <- data.frame(Time=data$period,Residuals=did_model$residuals)
ggplot(res,aes(x=Time, y = Residuals))+
  geom_point()+
  xlab("Time")
  ylab("Residuals")
  
B = 1000 #Number of bootstrap repetitions
coef_estimates <- matrix(NA,nrow=B,ncol=length(coef(did_model)))

for (b in 1:B) {
  data_sample <- data[sample(nrow(data),replace=TRUE),]
  model_sample <- lm(y ~ trt + post + did,data=data_sample)
  coef_estimates[b,]<-coef(model_sample)
}

bootstrap_se <- apply(coef_estimates,2,sd)