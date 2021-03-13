
#############################################
# Name: Qingyang Li                         #
# Topic: Simple Example on Factor Analysis  #
#############################################

library(dplyr)
library(ggplot2)

################################################################################

# read in the data
dat <- readr::read_csv("data/goblet.csv")

# data trasformation
dat <- dat %>%
  rename(ID = goblet) %>%
  mutate(w1 = x1/x3,
         w2 = x2/x3,
         w4 = x4/x3,
         w5 = x5/x3,
         w6 = x6/x3)%>%
  select(-c(2:7))

################################################################################

# determine the number of common factors we need using likelihood ratio test
p_value <- c()
for(k in 1:5){
  fa.dat <- factanal(x = dat[,-1], factors = k, rotation = "none")
  p_value[k] <- fa.dat$PVAL
}
print(round(p_value,3))

# We have an error when we try to fit a factor analysis model with more than 2 common factors.
# We need to choose 2 common factors in this case, even though the likelihood ratio test rejects the null hypothesis.
# The reason of failure for fitting a more than 2 common factors is the following.

cat('The degree of freedom when we fit a factor analysis model with 3 common factors is', ((5-3)^2-5-3)/2, 
    'so negative degree of freedom is not valid.')

################################################################################

# estimate a factor analysis model without rotation
fa.est <- factanal(x = dat[,-1], factors = 2, rotation = "none")
print(fa.est,cutof=0.0)

# "Uniquenesses" provides the estimates of specific variances.
# "Loadings" provides factor loading matrix. 
#  Factor loadings give information about the estimated correlation between the standardized variables and common factors.

# assess how good the factor analysis model is by using residual matrix.
lambda <- fa.est$loadings[,]
uniqueness <- fa.est$uniquenesses
res.mat <- round(cor(dat[,-1])-(lambda %*% t(lambda)+diag(uniqueness)),4)
sum(abs(resid.goblet)>0.1)

# Overall, we see only one correlation (between w5 and w6) that is off by more than a little bit.
# This is a good sign that our model is generally accounting for the correlation between variables.

################################################################################

# estimate a factor analysis model with rotation using varimax method
fa.est.rt <- factanal(x = dat[,-1], factors = 2, rotation = "varimax")
print(fa.est.rt,cutof=0.0)
lambda.rt <- fa.est.rt$loadings[,]

# generate plots of factor loadings without rotation and with rotation
p1 <- ggplot(data = as.data.frame(lambda),
       aes(x = Factor1, y = Factor2, label = rownames(lambda)))+
  geom_point()+
  geom_hline(yintercept=0,color="red", linetype="dashed")+
  geom_vline(xintercept=0,color="red", linetype="dashed")+
  geom_text(size=2,hjust = 0, nudge_y = 0.05)+
  labs(title="Factor loadings before 
    rotation",
       x = expression(hat(lambda)[i1]), 
       y = expression(hat(lambda)[i2]))+
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(data = as.data.frame(lambda.rt),
       aes(x = Factor1, y = Factor2, label = rownames(lambda.rt)))+
  geom_point()+
  geom_hline(yintercept=0,color="red", linetype="dashed")+
  geom_vline(xintercept=0,color="red", linetype="dashed")+
  geom_text(size=2,hjust = 0, nudge_y = 0.05)+
  labs(title="Factor loadings after 
    rotation",
       x = expression(hat(lambda)[i1]), 
       y = expression(hat(lambda)[i2]))+
  theme(plot.title = element_text(hjust = 0.5))

ggpubr::ggarrange(p1, p2, ncol=2, nrow=1)

# The final interpretation of the final common factors after rotation are:
# Factor1 is a measure of the goblet width.  
# Factor2 is a measure of the goblet base. 

################################################################################

# predict factor scores using Bartlett’s method
fa.est.rt.b <- factanal(x = dat[,-1],
                        factors = 2,
                        rotation = "varimax",
                        scores = "Bartlett")
b.score <- fa.est.rt.b$scores
head(b.score)
# predict factor scores using regression method
fa.est.rt.r <- factanal(x = dat[,-1],
                        factors = 2,
                        rotation = "varimax",
                        scores = "regression")
r.score <- fa.est.rt.r$scores
head(r.score)

# plot factor scores
ggplot(data = data.frame(b.score),
       aes(x = Factor1, y = Factor2, label = dat$ID))+
  geom_point()+ 
  geom_text(size=3,hjust = 0, nudge_x = 0.05)+
  geom_hline(yintercept=0,color="red", linetype="dashed")+
  geom_vline(xintercept=0,color="red", linetype="dashed")+
  labs(title="Factor Scores Using Bartlett's Method",
       x="Factor1", y = "Factor2")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data.frame(r.score),
       aes(x = Factor1, y = Factor2, label = dat$ID))+
  geom_point()+ 
  geom_text(size=3,hjust = 0, nudge_x = 0.05)+
  geom_hline(yintercept=0,color="red", linetype="dashed")+
  geom_vline(xintercept=0,color="red", linetype="dashed")+
  labs(title="Factor Scores Using Regression Method",
       x="Factor1", y = "Factor2")+
  theme(plot.title = element_text(hjust = 0.5))



