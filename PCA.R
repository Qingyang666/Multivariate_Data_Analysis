
#############################################
# Name: Qingyang Li                         #
# Topic: Principal Component Analysis       #
#############################################

library(dplyr)
library(ggfortify)
library(ggplot2)

# Subject-matter researchers are interested in grouping goblets that have the same shape although they may have different sizes.
# One way suggested by Manly(1994) to adjust the data is to divide each measurement by X3 (height).

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

# check whether we need to do standardization
round(apply(dat[,-1],2,sd),3)
# since the sd(w1) is almost three times of sd(w6), we need to standardize data.

# standardization
dat <- dat %>%
  mutate_at(c("w1","w2", "w4", "w5", "w6"),scale)

round(apply(dat[,-1],2,sd),3)

################################################################################

# using princomp() command to do PCA
# PCA with correlation matrix
pc.dat <- princomp(x = dat[,-1], cor = TRUE, scores = TRUE)
summary(pc.dat, loadings = TRUE, cutoff = 0.0)
# Notice:
# The standard deviation in the table of "Importance of components" shows us the square root of the eigenvalues in the correlation matrix.
# The proportion of variance in the table of "Importance of components" shows us the importance of the jth principal component.
# In the table of "Loadings", the eigenvectors of the corresponding eigenvalues are shown. 

################################################################################

# using eigen function to manually compute principal component
eg_cor <- eigen(cor(dat[,-1]))

# standard deviation
sqrt(eg_cor$values)

# proportion of variance
prop_var <- c()
for(i in 1:5){
  prop_var[i] <- eg_cor$values[i]/sum(eg_cor$values)
}
print(prop_var)

# loadings
round(eg_cor$vectors,3)

################################################################################

# First Method: scree plot
tibble(PC = paste0("PC",1:5),
       prop_var) %>%
  ggplot(aes(x = PC, y = prop_var, group = 1))+
  geom_point(size = 2)+
  geom_col(fill = "steelblue")+
  geom_line()+
  labs(title="Scree Plot",
       x="Principal Component", y = "Proportion of Varinace")+
  theme(plot.title = element_text(hjust = 0.5))
# Notice:
# By looking at the scree plot, the plot levels off close to 0 when the principal component is equal to 3. 
# It indicates the corresponding principal component are not contributing too much information to understand the data.

# Second Method: find the number of eigenvalues greater than 1
cat('The number of eignenvalues that are greater than 1 is', length(which(eg_cor$values>1)))

################################################################################

# Interpretation:
# By looking at principal component 1, it is a measure of the overall size of the goblet with not as much weight given to w6. 
# w1 and w2 measure the top of the goblet. w4 and w6 measure the base of the goblet. w5 is quite close to the zero. 
# Therefore, principal component 2 are contrasting the top of the goblet to the measurements in the base.

################################################################################
# PC scores:

# first method
head(pc.dat$scores)

# second method
head(predict(pc.dat))

# PC scores manually method
pc.score <- matrix(data = NA, nrow = nrow(dat) , ncol = 5)
for(j in 1:5){
  for(i in 1:nrow(dat)){
    pc.score[i,j] <- t(matrix(pc.dat$loadings[,j])) %*% matrix(scale(dat)[i,-1])
  }
}
head(pc.score)

# why manually compute PC scores are different with the first and the second method?
# Explanation will be in a new pdf file.

# Here is the way to get the right PC scores
pc.dat$scale <- apply(X = dat[,-1], MARGIN = 2, FUN = sd )
pc.score2 <- predict(pc.dat, newdata = dat)
head(pc.score2)
################################################################################

# scatter plot for first two principal components
ggplot(data = data.frame(pc.score2),
       aes(x = Comp.1, y = Comp.2, label = dat$ID))+
  geom_point()+ 
  geom_text(size=3,hjust = 0, nudge_x = 0.05)+
  geom_hline(yintercept=0,color="red", linetype="dashed")+
  geom_vline(xintercept=0,color="red", linetype="dashed")+
  labs(title="PC Scores",
       x="PC1", y = "PC2")+
  theme(plot.title = element_text(hjust = 0.5))





         