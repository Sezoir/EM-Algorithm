# Delete previous variables
rm(list=ls())
dev.off()

# Includes
library("ggplot2")

# EM algorithm function
# @param: x     : the data
# @param: K     : number of classes
# @param: it    : number of iterations
emAlgo<-function(x, K, it)
{
  # Initilise z_ik with a random soft allocation
  z_ik<-matrix(runif(K*length(x), 0, 1), ncol=K)
  # Normalise matrix (so sum of groups per sample = 1)
  correction<-z_ik[, 1]+ z_ik[, 2]
  z_ik[, 1]<-z_ik[, 1]/correction
  z_ik[, 2]<-z_ik[, 2]/correction
  # Initialise variables
  nk=0
  weights=0
  mean=0
  var=0
  # Iterate through the E and M steps
  for(iter in 1:it)
  {
    # Perform the M-step
    nk=colSums(z_ik)
    weights=nk/length(x)
    mean=apply(z_ik, 2, `%*%`, x)/nk
    var=colSums(z_ik*(outer(x, mean, "-")^2))/nk
    # Perform the E-step
    normal.d=t(sapply(x, dnorm, mean=mean, sd=sqrt(var)))
    z_ik=apply(normal.d*weights[col(normal.d)], 2, "/", rowSums(normal.d*weights[col(normal.d)]))
  }
  # Store in data frame
  output<-list(
    "parameters"=cbind(mean, sd=sqrt(var)),
    "z_ik"=z_ik
    )
  
  # Return Output
  return(output)
}

###########################################################
# Test function
###########################################################

# Number of samples
# n<-10

# Create data
# data1<-rnorm(n, mean=-10, sd=2)
# data2<-rnorm(n, mean=10, sd=0.3)
# x<-c(data1, data2)
# y = factor(c( rep("data1", n), rep("data2", n), rep("mixedData", 2*n) ))

# Plot data
# df <- data.frame(
#   type=y,
#   data=union(x,x)
# )
# ggplot(df, aes(x=data, color=type)) +
#   geom_density()

# Create data
x = c(4.54,1.57,1.41,1.77,1.43,0.07,0.05,4.19,-0.02,1.32)
y = factor(c(rep("data", length(x))))

# Plot data
df <- data.frame(
  type=y,
  data=x
)
ggplot(df, aes(x=data, color=type)) +
  geom_density()

# Test function
out<-emAlgo(x, 3, 10000)

# Output possible values
print(out$parameters)
print(out$z_ik)


