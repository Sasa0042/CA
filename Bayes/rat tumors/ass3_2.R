
# 2 -----------------------------------------------------------------------

# (1)  --------------------------------------------------------------------

## sample a dataset

library('wiqid')

set.seed(517)
y <- rep(0,10)

x <- runif(10,)
n <- rep(0,10)
for(j in 1:10){
  while(n[j] == 0){
    n[j] <- rpois(1,5)
  }
}

alpha <- rt3(1,0,2,4)
beta <- rt3(1,0,1,4)
  
for(j in 1:10){
  theta <- exp(alpha+beta*x[j])/(1+exp(alpha+beta*x[j]))
  y[j] <- rbinom(1,n[j],theta)
}


# (2) ---------------------------------------------------------------------

## compute the joint posterior distribution at a grid of points (α,β)

q <- function(alpha, beta){
  p <- dt3(alpha,0,2,4) * dt3(beta,0,1,4)
  for(j in 1:10){
    theta <- exp(alpha+beta*x[j])/(1+exp(alpha+beta*x[j]))
    p <- p * dbinom(y[j],n[j],theta)
  }
  return(p)
}

alpha <- seq(-6,3,length = 900)
beta <- seq(-6,3,length = 900)

z <- outer(alpha, beta, q)
contour(alpha, beta, z)

###  alpha: 0; beta: -1  var: (1, 4); cov: -1


## choose normal distribution as proposal; determine M

library('mnormt')

set.seed(517)
sample_size <- 10000
ratio <- rep(0,sample_size)

mu <- c(0, -1)
sigma <- matrix(c(1,-1,-1,4), ncol= 2)
  
for(i in 1:sample_size){
  s <-  rmnorm(1,mu,sigma)
  g <-  dmnorm(s,mu,sigma) 
  ratio[i] <- q(s[1],s[2])/g
}

hist(ratio)
max(ratio)  ## M = 8.304113e-07


## do rejection sampling

set.seed(517)
sample_size <- 10000
sample <- matrix(0,sample_size,2)
k <- 0

for(i in 1:sample_size){
  s <-  rmnorm(1,mu,sigma)
  g <-  dmnorm(s,mu,sigma) 
  ratio <- q(s[1],s[2])/(g*8.4e-07)
  
  if(ratio < 1){
    if(runif(1) < ratio){
      k <- k+1
      sample[k,] <- s
    }
  }else{
    print('ratio larger than 1')
    print(ratio)
  }
}

sample <- sample[1:k, ]
sample <- sample[1:1000, ]



# (3) ---------------------------------------------------------------------


## determine mean and var

library('dplyr')

den <- density(sample[,1])
denx=den$x
deny=den$y
den %>% plot(lwd = 2, col = "dodgerblue2")
denx[which.max(deny)] %>% abline(v = ., lwd = 2, col = "red3")
grid()
denx[which.max(deny)]  ## -0.04984014


den <- density(sample[,2])
denx=den$x
deny=den$y
den %>% plot(lwd = 2, col = "dodgerblue2")
denx[which.max(deny)] %>% abline(v = ., lwd = 2, col = "red3")
grid()
denx[which.max(deny)]  ## -1.134289

var(sample)


## draw plots

library(ggplot2)

ggplot(as.data.frame(sample), aes(x = sample[,1], y = sample[,2])) +
  geom_point(col = 'pink') +
  geom_density_2d(aes(colour = after_stat(level)))


mu <- c(-0.05, -1.13)
sigma <- var(sample)
sample2 <- rmnorm(1000, mu, sigma)

ggplot(as.data.frame(sample2), aes(x = sample2[,1], y = sample2[,2])) +
  geom_point(col = 'pink') +
  geom_density_2d(aes(colour = after_stat(level)))



# (4) ---------------------------------------------------------------------


### compute alpha+beta;  alpha-beta  posterior mean
library('mvtnorm')

sample_size <- 1000
w <- rep(0,sample_size)
aw <- rep(0,sample_size)
mw <- rep(0,sample_size)

for(i in 1:sample_size){
  s <- rmvt(1,df = 4, delta = mu, sigma = sigma)
  g <- dmvt(s,df = 4, delta = mu, sigma = sigma)
    
  w[i] <- q(s[1], s[2])/g
  aw[i] <- (s[1]+s[2])*w[i]
  mw[i] <- (s[1]-s[2])*w[i]
}

add <- sum(aw)/sum(w)
minus <- sum(mw)/sum(w)

(add+minus) / 2
(add-minus) / 2



# (5) ---------------------------------------------------------------------

# ESS

w_n <- rep(0,sample_size)
for (i in 1:sample_size) {
  w_n[i] <- (w[i]/sum(w))^2
}

1/sum(w_n)




