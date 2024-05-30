
set.seed(47)

### plot1

L=100
alpha = seq(-3,7,length=L)
beta = seq(-3, 37, length=L)

library(mnormt)
mu <- c(0, 10)
sigma <- matrix(c(4, 10, 10, 100), nrow= 2 )
f1 <- function(x,y) dmnorm(c(x,y), mu, sigma)


f = function(alpha, beta){
  x = c(-0.86, -0.30, -0.05, 0.73)
  y = c(0, 1, 3, 5)
  f = prod(choose(5,y)*(exp(alpha+beta*x)^y)/
             ((1+exp(alpha+beta*x))^5))*f1(alpha,beta)
}
f = Vectorize(f)

z = outer(alpha, beta, f)
contour(alpha, beta, z, xlab='alpha', ylab='beta')



### plot2

N = 1000

f_alpha = rowSums(z)
row = sample(1:L, N, replace=T, prob = f_alpha)
alpha_sample = alpha[row]

beta_sample = numeric(N)
for (i in 1:N){
  col = sample(1:L, 1, prob=z[row[i], ])
  beta_sample[i] = beta[col]
}


for (i in 1:N){
  alpha_sample[i] = alpha_sample[i] + runif(1,-0.05,0.05)
  beta_sample[i] = beta_sample[i] + runif(1,-0.2, 0.2)
}

plot(alpha_sample, beta_sample, pch=20, xlim=c(-3,7), 
     ylim=c(-3,37),xlab='alpha', ylab='beta')

xi <- -alpha_sample/beta_sample
n <- length(xi)
s <- sd(xi)
hstars <- 3.491*s*n^{-1/3}
nobreaks <- (max(xi)-min(xi))/hstars
h1 <- hist(xi, breaks=round(nobreaks))
xi_1 <- xi



### (2)

L=100
alpha = seq(-8,8,length=L)
beta = seq(-15, 40, length=L)

### 先验

library(mnormt)

mu <- c(0, 10)
sigma <- matrix(c(4, 10, 10, 100), nrow= 2 )
f1 <- function(x,y) dmnorm(c(x,y), mu, sigma)
f = Vectorize(f1)

z2 = outer(alpha, beta, f)
contour(alpha, beta, z2, xlab='alpha', ylab='beta', col = '#64DB8F')

### 似然

x = c(-0.86, -0.30, -0.05, 0.73)
y = c(0, 1, 3, 5)

f = function(alpha, beta){
  f = prod(choose(5,y)*(exp(alpha+beta*x)^y)/((1+exp(alpha+beta*x))^5))
}
f = Vectorize(f)

z1 = outer(alpha, beta, f)
contour(alpha, beta, z1, xlab='alpha', ylab='beta', col='#FD9A28', add=T)

### 后验

f = function(alpha, beta){
  f = prod(choose(5,y)*(exp(alpha+beta*x)^y)/
             ((1+exp(alpha+beta*x))^5))*f1(alpha,beta)
}
f = Vectorize(f)

z = outer(alpha, beta, f)
contour(alpha, beta, z, xlab='alpha', ylab='beta',add=T)

legend("top",                                        #图例位置为上方
       legend=c('likelihood', 'prior','poeterior'), 
       ncol=3, 
       cex=0.8, 
       bty="n", 
       col=c('#64DB8F','#FD9A28','black'), 
       lty=1,lwd=2) 




### (3) LD50

set.seed(47)

### uniform prior

L=100
alpha = seq(-3,7,length=L)
beta = seq(-3, 37, length=L)

f = function(alpha, beta){
  x = c(-0.86, -0.30, -0.05, 0.73)
  y = c(0, 1, 3, 5)
  f = prod(choose(5,y)*(exp(alpha+beta*x)^y)/
             ((1+exp(alpha+beta*x))^5))
}
f = Vectorize(f)

z = outer(alpha, beta, f)
contour(alpha, beta, z, xlab='alpha', ylab='beta')

N = 1000

f_alpha = rowSums(z)
row = sample(1:L, N, replace=T, prob = f_alpha)
alpha_sample = alpha[row]

beta_sample = numeric(N)
for (i in 1:N){
  col = sample(1:L, 1, prob=z[row[i], ])
  beta_sample[i] = beta[col]
}


for (i in 1:N){
  alpha_sample[i] = alpha_sample[i] + runif(1,-0.05,0.05)
  beta_sample[i] = beta_sample[i] + runif(1,-0.2, 0.2)
}

xi <- -alpha_sample/beta_sample
n <- length(xi)
s <- sd(xi)
hstars <- 3.491*s*n^{-1/3}
nobreaks <- (max(xi)-min(xi))/hstars
h2 <- hist(xi, breaks=round(nobreaks))
xi_2 <- xi


plot( h1, col = rgb(0,0,1,0.2), ylim = c(0,350), xlim=c(-0.5,0.5))
plot( h2, col=rgb(1,0,0,0.2), add=T, xlim=c(-0.5,0.5))  

legend("top",                                        #图例位置为上方
       legend=c('normal prior', 'uniform prior'), 
       ncol=2, 
       cex=0.8, 
       bty="n", 
       col=c(rgb(0,0,1,0.2),rgb(1,0,0,0.2)), 
       lty=1,lwd=2)  


plot(density(xi_1),lwd=2, col=rgb(0,0,1,0.2))
lines(density(xi_2),lwd=2, col=rgb(1,0,0,0.2))

legend("top",                                        #图例位置为上方
       legend=c('normal prior', 'uniform prior'), 
       ncol=2, 
       cex=0.8, 
       bty="n", 
       col=c(rgb(0,0,1,0.2),rgb(1,0,0,0.2)), 
       lty=1,lwd=2)       

