
### 3

# (b) ---------------------------------------------------------------------


t1 <- seq(-1.75,0.25, length=200)
t2 <- seq(0,2.5, length=250)
n <- c(58,90,48,57,103,57,86,112,273,64)
y <- c(16,9,10,13,19,20,18,17,35,55)

p <- function(t1,t2){
  a <- exp(t1+t2)/(1+exp(t1))
  b <- exp(t2)/(1+exp(t1))

  p <- (10^196)*a*b*(a+b)^(-2.5)
  for (i in 1:10) {
    p <- p*beta(a+y[i], b+n[i]-y[i])/beta(a,b)
  }
  return(p)
}

p <- Vectorize(p)
z <- outer(t1,t2,p)
contour(t1,t2,z)


## draw t1, t2 simulations:

N = 1000
f_t1 = rowSums(z)
row = sample(1:200, N, replace=T, prob = f_t1)
t1_sample = t1[row]

t2_sample = numeric(N)
for (i in 1:N){
  col = sample(1:250, 1, prob=z[row[i], ])
  t2_sample[i] = t2[col]
}


library(ggplot2)
sample <- as.data.frame(cbind(t1_sample,t2_sample))

ggplot(sample, aes(x = sample[,1], y = sample[,2])) +
  geom_point(col = 'pink') +
  geom_density_2d(aes(colour = after_stat(level)))


## transform to (α, β) scale:

alpha_sample <- exp(t1_sample+t2_sample)/(1+exp(t1_sample))
beta_sample <- exp(t2_sample)/(1+exp(t1_sample))

## sample θj from its conditional posterior distribution:
theta_sample <- matrix(rep(0,10*1000), ncol=10)

for (j in 1:10) {
  for (i in 1:1000) {
    theta_sample[i,j] <- rbeta(1,alpha_sample[i]+y[j], beta_sample[i]+n[j]-y[j])
  }
}



# (c) ---------------------------------------------------------------------

data <- as.data.frame(c(y/n))
colnames(data) <- 'raw_proportions'
data$mode <- rep(0,10)
data$median <- rep(0,10)
data$upper <- rep(0,10)
data$lower <- rep(0,10)

for (i in 1:10) {
  den <- density(theta_sample[,i])
  denx=den$x
  deny=den$y
  den %>% plot(lwd = 2, col = "dodgerblue2")
  denx[which.max(deny)] %>% abline(v = ., lwd = 2, col = "red3")
  grid()
  
  data$mode[i] <- denx[which.max(deny)]
}

for(i in 1:10){
  data$upper[i] <- sort(theta_sample[,i])[50]
  data$median[i] <- sort(theta_sample[,i])[500]
  data$lower[i] <- sort(theta_sample[,i])[950]
}

ggplot(data, aes(x = raw_proportions)) +
  geom_point(aes(y = mode), col='#69b3a2', )+
  geom_point(aes(y = median), col='#B33F2D')+
  geom_abline(slope= 1, intercept= 0 )+
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.2)+
  scale_color_manual(name = 'the colour', 
                      values = c('#69b3a2'='#69b3a2','#B33F2D'='#B33F2D'), 
                      labels = c('mode','median'))  













