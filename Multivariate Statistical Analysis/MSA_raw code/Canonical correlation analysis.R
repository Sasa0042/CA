library('SMPracticals')
data(frets)

sigma11 = cov(frets[,c(1,2)])
sigma22 = cov(frets[,c(3,4)])
sigma12 = cov(frets[,c(1,2)], frets[, c(3,4)])
sigma21 = t(sigma12)

inv_sigma11 <- solve(sigma11)
inv_sigma22 <- solve(sigma22)

### a1; a2
matrix = inv_sigma11 %*% sigma12 %*% inv_sigma22 %*% sigma21 
rou1 <- sqrt(eigen(matrix)$values[1])   ### sqrt(rou) not rou
rou2 <- sqrt(eigen(matrix)$values[2])
eigenvector1 <- eigen(matrix)$vectors[,1]   
eigenvector2 <- eigen(matrix)$vectors[,2]
### constrain: a' sigma11 a = 1
sum1 <- t(eigenvector1) %*% sigma11 %*% eigenvector1 
a1 <- eigenvector1/ sqrt(sum1)
sum2 <- t(eigenvector2) %*% sigma11 %*% eigenvector2 
a2 <- eigenvector2/ sqrt(sum2)

### b1; b2
matrix = inv_sigma22 %*% sigma21 %*% inv_sigma11 %*% sigma12 
eigenvector1 <- eigen(matrix)$vectors[,1]   
eigenvector2 <- eigen(matrix)$vectors[,2]
### constrain: b' sigma11 b = 1
sum1 <- t(eigenvector1) %*% sigma11 %*% eigenvector1 
b1 <- eigenvector1/ sqrt(sum1)
sum2 <- t(eigenvector2) %*% sigma11 %*% eigenvector2 
b2 <- eigenvector2/ sqrt(sum2)

#paste(a1, c('x1','x2'), sep = "*", collapse = "+")


### Q2

#### approach 1
ax1_data <- a1[1]*frets$l1 + a1[2]*frets$b1
by1_data <- b1[1]*frets$l2 + b1[2]*frets$b2 
first_c <- as.data.frame(cbind(ax1_data,by1_data))
colnames(first_c ) <- c('first_c_1','first_c_2')
cor(first_c, frets)  

#### approach 2, but it is covariance, maybe we need first standardize x,y
A <- cbind(a1, a2)
B <- cbind(b1, b2)
cov_xu <- sigma11 %*% A
cov_xv <- sigma12 %*% B
cov_yu <- sigma21 %*% A
cov_yv <- sigma22 %*% B


# Standard transformation -------------------------------------------------

frets1 <- as.data.frame(scale(frets))

sigma11 = cov(frets1[,c(1,2)])
sigma22 = cov(frets1[,c(3,4)])
sigma12 = cov(frets1[,c(1,2)], frets1[, c(3,4)])
sigma21 = t(sigma12)

inv_sigma11 <- solve(sigma11)
inv_sigma22 <- solve(sigma22)

### a1; a2
matrix = inv_sigma11 %*% sigma12 %*% inv_sigma22 %*% sigma21 
rou1 <- sqrt(eigen(matrix)$values[1])   ### sqrt(rou) not rou
rou2 <- sqrt(eigen(matrix)$values[2])
eigenvector1 <- eigen(matrix)$vectors[,1]   
eigenvector2 <- eigen(matrix)$vectors[,2]
### constrain: a' sigma11 a = 1
sum1 <- t(eigenvector1) %*% sigma11 %*% eigenvector1 
a1.2 <- eigenvector1/ sqrt(sum1)
sum2 <- t(eigenvector2) %*% sigma11 %*% eigenvector2 
a2.2 <- eigenvector2/ sqrt(sum2)

### b1; b2
matrix = inv_sigma22 %*% sigma21 %*% inv_sigma11 %*% sigma12 
eigenvector1 <- eigen(matrix)$vectors[,1]   
eigenvector2 <- eigen(matrix)$vectors[,2]
### constrain: b' sigma11 b = 1
sum1 <- t(eigenvector1) %*% sigma11 %*% eigenvector1 
b1.2 <- eigenvector1/ sqrt(sum1)
sum2 <- t(eigenvector2) %*% sigma11 %*% eigenvector2 
b2.2 <- eigenvector2/ sqrt(sum2)

### Q2

#### approach 2, now it is correlation coef
A <- cbind(a1.2, a2.2)
B <- cbind(b1.2, b2.2)
cov_xu <- sigma11 %*% A
cov_xv <- sigma12 %*% B
cov_yu <- sigma21 %*% A
cov_yv <- sigma22 %*% B


# 验证a* 与 a 间的关系 -----------------------------------------------------------

orign <- a1[1]*frets$l1 + a1[2]*frets$b1
star <- a1.2[1]*frets1$l1 + a1.2[2]*frets1$b1

scale(orign) == star

