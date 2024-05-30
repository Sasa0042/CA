
### distance discriminant

library(liver)
data("cereal")

data <- cereal[,c(2,4:11)]
data <- data[order(data$manuf),]
data_1 <- data[(data$manuf == 'G'), ]
data_2 <- data[(data$manuf == 'K'), ]
data_3 <- data[(data$manuf == 'Q'), ]
data_1 <- data_1[-c(2,5,6,8,19),-1]
data_2 <- data_2[-c(2,13,20),-1]
data_3 <- data_3[-c(1,8),-1]

colnames(data_1) <- c('x1','x2','x3','x4','x5','x6','x7','x8')
colnames(data_2) <- c('x1','x2','x3','x4','x5','x6','x7','x8')
colnames(data_3) <- c('x1','x2','x3','x4','x5','x6','x7','x8')

u1 <- colMeans(data_1)
u2 <- colMeans(data_2)
u3 <- colMeans(data_3)
sigma <- (16*var(data_1) + 19*var(data_2) + 5*var(data_3))/40

data_test <- rbind(data_1, data_2, data_3)
data_result <- rep(0,43)

for (i in 1:43){
  x <- as.vector(t(data_test[i,]))
  d1 <- t(x-u1) %*% solve(sigma) %*% (x-u1)
  d2 <- t(x-u2) %*% solve(sigma) %*% (x-u2)
  d3 <- t(x-u3) %*% solve(sigma) %*% (x-u3)
  if (d1 == min(d1,d2,d3)){
    data_result[i] = '1'
  }else if (d2 == min(d1,d2,d3)){
    data_result[i] = '2'
  }else{
    data_result[i] = '3'
  }
}
data_result



### (1) Fisher discrimination:

library(liver)
data("cereal")

data <- cereal[,c(2,4:11)]
data <- data[order(data$manuf),]
data_1 <- data[(data$manuf == 'G'), ]
data_2 <- data[(data$manuf == 'K'), ]
data_3 <- data[(data$manuf == 'Q'), ]
data_1 <- data_1[-c(2,4,5,8,19),-1]
data_2 <- data_2[-c(2,13,20),-1]
data_3 <- data_3[-c(1,7),-1]
data_3[6,c(6,7)] <- c(1,1)

data_test <- rbind(data_1, data_2, data_3)


xbar <- colMeans(data_test)
x1bar <- colMeans(data_1)
x2bar <- colMeans(data_2)
x3bar <- colMeans(data_3)

E <- 16*var(data_1) + 19*var(data_2) + 5*var(data_3)
B <- 17*(x1bar - xbar)%*%t(x1bar - xbar) + 20*(x2bar - xbar)%*%t(x2bar - xbar) + 
      6*(x3bar - xbar)%*%t(x3bar - xbar)

result <- eigen(solve(E) %*% B)
t1 <- Re(result$vectors[,1])
t(t1) %*% E %*% t1  /40   ### constraint: t'Spt = 1
t1 <- -t1 / sqrt(0.340647)
t2 <- Re(result$vectors[,2])
t(t2) %*% E %*% t2  /40
t2 <- -t2 / sqrt(0.6938936)



y11bar <- t(t1) %*% (x1bar - xbar)
y12bar <- t(t2) %*% (x1bar - xbar)

y21bar <- t(t1) %*% (x2bar - xbar)
y22bar <- t(t2) %*% (x2bar - xbar)

y31bar <- t(t1) %*% (x3bar - xbar)
y32bar <- t(t2) %*% (x3bar - xbar)


y1 <- rep(0,43)
y2 <- rep(0,43)
obs <- rep(0,43)

for (i in 1:43){
  x <- as.vector( t(data_test[i, ]))
  y1[i] <- t(t1) %*% (x-xbar)
  y2[i] <- t(t2) %*% (x-xbar)
  
  w1 <- (y1[i] - y11bar)^2 + (y2[i] - y12bar)^2
  w2 <- (y1[i] - y21bar)^2 + (y2[i] - y22bar)^2
  w3 <- (y1[i] - y31bar)^2 + (y2[i] - y32bar)^2
  
  if (w1 == min(w1,w2,w3)){
    obs[i] = '1'
  }else if (w2 == min(w1,w2,w3)){
    obs[i] = '2'
  }else{
    obs[i] = '3'
  }
}
print(obs)


type <- c(rep('1', 17), rep('2',20), rep('3',6))
draw_data <- as.data.frame(cbind(y1,y2,type))

plot(draw_data$y1, draw_data$y2, col=draw_data$type, pch=16)







