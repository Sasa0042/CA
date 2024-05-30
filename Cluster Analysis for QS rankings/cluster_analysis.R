
# Hierrarchical Clustering ------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  ##工作路径改为当前运行文件所在路径
getwd()

data <- read.csv('Rankings.csv')
data <- data[, c(11:19)]

data <- na.omit(data)

weight <- c(0.3,0.2, 0.15, 0.05, 0.1, 0.05, 0.05, 0.05, 0.05)

data$Overall.SCORE2 <- as.matrix(data) %*% weight / 98.885 * 100


# euclidean ---------------------------------------------------------------

data1 <- data[, -10]
data1 <- as.data.frame(scale(data1))

d1 <- dist(data1, method = 'euclidean')


hc=hclust(d1,method="single")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.7344228

hc=hclust(d1,method="complete")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.828204

hc=hclust(d1,method="average")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.8620978

hc=hclust(d1,method="centroid")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.8191313

hc=hclust(d1,method="ward.D")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.592691



# corr --------------------------------------------------------------------

full_score <- rep(100,9)
data1 <- rbind(full_score, data1)

d1 <- dist(data1, method = 'euclidean')
d <- as.matrix(d1)

dist_score <- d[-1,1]
cor(dist_score, data$Overall.SCORE2)  ## -0.9800178


# rank -----------------------------------------------------------------

rank <- as.data.frame(rank(dist_score))


# TreeAndLeaf ---------------------------------------------------------------

library("TreeAndLeaf")
library("RedeR")
library("igraph")
library("RColorBrewer")

data1 <- data[, -10]
data1 <- as.data.frame(scale(data1))

d1 <- dist(data1, method = 'euclidean')
hc=hclust(d1,method="average")


#-- Convert the 'hclust' object into a 'tree-and-leaf' object
tal <- treeAndLeaf(hc)

#--- Map attributes to the tree-and-leaf
#Note: 'refcol = 0' indicates that 'dat' rownames will be used as mapping IDs
tal <- att.mapv(g = tal, dat = data, refcol = 0)


#--- Set graph attributes using the 'att.setv' wrapper function
pal <- brewer.pal(9, "Greens")
tal <- att.setv(g = tal, from = "Academic.Reputation.Score", to = "nodeColor", 
                cols = pal, nquant = 10)
tal <- att.setv(g = tal, from = "Citations.per.Faculty.Score", to = "nodeSize",
                xlim = c(40, 120, 20), nquant = 5)

#--- Set graph attributes using 'att.addv' and 'att.adde' functions
tal <- att.addv(tal, "nodeFontSize", value = 1)
tal <- att.adde(tal, "edgeWidth", value = 10)


#--- Call RedeR application
rdp <- RedPort()
calld(rdp)
resetd(rdp)

#--- Send the tree-and-leaf to the interactive R/Java interface
addGraph(obj = rdp, g = tal, gzoom=5)

#--- Call 'relax' to fine-tune the leaf nodes
relax(rdp, p1=25, p2=200, p3=10,p4=100, p5=10, ps=TRUE)


#--- Add legends
addLegend.color(obj = rdp, tal, title = "Academic.Reputation.Score",
                position = "bottomright")
addLegend.size(obj = rdp, tal, title = "Citations.per.Faculty.Score")



# gower ---------------------------------------------------------------

library('cluster')

data1 <- data[, -10]
data1 <- as.data.frame(scale(data1))

weight <- c(0.3,0.2, 0.15, 0.05, 0.1, 0.05, 0.05, 0.05, 0.05)
d1 <- daisy(data1, metric = "gower", weights = weight)

hc=hclust(d1,method="single")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.7396017

hc=hclust(d1,method="complete")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.8314316

hc=hclust(d1,method="average")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.8805992

hc=hclust(d1,method="centroid")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.8615349

hc=hclust(d1,method="ward.D")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.623357


# corr --------------------------------------------------------------------

full_score <- rep(100,9)
data1 <- rbind(full_score, data1)

weight <- c(0.3,0.2, 0.15, 0.05, 0.1, 0.05, 0.05, 0.05, 0.05)
d1 <- daisy(data1, metric = "gower", weights = weight)
d <- as.matrix(d1)

dist_score <- d[-1,1]

cor(dist_score, data$Overall.SCORE2)  ##   -0.9987225


# rank -----------------------------------------------------------------

rank <- as.data.frame(rank(dist_score))


# TreeAndLeaf ---------------------------------------------------------------

data1 <- data[, -10]
data1 <- as.data.frame(scale(data1))

weight <- c(0.3,0.2, 0.15, 0.05, 0.1, 0.05, 0.05, 0.05, 0.05)
d1 <- daisy(data1, metric = "gower", weights = weight)
hc=hclust(d1,method="average")


#-- Convert the 'hclust' object into a 'tree-and-leaf' object
tal <- treeAndLeaf(hc)

#--- Map attributes to the tree-and-leaf
#Note: 'refcol = 0' indicates that 'dat' rownames will be used as mapping IDs
tal <- att.mapv(g = tal, dat = data, refcol = 0)


#--- Set graph attributes using the 'att.setv' wrapper function
pal <- brewer.pal(9, "Greens")
tal <- att.setv(g = tal, from = "Academic.Reputation.Score", to = "nodeColor", 
                cols = pal, nquant = 10)
tal <- att.setv(g = tal, from = "Citations.per.Faculty.Score", to = "nodeSize",
                xlim = c(40, 120, 20), nquant = 5)

#--- Set graph attributes using 'att.addv' and 'att.adde' functions
tal <- att.addv(tal, "nodeFontSize", value = 1)
tal <- att.adde(tal, "edgeWidth", value = 10)


#--- Call RedeR application
rdp <- RedPort()
calld(rdp)
resetd(rdp)

#--- Send the tree-and-leaf to the interactive R/Java interface
addGraph(obj = rdp, g = tal, gzoom=5)

#--- Call 'relax' to fine-tune the leaf nodes
relax(rdp, p1=25, p2=200, p3=10, p4=100, p5=10, ps=TRUE)


#--- Add legends
addLegend.color(obj = rdp, tal, title = "Academic.Reputation.Score",
                position = "bottomright")
addLegend.size(obj = rdp, tal, title = "Citations.per.Faculty.Score")



# gower2 ---------------------------------------------------------------

data1 <- data[ ,c(7,8:16)]
#data1$Country.Code <- factor(data1$Country.Code)
#data1$SIZE <- factor(data1$SIZE)
#data1$FOCUS <- factor(data1$FOCUS)
#data1$RES. <- factor(data1$RES.)
#data1$AGE <- factor(data1$AGE)
data1$STATUS <- factor(data1$STATUS)

library(Rsolnp)

f <- function(weight){
  d1 <- daisy(data1, metric = "gower", weights = weight)
  hc=hclust(d1,method="average")
  d2 <- cophenetic(hc)
  return(-cor(d1, d2)) 
}

g <- function(weight) {
  sum(weight) 
}

weight <- c(0.05,0.3,0.2, 0.1, 0.05, 0.1, 0.05, 0.05, 0.05, 0.05)

solnp(weight,
      f, 
      eqfun=g,
      eqB=1,
      LB=rep(0.01,10))


weight <- c(0.05,0.3,0.2, 0.1, 0.05, 0.1, 0.05, 0.05, 0.05, 0.05)
weight <- c(0.03, 0.485, 0.1, 0.1, 0.05, 0.06,
            0.04, 0.05, 0.04, 0.045)

d1 <- daisy(data1, metric = "gower", weights = weight)

hc=hclust(d1,method="single")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.5414754;  0.6879894

hc=hclust(d1,method="complete")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.8070669;  0.7634953

hc=hclust(d1,method="average")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.8586958;  0.8758143

hc=hclust(d1,method="centroid")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.770202;  0.802073

hc=hclust(d1,method="ward.D")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.6755116;  0.6888072


# corr --------------------------------------------------------------------

data1 <- data[ ,c(4,7,8:16)]

full_score <- rep(100,9)
new_row <- data.frame(FOCUS = 'FC', STATUS = "B", 
                      Academic.Reputation.Score=100,
                      Citations.per.Faculty.Score=100,
                      Employer.Reputation.Score = 100,
                      Employment.Outcomes.Score = 100,         
                      Faculty.Student.Score = 100,              
                      International.Faculty.Score = 100,        
                      International.Research.Network.Score = 100,
                      International.Students.Score = 100,       
                      Sustainability.Score = 100)

data1 <- rbind(new_row,data1)
data1$FOCUS <- factor(data1$FOCUS)
data1$STATUS <- factor(data1$STATUS)

weight <- c(0.02, 0.02, 0.26, 0.2, 0.15, 0.05, 0.1,
            0.05, 0.05, 0.05, 0.05)
d1 <- daisy(data1, metric = "gower", weights = weight)
d <- as.matrix(d1)
dist_score <- d[-1,1]

cor(dist_score, data$Overall.SCORE2)  ##  -0.9982113


# rank -----------------------------------------------------------------

rank <- as.data.frame(rank(dist_score))


# TreeAndLeaf ---------------------------------------------------------------

hc=hclust(d1,method="average")

#-- Convert the 'hclust' object into a 'tree-and-leaf' object
tal <- treeAndLeaf(hc)

#--- Map attributes to the tree-and-leaf
#Note: 'refcol = 0' indicates that 'dat' rownames will be used as mapping IDs
tal <- att.mapv(g = tal, dat = data, refcol = 0)


#--- Set graph attributes using the 'att.setv' wrapper function
pal <- brewer.pal(9, "Greens")
tal <- att.setv(g = tal, from = "Academic.Reputation.Score", to = "nodeColor", 
                cols = pal, nquant = 10)
tal <- att.setv(g = tal, from = "Citations.per.Faculty.Score", to = "nodeSize",
                xlim = c(40, 120, 20), nquant = 5)

#--- Set graph attributes using 'att.addv' and 'att.adde' functions
tal <- att.addv(tal, "nodeFontSize", value = 1)
tal <- att.adde(tal, "edgeWidth", value = 10)


#--- Call RedeR application
rdp <- RedPort()
calld(rdp)
resetd(rdp)

#--- Send the tree-and-leaf to the interactive R/Java interface
addGraph(obj = rdp, g = tal, gzoom=5)

#--- Call 'relax' to fine-tune the leaf nodes
relax(rdp, p1=25, p2=200, p3=20,p4=100, p5=10, ps=TRUE)


#--- Add legends
addLegend.color(obj = rdp, tal, title = "Academic.Reputation.Score",
                position = "bottomright")
addLegend.size(obj = rdp, tal, title = "Citations.per.Faculty.Score")







# dendrogram 50---------------------------------------------------------------

library(dendextend)

data1 <- data[, -10]
data1 <- as.data.frame(scale(data1))
data2 <- data1[c(1:50),]


d1 <- dist(data2, method = 'euclidean')
hc=hclust(d1,method="average")

weight <- c(0.3,0.2, 0.15, 0.05, 0.1, 0.05, 0.05, 0.05, 0.05)
d1 <- daisy(data2, metric = "gower", weights = weight)
hc=hclust(d1,method="average")



# 将hclust对象转换为dendrogram对象
dend <- as.dendrogram(hc)

dend  %>%
  set("branches_k_color", k=4) %>% 
  set("branches_lwd", 1.2) %>% plot(main = "dendrogram")

# %>% set("labels", "")

#cut_hc <- cutree(hc,k = 7)
#cut_hc[cut_hc == 5]


