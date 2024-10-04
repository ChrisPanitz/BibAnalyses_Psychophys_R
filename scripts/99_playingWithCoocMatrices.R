#occMat <- data.frame(
#  a = c(rep(1,10),rep(0,40)),
#  b = c(rep(0,10),rep(1,20),rep(0,20)),
#  c = c(rep(1,10),rep(0,40)),
#  d = c(rep(0,30),rep(1,20))
#)

occMat <- data.frame(
  a = c(rep(1,20),rep(0,80)),
  b = c(rep(0,20),rep(1,40),rep(0,40)),
  c = c(rep(1,20),rep(0,80)),
  d = c(rep(0,60),rep(1,40))
)
coocMat <- matrix(data = NA, nrow = dim(occMat)[2], ncol = dim(occMat)[2])

for (i in 1:dim(coocMat)[1]){
  for (j in 1:dim(coocMat)[2]){
    coocMat[i,j] <- sum(occMat[,i]*occMat[,j])
  }
}

### variant 1 ###
cij <- coocMat[1,3]

si <- sum(occMat[,1])
sj <- sum(occMat[,3])

Sa <- cij / (si*sj); Sa

### variant 2 ###
cij <- coocMat[1,3]

si <- sum(coocMat[1,c(2,3,4)])
sj <- sum(coocMat[3,c(1,2,4)])

Sa <- cij / (si*sj)

















coocMat <- matrix(round(runif(n = 50*50, min = 0, max = 50)), nrow = 50)

coocMat <- coocMat + t(coocMat)
coocMat <- coocMat * (1-diag(dim(coocMat)[1]))

coocMatSq <- coocMat^2


eiMat <- matrix(data = NA, nrow = dim(coocMat)[1], ncol = dim(coocMat)[1])
asMat <- matrix(data = NA, nrow = dim(coocMat)[1], ncol = dim(coocMat)[1])
eiMatSq <- matrix(data = NA, nrow = dim(coocMat)[1], ncol = dim(coocMat)[1])
asMatSq <- matrix(data = NA, nrow = dim(coocMat)[1], ncol = dim(coocMat)[1])

for (rowI in 1:dim(asMat)[1]){
  for (colI in 1:dim(asMat)[2]){  
    eiMat[rowI,colI] <- coocMat[rowI,colI]^2 / (sum(coocMat[rowI,])*sum(coocMat[,colI])) # Cobo (2011)
    eiMatSq[rowI,colI] <- coocMatSq[rowI,colI]^2 / (sum(coocMatSq[rowI,])*sum(coocMatSq[,colI])) # Cobo (2011)
    asMat[rowI,colI] <- 2*(sum(coocMat)/2)*coocMat[rowI,colI] /
      (sum(coocMat[rowI,])*sum(coocMat[,colI])) # van Eck & Waltmann (2014)
    asMatSq[rowI,colI] <- 2*(sum(coocMatSq)/2)*coocMatSq[rowI,colI] /
      (sum(coocMatSq[rowI,])*sum(coocMatSq[,colI])) # van Eck & Waltmann (2014)
  }
}

cor(as.vector(eiMat), as.vector(eiMatSq))
cor(as.vector(asMat), as.vector(asMatSq))

cor(as.vector(eiMat), as.vector(asMat))
cor(as.vector(eiMatSq), as.vector(asMatSq))

cor(as.vector(eiMat), as.vector(eiMatSq), method = "kendall")
cor(as.vector(asMat), as.vector(asMatSq), method = "kendall")

cor(as.vector(eiMat), as.vector(asMat), method = "kendall")
cor(as.vector(eiMatSq), as.vector(asMatSq), method = "kendall")

