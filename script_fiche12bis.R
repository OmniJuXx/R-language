X1<-c(rnorm(100,mean=1,sd=1),rnorm(100,mean=-1,sd=1))
X2<-c(rnorm(100,mean=1,sd=1),rnorm(100,mean=-1,sd=1))
donnees<-data.frame(X1,X2)

Y<-factor(c(rep(1,100),rep(0,100)))

plot(donnees[1:100,1],donnees[1:100,2],col="red",xlim=c(-3,3),ylim=c(-3,3),xlab="X1",ylab="X2")
points(donnees[101:200,1],donnees[101:200,2],pch='+',col="blue",xlim=c(-3,3),ylim=c(-3,3),xlab="X1",ylab="X2")

donnees1 <- data.frame(cbind(donnees,Y))

mu0 <- c(mean(X1[1:100]),mean(X2[1:100]))
mu1 <- c(mean(X1[101:200]),mean(X2[101:200]))
n <- nrow(donnees)
Sig <- 99/(n-2)*(cov(donnees[1:100,])+cov(donnees[101:200,]))
S <- (mu1-mu0)%*%t(mu1-mu0)
eigen(solve(Sig)%*%S)
w1 <- eigen(solve(Sig)%*%S)$vectors[,1]
w2 <- w1/sqrt(t(w1)%*%(Sig)%*%w1)
t(w2)%*%Sig%*%w2

library(MASS)
model <- lda(Y~.,data=donnees1)

prev <- predict(model)
Yhat <- prev$class
score <- prev$x

plot(score,rep(0,200),col=as.numeric(Yhat))



#on peut retrouver les scores en centrant les donnees de maniere a ce que le centre entre les centres de gravite (pondere par prior) soit en 0
w <- model$scaling
model$score[1]
prev <- predict(model)$x
donnees2 <- donnees-matrix(rep((mu0+mu1)/2,n),ncol=2,byrow=T)
prev1 <- apply(donnees2*matrix(rep((w),n),ncol=2,byrow=T),1,sum)
prev/prev1
