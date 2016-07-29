# Section 1
# Question 1 : Xbar ~ N(mu,sigma2/n) => sqrt(n)(Xbar-mu)/sigma ~ N(0,1)
# Question 2 : soit q le quantile d'ordre 1-alpha/2 d'une N(0,1), alors
# P(Xbar-q sigma/sqrt(n)<mu<Xbar+q sigma/sqrt(n))=1-alpha
# Longueur de l'intervalle = 2q sigma/sqrt(n), qui n'est pas aleatoire !
# Question 3
n=100
X=rnorm(n,5,1)
Xbar=mean(X)
q=qnorm(0.975)
IC=c(Xbar-q/sqrt(n),Xbar+q/sqrt(n))
# Question 4
intconf=function(donnees,alpha){
    Xbar=mean(donnees)
    q=qnorm(1-alpha/2)
    IC=c(Xbar-q/sqrt(n),Xbar+q/sqrt(n))
    return(IC)
}
# Question 5
B=1000
reponse=rep(0,B)
for (i in 1:B){
    X =rnorm(n,5,1)
    IC=intconf(X,0.05)
    reponse[i]=(5>=IC[1] & 5<=IC[2])
}
sum(reponse/B) # environ 950 fois

# Section 2
# Question 1 : sigmahat2=((X1-Xbar)^2+...+(Xn-Xbar)^2)/(n-1)
# Question 2 : loi du khi-deux a (n-1) ddl (par Cochran)
# Question 3 : T suit une loi de Student a (n-1) ddl (Cochran again)
# Question 4 : soit q le quantile d'ordre 1-alpha/2 d'une T(n-1), alors 
# P(Xbar-q sigmahat/sqrt(n)<mu<Xbar+q sigmahat/sqrt(n))=1-alpha 
# Question 5 :
n=100
X=rnorm(n,5,sqrt{2})
Xbar=mean(X)
sigmahat=sd(X)
q=qt(0.975,df=99)
IC=c(Xbar-q*sigmahat/sqrt(n),Xbar+q*sigmahat/sqrt(n))
# Question 6 :
t.test(X) # OK

# Section 3
# Question 1 : convergence en loi : sqrt{n}(Xbar-mu)/sigma => N(0,1)
# Question 2 : sigmahat2=((X1-Xbar)^2+...+(Xn-Xbar)^2)/(n-1)
# Question 3 : par Slutsky, convergence en loi, sqrt{n}(Xbar-mu)/sigmahat => N(0,1)
# donc, en notant q le quantile d'ordre 1-alpha/2 d'une N(0,1), on a
# P(Xbar-q sigmahat/sqrt(n)<mu<Xbar+q sigmahat/sqrt(n)) -> 1-alpha 
# IC asymptotique : [Xbar-q sigmahat/sqrt(n),Xbar+q sigmahat/sqrt(n)]
# Question 4 : attention, parametre de l'expo = inverse de la moyenne !!
n=100
X=rexp(n,rate=1/2)
Xbar=mean(X)
sigmahat=sd(X)
q=qnorm(0.975)
IC=c(Xbar-q*sigmahat/sqrt(n),Xbar+q*sigmahat/sqrt(n))
# Question 5 :
t.test(X) # OK

# Section 4
# Question 1
poulpe=read.table("poulpeF.csv",header=TRUE)
summary(poulpe)
X=poulpe$Poids
# Question 2
hist(X,prob=T) 
lines(density(X))# cet echantillon ne semble pas gaussien (population heterogene adultes/enfants)
# Question 3
mean(X)-qnorm(0.975)*sd(X)/sqrt(240) # 583
mean(X)+qnorm(0.975)*sd(X)/sqrt(240) # 696
# Question 4
t.test(X) # 95 percent confidence interval: 582.9252 696.3248
# Question 5 : il sera plus petit
t.test(X,conf.level=0.90) # 90 percent confidence interval: 592.0978 687.1522













