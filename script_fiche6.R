# Section 1
# Question 1
n=100
alpha=0.05
q=qnorm(1-alpha/2)
X=rnorm(n,5,1)
# Question 2 : alpha est la proba de rejeter H0 alors qu'elle est vraie
muhat=mean(X) # sous H0 : muhat~N(5,1/n)
TX=as.numeric((abs(sqrt(n)*(muhat-5))>q)) 
# si TX=0, on conserve H0 ; si TX=1, on opte pour H1
# Question 3 : similaire a la question 5, Section 1, de la Fiche 5
B=1000
decision=rep(0,B)
for (i in 1:B){
    X=rnorm(n,5,1)
    muhat=mean(X) 
    decision[i]=as.numeric((abs(sqrt(n)*(muhat-5))>q))
}
sum(decision)/B # le resultat est proche de 5% 

# Section 2
# Question 1 : 
# beta(mu) est la proba d'accepter H0 alors que H1 est vraie (i.e. mu ≠ 5)
# pi(mu) est la proba d'accepter H1, donc pi(mu)=1-beta(mu)
# Question 2 : on note Phi(x)=P(N(0,1)≤x) la cdf de la loi N(0,1).
# Si X~N(mu,1), alors muhat~N(mu,1/n) donc vu la procedure de test, on a
# beta(mu)=Phi(-sqrt(n)*(mu-5)+q)-Phi(-sqrt(n)*(mu-5)-q)
# pi(mu)=1-beta(mu)
# Question 3 :
mu=seq(4,6,by=0.01)
pi=1-(pnorm(-sqrt(n)*(mu-5)+q)-pnorm(-sqrt(n)*(mu-5)-q))
plot(mu,pi,type="l")
# Question 4 :
n=1000
mu=seq(4,6,by=0.01)
pi=1-(pnorm(-sqrt(n)*(mu-5)+q)-pnorm(-sqrt(n)*(mu-5)-q))
lines(mu,pi,type="l")
# Question 5 : il suffit d'adapter la méthode de la Section 1, question 3
mu=5.3
n=100
alpha=0.05
q=qnorm(1-alpha/2)
B=1000
decision=rep(0,B)
for (i in 1:B){
  X=rnorm(n,mu,1)
  muhat=mean(X) 
  decision[i]=as.numeric((abs(sqrt(n)*(muhat-5))<q))
}
sum(decision)/B # le resultat est proche de beta(5.3)=1-pi(5.3)=0.15
beta=pnorm(-sqrt(n)*(mu-5)+q)-pnorm(-sqrt(n)*(mu-5)-q)

# Section 3
# Question 1
poulpe=read.table("poulpeF.csv",header=TRUE)
summary(poulpe)
X=poulpe$Poids
# Question 2 : cette fois on ne connait pas l'ecart-type sigma...
# => on utilise son estimateur sigmahat, comme en fiche 5 ! 
# Via Slutsky, ceci donne un test de niveau asymptotique alpha=5%
n=length(X) # n=240
muhat=mean(X) 
sdhat=sd(X)
# sous H0, sqrt(n)*(muhat-600)/sdhat ≈ N(0,1)
TX=as.numeric((abs(sqrt(n)*(muhat-600)/sdhat)>q)) # TX=0 => on accepte H0 
# Question 3: le test de Student considere que les variables sont gaussiennes
# ceci est faux sur nos donnees, mais ne change rien car T(239) ≈ N(0,1)
t.test(X,mu=600,conf.level=0.95) # on accepte H0
# Question 4 : la proba critique est la proba d'observer, sous H0, une valeur de la statistique de test 
# SX = sqrt(240)*(muhat(X)-600)/sigmahat(X)
# au moins aussi elevee en valeur absolue que la valeur observee Sx pour X=x
# Question 5 : 
Sx=sqrt(240)*(muhat-600)/sdhat 
# Puisque, sous H0, SX ≈ N(0,1), on en deduit
Pvalue=2*(1-pnorm(Sx)) # environ 0.17, comme ci-dessus via t.test
# Question 6 : il s'agit d'un test asymptotique unilateral, donc region de rejet R=[q,infini[
alpha=0.05
q=pnorm(1-alpha)
TX=as.numeric((sqrt(n)*(muhat-580)/sdhat>q)) # TX=1 => on rejette H0
pvalue=1-pnorm(sqrt(n)*(muhat-580)/sdhat) # ≈ 0.02
t.test(X,mu=580,alternative="greater",conf.level=0.95) 
# on retrouve une p-value d'environ 0.02




