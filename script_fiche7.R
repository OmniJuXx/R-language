


# Section 1
# Question 1a : Xbar~N(mu1,1/n), Ybar~N(mu2,1/n), D~N(mu2-mu1,2/n).
# Sous H0, D~N(0,2/n) donc S(X)=D/sqrt(2/n)~N(0,1). 
# Question 1b : En notant q le quantile d'ordre 0.975 d'une N(0,1), 
# sous H0, P(-q<S(X)<q)=0.95, donc il suffit de comparer |S(x)| Ã  q :
# si |S(x)|<q, on decide mu1=mu2, sinon on decide mu1â‰ mu2
# si n=500, sous H0, D~N(0,2/500)=N(0,1/250)
x=seq(-0.5,0.5,by=0.01)
plot(x,dnorm(x,mean=0,sd=sqrt(1/250)),type="l",ylab="")
# Question 1c : si mu2=5.2, alors D~N(0.2,1/250)
lines(x,dnorm(x,mean=0.2,sd=sqrt(1/250)),type="l",ylab="")
# Risque de deuxieme espece
qmoins=qnorm(0.025,mean=0,sd=sqrt(1/250))
qplus=qnorm(0.975,mean=0,sd=sqrt(1/250))
pmoins=pnorm(qmoins,mean=0.2,sd=sqrt(1/250))
pplus=pnorm(qplus,mean=0.2,sd=sqrt(1/250))
beta=pplus-pmoins # beta = risque de deuxieme espece
dmoins=dnorm(qmoins,mean=0.2,sd=sqrt(1/250))
dplus=dnorm(qplus,mean=0.2,sd=sqrt(1/250))
segments(qmoins,0,qmoins,dmoins,col="red") # n'apparait pas : logique !
segments(qplus,0,qplus,dplus,col="red") # beta â‰? aire sous la gaussienne de droite Ã  gauche du trait rouge
# Question 2a : D~N(mu2-mu1,1/n1+1/n2) => donc, sous H0, D~N(0,1/n1+1/n2)
# Question 2b : Sous H0, S(X)=D/sqrt(1/n1+1/n2)~N(0,1). 
# donc si |S(x)|<q, on decide mu1=mu2, sinon on decide mu1â‰ mu2

# Section 2
# Question 1a : on considere les estimateurs non biaises usuels pour les variances.
# On a donc (sigma1hatcarre/sigma1carre)/(sigma2hatcarre/sigma2carre)~F(n1-1,n2-1)
# Sous A0, il s'ensuit que sigma1hatcarre/sigma2hatcarre~F(n1-1,n2-1) 
# Question 1b : soit q1 et q2 les quantiles d'ordres alpha/2 et 1-alpha/2 d'une F(n1-1,n2-1)
# si q1<f=sigma1hatcarre/sigma2hatcarre<q2, on accepte A0, sinon on opte pour A1
# Question 1c :
sigma1=1
sigma2=2
n1=10
n2=20
x=seq(0,4,by=0.01)
plot(x,df(x,n1-1,n2-1),type="l",ylab="",main="Densite de la loi de Fisher")
alpha=0.05
qmoins=qf(alpha/2,n1-1,n2-1)
qplus=qf(1-alpha/2,n1-1,n2-1)
segments(qmoins,0,qmoins,df(qmoins,n1-1,n2-1),col="red")
segments(qplus,0,qplus,df(qplus,n1-1,n2-1),col="red")
# Question 2a :
donnees=read.table("poulpe.csv",header=T,sep=";") 
# preciser que le separateur est un point-virgule, sinon tout va mal...
summary(donnees)
# Question 2b :
boxplot(Poids~Sexe,data=donnees,ylab="Poids",xlab="Sexe")
# la variance semble plus grande pour les males que pour les femelles
# Question 2c :
femelle=(donnees$Sexe=="Femelle")
male=(donnees$Sexe=="Male")
sigma1hatcarre=var(donnees$Poids[femelle])
sigma2hatcarre=var(donnees$Poids[male])
f=sigma1hatcarre/sigma2hatcarre # fâ‰?0.29
n1=sum(femelle)
n2=sum(male)
qmoins=qf(alpha/2,n1-1,n2-1) # qmoinsâ‰?0.31
qplus=qf(1-alpha/2,n1-1,n2-1) # qplusâ‰?3.05
(qmoins<=f & qplus>=f) # FALSE : on rejette A0
# Question 2d :
var.test(Poids~Sexe,conf.level=0.95,data=donnees) 
# 1 n'appartient pas Ã  l'IC Ã  95% => au niveau 5%, variances non egales
# Question 2e : pour retrouver la p-value en toute generalite
p=2*min(pf(f,n1-1,n2-1),1-pf(f,n1-1,n2-1))


# Section 3
# Question 1 : sous H0, D~N(0,sigma1^2/n1+sigma2^2/n2)
# donc, sous H0, D/sqrt(sigma1^2/n1+sigma2^2/n2)~N(0,1)
# Question 2 : si n1 & n2 -> infini, alors par Slutsky, sous H0
# D/sqrt(sigma1hat^2/n1+sigma2hat^2/n2)â‰ˆN(0,1)
xbar=mean(donnees$Poids[femelle])
ybar=mean(donnees$Poids[male])
t=(ybar-xbar)/sqrt(sigma1hatcarre/n1+sigma2hatcarre/n2) # tâ‰?3.75
alpha=0.05
q=pnorm(1-alpha/2)
(abs(t)<=q) # FALSE : avec ce test asymptotique, on rejette HO, mais cf. plus bas...
# pour la p-value :
pasymp=2*pnorm(-abs(t)) # pasympâ‰?0.00018 
# la Pc vaut 0.037, on considere donc les variances differentes et on utilise le test de Welch
# Question 3
t.test(Poids~Sexe,data=donnees,conf.level=0.95) # p-valueâ‰?0.001 => on rejette H0
# noter que p-valueâ‰ pasymp : test asymptotique non valide ici (n1 et n2 trop petits)






