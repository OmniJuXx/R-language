# Section 1.1
# Question 1 : X suit une loi binomiale B(10,0.5)
# Question 2
dbinom(7,size=10,prob=0.5) # ou directement
dbinom(7,10,0.5)
# Question 3
k=0:10
p=dbinom(k,10,0.5)
plot(k,p,type="h")
# Question 4
k=0:10
px=dbinom(k,10,0.5)
py=dbinom(k,10,0.75)
par(mfrow=c(1,2))
plot(k,px,type="h",ylab="Loi de X")
plot(k,py,type="h",ylab="Loi de Y") # X est stochastiquement inferieure a Y
par(mfrow=c(1,1))
# Question 5
pbinom(7,10,0.75)
# Question 6
k=0:10
F=pbinom(k,10,0.75)
plot(k,F,type="s",ylab="cdf de Y")
# Question 7
abline(h=0.5,lty=3) # mediane de Y = 8
# Question 8
qbinom(0.5,10,0.75)
qbinom(c(0.25,0.5,0.75),10,0.75)

# Section 1.2
# Question 1
x=seq(-4,4,by=0.1)
plot(x,dnorm(x),type="l",ylab="",ylim=c(0,1)) # par defaut m=0 et sd=1
lines(x,pnorm(x))
# Question 2
qnorm(0.95) # 1.645 utile pour les IC a 90%
# Question 3
x=seq(-4,4,by=0.1)
plot(x,dnorm(x),type="l",ylab="")
abline(h=0)
# Question 4
q=qnorm(0.95)
segments(q,0,q,dnorm(q))
# Question 5 : 0.95 a gauche et 0.05 a droite
# Question 6 : attention : le second parametre d'une gaussienne sous R est l'ecart-type, non la variance !!!

# Section 2
# Question 1
ech=rnorm(20)
# Question 2
hist(ech,prob=T)
# on peut aussi obtenir un estimateur a noyau par : plot(density(ech),main="Estimateur a noyau de la densite")
# Question 3
x=seq(-4,4,by=0.1)
lines(x,dnorm(x),col="red")
# Question 4
par(mfrow=c(1,3))
for (i in c(100,1000,10000)){
    ech=rnorm(i)
    hist(ech,prob=T)
    lines(x,dnorm(x),col="red")
}
par(mfrow=c(1,1))

# Section 3.1
# Question 1
x=seq(-4,4,by=0.1)
plot(x,dnorm(x),type="l",col="red",ylab="")
# Question 2
lines(x,dt(x,5),col="yellow")
lines(x,dt(x,30),col="orange") # si d grand, Td=N(O,1) a peu de choses pres
# Question 3
legend("topleft",legend=c("N(0,1)","T5", "T30"),col=c("red","yellow","orange"),lty=1)

# Section 3.2
# Question 1 : Loi Forte des Grands Nombres : si X1,X2,... sont i.i.d. et admettent une esperance m, alors (X1+...+Xn) tend presque surement vers m (convergence en proba pour la loi faible). 
# Question 2
set.seed(123)
x=rbinom(1000,1,0.6) # Bernoulli est un cas particulier de binomiale
# Question 3
S=cumsum(x) # vecteur des sommes cumulees
M=S/1:1000
plot(1:1000,M,type="l",xlab="n",ylab="Moyenne des Xi", main="Loi des grands nombres")
abline(h=0.6,col="red")

# Section 3.3
# Question 1 : S suit une loi binomiale B(N,p), donc E[S]=Np et V(S)=Np(1-p)
# Question 2 : (S-Np)/sqrt(Np(1-p)) tend en loi vers une N(0,1)
# Question 3
N=10
p=0.5
S10=rbinom(1000,N,p)
U10=(S10-N*p)/sqrt(N*p*(1-p))
N=30
p=0.5
S30=rbinom(1000,N,p)
U30=(S30-N*p)/sqrt(N*p*(1-p))
N=1000
p=0.5
S1000=rbinom(1000,N,p)
U1000=(S1000-N*p)/sqrt(N*p*(1-p))
# Question 4
x=seq(-4,4,by=0.01)
par(mfrow=c(1,3))
hist(U10,prob=T)
lines(x,dnorm(x),col="red")
hist(U30,prob=T)
lines(x,dnorm(x),col="red")
hist(U1000,prob=T)
lines(x,dnorm(x),col="red")


# Section 3.4
# Question 1
d=20
n=500
echs=matrix(rnorm(n*d),nrow=n)
# Question 2
echs2=echs*echs # on eleve chaque coefficient au carre
ech=apply(echs2,1,sum) # on somme chaque ligne 
# Question 3
par(mfrow=c(1,1))
hist(ech,prob=T,breaks=seq(0,51,by=3))
# Question 4
x=seq(0,51,by=0.1)
lines(x,dchisq(x,20),col="red")
# Remarque : pour superposer l'estimateur a noyau et le khi-deux
plot(density(ech),main="Estimateur a noyau de la densite")
x=seq(0,51,by=0.1)
lines(x,dchisq(x,20),col="red")

# Section 3.5
U=rchisq(500,8)
V=rchisq(500,16)
F=2*U/V
hist(F,prob=T)
x=seq(0,max(F),by=0.01)
lines(x,df(x,8,16),col="red")






