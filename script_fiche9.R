# Section 1
# Question 1 : Xi iid ~ B(theta)
# Question 2 : thetahat=Xbar=moyenne empirique des Xi => Sn~B(n,theta)
# Question 3 : sous H0, Sn~B(n,1/2)
n=100
alpha=0.05
qmoins=qbinom(alpha/2,size=n,prob=1/2)
qplus=qbinom(1-alpha/2,size=n,prob=1/2)
thetahat=mean(rbinom(n,size=1,prob=1/2))
T=n*thetahat
(T>qmoins & T<qplus) # H0 si TRUE, H1 si FALSE
T=55
(T>qmoins & T<qplus) # TRUE => on accepte H0 
# Question 4 :
prop.test(x=55,n=100,p=1/2)
# Question 5 :
theta=seq(0,1,by=0.01)
pi=1-(pbinom(qplus,size=n,p=theta)-pbinom(qmoins,size=n,p=theta))
plot(theta,pi,type="l",col="red")
# Question 6 : Sn≈N(n*theta,n*theta*(1-theta))
qmoinsgauss=qnorm(alpha/2,mean=n/2,sd=sqrt(n/4))
qplusgauss=qnorm(1-alpha/2,mean=n/2,sd=sqrt(n/4))
thetahat=mean(rbinom(n,size=1,prob=1/2))
T=n*thetahat
(T>qmoinsgauss & T<qplusgauss) # H0 si TRUE, H1 si FALSE
T=55
(T>qmoinsgauss & T<qplusgauss) # TRUE => on accepte H0 
# Question 7 :
theta=seq(0,1,by=0.01)
moyenne=n*theta
ecart=sqrt(n*theta*(1-theta))
pi2=1-(pnorm(qplusgauss,mean=moyenne,sd=ecart)-pnorm(qmoinsgauss,mean=moyenne,sd=ecart))
lines(theta,pi2,type="l")


# Section 2
# Question 1 : 
# sondage sans remise => loi hypergéométrique plutot que binomiale : OK si taux de sondage faible
# De plus, ce modele suppose que les personnes sondees voteront comme elles l'annoncent
# Question 2 : test unilateral
alpha=0.05
n=544+496
q=qbinom(alpha,size=n,prob=1/2)
T=544
T>q # TRUE => on accepte H0
# Question 3 :
prop.test(544,1040,p=0.5,alternative="less") # p-value=0.93
# Question 4 :
n=1000
alpha=0.05
p=seq(0,1,by=0.01)
q=qbinom(alpha,size=n,prob=p)
pi=1-pbinom(q,size=n,prob=1/2)
plot(p,pi,type="l",col="red")
