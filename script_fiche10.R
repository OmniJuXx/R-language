# Section 2
# Question 1
ozone=read.table("ozone.txt",header=T)
summary(ozone) # ce sont donc les premiere et troisieme colonnes que l'on va garder
ozone=ozone[,c(1,3)] # ou bien : ozone = ozone[,c("maxO3","T12")]
summary(ozone)
plot(maxO3~T12,data=ozone,pch=15,cex=.5) 
# Question 2 : vu le nuage de points, un ajustement lineaire ne semble pas farfelu
# Question 3
reg=lm(maxO3~T12,data=ozone)
summary(reg) # on a donc beta0hat=-27.42 et beta1hat=5.47
# Question 4
names(reg)
# Question 5
beta=reg$coefficients # ou plus court : beta=reg$coef
SCR=sum((ozone$maxO3-(beta[1]+beta[2]*ozone$T12))^2) # Somme des Carres Residuelle = 33947.85
n=nrow(ozone) # n=112
sigmahat=sqrt(SCR/(n-2)) # on retrouve la Residual Standard Error
# Question 6
abline(beta[1],beta[2],col="red")
points(mean(ozone$T12),mean(ozone$maxO3),pch=3,col="blue")
text(mean(ozone$T12),mean(ozone$maxO3)-5,"G",col="blue")
# Question 7
plot(reg$residuals,ylab="residus") # il vaut mieux regarder les residus studentises, cf. fiche suivante 
# Question 8 : on regarde la valeur correspondant a T12=25 sur la droite de regression
# Question 9
maxO3p=beta[1]+beta[2]*25 # 109.3
# Question 10 : il faut construire un data-frame avec le meme nom de variable
demain=data.frame(25)
colnames(demain)="T12"
predict(reg,newdata=demain) 
predict(reg,newdata=demain,interval="pred") # si on veut en plus un intervalle de prevision a 95%

# Section 3
# Question 1
x=seq(0,1,by=0.01) # les Xi sont ainsi regulierement repartis
n=length(x)
B=1000
matY=matrix(0,ncol=n,nrow=B)
beta=matrix(nrow=B,ncol=2)
for (i in 1:B){
    matY[i,]=1+4*x+rnorm(n)
    beta[i,]=coef(lm(matY[i,]~x))
}
# Question 2
summary(beta)
hist(beta[,1],freq=FALSE) 
lines(density(beta[,1])) # une brave tete de gaussienne
hist(beta[,2],freq=FALSE) 
lines(density(beta[,2])) # idem
var(beta[,1]) # Rappel : Vth0=(sigma2*somme des Xi^2)/(n*somme des (Xi-Xbar)^2) avec dans notre cas sigma2=1
Vth0=sum(x^2)/(n*sum((x-mean(x))^2)) # 0.039, donc ca coincide
var(beta[,2]) # Rappel : Vth1=sigma2/(somme des (Xi-Xbar)^2) avec dans notre cas sigma2=1
Vth1=1/sum((x-mean(x))^2) # happy end!





