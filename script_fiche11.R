# Exemple
# Question 1
ozone = read.table("ozone.txt",header=T)
summary(ozone)
ozone=ozone[,1:11] # selection de la variable a expliquer et des variables explicatives
# Question 2
summary(ozone) # OK
# Question 3
reg=lm(maxO3~.,data=ozone)
# Question 4
summary(reg) # Testees une par une, les seules variables significatives sont maxO3v et Ne9, mais la regression etant multiple et les variables explicatives non orthogonales, il est delicat d'utiliser ces tests. En effet, le test sur un coefficient revient a tester la significativite d'une variable alors que les autres sont dans le modele. Autrement dit, cela revient a tester que la variable n'apporte pas d'information supplementaire sachant que toutes les autres variables sont dans le modele. Il est donc important et plus pertinent d'appliquer des procedures de choix de modele comme ci-dessous.
# Question 5
library(leaps)
choix=regsubsets(maxO3~.,data=ozone,nbest=1,nvmax=11)
plot(choix,scale="bic") # Le critere est optimum pour la ligne en haut du graphique. Nous conservons, pour le critere BIC, le modele a 4 variables: T12, Ne9, Vx9 et maxO3v. Nous definissons ainsi un nouveau modele avec ces variables.
# Question 6
regbis=lm(maxO3~T12+Ne9+Vx9+maxO3v,data=ozone)
summary(regbis)
library(leaps)
choix2=regsubsets(maxO3~T12+Ne9+Vx9+maxO3v,data=ozone,nbest=1,nvmax=5)
plot(choix2,scale="bic")
# Question 7
res=rstudent(regbis)
summary(res) # on peut montrer que si notre modele est correct, i.e. si maxO3=beta0+beta1*T12+beta2*Ne9+beta3*Vx9+beta4*maxO3v+eps, avec les eps gaussiens independants et homoscedastiques, alors ces residus suivent une loi de Student a 106 ddl, i.e. en gros une loi N(0,1). Donc 95% d'entre eux doivent etre entre -2 et +2.
plot(res,ylab="residus")
abline(h=-2,col="red")
abline(h=2,col="red") # I love it when a plan comes together

# Exercice
# Question 1
n=1000
X15=matrix(10*runif(5*n),ncol=5)
Y=1+4*X15[,1]+5*X15[,2]+2*X15[,3]+6*X15[,4]+5*X15[,5]+rnorm(n)
# Question 2
X610=matrix(rnorm(5*n),ncol=5)
# Question 3
donnees=data.frame(cbind(Y,X15,X610)) # on met tout dans un data-frame
modele=lm(Y~.,data=donnees)
summary(modele)
# Question 4
library(leaps)
choix=regsubsets(Y~.,data=donnees,nbest=1,nvmax=10)
plot(choix,scale="bic") # on ne conserve que les variables pertinentes

