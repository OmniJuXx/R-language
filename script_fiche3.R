# Exemple introductif
x=seq(0,1,length=50)
y=sin(x)
plot(x,y)
plot(y~x)

# Section 1.1
donnees=read.table("ronfle.txt",header=T)
summary(donnees)
# Question 1
age=donnees$AGE 
hist(age) # probleme : on ne veut pas les effectifs, mais les densites
hist(age,prob=T) # probleme resolu : la somme des surfaces fait 1
# Question 2
hist(age,prob=T,main="Histogramme des ages")
# Question 3
hist(age,prob=T,breaks=c(20,40,55,80),main="Histogramme des ages")
# Question 4
boxplot(age)
# Question 5
par(mfrow=c(1,2))
boxplot(age)
hist(age,prob=TRUE,breaks=c(20,40,55,80),main="Histogramme des ages")
# Remarque : pour revenir a un seul graphe par fenetre, taper
par(mfrow=c(1,1))


# Section 1.2
# Question 1
plot(donnees$SEXE)
# Question 2
summary(donnees$ALCOOL) # la variable ALCOOL est quantitative
alcool=as.factor(donnees$ALCOOL)
summary(alcool) # la variable alcool est consideree comme qualitative
plot(alcool)

# Section 2
# Question 1
plot(donnees$POIDS~donnees$TAILLE) # ou plus elegant :
plot(POIDS~TAILLE,data=donnees)
# Question 2
boxplot(AGE~RONFLE,data=donnees) # les ronfleurs sont en moyenne plus ages

# Section 3
# Question 1
plot(POIDS~TAILLE,data=donnees,xlim=c(150,210),ylim=c(40,120))
# Question 2
plot(POIDS~TAILLE,data=donnees,xlim=c(150,210),ylim=c(40,120),xlab="taille",ylab="poids")
# Question 3
abline(v=median(donnees$TAILLE))
abline(h=median(donnees$POIDS))
# Question 4
points(mean(donnees$TAILLE),mean(donnees$POIDS),pch=3,col="red")
text(mean(donnees$TAILLE),mean(donnees$POIDS)-5,"G",col="red")

# Section 4
# Question 1
f=function(x,y){10*sin(sqrt(x^2+y^2))/sqrt(x^2+y^2)}
# Question 2
x=seq(-10,10,length=200)
y=x
# Question 3
z=outer(x,y,f)
# Question 4
persp(x,y,z)
# Question 5
persp(x,y,z,theta=30,phi =30)
# Question 6
contour(x,y,z)
image(x,y,z)
library(rgl) # il faut eventuellement commencer par telecharger ce package 
rgl.surface(x,y,z)

# Section 5.1
# Question 1
x=seq(0,4*pi,by=0.01)
plot(x,sin(x),type="l",col="red") # on ne veut pas de legende en ordonnees :
plot(x,sin(x),type="l",col="red",ylab="")
# Question 2
lines(x,cos(x))
# Question 3
abline(h=c(-1,1),col="green") # ajout des deux droites a la fois
# Question 4
abline(0,1,lty=3,col="blue")

# Section 5.2
# Question 1
boxplot(TAILLE~RONFLE,data=donnees) # le fait de ronfler ou non semble independant de la taille
# Question 2
boxplot(POIDS~RONFLE,data=donnees) # idem
# Question 3
par(mfrow=c(1,2))
boxplot(TAILLE~RONFLE,data=donnees)
boxplot(POIDS~RONFLE,data=donnees)
# Question 4 : ou bien directement via le menu (Export pour RStudio), ou bien "a la commande"
pdf("boxplots.pdf")
par(mfrow=c(1,2))
boxplot(TAILLE~RONFLE,data=donnees)
boxplot(POIDS~RONFLE,data=donnees)
dev.off()









