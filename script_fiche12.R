# Exemple
# Question 1
donnees=read.table("ronfle.txt",header=T)
summary(donnees)
# Question 2
library(MASS) # on commence par importer le package MASS
modele=lda(RONFLE~.,data=donnees)
modele # Prior probabilities of groups: il y a 65 non-ronfleurs pour 35 ronfleurs, donc pi0=0.35 et pi1=0.65. Group means: ce sont les moyennes mu0 et mu1 de chaque groupe, i.e. mu0 correspond aux caracteristiques moyennes d'un non-ronfleur et mu1 a celles d'un non-ronfleur. Coefficients of linear discriminants: c'est le vecteur w=[w1,...,wp] servant a definir la fonction score.
# Question 3 : on enleve les variables poids et taille, qui ont des coefficients w tres faibles et des moyennes quasi identiques d'un groupe a l'autre.
modelebis=lda(RONFLE~AGE+ALCOOL+SEXE+TABA,data=donnees)
modelebis #  les coefficients de w ont tres peu change. On conserve ce modele dans la suite.
# Question 4
prevbis=lda(RONFLE~AGE+ALCOOL+SEXE+TABA,data=donnees,CV=T)$class
prevbis
table(prevbis,donnees$RONFLE) # la verite est en colonnes, la prevision en lignes, le taux de mauvais classement est donc la somme des coefficients hors diagonale, i.e. (23+13)/100=36%. Pour le modele avec toutes les variables, on obtient :
prev=lda(RONFLE~.,data=donnees,CV=T)$class
table(prev,donnees$RONFLE) # donc 34% d'erreur de classement, donc comparable.
# Question 5 : on revient au modele complet
new=data.frame(AGE=42,POIDS=55,TAILLE=169,ALCOOL=0,SEXE="F",TABA="N")
predict(modele,newdata=new) # class correspond a un seuillage de posterior : le modele predit une proba de non-ronflement a pres de 80%, donc que cette personne est non-ronfleuse.


# Exercice
# Question 1 : pi0=n0/N et pi1=n1/N
# Question 2 : si on note f(x') la densité de la loi N(mu0,sigma2) et g(x") celle de la N(mu1,sigma2) 
# la vraisemblance est Ln(mu0,mu1,sigma2)=f(x'(1)...f(x'(n0))g(x"(1))...g(x"(n1))
# donc la log-vraisemblance est ln(mu0,mu1,sigma2)=log(Ln(mu0,mu1,sigma2)) 
# que l'on derive pour obtenir les estimateurs au max de vraisemblance
# mu0hat=(x'(1)+...+x'(n0))/n0
# mu1hat=(x"(1)+...+x"(n1))/n1
# sigma2hat=((x'(1)-mu0hat)^2+...+(x'(n0)-mu0hat)^2+(x"(1)-mu1hat)^2+...+(x"(n1)-mu0hat)^2)/n
# par la methode de plug-in, on obtient alors
# log(P(Y=1|X=x)/P(Y=0|X=x))=log(n1/n0)+((mu1hat-mu0hat)x-(mu1hat^2-mu0hat^2)/2)/sigma2hat
# on decide donc d'affecter le label 1 au nouvel individu x(n+1) si
# (mu1hat-mu0hat)x(n+1)/sigma2hat>(mu1hat^2-mu0hat^2)/(2*sigma2hat)-log(n1/n0)
# Question 3 : 
donnees2=donnees[,c(4,6)]
summary(donnees2)
decoupe=split(donnees2,donnees2$RONFLE) # creation d'une liste de deux data-frames
ronfleur=decoupe$O # data-frame des ronfleurs
nonronfleur=decoupe$N # data-frame des non-ronfleurs
summary(ronfleur)
summary(nonronfleur)
n0=nrow(nonronfleur)
n1=nrow(ronfleur)
n=n0+n1
mu0hat=mean(nonronfleur$ALCOOL)
mu1hat=mean(ronfleur$ALCOOL)
sigma2hat=((n0-1)*var(nonronfleur$ALCOOL)+(n1-1)*var(ronfleur$ALCOOL))/n
# Question 4 : 
x=3
((mu1hat-mu0hat)*x/sigma2hat>(mu1hat^2-mu0hat^2)/(2*sigma2hat)-log(n1/n0)) 
# FALSE => on le prevoit non ronfleur



