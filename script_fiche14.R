# Question 1
donnees=read.table("cancerprostate.txt",header=T,sep=";")
# Question 2
summary(donnees) # probleme sur les 4 variables qualitatives, vues comme quantitatives
for (i in 3:6){donnees[,i]=factor(donnees[,i])}
summary(donnees) # c'est bon
# Question 3(a)
modele0=glm(Y~log.acid,data=donnees,family=binomial)
summary(modele0)
# Question 3(b) : la probabilite critique associee au test H0 : beta1=0 contre H1 : beta1 non nul, vaut 0.03, donc on rejette H0 et on considere la variable log.acid comme significative.
# Question 3(c) : le modele donne log(p(x)/(1-p(x))=0.404+2.245x, on en deduit que p(x)=exp(0.404+2.245x)/(1+exp(0.404+2.245x)). Noter qu'en toute rigueur il faudrait noter phat(x) et non p(x)... 
# Question 3(d)
beta=coefficients(modele0)
x=seq(-2,2,by=0.01)
p=exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x))
plot(x,p,type="l",xlab="log.acid",ylab="p(x)")
abline(h=1/2,lty=3)
# Question 3(e) : l'equation p(x0)=1/2 donne x0=-beta[1]/beta[2]. Pour x>x0, on considere donc que le cancer a atteint les noeuds lymphatiques et on predit Yhat=1, et inversement si x<x0.
abline(v=-beta[1]/beta[2],lty=3)
# Question 4
modele=glm(Y~.,data=donnees,family=binomial)
summary(modele)
# Question 5
modele_selectionne=step(modele,direction="backward")
summary(modele_selectionne) # les variables age et grade ont ete supprimees. 
# Question 6 : on ne rentre que les variables du modele selectionne (on pourrait les rentrer toutes, ce qui ne changerait rien du moment qu'on donne les bons noms aux variables)
new=data.frame(acide=0.6,rayonx="1",taille="0",log.acid=-0.51)
prev=predict(modele_selectionne,newdata=new,type="response") # on trouve une proba de 0.48, donc on predit que le cancer n'a pas atteint les noeuds lymphatiques. Cependant, le resultat etant proche de 1/2, la prudence est de mise... 
predict(modele_selectionne,newdata=new,interval="pred")
# Question 7(a)
prevglobale=predict(modele_selectionne,newdata=donnees,type="response") # on dispose donc d'un vecteur de 53 probabilites
# Question 7(b)
prevglobaleY=prevglobale>0.5 # on a seuille par rapport a 1/2 
prevglobaleY # on obtient un vecteur de booleens qu'on transforme en 0 et 1
prevglobaleY=as.numeric(prevglobaleY) # il reste a dresser la table de contingence
table(donnees$Y,prevglobaleY)
# Question 7(c) : le taux de mal classes est a nouveau la somme des termes hors diagonale, qui peut se calculer comme suit
MC=sum(donnees$Y!=prevglobaleY)/length(prevglobaleY) # environ 20% de mal classes
# Question 8
library(boot)
cout=function(Y_obs,prevision_prob){return(mean(abs(Y_obs-prevision_prob)>0.5))}
cv.glm(donnees,modele_selectionne,cout)$delta[1] # environ 28% de mal classes, c'est deja moins bon...
