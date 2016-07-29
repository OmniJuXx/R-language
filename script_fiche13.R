# Question 1
ozone = read.table("ozone.txt",header=T)
summary(ozone)
ozone=ozone[,c("maxO3","vent")]
# Question 2
summary(ozone) # vent d'ouest dominant
# Question 3
boxplot(maxO3~vent,data=ozone) 
# Question 4 : il semble en effet y avoir un effet vent
# Question 5
modele=lm(maxO3~vent,data=ozone)
anova(modele)
# Question 5(a) : la proba critique associee au test de Fisher est Pr(>F)=0.02074, donc on rejette H0 : il existe au moins une modalite de la variable vent (i.e. une direction du vent) qui est significative pour expliquer le max de l'ozone.
# Question 5(b) : comme d'habitude, on a suppose les eps(i,j) gaussiens independants centres et de meme variance
# Question 6(a)
res=rstudent(modele)
plot(res)
sum(abs(res)>2)/length(res) # 9% des residus sont en dehors de [-2,2], ce qui est acceptable pour un IC a 95%
# Question 6(b)
par(mfrow=c(2,2))
plot(res[ozone[,"vent"]=="Est"],xlab="Est",ylab="Residus",ylim=c(min(res),max(res)),pch="+")
plot(res[ozone[,"vent"]=="Nord"],xlab="Nord",ylab="",ylim=c(min(res),max(res)),pch="+")
plot(res[ozone[,"vent"]=="Ouest"],xlab="Ouest",ylab="Residus",ylim=c(min(res),max(res)),pch="+")
plot(res[ozone[,"vent"]=="Sud"],xlab="Sud",ylab="",ylim=c(min(res),max(res)),pch="+") 
# Remarque : on peut faire plus elegant via le package lattice, mais passons...
# Question 6(c)
par(mfrow=c(1,1)) # on revient a un seul graphe pour la fenetre graphique
boxplot(res~vent,data=ozone,ylab="Residus")
# Question 7(a)
summary(modele) # c'est le vent d'est qui est pris comme reference car c'est le premier par ordre alphabetique
# Question 7(b) : pour l'est : y(i,j)=105.6+eps(i,j). Pour le vent de nord, y(i,j)=105.6-19.471+eps(i,j)=86.129+eps(i,j). Etc.
# Question 7(c) : avec une p-value de 0.77, le vent de sud est comparable au vent d'est du point de vue de l'effet sur l'ozone. Ceci se voyait sur le boxplot initial
boxplot(maxO3~vent,data=ozone)
# Question 8
modele2=lm(maxO3~C(vent,base=2),data=ozone)
summary(modele2) # cette fois, c'est la deuxieme modalite (par ordre alphabetique) qui est prise comme reference, c'est-a-dire le vent du nord.
# Question 9 : on a en effet pour le vent du nord y(i,j)=86.129+eps(i,j), et pour retrouver par exemple le vent d'est : y(i,j)=86.129+19.471+eps(i,j)=105.6+eps(i,j), qui correspond bien aux coefficients ci-dessus. Etc.
# Question 10 : les tests sur les coefficients ont change (cf. les p-values) : on voit par exemple que c'est le vent d'ouest qui ressemble au vent du nord, c'est-a-dire des vents de mer. Neanmoins, le resultat du test de Fisher de significativite globale est exactement le meme, ce qui est bien logique.

