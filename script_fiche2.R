# Section 1
donnees=read.table("ronfle.txt")
donnees[1,] # Probleme : le 1er individu correspond au nom des variables...
donnees=read.table("ronfle.txt",header=TRUE) 
donnees[1,] # Mission accomplished
summary(donnees)

# Section 2.1
age=donnees$AGE
# Question 1
n=length(age) # nombre d'individus = 100
moy=sum(age)/n
ecart=sqrt((sum(age^2))/n-moy^2) # premiere formule pour la variance empirique
ecart=sqrt(sum((age-moy)^2)/n) # seconde formule pour la variance empirique
# Question 2
vec=sort(age)
med=vec[n/2] # avec la convention med = inf{x tel que Fn(x) ≥ 1/2}
d1= vec[n/10] # avec la convention med = inf{x tel que Fn(x) ≥ 1/10}
# Question 3
mean(age)
sd(age) # attention ! sd est l'estimateur sans biais pour la variance (on divise par n-1)
median(age) # convention differente en R pour la mediane, voir l'aide.
quantile(age,0.1) # idem
# Question 4
quantile(age,probs=c(0.25,0.5,0.75))
summary(age) # on peut tout recuperer avec summary

# Section 2.2
donnees1=donnees[,c(1,2,3)]
# Question 1
mean(donnees1[,1]) # ou bien mean(donnees1$AGE)
mean(donnees1[,2]) # ou bien blablabla
mean(donnees1[,3])
# Question 2
apply(donnees1,2,mean) # moyenne de chaque colonne (argument "2") du tableau donnees1
# Question 3
apply(donnees1,2,sd) # ecart-type de chaque colonne du tableau donnees1
# Question 4
apply(donnees1,2,quantile,probs=0.1) # 1er decile de chaque colonne du tableau donnees1

# Section 2.3
decoupe=split(donnees,donnees$RONFLE) # creation d'une liste de deux data-frames
ronfleur=decoupe$O # data-frame des ronfleurs
nonronfleur=decoupe$N # data-frame des non-ronfleurs
# on peut lire les differents indicateurs dans le summary
summary(ronfleur)
summary(nonronfleur)
# on peut aussi les recuperer avec la fonction apply
apply(ronfleur[,1:4],2,mean)
apply(nonronfleur[,1:4],2,mean)  # les ronfleurs sont en moyenne plus vieux et boivent plus
apply(ronfleur[,1:4],2,median)
apply(nonronfleur[,1:4],2,median)
apply(ronfleur[,1:4],2,var)
apply(nonronfleur[,1:4],2,var)

# calcul des proportions
n1=nrow(ronfleur)
nbhommes1=sum(ronfleur$SEXE=="H")
nbhommes1/n1 # 86% des ronfleurs sont des hommes

n2=nrow(nonronfleur)
nbhommes2=sum(nonronfleur$SEXE=="H")
nbhommes2/n2 # 69% des non ronfleurs sont des hommes


