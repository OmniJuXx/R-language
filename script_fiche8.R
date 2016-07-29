##############################
# Winter is coming
##############################
# Question 1 :
N=c(380,435,483,410)
n=sum(N) # on veut tester si H0 : N ~ M(1708,1/4,1/4,1/4,1/4)
colnames(N) <- c("Automne","Hiver","Printemps","Ete")
barplot(N,col=c("brown","black","green4","gold"))
# Question 2 :
T=sum((N-n*rep(1/4,4))^2/(n*rep(1/4,4)))
alpha=0.05
q=qchisq(1-alpha,df=3)
T<q # FALSE : on rejette H0 => naissances non uniformement reparties sur les saisons
# pour la proba critique
pvalue=1-pchisq(T,df=3) # 0.004
# Question 3 :
x=seq(0,20,by=0.01)
plot(x,dchisq(x,df=3),type="l",ylab="")
segments(0,0,q,0,col="red")
text(T,0,"T",col="red")
# Question 4 :
chisq.test(N) # tout est dit





##############################
# Even God could not sink this ship
##############################
# Question 1 :
tab=matrix(c(203,118,178,212,122,167,528,673),ncol=4,byrow=T)
rownames(tab)=c("survie","mort")
colnames(tab)=c("C1","C2","C3","E")
# Question 2 :
tab1 <- matrix(rep(apply(tab,1,sum),4),ncol=4)
tab2 <- matrix(rep(apply(tab,2,sum),2),ncol=4,byrow=T)
tab3 <- tab1*tab2/2201 
T <- sum((tab-tab3)^2/tab3) # 190.4 
alpha=0.05
q=qchisq(1-alpha,df=3)
T<q # FALSE : on rejette H0 => variables non independantes
# Question 3 :
chisq.test(tab) # p-value < 2.2e-16
# Question 4 :
pvalue=1-pchisq(T,df=3) # 0, ce qui est equivalent a < 2.2e-16



