# Exercice 1
# Question 1
factorielle=function(n){
    if (n==0){
        return (1)
    }else{
        return(prod(1:n))}
    }
# Question 2
factorielle=function(n){
    if (n==0){
        return (1)
    }else{
        aux=1
        for(k in 1:n){aux=aux*k}
        return(aux)}
}

# Question 3
factorielle=function(n){return(gamma(n+1))}



# Exercice 2
# Question 1
mat=matrix(c(1,0,3,4,5,5,0,4,5,6,3,4,0,1,3,2),ncol=4)
rownames(mat)=c("ligne-1","ligne-2","ligne-3","ligne-4")
colnames(mat)=c("colonne 1","colonne 2","colonne 3","colonne 4")
# ou bien
mat=matrix(c(1,0,3,4,5,5,0,4,5,6,3,4,0,1,3,2),ncol=4)
rownames(mat)=rownames(mat,do.NULL = FALSE,prefix="ligne-")
colnames(mat)=colnames(mat,do.NULL = FALSE,prefix="colonne ")
# ou encore
mat=matrix(c(1,0,3,4,5,5,0,4,5,6,3,4,0,1,3,2),ncol=4)
dimnames(mat)=list(c("ligne-1","ligne-2","ligne-3","ligne-4"),c("colonne 1","colonne 2","colonne 3","colonne 4"))
# Question 2
determinant=det(mat)
inverse=solve(mat)
# Question 3
L=list(matrice=mat,determinant=determinant,inverse=inverse)
# Question 4
library(MASS)
ginv(mat)

# Exercice 3
A=matrix(c(5,4,-2,-1),ncol=2)
dec=eigen(A)
D=diag(dec$values)
P=dec$vectors
P%*%D%*%solve(P)  # on retrouve A
B=matrix(c(0,1,1,1,0,1,1,1,0),ncol=3)
eigen(B) # diagonalisation en base orthonormee

# Exercice 4
data(iris)
summary(iris) # donne un resume de ce data-frame
# Question 1
dim(iris)
# Question 2
iris[1:5,]
# Question 3
iris2=iris[iris[,5]=="versicolor",]
# Question 4
iris2[order(iris2$Petal.Length),] # ordre croissant
iris2[order(iris2$Petal.Length,decreasing=TRUE),] # ordre decroissant
