library(MixSim)
library(caret)
gaussien <- function(x,mu,i,pi,s){
  return (-0.5*t(x-mu[i,])%*%solve(s[,,i])%*%(as.matrix(x)-mu[i,])-(0.5*log(det(s[,,i])))+log(pi[i]))
}


#jeu 1 
set.seed(1234)
Q <- MixSim(BarOmega = 0.000005,  K = 2, p = 2,sph=TRUE)
A <- simdataset(n = 500, Pi = Q$Pi, Mu = Q$Mu, S = Q$S)
colors = c("green","red","brown","pink")
par(mar=c(0.1,0.1,0.1,0.1))
plot(A$X, col= colors[A$id+1], pch= 19 )
box()


liste_id=c()
n=500
k=2
nbr_good_answer=0
for(i in 1:n)
{

  for(j in 1:k)
  {
    score=gaussien(A$X[i,],Q$Mu,j,Q$Pi,Q$S)
    if(1<j)
    {
      if(save<score)
      {
        index=j
        save=score
      }
    }
    else
    {
      index=1
      save=score
    }
    
  }
  liste_id=c(liste_id,index)
 if(A$id[i]==index)
 {
   nbr_good_answer=nbr_good_answer+1
 }
  
}

print("accuracy =")
print(nbr_good_answer)
print(nbr_good_answer/n)
confusionMatrix(table(A$id,liste_id))

#jeu 2
set.seed(1234)
Q <- MixSim(BarOmega = 0.01,  K = 3, p = 2,sph=TRUE)
A <- simdataset(n = 500, Pi = Q$Pi, Mu = Q$Mu, S = Q$S)
colors = c("green","red","brown","pink")
par(mar=c(0.1,0.1,0.1,0.1))
plot(A$X, col= colors[A$id+1], pch= 19 )
box()


liste_id=c()
n=500
k=2
nbr_good_answer=0
for(i in 1:n)
{
  
  for(j in 1:k)
  {
    score=gaussien(A$X[i,],Q$Mu,j,Q$Pi,Q$S)
    if(1<j)
    {
      if(save<score)
      {
        index=j
        save=score
      }
    }
    else
    {
      index=1
      save=score
    }
    
  }
  liste_id=c(liste_id,index)
  if(A$id[i]==index)
  {
    nbr_good_answer=nbr_good_answer+1
  }
  
}

print("accuracy =")
print(nbr_good_answer)
print(nbr_good_answer/n)
confusionMatrix(table(A$id,liste_id))


#jeu 3
set.seed(1234)
Q <- MixSim(BarOmega = 0.1,  K = 3, p = 2,sph=FALSE)
A <- simdataset(n = 500, Pi = Q$Pi, Mu = Q$Mu, S = Q$S)
colors = c("green","red","brown","pink")
par(mar=c(0.1,0.1,0.1,0.1))
plot(A$X, col= colors[A$id+1], pch= 19 )
box()

liste_id=c()
n=500
k=2
nbr_good_answer=0
for(i in 1:n)
{
  
  for(j in 1:k)
  {
    score=gaussien(A$X[i,],Q$Mu,j,Q$Pi,Q$S)
    if(1<j)
    {
      if(save<score)
      {
        index=j
        save=score
      }
    }
    else
    {
      index=1
      save=score
    }
    
  }
  liste_id=c(liste_id,index)
  if(A$id[i]==index)
  {
    nbr_good_answer=nbr_good_answer+1
  }
  
}

print("accuracy =")
print(nbr_good_answer)
print(nbr_good_answer/n)
confusionMatrix(table(A$id,liste_id))
#partie 2


