setwd("C:/Users/Abhilasha/final year R project/abhilasha project")
options(digits=4)
#data <- read.csv(file="lastfm-data.csv")
data.artist <- read.csv(file="lastfm-matrix-artist.csv")




data.artist.ibs <- (data.artist[,!(names(data.artist) %in% c("user"))])


getCosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}


holder <- matrix(NA, nrow=ncol(data.artist.ibs),ncol=ncol(data.artist.ibs),dimnames=list(colnames(data.artist.ibs),colnames(data.artist.ibs)))
data.artist.ibs.similarity <- as.data.frame(holder)


for(i in 1:ncol(data.artist.ibs)) {
  for(j in 1:ncol(data.artist.ibs)) {
    data.artist.ibs.similarity[i,j]= getCosine(data.artist.ibs[i],data.artist.ibs[j])
  }
}


write.csv(data.artist.ibs.similarity,file="final-artist-similarity.csv")


data.artist.neighbours <- matrix(NA, nrow=ncol(data.artist.ibs.similarity),ncol=11,dimnames=list(colnames(data.artist.ibs.similarity)))

for(i in 1:ncol(data.artist.ibs)) 
{
  data.artist.neighbours[i,] <- (t(head(n=11,rownames(data.artist.ibs.similarity[order(data.artist.ibs.similarity[,i],decreasing=TRUE),][i]))))
}

write.csv(file="final-artist-item-neighbours.csv",x=data.artist.neighbours[,-1])



getScore <- function(history, similarities)
{
  x <- sum(history*similarities)/sum(similarities)
  x
}


holder <- matrix(NA, nrow=nrow(data.artist),ncol=ncol(data.artist)-1,dimnames=list((data.artist$user),colnames(data.artist[-1])))


for(i in 1:nrow(holder)) 
{
  # Loops through the products (columns)
  for(j in 1:ncol(holder)) 
  {
    
    user <- rownames(holder)[i]
    product <- colnames(holder)[j]
    
    
    if(as.integer(data.artist[data.artist$user==user,product]) == 1)
    { 
      holder[i,j]<-""
    } else {
      
      
      topN<-((head(n=11,(data.artist.ibs.similarity[order(data.artist.ibs.similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      
      topN.similarities<-topN.similarities[-1]
      topN.names<-topN.names[-1]
      
      
      topN.purchases<- data.artist[,c("user",topN.names)]
      topN.userPurchases<-topN.purchases[topN.purchases$user==user,]
      topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("user"))])
      
      
      holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)
      
    } 
  }  
} 


data.artist.user.scores <- holder
write.csv(file="final-user-scores.csv",data.artist.user.scores)


data.artist.user.scores.holder <- matrix(NA, nrow=nrow(data.artist.user.scores),ncol=100,dimnames=list(rownames(data.artist.user.scores)))
for(i in 1:nrow(data.artist.user.scores)) 
{
  data.artist.user.scores.holder[i,] <- names(head(n=100,(data.artist.user.scores[,order(data.artist.user.scores[i,],decreasing=TRUE)])[i,]))
}


write.csv(file="final-user-recommendations.csv",data.artist.user.scores.holder)

