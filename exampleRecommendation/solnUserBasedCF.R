# Lets make a helper function to calculate the scores
getScore <- function(history, similarities)
{
  x <- sum(history*similarities)/sum(similarities)
  x
}

setwd("/home/sdhandap/HackerRankRecommendation/exampleRecommendation")

# Read data from Last.FM frequency matrix  
data.germany <- read.csv(file="lastfm-matrix-germany.csv")


# A placeholder matrix
holder <- matrix(NA, nrow=nrow(data.germany),ncol=ncol(data.germany)-1,dimnames=list((data.germany$user),colnames(data.germany[-1])))

# Loop through the users (rows)
for(i in 1:nrow(holder)) 
{
  # Loops through the products (columns)
  for(j in 1:ncol(holder)) 
  {
    # Get the user's name and th product's name
    # We do this not to conform with vectors sorted differently 
    user <- rownames(holder)[i]
    product <- colnames(holder)[j]
    
    # We do not want to recommend products you have already consumed
    # If you have already consumed it, we store an empty string
    if(as.integer(data.germany[data.germany$user==user,product]) == 1)
    { 
      holder[i,j]<-""
    } else {
      
      # We first have to get a product's top 10 neighbours sorted by similarity
      topN<-((head(n=11,(data.germany.ibs.similarity[order(data.germany.ibs.similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      # Drop the first one because it will always be the same song
      topN.similarities<-topN.similarities[-1]
      topN.names<-topN.names[-1]
      
      # We then get the user's purchase history for those 10 items
      topN.purchases<- data.germany[,c("user",topN.names)]
      topN.userPurchases<-topN.purchases[topN.purchases$user==user,]
      topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("user"))])
      
      # We then calculate the score for that product and that user
      holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)
      
    } # close else statement
  } # end product for loop   
} # end user for loop

data.germany.user.scores <- holder

# We first have to get a product's top 10 neighbours sorted by similarity
topN<-((head(n=11,(data.germany.ibs.similarity[order(data.germany.ibs.similarity[,product],decreasing=TRUE),][product]))))
topN.names <- as.character(rownames(topN))
topN.similarities <- as.numeric(topN[,1])

# Drop the first one because it will always be the same song
topN.similarities<-topN.similarities[-1]
topN.names<-topN.names[-1]

# We then get the user's purchase history for those 10 items
topN.purchases<- data.germany[,c("user",topN.names)]
topN.userPurchases<-topN.purchases[topN.purchases$user==user,]
topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("user"))])

# We then calculate the score for that product and that user
holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)

# Lets make our recommendations pretty
data.germany.user.scores.holder <- matrix(NA, nrow=nrow(data.germany.user.scores),ncol=100,dimnames=list(rownames(data.germany.user.scores)))
for(i in 1:nrow(data.germany.user.scores)) 
{
  data.germany.user.scores.holder[i,] <- names(head(n=100,(data.germany.user.scores[,order(data.germany.user.scores[i,],decreasing=TRUE)])[i,]))
}