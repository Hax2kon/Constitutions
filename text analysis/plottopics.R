#####Haakon plotte Topic####
rm(list=ls())
library(devtools);library(austin);require(XML);require(tm);require(wordcloud);require(RColorBrewer);
library(topicmodels);library(tm);library(grDevices);library(SnowballC)

setwd("")
load("")

DocMat <- DocumentTermMatrix()
terms <- removeSparseTerms(DocMat,.95)

k <- 10 #Obs: This will be in use later


control_LDA_Gibbs <- list(estimate.beta=TRUE,
                          verbose=10,burnin=1000,iter=2000)
topicsLDA <- LDA(terms,k=k,method="Gibbs",control=control_LDA_Gibbs)


################################################################################
####                                                                        ####
####  PLOT 1: nt most frequent terms and their probability to be in term k  ####
####                                                                        ####
################################################################################
nt <- 30 #nt most frequent terms in DocMat
ntnames <- names(colSums(wfm(terms))[order(colSums(wfm(terms)),decreasing=TRUE)])[nt:1] #This extracts nt most frequent words
bsize <- 50 #Probabilities will be multiplied by this in order to scale up bubble size

#In order to scale up the bubble sizes, probability is multiplied
par(mar=c(5.1, 5.1, 5.1, 2.1)) #Rescale this if plot looks weird
plot(nt,k,ylim=c(1,nt),xlim=c(1,k),
     type="n",axes=FALSE,bty="n",ylab="",xlab="")
axis(3,at=1:k,labels=paste("Topic",1:k,sep=" "),
     las=2,col="white",col.ticks="grey30",col.axis="grey30")
for(i in 1:k){
  abline(v=i,lty="dotted",col="lightgrey")
}
for(i in 1:nt){
  abline(h=i,lty="dotted",col="lightgrey")
}
axis(2,at=1:nt,las=2,cex.axis=0.6,col="white",col.ticks="grey30",col.axis="grey30",
     labels=ntnames)
for(i in 1:nt){
  points(1:k,rep(i,k),pch=21,
         cex=(posterior(topicsLDA)$terms[,grep(ntnames[i],
                                               colnames(posterior(topicsLDA)$terms))])*bsize,
         bg="#22222270",col="#222222")
}


####################################################################################################################
####                                                                                                            ####
####  PLOT 2: nt unique terms among d most likely terms in each k topic and their probability to be in topic k  ####
####                                                                                                            ####
####################################################################################################################

d <- 10 #d most likely terms in each topic
bsize <- 100 #Probabilities will be multiplied by this in order to scale up bubble size



Terms <- terms(topicsLDA,d) #d most likely terms in each topic
ntnames <- unique(as.vector(Terms)) #unique terms among d most likely terms in each topic k
nt <- length(ntnames) #nt unique terms among d most likely terms in each topic k

par(mar=c(5.1, 5.1, 5.1, 2.1))
plot(0,0,ylim=c(1,nt),xlim=c(1,k),
     type="n",axes=FALSE,bty="n",ylab="",xlab="")
axis(3,at=1:k,labels=paste("Topic",1:k,sep=" "),
     las=2,col="white",col.ticks="grey30",col.axis="grey30")
for(i in 1:k){
  abline(v=i,lty="dotted",col="lightgrey")
}
for(i in 1:nt){
  abline(h=i,lty="dotted",col="lightgrey")
}
axis(2,at=1:nt,las=2,cex.axis=0.6,col="white",col.ticks="grey30",col.axis="grey30",
     labels=ntnames)
for(i in 1:nt){
  points(1:k,rep(i,k),pch=21,
         cex=(posterior(topicsLDA)$terms[,grep(ntnames[i],
                                               colnames(posterior(topicsLDA)$terms))])*bsize,
         bg="#22222270",col="#222222")
}

############################################################
####                                                    ####
####  PLOT 3: Probabiltity that document is in topic k  ####
####                                                    ####
############################################################

ptopic <- posterior(topicsLDA)$topics
index <- 1:50 #i documents among all documents (if there are few documents, simply set 1:nrow(ptopic))
beg <- index[1]
end <- index[length(index)]
bsize <- 10 #Probabilities will be multiplied by this in order to scale up bubble size


par(mar=c(1.1, 7.1, 5.1, 1.1))
plot(beg,k,ylim=c(beg,end),xlim=c(1,k),
     type="n",axes=FALSE,bty="n",ylab="",xlab="")
axis(3,at=1:k,labels=paste("Topic",1:k,sep=" "),
     las=2,col="white",col.ticks="grey30",col.axis="grey30")
for(i in 1:k){
  abline(v=i,lty="dotted",col="lightgrey")
}
for(i in beg:end){
  abline(h=i,lty="dotted",col="lightgrey")
}
axis(2,at=beg:end,las=2,cex.axis=0.6,col="white",col.ticks="grey30",col.axis="grey30",
     labels=rownames(ptopic)[index])
for(i in index){
  points(1:k,rep(i,k),pch=21,
         cex=(ptopic[i,1:k])*bsize,
         bg="#22222270",col="#222222")
}
for(i in index){
  points(as.vector(topics(topicsLDA)[i]),i,pch=21,
         cex=ptopic[i,as.vector(topics(topicsLDA)[i])]*bsize,
         bg="#5C000270",col="#5C0002")
}

