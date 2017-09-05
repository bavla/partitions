# operations on partitions
# Vladimir Batagelj, Amsterdam, September 2-3, 2017

setwd("C:/Users/batagelj/Documents/R/Rnet")

partInter <- function(P,Q){
  n <- length(P)
  if (n != length(Q)) return(NULL)
  D <- new.env(hash=TRUE,parent=emptyenv())
  S <- integer(n)
  for(u in 1:n){
    key <- paste(P[u],Q[u]) 
    if (!exists(key,env=D,inherits=FALSE)) assign(key,length(D)+1,env=D)
    S[u] <- get(key,env=D,inherits=FALSE) 
  }
  return(S)
}

partUnion <- function(P,Q){
  find <- function(D,key,Key){
    repeat{
      nkey <- get(key,env=D,inherits=FALSE)$i
      if(nkey == ' ') break
      if(nkey == Key) break
      key <- nkey
    }
    return(key)
  }

  n <- length(P)
  if (n != length(Q)) return(NULL)
  D <- new.env(hash=TRUE,parent=emptyenv())
  for(u in 1:n){
    key <- paste('p',P[u])
    if (!exists(key,env=D,inherits=FALSE)) assign(key,list(i=' ',v=0),env=D)
    key <- paste('q',Q[u])
    if (!exists(key,env=D,inherits=FALSE)) assign(key,list(i=' ',v=0),env=D)
  }
  for(u in 1:n){
    pkey <- paste('p',P[u]); qkey <- paste('q',Q[u])
    j <- find(D,pkey,qkey)
    assign(j,list(i=qkey,v=0),env=D)
  }
  k <- 0
  for(key in ls(D)){
    idx <- get(key,env=D,inherits=FALSE)$i
    if(idx == ' '){k <- k+1; assign(key,list(i=idx,v=k),env=D)}
  }
  S <- integer(n)
  for(u in 1:n){
    j <- find(D,paste('p',P[u]),' ')
    S[u] <- get(j,env=D,inherits=FALSE)$v
  }
  return(S)
}

partRandom <- function(n,k,names=NULL,classes=NULL,replace=TRUE){
  P <- sample(1:k,n,replace=replace); names(P) <- names
  return(P)
}

partCanon <- function(P){
  n <- length(P); S <- integer(n)
  D <- new.env(hash=TRUE,parent=emptyenv())
  for(u in 1:n){
    key <- as.character(P[u])
    if (!exists(key,env=D,inherits=FALSE)) assign(key,length(D)+1,env=D)
    S[u] <- get(key,env=D,inherits=FALSE)
  }
  return(S)
}

partSubeq <- function(P,Q) return (all(partInter(P,Q) == P))

partEq <- function(P,Q,strict=FALSE){
  return(all(partCanon(P)==partCanon(Q)))
}

partShuffle <- function(P) return(sample(P))

part2list <- function(P){
  Q <- partCanon(P); L <- list()
  for(i in 1:length(Q)) L[[Q[i]]] <- if(Q[i]>length(L)) i else c(L[[Q[i]]],i)
  return(L)
}

clust <- function(x) return(paste('{',paste(x,collapse=','),'}',sep=''))

part2string <- function(P) return(clust(sapply(part2list(P),clust)))

# to do

partBM <- function(N,P,BTypes){}
partGen <- function(P,start=FALSE){}   # start, next
# https://github.com/fxn/algorithm-combinatorics
# http://search.cpan.org/~fxn/Algorithm-Combinatorics/Combinatorics.pm
# https://ljwo.wordpress.com/2015/07/14/generating-set-partitions-replacing-virtual-functions-with-protected-inheritance/#more-349
# http://rprogramming.info/snippet/r/stirlingr_dewittpe_r
partExctract <- function(N,P,rowCls,colCls){}
partAgregate <- function(P,V){}

P <- c(1,2,3,3,3,2,1)
Q <- c(4,1,4,3,4,2,4)
(S <- partInter(P,Q))
(R <- partUnion(P,Q))
partSubeq(P,Q)
partSubeq(P,R)

n <- 15
lab <- paste('p',1:n)
pr <- partRandom(n,5,names=lab)
sr <- partCanon(pr)
partEq(pr,sr)
partShuffle(Q)
p <- c(0, 1, 1, 0, 2, 0, 3)
part2list(p)
part2string(p)

