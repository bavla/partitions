---
title: "partitions"
author: "Vladimir Batagelj"
date: "05 september 2017"
output: 
  html_document:
    number_sections: TRUE
    toc: TRUE
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
# R partitions

Partitions have an important role in data analysis (data aggregation, clustering, block modeling, etc.). Essentially the factorization in R produces partitions.

To support a work with partitions I started (in Amsterdam, September 2, 2017) to develop a package partitions.

## Introduction

A "standard"" approach to deal with large structures is the **divide and conquer** strategy that (recursively) breaks down a large structure to smaller, manageable sub-structures of the same or related type. 

Let ${\cal U}$ be a **set of units**, $u \in  {\cal U}$ a **unit**, and ${\cal C} = \{C_1, C_2, \ldots, C_k\}$, $\emptyset \subset C_i \subseteq {\cal U}$ a **partition** of ${\cal U}$ -- it holds: 
$$\bigcup_i C_i = {\cal U} \qquad \mbox{and} \qquad i \ne j \Rightarrow C_i \cap C_j = \emptyset.$$ 

In classic data analysis the units are usually described as lists of (measured) values of selected **variable**s ( properties, attributes)
$$X(u) = [ x_1(u), x_2(u), \ldots, x_m(u) ] $$
collected into a **data frame** $X$.

### Nested partitions
   
 
\cite[221-222]{naf} \cite[387-388]{adm} [^1]: Amsterdam, September 2, 2017.

Given two partitions ${\cal P} = \{ P_1, P_2, \ldots, P_p \}$ and ${\cal Q} = \{ Q_1, Q_2, \ldots, Q_q \}$ let us denote with $\sim_P$ and $\sim_Q$ the corresponding equivalence relations.

We define
$$ {\cal P} \sqsubseteq  {\cal Q} \ \equiv\  \forall P \in {\cal P} \, \exists Q \in {\cal Q}: P \subseteq Q $$
or equivalently
$$ {\cal P} \sqsubseteq  {\cal Q} \ \equiv\  \sim_P \subseteq \sim_Q $$
We also introduce two partitions ${\cal P} \sqcap  {\cal Q}$ and  ${\cal P} \sqcup  {\cal Q}$ determined by equivalence relations $\sim_P \cap \sim_Q$ and $(\sim_P \cup \sim_Q)^*$ where $*$ denotes the transitive closure.

### Aggregation

The **aggregation** of a cluster $C$ is again a list of values
$$ Y(C) = [ y_1(C), y_2(C), \ldots, y_m(C) ] $$
where $y_iC)$ is the aggregated value of the set of values $\{ x_i(u) : u \in C \}$ -- forming an **aggregated data frame** $Y$.

For example, for $x_i$ measured in a numerical scale their average is usually used
$$ y_i(C) = \frac{1}{|C|} \sum_{u \in C} x_i(u) $$
Different aggregation functions are available -- see Beliakov, Pradera, Calvo: [Aggregation Functions](http://www.springer.com/gp/book/9783540737209), 2007.

Some notes:

* This kind of aggregation can produce a big loss of information.
* It is not compatible with the recursive decomposition; but keeping $(|C|, y_i(C))$ is -- for $C_1 \cap C_2 = \emptyset$ we have
$$  y_i(C_1 \cup C_2) = \frac{|C_1| y_i(C_1) + |C_2| y_i(C_2)}{|C_1|+|C_2|}$$
* The aggregated descriptions need not to be from the same ``space'' as the descriptions of units.
### Symbolic data analysis
Edwin Diday proposed in late eighties an approach, named symbolic data analysis (SDA), in which the (aggregated)  descriptions can be **symbolic objects** (SO) such as: an interval, a list,  a histogram, a distribution, etc. We get \keyw{symbolic data frames}. See Billard, Diday: [Symbolic Data Analysis](http://eu.wiley.com/WileyCDA/WileyTitle/productCd-0470090170.html), 2006/2012. 
Using this approach we can reduce a big data frame into small, manageable symbolic data frame and preserve much more information. To analyze symbolic data frames new methods have to be developed -- SDA.
We found very interesting the representation with \keyw{discrete distribution}. 
The range $V$ of a variable $x_i$  is partitioned into $k_i$  subsets $\{ V_j  \}$. Then
$y_i(C) = [  y_{i1}(C), y_{i2}(C), \ldots, y_{ik_i}(C) ]$
where $y_{ij}(C) = | \{ u : x_i(u) \in V_j \} |$.
The description based on a discrete distribution enables us to consider variables that are measured in different types of measurement scales and based on a different number of original (individual) units. It is also compatible with recursive decomposition.% 
## Creating partitions
In a programming language a partition ${\cal C} = \{C_1, C_2, \ldots, C_k\}$ is usually represented with a table $P[u] = i$ iff $u \in C_i$ determining a function $P : {\cal U} \to 1 .. k$.
### Direct construction of a partition
```{r}
(P <- c(1,2,3,3,3,2,1))
(Q <- c(4,1,4,3,4,2,4))
```

### Factorization



### Random partition

```{r}
partRandom <- function(n,k,names=NULL,classes=NULL,replace=TRUE){
  P <- sample(1:k,n,replace=replace); names(P) <- names
  return(P)
}
n <- 15; lab <- paste('p',1:n,sep='')
(pr <- partRandom(n,5,names=lab))
(sr <- partRandom(n,5,names=lab))
```

### Canonical partition

```{r}
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
(sr <- partCanon(pr))
```

### Shuffle partition

Creates a random partition with the same sizes of classes
```{r}
partShuffle <- function(P) return(sample(P))
partShuffle(Q)
```

### Converting a partition to a list of classes

```{r}
part2list <- function(P){
  Q <- partCanon(P); L <- list()
  for(i in 1:length(Q)) L[[Q[i]]] <- if(Q[i]>length(L)) i else c(L[[Q[i]]],i)
  return(L)
}
p <- c(0, 1, 1, 0, 2, 0, 3)
part2list(p)
```

### Converting a partition to a string of a set of subsets

```{r}
clust <- function(x) return(paste('{',paste(x,collapse=','),'}',sep=''))
part2string <- function(P) return(clust(sapply(part2list(P),clust)))
part2string(p)
```


## Comparing partitions
```{r}
partSubeq <- function(P,Q) return (all(partInter(P,Q) == P))
partEq <- function(P,Q,strict=FALSE){
  return(all(partCanon(P)==partCanon(Q)))
}
partEq(pr,sr)
```

## Operations on partitions
### Computing  ${\cal P} \sqcap  {\cal Q}$

```{r}
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
(S <- partInter(P,Q))
```
### Computing  ${\cal P} \sqcup  {\cal Q}$

Can the partition  ${\cal P} \sqcup  {\cal Q}$ be computed fast?

An algorithm is the following:


1. construct a graph $\mathbf{G} = ( {\cal V}, \cal{E} )$ with
${\cal V} = {\cal U} \cup {\cal P} \cup {\cal Q}$
and
$$ {\cal E} =  \{ (u:P(u)) : u \in {\cal U} \} \cup  \{ (u:Q(u)) : u \in {\cal U} \} $$
where $(u:v)$ is an edge linking nodes $u$ and $v$, and $P(u) \, \equiv \, P \in {\cal P}: u \in P$.
2. the partition ${\cal P} \sqcup  {\cal Q}$  is equal to the connectivity partition of the constructed graph. 

```{r}
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
(R <- partUnion(P,Q))
partSubeq(P,Q)
partSubeq(P,R)
```

## Applications of partitions
```{r}
# to do
partBM <- function(N,P,BTypes){}
partGen <- function(P,start=FALSE){}   # start, next
# https://github.com/fxn/algorithm-combinatorics
# http://search.cpan.org/~fxn/Algorithm-Combinatorics/Combinatorics.pm
# https://ljwo.wordpress.com/2015/07/14/generating-set-partitions-replacing-virtual-functions-with-protected-inheritance/#more-349
# http://rprogramming.info/snippet/r/stirlingr_dewittpe_r
partExctract <- function(N,P,rowCls,colCls){}
partAgregate <- function(P,V){}
```
