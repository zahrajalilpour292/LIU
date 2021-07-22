
name <- "zahra jalilpour"
liuid <- "zahja096@student.liu.se"

##### 1-1-1 sheldon_game #####
sheldon_game <- function ( player1 , player2 ){
  sheldon_list <- c(1:5)
  names(sheldon_list) <- c("rock", "lizard", "spock", "scissors", "paper")
  stopifnot(player1 %in% names(sheldon_list), player2 %in% names(sheldon_list))
  var_1 <- which (names(sheldon_list) %in% player1)
  var_2 <- which (names(sheldon_list) %in% player2)
  dif = (var_2) - (var_1)
  len_s <- length(sheldon_list)
  if (dif < 0){
    dif <- dif + len_s
  }
  if ((dif == 1) || (dif ==3)){
    return ("player 1 wins!")
  }
  if ((dif == 2)||(dif ==4)){
    return("player 2 wins!")
  }
  if (dif == 0){
    return ("draw!")
  }
  
  
}



##### 1-2####
####1-2-1 my_moving_median()######
my_moving_median <- function(x, n, ...){
  x <- as.vector(x)
  #dots <- list(...)
  dots <- as.list(substitute(list(...)))[-1L]
  
  #out <- rep(NA)
  out <- c()
  stopifnot(is.atomic(x) ||is.list(x))
  stopifnot(is.numeric(n))
  
  for ( i in 1:(length(x)-n) ){
    if (length(dots)==0){
      out[i] <- median(x[i:(i+n)], na.rm = FALSE)}
    else
      out[i] <- median(x[i:(i+n)], na.rm = TRUE)}
  
  out
}

##### 1-2-2 #####
for_mult_table <- function (from, to){
  dim <- to-from+1
  new_matrix <- matrix(ncol=dim,nrow=dim)
  k <- 1
  for(i in from:to ){
    s <- 1
    for(j in from:to){
      new_matrix[k,s] <- (i*j)
      
      if(s<=dim)
      {
        s<-s+1
      }
      
    }
    if(k<=dim)
    {
      k<-k+1
    }}
  
  #rownames(x) <- colnames(x) <- one_to
  new_matrix
}



##### 1-3-1####
find_cumsum <- function(x, find_sum){
  #x <- as.vector(x)
  sum <- 0
  len <- length(x)
  i <- 1
  stopifnot(is.numeric(x))
  
  sum <- sum+x[i]
  while(i< len){
    i <- i+1
    
    sum <- sum+x[i]
    if (sum > find_sum){ 
      
      break
    }
  }
  
  return(sum)
  
}


#####1-3-2 ####

while_mult_table <- function (from, to){
  dim <- to-from+1
  new_matrix <- matrix(ncol=dim,nrow=dim)
  #id <- from:to
  k <- 1
  i <- from
  while(i <= to){
    s <- 1
    j <- from
    while(j <= to){
      new_matrix[k,s] <- (i*j)
      
      if(s<=dim)
      {
        s<-s+1
      }
      j <- j+1
    }
    if(k<=dim)
    {
      k<-k+1
    }
    i <- i+1
  }
  
  #rownames(x) <- colnames(x) <- one_to
  new_matrix
}



##### 1-4-1 repeat_find_cumsum #####
repeat_find_cumsum <- function(x, find_sum){
  #x <- as.vector(x)
  sum <- 0
  len <- length(x)
  i <- 1
  stopifnot(is.numeric(x))
  repeat{
    sum <- sum+x[i]
    i <- i+1
    if (i> len || sum > find_sum ){
      break}
  }
  
  sum
}


##### repeat_moving_median#####
repeat_my_moving_median <- function(x, n, ...){
  x <- as.vector(x)
  #n <- as.numeric(n)
  dots <- list(...)
  len <- length(x)
  out <- c()
  i <- 1
  stopifnot(is.atomic(x) ||is.list(x))
  stopifnot(is.numeric(n))
  repeat{
    out[i] <- median(x[i:(i+n)], na.rm = FALSE)
    if (length(dots)!=0){
      out[i] <- median(x[i:(i+n)], na.rm = TRUE)}
    i <- i+1
    if (i > (len-n))
      break}
  out
}



####### environment ######
in_environment <- function(env){
  hh <- ls(env)
  return(hh)}


######COV#####
cov <- function(X){
  #X <- as.data.frame(X)
  #x <-c()  
  stopifnot(length(X)!=1)
  out <- lapply(X, function(x) (sd(x) / mean(x)))
  out <- unlist(out)
  return(out)
  
}


##### moment #####
moment <- function(i) {
  
  stopifnot(is.numeric(i))
  function(x) {
    stopifnot(is.atomic(x) ||is.list(x))
    out<- c()
    for(j in seq_along(x)){
      out[j] <- ((x[j]-mean(x))^i)}
    med <-mean(out) 
    return(med)
  }
  
}

