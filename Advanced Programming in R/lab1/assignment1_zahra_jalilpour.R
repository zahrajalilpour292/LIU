

name <- "ZAHRA JALIL POUR"
liuid <- "zahja096@student.liu.se"


####1-1-1#####
library(Matrix)
my_num_vector <- function(){
  num_vector <- c(log(11,10) , cos(pi/5), exp(pi/3), ((1173%%7)/19))
  return(num_vector)
}


####1-1-2####
filter_my_vector <- function(x, leq){
  n <- length(x)
  for (i in 1:n){
   if (x[i] >= leq)  x[i]<- NA 
 }
  return(x)
}


####1-1-3 ######
dot_prod <- function(a, b) {
  n <- length(a)
  m <- length(b)
  if(m != n) stop("invalid argument")
  sum1 <- 0
  for(i in 1:n) sum1 <- sum1 + a[i]*b[i]
  sum1
}

###### 1-1-4#####
approx_e <- function(N){
  sum1 <- 0
  for (i in 0:N){
  
    sum1 <- sum1+1/factorial(i)}
  
  return(sum1)
}


##### 1-2-1 my_magic_matrix ####
my_magic_matrix <- function(){
  magic_matrix <- matrix(c(4,3,8,9,5,1,2,7,6), ncol=3)
  return(magic_matrix)
}


#####1-2-2 #####
calculate_elements <- function(A){
  n <-  ncol(A)
  m <-  nrow(A)
  s <- n*m
  return(s)
}

##### 1-2-3 row_to_zero(A,i)#######

row_to_zero <- function(A, i){
  A[i,] <- 0
  return(A)
}


#### 1-2-4  add_element_to_matrix #####
add_elements_to_matrix <- function(A, x, i , j){
  A[i,j] <- x+A[i,j]
  return(A)
}


##### 1-3 list####
##### 1-3-1 my_magic_list####
my_magic_list <- function(){
  new_list <- list(info="my own list", my_num_vector(), my_magic_matrix())
  return(new_list)
}


#### 1-3-2 change_info #####
change_info <- function(x , text){
   
   x$info<- text
   return(x)
}

#### 1-3-3 add_note ####
add_note <- function(x, note){
  x$note <- note
  return(x)
}



#### 1-3-4 sum_numeric_part #####
sum_numeric_parts <- function(x){
  new_list <- as.numeric(unlist(x))
  result <- sum(new_list, na.rm=TRUE)
  return(result)
}
  

#### 1-4 data.frames ####
#### 1-4-1  my_data.frame #####
my_data.frame <- function(){
  new_data.frame <- data.frame(id= 1:3, name= c("John","Liza","Azra"), income = c(7.30, 0.00, 15.21), rich=c(FALSE,FALSE,TRUE))
  return(new_data.frame)
}

#### 1-4-2 sort_head #####******
sort_head <- function(df, var.name, n){
  df <- df[order(df[var.name],decreasing=TRUE),]
  return (head(df,n))
}



##### 1-4-3 #####
add_median_variable <- function(df,j){
  df <- data.frame(df)
  compared_to_median <- function(x, median){
    if (x < median){
      return("Smaller")
    }
    if (x == median){
      return("Median")
    }
    if (x>median){
      return("Greater")
    }
    
  }
  new_column <-   lapply(df[[j]], compared_to_median,  median(df[[j]]))
  df[['compared_to_median']] <- unlist(new_column)
  return(df)
}

######
analyze_columns <- function(df, j){
  df <- data.frame(df)
  final_result <- list()
  col_names <- names(df[j])
  for ( i in seq_along(j)){
    
    mean1 <- mean(df[[i]]) 
    med1 <- median(df[[i]])
    sd1 <- sd(df[[i]])
    list_name <- col_names[i]
    final_result <- append(final_result,list(c(mean1,med1,sd1)))
    names(final_result[[i]]) <- c("mean", "median", "sd")
    names(final_result)[i]<- list_name
    
  }
  cor_column <- list(cor(df[j]))
  names(cor_column) <- "correlation_matrix"
  final_result <- append(final_result, cor_column)
  return(final_result)
  
}  

