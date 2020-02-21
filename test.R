#using r to implement weighted knn function and find out the accuracy.
#euclideanDist <- function(x, y) default distance function 
#wknn <- function(test, train, k_value) based on the k value, find out the prediction of 
#                                       the test point


setwd("~/Desktop/knn")
#read.table('val.csv', header=T, sep=',')

val<- read.csv('val.csv', header=T, sep=',',stringsAsFactors = FALSE)
train <- read.csv('train.csv', header=T, sep=',',stringsAsFactors = FALSE)
test <- read.csv('test.csv', header=T, sep=',',stringsAsFactors = FALSE)

#val_class<-c()


#deleting the fiirst col of val, which contains the classcification 
val <- val[-1]
train<-train[-1]
test<-test[-1]



normalize <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x))) 
}

#val<-as.data.frame(lapply(val[1:10], normalize))
#train<-as.data.frame(lapply(train[1:10], normalize))
#test<-as.data.frame(lapply(test[1:10], normalize))


##eucleain funticon to calculate the distance
euclideanDist <- function(x, y){
  eu_distance = 0
  for(i in c(1:(length(x)-1) ))
  {
    eu_distance = eu_distance + (x[[i]]-y[[i]])^2
  }
  eu_distance = sqrt(eu_distance)
  return(eu_distance)
}



##
wknn <- function(test, train, k_value){ 
  result <- c() #empty vector which stores the result
  for(i in c(1:nrow(test))){   
    dist =c() 
    class = c()
    class2 = class4 = 0 #classcification
    count = 0
    for(j in c(1:nrow(train))){ #for training set
      dist <- c(dist, euclideanDist(test[i,], train[j,])) #this vector will stores the euclideanDist function 
      #with test and train 
      class <- c(class, as.integer(train[j,][[10]])) #clomn 11 is the class number 2,4
    }
    
    eu <- data.frame(class, dist)
    eu <- eu[order(eu$dist),]  #sort the eu data frame
    eu <- eu[1:k,]               #take the first k number from the data fram 
    for(k in c(1:nrow(eu))){
      if(as.integer(eu[k,"class"]) == 2){
        class2 = class2+1/nrow(eu) #weighted knn
      }
      else
        class4 = class4+1/nrow(eu)
    }
    if(class2 > class4){        
      result <- c(result, 2)
    }
    else if(class2 < class4){
      result <- c(result, 4)
    }
  }
  print(result)
  return(result)
}


#k=11
k<-range(1,11)
result <- wknn(test, train, k) 
test[,2]<-result
count = 0
for(j in c(1:nrow(test)))
{
  if(test[j,2] == test[j,10])
  {
    count = count+1
  }
  acc = count/nrow(test)
  {
  write.table(acc, file = "acc.txt", sep = ",",
              row.names = FALSE)
  }
  print(acc)
}
