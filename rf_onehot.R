##checking out the claim from:
#http://rnowling.github.io/machine/learning/2015/08/10/random-forest-bias.html

library(randomForest)

#define functions
#converts integer to one-hot encoding
to_one_hot <- function(i, n){
  res <- rep(0, n); res[i] <- 1
  return(res)
}

#generates N random features from 1:k
randomFeatures <- function(k, N){
  sample(1:k, N, replace=T)
}

#extracts all gini values for a feature
gather_importance <- function(fname, mod){
  idx <- grep(fname, rownames(mod$importance))
  mod$importance[idx,4]
}

matrix_to_table <- function(x){

  result <- data.frame()
  for(cn in colnames(x)){
    tmp <- data.frame(cn, x[,cn])
    result <- rbind(result, tmp)
  }
  return(result)

}

##parameters
max_cat <- 32
Nsample <- 1000
Ntrees <- 100
Nrep <- 100

fnames <- c()
for (i in 2:max_cat){
  fnames <- c(fnames, paste("F",rep(i,i),"_",1:i,sep="") )
}

collect_means <- c()
collect_sums <- c()
collect_tables <- c()

for (iter in 1:Nrep){
  message("iteration: ", iter)

  ##generate a feature matrix
  X <- c()
  for(i in 2:max_cat){
    tmp <- t(sapply(randomFeatures(i, Nsample), to_one_hot, i))
    X <- cbind(X, tmp)
  }
  colnames(X) <- fnames

  #generate the response
  Y <- sample(1:2, Nsample, replace=T)

  #train the forest
  rfmod <- randomForest(X, as.factor(Y), importance=T, ntree=Ntrees)

  #means
  fmeans <- sapply(paste("F",2:max_cat,"_", sep=""), function(x){
    mean( gather_importance(x, rfmod) )
  })
  #means
  fsums <- sapply(paste("F",2:max_cat,"_", sep=""), function(x){
    sum( gather_importance(x, rfmod) )
  })
  #sums
  ftable <- data.frame()
  dev.null <- lapply(paste("F",2:max_cat,"_", sep=""), function(x){
    ftable <<- rbind(ftable, data.frame(x, gather_importance(x, rfmod)))
  })

  collect_means <- rbind(collect_means, fmeans)
  collect_sums <- rbind(collect_sums, fsums)
  collect_tables <- rbind(collect_tables, ftable)

}

par(mfrow=c(2,2))
boxplot( x...cn. ~ ., data=matrix_to_table(collect_means) , las=2, main="mean")
boxplot( x...cn. ~ ., data=matrix_to_table(collect_sums) , las=2, main="sum")
boxplot( gather_importance.x..rfmod. ~ . , data=collect_tables, las=2, main="independent")
