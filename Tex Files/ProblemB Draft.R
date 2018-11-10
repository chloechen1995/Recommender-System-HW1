library(rectools)

getInstEval()

# Setup functions
# Function for getting mode from a vector
vecmode <- function(x) {
  vals <- unique(x)
  freq <- tabulate(match(x, vals))
  vals[which.max(freq)]
}

# Edited predict.usrData function with locMeasure parameter
predict.usrData <- function (origData, newData, newItem, k, wtcovs = NULL, wtcats = NULL, locMeasure = mean) 
{
  traincovs <- !is.null(origData$usrCovs)
  newcovs <- !is.null(newData$cvrs)
  if (!(traincovs + newcovs %in% c(0, 2))) 
    stop("mismatch in having/not having covars, orig and new data")
  traincats <- !is.null(origData$usrCats)
  newcats <- !is.null(newData$cats)
  if (!(traincats + newcats %in% c(0, 2))) 
    stop("mismatch in having/not having cats, orig and new data")
  checkNewItem <- function(oneUsr) {
    whichOne <- which(oneUsr$itms == newItem)
    if (length(whichOne) > 1) {
      stop("same user/item pair encountered more than once")
    }
    if (length(whichOne) == 0) {
      return(c(NA, NA))
    }
    else return(c(whichOne, oneUsr$ratings[whichOne]))
  }
  found <- as.matrix(sapply(origData, checkNewItem))
  whoHasIt <- which(!is.na(found[1, ]))
  if (is.null(whoHasIt) | length(whoHasIt) == 0) 
    return(NA)
  origDataRatedNI <- origData[whoHasIt]
  found <- found[, whoHasIt, drop = FALSE]
  onecos <- function(y) cosDist(newData, y, wtcovs, wtcats)
  cosines <- sapply(origDataRatedNI, onecos)
  findKnghbourRtng <- function(ki) {
    ki <- min(ki, length(cosines))
    nearby <- order(cosines, decreasing = FALSE)[1:ki]
    locMeasure(as.numeric(found[2, nearby]))
  }
  sapply(k, findKnghbourRtng)
}

# Setup data
set.seed(9999)
testidxs <- sample(1:nrow(ivl),1000)
testset <- ivl[testidxs,]
trainset <- ivl[-testidxs,]
formedTrain <- formUserData(trainset[,1:3])
formedTest <- formUserData(testset[,1:3])

# Update user data
updateUserData <- function(oldData, newData)
{
  resultData <- oldData
  newData <- formUserData(newData[,1:3])
  for (i in 1:length(newData)) {
    hasSameUserID = FALSE
    print(newData[[i]]$userID)
    for (j in 1:length(resultData)) {
      if (resultData[[j]]$userID == newData[[i]]$userID) {
        print("Has same user ID")
        print(resultData[[j]]$itms)
        resultData[[j]]$itms = append(resultData[[j]]$itms, newData[[i]]$itms)
        resultData[[j]]$ratings = append(resultData[[j]]$ratings, newData[[i]]$ratings)
        print(resultData[[j]]$itms)
        hasSameUserID = TRUE
        break
      }
    }
    if (hasSameUserID == FALSE) {
      print("Does not have same ID")
      resultData[[length(resultData)+1]] <- newData[[i]]
    }
  }
  resultData
}
updatedFormedData <- updateUserData(oldData = formedTrain, newData = testset)

trainid <- t(trainset[1]) 
isin <- function(y) {!(y$userID %in% trainid)}z
z <- which(sapply(formedTest, isin))


updateUserData <- function(oldData, newData)
{
  resultData <- oldData
  newData <- formUserData(newData[,1:3])
  for (i in 1:length(newData)) {
    targetUserID <- newData[[i]]$userID
    indexStorageInOldData <- which(sapply(resultData, function(x) {x$userID == targetUserID}))
    print(indexStorageInOldData)
    if (length(indexStorageInOldData) == 0) {
      print("User ID not found")
      resultData[[length(resultData)+1]] <- newData[[i]]
    } else {
      print("Has same user ID")
      print(resultData[[indexStorageInOldData]]$itms)
      resultData[[indexStorageInOldData]]$itms = append(resultData[[indexStorageInOldData]]$itms, newData[[i]]$itms)
      resultData[[indexStorageInOldData]]$ratings = append(resultData[[indexStorageInOldData]]$ratings, newData[[i]]$ratings)
      print(resultData[[indexStorageInOldData]]$itms)
    }
  }
  resultData
}
updatedFormedData <- updateUserData(oldData = formedTrain, newData = testset)

d <- data.frame(uID=c(1,2,1),iID=c(5,1,8),rat=c(3,3,5))
db <-formUserData(d)
ndt <- data.frame(uID=c(5,2),iID=c(3,6),rat=c(4,1))
dbb <- formUserData(ndt)
newdb <- updateUserData(db,ndt)

## calculate the MAPE error
MAPE = function(df_list) {
  error_percentage = sum(abs(df_list['difference'])/abs(df_list['actual']))/nrow(df_list) 
  return (round(error_percentage, 3))
}

## calculate the PGEC error
PGEC = function(df_list) {
  accuracy = length(which((df_list['difference'] == 0.0) == TRUE))/ nrow(df_list)
  return (accuracy)
}

#Initialize relevant variables
usr_id <- vector()
actual <- vector()
predict <- vector()
difference <- vector()
#for each k value, do the following process once
#for each user in testset, predict each item and put the data in frame
lapply(1:25, function(k) {
  lapply(formedTest, function(usr) {
    itmNumList <- 1:length(usr$ratings)
    lapply(itmNumList, function(num) {
      #remove the record from the user to predict
      holdRating = usr$ratings[[num]] 
      tmpUsr <- usr
      tmpUsr$itms <- usr$itms[-num] 
      tmpUsr$ratings <- usr$ratings[-num] 
      result <- predict(formedTrain,tmpUsr,usr$itms[num],k,locMeasure=mean)
      diff = result - holdRating
      #append(usr_id, usr$userID)
      usr_id <<- c(usr_id, usr$userID)
      actual <<- c(actual, holdRating)
      predict <<- c(predict, result)
      difference <<- c(difference, diff)
    })
  })
}) 

kNNResult <- data.frame(
  usr_id,
  actual,
  predict,
  difference
)

write.csv(kNNResult, file = "meanResult.csv")

data = read.csv(file = "vecmodeResult.csv", header = TRUE, sep = ",")

## calculate the MAPE error
MAPE = function(df_list) {
  error_percentage = sum(abs(df_list['difference'])/abs(df_list['actual']))/nrow(df_list) 
  return (round(error_percentage, 3))
}

## calculate the PGEC error
PGEC = function(df_list) {
  accuracy = length(which((df_list['difference'] == 0.0) == TRUE))/ nrow(df_list)
  return (accuracy)
}

k_value = rep(1:25, each = 1000)
new_data = cbind(k_value, data)

data_split = split(new_data, new_data$k_value)

mape_df = data.frame(sapply(data_split, MAPE))
names(mape_df) = c('MAPE_Error')
pgec_df = data.frame(sapply(data_split, PGEC))
names(pgec_df) = c('PGEC_Accuracy')
final_error_df = cbind(mape_df, pgec_df)

graph_df = cbind(k_value = rownames(final_error_df), final_error_df)

library(ggplot2)

plot_graph = function(col_name){
  ggplot(graph_df, aes(x = factor(k_value, levels = graph_df$k_value), 
                       y = col_name)) + 
    geom_bar(stat = 'identity', fill = 'steelblue') + 
    geom_text(aes(label = col_name), vjust = -0.5, color = "black") +
    xlab('K_Value') + ylab('PGEC_Accuracy')
}

plot_graph(graph_df$MAPE_Error)
plot_graph(graph_df$PGEC_Accuracy)
