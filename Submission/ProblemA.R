# Problem A
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
