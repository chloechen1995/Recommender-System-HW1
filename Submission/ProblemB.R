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

data = read.csv(file = "medianResult.csv", header = TRUE, sep = ",")

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

file_list = c("meanResult.csv", 'vecmodeResult.csv', 'medianResult.csv')

final_df_make = function(input_file){
  data = read.csv(file = input_file, header = TRUE, sep = ",")
  k_value = rep(1:25, each = 1000)
  new_data = cbind(k_value, data)
  
  data_split = split(new_data, new_data$k_value)
  
  mape_df = data.frame(sapply(data_split, MAPE))
  names(mape_df) = c('MAPE_Error')
  pgec_df = data.frame(sapply(data_split, PGEC))
  names(pgec_df) = c('PGEC_Accuracy')
  final_error_df = cbind(mape_df, pgec_df)
  
  graph_df = cbind(k_value = rownames(final_error_df), final_error_df)
  return (graph_df)
  
}

library(ggplot2)

plot_graph = function(dataframe, data_col, colname){
    ggplot(dataframe, aes(x = factor(k_value, levels = dataframe$k_value),
    y = data_col)) +
    geom_bar(stat = 'identity', fill = 'steelblue') +
    geom_text(aes(label = data_col), vjust = -0.5, color = "black") +
    xlab('K_Value') +
    ylab(colname)
}


create_graph = function(input_file){
    graph_df = final_df_make(input_file)
    plot_graph(graph_df, graph_df$MAPE_Error, 'MAPE_Error')
    plot_graph(graph_df, graph_df$PGEC_Accuracy, 'PGEC_Accuracy')
}

## Use lapply to run the create_graph function for other csv files (median and mode)
# lapply(file_list, create_graph)

input_file = "meanResult.csv"
graph_df = final_df_make(input_file)
plot_graph(graph_df, graph_df$MAPE_Error, 'MAPE_Error')
plot_graph(graph_df, graph_df$PGEC_Accuracy, 'PGEC_Accuracy')
