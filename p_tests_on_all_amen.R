data <- data_short

amen_names <- c()
res <- c()

for (i in 1:length(colnames(data))) {
  if (substr(colnames(data[i]), 1, 5) == "amen_") {
    
    amen_names <- c(amen_names, colnames(data[i]))
    
    t1 <- 0.99

    try(t1 <- (t.test(data_short[data_short[, colnames(data[i])] == "TRUE", "price_log"], 
                      data_short[data_short[, colnames(data[i])]  == "FALSE", "price_log"]))$p.value)
    
    res <- c(res, t1)
    
  }
}

res_data <- data.frame(amen = amen_names, results = res)

res_data <- res_data[order(res_data$results),] 
