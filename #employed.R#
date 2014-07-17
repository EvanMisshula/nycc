#+name: employed
#+header: :quality 100
#+BEGIN_SRC R :file employed.jpeg :results graphics :export both :session nyc 
  nR <- nrow(df)
  employed <- df[,'employed']

  employed_data <- rep(-1,nR)

  employed_data[is.na(employed)] <- NA
  employed_data[as.numeric(employed)==1] <- NA
  employed_data[as.numeric(employed)==2] <- 3
  employed_data[as.numeric(employed)==3] <- NA     
  employed_data[as.numeric(employed)==4] <- 2

  employed_data[as.numeric(employed)==5] <- 1


  employedf <- factor(employed_data,levels = 1:3,
                           labels=c("unemployed","part-time","full-time"), ordered=TRUE)


  employedTtl <- table(employedf)
  employedPct <- round(100*prop.table(employedTtl),0)
  #jpeg("resp_marginals/ctOnPol.jpg")

  barplot(employedPct, 
main="Employment status completed percentage sample",
col = "steel blue")

  #dev.off()
#+end_src
