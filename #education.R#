#+name: education
#+header: :quality 100
#+BEGIN_SRC R :file education.jpeg :results graphics :export both :session nyc 
  nR <- nrow(df)
  EdCompleted <- df[,'EdCompleted']

  EdCompleted_data <- rep(-1,nR)

  EdCompleted_data[is.na(EdCompleted)] <- NA
  EdCompleted_data[as.numeric(EdCompleted)==1] <-4
  EdCompleted_data[as.numeric(EdCompleted)==2] <- 5
  EdCompleted_data[as.numeric(EdCompleted)==3] <- 2     
  EdCompleted_data[as.numeric(EdCompleted)==4] <- 4

  EdCompleted_data[as.numeric(EdCompleted)==5] <- 2
  EdCompleted_data[as.numeric(EdCompleted)==6] <- 3    
  EdCompleted_data[as.numeric(EdCompleted)==7] <- 4
  EdCompleted_data[as.numeric(EdCompleted)==8] <- 1

  EdCompleted_data[as.numeric(EdCompleted)==9] <- 2
  EdCompleted_data[as.numeric(EdCompleted)==10] <- NA
  EdCompleted_data[as.numeric(EdCompleted)==11] <- 3
  EdCompleted_data[as.numeric(EdCompleted)==12] <- 3
EdCompleted_data[as.numeric(EdCompleted)==13] <- 4


  EdCompletedf <- factor(EdCompleted_data,levels = 1:5,
                           labels=c("lt-hs","ged","some-coll",
                               "Assoc","BA+"), ordered=TRUE)


  EdCompletedTtl <- table(EdCompletedf)
  EdCompletedPct <- round(100*prop.table(EdCompletedTtl),0)
  #jpeg("resp_marginals/ctOnPol.jpg")

  barplot(EdCompletedPct, 
main="Education completed percentage sample",
col = "steel blue")

  #dev.off()
#+end_src
