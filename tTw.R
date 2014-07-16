#+name: ThreatenedTwitter
#+header: :quality 100
#+BEGIN_SRC R :file ThreatenedTwitter.jpeg :results graphics :export both :session nyc 
  nR <- nrow(df)
  ThreatenedTwitterCount <- df[,'ThreatenedTwitter']

  ThreatenedTwitterCount_data <- rep(-1,nR)

  ThreatenedTwitterCount_data[is.na(ThreatenedTwitterCount)] <- NA
  ThreatenedTwitterCount_data[as.numeric(ThreatenedTwitterCount)==1] <-NA
  ThreatenedTwitterCount_data[as.numeric(ThreatenedTwitterCount)==2] <- 1
  ThreatenedTwitterCount_data[as.numeric(ThreatenedTwitterCount)==3] <- 2     
  ThreatenedTwitterCount_data[as.numeric(ThreatenedTwitterCount)==4] <- 11

  ThreatenedTwitterCount_data[as.numeric(ThreatenedTwitterCount)==5] <- 3
  ThreatenedTwitterCount_data[as.numeric(ThreatenedTwitterCount)==6] <- 4    
  ThreatenedTwitterCount_data[as.numeric(ThreatenedTwitterCount)==7] <- 5
  ThreatenedTwitterCount_data[as.numeric(ThreatenedTwitterCount)==8] <- 6

  ThreatenedTwitterCount_data[as.numeric(ThreatenedTwitterCount)==9] <- 7
  ThreatenedTwitterCount_data[as.numeric(ThreatenedTwitterCount)==10] <- 8
  ThreatenedTwitterCount_data[as.numeric(ThreatenedTwitterCount)==11] <- 9
  ThreatenedTwitterCount_data[as.numeric(ThreatenedTwitterCount)==12] <- 10
ThreatenedTwitterCount_data[as.numeric(ThreatenedTwitterCount)==13] <- 12


  ThreatenedTwitterCountf <- factor(ThreatenedTwitterCount_data,levels = 1:12,
                           labels=c(0:9,"10+","Dec"), ordered=TRUE)


  ThreatenedTwitterCountTtl <- table(ThreatenedTwitterCountf)
  ThreatenedTwitterCountPct <- round(100*prop.table(ThreatenedTwitterCountTtl),0)
  #jpeg("resp_marginals/ctOnPol.jpg")

  barplot(ThreatenedTwitterCountPct, 
main="Percent aware and number of threats on Twitter", 
col = "steel blue")

  #dev.off()
#+end_src
