#+name: ThreatenedFacebook
#+header: :quality 100
#+BEGIN_SRC R :file ThreatenedFacebook.jpeg :results graphics :export both :session nyc 
  nR <- nrow(df)
  ThreatenedFacebookCount <- df[,'ThreatenedFacebook']

  ThreatenedFacebookCount_data <- rep(-1,nR)

  ThreatenedFacebookCount_data[is.na(ThreatenedFacebookCount)] <- NA
  ThreatenedFacebookCount_data[as.numeric(ThreatenedFacebookCount)==1] <-NA
  ThreatenedFacebookCount_data[as.numeric(ThreatenedFacebookCount)==2] <- 1
  ThreatenedFacebookCount_data[as.numeric(ThreatenedFacebookCount)==3] <- 2     
  ThreatenedFacebookCount_data[as.numeric(ThreatenedFacebookCount)==4] <- 11

  ThreatenedFacebookCount_data[as.numeric(ThreatenedFacebookCount)==5] <- 3
  ThreatenedFacebookCount_data[as.numeric(ThreatenedFacebookCount)==6] <- 4    
  ThreatenedFacebookCount_data[as.numeric(ThreatenedFacebookCount)==7] <- 5
  ThreatenedFacebookCount_data[as.numeric(ThreatenedFacebookCount)==8] <- 6

  ThreatenedFacebookCount_data[as.numeric(ThreatenedFacebookCount)==9] <- 7
  ThreatenedFacebookCount_data[as.numeric(ThreatenedFacebookCount)==10] <- 8
  ThreatenedFacebookCount_data[as.numeric(ThreatenedFacebookCount)==11] <- 9
  ThreatenedFacebookCount_data[as.numeric(ThreatenedFacebookCount)==12] <- 10
ThreatenedFacebookCount_data[as.numeric(ThreatenedFacebookCount)==13] <- 12


  ThreatenedFacebookCountf <- factor(ThreatenedFacebookCount_data,levels = 1:12,
                           labels=c(0:9,"10+","Dec"), ordered=TRUE)


  ThreatenedFacebookCountTtl <- table(ThreatenedFacebookCountf)
  ThreatenedFacebookCountPct <- round(100*prop.table(ThreatenedFacebookCountTtl),0)
  #jpeg("resp_marginals/ctOnPol.jpg")

  barplot(ThreatenedFacebookCountPct, main="Percent aware of threats on Facebook", col = "steel blue")

  #dev.off()
#+end_src
