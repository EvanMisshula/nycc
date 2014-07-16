#+name: AttendAVRallyL12mo
#+header: :quality 100
#+BEGIN_SRC R :file AttendAVRallyL12mo.jpeg :results graphics :export both :session nyc 
  nR <- nrow(df)
  AttendAVRallyL12mo <- df[,'AttendAVRallyL12moSmedia']

  AttendAVRallyL12mo_data <- rep(-1,nR)

  AttendAVRallyL12mo_data[is.na(AttendAVRallyL12mo)] <- NA
  AttendAVRallyL12mo_data[as.numeric(AttendAVRallyL12mo)==1] <-1
  AttendAVRallyL12mo_data[as.numeric(AttendAVRallyL12mo)==2] <- 2
  AttendAVRallyL12mo_data[as.numeric(AttendAVRallyL12mo)==3] <- 2     
  AttendAVRallyL12mo_data[as.numeric(AttendAVRallyL12mo)==4] <- 2

  AttendAVRallyL12mo_data[as.numeric(AttendAVRallyL12mo)==5] <- 2
  AttendAVRallyL12mo_data[as.numeric(AttendAVRallyL12mo)==6] <- 2    
  AttendAVRallyL12mo_data[as.numeric(AttendAVRallyL12mo)==7] <- 2
  AttendAVRallyL12mo_data[as.numeric(AttendAVRallyL12mo)==8] <- 2

  AttendAVRallyL12mo_data[as.numeric(AttendAVRallyL12mo)==9] <- 2
  AttendAVRallyL12mo_data[as.numeric(AttendAVRallyL12mo)==10] <- 2
  AttendAVRallyL12mo_data[as.numeric(AttendAVRallyL12mo)==11] <- 2
  AttendAVRallyL12mo_data[as.numeric(AttendAVRallyL12mo)==12] <- 2
AttendAVRallyL12mo_data[as.numeric(AttendAVRallyL12mo)==13] <- NA


  AttendAVRallyL12mof <- factor(AttendAVRallyL12mo_data,levels = 1:2,
                           labels=c("No","Yes"), ordered=TRUE)


  AttendAVRallyL12moTtl <- table(AttendAVRallyL12mof)
  AttendAVRallyL12moPct <- round(100*prop.table(AttendAVRallyL12moTtl),0)
  #jpeg("resp_marginals/ctOnPol.jpg")

  barplot(AttendAVRallyL12moPct, 
main="Percent attended an anti-violence rally",
col = "steel blue")

  #dev.off()
#+end_src
