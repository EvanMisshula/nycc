#+name: helped_by_neighbor
#+header: :quality 100
#+BEGIN_SRC R :file helped_by_neighbor.jpeg :results graphics :export both :session nyc 
  nR <- nrow(df)
  HelpedByNeighbor <- df[,'HelpedByNeighbor']

  HelpedByNeighbor_data <- rep(-1,nR)

  HelpedByNeighbor_data[is.na(HelpedByNeighbor)] <- NA
  HelpedByNeighbor_data[as.numeric(HelpedByNeighbor)==1] <-NA
  HelpedByNeighbor_data[as.numeric(HelpedByNeighbor)==2] <-2
  HelpedByNeighbor_data[as.numeric(HelpedByNeighbor)==3] <- 3
  HelpedByNeighbor_data[as.numeric(HelpedByNeighbor)==4] <- 4

  HelpedByNeighbor_data[as.numeric(HelpedByNeighbor)==5] <- NA
  HelpedByNeighbor_data[as.numeric(HelpedByNeighbor)==6] <- NA
  HelpedByNeighbor_data[as.numeric(HelpedByNeighbor)==7] <- NA
  HelpedByNeighbor_data[as.numeric(HelpedByNeighbor)==8] <- 1

  HelpedByNeighborf <- factor(HelpedByNeighbor_data,levels = 1:4,
                           labels=c("last-wk","last-month",
                               "last-year","never"), ordered=TRUE)


  HelpedByNeighborTtl <- table(HelpedByNeighborf)
  HelpedByNeighborPct <- round(100*prop.table(HelpedByNeighborTtl),0)
  #jpeg("resp_marginals/ctOnPol.jpg")

  barplot(HelpedByNeighborPct, main="Percent helped a neighbor in the last 12 months",col = "steel blue")

  #dev.off()
#+end_src
