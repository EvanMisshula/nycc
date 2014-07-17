#+name: timeOutside
#+header: :quality 100
#+BEGIN_SRC R :file timeOutside.jpeg :results graphics :export both :session nyc 
timeOutside <- as.numeric(levels(df$TimeOutsideHere)[as.numeric(df$TimeOutsideHere)])

pctTimeOutside <- round(100*prop.table(table(timeOutside)),0)

  barplot(pctTimeOutside, 
main="Respondent time outside the nighborhood percentage sample",
col = "steel blue")

  #dev.off()
#+end_src
