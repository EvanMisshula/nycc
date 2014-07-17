#+name: age
#+header: :quality 100
#+BEGIN_SRC R :file age.jpeg :results graphics :export both :session nyc 
age <- as.numeric(levels(df$age)[as.numeric(df$age)])

pctAge <- prop.table(table(age))

pctAgeP <- round(100*prop.table(age),0)
barplot(pctAgeP,main = "Pct of respondents at each age", col="steel blue")
#+END_SRC
