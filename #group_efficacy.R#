#+name: efficacy_group
#+header: :quality 100
#+BEGIN_SRC R :file efficacy_group.jpeg :results graphics :export both :session nyc 

    efficacy_components <- c(
    "Helped a neighbor","Helped by a neighbor",
"Conflict with a neighbor (reversed)")

    ctdata <- matrix(c(HelpedNeighborPct,
                     HelpedByNeighborPct,
                     lastNeighborConflictTimePct),byrow=TRUE,nrow=3)
    colnames(ctdata)<-names(HelpedNeighborPct)
    row.names(ctdata)<-efficacy_components
    #pdf("grouped_scenario/civilTrust.pdf")
    #jpeg("grouped_scenario/civilTrust.jpg")
    city_col <- c("darkblue",
                    "steelblue",
                    "red")


  ctdataPct <- round(100*prop.table(as.table(ctdata),1),0)
    barplot(ctdataPct,
            main=paste("Construct of efficacy"),
            col=city_col,
            beside = TRUE)
    legend(x="top",legend = efficacy_components, fill = city_col,cex = 1.0)

    #dev.off()
#+END_SRC
