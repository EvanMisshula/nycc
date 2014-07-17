
#assign
rscore <- list()
#rscore.h <- list()
#rscore.e  <- list()
rscorer <- list()
scenarios <- list()
pctDecline <- list()
counts <- list()
#counts.h <- list()
#counts.e <- list()
pctRDecline <- list()
subt <- list()

for(j in 23:38) {
    scenarios[[varNames[j]]] <- rep(-1,nR)
    rscore[[varNames[j]]] <- -1
    rscorer[[varNames[j]]] <- -1
    pctRDecline[[varNames[j]]] <- -1
    counts[[varNames[j]]] <- rep(-1,nR)
    subt[[varNames[j]]] <- -1
    scenarios[[varNames[j]]] <- rep(-1,nR)
    cat(class(df[,j]),"\n",sep="")
}
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


for(j in (23:38)) {
    
    scenarios[[j-22]]["Ignore"==trim(levels(df[,j])[df[,j]])] <- 1
    scenarios[[j-22]]["React verbally"==trim(levels(df[,j])[df[,j]])] <- 2
    scenarios[[j-22]]["Get Physical"==trim(levels(df[,j])[df[,j]])] <- 3
    scenarios[[j-22]]["Pull a weapon"==trim(levels(df[,j])[df[,j]])] <- 4
    scenarios[[j-22]]["Use a weapon"==trim(levels(df[,j])[df[,j]])] <- 5
    scenarios[[j-22]]["Not sure/Decline"==trim(levels(df[,j])[df[,j]])] <- 6
    rscorer[[j-22]] <- median(scenarios[[j-22]][scenarios[[j-22]]!=6])
    rscore[[j-22]]  <- mean(scenarios[[j-22]][scenarios[[j-22]]!=6])
#    rscore.h[[j-22]]<- mean(scenarios[[j-22]][scenarios[[j-22]]!=6 &
#                                              df$location=="Harlem"])
#    rscore.e[[j-22]]<- mean(scenarios[[j-22]][scenarios[[j-22]]!=6 &
#                                              df$location=="EastNY"])

    pctRDecline[[j-22]] <- round(sum(scenarios[[j-22]]==6)/length(scenarios[[j-22]]),2)
    
    scenarios[[j-22]]<- factor(scenarios[[j-22]],
                          levels = 1:6,
                          labels = c(
                              "Ignore",
                              "VReact",
                              "PReact",
                              "DWeapon",
                              "UWeapon",
                              "Decline"
                              ),ordered=TRUE)
    counts[[j-22]] <- table(scenarios[[j-22]])
#    counts.h[[j-22]]<- table(scenarios[[j-22]][df$location=="Harlem"])
#    counts.e[[j-22]]<- table(scenarios[[j-22]][df$location=="EastNY"])
}

# SBX

for(j in (23:38)) {
    
    scenarios[[j-22]][381:580][==levels(df[,j])[df[,j]]] <- 1
    scenarios[[j-22]][381:580][==levels(df[,j])[df[,j]]] <- 1
    scenarios[[j-22]][381:580][==levels(df[,j])[df[,j]]] <- 2
    scenarios[[j-22]][381:580][==levels(df[,j])[df[,j]]] <- 2
    scenarios[[j-22]][381:580][==levels(df[,j])[df[,j]]] <- 3
    scenarios[[j-22]][381:580][==levels(df[,j])[df[,j]]] <- 3
    scenarios[[j-22]][381:580][==levels(df[,j])[df[,j]]] <- 4
    scenarios[[j-22]][381:580][==levels(df[,j])[df[,j]]] <- 4
    scenarios[[j-22]][381:580][==levels(df[,j])[df[,j]]] <- 5
    scenarios[[j-22]][381:580][==levels(df[,j])[df[,j]]] <- 5
    scenarios[[j-22]][381:580][==levels(df[,j])[df[,j]]] <- 6
    rscorer[[j-22]] <- median(scenarios[[j-22]][scenarios[[j-22]]!=6])
    rscore[[j-22]]  <- mean(scenarios[[j-22]][scenarios[[j-22]]!=6])
    pctRDecline[[j-22]] <- round(sum(scenarios[[j-22]]==6)/length(scenarios[[j-22]]),2)
    
    scenarios[[j-22]]<- factor(scenarios[[j-22]],
                          levels = 1:6,
                          labels = c(
                              "Ignore",
                              "VReact",
                              "PReact",
                              "DWeapon",
                              "UWeapon",
                              "Decline"
                              ),ordered=TRUE)
    counts[[j-22]] <- table(scenarios[[j-22]])
#    counts.h[[j-22]]<- table(scenarios[[j-22]][df$location=="Harlem"])
#    counts.e[[j-22]]<- table(scenarios[[j-22]][df$location=="EastNY"])
}


for(j in (24:39)) {
    jpeg(paste("scenario_graphs/",varNames[j],".jpg",sep = ""))
    subt[[j-23]] <- paste("r-score = ",
                  round(rscore[[j-23]],2),", ",
                  levels(scenarios[[j-23]])[rscorer[[j-23]]],", ",
                  "rank = ",17-(rank(unlist(rscore),ties.method = "random")[j-23]),
                  sep = "")
    cat(subt[[j-23]],"\n")                                     
    barplot(counts[[j-23]], main=varNames[j],col = "blue",sub=subt[[j-23]])
    dev.off()
}

medianAction <- levels(scenarios[[1]])[unlist(rscorer)[order(unlist(rscore),decreasing = TRUE)]]
scenes <- unlist(rscore)[order(unlist(rscore),decreasing = TRUE)]
scene_rank <- data.frame(scenarios=scenes,medianAction=medianAction)

write.table(scene_rank,file="scenario_graphs/rank.txt",sep="\t")
