
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

for(j in (23:38)) {
    rscorer[[j-22]] <- median(dfnbx[,j-22][dfnbx[,j-22]!=6],na.rm = TRUE)
    rscore[[j-22]]  <- mean(dfnbx[,j-22][dfnbx[,j-22]!=6],na.rm = TRUE)

    pctRDecline[[j-22]] <- round(sum(dfnbx[,j-22]==6)/length(dfnbx[,j-22]),2)

dfnbx[,j-22] <- factor(dfnbx[,j-22],
                          levels = 1:6,
                          labels = c(
                              "Ignore",
                              "VReact",
                              "PReact",
                              "DWeapon",
                              "UWeapon",
                              "Decline"
                              ),ordered=TRUE)
    counts[[j-22]] <- table(dfnbx[,j-22])
#    counts.h[[j-22]]<- table(dfnbx[[j-22]][df$location=="Harlem"])
#    counts.e[[j-22]]<- table(dfnbx[[j-22]][df$location=="EastNY"])
}
