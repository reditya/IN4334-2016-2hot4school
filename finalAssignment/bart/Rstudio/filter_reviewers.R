filter_alldata[,"queue_r2"] <- as.numeric(filter_alldata[, "queue_r2"])
is.na(filter_alldata[,"queue_r2"]) <- 0

filter_alldata[,"Hqueue"] = as.numeric(pmax(filter_alldata[,"queue_r1"], filter_alldata[,"queue_r2"], na.rm = TRUE))
filter_alldata$rqHgroup <- as.factor(cut(filter_alldata$Hqueue,quantile(filter_alldata$Hqueue,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

boxplot(MINUTES~rqHgroup, data = filter_alldata, outline=FALSE)
kruskal.test(MINUTES~rqHgroup, data=filter_alldata)
pairwise.wilcox.test(filter_alldata$MINUTES, filter_alldata$rqHgroup, p.adj="bonferroni", exact=F)

kruskal.test(POSITIVITY~rqHgroup, data=filter_alldata)
pairwise.wilcox.test(filter_alldata$POSITIVITY, filter_alldata$rqHgroup, p.adj="bonferroni", exact=F)

filter_alldata[,"Lqueue"] = as.numeric(pmin(filter_alldata[,"queue_r1"], filter_alldata[,"queue_r2"], na.rm = TRUE))
filter_alldata$rqLgroup <- as.factor(cut(filter_alldata$Lqueue,quantile(filter_alldata$Lqueue,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

boxplot(MINUTES~rqLgroup, data = filter_alldata, outline=FALSE)
kruskal.test(MINUTES~rqLgroup, data=filter_alldata)
pairwise.wilcox.test(filter_alldata$MINUTES, filter_alldata$rqLgroup, p.adj="bonferroni", exact=F)

kruskal.test(POSITIVITY~rqLgroup, data=filter_alldata)
pairwise.wilcox.test(filter_alldata$POSITIVITY, filter_alldata$rqLgroup, p.adj="bonferroni", exact=F)


filter_dataR1 = filter_alldata[,c("ID","BRANCH","PROJECT","CREATEDON","TIMEINSECOND","POSITIVITY",
                                  "queue_r1", "MINUTES")]
filter_dataR2 = filter_alldata[,c("ID","BRANCH","PROJECT","CREATEDON","TIMEINSECOND","POSITIVITY",
                                   "queue_r2", "MINUTES")]

names(filter_dataR1)[7] <- "queue"
names(filter_dataR2)[7] <- "queue"


filter_dataR = rbind(filter_dataR1,filter_dataR2)
filter_dataR$rqgroup <- as.factor(cut(filter_dataR$queue,quantile(filter_dataR$queue,(0:4)/4, na.rm=TRUE), include.lowest=TRUE, labels=LETTERS[1:4]))

boxplot(MINUTES~rqgroup, data = filter_dataR, outline=FALSE)
kruskal.test(MINUTES~rqgroup, data=filter_dataR)
pairwise.wilcox.test(filter_dataR$MINUTES, filter_dataR$rqgroup, p.adj="bonferroni", exact=F)

kruskal.test(POSITIVITY~rqgroup, data=filter_dataR)
pairwise.wilcox.test(filter_dataR$POSITIVITY, filter_dataR$rqgroup, p.adj="bonferroni", exact=F)