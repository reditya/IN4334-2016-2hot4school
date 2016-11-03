
filter_alldata$pwegroup <- as.factor(cut(filter_alldata$SUBMITTED_PATCHES,quantile(filter_alldata$SUBMITTED_PATCHES,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

boxplot(MINUTES~pwegroup, data = filter_alldata, outline=FALSE)
kruskal.test(MINUTES~pwegroup, data=filter_alldata)
pairwise.wilcox.test(filter_alldata$MINUTES, filter_alldata$pwegroup, p.adj="bonferroni", exact=F)

kruskal.test(POSITIVITY~pwegroup, data=filter_alldata)
pairwise.wilcox.test(filter_alldata$POSITIVITY, filter_alldata$pwegroup, p.adj="bonferroni", exact=F)


filter_alldata$acceptPer <-  filter_alldata$ACCEPTED_PATCHES / filter_alldata$SUBMITTED_PATCHES


dataA <- filter_alldata[which(filter_alldata$pwegroup=='A'),]
dataA = subset(dataA, SUBMITTED_PATCHES > 0)
dataA$argroup <- as.factor(cut(dataA$acceptPer,quantile(dataA$acceptPer,c((0:3)*0.05,1)), include.lowest=TRUE, labels=LETTERS[1:4]))
boxplot(MINUTES~argroup, data = dataA, outline=FALSE)
kruskal.test(MINUTES~argroup, data=dataA)
pairwise.wilcox.test(dataA$MINUTES, dataA$argroup, p.adj="bonferroni", exact=F)
kruskal.test(POSITIVITY~argroup, data=dataA)
pairwise.wilcox.test(dataA$POSITIVITY, dataA$argroup, p.adj="bonferroni", exact=F)


dataB <- filter_alldata[which(filter_alldata$pwegroup=='B'),]
dataB$argroup <- as.factor(cut(dataB$acceptPer,quantile(dataB$acceptPer,c((0:3)*0.05,1)), include.lowest=TRUE, labels=LETTERS[1:4]))
boxplot(MINUTES~argroup, data = dataB, outline=FALSE)
kruskal.test(MINUTES~argroup, data=dataB)
pairwise.wilcox.test(dataB$MINUTES, dataB$argroup, p.adj="bonferroni", exact=F)
kruskal.test(POSITIVITY~argroup, data=dataB)
pairwise.wilcox.test(dataB$POSITIVITY, dataB$argroup, p.adj="bonferroni", exact=F)


dataC <- filter_alldata[which(filter_alldata$pwegroup=='C'),]
dataC$argroup <- as.factor(cut(dataC$acceptPer,quantile(dataC$acceptPer,c((0:3)*0.05,1)), include.lowest=TRUE, labels=LETTERS[1:4]))
boxplot(MINUTES~argroup, data = dataC, outline=FALSE)
kruskal.test(MINUTES~argroup, data=dataC)
pairwise.wilcox.test(dataC$MINUTES, dataC$argroup, p.adj="bonferroni", exact=F)
kruskal.test(POSITIVITY~argroup, data=dataC)
pairwise.wilcox.test(dataC$POSITIVITY, dataC$argroup, p.adj="bonferroni", exact=F)

dataD <- filter_alldata[which(filter_alldata$pwegroup=='D'),]
dataD$argroup <- as.factor(cut(dataD$acceptPer,quantile(dataD$acceptPer,c((0:3)*0.05,1)), include.lowest=TRUE, labels=LETTERS[1:4]))
boxplot(MINUTES~argroup, data = dataD, outline=FALSE)
kruskal.test(MINUTES~argroup, data=dataD)
pairwise.wilcox.test(dataD$MINUTES, dataD$argroup, p.adj="bonferroni", exact=F)
kruskal.test(POSITIVITY~argroup, data=dataD)
pairwise.wilcox.test(dataD$POSITIVITY, dataD$argroup, p.adj="bonferroni", exact=F)


data0 = subset(filter_alldata, SUBMITTED_PATCHES > 0)
data0$argroup <- as.factor(cut(data0$acceptPer,quantile(data0$acceptPer,c((0:3)*0.05,1)), include.lowest=TRUE, labels=LETTERS[1:4]))
kruskal.test(MINUTES~argroup, data=data0)
pairwise.wilcox.test(data0$MINUTES, data0$argroup, p.adj="bonferroni", exact=F)
kruskal.test(POSITIVITY~argroup, data=data0)
pairwise.wilcox.test(data0$POSITIVITY, data0$argroup, p.adj="bonferroni", exact=F)