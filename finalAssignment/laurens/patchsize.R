#####################################
# Effect of Patch Size on Time
#####################################

# nearly equal number of observations in each group using quantile function

filter_alldata$group <- as.factor(cut(filter_alldata$SIZE,quantile(filter_alldata$SIZE,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

#Kruskal-Wallis tests
kruskal.test(TIMEINSECOND~group, data=filter_alldata)
kruskal.test(POSITIVITY~group, data=filter_alldata)

#Man-Whitney with Bonferroni correction
pairwise.wilcox.test(filter_alldata$TIMEINSECOND, filter_alldata$group, p.adj="bonferroni", exact=F)
pairwise.wilcox.test(filter_alldata$POSITIVITY, filter_alldata$group, p.adj="bonferroni", exact=F)

patchA <- filter_alldata[which(filter_alldata$group=='A'),]
patchB <- filter_alldata[which(filter_alldata$group=='B'),]
patchC <- filter_alldata[which(filter_alldata$group=='C'),]
patchD <- filter_alldata[which(filter_alldata$group=='D'),]

boxplot(filter_alldata$DAYS ~ filter_alldata$group, ylab="Review time (days)", names=c("A", "B", "C", "D"), outline=FALSE)

# Mean and Median values for each group
summary(patchA$DAYS)
summary(patchB$DAYS)
summary(patchC$DAYS)
summary(patchD$DAYS)

# Positivity for each group:
pos_A = nrow(patchA[which(patchA$POSITIVITY==1),])/nrow(patchA)
pos_B = nrow(patchB[which(patchB$POSITIVITY==1),])/nrow(patchB)
pos_C = nrow(patchC[which(patchC$POSITIVITY==1),])/nrow(patchC)
pos_D = nrow(patchD[which(patchD$POSITIVITY==1),])/nrow(patchD)

# Split data into 4 bins for two groups: POSTIVITY=1 and POSTIVITY=-1
accepted <- filter_alldata[ which(filter_alldata$POSITIVITY==1), ]
rejected <- filter_alldata[ which(filter_alldata$POSITIVITY==0), ]

accepted$group <- as.factor(cut(accepted$SIZE,quantile(accepted$SIZE,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))
rejected$group <- as.factor(cut(rejected$SIZE,quantile(rejected$SIZE,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

boxplot(accepted$DAYS ~ accepted$group, ylab="Acceptance time (days)", col=c("red","green", "blue", "yellow"), names=c("A", "B", "C", "D"), outline=FALSE)
boxplot(rejected$DAYS ~ rejected$group, ylab="Reject time (days)", col=c("red","green", "blue", "yellow"), names=c("A", "B", "C", "D"), outline=FALSE)

# KW for accepted
kruskal.test(TIMEINSECOND~group, data=accepted)

#post-hoc MWW test for accepted
pairwise.wilcox.test(accepted$TIMEINSECOND, accepted$group, p.adj="bonferroni", exact=F)

# KW for rejected
kruskal.test(TIMEINSECOND~group, data=rejected)

#post-hoc MWW test for rejected
pairwise.wilcox.test(rejected$TIMEINSECOND, rejected$group, p.adj="bonferroni", exact=F)

#####################################
# Patch Size on Number of Revisions
#####################################

#KW test for # of revisions
kruskal.test(REVISIONS~group, data=accepted)

#Result: there is stat sign difference somewhere

#MWW
pairwise.wilcox.test(accepted$REVISIONS, accepted$group, p.adj="bonferroni", exact=F)

boxplot(accepted$REVISIONS ~ accepted$group, ylab="Number of revisions", col=c("red","green", "blue", "yellow"), names=c("A", "B", "C", "D"), outline=FALSE)