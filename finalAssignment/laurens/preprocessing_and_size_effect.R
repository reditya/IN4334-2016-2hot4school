time_positivity <- read.csv("time_positivity.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
organization <- read.csv("organization.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
reviewer_activity <- read.csv("reviewer_activity.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
revisions_nums <- read.csv("revisions_nums.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
patchsize_result <- read.csv("patchsize.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
review_queue <- read.csv("review_queue.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
patchwriter_experience <- read.csv("patchwriter_experience.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)

alldata <- merge(x = time_positivity, y = organization, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
alldata <- merge(x = alldata, y = reviewer_activity, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
alldata <- merge(x = alldata, y = revisions_nums, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
alldata <- merge(x = alldata, y = patchsize_result, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
alldata <- merge(x = alldata, y = review_queue, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
alldata <- merge(x = alldata, y = patchwriter_experience, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)

filter_alldata = alldata[,c("ID","BRANCH","PROJECT","CREATEDON","TIMEINSECOND","POSITIVITY",
                     "R1_org","R2_org","submitter_email",
                     "submitter_org","R1_ACTIVITY","R1_EMAIL", "R2_ACTIVITY","R2_EMAIL","REVISIONS","SIZE",
                     "SUBMITTED_PATCHES","ACCEPTED_PATCHES",
                     "queue_r1", "queue_r2"),]

# filter preprocessing
# filter positivity, take only +1 and -1
filter_alldata = filter_alldata[!is.na(filter_alldata$POSITIVITY),]

# filter openstack proposal bot, NA in the email, size > 0
filter_alldata = filter_alldata[filter_alldata$submitter_org != "N/A" & filter_alldata$TIMEINSECOND>= 0 & filter_alldata$SIZE> 0,]
filter_alldata = filter_alldata[!is.na(filter_alldata$submitter_org),]

# slowest patch
filter_alldata$MINUTES <- as.numeric(filter_alldata$TIMEINSECOND/60)
filter_alldata$DAYS <- as.numeric(filter_alldata$TIMEINSECOND/60/60/24)
filter_alldata = filter_alldata[filter_alldata$MINUTES < quantile(filter_alldata$MINUTES, 0.95),]

# reviewer lookup
list_reviewer <- read.csv("total_reviewer_activity.csv", header = TRUE, sep='|')
least_reviewer = list_reviewer[list_reviewer$MAX_R_ACTIVITY < quantile(list_reviewer$MAX_R_ACTIVITY,0.05),]
filter_alldata = filter_alldata[!(filter_alldata$R1_EMAIL %in% least_reviewer$R_EMAIL | filter_alldata$R2_EMAIL %in% least_reviewer$R_EMAIL),]

# filter review that's too fast, because it was reviewed by themself
filter_alldata = filter_alldata[filter_alldata$submitter_email != filter_alldata$R1_EMAIL,]

# filter on createdon = 1 Jan 2015, 00:00:00 until 30 June 2016, 23:59:59
# on unix timestamp : 1420070400 < createdon < 1467331199
filter_alldata = filter_alldata[filter_alldata$CREATEDON > 1420070400 & filter_alldata$CREATEDON < 1467331199, ]

#####################################
# Effect of Patch Size on Time
#####################################

# nearly equal number of observations in each group using quantile function

filter_alldata$group <- as.factor(cut(filter_alldata$SIZE,quantile(filter_alldata$SIZE,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

#Kruskal-Wallis test
kruskal.test(TIMEINSECOND~group, data=filter_alldata)

#Man-Whitney with Bonferroni correction
pairwise.wilcox.test(filter_alldata$TIMEINSECOND, filter_alldata$group, p.adj="bonferroni", exact=F)

patchA <- filter_alldata[which(filter_alldata$group=='A'),]
patchB <- filter_alldata[which(filter_alldata$group=='B'),]
patchC <- filter_alldata[which(filter_alldata$group=='C'),]
patchD <- filter_alldata[which(filter_alldata$group=='D'),]
boxplot(patchA$MINUTES, patchB$MINUTES, patchC$MINUTES, patchD$MINUTES, col=c("red","green", "blue"), names=c("A", "B", "C", "D"))

# Mean and Median values for each group
summary(patchA)
summary(patchB)
summary(patchC)
summary(patchD)

# Positivity for each group:
pos_A = nrow(patchA[which(patchA$POSITIVITY==1),])/nrow(patchA)
pos_B = nrow(patchB[which(patchB$POSITIVITY==1),])/nrow(patchB)
pos_C = nrow(patchC[which(patchC$POSITIVITY==1),])/nrow(patchC)
pos_D = nrow(patchD[which(patchD$POSITIVITY==1),])/nrow(patchD)

# Split data into 4 bins for two groups: POSTIVITY=1 and POSTIVITY=-1
accepted <- filter_alldata[ which(filter_alldata$POSITIVITY==1), ]
rejected <- filter_alldata[ which(filter_alldata$POSITIVITY==-1), ]

accepted$group <- as.factor(cut(accepted$SIZE,quantile(accepted$SIZE,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))
rejected$group <- as.factor(cut(rejected$SIZE,quantile(rejected$SIZE,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

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
kruskal.test(REVISIONS~group, data=filter_alldata)

#Result: there is stat sign difference somewhere

#MWW
pairwise.wilcox.test(filter_alldata$REVISIONS, filter_alldata$group, p.adj="bonferroni", exact=F)

#Result: effect of pach size on the rounds of revisions are stat different for all groups.

#correlation : size vs time
#spearman
cor.s = cor.test(filter_alldata$SIZE, filter_alldata$TIMEINSECOND, method="spearman")
cor.s

# Correlation of accepted/rejected patches (size with time) 

# Accepted patches
cor.a = cor.test(accepted$SIZE, accepted$TIMEINSECOND, method="spearman")

# Rejected patches
cor.r = cor.test(rejected$SIZE, rejected$TIMEINSECOND, method="spearman")

###########################################
# Effect of Organization factor on Time
###########################################

get_top_perc_org <- function(df, perc) {
  df$perc <- df$freq / sum(df$freq) * 100
  df <- df[order(-df$perc),]
  for (i in 1:nrow(df)) {
    if(sum(df[1:i,]$perc) >= perc) {
      return(df[1:i,])
    }
  }
}

# Select top 80% submitter organizations
s_org <- data.frame(table(filter_alldata$submitter_org))
colnames(s_org) <- c('org','freq')
# s_org <- get_top_perc_org(s_org, 80)
s_org <- s_org[order(-s_org$freq),]
s_org <- s_org[1:2,]
filter_alldata$submitter_org[!filter_alldata$submitter_org %in% s_org$org] <- "Other"

# Select top 80% reviewer 1 organizations
R1_org <- data.frame(table(filter_alldata$R1_org))
colnames(R1_org) <- c('org','freq')
# R1_org <- get_top_perc_org(R1_org, 80)
R1_org <- R1_org[order(-R1_org$freq),]
R1_org <- R1_org[1:2,]
filter_alldata$R1_org[!filter_alldata$R1_org %in% R1_org$org] <- "Other"

# Select top 80% reviewer 2 organizations
R2_org <- data.frame(table(filter_alldata$R2_org))
colnames(R2_org) <- c('org','freq')
# R2_org <- get_top_perc_org(R2_org, 80)
R2_org <- R2_org[order(-R2_org$freq),]
R2_org <- R2_org[1:2,]
filter_alldata$R2_org[!filter_alldata$R2_org %in% R2_org$org] <- "Other"

# Select top 80% of reviewer 1 and 2 organizations
R12_org <- data.frame(table(filter_alldata$R1_org, filter_alldata$R2_org))
colnames(R12_org) <- c('R1_org', 'R2_org','freq')
# R12_org <- get_top_perc_org(R12_org, 80)

#KW for the whole sample
kruskal.test(TIMEINSECOND~as.factor(submitter_org), data=filter_alldata)
kruskal.test(TIMEINSECOND~as.factor(R1_org), data=filter_alldata)
kruskal.test(TIMEINSECOND~as.factor(R2_org), data=filter_alldata)
#Result: all stat sig

#MWW for the whole dataset

#submitters
pairwise.wilcox.test(filter_alldata$TIMEINSECOND, filter_alldata$submitter_org, p.adj="bonferroni", exact=F)

#reviewer 1s
pairwise.wilcox.test(filter_alldata$TIMEINSECOND, filter_alldata$R1_org, p.adj="bonferroni", exact=F)

#reviewer 2s
pairwise.wilcox.test(filter_alldata$TIMEINSECOND, filter_alldata$R2_org, p.adj="bonferroni", exact=F)

#KW for accepted
kruskal.test(TIMEINSECOND~submitter_org, data=accepted)
kruskal.test(TIMEINSECOND~R1_org, data=accepted)
kruskal.test(TIMEINSECOND~R2_org, data=accepted)

#KW for rejected
kruskal.test(TIMEINSECOND~submitter_org, data=rejected)
kruskal.test(TIMEINSECOND~R1_org, data=rejected)
kruskal.test(TIMEINSECOND~R2_org, data=rejected) # Not stat sig

# To find out where the difference is do pariwise wilcox

# Select only IBM and Red Hat interactions (R1_org and s_org)
IBMRH <-filter_alldata[ which(filter_alldata$R1_org %in% c('IBM','Red Hat') & filter_alldata$submitter_org %in% c('IBM','Red Hat')), ]

#KW for IBM-Red Hat data only
kruskal.test(TIMEINSECOND~as.factor(submitter_org), data=IBMRH)
kruskal.test(TIMEINSECOND~as.factor(R1_org), data=IBMRH)
# Stat sig
summary(IBMRH[which(as.character(IBMRH$submitter_org)=='IBM'), c('DAYS')])
summary(IBMRH[which(as.character(IBMRH$submitter_org)=='Red Hat'), c('DAYS')])

summary(filter_alldata[which(as.character(filter_alldata$R1_org)==as.character(filter_alldata$R2_org)), c('DAYS')])
summary(filter_alldata[which(as.character(filter_alldata$R1_org)!=as.character(filter_alldata$R2_org)), c('DAYS')])

#####################################
# Effect of Organization on Positivity
#####################################
pos <- data.frame(table(filter_alldata$R1_EMAIL, filter_alldata$R1_org, filter_alldata$R2_EMAIL, filter_alldata$R2_org, filter_alldata$submitter_org))
colnames(pos) <- c('R1_email', 'R1_org', 'R2_email', 'R2_org', 'submitter_org', 'freq')
pos <- pos[pos$freq!=0,]
pos <- pos[pos$R2_email!='',]
pos <- pos[order(-pos$freq),]
pos$pos <- 0
for (i in 1:nrow(pos)) {
  pos[i,]$pos <- nrow(filter_alldata[which(filter_alldata$POSITIVITY == 1 & filter_alldata$R1_EMAIL==pos[i,]$R1_email & filter_alldata$R2_EMAIL==pos[i,]$R2_email & filter_alldata$submitter_org==pos[i,]$submitter_org),]) / nrow(filter_alldata[which(filter_alldata$R1_EMAIL==pos[i,]$R1_email & filter_alldata$R2_EMAIL==pos[i,]$R2_email & filter_alldata$submitter_org==pos[i,]$submitter_org),])
}
boxplot(pos~R1_org*R2_org*submitter_org, data=pos, xlab="Organization", ylab="Positivity", ylim=c(0.9,1))