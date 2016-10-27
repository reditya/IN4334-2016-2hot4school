time_positivity <- read.csv("timePositivity.csv", header = TRUE, sep='|')
organization <- read.csv("organization_result.csv", header = TRUE, sep='|')
reviewer_activity <- read.csv("reviewerActivity.csv", header = TRUE, sep='|')
revisions_nums <- read.csv("revisions_nums.csv", header = TRUE, sep='|')
patchsize_result <- read.csv("patchsize_result.csv", header = TRUE, sep='|')
#review_queue <- read.csv("review_queue.csv", header = TRUE, sep='|')
patchwriter_experience <- read.csv("patchwriter_experience.csv", header = TRUE, sep='|')

alldata <- merge(x = time_positivity, y = organization, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
alldata <- merge(x = alldata, y = reviewer_activity, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
alldata <- merge(x = alldata, y = revisions_nums, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
alldata <- merge(x = alldata, y = patchsize_result, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
#alldata <- merge(x = alldata, y = review_queue, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
alldata <- merge(x = alldata, y = patchwriter_experience, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)

filter_alldata = alldata[,c("ID","BRANCH","PROJECT","CREATEDON","TIMEINSECOND","POSITIVITY",
                     "R1_org","R2_org","R3_org","R4_org","R5_org","R6_org","R7_org","R8_org",
                     "submitter_org","R1_ACTIVITY","R2_ACTIVITY","REVISIONS","SIZE",
                     "num","accepted")]

# filter preprocessing
# filter positivity, take only +1 and -1
filter_alldata = filter_alldata[!is.na(filter_alldata$POSITIVITY),]

# filter openstack proposal bot, NA in the email
filter_alldata = filter_alldata[filter_alldata$submitter_org != "N/A",]
filter_alldata = filter_alldata[!is.na(filter_alldata$submitter_org),]

# slowest patch



summary(filter_alldata)
total3 = total2[,c("ID","BRANCH","PROJECT","TIMEINSECOND","POSITIVITY","SIZE","REVISIONS")]
total4 = na.omit(total3[total3$POSITIVITY > -2 & total3$SIZE > 0 & total3$TIMEINSECOND >= 0,])

total4$POS <- ifelse(total4$POSITIVITY==1, "r+", "r-")
total4$POS <- as.factor(total4$POS)

total4$group <- as.factor(cut(total4$SIZE,quantile(total4$SIZE,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

cor.test(total4$SIZE, total4$TIMEINSECOND, method="spearm", alternative="greater")

kruskal.test(TIMEINSECOND~group, data=total4)

pairwise.wilcox.test(total4$TIMEINSECOND, total4$group, p.adj="bonferroni", exact=F)

summary(total4[which(total4$group=='A'),])
posA = 4214/4464

summary(total4[which(total4$group=='B'),])
posA = 4117/4382

summary(total4[which(total4$group=='C'),])
posA = 4117/4382

