library(stringr)
library(stringi)

# Load all csv data
time_positivity <- read.csv("timePositivity.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
organization <- read.csv("new_organization.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
reviewer_activity <- read.csv("newReviewerActivity.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
revisions_nums <- read.csv("revisions_nums.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
patchsize_result <- read.csv("patchsize_result.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
review_queue <- read.csv("newreview_queue.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
patchwriter_experience <- read.csv("patchwriter_experience.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)

# Join the CSV
alldata <- merge(x = time_positivity, y = organization, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
alldata <- merge(x = alldata, y = reviewer_activity, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
alldata <- merge(x = alldata, y = revisions_nums, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
alldata <- merge(x = alldata, y = patchsize_result, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
alldata <- merge(x = alldata, y = review_queue, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
alldata <- merge(x = alldata, y = patchwriter_experience, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)

# Choose subsets of columns
#filter_alldata = alldata[,c("ID","BRANCH","PROJECT","CREATEDON","TIMEINSECOND","POSITIVITY",
#                     "R1_org","R2_org","submitter_email",
#                     "submitter_org","R1_ACTIVITY","R1_EMAIL_ACTIVITY", "R2_ACTIVITY","R2_EMAIL_ACTIVITY","REVISIONS","SIZE",
#                     "SUBMITTED_PATCHES","ACCEPTED_PATCHES",
#                     "queue_r1", "queue_r2"),]

# Choose subsets of columns
filter_alldata = alldata[,c("ID","BRANCH","PROJECT","CREATEDON","TIMEINSECOND","POSITIVITY",
                     "R1_org","R2_org","submitter_email","R1_ACTIVITY","R2_ACTIVITY",
                     "submitter_org","R1_email", "R2_email","REVISIONS","SIZE",
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
filter_alldata$DAYS <- as.numeric(filter_alldata$MINUTES/60/24)
filter_alldata = filter_alldata[filter_alldata$MINUTES < quantile(filter_alldata$MINUTES, 0.95),]

# reviewer lookup
#list_reviewer <- read.csv("totalReviewerActivity.csv", header = TRUE, sep='|')
#least_reviewer = list_reviewer[list_reviewer$MAX_R_ACTIVITY < quantile(list_reviewer$MAX_R_ACTIVITY,0.05),]
#filter_alldata = filter_alldata[!(filter_alldata$R1_EMAIL_ACTIVITY %in% least_reviewer$R_EMAIL | filter_alldata$R2_EMAIL_ACTIVITY %in% least_reviewer$R_EMAIL),]

# filter review that's too fast, because it was reviewed by themself
filter_alldata = filter_alldata[filter_alldata$submitter_email != filter_alldata$R1_email,]

# filter on createdon = 1 Jan 2015, 00:00:00 until 30 June 2016, 23:59:59
# on unix timestamp : 1420070400 < createdon < 1467331199
filter_alldata = filter_alldata[filter_alldata$CREATEDON > 1420070400 & filter_alldata$CREATEDON < 1467331199, ]

# filter on least active reviewer
filter_dataR1 = filter_alldata[,c("ID","BRANCH","PROJECT","CREATEDON","TIMEINSECOND","POSITIVITY",
                                  "R1_org", "R1_email")]
filter_dataR2 = filter_alldata[,c("ID","BRANCH","PROJECT","CREATEDON","TIMEINSECOND","POSITIVITY",
                                  "R2_org", "R2_email")]

names(filter_dataR1)[7] <- "org"
names(filter_dataR2)[7] <- "org"
names(filter_dataR1)[8] <- "email"
names(filter_dataR2)[8] <- "email"

filter_dataR = rbind(filter_dataR1,filter_dataR2)
total <- aggregate(cbind(count = email) ~ email,
                   data=filter_dataR,
                   FUN = function(x){NROW(x)})
list_reviewer <- total[which(total$email!="N/A"),]
least_reviewer = list_reviewer[list_reviewer$count < quantile(list_reviewer$count,0.05),]

filter_alldata = filter_alldata[!(filter_alldata$R1_email %in% least_reviewer$email | filter_alldata$R2_email %in% least_reviewer$email),]






#############################
# Reviewer Activity on Time
#############################


# Retrieve required columns for intial data
trv <- filter_alldata[,c("R1_ACTIVITY", "R2_ACTIVITY", "MINUTES", "POSITIVITY")]
trv$POS <- ifelse(trv$POSITIVITY == 1,"r+","r-")

#############################################################
# pick one from these methods to measure reviewer activity
#############################################################

# 1st hypothesis: R2 drag the time, then use the lower reviewer activity #
# create new column for the lowest value of reviewer activity, if reviewer activity of the second reviewer is null then pick reviewer activity of the first reviewer
trv$R_ACTIVITY <- pmin(trv$R1_ACTIVITY, trv$R2_ACTIVITY)
trv$R_ACTIVITY[is.na(trv$R_ACTIVITY)] <- trv$R1_ACTIVITY[is.na(trv$R_ACTIVITY)]

# 2nd hypothesis: last reviewer determine the time, then use the reviewer activity of second reviewer (or the first one if the second is missing) #
# create new column for the reviewer activity of second reviewer, if reviewer activity of the second reviewer is null then pick reviewer activity of the first reviewer
trv$R_ACTIVITY <- trv$R2_ACTIVITY
trv$R_ACTIVITY[is.na(trv$R_ACTIVITY)] <- trv$R1_ACTIVITY[is.na(trv$R_ACTIVITY)]

# 3rd hypothesis: each reviewer do the review in parallel, then use the average number of reviewer activity #
# create new column for the reviewer activity of second reviewer, if reviewer activity of the second reviewer is null then pick reviewer activity of the first reviewer
trv$R_ACTIVITY <- rowMeans(trv[, 1:2], na.rm = TRUE)

# split the data, between having 2 reviewers and not
trv_1 <- trv[is.na(trv$R2_ACTIVITY),]
trv_2 <- trv[!is.na(trv$R2_ACTIVITY),]
#trv <- trv_1 
#or
#trv <- trv_2

#############################################################
#############################################################

# Retrieve required columns for intial data
trv <- trv[,c("R_ACTIVITY", "MINUTES", "POS")]

#splitting submitters into 4 groups
trv$GROUP <- as.factor(cut(trv$R_ACTIVITY, quantile(trv$R_ACTIVITY,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

# KW
kruskal.test(MINUTES~GROUP, data=trv)
# result: statistically significant

# MWW
pairwise.wilcox.test(trv$MINUTES, trv$GROUP, p.adj="bonferroni", exact=F)

# Mean and Medians for response time for groups
summary(trv[which(trv$GROUP=='A'),])
summary(trv[which(trv$GROUP=='B'),])
summary(trv[which(trv$GROUP=='C'),])
summary(trv[which(trv$GROUP=='D'),])

