setwd("/Users/Bart/Documents/TU/Master/IN4334/results")

library(stringr)
library(stringi)
library(gtools)
library(plyr)

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
                            "R1_org","R2_org","submitter_email",
                            "submitter_org","R1_email", "R2_email","R1_EMAIL_A","R2_EMAIL_A","R1_ACTIVITY","R2_ACTIVITY",
                            "REVISIONS","SIZE",
                            "SUBMITTED_PATCHES","ACCEPTED_PATCHES",
                            "queue_r1", "queue_r2"),]

# filter on createdon = 1 Jan 2015, 00:00:00 until 30 June 2016, 23:59:59
# on unix timestamp : 1420070400 < createdon < 1467331199
filter_alldata = filter_alldata[filter_alldata$CREATEDON > 1420070400 & filter_alldata$CREATEDON < 1467331199, ]

# filter preprocessing
# filter positivity, take only +1 and -1
filter_alldata = filter_alldata[!is.na(filter_alldata$POSITIVITY),]

# filter openstack proposal bot, NA in the email, size > 0
filter_alldata = filter_alldata[filter_alldata$submitter_org != "N/A" & filter_alldata$TIMEINSECOND>= 0 & filter_alldata$SIZE> 0,]
filter_alldata = filter_alldata[!is.na(filter_alldata$submitter_org),]

# filter review that's too fast, because it was reviewed by themself
filter_alldata = filter_alldata[filter_alldata$submitter_email != filter_alldata$R1_email,]

# slowest patch, remove the slowest 5%
filter_alldata$MINUTES <- as.numeric(filter_alldata$TIMEINSECOND/60)
filter_alldata$DAYS <- as.numeric(filter_alldata$MINUTES/60/24)
filter_alldata_slowest = filter_alldata[filter_alldata$MINUTES < quantile(filter_alldata$MINUTES, 0.95),]

# reviewer lookup
#list_reviewer <- read.csv("totalReviewerActivity.csv", header = TRUE, sep='|')
#least_reviewer = list_reviewer[list_reviewer$MAX_R_ACTIVITY < quantile(list_reviewer$MAX_R_ACTIVITY,0.05),]
#filter_alldata = filter_alldata[!(filter_alldata$R1_EMAIL %in% least_reviewer$R_EMAIL | filter_alldata$R2_EMAIL %in% least_reviewer$R_EMAIL),]

# filter on createdon = 1 Jan 2015, 00:00:00 until 30 June 2016, 23:59:59
# on unix timestamp : 1420070400 < createdon < 1467331199
#filter_alldata = filter_alldata[filter_alldata$CREATEDON > 1420070400 & filter_alldata$CREATEDON < 1467331199, ]

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

list_reviewer <- list_reviewer[order(-list_reviewer$count),]


# calculate compound sum for each row
for (row in 1:nrow(list_reviewer)) { 
  if(row == 1)
  {
    list_reviewer$sums[row] = list_reviewer$count[row]
  }
  else
  {
    list_reviewer$sums[row] = list_reviewer$sum[row-1] + list_reviewer$count[row]
  }
}

threshold = 95/100*sum(list_reviewer$count)
least_reviewer <- list_reviewer[which(list_reviewer$sums>threshold),]

filter_alldata_reviewer = filter_alldata[!(filter_alldata$R1_email %in% least_reviewer$email | filter_alldata$R2_email %in% least_reviewer$email),]

# merge filter_alldata with filter_alldata_slowest and filter_alldata_reviewer
filter_alldata <- merge(x = filter_alldata, y = filter_alldata_slowest, by = c("ID", "BRANCH", "PROJECT"))
filter_alldata <- merge(x = filter_alldata, y = filter_alldata_reviewer, by = c("ID", "BRANCH", "PROJECT"))