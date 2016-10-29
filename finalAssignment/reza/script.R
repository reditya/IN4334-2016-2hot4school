# Load all csv data
time_positivity <- read.csv("timePositivity.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
organization <- read.csv("new_organization.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
reviewer_activity <- read.csv("reviewerActivity.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
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
filter_alldata = filter_alldata[filter_alldata$MINUTES < quantile(filter_alldata$MINUTES, 0.95),]

# reviewer lookup
least_reviewer <- read.csv("totalReviewerActivity.csv", header = TRUE, sep='|')
least_reviewer = list_reviewer[list_reviewer$MAX_R_ACTIVITY < quantile(list_reviewer$MAX_R_ACTIVITY,0.05),]
filter_alldata = filter_alldata[!(filter_alldata$R1_EMAIL %in% least_reviewer$r_email | filter_alldata$R2_EMAIL %in% least_reviewer$r_email),]

# filter review that's too fast, because it was reviewed by themself
filter_alldata = filter_alldata[filter_alldata$submitter_email != filter_alldata$R1_EMAIL,]

# filter on createdon = 1 Jan 2015, 00:00:00 until 30 June 2016, 23:59:59
# on unix timestamp : 1420070400 < createdon < 1467331199
filter_alldata = filter_alldata[filter_alldata$CREATEDON > 1420070400 & filter_alldata$CREATEDON < 1467331199, ]

###########
# CONFLICT
###########

conflict <- read.csv("conflict.csv", header=TRUE, sep="|",stringsAsFactors=FALSE)
cfilter <- merge(x = conflict, y = filter_alldata, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
cfilter = cfilter[!is.na(cfilter$POSITIVITY),]

cfilter = cfilter[,c("ID","BRANCH","PROJECT","CONFLICT","POSITIVITY","MINUTES")]
cfilter$POS <- ifelse(cfilter$POSITIVITY == 1,"r+","r-")
cfilter$POS <- as.factor(cfilter$POS)

# conflict yes, review positive
cfilter_cy_rp <- cfilter[ which(grepl("Y",cfilter$CONFLICT) & cfilter$POS == "r+"),]
# conflict yes, review negative
cfilter_cy_rn <- cfilter[ which(grepl("Y",cfilter$CONFLICT) & cfilter$POS == "r-"),]
# conflict no, review positive
cfilter_cn_rp <- cfilter[ which(!grepl("Y",cfilter$CONFLICT) & cfilter$POS == "r+"),]
# conflict no, review negative
cfilter_cn_rn <- cfilter[ which(!grepl("Y",cfilter$CONFLICT) & cfilter$POS == "r-"),]

# boxplot
boxplot(cfilter_cy_rp$MINUTES, cfilter_cy_rn, cfilter_cn_rp, cfilter_cn_rn, 
          col=c("red","green","blue","yellow"), 
          names=c("c+|r+", "c+|r-","c-|r+","c-|r-"))



###########################################
# Effect of Component factor on Time
###########################################

# create component vs time data frame
ct <- filter_alldata[,c("PROJECT","MINUTES","POSITIVITY"),]
ct$POS <- ifelse(ct$POSITIVITY == 1,"r+","r-")
ct$MINUTES <- as.factor(ct$MINUTES)
ct$PROJECT <- as.factor(ct$PROJECT)

#KW
kruskal.test(MINUTES~PROJECT, data=ct)

#MWW
pairwise.wilcox.test(as.numeric(ct$MINUTES), ct$PROJECT, p.adj="bonferroni", exact=F)

#convert minutes to factor again
ct$MINUTES <- as.numeric(ct$MINUTES)
ct$POS <- as.factor(ct$POS)

# cases : <0.05, neutron-cinder, nova-cinder, keystone-glance, neutron-glance, nova-glance, 
#               nova-keystone, nova-neutron, swift-nova
# we see here that nova occurs in all of the significant cases
# what does it mean?

summary(ct[which(ct$PROJECT == "openstack/swift"),])
summary(ct[which(ct$PROJECT == "openstack/neutron"),])

# component effect on positivity 
# subsets of r- and r+
ct_rp <- ct[ which(ct$POS=='r+'), ]
ct_rm <- ct[ which(ct$POS=='r-'), ]

#KW for r+
kruskal.test(MINUTES~PROJECT, data=ct_rm)

#KW for r-
kruskal.test(MINUTES~PROJECT, data=ct_rp)

#stat sig in both cases for r+ and r-
#mww for r+
pairwise.wilcox.test(as.numeric(ct_rp$MINUTES), ct_rp$PROJECT, p.adj="bonferroni", exact=F)
#stat difference in keystone-glance, neutron-cinder,neutron-glance, nova-cinder, nova-keystone, nova-neutron, swift-nova
summary(ct_rp[which(ct_rp$PROJECT == "openstack/glance"),])
summary(ct_rp[which(ct_rp$PROJECT == "openstack/neutron"),])

#mww for r-
pairwise.wilcox.test(as.numeric(ct_rm$MINUTES), ct_rm$PROJECT, p.adj="bonferroni", exact=F)
#stat difference in nova-cinder, nova-keyston, nova-neutron, swift-nova (all contains nova)

###########################################
# Effect of Organization factor on Time
###########################################

