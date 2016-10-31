library(stringr)
library(stringi)

# Load all csv data
time_positivity <- read.csv("timePositivity.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
organization <- read.csv("new_organization.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
#reviewer_activity <- read.csv("newReviewerActivity.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
revisions_nums <- read.csv("revisions_nums.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
patchsize_result <- read.csv("patchsize_result.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
review_queue <- read.csv("newreview_queue.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)
patchwriter_experience <- read.csv("patchwriter_experience.csv", header = TRUE, sep='|', stringsAsFactors=FALSE)

# Join the CSV
alldata <- merge(x = time_positivity, y = organization, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
#alldata <- merge(x = alldata, y = reviewer_activity, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
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

###########
# CONFLICT
###########

conflict <- read.csv("conflict.csv", header=TRUE, sep="|",stringsAsFactors=FALSE)
cfilter <- merge(x = conflict, y = filter_alldata, by = c("ID", "BRANCH", "PROJECT"), all.x = TRUE)
cfilter = cfilter[!is.na(cfilter$POSITIVITY),]

cfilter = cfilter[,c("ID","BRANCH","PROJECT","CONFLICT","POSITIVITY","DAYS","R1_org","R2_org","submitter_org")]
# create a flag for positivity
cfilter$POS <- ifelse(cfilter$POSITIVITY == 1,"r+","r-")
cfilter$POS <- as.factor(cfilter$POS)
# create a flag for conflict
cfilter$CONFLICT_FLAG <- ifelse(grepl("Y",cfilter$CONFLICT),"Y","N")
# create a number of patchsets with conflict column
cfilter$CY_COUNT <- str_count(cfilter$CONFLICT,"Y")
# create a number of patchsets without conflict column
cfilter$CN_COUNT <- str_count(cfilter$CONFLICT,"N")
# create a percentage of patchsets with conflict
cfilter$CP <- str_count(cfilter$CONFLICT,"Y")/stri_length(cfilter$CONFLICT)

# Summary
cfilter$DAYS <- as.numeric(cfilter$DAYS)
cfilter_cy = cfilter[which(cfilter$CONFLICT_FLAG=="Y"),]
cfilter_cn = cfilter[which(cfilter$CONFLICT_FLAG=="N"),]

summary(cfilter_cy)
summary(cfilter_cn)

# Boxplot
boxplot(cfilter_cy$DAYS, cfilter_cn$DAYS,
        col=c("red","green"), 
        names=c("Yes", "No"), 
        xlab="Occurence of conflict",
        ylab="Days",
        outline=FALSE)

# Findings
# Patches with conflict have longer time to review compared to patches without conflict

# Check on positivity
# Patches with conflict
cy_accepted = 3698/3941
cy_rejected = 243/3941

# Patches without conflict
cn_accepted = 8901/9439
cn_rejected = 538/8901

# Findings : No significant effect on positivity but let's take a look in detail on patches with conflict
# We want to see whether the number of conflict will affect positivity
# divide by 4 group by conflict occurence
cfilter_cy$groupCY <- cut(cfilter_cy$CY_COUNT, 
                       breaks = c(-Inf, 1, 2, 3,4,Inf),
                       labels = c("0", "1", "2", "3", ">3"),
                       right = FALSE)
totalAB <- aggregate(cbind(count = groupCY) ~ groupCY,
                     data=cfilter_cy,
                     FUN = function(x){NROW(x)})

##
cfilter_cy_1 = cfilter_cy[which(cfilter_cy$group=="1"),]
cfilter_cy_2 = cfilter_cy[which(cfilter_cy$group=="2"),]
cfilter_cy_3 = cfilter_cy[which(cfilter_cy$group=="3"),]
cfilter_cy_gt3 = cfilter_cy[which(cfilter_cy$group==">3"),]

# The summary
summary(cfilter_cy_1)
summary(cfilter_cy_2)
summary(cfilter_cy_3)
summary(cfilter_cy_gt3)

# rejected patch
ratio_cy_1 = 188/2514
ratio_cy_2 = 39/845
ratio_cy_3 = 10/313
ratio_cy_gt3 = 6/269

## Findings
# There is a little difference in percentage of rejected patches in each subsets of group

####### 
# CONFLICT ORGANIZATION
#######

## is there any difference between patches with internal or external organization conflict
summary(cfilter_cy[which((cfilter_cy$submitter_org==cfilter_cy$R1_org & 
                           cfilter_cy$submitter_org ==cfilter_cy$R2_org) | (cfilter_cy$submitter_org==cfilter_cy$R1_org & 
                                                                              cfilter_cy$R2_org=="N/A")),])
## check per organization
## for big 4 : IBM, Red Hat, Mirantis, HPE
## We want to see when there is a conflict, how does conflict from different organization (submitter_org) will affect time
cfilter_cy$DAYS <- as.numeric(cfilter_cy$DAYS)
cfilter_cy$submitter_org <- as.factor(cfilter_cy$submitter_org)
kruskal.test(DAYS~submitter_org,data=cfilter_cy[which(cfilter_cy$submitter_org == "IBM" | 
                                                  cfilter_cy$submitter_org == "HPE" | 
                                                  cfilter_cy$submitter_org == "Red Hat" |
                                                  cfilter_cy$submitter_org == "Mirantis"),])

cfilter_cy_top = cfilter_cy[which(cfilter_cy$submitter_org == "IBM" | 
                          cfilter_cy$submitter_org == "HPE" | 
                          cfilter_cy$submitter_org == "Red Hat" |
                          cfilter_cy$submitter_org == "Mirantis"),]

cfilter_cy_top$DAYS <- as.numeric(cfilter_cy_top$DAYS)
summary(cfilter_cy_top[which(cfilter_cy_top$submitter_org == "IBM"),])
summary(cfilter_cy_top[which(cfilter_cy_top$submitter_org == "Red Hat"),])

cfilter_cn_top = cfilter_cn[which(cfilter_cn$submitter_org == "IBM" | 
                                    cfilter_cn$submitter_org == "HPE" | 
                                    cfilter_cn$submitter_org == "Red Hat" |
                                    cfilter_cn$submitter_org == "Mirantis"),]

cfilter_cn_top$DAYS <- as.numeric(cfilter_cn_top$DAYS)
cfilter_cn_top$submitter_org <- as.factor(cfilter_cn_top$submitter_org)

# KW & MWW
kruskal.test(DAYS~submitter_org,data=cfilter_cy_top)
kruskal.test(DAYS~submitter_org,data=cfilter_cn_top)
pairwise.wilcox.test(as.numeric(cfilter_cy_top$DAYS), cfilter_cy_top$submitter_org, p.adj="bonferroni", exact=F)
# when there is a conflict, only IBM-Red Hat and Mirantis-Red Hat has stat diff in time
pairwise.wilcox.test(as.numeric(cfilter_cn_top$DAYS), cfilter_cn_top$submitter_org, p.adj="bonferroni", exact=F)
# when there is no conflict, only Mirantis-Red Hat has stat diff in time

totalAB <- aggregate(cbind(count = submitter_org) ~ submitter_org,
                     data=cfilter_cn_top,
                     FUN = function(x){NROW(x)})

# boxplot for conflict = yes, per submitter organization
cy_i = cfilter_cy_top[which(cfilter_cy_top$submitter_org == "IBM"),]
cy_h = cfilter_cy_top[which(cfilter_cy_top$submitter_org == "HPE"),]
cy_r = cfilter_cy_top[which(cfilter_cy_top$submitter_org == "Red Hat"),]
cy_m = cfilter_cy_top[which(cfilter_cy_top$submitter_org == "Mirantis"),]

boxplot(cy_i$DAYS, cy_h$DAYS, cy_r$DAYS, cy_m$DAYS,
        col=c("red","green","blue","yellow"), 
        names=c("IBM", "HPE","Red Hat","Mirantis"), 
        xlab="Organization",
        ylab="Days",
        outline=FALSE)

# boxplot for conflict = no, per submitter organization
cn_i = cfilter_cn_top[which(cfilter_cn_top$submitter_org == "IBM"),]
cn_h = cfilter_cn_top[which(cfilter_cn_top$submitter_org == "HPE"),]
cn_r = cfilter_cn_top[which(cfilter_cn_top$submitter_org == "Red Hat"),]
cn_m = cfilter_cn_top[which(cfilter_cn_top$submitter_org == "Mirantis"),]

boxplot(cy_i$DAYS, cy_h$DAYS, cy_r$DAYS, cy_m$DAYS,
        col=c("red","green","blue","yellow"), 
        names=c("IBM", "HPE","Red Hat","Mirantis"), 
        xlab="Organization",
        ylab="Days",
        outline=FALSE)



# divide by 4 group by percentage of conflict
cfilter$group <- cut(cfilter$CP, 
                     breaks = c(-Inf, 0.25, 0.5, 0.75, Inf),
                     labels = c("<25%", "25-50%", "50-75%", "75-100%"),
                     right = FALSE)

# KW
kruskal.test(DAYS~group,data=cfilter)

# MWW
pairwise.wilcox.test(as.numeric(cfilter$DAYS), cfilter$group, p.adj="bonferroni", exact=F)

# summary of <25% vs 75-100%
cfilter$DAYS <- as.numeric(cfilter$DAYS)
summary(cfilter[which(cfilter$group=="<25%"),])
summary(cfilter[which(cfilter$group=="75-100%"),])

# divide by 4 group by conflict occurence
cfilter$groupCY <- cut(cfilter$CY_COUNT, 
                       breaks = c(-Inf, 1, 2, 3,4,Inf),
                       labels = c("0", "1", "2", "3", ">3"),
                       right = FALSE)

# conflict yes, review positive
cfilter_cy_rp <- cfilter[ which(grepl("Y",cfilter$CONFLICT) & cfilter$POS == "r+"),]
# conflict yes, review negative
cfilter_cy_rn <- cfilter[ which(grepl("Y",cfilter$CONFLICT) & cfilter$POS == "r-"),]
# conflict no, review positive
cfilter_cn_rp <- cfilter[ which(!grepl("Y",cfilter$CONFLICT) & cfilter$POS == "r+"),]
# conflict no, review negative
cfilter_cn_rn <- cfilter[ which(!grepl("Y",cfilter$CONFLICT) & cfilter$POS == "r-"),]

# boxplot
boxplot(cfilter_cy_rp$DAYS, cfilter_cn_rp$DAYS, cfilter_cy_rn$DAYS, cfilter_cn_rn$DAYS, 
          col=c("red","green","blue","yellow"), 
          names=c("c+|r+", "c-|r+","c+|r-","c-|r-"), outline=FALSE)

#KW
cfilter$DAYS <- as.factor(cfilter$DAYS)
cfilter$CONFLICT_FLAG <- as.factor(cfilter$CONFLICT_FLAG)
cfilter$groupCY <- as.factor(cfilter$groupCY)
kruskal.test(DAYS~groupCY,data=cfilter)


pairwise.wilcox.test(as.numeric(cfilter$MINUTES), cfilter$group, p.adj="bonferroni", exact=F)
pairwise.wilcox.test(as.numeric(cfilter$DAYS), cfilter$groupCY, p.adj="bonferroni", exact=F)


#summary
cfilter$MINUTES <- as.numeric(cfilter$MINUTES)
summary(cfilter[which(cfilter$group=="<25%"),])
summary(cfilter[which(cfilter$group=="25-50%"),])

g1 <- cfilter[cfilter$group == "<25%",] 
g2 <- cfilter[cfilter$group == "25-50%",] 
g3 <- cfilter[cfilter$group == "50-75%",] 
g4 <- cfilter[cfilter$group == "75-100%",] 

# boxplot
boxplot(g1$MINUTES, g2$MINUTES, g3$MINUTES, g4$MINUTES, 
        col=c("red","green","blue","yellow"), 
        names=c("<25%", "25-50%","50-75%","75-100%"))

boxplot(g1$CP, g2$CP, g3$CP, g4$CP, 
        col=c("red","green","blue","yellow"), 
        names=c("<25%", "25-50%","50-75%","75-100%"))

#
y1 <- cfilter[cfilter$groupCY == "0",] 
y2 <- cfilter[cfilter$groupCY == "1",] 
y3 <- cfilter[cfilter$groupCY == "2",] 
y4 <- cfilter[cfilter$groupCY == "3",]
y5 <- cfilter[cfilter$groupCY == "4",]
y6 <- cfilter[cfilter$groupCY == "5",] 
y7 <- cfilter[cfilter$groupCY == "6",] 
y8 <- cfilter[cfilter$groupCY == "7",] 
y9 <- cfilter[cfilter$groupCY == "8",]
y10 <- cfilter[cfilter$groupCY == "9",]
y11 <- cfilter[cfilter$groupCY == "10",]
y12 <- cfilter[cfilter$groupCY == ">10",]


boxplot(y1$DAYS, y2$DAYS, y3$DAYS, y4$DAYS, y5$DAYS ,
        y6$DAYS, y7$DAYS, y8$DAYS, y9$DAYS, y10$DAYS , y11$DAYS, y12$DAYS)

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
library(reshape)
library(reshape2)
mimi = pairwise.wilcox.test(as.numeric(ct$MINUTES), ct$PROJECT, p.adj="bonferroni", exact=F)
melt(mimi[[3]])
#convert minutes to factor again
ct$MINUTES <- as.numeric(ct$MINUTES)
ct$POS <- as.factor(ct$POS)

# cases : <0.05, neutron-cinder, nova-cinder, keystone-glance, neutron-glance, nova-glance, 
#               nova-keystone, nova-neutron, swift-nova
# we see here that nova occurs in all of the significant cases
# what does it mean?

summary(ct[which(ct$PROJECT == "openstack/swift"),])
summary(ct[which(ct$PROJECT == "openstack/glance"),])

# component effect on positivity 
# subsets of r- and r+
ct_rp <- ct[ which(ct$POS=='r+'), ]
ct_rm <- ct[ which(ct$POS=='r-'), ]

#KW for r+
kruskal.test(MINUTES~PROJECT, data=ct_rm)

#KW for r-
kruskal.test(MINUTES~PROJECT, data=ct_rp)

#stat sig in both cases for r+ and r- group
#mww for r+
pairwise.wilcox.test(as.numeric(ct_rp$MINUTES), ct_rp$PROJECT, p.adj="bonferroni", exact=F)
#stat difference in keystone-glance, neutron-cinder,neutron-glance, nova-cinder, nova-keystone, nova-neutron, swift-nova
summary(ct_rp[which(ct_rp$PROJECT == "openstack/glance"),])
summary(ct_rp[which(ct_rp$PROJECT == "openstack/neutron"),])

#mww for r-
pairwise.wilcox.test(as.numeric(ct_rm$MINUTES), ct_rm$PROJECT, p.adj="bonferroni", exact=F)
#stat difference in nova-cinder, nova-keyston, nova-neutron, swift-nova (all contains nova)

# component on positivity. let's count the acceptance ratio
summary(ct[which(ct$PROJECT == "openstack/cinder"),])
summary(ct[which(ct$PROJECT == "openstack/glance"),])
summary(ct[which(ct$PROJECT == "openstack/keystone"),])
summary(ct[which(ct$PROJECT == "openstack/neutron"),])
summary(ct[which(ct$PROJECT == "openstack/swift"),])
summary(ct[which(ct$PROJECT == "openstack/nova"),])

pos_cinder = 2448/2592
pos_glance = 463/501
pos_keystone = 1388/1438
pos_neutron = 3526/3678
pos_swift = 690/711
pos_nova = 4084/4460

# range : 0.92 - 0.97
# nova is more difficult than the others, particularly keystone and swift (as the bottom 2)

###########################################
# Effect of Organization factor on Time
###########################################

