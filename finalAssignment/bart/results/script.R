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
#REVIEWER ACTIVITY
###########

filter_alldata$ACTIVITY <- ifelse(!is.na(filter_alldata$R2_ACTIVITY),
                                  pmin(filter_alldata$R1_ACTIVITY,filter_alldata$R2_ACTIVITY),
                                  filter_alldata$R1_ACTIVITY)
filter_alldata$ACTIVITY <- as.numeric(filter_alldata$ACTIVITY)
filter_alldata$groupAct <- as.factor(cut(filter_alldata$ACTIVITY, quantile(filter_alldata$ACTIVITY,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

names(total)[1] <- "R1_email"
names(total)[2] <- "R1_act"
#total = total[which(total$R1_email!="N/A" & (total$R1_act>50 & total$R1_act<1000)),]
total2 = total
names(total2)[1] <- "R2_email"
names(total2)[2] <- "R2_act"

cc <- merge(x = filter_alldata, y = total, by = c("R1_email"), all.x = TRUE)
cc <- merge(x = cc, y = total2, by = c("R2_email"), all.x = TRUE)

cc$newAct <- ifelse(!is.na(cc$R2_act),
                        pmin(cc$R2_act,cc$R1_act),
                        cc$R1_act)

cc = cc[!is.na(cc$newAct),]

cc$groupNewAct <- as.factor(cut(cc$newAct, quantile(cc$newAct,(0:10)/10), include.lowest=TRUE, labels=LETTERS[1:10]))
cc$groupAct <- as.factor(cut(cc$ACTIVITY, quantile(cc$ACTIVITY,(0:10)/10), include.lowest=TRUE, labels=LETTERS[1:10]))

cc$groupNewAct2 <- cut(cc$newAct, 
                                   breaks = c(-Inf, 100, 200, 300,Inf),
                                   labels = c("A", "B", "C", "D"),
                                   right = FALSE)

A = (cc[which(cc$groupNewAct == "A"),])
B = (cc[which(cc$groupNewAct == "B"),])
C = (cc[which(cc$groupNewAct == "C"),])
D = (cc[which(cc$groupNewAct == "D"),])
E = (cc[which(cc$groupNewAct == "E"),])
F = (cc[which(cc$groupNewAct == "F"),])
G = (cc[which(cc$groupNewAct == "G"),])
H = (cc[which(cc$groupNewAct == "H"),])
I = (cc[which(cc$groupNewAct == "I"),])
J = (cc[which(cc$groupNewAct == "J"),])

summary(A$DAYS)
summary(B$DAYS)
summary(C$DAYS)
summary(D$DAYS)
summary(E$DAYS)
summary(F$DAYS)
summary(G$DAYS)
summary(H$DAYS)
summary(I$DAYS)
summary(J$DAYS)

boxplot(A$DAYS,B$DAYS,C$DAYS,D$DAYS,
        E$DAYS,F$DAYS,G$DAYS,H$DAYS,
        I$DAYS,J$DAYS,outline=FALSE)

filter_alldata$groupActNew2 <- cut(filter_alldata$ACTIVITY, 
                          breaks = c(-Inf, 500, 700, 1000,Inf),
                          labels = c("A", "B", "C", "D"),
                          right = FALSE)

pairwise.wilcox.test(as.numeric(cc$DAYS), cc$groupNewAct, p.adj="bonferroni", exact=F)
A = (filter_alldata[which(filter_alldata$groupActNew2 == "A"),])
B = (filter_alldata[which(filter_alldata$groupActNew2 == "B"),])
C = (filter_alldata[which(filter_alldata$groupActNew2 == "C"),])
D = (filter_alldata[which(filter_alldata$groupActNew2 == "D"),])

summary(A$DAYS)
summary(B$DAYS)
summary(C$DAYS)
summary(D$DAYS)

boxplot(A$DAYS,B$DAYS,C$DAYS,D$DAYS,outline=TRUE)

f <- read.csv("newRA.csv", header=TRUE, sep="|",stringsAsFactors=FALSE)
f_R1 = f[,c("ID","BRANCH","PROJECT","CREATEDON","R1_EMAIL_A","R1_GRANTEDON","R1_ACTIVITY")]
f_R2 = f[,c("ID","BRANCH","PROJECT","CREATEDON","R2_EMAIL_A","R2_GRANTEDON","R2_ACTIVITY")]

names(f_R1)[5] <- "email"
names(f_R1)[6] <- "grantedon"
names(f_R1)[7] <- "activity"
names(f_R2)[5] <- "email"
names(f_R2)[6] <- "grantedon"
names(f_R2)[7] <- "activity"

f_All = rbind(f_R1,f_R2)

f_All$days = (f_All$grantedon-f_All$CREATEDON)/60/60/24
f_All = f_All[!is.na(f_All$activity),]
f_All = f_All[which(f_All$days<20),]

f_All$group <- as.factor(cut(f_All$activity, quantile(f_All$activity,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

kruskal.test(days~group,data=f_All)
pairwise.wilcox.test(as.numeric(f_All$days), f_All$group, p.adj="bonferroni", exact=F)

A = (f_All[which(f_All$group == "A"),])
B = (f_All[which(f_All$group == "B"),])
C = (f_All[which(f_All$group == "C"),])
D = (f_All[which(f_All$group == "D"),])

plot(filter_alldata$CREATEDON, filter_alldata$DAYS)

summary(A$days)
summary(B$days)
summary(C$days)
summary(D$days)

boxplot(A$days,B$days,C$days,D$days)

###########


write.csv(filter_alldata,file="hahaha.csv")


kruskal.test(DAYS~groupNew,data=mamama)
pairwise.wilcox.test(as.numeric(mamama$DAYS), mamama$groupNew, p.adj="bonferroni", exact=F)

A = (mamama[which(mamama$groupNew == "A"),])
B = (mamama[which(mamama$groupNew == "B"),])
C = (mamama[which(mamama$groupNew == "C"),])
D = (mamama[which(mamama$groupNew == "D"),])

boxplot(A$DAYS,B$DAYS,C$DAYS,D$DAYS)

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

#check for submitter organization first
filter_alldata$DAYS = as.numeric(filter_alldata$DAYS)
filter_alldata$submitter_org = as.factor(filter_alldata$submitter_org)
kruskal.test(DAYS~submitter_org, data=filter_alldata)

#stat significant for submitter organization
# change organization gmail.com to *independent
filter_alldata$submitter_org[filter_alldata$submitter_org == "gmail.com"] <- "*independent"
filter_alldata$submitter_org[filter_alldata$submitter_org == "HP"] <- "HPE"
filter_alldata$submitter_org[filter_alldata$submitter_org == "gessau.net"] <- "IBM"

#apply to R1_org and R2_org as well
filter_alldata$R1_org[filter_alldata$R1_org == "gmail.com"] <- "*independent"
filter_alldata$R1_org[filter_alldata$R1_org == "HP"] <- "HPE"
filter_alldata$R1_org[filter_alldata$R1_org == "gessau.net"] <- "IBM"

filter_alldata$R2_org[filter_alldata$R2_org == "gmail.com"] <- "*independent"
filter_alldata$R2_org[filter_alldata$R2_org == "HP"] <- "HPE"
filter_alldata$R2_org[filter_alldata$R2_org == "gessau.net"] <- "IBM"

# aggregate by s_org
summary_sOrg <- aggregate(cbind(count = submitter_org) ~ submitter_org,
                   data=filter_alldata,
                   FUN = function(x){NROW(x)})

# threshold 2%, if lower than 2%, set organization to other
summary_sOrg <- summary_sOrg[order(-summary_sOrg$count),]
summary_sOrg$perc <- summary_sOrg$count/sum(summary_sOrg$count)*100
summary_sOrg$new_org <- ifelse(summary_sOrg$perc>2, as.character(summary_sOrg$submitter_org), "Other")

# add new organization in filter_alldata
vuvu <- merge(x = filter_alldata, y = summary_sOrg, 
                    by = c("submitter_org"), all.x = TRUE)

# summary of new table
summary_sOrg_new <- aggregate(summary_sOrg$count,by=list(Organization=summary_sOrg$new_org), FUN=sum)

# check pairwise comparison in the new organization DF (vuvu)
vuvu$new_org = as.factor(vuvu$new_org)
kruskal.test(DAYS~new_org, data=vuvu)
pairwise.wilcox.test(as.numeric(vuvu$DAYS), vuvu$new_org, p.adj="bonferroni", exact=F)

# get the mean and median
submitter_summary_mean <- aggregate(vuvu$DAYS, by=list(vuvu$new_org), mean)

submitter_summary_median <- aggregate(vuvu$DAYS, by=list(vuvu$new_org), median)

names(submitter_summary_mean)[1] <- "new_org"
names(submitter_summary_mean)[2] <- "mean"
names(submitter_summary_median)[1] <- "new_org"
names(submitter_summary_median)[2] <- "median"
names(summary_sOrg_new)[1] <- "new_org"
names(summary_sOrg_new)[2] <- "count"

# Here we can see the total patches, mean, and median for company with >2% contribution
submitter_summary <- join_all(list(summary_sOrg_new,submitter_summary_mean,
                              submitter_summary_median), 
                              by = 'new_org', type = 'full')

# find top reviewer organization
aR1 = data.frame(filter_alldata[,c("R1_org")])
aR2 = data.frame(filter_alldata[,c("R2_org")])

names(aR1)[1] <- "org"
names(aR2)[1] <- "org"

fR = rbind(aR1,aR2)
total_fR <- aggregate(cbind(count = org) ~ org,
                   data=fR,
                   FUN = function(x){NROW(x)})
total_fR <- total_fR[which(total_fR$org!="N/A"),]
# order
total_fR <- total_fR[order(-total_fR$count),]
# add %
total_fR$perc <- total_fR$count/sum(total_fR$count)*100

# take biggest 2% (the same as submitter). set other org as other
total_fR$new_org <- ifelse(total_fR$perc>2, as.character(total_fR$org), "Other")

# add new organization by merge. but first, change names into R1_org and R2_org
names(total_fR)[1] <- "R1_org"
names(total_fR)[4] <- "new_R1_org"

total_fR2 = total_fR
names(total_fR2)[1] <- "R2_org"
names(total_fR2)[4] <- "new_R2_org"

vuvu <- merge(x = vuvu, y = total_fR, 
         by = c("R1_org"), all.x = TRUE)
vuvu <- merge(x = vuvu, y = total_fR2, 
              by = c("R2_org"), all.x = TRUE)

# create a tuple of reviewing organization i.e. (IBM,Red Hat)
# first create a function that accepts 2 parameter (R1, R2) and return in the tuple format

createTuple <- function(r1, r2) {
  if(!is.na(r2))
  {
    sort = mixedsort(c(r1,r2))
    stringSort = paste(sort[1],sort[2], sep=",")
    as.character(stringSort)
  }
  else
  {
    as.character(r1)
  }
}

# assign a reviewer tuple in each row of vuvu DF
for (row in 1:nrow(vuvu)) { 
  vuvu$reviewer_tuple[row] <- createTuple(vuvu$new_R1_org[row], vuvu$new_R2_org[row]) 
}

# check count from each tuple
tuple_summary <- aggregate(cbind(count = reviewer_tuple) ~ reviewer_tuple,
                          data=vuvu,
                          FUN = function(x){NROW(x)})
tuple_summary <- tuple_summary[order(-tuple_summary$count),]
tuple_summary$perc <- tuple_summary$count/sum(tuple_summary$count)*100

# we are particularly interested to see the >2% tuple so we filter the review in top 2%
# tuple_summary <- tuple_summary[which(tuple_summary$perc>2),]
vuvu_filter = vuvu[vuvu$reviewer_tuple %in% tuple_summary$reviewer_tuple,]

# check kruskal test
vuvu_filter$reviewer_tuple <- as.factor(vuvu_filter$reviewer_tuple)
kruskal.test(DAYS~reviewer_tuple,data=vuvu_filter)

# significant! so let's continue to MWW
# MWW
haha = vuvu_filter[grepl("IBM",vuvu_filter$reviewer_tuple) |
                     grepl("Red Hat",vuvu_filter$reviewer_tuple) |
                     grepl("Rackspace",vuvu_filter$reviewer_tuple) |
                     grepl("HPE",vuvu_filter$reviewer_tuple),]
pairwise.wilcox.test(as.numeric(haha$DAYS), haha$reviewer_tuple, p.adj="bonferroni", exact=F)

# get the mean and median
reviewer_mean <- aggregate(vuvu$DAYS, by=list(vuvu$reviewer_tuple), mean)

reviewer_median <- aggregate(vuvu$DAYS, by=list(vuvu$reviewer_tuple), median)

names(reviewer_mean)[1] <- "new_rev_org"
names(reviewer_mean)[2] <- "mean"
names(reviewer_median)[1] <- "new_rev_org"
names(reviewer_median)[2] <- "median"
names(tuple_summary)[1] <- "new_rev_org"
names(tuple_summary)[2] <- "count"

# Here we can see the total patches, mean, and median for company with >2% contribution
reviewer_summary <- join_all(list(tuple_summary,reviewer_mean,
                                   reviewer_median), 
                              by = 'new_rev_org', type = 'full')

# remove NA
reviewer_summary = reviewer_summary[!is.na(reviewer_summary$count),]

# review organization is statistically significant

# we want to zoom in to 4 companies (IBM, Red Hat, Rackspace, HPE)
# we are interested to see how each of the company works with others apart from the 3 remaining companies
# so we create a group
# create a group and filter top 4 companies to observe it better
createGroup <- function(r1){
  groups <- c('IBM', 'Red Hat', 'Rackspace', 'HPE')
  for(i in groups)
  {
    if(grepl(i,r1))
    {
      bool <- TRUE
      for(j in setdiff(groups,c(i)))
      {
        if(grepl(j, r1)) {
          bool <- FALSE
        }
      }
      if(bool) {
        return(as.character(i))
      }
      else{return(NA)}
    }
  }
  return(NA)
}

for (row in 1:nrow(vuvu)) { 
  vuvu$new_reviewer_group[row] <- createGroup(vuvu$reviewer_tuple[row]) 
}

vuvu$new_reviewer_group <- as.factor(vuvu$new_reviewer_group)
# filter out R1 = R2 and R2 = NA
vuvu_new <- vuvu[which(vuvu$R1_org != vuvu$R2_org & !is.na(vuvu$R2_org)),]
kruskal.test(DAYS~new_reviewer_group,data=vuvu_new)
pairwise.wilcox.test(as.numeric(vuvu_new$DAYS), vuvu_new$new_reviewer_group, p.adj="bonferroni", exact=F)

# HPE-Red Hat is not stat significant in term of review time
# The rest is stat significant
# IBM-Red Hat is less stat significant than the other tuple

# Which organization works better with others?
b_IBM = vuvu_new[which(vuvu_new$new_reviewer_group == "IBM")]
b_RH = vuvu_new[which(vuvu_new$new_reviewer_group == "IBM")]
b_IBM = vuvu_new[which(vuvu_new$new_reviewer_group == "IBM")]
b_IBM = vuvu_new[which(vuvu_new$new_reviewer_group == "IBM")]

# function with R1,R2,P
# the output should be 0 : P's org is not in R's org
# 1 : P's org equal to one of R's org
# 2 : P's org equal to both R's org
createRPGroup <- function(r1,r2,p) {
  review <- c(r1, r2)
  if(is.na(r2))
  {
    if(!(p %in% review)) {
      return("0")
    }
    else {
      return("2")
    }
  }
  else
  {
    if(!(p %in% review)) {
      return("0")
    }
    else if(xor(p == r1, p == r2)) {
      return("1")
    }
    else {
      return("2")
    }
  }
}

# let's apply it
for (row in 1:nrow(vuvu)) { 
  vuvu$new_RP_group[row] <- 
    createRPGroup(vuvu$R1_org[row],
                  vuvu$R2_org[row],
                  vuvu$submitter_org[row]) 
}

vuvu$new_RP_group <- as.factor(vuvu$new_RP_group)
kruskal.test(DAYS~new_RP_group,data=vuvu)
pairwise.wilcox.test(as.numeric(vuvu$DAYS), vuvu$new_RP_group, p.adj="bonferroni", exact=F)

a0 = vuvu[which(vuvu$new_RP_group == "0"),]
a1 = vuvu[which(vuvu$new_RP_group == "1"),]
a2 = vuvu[which(vuvu$new_RP_group == "2"),]

boxplot(a0$DAYS,a1$DAYS,a2$DAYS,outline=FALSE)

summary(a0$DAYS)
summary(a1$DAYS)
summary(a2$DAYS)

a0 = vuvu[which(vuvu$new_RP_group == "0" & vuvu$new_org == "Red Hat"),]
a1 = vuvu[which(vuvu$new_RP_group == "1" & vuvu$new_org == "Red Hat"),]
a2 = vuvu[which(vuvu$new_RP_group == "2" & vuvu$new_org == "Red Hat"),]

boxplot(a0$DAYS,a1$DAYS,a2$DAYS,outline=FALSE)

summary(a0$DAYS)
summary(a1$DAYS)
summary(a2$DAYS)

############################ 
# organization on positivity
############################




