# KW test submitter all data
kruskal.test(DAYS~as.factor(submitter_org),data=filter_alldata)

# Create overview of contribution of organizations for submitter and reviewer
s_org <- data.frame(table(filter_alldata$submitter_org))
colnames(s_org) <- c("org", "freq")
s_org$perc <- s_org$freq / sum(s_org$freq) * 100
s_org <- s_org[order(-s_org$perc),]

# Let's consider only top 4 companies
top4_org <- c("Red Hat", "IBM", "HPE", "Mirantis")
top4_data <- filter_alldata
top4_data$submitter_org[!top4_data$submitter_org %in% top4_org] <- "Other"
top4_data <- top4_data[which(top4_data$submitter_org != "Other"),]

redhat <- top4_data[which(top4_data$submitter_org == "Red Hat"),]
redhat_pos <- nrow(redhat[which(redhat$POSITIVITY == 1),]) / nrow(redhat)

IBM <- top4_data[which(top4_data$submitter_org == "IBM"),]
IBM_pos <- nrow(IBM[which(IBM$POSITIVITY == 1),]) / nrow(IBM)

mirantis <- top4_data[which(top4_data$submitter_org == "Mirantis"),]
mirantis_pos <- nrow(mirantis[which(mirantis$POSITIVITY == 1),]) / nrow(mirantis)

redhat <- top4_data[which(top4_data$submitter_org == "Red Hat"),]
redhat_pos <- nrow(redhat[which(redhat$POSITIVITY == 1),]) / nrow(redhat)

# pairwise test top 4 data
pairwise.wilcox.test(top4_data$DAYS, top4_data$submitter_org, p.adj="bonferroni", exact=F)
pairwise.wilcox.test(top4_data$POSITIVITY, top4_data$submitter_org, p.adj="bonferroni", exact=F)

# function with R1,R2,P
# the output should be 0 : P's org is not in R's org
# 1 : P's org equal to one of R's org
# 2 : P's org equal to both R's org
createRPGroup <- function(r1,r2,p) {
  review <- c(r1, r2)
  if(is.na(r2))
  {
    if(!(p %in% review)) {
      return("A")
    }
    else {
      return("C")
    }
  }
  else
  {
    if(!(p %in% review)) {
      return("A")
    }
    else if(xor(p == r1, p == r2)) {
      return("B")
    }
    else {
      return("C")
    }
  }
}

# let's apply it
for (row in 1:nrow(filter_alldata)) { 
  filter_alldata$group[row] <- 
    createRPGroup(filter_alldata$R1_org[row],
                  filter_alldata$R2_org[row],
                  filter_alldata$submitter_org[row])
}
filter_alldata$group <- as.factor(filter_alldata$group)

patchA <- filter_alldata[which(filter_alldata$group == "A"),]
patchB <- filter_alldata[which(filter_alldata$group == "B"),]
patchC <- filter_alldata[which(filter_alldata$group == "C"),]

pos_A <- nrow(patchA[which(patchA$POSITIVITY == 1),]) / nrow(patchA)
pos_B <- nrow(patchB[which(patchB$POSITIVITY == 1),]) / nrow(patchB)
pos_C <- nrow(patchC[which(patchC$POSITIVITY == 1),]) / nrow(patchC)

boxplot(filter_alldata$DAYS ~ filter_alldata$group, ylab="Review time (days)", names=c("A", "B", "C"), outline=FALSE)

# KW test groups
kruskal.test(DAYS~group,data=filter_alldata)
kruskal.test(POSITIVITY~group,data=filter_alldata)

#pairwise test groups
pairwise.wilcox.test(filter_alldata$DAYS, filter_alldata$group, p.adj="bonferroni", exact=F)
pairwise.wilcox.test(filter_alldata$POSITIVITY, filter_alldata$group, p.adj="bonferroni", exact=F)

# Zoom in on Red Hat, IBM and HPE

# IBM
IBM <- filter_alldata[which(filter_alldata$submitter_org == "IBM"),]
# create groups for IBM
for (row in 1:nrow(IBM)) { 
  IBM$group[row] <- 
    createRPGroup(IBM$R1_org[row],
                  IBM$R2_org[row],
                  IBM$submitter_org[row])
}
IBM$group <- as.factor(IBM$group)

# KW test on IBM
kruskal.test(DAYS~group,data=IBM)
kruskal.test(POSITIVITY~group,data=IBM)

# Pairwise test on IBM
pairwise.wilcox.test(IBM$DAYS, IBM$group, p.adj="bonferroni", exact=F)
pairwise.wilcox.test(IBM$POSITIVITY, IBM$group, p.adj="bonferroni", exact=F)

patchAIBM <- IBM[which(IBM$group=="A"),]
patchBIBM <- IBM[which(IBM$group=="B"),]
patchCIBM <- IBM[which(IBM$group=="C"),]

posAIBM <- nrow(patchAIBM[which(patchAIBM$POSITIVITY==1),]) / nrow(patchAIBM)
posBIBM <- nrow(patchBIBM[which(patchBIBM$POSITIVITY==1),]) / nrow(patchBIBM)
posCIBM <- nrow(patchCIBM[which(patchCIBM$POSITIVITY==1),]) / nrow(patchCIBM)

# HPE
HPE <- filter_alldata[which(filter_alldata$submitter_org == "HPE"),]
# create groups for HPE
for (row in 1:nrow(HPE)) { 
  HPE$group[row] <- 
    createRPGroup(HPE$R1_org[row],
                  HPE$R2_org[row],
                  HPE$submitter_org[row])
}
HPE$group <- as.factor(HPE$group)

# KW test on HPE
kruskal.test(DAYS~group,data=HPE)
kruskal.test(POSITIVITY~group,data=HPE)

# Pairwise test on HPE
pairwise.wilcox.test(HPE$DAYS, HPE$group, p.adj="bonferroni", exact=F)
pairwise.wilcox.test(HPE$POSITIVITY, HPE$group, p.adj="bonferroni", exact=F)

patchAHPE <- HPE[which(HPE$group=="A"),]
patchBHPE <- HPE[which(HPE$group=="B"),]
patchCHPE <- HPE[which(HPE$group=="C"),]

posAHPE <- nrow(patchAHPE[which(patchAHPE$POSITIVITY==1),]) / nrow(patchAHPE)
posBHPE <- nrow(patchBHPE[which(patchBHPE$POSITIVITY==1),]) / nrow(patchBHPE)
posCHPE <- nrow(patchCHPE[which(patchCHPE$POSITIVITY==1),]) / nrow(patchCHPE)

# Red Hat
redhat <- filter_alldata[which(filter_alldata$submitter_org == "Red Hat"),]
# create groups for Red Hat
for (row in 1:nrow(redhat)) { 
  redhat$group[row] <- 
    createRPGroup(redhat$R1_org[row],
                  redhat$R2_org[row],
                  redhat$submitter_org[row])
}
redhat$group <- as.factor(redhat$group)

# KW test on Red Hat
kruskal.test(DAYS~group,data=redhat)
kruskal.test(POSITIVITY~group,data=redhat)

# Pairwise test on Red Hat
pairwise.wilcox.test(redhat$DAYS, redhat$group, p.adj="bonferroni", exact=F)
pairwise.wilcox.test(redhat$POSITIVITY, redhat$group, p.adj="bonferroni", exact=F)

patchAredhat <- redhat[which(redhat$group=="A"),]
patchBredhat <- redhat[which(redhat$group=="B"),]
patchCredhat <- redhat[which(redhat$group=="C"),]

posAredhat <- nrow(patchAredhat[which(patchAredhat$POSITIVITY==1),]) / nrow(patchAredhat)
posBredhat <- nrow(patchBredhat[which(patchBredhat$POSITIVITY==1),]) / nrow(patchBredhat)
posCredhat <- nrow(patchCredhat[which(patchCredhat$POSITIVITY==1),]) / nrow(patchCredhat)