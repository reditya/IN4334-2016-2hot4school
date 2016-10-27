st<- read.csv("time_size.csv", header = TRUE, sep=',')

st$group <- as.factor(cut(st$loc,quantile(st$loc,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

#Kruskal-Wallis test
kruskal.test(time~group, data=st)
qchisq(0.950, 3)

pairwise.wilcox.test(st$time, st$group, p.adj="bonferroni", exact=F)
summary(st[which(st$group=='A'),])

pos_A = 2172/2588
pos_B = 2006/2432
pos_C = 1978/2495
pos_D = 1856/2497

st<- read.csv("time_size.csv", header = TRUE, sep=',')

st_rp <- st[ which(st$trans=='r+'), ]
st_rm <- st[ which(st$trans=='r-'), ]

st_rp$group <- as.factor(cut(st_rp$loc,quantile(st_rp$loc,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))
st_rm$group <- as.factor(cut(st_rm$loc,quantile(st_rm$loc,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

st_rp$group <- as.factor(cut(st_rp$loc,quantile(st_rp$loc,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))
st_rm$group <- as.factor(cut(st_rm$loc,quantile(st_rm$loc,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

# KW for R+
kruskal.test(time~group, data=st_rp)

pairwise.wilcox.test(st_rp$time, st_rp$group, p.adj="bonferroni", exact=F)
