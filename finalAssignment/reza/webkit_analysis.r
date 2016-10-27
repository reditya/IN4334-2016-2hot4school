
########################################################
#		 == Minimizing Time ==
########################################################

#####################################
# Effect of Patch Size on Time
#####################################

st<- read.csv("time_size.csv", header = TRUE, sep=',')

st$group <- as.factor( cut(st$loc, 4, labels=c('a','b','c','d')))
st$group <- as.factor(cut2(st$loc, g=4, labels=True))

# nearly equal number of observations in each group using quantile function

st<- read.csv("time_size.csv", header = TRUE, sep=',')

st$group <- as.factor(cut(st$loc,quantile(st$loc,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

#Kruskal-Wallis test
kruskal.test(time~group, data=st)

Kruskal-Wallis rank sum test

data:  time by group 
Kruskal-Wallis chi-squared = 83.7832, df = 3, p-value < 2.2e-16


#compare this value to chi-sq 83 >7
qchisq(0.950, 3)
[1] 7.814728


#Man-Whitney with Bonferroni correction
pairwise.wilcox.test(st$time, st$group, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  st$time and st$group 

  A       B       C     
B 0.2236  -       -     
C 4.3e-06 0.0268  -     
D < 2e-16 5.8e-10 0.0012

P value adjustment method: bonferroni

#Conclusion: stat diff between all groups (p-value < 0.05) except A-B. 
# Response time for patches between groups is stat sign except group A-B. 

# Mean and Median values for each group

summary(st[which(st$group=='A'),])
 trans          time               loc        group   
 r-: 416   Min.   :   0.267   Min.   :0.000   A:2588  
 r+:2172   1st Qu.:   7.817   1st Qu.:2.000   B:   0  
           Median :  49.892   Median :4.000   C:   0  
           Mean   : 494.007   Mean   :3.947   D:   0  
           3rd Qu.: 399.062   3rd Qu.:6.000           
           Max.   :7249.167   Max.   :8.000           
 
summary(st[which(st$group=='B'),])
 trans          time               loc        group   
 r-: 426   Min.   :   0.367   Min.   : 9.00   A:   0  
 r+:2006   1st Qu.:   9.508   1st Qu.:12.00   B:2432  
           Median :  54.442   Median :16.00   C:   0  
           Mean   : 556.162   Mean   :17.71   D:   0  
           3rd Qu.: 483.479   3rd Qu.:23.00           
           Max.   :7239.433   Max.   :30.00           
summary(st[which(st$group=='C'),])
 trans          time               loc         group   
 r-: 517   Min.   :   0.417   Min.   : 31.00   A:   0  
 r+:1978   1st Qu.:  12.292   1st Qu.: 42.00   B:   0  
           Median :  64.583   Median : 55.00   C:2495  
           Mean   : 608.733   Mean   : 59.06   D:   0  
           3rd Qu.: 568.800   3rd Qu.: 75.00           
           Max.   :7244.717   Max.   :103.00           
summary(st[which(st$group=='D'),])
 trans          time               loc          group   
 r-: 641   Min.   :   0.483   Min.   :  104.0   A:   0  
 r+:1856   1st Qu.:  16.733   1st Qu.:  145.0   B:   0  
           Median :  90.317   Median :  216.0   C:   0  
           Mean   : 624.766   Mean   :  443.8   D:2497  
           3rd Qu.: 654.850   3rd Qu.:  385.0           
           Max.   :7226.133   Max.   :25927.0  

# Positivity for each group:
pos_A = 2172/2588
pos_B = 2006/2432
pos_C = 1978/2495
pos_D = 1856/2497

> pos_A
[1] 0.8392581
> pos_B
[1] 0.8248355
> pos_C
[1] 0.7927856
> pos_D
[1] 0.743292

# Split data into 4 bins for two groups: R+ and R-

st<- read.csv("time_size.csv", header = TRUE, sep=',')

st_rp <- st[ which(st$trans=='r+'), ]
st_rm <- st[ which(st$trans=='r-'), ]

st_rp$group <- as.factor(cut(st_rp$loc,quantile(st_rp$loc,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))
st_rm$group <- as.factor(cut(st_rm$loc,quantile(st_rm$loc,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

st_rp$group <- as.factor(cut(st_rp$loc,quantile(st_rp$loc,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))
st_rm$group <- as.factor(cut(st_rm$loc,quantile(st_rm$loc,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

# KW for R+
kruskal.test(time~group, data=st_rp)

	Kruskal-Wallis rank sum test

data:  time by group 
Kruskal-Wallis chi-squared = 55.3165, df = 3, p-value = 5.878e-12


#post-hoc MWW test for R+
pairwise.wilcox.test(st_rp$time, st_rp$group, p.adj="bonferroni", exact=F)

Pairwise comparisons using Wilcoxon rank sum test 

data:  st_rp$time and st_rp$group 

  A       B       C      
B 0.02759 -       -      
C 0.00016 1.00000 -      
D 1.5e-12 0.00011 0.01420

P value adjustment method: bonferroni 

#Result: acceptance time between groups is statistically different, except for the B-C group.

#Calculating effect size for A-B group:

wilcox_test(time ~ factor(group), data=st_rp[st_rp$group=='A'|st_rp$group=='B',], distribution="approximate")

	Approximative Wilcoxon Mann-Whitney Rank Sum Test

data:  time by factor(group) (A, B) 
Z = -2.8339, p-value = 0.003
alternative hypothesis: true mu is not equal to 0 

> summary(st_rp)
 trans          time               loc        group   
 r-:   0   Min.   :   0.267   Min.   :    0   A:2172  
 r+:8012   1st Qu.:   9.067   1st Qu.:    8   B:1882  
           Median :  48.275   Median :   28   C:1957  
           Mean   : 512.322   Mean   :  126   D:2001  
           3rd Qu.: 421.754   3rd Qu.:   93           
           Max.   :7244.717   Max.   :25927           
> 2172+1882
[1] 4054

#Effect size r = 0.04, p=0.003 (p <0.05)
> 2.8339/sqrt(4054)
[1] 0.04450847


#Mean and Medians for R+ group

summary(st_rp[which(st_rp$group=='A'),])
summary(st_rp[which(st_rp$group=='B'),])
summary(st_rp[which(st_rp$group=='C'),])
summary(st_rp[which(st_rp$group=='D'),])

> summary(st_rp[which(st_rp$group=='A'),])
 trans          time               loc        group   
 r-:   0   Min.   :   0.267   Min.   :0.000   A:2172  
 r+:2172   1st Qu.:   6.608   1st Qu.:2.000   B:   0  
           Median :  38.717   Median :4.000   C:   0  
           Mean   : 439.780   Mean   :3.971   D:   0  
           3rd Qu.: 313.583   3rd Qu.:6.000           
           Max.   :7239.183   Max.   :8.000           
> summary(st_rp[which(st_rp$group=='B'),])
 trans          time               loc        group   
 r-:   0   Min.   :   0.367   Min.   : 9.00   A:   0  
 r+:1882   1st Qu.:   7.987   1st Qu.:12.00   B:1882  
           Median :  46.308   Median :16.00   C:   0  
           Mean   : 530.943   Mean   :16.91   D:   0  
           3rd Qu.: 436.842   3rd Qu.:22.00           
           Max.   :7239.433   Max.   :28.00           
> summary(st_rp[which(st_rp$group=='C'),])
 trans          time               loc        group   
 r-:   0   Min.   :   0.417   Min.   :29.00   A:   0  
 r+:1957   1st Qu.:   9.867   1st Qu.:37.00   B:   0  
           Median :  47.883   Median :50.00   C:1957  
           Mean   : 541.859   Mean   :53.73   D:   0  
           3rd Qu.: 452.700   3rd Qu.:67.00           
           Max.   :7244.717   Max.   :93.00           
> summary(st_rp[which(st_rp$group=='D'),])
 trans          time               loc          group   
 r-:   0   Min.   :   0.483   Min.   :   94.0   A:   0  
 r+:2001   1st Qu.:  13.200   1st Qu.:  133.0   B:   0  
           Median :  63.667   Median :  199.0   C:   0  
           Mean   : 544.662   Mean   :  431.5   D:2001  
           3rd Qu.: 530.400   3rd Qu.:  353.0           
           Max.   :7221.083   Max.   :25927.0 


# KW for R-
kruskal.test(time~group, data=st_rm)

	Kruskal-Wallis rank sum test

data:  time by group 
Kruskal-Wallis chi-squared = 6.7093, df = 3, p-value = 0.08177
#Result: no difference


# Mean and Medians for R- group

> summary(st_rm[which(st_rm$group=='A'),])
 trans         time               loc         group  
 r-:506   Min.   :   0.683   Min.   : 1.000   A:506  
 r+:  0   1st Qu.:  21.717   1st Qu.: 2.000   B:  0  
          Median : 163.908   Median : 4.000   C:  0  
          Mean   : 722.005   Mean   : 4.931   D:  0  
          3rd Qu.: 728.771   3rd Qu.: 7.000          
          Max.   :7249.167   Max.   :11.000          
> summary(st_rm[which(st_rm$group=='B'),])
 trans         time              loc        group  
 r-:504   Min.   :   0.45   Min.   :12.00   A:  0  
 r+:  0   1st Qu.:  24.06   1st Qu.:17.00   B:504  
          Median : 141.77   Median :24.00   C:  0  
          Mean   : 832.73   Mean   :26.47   D:  0  
          3rd Qu.: 907.80   3rd Qu.:35.00          
          Max.   :7136.48   Max.   :48.00          
> summary(st_rm[which(st_rm$group=='C'),])
 trans         time               loc        group  
 r-:491   Min.   :   0.817   Min.   : 49.0   A:  0  
 r+:  0   1st Qu.:  34.958   1st Qu.: 61.5   B:  0  
          Median : 157.550   Median : 81.0   C:491  
          Mean   : 779.795   Mean   : 86.0   D:  0  
          3rd Qu.: 856.425   3rd Qu.:108.0          
          Max.   :7127.483   Max.   :140.0          
> summary(st_rm[which(st_rm$group=='D'),])
 trans         time               loc          group  
 r-:499   Min.   :   1.283   Min.   :  141.0   A:  0  
 r+:  0   1st Qu.:  34.217   1st Qu.:  193.5   B:  0  
          Median : 174.033   Median :  298.0   C:  0  
          Mean   : 876.299   Mean   :  484.1   D:499  
          3rd Qu.:1031.658   3rd Qu.:  505.5          
          Max.   :7226.133   Max.   :12107.0 


#####################################
# Patch Size on Number of Revisions
#####################################

sr<- read.csv("size_revisions.csv", header = TRUE, sep=',')

sr$group <- as.factor(cut(sr$loc,quantile(sr$loc,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

#KW test for # of revisions
kruskal.test(rev~group, data=sr)

	Kruskal-Wallis rank sum test

data:  rev by group 
Kruskal-Wallis chi-squared = 1326.58, df = 3, p-value < 2.2e-16
#Result: there is stat sign difference somewhere


#MWW
pairwise.wilcox.test(sr$rev, sr$group, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  sr$rev and sr$group 

  A      B      C     
B <2e-16 -      -     
C <2e-16 <2e-16 -     
D <2e-16 <2e-16 <2e-16

P value adjustment method: bonferroni 

#Result: effect of pach size on the rounds of revisions are stat different for all groups.

summary(sr[which(sr$group=='A'),])
summary(sr[which(sr$group=='B'),])
summary(sr[which(sr$group=='C'),])
summary(sr[which(sr$group=='D'),])

 summary(sr[which(sr$group=='A'),])
     bugid            loc              rev         group   
 Min.   :58304   Min.   : 1.000   Min.   : 1.000   A:1799  
 1st Qu.:64911   1st Qu.: 4.000   1st Qu.: 1.000   B:   0  
 Median :71608   Median : 8.000   Median : 1.000   C:   0  
 Mean   :71332   Mean   : 9.727   Mean   : 1.322   D:   0  
 3rd Qu.:77956   3rd Qu.:15.000   3rd Qu.: 1.000           
 Max.   :83708   Max.   :23.000   Max.   :13.000           

 summary(sr[which(sr$group=='B'),])
     bugid            loc             rev         group   
 Min.   :58316   Min.   :24.00   Min.   : 1.000   A:   0  
 1st Qu.:64437   1st Qu.:33.00   1st Qu.: 1.000   B:1804  
 Median :71694   Median :44.00   Median : 1.000   C:   0  
 Mean   :71353   Mean   :45.29   Mean   : 1.706   D:   0  
 3rd Qu.:78142   3rd Qu.:57.00   3rd Qu.: 2.000           
 Max.   :83749   Max.   :72.00   Max.   :22.000           

 summary(sr[which(sr$group=='C'),])
     bugid            loc             rev         group   
 Min.   :58305   Min.   : 73.0   Min.   : 1.000   A:   0  
 1st Qu.:65184   1st Qu.: 94.0   1st Qu.: 1.000   B:   0  
 Median :71500   Median :122.0   Median : 2.000   C:1785  
 Mean   :71380   Mean   :126.3   Mean   : 2.438   D:   0  
 3rd Qu.:77887   3rd Qu.:155.0   3rd Qu.: 3.000           
 Max.   :83748   Max.   :206.0   Max.   :16.000           

 summary(sr[which(sr$group=='D'),])
     bugid            loc               rev         group   
 Min.   :58320   Min.   :  207.0   Min.   : 1.000   A:   0  
 1st Qu.:65388   1st Qu.:  284.0   1st Qu.: 1.000   B:   0  
 Median :71407   Median :  412.0   Median : 3.000   C:   0  
 Mean   :71328   Mean   :  793.7   Mean   : 3.605   D:1792  
 3rd Qu.:77626   3rd Qu.:  750.0   3rd Qu.: 5.000  


# correlation

st<- read.csv("time_size.csv", header = TRUE, sep=',')

#correlation : size vs time
#spearman
cor.s = cor.test(st$loc, st$time, method="spearman")
cor.s

Spearmans rank correlation rho

data:  st$loc and st$time 
S = 151487229003, p-value < 2.2e-16
alternative hypothesis: true rho is not equal to 0 
sample estimates:
      rho 
0.0943409 

# Correlation of r+/r- patches (loc with time) 
st_rp <- st[ which(st$trans=='r+'), ]
st_rm <- st[ which(st$trans=='r-'), ]

# r+ patches
cor.s_rp = cor.test(st_rp$loc, st_rp$time, method="spearman")

# r- patches
cor.s_rm = cor.test(st_rm$loc, st_rm$time, method="spearman")

#############################
# Effect of Priority on Time
#############################

pt <- read.csv("time_priority.csv", header = TRUE, sep=',')

# KW: time and priority
kruskal.test(pt$time~pt$priority)
                           
	Kruskal-Wallis rank sum test

data:  pt$time by pt$priority 
Kruskal-Wallis chi-squared = 13.7688, df = 4, p-value = 0.008071
#The diff is stat sign

#MWW
pairwise.wilcox.test(pt$time, pt$priority, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  pt$time and pt$priority 

   P1     P2     P3     P4    
P2 1.0000 -      -      -     
P3 0.0480 0.0036 -      -     
P4 1.0000 1.0000 1.0000 -     
P5 1.0000 1.0000 1.0000 1.0000

P value adjustment method: bonferroni

# selecting only subset of patches with P1, P2 and P3 
pt_3 <- subset(pt, priority %in% c("P1", "P2", "P3"), select=trans:priority)

# KW: time and priority
kruskal.test(pt_3$time~pt_3$priority)

	Kruskal-Wallis rank sum test

data:  pt_3$time by pt_3$priority 
Kruskal-Wallis chi-squared = 12.707, df = 2, p-value = 0.001741
# the diff is stat sign.

#MWW
pairwise.wilcox.test(pt_3$time, pt_3$priority, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  pt_3$time and pt_3$priority 

   P1     P2    
P2 1.0000 -     
P3 0.0144 0.0011

P value adjustment method: bonferroni 

#median values for each priority group
summary(pt_3[which(pt_3$priority=='P1'),])
 trans         time          priority
 r-: 39   Min.   :   0.800   P1:251  
 r+:212   1st Qu.:   9.533   P2:  0  
          Median :  67.650   P3:  0  
          Mean   : 575.170   P4:  0  
          3rd Qu.: 569.958   P5:  0  
          Max.   :6870.767           

summary(pt_3[which(pt_3$priority=='P2'),])
 trans          time          priority 
 r-:1925   Min.   :   0.267   P1:   0  
 r+:7721   1st Qu.:  11.217   P2:9646  
           Median :  61.983   P3:   0  
           Mean   : 567.797   P4:   0  
           3rd Qu.: 510.904   P5:   0  
           Max.   :7249.167            
summary(pt_3[which(pt_3$priority=='P3'),])
 trans        time         priority
 r-:26   Min.   :   2.25   P1: 0   
 r+:66   1st Qu.:  36.93   P2: 0   
         Median : 226.03   P3:92   
         Mean   : 723.83   P4: 0   
         3rd Qu.: 754.01   P5: 0   
         Max.   :6956.20 

# A post-hoc test using Mann-Whitney tests with Bonferroni correction showed the significant differences between P1 and P3 (with median time values being 68 and 226 minutes repectively, p < 0.05) and between P2 and P3 (with median time values being 62 and 226 minutes repectively, p < 0.01).

#or using which function
pt_3<- pt[ which(pt$priority=='P1' | pt$priority=='P2' | pt$priority=='P3'), ]
 

# Priority for transaction type on the subset of P1-P3

# Subsets of r+/r- patches 
pt_rp <- pt_3[ which(pt_3$trans=='r+'), ]
pt_rm <- pt_3[ which(pt_3$trans=='r-'), ]

#for R+

kruskal.test(time~priority, data=pt_rp)
	Kruskal-Wallis rank sum test

data:  time by priority 
Kruskal-Wallis chi-squared = 13.1394, df = 2, p-value = 0.001402
# the diff is stat sign     

#MWW
pairwise.wilcox.test(pt_rp$time, pt_rp$priority, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  pt_rp$time and pt_rp$priority 

   P1      P2     
P2 1.00000 -      
P3 0.01846 0.00096

P value adjustment method: bonferroni 
        

#for R-
kruskal.test(time~priority, data=pt_rm)

	Kruskal-Wallis rank sum test

data:  time by priority 
Kruskal-Wallis chi-squared = 0.2325, df = 2, p-value = 0.8903
# the diff is not stat sign

###########################################
# Effect of Reveiw Queues on Time
###########################################

qt<- read.csv("time_rqueue.csv", header = TRUE, sep=';')

qt_rp <- qt[ which(qt$trans=='r+'), ]
qt_rm <- qt[ which(qt$trans=='r-'), ]

#KW for the whole sample
kruskal.test(qt$time~qt$rq1)

	Kruskal-Wallis rank sum test

data:  qt$time by qt$rq1 
Kruskal-Wallis chi-squared = 28.1587, df = 11, p-value = 0.003061
# there is stat diff

#MWW
pairwise.wilcox.test(qt$time, qt$rq1, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  qt$time and qt$rq1 

   0    1    2    3    4    5    6    7    8    9    10  
1  0.30 -    -    -    -    -    -    -    -    -    -   
2  1.00 1.00 -    -    -    -    -    -    -    -    -   
3  1.00 1.00 1.00 -    -    -    -    -    -    -    -   
4  1.00 1.00 1.00 1.00 -    -    -    -    -    -    -   
5  0.91 1.00 1.00 1.00 1.00 -    -    -    -    -    -   
6  1.00 1.00 1.00 1.00 1.00 1.00 -    -    -    -    -   
7  1.00 1.00 1.00 1.00 1.00 1.00 1.00 -    -    -    -   
8  1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 -    -    -   
9  1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 -    -   
10 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 -   
11 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00

P value adjustment method: bonferroni 

# Splitting into 3 groups
qt1$group <- as.factor(cut(qt1$rq1, c(0,1,3,11), include.lowest=TRUE, labels=LETTERS[1:3]))

> summary(qt1)
       id              rq1               rq2          trans          time          group   
 Min.   :  2.00   Min.   : 0.0000   Min.   : 0.0000   r-:1159   Min.   :   0.267   A:5245  
 1st Qu.: 22.00   1st Qu.: 0.0000   1st Qu.: 0.0000   r+:4901   1st Qu.:  11.600   B: 664  
 Median : 42.00   Median : 0.0000   Median : 0.0000             Median :  67.658   C: 151  
 Mean   : 61.42   Mean   : 0.5984   Mean   : 0.5977             Mean   : 576.330           
 3rd Qu.: 89.00   3rd Qu.: 1.0000   3rd Qu.: 1.0000             3rd Qu.: 536.404           
 Max.   :435.00   Max.   :11.0000   Max.   :11.0000             Max.   :7249.167           

#KW
kruskal.test(time~group, data=qt1)

	Kruskal-Wallis rank sum test

data:  time by group 
Kruskal-Wallis chi-squared = 15.3006, df = 2, p-value = 0.0004759

#MWW
pairwise.wilcox.test(qt1$time, qt1$group, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  qt1$time and qt1$group 

  A       B      
B 0.26586 -      
C 0.00092 0.03365

P value adjustment method: bonferroni 

# A and C, and B and C are different. 

> summary(qt1[which(qt1$group=='A'),])
       id              rq1              rq2          trans          time          group   
 Min.   :  2.00   Min.   :0.0000   Min.   : 0.0000   r-:1019   Min.   :   0.267   A:5245  
 1st Qu.: 22.00   1st Qu.:0.0000   1st Qu.: 0.0000   r+:4226   1st Qu.:  10.800   B:   0  
 Median : 49.00   Median :0.0000   Median : 0.0000             Median :  62.517   C:   0  
 Mean   : 64.38   Mean   :0.2545   Mean   : 0.3422             Mean   : 583.679           
 3rd Qu.: 93.00   3rd Qu.:1.0000   3rd Qu.: 1.0000             3rd Qu.: 555.200           
 Max.   :435.00   Max.   :1.0000   Max.   :10.0000             Max.   :7249.167           
> summary(qt1[which(qt1$group=='B'),])
       id              rq1             rq2         trans         time          group  
 Min.   :  4.00   Min.   :2.000   Min.   : 0.000   r-:116   Min.   :   0.667   A:  0  
 1st Qu.: 14.00   1st Qu.:2.000   1st Qu.: 1.000   r+:548   1st Qu.:  14.908   B:664  
 Median : 22.00   Median :2.000   Median : 2.000            Median :  89.942   C:  0  
 Mean   : 40.94   Mean   :2.271   Mean   : 1.986            Mean   : 536.295          
 3rd Qu.: 49.00   3rd Qu.:3.000   3rd Qu.: 2.000            3rd Qu.: 481.775          
 Max.   :435.00   Max.   :3.000   Max.   :11.000            Max.   :7136.483          
> summary(qt1[which(qt1$group=='C'),])
       id              rq1              rq2         trans         time          group  
 Min.   :  4.00   Min.   : 4.000   Min.   : 0.000   r-: 24   Min.   :   1.217   A:  0  
 1st Qu.: 22.00   1st Qu.: 4.000   1st Qu.: 2.000   r+:127   1st Qu.:  27.783   B:  0  
 Median : 22.00   Median : 5.000   Median : 3.000            Median : 157.850   C:151  
 Mean   : 48.72   Mean   : 5.185   Mean   : 3.364            Mean   : 497.118          
 3rd Qu.: 27.00   3rd Qu.: 6.000   3rd Qu.: 5.000            3rd Qu.: 439.225          
 Max.   :435.00   Max.   :11.000   Max.   :10.000            Max.   :5149.900          


###########################################
# Effect of Component factor on Time
###########################################

ct<- read.csv("time_component.csv", header = TRUE, sep=',')

#KW
kruskal.test(time~comp, data=ct)

	Kruskal-Wallis rank sum test

data:  time by comp 
Kruskal-Wallis chi-squared = 29.9235, df = 6, p-value = 4.065e-05

#MWW
pairwise.wilcox.test(ct$time, ct$comp, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  ct$time and ct$comp 

          bindings css    dom    html   inspector page   
css       1.0000   -      -      -      -         -      
dom       0.4360   1.0000 -      -      -         -      
html      1.0000   1.0000 1.0000 -      -         -      
inspector 1.0000   1.0000 0.5616 1.0000 -         -      
page      1.0000   0.6392 0.0065 0.0747 1.0000    -      
rendering 0.0167   0.5783 1.0000 0.9199 0.0220    7.5e-05

P value adjustment method: bonferroni

# 4 cases: rendering-bindings, rendering-inspector, rendering-page, page-dom.

> summary(ct[which(ct$comp=='rendering'),])
 trans          time                 comp     
 r-: 350   Min.   :   0.317   bindings :   0  
 r+:1361   1st Qu.:  15.150   css      :   0  
           Median : 101.317   dom      :   0  
           Mean   : 673.279   html     :   0  
           3rd Qu.: 713.700   inspector:   0  
           Max.   :7249.167   page     :   0  
                              rendering:1711  
> summary(ct[which(ct$comp=='bindings'),])
 trans         time                 comp     
 r-:241   Min.   :   0.367   bindings :1228  
 r+:987   1st Qu.:  12.896   css      :   0  
          Median :  71.592   dom      :   0  
          Mean   : 509.005   html     :   0  
          3rd Qu.: 409.783   inspector:   0  
          Max.   :7221.083   page     :   0  
                             rendering:   0  
> summary(ct[which(ct$comp=='inspector'),])
 trans          time                 comp     
 r-: 354   Min.   :   0.317   bindings :   0  
 r+:1335   1st Qu.:  13.983   css      :   0  
           Median :  57.500   dom      :   0  
           Mean   : 555.667   html     :   0  
           3rd Qu.: 670.317   inspector:1689  
           Max.   :7226.133   page     :   0  
                              rendering:   0  
> summary(ct[which(ct$comp=='page'),])
 trans         time                 comp     
 r-:309   Min.   :   0.383   bindings :   0  
 r+:970   1st Qu.:  10.425   css      :   0  
          Median :  58.000   dom      :   0  
          Mean   : 549.523   html     :   0  
          3rd Qu.: 409.467   inspector:   0  
          Max.   :7188.350   page     :1279  
                             rendering:   0  
> summary(ct[which(ct$comp=='dom'),])
 trans          time                comp     
 r-: 304   Min.   :   0.35   bindings :   0  
 r+:1037   1st Qu.:  15.67   css      :   0  
           Median :  90.72   dom      :1341  
           Mean   : 601.36   html     :   0  
           3rd Qu.: 531.73   inspector:   0  
           Max.   :7221.08   page     :   0  
                             rendering:   0 


#Component effect for transaction type - not in the paper

# Subsets of r+/r- patches 
ct_rp <- ct[ which(ct$trans=='r+'), ]
ct_rm <- ct[ which(ct$trans=='r-'), ]

#KW for r+
kruskal.test(time~comp, data=ct_rp)

	Kruskal-Wallis rank sum test

data:  time by comp 
Kruskal-Wallis chi-squared = 26.9571, df = 6, p-value = 0.0001475

#since there is sig dif in r+, let's see where


#KW for r-
kruskal.test(time~comp, data=ct_rm)

	Kruskal-Wallis rank sum test

data:  time by comp 
Kruskal-Wallis chi-squared = 8.7947, df = 6, p-value = 0.1855
 
#MWW for r+
pairwise.wilcox.test(ct_rp$time, ct_rp$comp, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  ct_rp$time and ct_rp$comp 

          bindings css    dom    html   inspector page   
css       1.0000   -      -      -      -         -      
dom       1.0000   1.0000 -      -      -         -      
html      1.0000   1.0000 1.0000 -      -         -      
inspector 1.0000   1.0000 1.0000 1.0000 -         -      
page      0.7688   0.1853 0.0025 0.0960 0.4118    -      
rendering 0.2049   1.0000 1.0000 1.0000 0.1475    7.4e-05

P value adjustment method: bonferroni 

# 2 cases for r+: rendering-page, page-dom.


###########################################
# Effect of Organization factor on Time
###########################################

ot<- read.csv("time_rorg_sorg.csv", header = TRUE, sep=',')

#KW for the whole sample
kruskal.test(time~sorg, data=ot)

	Kruskal-Wallis rank sum test

data:  time by sorg 
Kruskal-Wallis chi-squared = 272.0565, df = 5, p-value < 2.2e-16

kruskal.test(time~rorg, data=ot)

	Kruskal-Wallis rank sum test

data:  time by rorg 
Kruskal-Wallis chi-squared = 43.1515, df = 5, p-value = 3.443e-08


#MWW for the whole dataset

#submitters

> pairwise.wilcox.test(ot$time, ot$sorg, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  ot$time and ot$sorg 

  a       g       i      n      o     
g < 2e-16 -       -      -      -     
i 4.5e-07 0.0067  -      -      -     
n 0.0040  1.0000  0.1035 -      -     
o < 2e-16 < 2e-16 1.0000 0.0028 -     
r 4.6e-05 1.0000  0.1640 1.0000 0.0036

P value adjustment method: bonferroni 

# the effect of submitter factor on response time is stat sign, in particular:
# A vs G, A-I, A-N, A-O, A-R, G-I, G-O, N-O, R-O. 

#reviewers

> pairwise.wilcox.test(ot$time, ot$rorg, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  ot$time and ot$rorg 

  a       g       i       n       o      
g 1.00000 -       -       -       -      
i 0.02383 0.04957 -       -       -      
n 0.38438 0.11480 0.00593 -       -      
o 0.08642 0.26336 1.00000 0.00328 -      
r 7e-05   0.00036 1.00000 0.00014 1.00000

P value adjustment method: bonferroni 

# the effect of reviewing org factor on response time is stat sign, in particular:
# A vs I, A-O, A-R, G-I, G-R, I-N, N-O, N-R. 

# for transition type

ot_rp <- ot[ which(ot$trans=='r+'), ]
ot_rm <- ot[ which(ot$trans=='r-'), ]

#KW

kruskal.test(time~rorg, data=ot_rp)

	Kruskal-Wallis rank sum test

data:  time by rorg 
Kruskal-Wallis chi-squared = 32.0729, df = 5, p-value = 5.747e-06
# the effect of reviewing org on the acceptance is stat sign

kruskal.test(time~sorg, data=ot_rp)

	Kruskal-Wallis rank sum test

data:  time by sorg 
Kruskal-Wallis chi-squared = 155.5517, df = 5, p-value < 2.2e-16
# the effect of submitting org on acceptance is stat sign

kruskal.test(time~rorg, data=ot_rm)

	Kruskal-Wallis rank sum test

data:  time by rorg 
Kruskal-Wallis chi-squared = 7.1571, df = 5, p-value = 0.2092
# the effect of the reviewing organization on response time of a negative outcome is NOT stat significant.

kruskal.test(time~sorg, data=ot_rm)

	Kruskal-Wallis rank sum test

data:  time by sorg 
Kruskal-Wallis chi-squared = 42.8464, df = 5, p-value = 3.97e-08
# the effect of submitting org on response time of rejection is stat sign.

# we can see that it matters whose patches are being reviewed, review time depends on the organization filing patches + on the organization reviewing patches (in the case of positive outcome of the review)

#since there is sig dif for sorg, let's see where

#MWW
pairwise.wilcox.test(ot_rp$time, ot_rp$sorg, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  ot_rp$time and ot_rp$sorg 

  a       g       i       n       o      
g 1.4e-12 -       -       -       -      
i 0.00021 0.08940 -       -       -      
n 0.34011 1.00000 0.41632 -       -      
o < 2e-16 8.4e-14 1.00000 0.03865 -      
r 0.00244 1.00000 0.96164 1.00000 0.24507

P value adjustment method: bonferroni 
# diff between patches from A vs G, A-I, A-O, A-R, G-O, N-O.


# Select only Apple and Google interactions (rorg and sorg)
agt <-ot[ which(ot$rorg %in% c('a','g') & ot$sorg %in% c('a','g')), ]
dim(agt)

#KW for A-G data only
kruskal.test(time~rorg, data=agt)

	Kruskal-Wallis rank sum test

data:  time by rorg 
Kruskal-Wallis chi-squared = 14.5688, df = 1, p-value = 0.0001351

kruskal.test(time~sorg, data=agt)

	Kruskal-Wallis rank sum test

data:  time by sorg 
Kruskal-Wallis chi-squared = 90.9959, df = 1, p-value < 2.2e-16

#the diff is stat sign.

#time taken to have their patches be reviewed
 
> summary(ot[which(ot$sorg=='a'), c('time','sorg')])
      time          sorg    
 Min.   :   0.317   a:2008  
 1st Qu.:   6.696   g:   0  
 Median :  29.333   i:   0  
 Mean   : 411.006   n:   0  
 3rd Qu.: 241.404   o:   0  
 Max.   :7192.317   r:   0  
> summary(ot[which(ot$sorg=='g'), c('time','sorg')])
      time          sorg    
 Min.   :   0.267   a:   0  
 1st Qu.:  11.783   g:6010  
 Median :  61.817   i:   0  
 Mean   : 565.122   n:   0  
 3rd Qu.: 503.071   o:   0  
 Max.   :7249.167   r:   0  
> summary(ot[which(ot$sorg=='r'), c('time','sorg')])
      time         sorg   
 Min.   :   1.00   a:  0  
 1st Qu.:  17.90   g:  0  
 Median :  68.67   i:  0  
 Mean   : 494.46   n:  0  
 3rd Qu.: 473.27   o:  0  
 Max.   :7190.48   r:231  
> summary(ot[which(ot$sorg=='i'), c('time','sorg')])
      time          sorg  
 Min.   :   2.183   a: 0  
 1st Qu.:  39.775   g: 0  
 Median : 236.183   i:95  
 Mean   : 835.718   n: 0  
 3rd Qu.:1136.575   o: 0  
 Max.   :7055.033   r: 0  
> summary(ot[which(ot$sorg=='n'), c('time','sorg')])
      time          sorg   
 Min.   :   0.867   a:  0  
 1st Qu.:  17.050   g:  0  
 Median :  55.550   i:  0  
 Mean   : 510.624   n:194  
 3rd Qu.: 565.571   o:  0  
 Max.   :7136.483   r:  0  
> summary(ot[which(ot$sorg=='o'), c('time','sorg')])
      time         sorg    
 Min.   :   0.55   a:   0  
 1st Qu.:  25.74   g:   0  
 Median : 199.44   i:   0  
 Mean   : 811.09   n:   0  
 3rd Qu.: 875.20   o:1474  
 Max.   :7239.43   r:   0  

# time taken to review patches of others

summary(ot[which(ot$rorg=='a'), c('time','rorg')])
      time          rorg    
 Min.   :   0.317   a:3603  
 1st Qu.:   9.583   g:   0  
 Median :  58.317   i:   0  
 Mean   : 579.052   n:   0  
 3rd Qu.: 511.383   o:   0  
 Max.   :7239.433   r:   0  
> summary(ot[which(ot$rorg=='g'), c('time','rorg')])
      time          rorg    
 Min.   :   0.267   a:   0  
 1st Qu.:  11.983   g:5653  
 Median :  62.517   i:   0  
 Mean   : 565.584   n:   0  
 3rd Qu.: 507.817   o:   0  
 Max.   :7244.717   r:   0  
> summary(ot[which(ot$rorg=='r'), c('time','rorg')])
      time         rorg   
 Min.   :   0.55   a:  0  
 1st Qu.:  28.47   g:  0  
 Median : 125.33   i:  0  
 Mean   : 580.57   n:  0  
 3rd Qu.: 648.46   o:  0  
 Max.   :7190.48   r:300  
> summary(ot[which(ot$rorg=='i'), c('time','rorg')])
      time          rorg   
 Min.   :   0.833   a:  0  
 1st Qu.:  17.525   g:  0  
 Median : 239.117   i:120  
 Mean   : 613.470   n:  0  
 3rd Qu.: 524.775   o:  0  
 Max.   :6491.250   r:  0  
> summary(ot[which(ot$rorg=='n'), c('time','rorg')])
      time          rorg  
 Min.   :   1.533   a: 0  
 1st Qu.:   6.433   g: 0  
 Median :  19.842   i: 0  
 Mean   : 286.904   n:86  
 3rd Qu.: 388.742   o: 0  
 Max.   :4591.133   r: 0  
> summary(ot[which(ot$rorg=='o'), c('time','rorg')])
      time          rorg   
 Min.   :   1.267   a:  0  
 1st Qu.:  22.817   g:  0  
 Median :  72.175   i:  0  
 Mean   : 615.507   n:  0  
 3rd Qu.: 552.779   o:250  
 Max.   :7249.167   r:  0  


#Check now to see if the is difference in r+ / r-

agt_rp <- agt[ which(agt$trans=='r+'), ]
agt_rm <- agt[ which(agt$trans=='r-'), ]


 summary(ot[which(ot$rorg=='a'&ot$sorg=='a'), c('time','rorg','sorg')])
      time          rorg     sorg    
 Min.   :   0.317   a:1730   a:1730  
 1st Qu.:   6.271   g:   0   g:   0  
 Median :  26.492   i:   0   i:   0  
 Mean   : 398.075   n:   0   n:   0  
 3rd Qu.: 199.617   o:   0   o:   0  
 Max.   :7192.317   r:   0   r:   0  
> summary(ot[which(ot$rorg=='a'&ot$sorg=='g'), c('time','rorg','sorg')])
      time         rorg     sorg    
 Min.   :   0.35   a:1095   a:   0  
 1st Qu.:  13.24   g:   0   g:1095  
 Median :  99.88   i:   0   i:   0  
 Mean   : 696.80   n:   0   n:   0  
 3rd Qu.: 656.82   o:   0   o:   0  
 Max.   :7224.32   r:   0   r:   0  
> summary(ot[which(ot$rorg=='g'&ot$sorg=='g'), c('time','rorg','sorg')])
      time          rorg     sorg    
 Min.   :   0.267   a:   0   a:   0  
 1st Qu.:  11.250   g:4817   g:4817  
 Median :  54.783   i:   0   i:   0  
 Mean   : 532.270   n:   0   n:   0  
 3rd Qu.: 437.283   o:   0   o:   0  
 Max.   :7244.717   r:   0   r:   0  
> summary(ot[which(ot$rorg=='g'&ot$sorg=='a'), c('time','rorg','sorg')])
      time          rorg    sorg   
 Min.   :   0.417   a:  0   a:221  
 1st Qu.:   8.717   g:221   g:  0  
 Median :  46.067   i:  0   i:  0  
 Mean   : 494.121   n:  0   n:  0  
 3rd Qu.: 414.717   o:  0   o:  0  
 Max.   :7106.583   r:  0   r:  0 


# 1) difference in how apple reviews their own (26 min) vs google (100 min) patches
# 2) difference in how google reviews their own (55 min) vs apple reviews their own patches (26 min)
# 3) google treats apple patches (46 min) same as their own patches (55 min)


###########################################
# Patch Writer Experience on Time
###########################################

tsb<- read.csv("time_submitter.csv", header = TRUE, sep=',')

#splitting submitters into 4 groups
tsb$group <- as.factor(cut(tsb$patches, quantile(tsb$patches,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

#KW
kruskal.test(time~group, data=tsb)

	Kruskal-Wallis rank sum test

data:  time by group 
Kruskal-Wallis chi-squared = 111.1741, df = 3, p-value < 2.2e-16
#Result: there is stat sign

#MWW
pairwise.wilcox.test(tsb$time, tsb$group, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  tsb$time and tsb$group 

  A       B       C    
B 0.057   -       -    
C < 2e-16 6.3e-09 -    
D 4.9e-16 6.8e-08 1.000

P value adjustment method: bonferroni 

#Result: difference for response time for A/B submitters is stat sign when compared to C or D groups

#Mean and median values of the response time for groups

summary(tsb[which(tsb$group=='A'),])
summary(tsb[which(tsb$group=='B'),])
summary(tsb[which(tsb$group=='C'),])
summary(tsb[which(tsb$group=='D'),])

> summary(tsb[which(tsb$group=='A'),])
      sid             time             patches      group   
 Min.   :  1.0   Min.   :   0.267   Min.   : 1.00   A:2564  
 1st Qu.:165.0   1st Qu.:  16.471   1st Qu.: 6.00   B:   0  
 Median :310.5   Median : 102.100   Median :11.00   C:   0  
 Mean   :354.4   Mean   : 681.696   Mean   :11.94   D:   0  
 3rd Qu.:529.8   3rd Qu.: 653.671   3rd Qu.:17.00           
 Max.   :981.0   Max.   :7249.167   Max.   :26.00           
> summary(tsb[which(tsb$group=='B'),])
      sid             time             patches      group   
 Min.   :  8.0   Min.   :   0.333   Min.   :27.00   A:   0  
 1st Qu.: 69.0   1st Qu.:  12.350   1st Qu.:37.00   B:2503  
 Median :176.0   Median :  76.633   Median :44.00   C:   0  
 Mean   :207.8   Mean   : 632.744   Mean   :45.54   D:   0  
 3rd Qu.:269.0   3rd Qu.: 584.283   3rd Qu.:56.00           
 Max.   :796.0   Max.   :7239.433   Max.   :67.00           
> summary(tsb[which(tsb$group=='C'),])
      sid             time             patches      group   
 Min.   :  5.0   Min.   :   0.317   Min.   : 70.0   A:   0  
 1st Qu.: 55.0   1st Qu.:   9.004   1st Qu.: 88.0   B:   0  
 Median :114.0   Median :  42.550   Median : 98.0   C:2508  
 Mean   :165.8   Mean   : 490.819   Mean   :106.3   D:   0  
 3rd Qu.:262.0   3rd Qu.: 403.921   3rd Qu.:120.0           
 Max.   :737.0   Max.   :7097.250   Max.   :150.0           
> summary(tsb[which(tsb$group=='D'),])
      sid             time             patches      group   
 Min.   :  4.0   Min.   :   0.317   Min.   :156.0   A:   0  
 1st Qu.:  7.0   1st Qu.:   9.917   1st Qu.:185.0   B:   0  
 Median : 32.0   Median :  47.900   Median :287.0   C:   0  
 Mean   :118.4   Mean   : 470.786   Mean   :266.7   D:2437  
 3rd Qu.:111.0   3rd Qu.: 389.733   3rd Qu.:297.0           
 Max.   :435.0   Max.   :7226.133   Max.   :396.0 

#############################
# Reviewer Activity on Time
#############################

trv<- read.csv("time_reviewer.csv", header = TRUE, sep=',')

#splitting submitters into 4 groups
trv$group <- as.factor(cut(trv$patches, quantile(trv$patches,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

#KW
kruskal.test(time~group, data=trv)

	Kruskal-Wallis rank sum test

data:  time by group 
Kruskal-Wallis chi-squared = 40.5432, df = 3, p-value = 8.173e-09

#Result: there is stat sign

#MWW
pairwise.wilcox.test(trv$time, trv$group, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  trv$time and trv$group 

  A       B       C      
B 0.83390 -       -      
C 4.4e-08 0.00045 -      
D 8.9e-05 0.02944 1.00000

P value adjustment method: bonferroni 

#Result: difference for response time for A/B reviewers is stat sign when compared to C or D groups


#Mean and Medians for response time for groups

summary(trv[which(trv$group=='A'),])
summary(trv[which(trv$group=='B'),])
summary(trv[which(trv$group=='C'),])
summary(trv[which(trv$group=='D'),])

> summary(trv[which(trv$group=='A'),])
      rid             time             patches      group   
 Min.   :  2.0   Min.   :   0.383   Min.   : 31.0   A:2657  
 1st Qu.: 44.0   1st Qu.:  13.683   1st Qu.: 60.0   B:   0  
 Median : 96.0   Median :  83.700   Median :112.0   C:   0  
 Mean   :111.9   Mean   : 620.755   Mean   :104.5   D:   0  
 3rd Qu.:151.0   3rd Qu.: 629.467   3rd Qu.:139.0           
 Max.   :435.0   Max.   :7249.167   Max.   :168.0           
> summary(trv[which(trv$group=='B'),])
      rid              time             patches      group   
 Min.   :  7.00   Min.   :   0.267   Min.   :171.0   A:   0  
 1st Qu.: 37.00   1st Qu.:  10.579   1st Qu.:225.0   B:2586  
 Median : 54.00   Median :  76.375   Median :242.0   C:   0  
 Mean   : 56.51   Mean   : 634.156   Mean   :245.5   D:   0  
 3rd Qu.: 89.00   3rd Qu.: 572.862   3rd Qu.:249.0           
 Max.   :114.00   Max.   :7239.433   Max.   :355.0           
> summary(trv[which(trv$group=='C'),])
      rid             time             patches      group   
 Min.   :14.00   Min.   :   0.317   Min.   :377.0   A:   0  
 1st Qu.:27.00   1st Qu.:  10.350   1st Qu.:440.0   B:   0  
 Median :30.00   Median :  46.017   Median :634.0   C:2787  
 Mean   :45.42   Mean   : 515.713   Mean   :592.4   D:   0  
 3rd Qu.:72.00   3rd Qu.: 457.675   3rd Qu.:770.0           
 Max.   :93.00   Max.   :7224.317   Max.   :770.0           
> summary(trv[which(trv$group=='D'),])
      rid             time             patches       group   
 Min.   : 4.00   Min.   :   0.333   Min.   : 929.0   A:   0  
 1st Qu.: 4.00   1st Qu.:  11.217   1st Qu.: 929.0   B:   0  
 Median :22.00   Median :  56.892   Median :1053.0   C:   0  
 Mean   :13.56   Mean   : 496.135   Mean   : 994.9   D:1982  
 3rd Qu.:22.00   3rd Qu.: 389.308   3rd Qu.:1053.0           
 Max.   :22.00   Max.   :7226.133   Max.   :1053.0 


##########################################################
# 		== Maximizing Positivity ==
##########################################################

#####################################
# Effect of Priority on Positivity
#####################################

prp<- read.csv("positivity_priority.csv", header = TRUE, sep=',')

#KW

kruskal.test(pos~priority, data=prp)

	Kruskal-Wallis rank sum test

data:  pos by priority 
Kruskal-Wallis chi-squared = 11.7802, df = 4, p-value = 0.01906

#difference is stat.sign.

#MWW

 pairwise.wilcox.test(prp$pos, prp$priority, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  prp$pos and prp$priority 

   P1    P2    P3    P4   
P2 0.027 -     -     -    
P3 0.222 1.000 -     -    
P4 1.000 1.000 1.000 -    
P5 0.901 1.000 1.000 1.000

P value adjustment method: bonferroni 

#P1-P2 is different

#subset for priority
prp1 <- subset(prp, priority %in% c('P1','P2','P3'))

#KW for 3 levels of priority

kruskal.test(pos~priority, data=prp1)

	Kruskal-Wallis rank sum test

data:  pos by priority 
Kruskal-Wallis chi-squared = 10.5055, df = 2, p-value = 0.005233

#MWW for 3 levels of priority

 pairwise.wilcox.test(prp1$pos, prp1$priority, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  prp1$pos and prp1$priority 

   P1    P2   
P2 0.008 -    
P3 0.067 0.863

P value adjustment method: bonferroni 

#P1-P2 is stat significant

>  summary(prp1[ which(prp1$priority=='P1'),])
 priority      pos        
 P1:40    Min.   :0.0000  
 P2: 0    1st Qu.:0.7500  
 P3: 0    Median :1.0000  
 P4: 0    Mean   :0.8467  
 P5: 0    3rd Qu.:1.0000  
          Max.   :1.0000  
> 
> summary(prp1[ which(prp1$priority=='P2'),])
 priority      pos        
 P1: 0    Min.   :0.5806  
 P2:51    1st Qu.:0.7275  
 P3: 0    Median :0.8311  
 P4: 0    Mean   :0.8044  
 P5: 0    3rd Qu.:0.8894  
          Max.   :0.9773  
> summary(prp1[ which(prp1$priority=='P3'),])
 priority      pos        
 P1: 0    Min.   :0.0000  
 P2: 0    1st Qu.:0.5278  
 P3:19    Median :0.7857  
 P4: 0    Mean   :0.6707  
 P5: 0    3rd Qu.:0.9286  
          Max.   :1.0000 


#####################################
# Effect of Component on Positivity
#####################################

cp<- read.csv("positivity_component.csv", header = TRUE, sep=',')

#KW
kruskal.test(pos~comp, data=cp)

	Kruskal-Wallis rank sum test

data:  pos by comp 
Kruskal-Wallis chi-squared = 5.4067, df = 6, p-value = 0.4928
# no difference


# Median Positivity per Component

 summary(cp[ which(cp$comp=='bindings'),])
        comp         pos        
 bindings :27   Min.   :0.0000  
 css      : 0   1st Qu.:0.6705  
 dom      : 0   Median :0.8000  
 html     : 0   Mean   :0.7444  
 inspector: 0   3rd Qu.:0.8814  
 page     : 0   Max.   :1.0000  
 rendering: 0                   

 summary(cp[ which(cp$comp=='css'),])
        comp         pos        
 bindings : 0   Min.   :0.3333  
 css      :27   1st Qu.:0.6937  
 dom      : 0   Median :0.8421  
 html     : 0   Mean   :0.7707  
 inspector: 0   3rd Qu.:0.9005  
 page     : 0   Max.   :1.0000  
 rendering: 0                   

 summary(cp[ which(cp$comp=='dom'),])
        comp         pos        
 bindings : 0   Min.   :0.0000  
 css      : 0   1st Qu.:0.6667  
 dom      :29   Median :0.7907  
 html     : 0   Mean   :0.7362  
 inspector: 0   3rd Qu.:0.8571  
 page     : 0   Max.   :1.0000  
 rendering: 0                   

 summary(cp[ which(cp$comp=='html'),])
        comp         pos        
 bindings : 0   Min.   :0.3333  
 css      : 0   1st Qu.:0.6812  
 dom      : 0   Median :0.7794  
 html     :27   Mean   :0.7590  
 inspector: 0   3rd Qu.:0.8681  
 page     : 0   Max.   :1.0000  
 rendering: 0                   

summary(cp[ which(cp$comp=='inspector'),])
        comp         pos        
 bindings : 0   Min.   :0.6667  
 css      : 0   1st Qu.:0.8016  
 dom      : 0   Median :0.8451  
 html     : 0   Mean   :0.8427  
 inspector:12   3rd Qu.:0.9032  
 page     : 0   Max.   :1.0000  
 rendering: 0                   

summary(cp[ which(cp$comp=='page'),])
        comp         pos        
 bindings : 0   Min.   :0.2000  
 css      : 0   1st Qu.:0.6522  
 dom      : 0   Median :0.7500  
 html     : 0   Mean   :0.7338  
 inspector: 0   3rd Qu.:0.8571  
 page     :33   Max.   :1.0000  
 rendering: 0                   

summary(cp[ which(cp$comp=='rendering'),])
        comp         pos        
 bindings : 0   Min.   :0.0000  
 css      : 0   1st Qu.:0.7290  
 dom      : 0   Median :0.8056  
 html     : 0   Mean   :0.7739  
 inspector: 0   3rd Qu.:0.8856  
 page     : 0   Max.   :1.0000  
 rendering:31 


#####################################
# Effect of Organization on Positivity
#####################################

op<- read.csv("positivity_org.csv", header = TRUE, sep=',')

pdf("boxplots_positivity_org.pdf")
boxplot(pos~rorg*sorg, data=op, xlab="Organization", ylab="Positivity")
dev.off()

#KW

> kruskal.test(pos~rorg, data=op)

	Kruskal-Wallis rank sum test

data:  pos by rorg 
Kruskal-Wallis chi-squared = 3.3198, df = 5, p-value = 0.6508

> kruskal.test(pos~sorg, data=op)

	Kruskal-Wallis rank sum test

data:  pos by sorg 
Kruskal-Wallis chi-squared = 24.036, df = 5, p-value = 0.0002137

# there is a stat.sig in the outcome of the review (positivity) and organization submitting patches for review

#MWW for pos~sorg interaction

 pairwise.wilcox.test(op$pos, op$sorg, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  op$pos and op$sorg 

  a       g     i     n     o    
g 0.044   -     -     -     -    
i 1.000   0.482 -     -     -    
n 1.000   1.000 1.000 -     -    
o 4.7e-05 0.102 0.178 0.690 -    
r 1.000   1.000 1.000 1.000 1.000

P value adjustment method: bonferroni 
# outcome of the review is stat sign comparing A (.92) to G (.82) patches, A-O (0.7). 

> summary(op[which(op$sorg=='a'), c('pos','rorg','sorg')])
      pos         rorg   sorg  
 Min.   :0.4000   a:22   a:47  
 1st Qu.:0.8000   g:16   g: 0  
 Median :0.9167   i: 0   i: 0  
 Mean   :0.8770   n: 1   n: 0  
 3rd Qu.:1.0000   o: 5   o: 0  
 Max.   :1.0000   r: 3   r: 0  
> summary(op[which(op$sorg=='g'), c('pos','rorg','sorg')])
      pos         rorg   sorg  
 Min.   :0.4000   a:20   a: 0  
 1st Qu.:0.7108   g:19   g:47  
 Median :0.8151   i: 1   i: 0  
 Mean   :0.7957   n: 1   n: 0  
 3rd Qu.:0.9111   o: 3   o: 0  
 Max.   :1.0000   r: 3   r: 0  
> summary(op[which(op$sorg=='i'), c('pos','rorg','sorg')])
      pos         rorg  sorg  
 Min.   :0.0000   a:6   a: 0  
 1st Qu.:0.7529   g:4   g: 0  
 Median :1.0000   i:1   i:12  
 Mean   :0.8337   n:0   n: 0  
 3rd Qu.:1.0000   o:1   o: 0  
 Max.   :1.0000   r:0   r: 0  
> summary(op[which(op$sorg=='n'), c('pos','rorg','sorg')])
      pos         rorg   sorg  
 Min.   :0.0000   a:10   a: 0  
 1st Qu.:0.6667   g: 9   g: 0  
 Median :0.8377   i: 1   i: 0  
 Mean   :0.7732   n: 1   n:28  
 3rd Qu.:1.0000   o: 4   o: 0  
 Max.   :1.0000   r: 3   r: 0  
> summary(op[which(op$sorg=='r'), c('pos','rorg','sorg')])
      pos         rorg   sorg  
 Min.   :0.0000   a:10   a: 0  
 1st Qu.:0.5536   g:10   g: 0  
 Median :0.8348   i: 1   i: 0  
 Mean   :0.7238   n: 0   n: 0  
 3rd Qu.:1.0000   o: 4   o: 0  
 Max.   :1.0000   r: 3   r:28  
> summary(op[which(op$sorg=='o'), c('pos','rorg','sorg')])
      pos         rorg   sorg  
 Min.   :0.1250   a:21   a: 0  
 1st Qu.:0.5580   g:17   g: 0  
 Median :0.7033   i: 1   i: 0  
 Mean   :0.6895   n: 1   n: 0  
 3rd Qu.:0.8000   o: 5   o:48  
 Max.   :1.0000   r: 3   r: 0


# Positivity for Apple-Google-Rest(X)
op3<- read.csv("positivity_org_agx.csv", header = TRUE, sep=',')

#KW

> kruskal.test(pos~sorg, data=op3)

	Kruskal-Wallis rank sum test

data:  pos by sorg 
Kruskal-Wallis chi-squared = 10.6721, df = 2, p-value = 0.004815
# there is stat sig.

> kruskal.test(pos~rorg, data=op3)

	Kruskal-Wallis rank sum test

data:  pos by rorg 
Kruskal-Wallis chi-squared = 0.3227, df = 2, p-value = 0.851


#MWW
pairwise.wilcox.test(op3$pos, op3$sorg, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  op3$pos and op3$sorg 

  a      g     
g 0.0088 -     
x 0.0107 1.0000

P value adjustment method: bonferroni 
# outcome of the review for patches from Apple is stat.sig in comparison with either patches from G or X.


#Positivity updated with filter: less than 10 patches are removed.
agx_new<- read.csv("positivity_org_updated_count.csv", header = TRUE, sep=',')

agx_new$f1f2 <- interaction(agx_new$rorg, agx_new$sorg)

agx_new$f1f2 <- ordered(agx_new$f1f2, levels=c("a.a", "a.g", "a.x", "g.g", "g.a","g.x","x.x","x.a", "x.g"))

#KW for the interaction of two factors 
kruskal.test(pos~f1f2, data=agx_new)

	Kruskal-Wallis rank sum test

data:  pos by f1f2 
Kruskal-Wallis chi-squared = 38.6041, df = 8, p-value = 5.82e-06

#MWW
pairwise.wilcox.test(agx_new$pos, agx_new$f1f2, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  agx_new$pos and agx_new$f1f2 

    a.a     a.g     a.x     g.g     g.a     g.x     x.x     x.a    
a.g 0.00152 -       -       -       -       -       -       -      
a.x 0.00833 1.00000 -       -       -       -       -       -      
g.g 0.10379 1.00000 1.00000 -       -       -       -       -      
g.a 0.51843 1.00000 1.00000 1.00000 -       -       -       -      
g.x 0.00016 1.00000 1.00000 0.05748 1.00000 -       -       -      
x.x 0.00074 1.00000 1.00000 0.74564 1.00000 1.00000 -       -      
x.a 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 -      
x.g 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000

P value adjustment method: bonferroni

# Mean and Median values for time vs. org

# R+

summary(ot_rp[ which(ot_rp$sorg=='a' & ot_rp$rorg=='a'),])
 trans          time          rorg     sorg    
 r-:   0   Min.   :   0.317   a:1604   a:1604  
 r+:1604   1st Qu.:   5.850   g:   0   g:   0  
           Median :  25.283   i:   0   i:   0  
           Mean   : 391.505   n:   0   n:   0  
           3rd Qu.: 195.733   o:   0   o:   0  
           Max.   :7192.317   r:   0   r:   0  

summary(ot_rp[ which(ot_rp$sorg=='g' & ot_rp$rorg=='a'),])
 trans         time         rorg    sorg   
 r-:  0   Min.   :   0.35   a:844   a:  0  
 r+:844   1st Qu.:  11.07   g:  0   g:844  
          Median :  72.68   i:  0   i:  0  
          Mean   : 617.23   n:  0   n:  0  
          3rd Qu.: 560.86   o:  0   o:  0  
          Max.   :7224.32   r:  0   r:  0  

summary(ot_rp[ which(ot_rp$sorg=='g' & ot_rp$rorg=='g'),])
 trans          time          rorg     sorg    
 r-:   0   Min.   :   0.267   a:   0   a:   0  
 r+:3896   1st Qu.:   9.383   g:3896   g:3896  
           Median :  45.075   i:   0   i:   0  
           Mean   : 483.916   n:   0   n:   0  
           3rd Qu.: 376.333   o:   0   o:   0  
           Max.   :7244.717   r:   0   r:   0
  
summary(ot_rp[ which(ot_rp$sorg=='a' & ot_rp$rorg=='g'),])
 trans         time          rorg    sorg   
 r-:  0   Min.   :   0.417   a:  0   a:180  
 r+:180   1st Qu.:   7.871   g:180   g:  0  
          Median :  41.658   i:  0   i:  0  
          Mean   : 482.963   n:  0   n:  0  
          3rd Qu.: 361.563   o:  0   o:  0  
          Max.   :7106.583   r:  0   r:  0

# R-

summary(ot_rm[ which(ot_rm$sorg=='a' & ot_rm$rorg=='a'),]) 
 trans         time         rorg    sorg   
 r-:126   Min.   :   0.45   a:126   a:126  
 r+:  0   1st Qu.:  14.53   g:  0   g:  0  
          Median :  59.84   i:  0   i:  0  
          Mean   : 481.71   n:  0   n:  0  
          3rd Qu.: 393.98   o:  0   o:  0  
          Max.   :4992.70   r:  0   r:  0  
> 
> summary(ot_rm[ which(ot_rm$sorg=='g' & ot_rm$rorg=='a'),])
 trans         time          rorg    sorg   
 r-:251   Min.   :   0.683   a:251   a:  0  
 r+:  0   1st Qu.:  30.183   g:  0   g:251  
          Median : 283.367   i:  0   i:  0  
          Mean   : 964.359   n:  0   n:  0  
          3rd Qu.:1028.083   o:  0   o:  0  
          Max.   :7127.833   r:  0   r:  0  
> 
> summary(ot_rm[ which(ot_rm$sorg=='g' & ot_rm$rorg=='g'),])
 trans         time          rorg    sorg   
 r-:921   Min.   :   1.183   a:  0   a:  0  
 r+:  0   1st Qu.:  22.833   g:921   g:921  
          Median : 101.533   i:  0   i:  0  
          Mean   : 736.815   n:  0   n:  0  
          3rd Qu.: 758.883   o:  0   o:  0  
          Max.   :7226.133   r:  0   r:  0  
>   
> summary(ot_rm[ which(ot_rm$sorg=='a' & ot_rm$rorg=='g'),])
 trans        time          rorg   sorg  
 r-:41   Min.   :   0.817   a: 0   a:41  
 r+: 0   1st Qu.:  23.267   g:41   g: 0  
         Median :  80.083   i: 0   i: 0  
         Mean   : 543.105   n: 0   n: 0  
         3rd Qu.: 490.617   o: 0   o: 0  
         Max.   :3328.833   r: 0   r: 0  

#############################
# Review Queue on Positivity
#############################

pq<- read.csv("positivity_rqueue.csv", header = TRUE, sep=';')

#splitting rq into 3 groups [0,1,3,11]
pq$group <- as.factor(cut(pq$rq, c(0,1,3,11), include.lowest=TRUE, labels=LETTERS[1:3]))

#KW
kruskal.test(pos~group, data=pq)

	Kruskal-Wallis rank sum test

data:  pos by group 
Kruskal-Wallis chi-squared = 15.7565, df = 2, p-value = 0.0003789
#there is stat diff

#MWW
> pairwise.wilcox.test(pq$pos, pq$group, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  pq$pos and pq$group 

  A       B      
B 0.71968 -      
C 0.00018 0.03903

P value adjustment method: bonferroni 

# diff between A and C (the median value of queue length being 0 and 5 patches and positivity being 0.84 and 1, p< 0.01) and B and C (with medians of review queue being 2 and 5 patches and positivity being 0.88 and 1.0 respectively). 


summary(pq)
       id              rq              pos         group 
 Min.   :  2.0   Min.   : 0.000   Min.   :0.0000   A:98  
 1st Qu.: 22.0   1st Qu.: 0.000   1st Qu.:0.7500   B:53  
 Median : 57.0   Median : 2.000   Median :0.8829   C:48  
 Mean   : 84.6   Mean   : 2.312   Mean   :0.8362         
 3rd Qu.: 95.0   3rd Qu.: 3.000   3rd Qu.:1.0000         
 Max.   :435.0   Max.   :11.000   Max.   :1.0000         

> summary(pq[which(pq$group=='A'),])
       id               rq              pos         group 
 Min.   :  2.00   Min.   :0.0000   Min.   :0.5000   A:98  
 1st Qu.: 27.00   1st Qu.:0.0000   1st Qu.:0.7440   B: 0  
 Median : 61.00   Median :0.0000   Median :0.8446   C: 0  
 Mean   : 84.49   Mean   :0.4796   Mean   :0.8284         
 3rd Qu.:104.00   3rd Qu.:1.0000   3rd Qu.:0.9371         
 Max.   :435.00   Max.   :1.0000   Max.   :1.0000         
> summary(pq[which(pq$group=='B'),])
       id               rq             pos         group 
 Min.   :  4.00   Min.   :2.000   Min.   :0.0000   A: 0  
 1st Qu.: 22.00   1st Qu.:2.000   1st Qu.:0.7500   B:53  
 Median : 57.00   Median :2.000   Median :0.8750   C: 0  
 Mean   : 73.66   Mean   :2.434   Mean   :0.8212         
 3rd Qu.: 93.00   3rd Qu.:3.000   3rd Qu.:1.0000         
 Max.   :435.00   Max.   :3.000   Max.   :1.0000         
> summary(pq[which(pq$group=='C'),])
       id              rq              pos         group 
 Min.   :  4.0   Min.   : 4.000   Min.   :0.0000   A: 0  
 1st Qu.: 22.0   1st Qu.: 4.000   1st Qu.:0.8571   B: 0  
 Median : 55.5   Median : 5.000   Median :1.0000   C:48  
 Mean   : 96.9   Mean   : 5.917   Mean   :0.8688         
 3rd Qu.: 61.0   3rd Qu.: 7.000   3rd Qu.:1.0000         
 Max.   :435.0   Max.   :11.000   Max.   :1.0000 

###########################################
# Effect of Reviewer Activity on Positivity
###########################################

pp<- read.csv("positivity_reviewer.csv", header = TRUE, sep=',')

#KW
kruskal.test(pos~patches, data=pp)

	Kruskal-Wallis rank sum test

data:  pos by patches 
Kruskal-Wallis chi-squared = 42.9179, df = 46, p-value = 0.6021
# no correlation of number of patches on review outcome

# split reviewers into 4 groups according to their review activity (# of patches)
pp$group <- as.factor(cut(pp$patches, quantile(pp$patches,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

#KW for groups
kruskal.test(pos~group, data=pp)

	Kruskal-Wallis rank sum test

data:  pos by group 
Kruskal-Wallis chi-squared = 1.5062, df = 3, p-value = 0.6808

# Result: there is NO stat sign diff between groups

summary(pp[which(pp$group=='A'),])
summary(pp[which(pp$group=='B'),])
summary(pp[which(pp$group=='C'),])
summary(pp[which(pp$group=='D'),])

> summary(pp[which(pp$group=='A'),])
    patches           pos         group 
 Min.   :31.00   Min.   :0.5806   A:13  
 1st Qu.:36.00   1st Qu.:0.7561   B: 0  
 Median :39.00   Median :0.8293   C: 0  
 Mean   :39.38   Mean   :0.8208   D: 0  
 3rd Qu.:44.00   3rd Qu.:0.9189         
 Max.   :48.00   Max.   :0.9787         
> summary(pp[which(pp$group=='B'),])
    patches            pos         group 
 Min.   : 55.00   Min.   :0.6024   A: 0  
 1st Qu.: 69.00   1st Qu.:0.7857   B:13  
 Median : 81.00   Median :0.8372   C: 0  
 Mean   : 84.92   Mean   :0.8092   D: 0  
 3rd Qu.:106.00   3rd Qu.:0.8493         
 Max.   :129.00   Max.   :0.9130         
> summary(pp[which(pp$group=='C'),])
    patches           pos         group 
 Min.   :133.0   Min.   :0.5988   A: 0  
 1st Qu.:142.0   1st Qu.:0.7121   B: 0  
 Median :167.5   Median :0.7515   C:12  
 Mean   :168.8   Mean   :0.7759   D: 0  
 3rd Qu.:180.0   3rd Qu.:0.8472         
 Max.   :228.0   Max.   :0.9779         
> summary(pp[which(pp$group=='D'),])
    patches            pos         group 
 Min.   : 235.0   Min.   :0.6114   A: 0  
 1st Qu.: 249.0   1st Qu.:0.7223   B: 0  
 Median : 377.0   Median :0.8255   C: 0  
 Mean   : 490.1   Mean   :0.8026   D:13  
 3rd Qu.: 634.0   3rd Qu.:0.8843         
 Max.   :1053.0   Max.   :0.9046 


###########################################
# Patch Writer Experience on Positivity
###########################################

psb<- read.csv("positivity_submitter.csv", header = TRUE, sep=',')

#splitting submitters into 4 groups
psb$group <- as.factor(cut(psb$patches, quantile(psb$patches,(0:4)/4), include.lowest=TRUE, labels=LETTERS[1:4]))

#KW 
kruskal.test(pos~group, data=psb)

	Kruskal-Wallis rank sum test

data:  pos by group 
Kruskal-Wallis chi-squared = 17.9252, df = 3, p-value = 0.0004558
# Result: there is stat sign diff somewhere

 
#MWW
pairwise.wilcox.test(psb$pos, psb$group, p.adj="bonferroni", exact=F)

	Pairwise comparisons using Wilcoxon rank sum test 

data:  psb$pos and psb$group 

  A      B      C     
B 0.0758 -      -     
C 0.0118 1.0000 -     
D 0.0025 0.0181 0.1024

P value adjustment method: bonferroni 

#Result: the difference between positivity for group A-C, A-D, B-D. First patch writers (A group) have positive outcome (median pos = 1.0). Less active developers (B group) are less likely to get a positive review, while active developers are more likely to get their patches accepted. 

#Mean and Medians for groups

summary(psb[which(psb$group=='A'),])
summary(psb[which(psb$group=='B'),])
summary(psb[which(psb$group=='C'),])
summary(psb[which(psb$group=='D'),])

> summary(psb[which(psb$group=='A'),])
     subid          patches           pos         group  
 Min.   :  1.0   Min.   :1.000   Min.   :0.0000   A:136  
 1st Qu.:378.2   1st Qu.:1.000   1st Qu.:0.5000   B:  0  
 Median :526.5   Median :1.000   Median :1.0000   C:  0  
 Mean   :527.9   Mean   :1.419   Mean   :0.7132   D:  0  
 3rd Qu.:734.2   3rd Qu.:2.000   3rd Qu.:1.0000          
 Max.   :979.0   Max.   :2.000   Max.   :1.0000          
> summary(psb[which(psb$group=='B'),])
     subid          patches           pos         group  
 Min.   : 13.0   Min.   :3.000   Min.   :0.0000   A:  0  
 1st Qu.:292.0   1st Qu.:3.000   1st Qu.:0.5000   B:125  
 Median :440.0   Median :4.000   Median :0.6667   C:  0  
 Mean   :477.7   Mean   :4.168   Mean   :0.6985   D:  0  
 3rd Qu.:685.0   3rd Qu.:5.000   3rd Qu.:0.8333          
 Max.   :981.0   Max.   :6.000   Max.   :1.0000          
> summary(psb[which(psb$group=='C'),])
     subid          patches           pos         group  
 Min.   :  2.0   Min.   : 7.00   Min.   :0.1875   A:  0  
 1st Qu.:158.0   1st Qu.: 8.00   1st Qu.:0.6000   B:  0  
 Median :301.0   Median :10.00   Median :0.7333   C:113  
 Mean   :346.9   Mean   :11.04   Mean   :0.7278   D:  0  
 3rd Qu.:516.0   3rd Qu.:14.00   3rd Qu.:0.8750          
 Max.   :897.0   Max.   :17.00   Max.   :1.0000          
> summary(psb[which(psb$group=='D'),])
     subid          patches            pos         group  
 Min.   :  4.0   Min.   : 18.00   Min.   :0.3684   A:  0  
 1st Qu.: 67.5   1st Qu.: 27.00   1st Qu.:0.6858   B:  0  
 Median :160.0   Median : 43.00   Median :0.8148   C:  0  
 Mean   :197.9   Mean   : 65.98   Mean   :0.7913   D:122  
 3rd Qu.:276.5   3rd Qu.: 74.75   3rd Qu.:0.9038          
 Max.   :796.0   Max.   :396.00   Max.   :1.0000 



#################################################################
#		 == Multiple Linear Regression ==
#################################################################

# df1 - response variable is time

df1 <- read.csv("export.csv", header = TRUE, sep=',')

res1 = lm(time~size + priority + comp + queue + S_Apple + S_Google + S_RIM + S_Intel + S_Igalia + R_Apple + R_Google + R_RIM + R_Igalia + R_Intel + s_exp + r_exp, data=df1)

summary(res1)

Call:
lm(formula = time ~ size + priority + comp + queue + S_Apple + 
    S_Google + S_RIM + S_Intel + S_Igalia + R_Apple + R_Google + 
    R_RIM + R_Igalia + R_Intel + s_exp + r_exp, data = df1)

Residuals:
    Min      1Q  Median      3Q     Max 
-1099.3  -552.0  -428.2   -61.9  6736.4 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  7.620e+02  1.382e+02   5.514  3.6e-08 ***
size        -6.661e-03  1.908e-02  -0.349 0.727066    
priority     2.519e+01  5.443e+01   0.463 0.643517    
comp        -4.172e+00  9.050e+00  -0.461 0.644816    
queue       -1.337e+00  1.210e+01  -0.110 0.912021    
S_Apple     -4.229e+02  4.497e+01  -9.404  < 2e-16 ***
S_Google    -2.242e+02  3.931e+01  -5.704  1.2e-08 ***
S_RIM       -2.877e+02  9.013e+01  -3.192 0.001418 ** 
S_Intel     -2.754e+02  9.256e+01  -2.975 0.002935 ** 
S_Igalia     1.919e+02  1.573e+02   1.220 0.222336    
R_Apple      1.047e+02  8.517e+01   1.229 0.219058    
R_Google     3.889e+01  8.611e+01   0.452 0.651570    
R_RIM       -2.646e+01  1.036e+02  -0.255 0.798428    
R_Igalia    -2.253e+02  1.594e+02  -1.413 0.157561    
R_Intel     -4.139e+02  1.529e+02  -2.706 0.006815 ** 
s_exp       -3.790e-01  1.001e-01  -3.788 0.000153 ***
r_exp       -3.279e-02  3.465e-02  -0.947 0.343880    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 1201 on 9995 degrees of freedom
Multiple R-squared: 0.01435,	Adjusted R-squared: 0.01277 
F-statistic: 9.096 on 16 and 9995 DF,  p-value: < 2.2e-16 


# df2 - response variable is outcome (positivity)

df2 <- read.csv("export_outcome_dummy.csv", header = TRUE, sep=',')

res2 = lm(outcome~size + priority + comp + queue + S_Apple + S_Google + S_RIM + S_Intel + S_Igalia + R_Apple + R_Google + R_RIM + R_Igalia + R_Intel + s_exp + r_exp, data=df2)
> summary(res2)

Call:
lm(formula = outcome ~ size + priority + comp + queue + S_Apple + 
    S_Google + S_RIM + S_Intel + S_Igalia + R_Apple + R_Google + 
    R_RIM + R_Igalia + R_Intel + s_exp + r_exp, data = df2)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.0929  0.1086  0.3428  0.4506  1.1365 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.235e-01  8.968e-02   4.722 2.36e-06 ***
size        -7.598e-06  1.238e-05  -0.614 0.539542    
priority    -5.784e-02  3.532e-02  -1.638 0.101554    
comp        -3.309e-02  5.873e-03  -5.634 1.81e-08 ***
queue        1.562e-02  7.852e-03   1.989 0.046726 *  
S_Apple      4.373e-01  2.918e-02  14.986  < 2e-16 ***
S_Google     2.281e-01  2.551e-02   8.941  < 2e-16 ***
S_RIM        1.970e-01  5.849e-02   3.367 0.000761 ***
S_Intel      1.524e-01  6.006e-02   2.537 0.011202 *  
S_Igalia     2.519e-01  1.021e-01   2.468 0.013596 *  
R_Apple      4.124e-02  5.527e-02   0.746 0.455620    
R_Google     1.866e-02  5.588e-02   0.334 0.738387    
R_RIM       -1.025e-01  6.725e-02  -1.525 0.127395    
R_Igalia     7.708e-03  1.035e-01   0.075 0.940614    
R_Intel      4.338e-01  9.925e-02   4.370 1.25e-05 ***
s_exp        6.339e-04  6.494e-05   9.761  < 2e-16 ***
r_exp        1.925e-05  2.248e-05   0.856 0.391891    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.7796 on 9995 degrees of freedom
Multiple R-squared: 0.0512,	Adjusted R-squared: 0.04968 
F-statistic: 33.71 on 16 and 9995 DF,  p-value: < 2.2e-16

