---
title: "Psychometrics Gatekeeper Prelim Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
################
Data cleaning
###############
```{r}
library(foreign)
library(nnet)
library(ggplot2)
library(prettyR)
library(nlme)
library(prettyR)
library(descr)
library(Amelia)
library(mitools)
library(BaylorEdPsych)
library(openxlsx)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(DescTools)
library(MissMech)
library(robustlmm)
library(jtools)
library(lmtest)
library(lmerTest)
library(MuMIn)
library(HLMdiag)
library(Hmisc)
library(stargazer)
library(paran)
library(caret)

setwd("P:/Evaluation/TN Lives Count_Writing/3_Target1_SUICClinicalTrainingComparison/3_Data & Analyses")
datPre = read.csv("Pre.csv", header = FALSE, row.names = NULL)

datPre = t(datPre)
write.csv(datPre, "datPre.csv", row.names = FALSE)
datPre = read.csv("datPre.csv", header = TRUE)
head(datPre)
dim(datPre)

datPre = datPre[,c(1, 3, 7:18, 21:35, 38:45, 49:72, 78:80, 83, 85, 94)]
datPre = data.frame(datPre)
head(datPre)

colnames(datPre) = c("ID", "Treatment", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB","Age", "Gender", "Eth", "Race", "Edu", "Clinical_Staff")
head(datPre)
### Get rif of first row once you figure out which variables to keep
datPre = datPre[-1,]
datPre = data.frame(datPre)
head(datPre)

#Only retain clincial staff 1
datPre = subset(datPre, Clinical_Staff == 1)
head(datPre)
dim(datPre)

setwd("P:/Evaluation/TN Lives Count_Writing/3_Target1_SUICClinicalTrainingComparison/3_Data & Analyses")
datPost = read.csv("Post.csv", header = FALSE, row.names = NULL)

datPost = t(datPost)
write.csv(datPost, "datPost.csv", row.names = FALSE)
datPost = read.csv("datPost.csv", header = TRUE)
head(datPost)

datPost = datPost[,c(3, 5, 15:26, 29:43, 46:53, 57:80)]

datPost = data.frame(datPost)

colnames(datPost) = c("ID", "Treatment", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB")
head(datPost)

# making treatment null, because it does not change and it will make so it doesn't repeat later 
datPost$ID = as.factor(datPost$ID)
datPost = datPost[-1,]
datPost$Treatment = NULL
datPost = data.frame(datPost)

dim(datPre)
dim(datPost)


write.csv(datPre, "datPre.csv", row.names = FALSE)
datPre = read.csv("datPre.csv", header = TRUE)

write.csv(datPost, "datPost.csv", row.names = FALSE)
datPost = read.csv("datPost.csv", header = TRUE)

#Should not have ID one, because they are the wrong code and not in datPre
datPrePost = merge(datPre, datPost, by = "ID",  all.x = TRUE, sort = TRUE)



dat3month = read.csv("3month.csv", header  = TRUE)
head(dat3month)
dat3month = dat3month[c(7, 11:22, 23:69)]
dim(datPost)
head(datPost)

# Now rename everything 
colnames(dat3month) = c("ID", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB")


datPrePost3month = merge(datPrePost, dat3month, by = "ID", all.x = TRUE, sort = TRUE)

head(datPrePost3month)


### Now make long format
### These variables are not included: 							

datPrePost3month = reshape(datPrePost3month, varying  = list(c("Sec1Qa.x", "Sec1Qa.y", "Sec1Qa"), c("Sec1Qb.x", "Sec1Qb.y", "Sec1Qb"), c("Sec1Qc.x", "Sec1Qc.y", "Sec1Qc"), c("Sec1Qd.x", "Sec1Qd.y", "Sec1Qd"), c("Sec1Qe.x", "Sec1Qe.y", "Sec1Qe"), c("Sec1Qf.x", "Sec1Qf.y", "Sec1Qf"), c("Sec1Qg.x", "Sec1Qg.y", "Sec1Qg"), c("Sec1Qh.x", "Sec1Qh.y", "Sec1Qh"), c("Sec1Qi.x", "Sec1Qi.y", "Sec1Qi"), c("Sec1Qj.x", "Sec1Qj.y", "Sec1Qj"), c("Sec1Qk.x", "Sec1Qk.y", "Sec1Qk"), c("Sec1Ql.x", "Sec1Ql.y", "Sec1Ql"), c("Sec2Qa.x", "Sec2Qa.y", "Sec2Qa"), c("Sec2Qb.x", "Sec2Qb.y", "Sec2Qb"), c("Sec2Qc.x", "Sec2Qc.y", "Sec2Qc"), c("Sec2Qd.x", "Sec2Qd.y", "Sec2Qd"), c("Sec2Qe.x", "Sec2Qe.y", "Sec2Qe"), c("Sec2Qf.x", "Sec2Qf.y", "Sec2Qf"), c("Sec2Qg.x", "Sec2Qg.y", "Sec2Qg"), c("Sec2Qh.x", "Sec2Qh.y", "Sec2Qh"), c("Sec2Qi.x", "Sec2Qi.y", "Sec2Qi"), c("Sec2Qj.x", "Sec2Qj.y", "Sec2Qj"), c("Sec2Qk.x", "Sec2Qk.y", "Sec2Qk"), c("Sec2Ql.x", "Sec2Ql.y", "Sec2Ql"), c("Sec2Qm.x", "Sec2Qm.y", "Sec2Qm"), c("Sec2Qn.x", "Sec2Qn.y", "Sec2Qn"), c("Sec2Qo.x", "Sec2Qo.y", "Sec2Qo"), c("Sec3Qa.x", "Sec3Qa.y","Sec3Qa"), c("Sec3Qb.x", "Sec3Qb.y", "Sec3Qb"), c("Sec3Qc.x", "Sec3Qc.y", "Sec3Qc"), c("Sec3Qd.x", "Sec3Qd.y", "Sec3Qd"), c("Sec3Qe.x", "Sec3Qe.y", "Sec3Qe"), c("Sec3Qf.x", "Sec3Qf.y", "Sec3Qf"), c("Sec3Qg.x", "Sec3Qg.y", "Sec3Qg"), c("Sec3Qh.x", "Sec3Qh.y", "Sec3Qh"), c("Sec4QaA.x", "Sec4QaA.y", "Sec4QaA"), c("Sec4QaB.x", "Sec4QaB.y", "Sec4QaB"), c("Sec4QbA.x", "Sec4QbA.y", "Sec4QbA"), c("Sec4QbB.x", "Sec4QbB.y", "Sec4QbB"), c("Sec4QcA.x", "Sec4QcA.y", "Sec4QcA"), c("Sec4QcB.x", "Sec4QcB.y", "Sec4QcB"), c("Sec4QdA.x", "Sec4QdA.y", "Sec4QdA"), c("Sec4QdB.x", "Sec4QdB.y", "Sec4QdB"), c("Sec4QeA.x", "Sec4QeA.y", "Sec4QeA"), c("Sec4QeB.x", "Sec4QeB.y", "Sec4QeB"), c("Sec4QfA.x", "Sec4QfA.y", "Sec4QfA"), c("Sec4QfB.x", "Sec4QfB.y", "Sec4QfB"), c("Sec4QgA.x", "Sec4QgA.y", "Sec4QgA"), c("Sec4QgB.x", "Sec4QgB.y", "Sec4QgB"), c("Sec4QhA.x", "Sec4QhA.y", "Sec4QhA"), c("Sec4QhB.x", "Sec4QhB.y", "Sec4QhB"), c("Sec4QiA.x", "Sec4QiA.y", "Sec4QiA"), c("Sec4QiB.x", "Sec4QiB.y", "Sec4QiB"), c("Sec4QjA.x", "Sec4QjA.y", "Sec4QjA"), c("Sec4QjB.x", "Sec4QjB.y", "Sec4QjB"), c("Sec4QkA.x", "Sec4QkA.y", "Sec4QkA"), c("Sec4QkB.x", "Sec4QkB.y", "Sec4QkB"), c("Sec4QlA.x", "Sec4QlA.y", "Sec4QlA"), c("Sec4QlB.x", "Sec4QlB.y", "Sec4QlB")), direction = "long", times =c(0,1,2))

head(datPrePost3month)


write.csv(datPrePost3month, "datPrePost3month.csv", row.names = FALSE)
datPrePost3month = read.csv("datPrePost3month.csv", header = TRUE)




describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = as.factor(datPrePost3month$Sec1Qf.x)
describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = ifelse(datPrePost3month$Sec1Qf.x== 5, NA, datPrePost3month$Sec1Qf.x)
describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = ifelse(datPrePost3month$Sec1Qf.x == 2,1,0)
describe.factor(datPrePost3month$Sec1Qf.x)

describe.factor(datPrePost3month$Sec1Qi.x)
datPrePost3month$Sec1Qi.x = ifelse(datPrePost3month$Sec1Qi.x == 5, NA, datPrePost3month$Sec1Qi.x)
datPrePost3month$Sec1Qi.x = ifelse(datPrePost3month$Sec1Qi.x == 9, NA, datPrePost3month$Sec1Qi.x)
describe.factor(datPrePost3month$Sec1Qi.x)



describe.factor(datPrePost3month$Sec1Qg.x)
datPrePost3month$Sec1Qg.x = ifelse(datPrePost3month$Sec1Qg.x == 3, NA, datPrePost3month$Sec1Qg.x)
describe.factor(datPrePost3month$Sec1Qg.x)



describe.factor(datPrePost3month$Sec1Qh.x)
datPrePost3month$Sec1Qh.x = ifelse(datPrePost3month$Sec1Qh.x == 4, NA, datPrePost3month$Sec1Qh.x)
describe.factor(datPrePost3month$Sec1Qh.x)

describe.factor(datPrePost3month$Sec1Qk.x) 
datPrePost3month$Sec1Qk.x =ifelse(datPrePost3month$Sec1Qk.x == 5, NA, datPrePost3month$Sec1Qk.x)
describe.factor(datPrePost3month$Sec1Qk.x)


describe.factor(datPrePost3month$Sec1Ql.x)
datPrePost3month$Sec1Ql.x = ifelse(datPrePost3month$Sec1Ql.x > 1, NA, datPrePost3month$Sec1Ql.x)
describe.factor(datPrePost3month$Sec1Ql.x)





# 5 = -3; 4 = -2, 3 = -1, 6=0, 7=1,  8 = 2, 1 = NA, NA = NA, 9 = 3
describe.factor(datPrePost3month$Sec4QfA.x)
datPrePost3month$Sec4QfA.x = ifelse(datPrePost3month$Sec4QfA.x == " ", NA, ifelse(datPrePost3month$Sec4QfA.x == "-", NA, datPrePost3month$Sec4QfA.x))
describe.factor(datPrePost3month$Sec4QfA.x)

datPrePost3month$Sec4QfA.x = ifelse(datPrePost3month$Sec4QfA.x == 5, -3, ifelse(datPrePost3month$Sec4QfA.x  == 4, -2, ifelse(datPrePost3month$Sec4QfA.x == 3, -1, ifelse(datPrePost3month$Sec4QfA.x == 6, 0, ifelse(datPrePost3month$Sec4QfA.x == 7, 1, ifelse(datPrePost3month$Sec4QfA.x == 8, 2, ifelse(datPrePost3month$Sec4QfA.x == 1, NA, ifelse(datPrePost3month$Sec4QfA.x == 9, 3, datPrePost3month$Sec4QfA.x ))))))))
describe.factor(datPrePost3month$Sec4QfA.x)

describe.factor(datPrePost3month$Sec4QfB.x)
datPrePost3month$Sec4QfB.x = ifelse(datPrePost3month$Sec4QfB.x == -23, NA, datPrePost3month$Sec4QfB.x)
describe.factor(datPrePost3month$Sec4QfB.x)

describe.factor(datPrePost3month$Sec4QgB.x)
datPrePost3month$Sec4QgB.x = ifelse(datPrePost3month$Sec4QgB.x == -11, NA, datPrePost3month$Sec4QgB.x)
describe.factor(datPrePost3month$Sec4QgB.x)


describe.factor(datPrePost3month$Sec4QhA.x)
datPrePost3month$Sec4QhA.x = ifelse(datPrePost3month$Sec4QhA.x == -4, NA, datPrePost3month$Sec4QhA.x)
describe.factor(datPrePost3month$Sec4QhA.x)

describe.factor(datPrePost3month$Sec4QeB.x)
datPrePost3month$Sec4QeB.x = ifelse(datPrePost3month$Sec4QeB.x == -32, NA, ifelse(datPrePost3month$Sec4QeB.x == -4, NA, datPrePost3month$Sec4QeB.x))
describe.factor(datPrePost3month$Sec4QeB.x)

describe.factor(datPrePost3month$Sec2Qh.x)
datPrePost3month$Sec2Qh.x = ifelse(datPrePost3month$Sec2Qh.x == 56, NA, datPrePost3month$Sec2Qh.x)
describe.factor(datPrePost3month$Sec2Qh.x)

summary(datPrePost3month)

summary(datPrePost3month)

head(datPrePost3month)

datPrePost3month$Sec4QaA.x =  datPrePost3month$Sec4QaA.x--2.71
datPrePost3month$Sec4QaB.x =  datPrePost3month$Sec4QaB.x- 1.86 

datPrePost3month$Sec4QbA.x =  datPrePost3month$Sec4QbA.x--2.71
datPrePost3month$Sec4QbB.x =  datPrePost3month$Sec4QbB.x- 1.86 

datPrePost3month$Sec4QcA.x =  datPrePost3month$Sec4QcA.x--2.14
datPrePost3month$Sec4QcB.x =  datPrePost3month$Sec4QcB.x-2.14 

datPrePost3month$Sec4QdA.x =  datPrePost3month$Sec4QdA.x-1.29 
datPrePost3month$Sec4QdB.x =  datPrePost3month$Sec4QdB.x--2.71

datPrePost3month$Sec4QeA.x =  datPrePost3month$Sec4QeA.x-2.43 
datPrePost3month$Sec4QeB.x =  datPrePost3month$Sec4QeB.x--2.71 

datPrePost3month$Sec4QfA.x =  datPrePost3month$Sec4QfA.x--2 
datPrePost3month$Sec4QfB.x =  datPrePost3month$Sec4QfB.x-2.57


datPrePost3month$Sec4QgA.x =  datPrePost3month$Sec4QgA.x-2  
datPrePost3month$Sec4QgB.x =  datPrePost3month$Sec4QgB.x--1.29 

datPrePost3month$Sec4QhA.x =  datPrePost3month$Sec4QhA.x--2.29 
datPrePost3month$Sec4QhB =   datPrePost3month$Sec4QhB.x-2.14

datPrePost3month$Sec4QiA.x =  datPrePost3month$Sec4QiA.x--1.29 
datPrePost3month$Sec4QiB.x =  datPrePost3month$Sec4QiB.x-1.29  

datPrePost3month$Sec4QjA.x =  datPrePost3month$Sec4QjA.x-2.29 
datPrePost3month$Sec4QjB.x =  datPrePost3month$Sec4QjB.x--2.43  

datPrePost3month$Sec4QkA.x =  datPrePost3month$Sec4QkA.x--2.42  
datPrePost3month$Sec4QkB.x =  datPrePost3month$Sec4QkB.x-2.43 

datPrePost3month$Sec4QlA.x =  datPrePost3month$Sec4QlA.x-2.00 
datPrePost3month$Sec4QlB.x =  datPrePost3month$Sec4QlB.x-3.00 


head(datPrePost3month)

datPrePost3monthSec1 = datPrePost3month[,c(1,10:21)]
datPrePost3monthSec1Psych = datPrePost3monthSec1

head(datPrePost3monthSec1)

#here
describe.factor(datPrePost3monthSec1Psych$time)
head(datPrePost3monthSec1Psych)

datPrePost3monthSec1Psych = data.frame(datPrePost3monthSec1Psych)
write.csv(datPrePost3monthSec1Psych, "datPrePost3monthSec1Psych.csv", row.names = FALSE)
datPrePost3monthSec1Psych = read.csv("datPrePost3monthSec1Psych.csv", header = TRUE)

head(datPrePost3month)
datPrePost3monthSec2 = datPrePost3month[,c(1,22:36)]
head(datPrePost3monthSec2)

datPrePost3monthSec2Psych = datPrePost3monthSec2
describe.factor(datPrePost3monthSec2Psych$time)

head(datPrePost3monthSec2Psych)

datPrePost3monthSec2Psych = data.frame(datPrePost3monthSec2Psych)
write.csv(datPrePost3monthSec2Psych, "datPrePost3monthSec2Psych.csv", row.names = FALSE)
datPrePost3monthSec2Psych = read.csv("datPrePost3monthSec2Psych.csv", header = TRUE)


head(datPrePost3month)
datPrePost3monthSec3 = datPrePost3month[,c(1, 37:44)]
### Need to get reverse scoring for A,C,E,G
head(datPrePost3monthSec3)
summary(datPrePost3monthSec3)

datPrePost3monthSec3$Sec3Qa.x = ifelse(datPrePost3monthSec3$Sec3Qa.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qa.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qa.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qa.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qa.x == 5,1,datPrePost3monthSec3$Sec3Qa.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qa.x)


datPrePost3monthSec3$Sec3Qc.x = ifelse(datPrePost3monthSec3$Sec3Qc.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qc.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qc.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qc.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qc.x == 5,1,datPrePost3monthSec3$Sec3Qc.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qc.x)

datPrePost3monthSec3$Sec3Qd.x = ifelse(datPrePost3monthSec3$Sec3Qd.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qd.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qd.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qd.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qd.x == 5,1,datPrePost3monthSec3$Sec3Qd.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qd.x)


datPrePost3monthSec3$Sec3Qg.x = ifelse(datPrePost3monthSec3$Sec3Qg.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qg.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qg.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qg.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qg.x == 5,1,datPrePost3monthSec3$Sec3Qg.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qg.x)


datPrePost3monthSec3Psych = datPrePost3monthSec3

describe.factor(datPrePost3monthSec3Psych$time)


datPrePost3monthSec3Psych = data.frame(datPrePost3monthSec3Psych)
write.csv(datPrePost3monthSec3Psych, "datPrePost3monthSec3Psych.csv", row.names = FALSE)
datPrePost3monthSec3Psych = read.csv("datPrePost3monthSec3Psych.csv", header = TRUE)
head(datPrePost3monthSec3Psych)

head(datPrePost3month)
datPrePost3monthSec4 = datPrePost3month[,c(1, 45:68)]
head(datPrePost3monthSec4)

datPrePost3monthSec4Psych = datPrePost3monthSec4

describe.factor(datPrePost3monthSec4Psych$time)

datPrePost3monthSec4Psych = data.frame(datPrePost3monthSec4Psych)
write.csv(datPrePost3monthSec4Psych, "datPrePost3monthSec4Psych.csv", row.names = FALSE)
datPrePost3monthSec4Psych = read.csv("datPrePost3monthSec4Psych.csv", header = TRUE)

datPrePost3monthSec3F1Psych = datPrePost3monthSec3Psych[c("Sec3Qa.x", "Sec3Qc.x", "Sec3Qd.x", "Sec3Qg.x")]
datPrePost3monthSec3F2Psych = datPrePost3monthSec3Psych[c("Sec3Qe.x", "Sec3Qf.x", "Sec3Qb.x")]

```


ID's are included so need to get rid of that value

When there is a 1 in the variable that coorsponds to the items in section one.  For example, parallel1 and omegaSec1Base are both doing analyses with section one itmes


Below are the omega values for each section of measures.  You can interpret omega values like Cronbach alpha.  See  Peters(2014): https://drive.google.com/open?id=1S5qnisksx5RXwE5qD9MH8MHq0LFkndgX
```{r}
omegaSec1 = omega(datPrePost3monthSec1Psych[c(-1)])
summary(omegaSec1)

omegaSec2 =  omega(datPrePost3monthSec2Psych[c(-1)])
summary(omegaSec2)


omegaSec3F1 =  omega(datPrePost3monthSec3F1Psych)
summary(omegaSec3F1)

omegaSec3F2 = omega(datPrePost3monthSec3F2Psych)
summary(omegaSec3F2)

head(datPrePost3monthSec3Psych)
summary(omegaSec3)

omegaSec4 =  omega(datPrePost3monthSec4Psych[c(-1)])
summary(omegaSec4)
```
Now we have the parallel analyses for all four measures.  I am using factor analysis instead of principal component, because factor analysis assumes some error in the measurement, which seems like a better assumption for us.

I have some problems with the parrell analysis particularly with measure two, because the first eigenvalue is 8 and the second is less than .1 and it says that four factors should be retained, which seems ridiculous.
```{r}
datPrePost3monthSec1PsychComplete = na.omit(datPrePost3monthSec1Psych)
paran(x = datPrePost3monthSec1PsychComplete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)

datPrePost3monthSec2PsychComplete = na.omit(datPrePost3monthSec2Psych)
paran(x = datPrePost3monthSec2PsychComplete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)


datPrePost3monthSec3PsychComplete = na.omit(datPrePost3monthSec3Psych)
paran(x = datPrePost3monthSec3PsychComplete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)


datPrePost3monthSec3F1PsychComplete = na.omit(datPrePost3monthSec3F1Psych)
paran(x = datPrePost3monthSec3F1PsychComplete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)

datPrePost3monthSec3F2PsychComplete = na.omit(datPrePost3monthSec3F2Psych)
paran(x = datPrePost3monthSec3F2PsychComplete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)

datPrePost3monthSec4PsychComplete = na.omit(datPrePost3monthSec4Psych)
paran(x = datPrePost3monthSec4PsychComplete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)

```
Now I am trying the MAP and VSS tests.  It seems like it produces the MAP test, with this statement. "The Velicer MAP achieves a minimum of".  
I am also limiting it to four possible factors instead of the default eight, because the output is too unwelidly.
```{r}
vss(datPrePost3monthSec1Psych, n = 4)
vss(datPrePost3monthSec2Psych, n = 4)
vss(datPrePost3monthSec3F1Psych, n = 4)
vss(datPrePost3monthSec3F2Psych, n = 4)
vss(datPrePost3monthSec4Psych, n = 4)
```
Get a sample to run the EFA on sample.  If you select 4 it grabs all the 4's.
N is the same for all data sets so you are fine just selecting one
```{r}
Sec1Psych = createDataPartition(y = datPrePost3monthSec1Psych$ID, p = .35, list = FALSE)
Sec2Psych = createDataPartition(y = datPrePost3monthSec2Psych$ID, p = .35, list = FALSE)
Sec3Psych = createDataPartition(y = datPrePost3monthSec3Psych$ID, p = .35, list = FALSE)
Sec4Psych = createDataPartition(y = datPrePost3monthSec4Psych$ID, p = .35, list = FALSE)

Sec1PsychEFA = datPrePost3monthSec1Psych[Sec1Psych,]

Sec1PsychCFA = datPrePost3monthSec1Psych[-Sec1Psych,]

Sec1PsychEFA = datPrePost3monthSec1Psych[Sec1Psych,]
Sec1PsychCFA = datPrePost3monthSec1Psych[-Sec1Psych,]

Sec2PsychEFA = datPrePost3monthSec2Psych[Sec2Psych,]
Sec2PsychCFA = datPrePost3monthSec2Psych[-Sec2Psych,]

Sec3PsychEFA = datPrePost3monthSec3Psych[Sec3Psych,]
Sec3PsychCFA = datPrePost3monthSec3Psych[-Sec3Psych,]

Sec4PsychEFA = datPrePost3monthSec4Psych[Sec4Psych,]
Sec4PsychCFA = datPrePost3monthSec4Psych[-Sec4Psych,]
```



Use GLS, because that is better for ordinal data (doesn't assume data is continous, more interations, no more than 3 factors, we get heywood cases), missing equals true is median.  Use cor equals poly, because data is ordinal 

Should do 1,000, but takes too much time

Positive definite problems ok happens with polychoric correlations: https://personality-project.org/r/html/cor.smooth.html
```{r}
Sec1 <- fa(r = Sec1PsychEFA[c(-1)], nfactors = 3, n.iter = 100, fm = "gls", missing = TRUE, impute = "median", cor = "poly")
Sec1
fa.diagram(Sec1)

# Should use GLS, but it takes way too long
Sec2 <- fa(r = Sec2PsychEFA[c(-1)], nfactors = 3, fm = "gls", missing = TRUE, impute = "median", cor = "poly")
Sec2
fa.diagram(Sec2)


Sec3 <- fa(r = Sec3PsychEFA[c(-1)], nfactors = 2, fm = "gls", missing = TRUE, impute = "median", cor = "poly")
Sec3
fa.diagram(Sec3)

head(Sec3PsychEFA[c(-1)])


Sec3Factor1 <- fa(r = Sec3PsychEFA[c(-1)], nfactors = 1, fm = "gls", missing = TRUE, impute = "median", cor = "poly")
fa.diagram(Sec3Factor1)


anova(Sec3, Sec3Factor1)

# Responses are somewhat continious the defaults will work
Sec4F4 <- fa(r = Sec4PsychEFA[c(-1)], nfactors = 4, missing = TRUE, impute = "median")
Sec4F4
fa.diagram(Sec4F4)

Sec4F2 <- fa(r = Sec4PsychEFA[c(-1)], nfactors = 2, missing = TRUE, impute = "median")
Sec4F2
fa.diagram(Sec4F2)

```
Drop H two factor with second as e, f, b

Now I am testing each model with a CFA assuming each construct is unidimensional

For model1, something seems wrong.  The factor loading are very low and the CFI and TLI are way too high.  Probably just such a bad fit that the numbers are werid, but I will keep looking.
```{r}
model1 = 'SA =~ Sec1Qa.x + Sec1Qb.x + Sec1Qc.x + Sec1Qd.x + Sec1Qe.x + Sec1Qf.x + Sec1Qe.x + Sec1Qf.x + Sec1Qg.x + Sec1Qh.x + Sec1Qi.x + Sec1Qk.x + Sec1Ql.x'

fit1 = cfa(model1, estimator = "MLR",  missing = "ML", data = Sec1PsychCFA[c(-1)])
summary(fit1, fit.measures = TRUE, standardized = TRUE)



model2 = 'SA =~ Sec2Qa.x + Sec2Qb.x + Sec2Qc.x + Sec2Qd.x + Sec2Qe.x + Sec2Qf.x + Sec2Qg.x + Sec2Qh.x + Sec2Qi.x + Sec2Qj.x + Sec2Qk.x + Sec2Ql.x + Sec2Qm.x + Sec2Qn.x + Sec2Qo.x'

fit2 = cfa(model2, estimator = "MLR",  missing = "ML", data = Sec2PsychCFA[c(-1)])
summary(fit2, fit.measures = TRUE, standardized = TRUE)


#### Measure three

model3 = 'SA =~ Sec3Qa.x + Sec3Qb.x + Sec3Qc.x + Sec3Qd.x + Sec3Qe.x + Sec3Qf.x + Sec3Qg.x + Sec3Qh.x'

fit3 = cfa(model3, estimator = "MLR",  missing = "ML", data = Sec3PsychCFA[c(-1)])
summary(fit3, fit.measures = TRUE, standardized = TRUE)

model3Try2 = 'F1 =~ Sec3Qa.x +  + Sec3Qc.x + Sec3Qd.x +Sec3Qg.x 
              F2 =~  Sec3Qe.x + Sec3Qf.x + Sec3Qb.x'

fit3Try2 = cfa(model3Try2, estimator = "MLR",  missing = "ML", data = Sec3PsychCFA[c(-1)])
summary(fit3Try2, fit.measures = TRUE, standardized = TRUE)

anova(fit3Try2,fit3) 


modificationindices(fit3Try2)


model4 = 'SA =~ Sec4QaA.x + Sec4QaB.x + Sec4QbA.x + Sec4QbB.x + Sec4QcA.x +Sec4QcB.x + Sec4QdA.x + Sec4QdB.x + Sec4QeA.x + Sec4QeB.x + Sec4QfA.x + Sec4QfB.x + Sec4QgA.x + Sec4QgB.x + Sec4QhA.x + Sec4QhB.x + Sec4QiA.x + Sec4QiB.x + Sec4QjA.x + Sec4QjB.x + Sec4QkA.x + Sec4QkB.x + Sec4QlA.x + Sec4QlB.x'


fit4 = cfa(model4, estimator = "MLR",  missing = "ML", data = Sec4PsychCFA[c(-1)])
summary(fit4, fit.measures = TRUE, standardized = TRUE)


model42 = 'F1 =~ Sec4QdA.x + Sec4QaA.x + Sec4QlA.x + Sec4QhA.x + Sec4QeB.x + Sec4QiA.x + Sec4QgB.x + Sec4QfA.x + Sec4QbA.x


F2=~ Sec4QhB.x + Sec4QfB.x + Sec4QgA.x + Sec4QeA.x + Sec4QbB.x + Sec4QcA.x + Sec4QdB.x + Sec4QjA.x + Sec4QaB.x + Sec4QiB.x'


fit42 = cfa(model42, estimator = "MLR",  missing = "ML", data = Sec4PsychCFA[c(-1)])
summary(fit42, fit.measures = TRUE, standardized = TRUE)

```
#################
IRT for Section 2
#################

Now IRT for measure two
Needed to get rid of item D, because there is no response for 1
```{r}

Sec2IRT =  datPrePost3monthSec2Psych[c(-1)]

Sec2IRT$Sec2Qd.x = NULL
describe(Sec2IRT)

fitOrdGRM_Measure2_2PL = grm(data = Sec2IRT, constrained = FALSE)
fitOrdGRM_Measure2_1PL = grm(data = Sec2IRT, constrained = TRUE)
anova(fitOrdGRM_Measure2_1PL, fitOrdGRM_Measure2_2PL)


summary(fitOrdGRM_Measure2)

information(fitOrdGRM_Measure2, c(-3, 1))

plot(fitOrdGRM_Measure2, category = 1, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM_Measure2, category = 2, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM_Measure2, category = 3, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM_Measure2, category = 4, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)


vals <- plot(fitOrdGRM_Measure2, type = "IIC", items = 0, plot = FALSE, zrange = c(-3,3)) 

plot(vals[, "z"], 1 / sqrt(vals[, "test.info"]), type = "l", lwd = 2, xlab = "Ability", ylab = "Standard Error", main = "Standard Error of Measurement")

```
#####################
IRT for Section 3 F1
#####################

F1 =~ Sec3Qa.x +  + Sec3Qc.x + Sec3Qd.x +Sec3Qg.x 
F2 =~  Sec3Qe.x + Sec3Qf.x + Sec3Qb.x

For F1, get rid of those who responded in category 1, because so few people responded in that category
```{r}

datM3F1 =  datPrePost3monthSec3Psych
datM3F2 =  datPrePost3monthSec3Psych

head(datM3F1)
datM3F1 = datM3F1[c("Sec3Qa.x", "Sec3Qc.x", "Sec3Qd.x", "Sec3Qg.x")]
datM3F2 = datM3F2[c("Sec3Qe.x", "Sec3Qf.x", "Sec3Qb.x")]

describe(datM3F1)
describe(datM3F2)

datM3F1 = data.frame(apply(datM3F1, 2, function(x){ifelse(x == 1, NA, x)}))
datM3F1 = data.frame(apply(datM3F1, 2, function(x){ifelse(x == 2, 1, ifelse(x == 3,2,ifelse(x == 4, 3, ifelse(x == 5, 4, x))))}))
describe(datM3F1)


datM3F1 = data.frame(datM3F1)
describe(datM3F1)

fitOrdGRM_Measure3F1_2PL = grm(data = datM3F1, constrained = FALSE)
fitOrdGRM_Measure3F1_1PL = grm(data = datM3F1, constrained = TRUE)
anova(fitOrdGRM_Measure3F1_1PL,fitOrdGRM_Measure3F1_2PL)
summary(fitOrdGRM_Measure3F1_2PL)

information(fitOrdGRM_Measure3F1_2PL, c(-3, 1))

margins(fitOrdGRM_Measure3F1_2PL, "three")
margins(fitOrdGRM_Measure3F1_2PL, "two")

plot(fitOrdGRM_Measure3F1_2PL, category = 1, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM_Measure3F1_2PL, category = 2, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM_Measure3F1_2PL, category = 3, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM_Measure3F1_2PL, category = 4, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)


vals <- plot(fitOrdGRM_Measure3F1_2PL, type = "IIC", items = 0, plot = FALSE, zrange = c(-3,3)) 

plot(vals[, "z"], 1 / sqrt(vals[, "test.info"]), type = "l", lwd = 2, xlab = "Ability", ylab = "Standard Error", main = "Standard Error of Measurement")

```
#####################
IRT for Section 3 F2
#####################
F2 =~  Sec3Qe.x + Sec3Qf.x + Sec3Qb.x

```{r}
datM3F2 =  datPrePost3monthSec3Psych
datM3F2 = datM3F2[c("Sec3Qe.x", "Sec3Qf.x", "Sec3Qb.x")]

describe(datM3F2)

fitOrdGRM_Measure3F2_2PL = grm(data = datM3F2, constrained = FALSE)
fitOrdGRM_Measure3F2_1PL = grm(data = datM3F2, constrained = TRUE)
anova(fitOrdGRM_Measure3F2_1PL,fitOrdGRM_Measure3F2_2PL)
summary(fitOrdGRM_Measure3F2_2PL)

information(fitOrdGRM_Measure3F2_2PL, c(-3, 1))


margins(fitOrdGRM_Measure3F2_2PL, type = ("three-way"), rule = 3.5)
margins(fitOrdGRM_Measure3F2_2PL, "two")

plot(fitOrdGRM_Measure3F2_2PL, category = 1, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM_Measure3F2_2PL, category = 2, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM_Measure3F2_2PL, category = 3, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM_Measure3F2_2PL, category = 4, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)


vals <- plot(fitOrdGRM_Measure3F2_2PL, type = "IIC", items = 0, plot = FALSE, zrange = c(-3,3)) 

plot(vals[, "z"], 1 / sqrt(vals[, "test.info"]), type = "l", lwd = 2, xlab = "Ability", ylab = "Standard Error", main = "Standard Error of Measurement")

```


Now DIF for measure two

Previous example
genderDIF = lordif(resp.data = CIL_CKY, group = CIL_CKYDemo$Female, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(genderDIF)
plot(genderDIF)
Maybe if we have trouble with no responses on category one just drop that one from the model

```{r}
library(foreign)
library(nnet)
library(ggplot2)
library(prettyR)
library(nlme)
library(prettyR)
library(descr)
library(Amelia)
library(mitools)
library(BaylorEdPsych)
library(openxlsx)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(DescTools)
library(MissMech)
library(robustlmm)
library(jtools)
library(lmtest)
library(lmerTest)
library(MuMIn)
library(HLMdiag)
library(Hmisc)
library(stargazer)
library(paran)
library(caret)

setwd("P:/Evaluation/TN Lives Count_Writing/3_Target1_SUICClinicalTrainingComparison/3_Data & Analyses")
datPre = read.csv("Pre.csv", header = FALSE, row.names = NULL)

datPre = t(datPre)
write.csv(datPre, "datPre.csv", row.names = FALSE)
datPre = read.csv("datPre.csv", header = TRUE)
head(datPre)
dim(datPre)

datPre = datPre[,c(1, 3, 7:18, 21:35, 38:45, 49:72, 78:80, 83, 85, 94)]
datPre = data.frame(datPre)
head(datPre)

colnames(datPre) = c("ID", "Treatment", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB","Age", "Gender", "Eth", "Race", "Edu", "Clinical_Staff")
head(datPre)
### Get rif of first row once you figure out which variables to keep
datPre = datPre[-1,]
datPre = data.frame(datPre)
head(datPre)

#Only retain clincial staff 1
datPre = subset(datPre, Clinical_Staff == 1)
head(datPre)
dim(datPre)

setwd("P:/Evaluation/TN Lives Count_Writing/3_Target1_SUICClinicalTrainingComparison/3_Data & Analyses")
datPost = read.csv("Post.csv", header = FALSE, row.names = NULL)

datPost = t(datPost)
write.csv(datPost, "datPost.csv", row.names = FALSE)
datPost = read.csv("datPost.csv", header = TRUE)
head(datPost)

datPost = datPost[,c(3, 5, 15:26, 29:43, 46:53, 57:80)]

datPost = data.frame(datPost)

colnames(datPost) = c("ID", "Treatment", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB")
head(datPost)

# making treatment null, because it does not change and it will make so it doesn't repeat later 
datPost$ID = as.factor(datPost$ID)
datPost = datPost[-1,]
datPost$Treatment = NULL
datPost = data.frame(datPost)

dim(datPre)
dim(datPost)


write.csv(datPre, "datPre.csv", row.names = FALSE)
datPre = read.csv("datPre.csv", header = TRUE)

write.csv(datPost, "datPost.csv", row.names = FALSE)
datPost = read.csv("datPost.csv", header = TRUE)

#Should not have ID one, because they are the wrong code and not in datPre
datPrePost = merge(datPre, datPost, by = "ID",  all.x = TRUE, sort = TRUE)



dat3month = read.csv("3month.csv", header  = TRUE)
head(dat3month)
dat3month = dat3month[c(7, 11:22, 23:69)]
dim(datPost)
head(datPost)

# Now rename everything 
colnames(dat3month) = c("ID", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB")


datPrePost3month = merge(datPrePost, dat3month, by = "ID", all.x = TRUE, sort = TRUE)

head(datPrePost3month)


### Now make long format
### These variables are not included: 							

datPrePost3month = reshape(datPrePost3month, varying  = list(c("Sec1Qa.x", "Sec1Qa.y", "Sec1Qa"), c("Sec1Qb.x", "Sec1Qb.y", "Sec1Qb"), c("Sec1Qc.x", "Sec1Qc.y", "Sec1Qc"), c("Sec1Qd.x", "Sec1Qd.y", "Sec1Qd"), c("Sec1Qe.x", "Sec1Qe.y", "Sec1Qe"), c("Sec1Qf.x", "Sec1Qf.y", "Sec1Qf"), c("Sec1Qg.x", "Sec1Qg.y", "Sec1Qg"), c("Sec1Qh.x", "Sec1Qh.y", "Sec1Qh"), c("Sec1Qi.x", "Sec1Qi.y", "Sec1Qi"), c("Sec1Qj.x", "Sec1Qj.y", "Sec1Qj"), c("Sec1Qk.x", "Sec1Qk.y", "Sec1Qk"), c("Sec1Ql.x", "Sec1Ql.y", "Sec1Ql"), c("Sec2Qa.x", "Sec2Qa.y", "Sec2Qa"), c("Sec2Qb.x", "Sec2Qb.y", "Sec2Qb"), c("Sec2Qc.x", "Sec2Qc.y", "Sec2Qc"), c("Sec2Qd.x", "Sec2Qd.y", "Sec2Qd"), c("Sec2Qe.x", "Sec2Qe.y", "Sec2Qe"), c("Sec2Qf.x", "Sec2Qf.y", "Sec2Qf"), c("Sec2Qg.x", "Sec2Qg.y", "Sec2Qg"), c("Sec2Qh.x", "Sec2Qh.y", "Sec2Qh"), c("Sec2Qi.x", "Sec2Qi.y", "Sec2Qi"), c("Sec2Qj.x", "Sec2Qj.y", "Sec2Qj"), c("Sec2Qk.x", "Sec2Qk.y", "Sec2Qk"), c("Sec2Ql.x", "Sec2Ql.y", "Sec2Ql"), c("Sec2Qm.x", "Sec2Qm.y", "Sec2Qm"), c("Sec2Qn.x", "Sec2Qn.y", "Sec2Qn"), c("Sec2Qo.x", "Sec2Qo.y", "Sec2Qo"), c("Sec3Qa.x", "Sec3Qa.y","Sec3Qa"), c("Sec3Qb.x", "Sec3Qb.y", "Sec3Qb"), c("Sec3Qc.x", "Sec3Qc.y", "Sec3Qc"), c("Sec3Qd.x", "Sec3Qd.y", "Sec3Qd"), c("Sec3Qe.x", "Sec3Qe.y", "Sec3Qe"), c("Sec3Qf.x", "Sec3Qf.y", "Sec3Qf"), c("Sec3Qg.x", "Sec3Qg.y", "Sec3Qg"), c("Sec3Qh.x", "Sec3Qh.y", "Sec3Qh"), c("Sec4QaA.x", "Sec4QaA.y", "Sec4QaA"), c("Sec4QaB.x", "Sec4QaB.y", "Sec4QaB"), c("Sec4QbA.x", "Sec4QbA.y", "Sec4QbA"), c("Sec4QbB.x", "Sec4QbB.y", "Sec4QbB"), c("Sec4QcA.x", "Sec4QcA.y", "Sec4QcA"), c("Sec4QcB.x", "Sec4QcB.y", "Sec4QcB"), c("Sec4QdA.x", "Sec4QdA.y", "Sec4QdA"), c("Sec4QdB.x", "Sec4QdB.y", "Sec4QdB"), c("Sec4QeA.x", "Sec4QeA.y", "Sec4QeA"), c("Sec4QeB.x", "Sec4QeB.y", "Sec4QeB"), c("Sec4QfA.x", "Sec4QfA.y", "Sec4QfA"), c("Sec4QfB.x", "Sec4QfB.y", "Sec4QfB"), c("Sec4QgA.x", "Sec4QgA.y", "Sec4QgA"), c("Sec4QgB.x", "Sec4QgB.y", "Sec4QgB"), c("Sec4QhA.x", "Sec4QhA.y", "Sec4QhA"), c("Sec4QhB.x", "Sec4QhB.y", "Sec4QhB"), c("Sec4QiA.x", "Sec4QiA.y", "Sec4QiA"), c("Sec4QiB.x", "Sec4QiB.y", "Sec4QiB"), c("Sec4QjA.x", "Sec4QjA.y", "Sec4QjA"), c("Sec4QjB.x", "Sec4QjB.y", "Sec4QjB"), c("Sec4QkA.x", "Sec4QkA.y", "Sec4QkA"), c("Sec4QkB.x", "Sec4QkB.y", "Sec4QkB"), c("Sec4QlA.x", "Sec4QlA.y", "Sec4QlA"), c("Sec4QlB.x", "Sec4QlB.y", "Sec4QlB")), direction = "long", times =c(0,1,2))

head(datPrePost3month)


write.csv(datPrePost3month, "datPrePost3month.csv", row.names = FALSE)
datPrePost3month = read.csv("datPrePost3month.csv", header = TRUE)




describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = as.factor(datPrePost3month$Sec1Qf.x)
describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = ifelse(datPrePost3month$Sec1Qf.x== 5, NA, datPrePost3month$Sec1Qf.x)
describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = ifelse(datPrePost3month$Sec1Qf.x == 2,1,0)
describe.factor(datPrePost3month$Sec1Qf.x)

describe.factor(datPrePost3month$Sec1Qi.x)
datPrePost3month$Sec1Qi.x = ifelse(datPrePost3month$Sec1Qi.x == 5, NA, datPrePost3month$Sec1Qi.x)
datPrePost3month$Sec1Qi.x = ifelse(datPrePost3month$Sec1Qi.x == 9, NA, datPrePost3month$Sec1Qi.x)
describe.factor(datPrePost3month$Sec1Qi.x)



describe.factor(datPrePost3month$Sec1Qg.x)
datPrePost3month$Sec1Qg.x = ifelse(datPrePost3month$Sec1Qg.x == 3, NA, datPrePost3month$Sec1Qg.x)
describe.factor(datPrePost3month$Sec1Qg.x)



describe.factor(datPrePost3month$Sec1Qh.x)
datPrePost3month$Sec1Qh.x = ifelse(datPrePost3month$Sec1Qh.x == 4, NA, datPrePost3month$Sec1Qh.x)
describe.factor(datPrePost3month$Sec1Qh.x)

describe.factor(datPrePost3month$Sec1Qk.x) 
datPrePost3month$Sec1Qk.x =ifelse(datPrePost3month$Sec1Qk.x == 5, NA, datPrePost3month$Sec1Qk.x)
describe.factor(datPrePost3month$Sec1Qk.x)


describe.factor(datPrePost3month$Sec1Ql.x)
datPrePost3month$Sec1Ql.x = ifelse(datPrePost3month$Sec1Ql.x > 1, NA, datPrePost3month$Sec1Ql.x)
describe.factor(datPrePost3month$Sec1Ql.x)





# 5 = -3; 4 = -2, 3 = -1, 6=0, 7=1,  8 = 2, 1 = NA, NA = NA, 9 = 3
describe.factor(datPrePost3month$Sec4QfA.x)
datPrePost3month$Sec4QfA.x = ifelse(datPrePost3month$Sec4QfA.x == " ", NA, ifelse(datPrePost3month$Sec4QfA.x == "-", NA, datPrePost3month$Sec4QfA.x))
describe.factor(datPrePost3month$Sec4QfA.x)

datPrePost3month$Sec4QfA.x = ifelse(datPrePost3month$Sec4QfA.x == 5, -3, ifelse(datPrePost3month$Sec4QfA.x  == 4, -2, ifelse(datPrePost3month$Sec4QfA.x == 3, -1, ifelse(datPrePost3month$Sec4QfA.x == 6, 0, ifelse(datPrePost3month$Sec4QfA.x == 7, 1, ifelse(datPrePost3month$Sec4QfA.x == 8, 2, ifelse(datPrePost3month$Sec4QfA.x == 1, NA, ifelse(datPrePost3month$Sec4QfA.x == 9, 3, datPrePost3month$Sec4QfA.x ))))))))
describe.factor(datPrePost3month$Sec4QfA.x)

describe.factor(datPrePost3month$Sec4QfB.x)
datPrePost3month$Sec4QfB.x = ifelse(datPrePost3month$Sec4QfB.x == -23, NA, datPrePost3month$Sec4QfB.x)
describe.factor(datPrePost3month$Sec4QfB.x)

describe.factor(datPrePost3month$Sec4QgB.x)
datPrePost3month$Sec4QgB.x = ifelse(datPrePost3month$Sec4QgB.x == -11, NA, datPrePost3month$Sec4QgB.x)
describe.factor(datPrePost3month$Sec4QgB.x)


describe.factor(datPrePost3month$Sec4QhA.x)
datPrePost3month$Sec4QhA.x = ifelse(datPrePost3month$Sec4QhA.x == -4, NA, datPrePost3month$Sec4QhA.x)
describe.factor(datPrePost3month$Sec4QhA.x)

describe.factor(datPrePost3month$Sec4QeB.x)
datPrePost3month$Sec4QeB.x = ifelse(datPrePost3month$Sec4QeB.x == -32, NA, ifelse(datPrePost3month$Sec4QeB.x == -4, NA, datPrePost3month$Sec4QeB.x))
describe.factor(datPrePost3month$Sec4QeB.x)

describe.factor(datPrePost3month$Sec2Qh.x)
datPrePost3month$Sec2Qh.x = ifelse(datPrePost3month$Sec2Qh.x == 56, NA, datPrePost3month$Sec2Qh.x)
describe.factor(datPrePost3month$Sec2Qh.x)

summary(datPrePost3month)

summary(datPrePost3month)

head(datPrePost3month)

datPrePost3month$Sec4QaA.x =  datPrePost3month$Sec4QaA.x--2.71
datPrePost3month$Sec4QaB.x =  datPrePost3month$Sec4QaB.x- 1.86 

datPrePost3month$Sec4QbA.x =  datPrePost3month$Sec4QbA.x--2.71
datPrePost3month$Sec4QbB.x =  datPrePost3month$Sec4QbB.x- 1.86 

datPrePost3month$Sec4QcA.x =  datPrePost3month$Sec4QcA.x--2.14
datPrePost3month$Sec4QcB.x =  datPrePost3month$Sec4QcB.x-2.14 

datPrePost3month$Sec4QdA.x =  datPrePost3month$Sec4QdA.x-1.29 
datPrePost3month$Sec4QdB.x =  datPrePost3month$Sec4QdB.x--2.71

datPrePost3month$Sec4QeA.x =  datPrePost3month$Sec4QeA.x-2.43 
datPrePost3month$Sec4QeB.x =  datPrePost3month$Sec4QeB.x--2.71 

datPrePost3month$Sec4QfA.x =  datPrePost3month$Sec4QfA.x--2 
datPrePost3month$Sec4QfB.x =  datPrePost3month$Sec4QfB.x-2.57


datPrePost3month$Sec4QgA.x =  datPrePost3month$Sec4QgA.x-2  
datPrePost3month$Sec4QgB.x =  datPrePost3month$Sec4QgB.x--1.29 

datPrePost3month$Sec4QhA.x =  datPrePost3month$Sec4QhA.x--2.29 
datPrePost3month$Sec4QhB =   datPrePost3month$Sec4QhB.x-2.14

datPrePost3month$Sec4QiA.x =  datPrePost3month$Sec4QiA.x--1.29 
datPrePost3month$Sec4QiB.x =  datPrePost3month$Sec4QiB.x-1.29  

datPrePost3month$Sec4QjA.x =  datPrePost3month$Sec4QjA.x-2.29 
datPrePost3month$Sec4QjB.x =  datPrePost3month$Sec4QjB.x--2.43  

datPrePost3month$Sec4QkA.x =  datPrePost3month$Sec4QkA.x--2.42  
datPrePost3month$Sec4QkB.x =  datPrePost3month$Sec4QkB.x-2.43 

datPrePost3month$Sec4QlA.x =  datPrePost3month$Sec4QlA.x-2.00 
datPrePost3month$Sec4QlB.x =  datPrePost3month$Sec4QlB.x-3.00 

#Then created dicotmoized variables
#Gender: Males = 1, Female = 0 no
##Race: White =1, other racial identity
#Edu: Bachelors or lower = 1, higher than Bachelors = 0

describe.factor(datPrePost3month$Gender)
datPrePost3month$Gender = ifelse(datPrePost3month$Gender == 1,1,0)
datPrePost3month$Race = ifelse(datPrePost3month$Race == 5, 0, 1)
describe.factor(datPrePost3month$Edu)
datPrePost3month$Edu = ifelse(datPrePost3month$Edu < 6, 1, 0)


head(datPrePost3month)
## Now we need to add back the demographics
#Need 3,4,5,7
head(datPrePost3month)

datPrePost3monthSec1 = datPrePost3month[,c(1,4,5,6,9,10:21)]
datPrePost3monthSec1Psych = datPrePost3monthSec1

head(datPrePost3monthSec1)

#here
describe.factor(datPrePost3monthSec1Psych$time)
head(datPrePost3monthSec1Psych)

datPrePost3monthSec1Psych = data.frame(datPrePost3monthSec1Psych)
write.csv(datPrePost3monthSec1Psych, "datPrePost3monthSec1Psych.csv", row.names = FALSE)
datPrePost3monthSec1Psych = read.csv("datPrePost3monthSec1Psych.csv", header = TRUE)

head(datPrePost3month)
datPrePost3monthSec2 = datPrePost3month[,c(1,4,5,6,9,22:36)]
head(datPrePost3monthSec2)

datPrePost3monthSec2Psych = datPrePost3monthSec2
describe.factor(datPrePost3monthSec2Psych$time)

head(datPrePost3monthSec2Psych)

datPrePost3monthSec2Psych = data.frame(datPrePost3monthSec2Psych)
write.csv(datPrePost3monthSec2Psych, "datPrePost3monthSec2Psych.csv", row.names = FALSE)
datPrePost3monthSec2Psych = read.csv("datPrePost3monthSec2Psych.csv", header = TRUE)


head(datPrePost3month)
datPrePost3monthSec3 = datPrePost3month[,c(1,4,5,6,9, 37:44)]
### Need to get reverse scoring for A,C,E,G
head(datPrePost3monthSec3)
summary(datPrePost3monthSec3)

datPrePost3monthSec3$Sec3Qa.x = ifelse(datPrePost3monthSec3$Sec3Qa.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qa.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qa.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qa.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qa.x == 5,1,datPrePost3monthSec3$Sec3Qa.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qa.x)


datPrePost3monthSec3$Sec3Qc.x = ifelse(datPrePost3monthSec3$Sec3Qc.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qc.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qc.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qc.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qc.x == 5,1,datPrePost3monthSec3$Sec3Qc.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qc.x)

datPrePost3monthSec3$Sec3Qd.x = ifelse(datPrePost3monthSec3$Sec3Qd.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qd.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qd.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qd.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qd.x == 5,1,datPrePost3monthSec3$Sec3Qd.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qd.x)


datPrePost3monthSec3$Sec3Qg.x = ifelse(datPrePost3monthSec3$Sec3Qg.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qg.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qg.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qg.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qg.x == 5,1,datPrePost3monthSec3$Sec3Qg.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qg.x)


datPrePost3monthSec3Psych = datPrePost3monthSec3

describe.factor(datPrePost3monthSec3Psych$time)


datPrePost3monthSec3Psych = data.frame(datPrePost3monthSec3Psych)
write.csv(datPrePost3monthSec3Psych, "datPrePost3monthSec3Psych.csv", row.names = FALSE)
datPrePost3monthSec3Psych = read.csv("datPrePost3monthSec3Psych.csv", header = TRUE)
head(datPrePost3monthSec3Psych)

head(datPrePost3month)
datPrePost3monthSec4 = datPrePost3month[,c(1,4,5,6,9, 45:68)]
head(datPrePost3monthSec4)

datPrePost3monthSec4Psych = datPrePost3monthSec4

describe.factor(datPrePost3monthSec4Psych$time)

datPrePost3monthSec4Psych = data.frame(datPrePost3monthSec4Psych)
write.csv(datPrePost3monthSec4Psych, "datPrePost3monthSec4Psych.csv", row.names = FALSE)
datPrePost3monthSec4Psych = read.csv("datPrePost3monthSec4Psych.csv", header = TRUE)
```
Now DIFF for Measure 2

Need to drop those who responded one to any of the items for measure two.
Treating the one's as NA's, but could treat them as two or do binary.  Not really sure what to do.  
```{r}
describe(datPrePost3monthSec2Psych)

datIRT_M2 = datPrePost3monthSec2Psych
datIRT_M2Descript = datIRT_M2[c(1:5)]

datIRT_M2Items = datIRT_M2[c(6:20)]

datIRT_M2Items = data.frame(apply(datIRT_M2Items, 2, function(x){ifelse(x == 1, NA, x)}))


genderM2_DIF = lordif(resp.data = datIRT_M2Items, group = datIRT_M2Descript$Gender, criterion = "Chisqr", alpha = .01, minCell = 5, MonteCarlo = TRUE)
summary(genderM2_DIF)
plot(genderM2_DIF)

DFIT(genderM2_DIF)

raceM2_DIF = lordif(resp.data = datIRT_M2Items, group = datIRT_M2Descript$Race, criterion = "R2", alpha = .01, minCell = 5, MonteCarlo = TRUE)
summary(raceM2_DIF)
plot(raceM2_DIF)


timeM2_DIF = lordif(resp.data = datIRT_M2Items, group = datIRT_M2Descript$time, criterion = "Chisqr", alpha = .01, minCell = 5, MonteCarlo = TRUE)
summary(timeM2_DIF)
plot(timeM2_DIF)

test = DFIT(timeM2_DIF)
test$DFIT
```
#################
DIF Measure 3 F1
#################

F1 =~ Sec3Qa.x +  + Sec3Qc.x + Sec3Qd.x +Sec3Qg.x 
F2 =~  Sec3Qe.x + Sec3Qf.x + Sec3Qb.x

```{r}
datIRT_M3 = datPrePost3monthSec3Psych
datIRT_M3Descript = datIRT_M3[c(1:5)]

datIRT_M3Items = datIRT_M3[c("Sec3Qa.x", "Sec3Qc.x", "Sec3Qd.x", "Sec3Qg.x")]
datIRT_M3Items = data.frame(apply(datIRT_M3Items, 2, function(x){ifelse(x == 1, NA, x)}))
datIRT_M3Items = data.frame(apply(datIRT_M3Items, 2, function(x){ifelse(x == 2, 1, ifelse(x == 3,2,ifelse(x == 4, 3, ifelse(x == 5, 4, x))))}))

describe(datIRT_M3Items)
datIRT_M3Items = data.frame(apply(datIRT_M3Items, 2, function(x){ifelse(x == 1, NA, x)}))

genderM3_DIF = lordif(resp.data = datIRT_M3Items, group = datIRT_M3Descript$Gender, criterion = "Chisqr", alpha = .01, minCell = 5))
summary(genderM3_DIF)
plot(genderM3_DIF)


raceM3_DIF = lordif(resp.data = datIRT_M3Items, group = datIRT_M3Descript$Race, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(raceM3_DIF)
plot(raceM3_DIF)


timeM3_DIF = lordif(resp.data = datIRT_M3Items, group = datIRT_M3Descript$time, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(timeM3_DIF)
plot(timeM3_DIF)
```
#################
DIF Measure 3 F2
#################
F2 =~  Sec3Qe.x + Sec3Qf.x + Sec3Qb.x
## Cannot do DIF with this measure need at least four items
Do Measurement invariance testing with this one
F2 =~  Sec3Qe.x + Sec3Qf.x + Sec3Qb.x

modelMIWO = measurementInvariance(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = itemsOnlyWO, group = "G3..Race.", strict = TRUE)
summary(modelMIWO, fit.measure = TRUE)


model3Try2 = 'F1 =~ Sec3Qa.x +  + Sec3Qc.x + Sec3Qd.x +Sec3Qg.x 
              

fit3Try2 = cfa(model3Try2, estimator = "MLR",  missing = "ML", data = Sec3PsychCFA[c(-1)])
summary(fit3Try2, fit.measures = TRUE, standardized = TRUE)

```{r}
datMI_M3 = datPrePost3monthSec3Psych

modelM3F1 = 'F2 =~  Sec3Qe.x + Sec3Qf.x + Sec3Qb.x'
fitM3F1 = cfa(modelM3F1, estimator = "MLR",  missing = "ML", data = datMI_M3)
summary(fitM3F1, fit.measures = TRUE, standardized = TRUE)

datMI_M3$Eth

modelM3F1Gender = measurementInvariance(modelM3F1, estimator = "MLR",  missing = "ML", data = datMI_M3, group = "Gender", strict = TRUE)
modelM3F1Gender


modelM3F1Eth = measurementInvariance(modelM3F1, estimator = "MLR",  missing = "ML", data = datMI_M3, group = "Eth", strict = TRUE)
modelM3F1Eth

```
