---
title: "Psychometrics Gatekeeper Prelim Results"
output: ''
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
When there is a 1 in the variable that coorsponds to the items in section one.  For example, parallel1 and omegaSec1Base are both doing analyses with section one itmes


Below are the omega values for each section of measures.  You can interpret omega values like Cronbach alpha.  See  Peters(2014): https://drive.google.com/open?id=1S5qnisksx5RXwE5qD9MH8MHq0LFkndgX
```{r}
omegaSec1Base = omega(datPrePost3monthSec1Base)
summary(omegaSec1Base)

omegaSec2Base =  omega(datPrePost3monthSec2Base)
summary(omegaSec2Base)

omegaSec3Base =  omega(datPrePost3monthSec3Base)
summary(omegaSec3Base)

omegaSec4Base =  omega(datPrePost3monthSec4Base)
summary(omegaSec4Base)

```
Correlation matrices for each item for each measure

I used perason's correlation for the first, because they are all binary and when this happens pearson's approximates to the phi coefficient, which is what we want for correlations between binary variables.  I use spearman for the other measures, because those are ordinal.
```{r}
summary(datPrePost3monthSec1Base)
datPrePost3monthSec1Base = as.matrix(datPrePost3monthSec1Base)
datPrePost3monthSec2Base = as.matrix(datPrePost3monthSec2Base)
datPrePost3monthSec3Base = as.matrix(datPrePost3monthSec3Base)
datPrePost3monthSec4Base = as.matrix(datPrePost3monthSec4Base)

rcorr(datPrePost3monthSec1Base)
rcorr(datPrePost3monthSec2Base, type = "spearman")
rcorr(datPrePost3monthSec3Base, type = "spearman")
rcorr(datPrePost3monthSec4Base, type = "spearman")

```




Now we have the parallel analyses for all four measures.  I am using factor analysis instead of principal component, because factor analysis assumes some error in the measurement, which seems like a better assumption for us.

I have some problems with the parrell analysis particularly with measure two, because the first eigenvalue is 8 and the second is less than .5 and it says that four factors should be retained, which seems ridiculous.
```{r}
parallel1 = fa.parallel(datPrePost3monthSec1Base, fa= "fa")
parallel1$fa.values

parallel2 = fa.parallel(datPrePost3monthSec2Base, fa= "fa")
parallel2$fa.values

parallel3 = fa.parallel(datPrePost3monthSec3Base, fa= "fa")
parallel3$fa.values

parallel4 = fa.parallel(datPrePost3monthSec4Base, fa= "fa")
parallel4$fa.values

```
Now I am trying the MAP and VSS tests.  It seems like it produces the MAP test, with this statement. "The Velicer MAP achieves a minimum of".  
I am also limiting it to four possible factors instead of the default eight, because the output is too unwelidly.
```{r}
vss(datPrePost3monthSec1Base, n = 4)
vss(datPrePost3monthSec2Base, n = 4)
vss(datPrePost3monthSec3Base, n = 4)
vss(datPrePost3monthSec4Base, n = 4)
```
Now I am testing each model with a CFA assuming each construct is unidimensional
```{r}
model1 = 'SA =~ Sec1Qa.x + Sec1Qb.x + Sec1Qc.x + Sec1Qd.x + Sec1Qe.x + Sec1Qf.x + Sec1Qe.x + Sec1Qf.x + Sec1Qg.x + Sec1Qh.x + Sec1Qi.x + Sec1Qk.x + Sec1Ql.x'

fit1 = cfa(model1, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = datPrePost3monthSec1Base)
summary(fit1, fit.measures = TRUE)


head(datPrePost3monthSec2Base)
model2 = 'SA =~ Sec2Qa.x + Sec2Qb.x + Sec2Qc.x + Sec2Qd.x + Sec2Qe.x + Sec2Qf.x + Sec2Qg.x + Sec2Qh.x + Sec2Qi.x + Sec2Qj.x + Sec2Qk.x + Sec2Ql.x + Sec2Qm.x + Sec2Qn.x + Sec2Qo.x'

fit2 = cfa(model2, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = datPrePost3monthSec2Base)
summary(fit2, fit.measures = TRUE)


head(datPrePost3monthSec3Base)
model3 = 'SA =~ Sec3Qa.x + Sec3Qb.x + Sec3Qc.x + Sec3Qd.x + Sec3Qe.x + Sec3Qf.x + Sec3Qg.x + Sec3Qh.x'

fit3 = cfa(model3, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = datPrePost3monthSec3Base)
summary(fit3, fit.measures = TRUE)


head(datPrePost3monthSec4Base)
model4 = 'SA =~ Sec4QaA.x + Sec4QaB.x + Sec4QbA.x + Sec4QbB.x + Sec4QcA.x +Sec4QcB.x + Sec4QdA.x + Sec4QdB.x + Sec4QeA.x + Sec4QeB.x + Sec4QfA.x + Sec4QfB.x + Sec4QgA.x + Sec4QgB.x + Sec4QhA.x + Sec4QhB.x + Sec4QiA.x + Sec4QiB.x + Sec4QjA.x + Sec4QjB.x + Sec4QkA.x + Sec4QkB.x + Sec4QlA.x + Sec4QlB.x'


fit4 = cfa(model4, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = datPrePost3monthSec4Base)
summary(fit4, fit.measures = TRUE)

```
