library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  ##工作路径改为当前运行文件所在路径
getwd()

data <- read.csv('cognitive_data.csv')


# Preprocessing data ------------------------------------------------------

data <- na.omit(data)

data$x2 <- as.numeric(gsub("[^0-9]", "", data$x2))

data <- na.omit(data)

#write.csv(data[c(0:100),], file = 'cognitive_new.csv')


library(moments)
skewness(data)
kurtosis(data)

## polychoric correlation 

library(polycor)
cor_polychoric <- hetcor(data)$correlations


library(psych)
KMO(cor_polychoric)
bartlett.test(data)

library(ltm)
cronbach.alpha(data)

# # of factors ---------------------------------------------------------------------

eigen.values <- eigen(cor_polychoric)  # eigenvalues greater than 1.0 : 2
eigen.values$values

VSS(data) # 1 or 2

library('EFA.dimensions')
MAP(cor_polychoric, Ncases=590)  ## 1

library('EFAtools')
CD(data) # 2


library('random.polychor.pa')
random.polychor.pa(data.matrix=data, q.eigen = 0.95, nrep=100)  # 1 or 3


# EFA ---------------------------------------------------------------------

library(GPArotation)

fa_2 <- fa(cor_polychoric, nfactors = 2, rotate = "oblimin", fm="pa")
fa(cor_polychoric, nfactors = 2, rotate = "none", fm="pa")
print(fa_2, cut=0.25)
fa.diagram(fa_2, digits = 2)

fa_3 <- fa(cor_polychoric, nfactors = 3, rotate = "oblimin", fm="pa")
fa(cor_polychoric, nfactors = 3, rotate = "none", fm="pa")
print(fa_3, cut=0.3)
fa.diagram(fa_3, digits = 3)


## Reliability Check of Item Groups
psych::alpha(data[ ,c(2:4,6:8)], check.keys=TRUE) # cronbach alpha per factor
psych::alpha(data[ ,9:10], check.keys=TRUE)
psych::alpha(data[ ,c(1,5)], check.keys=TRUE)



# CFA-----------------------------------------------------------------------

library(lavaan)

          
model <- 'MR1 =~ x2+x3+x4+x6+x7+x8
	        MR2 =~ x9+x10
	        MR3 =~ x1+x5
	       '

fit <- cfa(model, sample.cov=cor_polychoric, sample.nobs = 590,std.lv=T)

summary(fit, fit.measures = T, # 输出拟合指标
        standard = T)  # 输出标准化解


###画图
library(semPlot)
semPaths(fit, what = "path", whatLabels = "stand", style = "lisrel", 
         layout = "tree2", rotation = 2, color = "lightblue", nCharNodes = 0)

library(semTools)
reliability(fit)



# IRT ---------------------------------------------------------------------
library('mirt')
library('ggmirt')

fitgpcm_2 <- mirt(data, model = 2, itemtype = 'gpcm')
summary(fitgpcm_2, suppress = 0.3)

fitgpcm_3 <- mirt(data, model = 3, itemtype = 'gpcm')
summary(fitgpcm_3, suppress = 0.3)



fitGraded_2 <- mirt(data, model = 2, itemtype = "graded")
summary(fitGraded_2, suppress = 0.3)

fitGraded_3 <- mirt(data, 3, itemtype = "graded")
summary(fitGraded_3, suppress = 0.3)


# Goodness of fit ---------------------------------------------------------------------

M2(fitGraded_2, type = "C2", calcNULL = FALSE, QMC = TRUE)
M2(fitGraded_3, type = "C2", calcNULL = FALSE, QMC = TRUE)
M2(fitgpcm_2, type = "C2", calcNULL = FALSE, QMC = TRUE)
M2(fitgpcm_3, type = "C2", calcNULL = FALSE, QMC = TRUE)

#Item fit
itemfit(fitGraded_2, QMC = TRUE, p.adjust = 'fdr')  
itemfit(fitGraded_3, QMC = TRUE, p.adjust = 'fdr')  
itemfit(fitgpcm_2, QMC = TRUE, p.adjust = 'fdr') 
itemfit(fitgpcm_3, QMC = TRUE, p.adjust = 'fdr') 

#Person fit
personfitPlot(fitGraded_2)
personfitPlot(fitGraded_3)

personfitPlot(fitgpcm_2)
personfitPlot(fitgpcm_3)


#p-values to evaluate local dependence. 
residuals(fitGraded_2, df.p = TRUE, p.adjust = 'fdr', type='LDG2')
residuals(fitGraded_2, df.p = TRUE, p.adjust = 'fdr', type='LD')
residuals(fitGraded_2, df.p = TRUE, p.adjust = 'fdr', type='Q3')

residuals(fitGraded_3, df.p = TRUE, p.adjust = 'fdr', type='LDG2')
residuals(fitgpcm_2, df.p = TRUE, p.adjust = 'fdr', type='LDG2')
residuals(fitgpcm_3, df.p = TRUE, p.adjust = 'fdr', type='LDG2')


## Nested model comparison 
anova(fitGraded_2, fitGraded_3)


# Plots -----------------------------------------------------------------

plot(fitGraded_1)
plot(fitGraded_1, type = 'info')
plot(fitGraded_1, type = 'trace')
plot(fitGraded_1, type = 'infotrace')

itemplot(fitGraded_1, 4, type = 'info')
itemplot(fitGraded_1, 4, type = 'threshold')
itemplot(fitGraded_1, 3, type = 'trace')
itemplot(fitGraded_1, 3, type = 'infotrace')


# CFA_GRM -----------------------------------------------------------------


cfa2 <- mirt.model('F1 = 2,3,4,5,6,7,8
                    F2 = 1,9,10
                    COV = F1*F2')

Mol2 <- mirt(data, cfa2, itemtype='gpcm', SE=T, TOL=0.001)

summary(Mol2, suppress = 0.3)




