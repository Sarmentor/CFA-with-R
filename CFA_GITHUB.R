
#without scientific notation
options(scipen=999)
#scientific notation
options(scipen=0)

options(digits=8)

##Factorial Analysis - BEGIN##

#data.df<-read.csv("data.csv",sep=";")
data.df<-read.csv("dados_livro.csv",sep=",")

data.df <- data.df[,-c(1)]
### Descriptive analysis of Q1 to Q21 variables
# Identification of the variables used in factor analysis
survey<-data.df[, paste("Q", 1:21, sep="")]
# Descriptive analysis for each variable
summary(survey)

### Correlation between variables Q1 to Q21
correlation <- cor(survey, method="spearman")
correlation

### Bartlett Sphericity test
library (psych)
cortest.bartlett(correlation, n=nrow(data.df))

### KMO Measure
library (psych)
KMO(correlation)

### Kaiser criterion
library (psych)
eigen(correlation)

### Scree plot criterion
library(nFactors)
scree(correlation, hline=-1) # hline=-1 draw a horizontal line at -1

### Explained variance for each component
pc <- prcomp(survey,scale.=F)
summary(pc)

### Principal Component method
library (psych)
pca <- principal(correlation,nfactors=4, rotate="none")

### Principal Component method with varimax rotation
library (psych)
pca.varimax <- principal(correlation,nfactors=4, rotate="varimax")

### Internal consistency
# PC1 (Q1, ..., Q9)
library (psych)
alpha(survey[c(paste("Q",1:9,sep=""))])

### Internal consistency
# PC2 (Q10...Q14)
library (psych)
alpha(survey[c(paste("Q",10:14,sep=""))])

### Internal consistency
# PC2 (Q15...Q18)
library (psych)
alpha(survey[c(paste("Q",15:18,sep=""))])

### Internal consistency
# PC2 (Q19...Q21)
library (psych)
alpha(survey[c(paste("Q",19:21,sep=""))])

### Internal Consistency of the Model with CR ###

#factor loading for each item


# Compute residual variance of each item
re <- 1 - sl^2

# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))


### Internal Consistency of the Model with AVE ###



##Factorial Analysis - END##


##Confirmatory Analysis - Begin##
library(foreign)
library(sem)

pca.coefs <- matrix(as.numeric(pca.varimax$loadings[1:length(pca.varimax$loadings)]),nrow=21,ncol=4,byrow=FALSE) 

nfactor <- apply(pca.coefs,1,FUN=function(x){
  which.max(x)
  })

crave <- data.frame(matrix(ncol=2,nrow=0),stringsAsFactors=FALSE)


sapply(unique(nfactor), function(x){
    coeff  <- pca.coefs[which(x==nfactor),x]
    coeff2 <- pca.coefs[which(x==nfactor),x]^2
    e2 <- 1-coeff2
    cr <- sum(coeff)^2 / (sum(coeff)^2+sum(e2))
    ave <- sum(coeff2) / (sum(coeff2)+sum(e2))
    crave <<- rbind(crave,c(cr,ave))
})

names(crave) <- c("CR","AVE") 
crave

#matriz correlacional com AVE na diagonal
library(psych)
library(GPArotation)
m.cor.ave <- fa(correlation, nfactors=4)$score.cor

for (i in 1:4){
  m.cor.ave[i,i] <- sqrt(crave[i,"AVE"])
}
m.cor.ave


##
dataCov <- cov(data.df)

## CFA with Lavaan package 
library(lavaan)

Q.model <-'FACTOR1 =~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9
FACTOR2 =~ Q10 + Q11 + Q12 + Q13 + Q14
FACTOR3 =~ Q15 + Q16 + Q17 + Q18
FACTOR4 =~ Q19 + Q20 + Q21'


fit <- sem(Q.model, data = data.df)
summary(fit, fit.measures=TRUE)

#Every combination of variables (SEM package)
cfa.model <- specifyModel(file="measurementsmodel.txt")
cfaOut<-sem(cfa.model,S=dataCov,N=204)
summary(cfaOut)

#Our specified model
#with lavaan 

library(lavaan)

Q.model.2 <-'FACTOR1 =~ Q1 + Q2 + Q3 + Q4 + Q6 + Q7 + Q8 + Q9
FACTOR2 =~ Q12 + Q13 + Q14
FACTOR3 =~ Q15 + Q16 + Q17 + Q18
FACTOR4 =~ Q20 + Q21'


fit <- sem(Q.model.2, data = data.df)
summary(fit, fit.measures=TRUE)



#with sem
library(sem)
cfa.model.2<-specifyModel("adjustedmodel.txt")
cfaOut.2<-sem(cfa.model.2,S=dataCov,N=204)
summary(cfaOut.2)

#with lavaan
library(lavaan)

Q.model.3 <-'FACTOR1 =~ Q1 + Q2 + Q3 + Q4 + Q6 + Q7 + Q8 + Q9
FACTOR2 =~ Q12 + Q13 + Q14
FACTOR3 =~ Q15 + Q16 + Q17 + Q18
FACTOR4 =~ Q20 + Q21'

cfaOut.3<-sem(Q.model.3,data=data.df)
summary(cfaOut.3, fit.measures=TRUE)
parameterEstimates(cfaOut.3, standardized=TRUE)

library(dplyr) 
library(tidyr)
library(knitr)

parameterEstimates(cfaOut.3, standardized=TRUE) %>% filter(op == "~~", lhs %in% c("FACTOR1", "FACTOR2", "FACTOR3","FACTOR4"), !is.na(pvalue)) %>% mutate(stars = ifelse(pvalue < .001, "***", ifelse(pvalue < .01, "**", ifelse(pvalue < .05, "*", "")))) %>% select('Factor 1'=lhs,'Factor 2'=rhs, Correlation=est,sig=stars) %>% kable(digits = 3, format="pandoc", caption="Table: Latent Factor Correlations")


##Confirmatory Analysis - End##