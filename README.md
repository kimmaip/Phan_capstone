## Welcome to Kimmai Phan's Capstone Project

## Task 1: Background 

I am interested in understanding how abberant epigenetic mechanisms can result in disease phenotypes in different organ systems. One project I did during my undergrad involved studying the effects of a point mutation in DNMT1, an enzyme responsible for maintaining DNA methylation patterns. If aberrant methylation occurs, gene expression goes unregulated and can result in disease or early mortality.  Individuals diagnosed with hereditary sensory autonomic neuropathy type 1E (HSAN1E) are often associated with a genetic mutation in DNMT1. However, the role of this mutation affecting methylation in HSAN1E and its neurological symptoms is not fully understood. It is suggested that neuropathies associated with HSAN1E and other degenerative neurological disorders, such as dementia, have similar epigenetic origin due to the association of aberrant DNA methylation with genes related to metabolic pathways.   


## Task 2: What do we want to look into?


It is not known whether induced pluripotent stem cells (iPSCs) from HSAN1E patients have metabolic deficiencies in their cytoplasm compared to cell lines that do not have a mutation in DNMT1. We propose to use fluorescence lifetime imaging microscopy (FLIM) to observe the fraction of bound (fB ratio) of NADH to measure metabolic activity between these different cell lines.   


## Task 3: Hypothesis  


If patient-specific iPSCs that contain this mutation in DNMT1 have impaired metabolism, then analysis of FLIM images of these cells should reveal lower cytoplasmic fB NADH ratio values, which are indicative of an increased dependence on glycolysis and metabolic dysfunction.  


## Task 4: Defining Variables  

The dependent variable that will be observed are the fraction of bound (fB) ratios of NADH, an electron carrier important in metabolic activities such as oxidative phosphorylation and glycolysis, particularly in the cytoplasm where these processes occur. The predictor variable would be cell lines: those that have the mutation in DNMT1 and ones that do not have the mutation. The predictor variable would be a sorted variable, and the dependent variable would be measured.  

## Task 5: Statistical Hypothesis  

**Null hypothesis:** There is no difference in mean cytoplasmic fB ratios between cell lines with the mutation and cell lines without the mutation.  

**Alternate hypothesis:** The mean cytoplasmic fB ratios of cell lines with the mutation is lower than the cell lines without the mutation.  

## Task 6: Defining Statistical Test  

Given that I have two levels or two explanatory groups, I can only do a T-test. As my dependent variable’s (cytoplasmic fB ratios) measurements are continuous, random, and are not intrinsically related, **I can do an independent/unpaired t-test**. I only have one dependent variable that I am testing the means of between two different groups. It would not benefit me to do a 1 sample t-test as there is no nominal standard of values for this particular measurement since fB ratios are distinct depending on cell type and cell age. MANOVA is a potential statistical test I could do if I had more dependent variables. ANOVA is also a potential statistical test I could do if I had more independent/explanatory variables.   

## Task 7: List procedures and rules for experimental unit selection  

For this experiment, I have two main groups of immortalized cell lines. One main group will be cell lines that contain the patient-specific mutation in DNMT1. In this group I have 3 biological/independent replicates.The other group which does not have the patient-specific mutation comprises 3 biological replicates. The experimental unit itself is the fB ratio that is obtained after doing SimFCS analysis on cytoplasmic regions of cells in FLIM images acquired of all the independent replicates. As the cell lines are not intrinsically related, it can be thought that each time a FLIM image is taken of a cell line, it is completely random and independent from images taken of different cell lines.

## Task 8: Simulate and Graph expected results

```
library(tidyverse)
library(dplyr)
library(ggplot2)
```

```
#initialize
n<-3 # number of independent replicates per group
b<-0.7 #expected fB value in normal metabolism
a<-0.5 #expected fB value in impaired metabolism
sd<-0.1 #expected standard deviation of Outcome variable

simulate_lfd<- function(n, b, a, sd) { 
  
id<-1:(n*2)
mutant<-rep(c("Y", "N"), each=n)
y<-rnorm(n, a, sd) #yes mutation
n<-rnorm(n, b, sd) #no mutation
cyto<-c(y, n) #munge

data<-data.frame (id=id, mut=mutant, cyto=cyto)
    }

flim<- simulate_lfd(n,b,a,sd)

#graph

ggplot(flim, aes(mut, cyto))+
  geom_jitter(width=0.1, color="purple")+
  labs(title= "Metabolic Activity in Cells with a Point Mutation in DNMT1", x="Mutation Present", y= "Fraction of Bound NADH")
```
![Image](src)

## Task 9: Write and Perform Monte Carlo Analysis 

```

n<-6 # number of independent replicates per group
b<-0.7 #expected fB value in normal metabolism
a<-0.45 #expected fB value in impaired metabolism
sd<-0.15 #expected standard deviation of Outcome variable
sims<-100


#creating a covariance matrix
#estimate correlation coefficients between cyto and nuc to  
pval <- replicate(
  sims, {
        sample<-simulate_lfd(n, b, a, sd)
        
          ans<-t.test(sample$cyto[1:n],sample$cyto[(n+1):(n*2)], 
            alternative = "less", 
            paired = F, 
            var.equal = F, 
            conf.level = 0.95) #perform the t-test
  
  pval<-ans$p.value #get the p-value and store it

    
    }
  )

pwr.pct <- sum(pval<0.05)/sims*100
paste(pwr.pct, sep="", "% power. Change 'n' in your initializer for higher or lower power. ")
```
![Image](src)

To have a 12% risk of obtaining a type II error, given the parameters, I would need at least an n of 6 independent replicates for each of my explantory groups.

