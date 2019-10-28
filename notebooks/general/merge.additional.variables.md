Read in data from output of categorical_extension

```r
script.dir <- "~/Desktop/ABCD/analysis-nda18/notebooks/general/"
setwd(script.dir)

nda18.ext=readRDS("nda2.0.1_ext.Rds")


Assume data of family history of problem summary scores and neurocog principal component scores are ready. Details see rds release notes of 2.0.1

#merge nda18.ext, pc scores and dat.prob
#dat.prob:Family history of problem summary scores
#npc: neurocog principal component scores

new.vars=merge(dat.prob,npc.dat,by=c("src_subject_id","eventname"),all.x=T)
```

BOCF under new name

```r
colnames(new.vars)[-c(1,2)]=paste0(colnames(new.vars)[-c(1,2)],".bl")
new.vars=new.vars[,-2]


nda18.1=merge(nda18.ext,new.vars,by=c("src_subject_id"),all.x=T)
dim(nda18.1)
```
```r
#################################################################################
#for internal DEAP development only
# nda18.1$event_name=nda18.1$eventname
# nda18.1$interview_datetime=nda18.1$interview_date
# nda18.1$sex_at_birth=nda18.1$sex 
#################################################################################
```
Merge with Genetic_Ancestry_Factors.4 (assume GAF.dat is reday). GAF variables are considered invariant overtime (BOCF) directly under the variable name;

```r
nda18.1=merge(nda18.1,GAF.dat,by="subjectid",all.x=T)
```

Merge with zigosity data (assume zyg.dat is reday). Currently only avaialbe between twins with consistent rel_family_id and 
rel_group_id (excluding triplets). zyg data are considered invariant overtime (BOCF) directly under the variable name;

```r
nda18=merge(nda18.1,zyg.dat,by="subjectid",all.x=T)

ind=which(nda18$rel_relationship=="twin" & is.na(nda18$PI_HAT))
nda18$Zygosity[ind]="missing" #twin, but PI_HAT wasn't computed (details see RDS release notes)
nda18$Zygosity=as.factor(nda18$Zygosity)

dim(nda18)

saveRDS(nda18, "nda2.0.1.Rds")
```

Done.
