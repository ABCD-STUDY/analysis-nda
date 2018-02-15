## Quality control measure for imaging data

Quality control information can be used to filter the derived data. Or in some cases to test if an observed effect correlates with image quality. The specific way of filtering is left to the user as different analysis might require different approaches for censoring data.

## Participants which pass FreeSurfer QC

List participants which passed the FreeSurfer quality control check (this does not mean they are all equally good).

```r
goodFreeSurfer <- nda17$src_subject_id[which(nda17$fsqc_qc == "pass")]
goodFreeSurfer <- goodFreeSurfer[!is.na(goodFreeSurfer)]
print(paste("Yay: ", length(goodFreeSurfer), " Nay: ", length(nda17$src_subject_id[!is.na(nda17$fsqc_qc)]) - length(goodFreeSurfer), sep=""))
```

## Participants with a minimum degree of freedom

One approach is to use a threshold on the degrees of freedom to identify a percentage of participants with data of questionable quality.

```r
percentage = 0.1
threshold = as.numeric(quantile(as.numeric(nda17$mid_beta_seg_dof.x), probs=c(percentage), na.rm= TRUE))
goodTask <- nda17$src_subject_id[as.numeric(nda17$mid_beta_seg_dof.x) >= threshold]
goodTask <- goodTask[!is.na(goodTask)]
print(paste("Yay: ", length(goodTask), " Nay: ", length(nda17$src_subject_id[!is.na(nda17$mid_beta_seg_dof.x)]) - length(goodTask), sep=""))
```
