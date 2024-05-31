# meta-measures
Calculate measures of metacognitive monitoring for 2AFC tasks

HMeta-d-master folder should be download and included in the working directory.

M-ratio can be calculated as below (and the distribution of the m-ratio adjusted):

```
# meta ratio
mydata$meta_efficiency <- mydata$sensitivity_hier/mydata$da

# bounded meta ratio
mydata$bounded_meta_efficiency <- ifelse(mydata$meta_efficiency > 2*median(mydata$meta_efficiency, na.rm = T), 2*median(mydata$meta_efficiency, na.rm = T), mydata$meta_efficiency)
mydata$bounded_meta_efficiency <- ifelse(mydata$meta_efficiency < 0, 0, mydata$bounded_meta_efficiency)

# log ratio
mydata$log_meta_efficiency <- ifelse(mydata$meta_efficiency < 0.1, 0.1, mydata$meta_efficiency )
mydata$log_meta_efficiency <- log(mydata$log_meta_efficiency)

# bounded meta d'
mydata$bounded_meta_sensitivity <- ifelse(mydata$sensitivity_sse > 2*median(mydata$sensitivity_sse, na.rm = T), 2*median(mydata$sensitivity_sse, na.rm = T), mydata$sensitivity_sse)
mydata$bounded_meta_sensitivity <- ifelse(mydata$sensitivity_sse < 0, 0, mydata$bounded_meta_sensitivity)
```
