# Statistics: Learning from Data

## Examining Distributions

There are three ways to examine a distribution

1. **Plot a Graph**: This is the recommended first step to understanding a distribution. Using the appropriate graph helps visualize the data, look for patterns, detect outliers and understand the overall structure.
2. **Compute a Statistic**: While graphs are a powerful way to get started, there is a subjective element to its interpretation. Here is where a numerical summary measure of the data comes in handy. It is usually referred to as a **statistic**.
3. **Fit a Model**: 

**Outliers**: An _outlier_ is a data point that stands out from the rest of the data. There are two kinds of outliers

1. `Potential Outlier` is any observation that lies more than *1.5 x IQR* away from the quartiles.
2. `Suspected Outlier`

<div markdown = "0">\[ \alpha \]</div>
("http://placehold.it/350x150")
                                                                        
                                                  
                              

### Plot Graphs


### Compute Statistic

```r
library(ggplot2) 
qplot(tip, data = tips)
```

### Fit Density