
# _**This package has been retired. All funcitonality has been rolled into the new package ggnomics**_
## **ggideo**
**ggideo** is an R package that provides data visualizations using `ggplot2`. It offers functions to dynamically plot genomes for exploratory data analysis.

### **Installation**

You can install **ggideo** from GitHub using the following command:

```{r}
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

devtools::install_github("matthewwdavis/ggideo")
```


### **Example Usage**

Here's a simple example of how to use **ggideo** to create a plot evaluating chromosome length and telomere presence:

```{r}
library(ggideo)

# Generate the data and the plot, stored as a list
genome.plot <- ggideoprimary("path/to/fasta/file")

# Print the plot
genome.plot$ideogram

# Print the resulting table
genome.plot$genomic.table
```
