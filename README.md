- ***This project is no longer maintained.***
- ***This project on empirical asset pricing is outdated and uses methods that were more popular in 90s-10s.***

# port(folio)R

version 0.1.1

R CMD check: 0 errors ✓ \| 0 warnings ✓ \| 0 note ✓

## Overview

portR is a series of functions designed to help us conduct empirical asset pricing research. Almost all functions caring about the cross-sectional relationship between variables in a data.table:

-   `crs_corr()` Cross-sectional Pearson and Spearman correlation
-   `crs_pers()` Cross-sectional variable persistence
-   `crs_sum()` Robust cross-sectional descriptive statistics
-   `crs_port()` Univariate portfolio analysis
-   `crs_port2()` Bivariate portfolio analysis
-   `crs_reg()` Fama-MacBeth regression with Newey-West adjustment

Besides, portR provides two functions for rolling calculation by group, with quite high speed:

-   `roll_f1()` Group-rolling apply
-   `roll_reg()` Group-rolling regression and apply function to the lm.fit result

And there are some useful data processing tricks, like:

-   `dropnas()` Drop NAs in the specified variable, can also be used to select sub-columns
-   `winsor()` Winsorization and truncation on a vector
-   `%ym-%` A shorthand to shift YYYY-MM date

## Installation

Currently, portR can only be installed via GitHub, although this is a little bit troublesome.

```{r, eval = FALSE}
# install.packages("devtools")
devtools::install_github("abestwyc/portR")
```

It is worth mentioning that portR have passed the `R CMD Check` with 0 errors, 0 warnings and 0 notes, I believe that you can install it directly on CRAN soon.

## Warning

Before using portR, please check the variable names in your data, your variable names cannot be the same as the parameter name!!! If the variable name is the same as parameter name, the result may be wrong or unpredictable.

The reason why this warning exists is that portR is written almost entirely using `data.table`, which contains a lot of non-standard operations. And the non-standard operations will have unpredictable side effects on the lexical scope and environment.

## Usage

```{r, message = FALSE}
library(portR)

crs_corr(stock, "month", Use = c("return", "size", "explain"))

crs_pers(stock, "month", "stkcd", c("return", "size"), 3:9)

crs_sum(stock, Use = c("return", "size", "explain"), Date = "month")

crs_port(stock, "return", "explain", "month", "size", c("factor1", "factor2"), T, 5, F)

crs_port2(stock, "return", "size", "explain", "month", NULL, NULL, F, F, c(4, 5))

crs_reg(stock, "return", c("size", "explain"), "month")

```

## Getting help

If you encounter a clear bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/abestwyc/portR/issues).

For questions and other discussion, just email me.

* email: [weiyc21\@mails.tsinghua.edu.cn](mailto:weiyc21@mails.tsinghua.edu.cn) 
