# abBootPackage

**abBootPackage** provides three bootstrap methods for constructing confidence intervals (CIs) for the difference in conversion rates in A/B testing:

- **Percentile bootstrap CI**
- **Basic bootstrap CI**
- **Bootstrap-t CI**

This package is designed for simple A/B experiments where each user either converts (1) or does not convert (0).
It demonstrates widely used bootstrap techniques in online experimentation and statistical inference.

---

## Installation

Install this package directly from GitHub:

```{r}
# install.packages("devtools")
devtools::install_github("AAEEEEEE/abBootPackage")
```

## Usage

Here is a simple example demonstrating all three bootstrap methods:

```{r}
library(abBootPackage)

nA <- 1000; yA <- 70
nB <- 1000; yB <- 95

# Percentile bootstrap CI
boot_ab_perc(nA, yA, nB, yB, B = 2000)

# Basic bootstrap CI
boot_ab_basic(nA, yA, nB, yB, B = 2000)

# Bootstrap-t CI
boot_ab_t(nA, yA, nB, yB, B = 2000)
```

## Example Result

```{r}
$pA
[1] 0.07

$pB
[1] 0.095

$diff
[1] 0.025

$ci
[1] -0.045  0.003  # Example only
```
