# MCHT News

## Version 0.2.0
---

### Functions

- Created `stat_t()`, the test statistic for the t-test
- Created `stat_t_two_sample()`, the test statistic for Welch's two-sample
    t-test

## Version 0.1.0
---

- Created `MCHTest()`, which creates `MCHTest`-class objects that handle a
    particular Monte Carlo or bootstrap test
- Created `pval()`, a function for computing p-values for tests based on
    empirical distributions, and supports breaking ties via generating uniformly
    distributed random variables
- Create a print method for `MCHTest`-class objects that prints information
    about the parameters of the object
- Created `is.MCHTest()` function to check whether an object inherits from the
    `MCHTest` class
- Created `get_MCHTest_settings()` which creates a list with all objects that
    together uniquely characterize an `MCHTest`-class object

## Version 0.0.0.9000
---

### Setup

- Package **MCHT** created.
