<!-- README.md is generated from README.Rmd. Please edit that file -->



# MCHT

*Version 0.0.0.9000*

**MCHT** is a package implementing an interface for creating and using Monte
Carlo tests. The primary function of the package is `MCHTest()`, which creates
functions with S3 class `MCHTest` that perform a Monte Carlo test.

## Installation

**MCHT** is not presently available on CRAN. You can download and install 
**MCHT** from GitHub using [**devtools**](https://github.com/r-lib/devtools) via
the R command `devtools::install_github("ntguardian/MCHT")`.

## Monte Carlo Hypothesis Testing

Monte Carlo testing is a form of hypothesis testing where the <img src="svgs/2ec6e630f199f589a2402fdf3e0289d5.svg" align=middle width=8.270567249999992pt height=14.15524440000002pt/>-values are
computed using the empirical distribution of the test statistic computed from
data simulated under the null hypothesis. These tests are used when the
distribution of the test statistic under the null hypothesis is intractable or
difficult to compute, or as an exact test (that is, a test where the
distribution used to compute <img src="svgs/2ec6e630f199f589a2402fdf3e0289d5.svg" align=middle width=8.270567249999992pt height=14.15524440000002pt/>-values is appropriate for any sample size, not
just large sample sizes).

Suppose that <img src="svgs/aabe1517ce1102595512b736cbf264bb.svg" align=middle width=15.831502499999988pt height=14.15524440000002pt/> is the observed value of the test statistic and large values
of <img src="svgs/aabe1517ce1102595512b736cbf264bb.svg" align=middle width=15.831502499999988pt height=14.15524440000002pt/> are evidence against the null hypothesis; normally, <img src="svgs/2ec6e630f199f589a2402fdf3e0289d5.svg" align=middle width=8.270567249999992pt height=14.15524440000002pt/>-values would be
computed as <img src="svgs/94e4cf2543ecdf13ca181360434546e6.svg" align=middle width=70.60317329999998pt height=24.65753399999998pt/>, where <img src="svgs/1ef429296c00d0d4dc9914bfb2f6ec6f.svg" align=middle width=147.44866949999997pt height=24.65753399999998pt/> is the cumulative
distribution functions and <img src="svgs/49aebd2501b0bf3a5225ca26ba123672.svg" align=middle width=18.205948199999987pt height=22.465723500000017pt/> is the random variable version of <img src="svgs/aabe1517ce1102595512b736cbf264bb.svg" align=middle width=15.831502499999988pt height=14.15524440000002pt/>. We
cannot use <img src="svgs/b8bc815b5e9d5177af01fd4d3d3c2f10.svg" align=middle width=12.85392569999999pt height=22.465723500000017pt/> for some reason; it's intractable, or the <img src="svgs/b8bc815b5e9d5177af01fd4d3d3c2f10.svg" align=middle width=12.85392569999999pt height=22.465723500000017pt/> provided is only
appropriate for large sample sizes.

Instead of using <img src="svgs/b8bc815b5e9d5177af01fd4d3d3c2f10.svg" align=middle width=12.85392569999999pt height=22.465723500000017pt/> we will use <img src="svgs/15c3c9c70eb47be5a6e886765530f5d7.svg" align=middle width=22.21695959999999pt height=31.141535699999984pt/>, which is the empirical CDF of
the same test statistic computed from simulated data following the distribution
prescribed by the null hypothesis of the test. For the sake of simplicity in
this presentation, assume that <img src="svgs/e257acd1ccbe7fcb654708f1a866bfe9.svg" align=middle width=11.027402099999989pt height=22.465723500000017pt/> is a continuous random variable. Now our
<img src="svgs/2ec6e630f199f589a2402fdf3e0289d5.svg" align=middle width=8.270567249999992pt height=14.15524440000002pt/>-value is <img src="svgs/422c7ea56f597c35a6c675a532225eb9.svg" align=middle width=80.78808375pt height=31.141535699999984pt/>, where <img src="svgs/58cde395c6c761b95ba2dcfde3b9699e.svg" align=middle width=211.48993635000002pt height=32.256008400000006pt/> and <img src="svgs/bc3c694d37b92361e3102381d7c007e6.svg" align=middle width=28.21459079999999pt height=30.267491100000004pt/> is an
independent random copy of <img src="svgs/49aebd2501b0bf3a5225ca26ba123672.svg" align=middle width=18.205948199999987pt height=22.465723500000017pt/> computed from simulated data with a sample size
of <img src="svgs/55a049b8f161ae7cfeb0197d75aff967.svg" align=middle width=9.86687624999999pt height=14.15524440000002pt/>.

The power of these tests increase with <img src="svgs/f9c4988898e7f532b9f826a75014ed3c.svg" align=middle width=14.99998994999999pt height=22.465723500000017pt/> (see [1]) but modern computers are
able to simulate large <img src="svgs/f9c4988898e7f532b9f826a75014ed3c.svg" align=middle width=14.99998994999999pt height=22.465723500000017pt/> quickly, so this is rarely an issue. The procedure
above also assumes that there are no nuisance parameters and the distribution of
<img src="svgs/49aebd2501b0bf3a5225ca26ba123672.svg" align=middle width=18.205948199999987pt height=22.465723500000017pt/> can effectively be known precisely when the null hypothesis is true (and
all other conditions of the test are met, such as distributional assumptions). A
different procedure needs to be applied when nuisance parameters are not
explicitly stated under the null hypothesis. [2] suggests a procedure using
optimization techniques (recommending simulated annealing specifically) to
adversarially select values for nuisance parameters valid under the null
hypothesis that maximize the <img src="svgs/2ec6e630f199f589a2402fdf3e0289d5.svg" align=middle width=8.270567249999992pt height=14.15524440000002pt/>-value computed from the simulated data. This
procedure is often called *maximized Monte Carlo* (MMC) testing. That is the
procedure employed here. (In fact, the tests created by `MCHTest()` are the
tests described in [2].) Unfortunately, MMC, while conservative and exact, has
much less power than if the unknown parameters were known, perhaps due to the
behavior of samples under distributions with parameter values distant from the
true parameter values (see [3]).

## Bootstrap Hypothesis Testing

Bootstrap statistical testing is very similar to Monte Carlo testing; the key
difference is that bootstrap testing uses information from the sample. For
example a parametric bootstrap test would estimate the parameters of the
distribution the data is assumed to follow and generate datasets from that
distribution using those estimates as the actual parameter values. A permutation
test (like Fisher's permutation test; see [4]) would use the original dataset
values but randomly shuffle the labeles (stating which sample an observation
belongs to) to generate new data sets and thus new simulated test statistics.
<img src="svgs/2ec6e630f199f589a2402fdf3e0289d5.svg" align=middle width=8.270567249999992pt height=14.15524440000002pt/>-values are essentially computed the same way.

Unlike Monte Carlo tests and MMC, these tests are not exact tests. That said,
they often have good finite sample properties. (See [3].)

See the documentation for more details and references.

## Examples

`MCHTest()` is the main function of the package and can create functions with S3
class `MCHTest` that perform Monte Carlo hypothesis tests.

For example, the code below creates the Monte Carlo equivalent of a <img src="svgs/4f4f4e395762a3af4575de74c019ebb5.svg" align=middle width=5.936097749999991pt height=20.221802699999984pt/>-test.


```r
library(MCHT)
#> .------..------..------..------.
#> |M.--. ||C.--. ||H.--. ||T.--. |
#> | (\/) || :/\: || :/\: || :/\: |
#> | :\/: || :\/: || (__) || (__) |
#> | '--'M|| '--'C|| '--'H|| '--'T|
#> `------'`------'`------'`------' v. 0.0.0.9000
#> Type citation("MCHT") for citing this R package in publications
library(doParallel)
#> Loading required package: foreach
#> Loading required package: iterators
#> Loading required package: parallel

registerDoParallel(detectCores())  # Necessary for parallelization, and if not
                                   # done the resulting function will complain
                                   # on the first use

ts <- function(x, mu = 0) {sqrt(length(x)) * (mean(x) - mu)/sd(x)}
sg <- function(x, mu = 0) {
  x <- x + mu
  ts(x)
}
rg <- rnorm

mc.t.test <- MCHTest(ts, sg, rg, seed = 20181001, test_params = "mu", 
                     lock_alternative = FALSE,
                     method = "Monte Carlo One Sample t-Test")
```

The object `mc.t.test()` is an S3 class, and a callable function.


```r
class(mc.t.test)
#> [1] "MCHTest"
```

`print()` will print relevant information about the construction of the test.


```r
mc.t.test
#> 
#> 	Details for Monte Carlo One Sample t-Test
#> 
#> Seed:  20181001 
#> Replications:  10000 
#> Tested Parameters:  mu 
#> Default mu:  0 
#> 
#> Memoisation enabled
```

Once this object is created, we can use it for performing hypothesis tests.


```r
dat <- c(2.3, -0.13, 1.42, 1.51, 3.43, -0.96, 0.59, 0.62, 1.28, 4.07)

t.test(dat, mu = 1, alternative = "two.sided")  # For reference
#> 
#> 	One Sample t-test
#> 
#> data:  dat
#> t = 0.84975, df = 9, p-value = 0.4175
#> alternative hypothesis: true mean is not equal to 1
#> 95 percent confidence interval:
#>  0.3135303 2.5124697
#> sample estimates:
#> mean of x 
#>     1.413
mc.t.test(dat, mu = 1)
#> Loading required package: rngtools
#> Loading required package: pkgmaker
#> Loading required package: registry
#> 
#> Attaching package: 'pkgmaker'
#> The following object is masked from 'package:base':
#> 
#>     isFALSE
#> 
#> 	Monte Carlo One Sample t-Test
#> 
#> data:  dat
#> S = 0.84975, p-value = 0.9885
mc.t.test(dat, mu = 1, alternative = "two.sided")
#> 
#> 	Monte Carlo One Sample t-Test
#> 
#> data:  dat
#> S = 0.84975, p-value = 0.023
#> alternative hypothesis: true mu is not equal to 1
```

This is the simplest example; `MCHTest()` can create more involved Monte Carlo
tests. See other documentation for details.

## Planned Future Features

* Permutation tests are a form of Monte Carlo test; perhaps we could add a class
  for constructing permutation tests like we did for Monte Carlo tests.
* Bootstrap tests are yet another form of Monte Carlo tests; we could give some
  constructor for making bootstrap tests too.

## References

1. A. C. A. Hope, *A simplified Monte Carlo test procedure*, JRSSB, vol. 30
   (1968) pp. 582-598
2. J-M Dufour, *Monte Carlo tests with nuisance parameters: A general approach
   to finite-sample inference and nonstandard asymptotics*, Journal of
   Econometrics, vol. 133 no. 2 (2006) pp. 443-477
3. J. G. MacKinnon, *Bootstrap hypothesis testing* in *Handbook of computational
   econometrics* (2009) pp. 183-213
4. R. A. Fisher, *The design of experiments* (1935)
