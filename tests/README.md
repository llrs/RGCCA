Tests and Coverage
================
04 julio, 2018 10:46:19

-   [Coverage](#coverage)
-   [Unit Tests](#unit-tests)

This output is created by [covrpage](https://github.com/yonicd/covrpage).

Coverage
--------

Coverage summary is created using the [covr](https://github.com/r-lib/covr) package.

| Object                                      | Coverage (%) |
|:--------------------------------------------|:------------:|
| RGCCA                                       |     73.64    |
| [R/rgccak.R](../R/rgccak.R)                 |     59.89    |
| [R/sgcca.R](../R/sgcca.R)                   |     65.04    |
| [R/norm2.R](../R/norm2.R)                   |     66.67    |
| [R/sgccak.R](../R/sgccak.R)                 |     75.00    |
| [R/initsvd.R](../R/initsvd.R)               |     80.00    |
| [R/rgcca.R](../R/rgcca.R)                   |     86.51    |
| [R/scale2.R](../R/scale2.R)                 |     88.24    |
| [R/cov2.R](../R/cov2.R)                     |     90.00    |
| [R/tau.estimate.R](../R/tau.estimate.R)     |     90.91    |
| [R/BinarySearch.R](../R/BinarySearch.R)     |     91.30    |
| [R/deflation.R](../R/deflation.R)           |    100.00    |
| [R/defl.select.R](../R/defl.select.R)       |    100.00    |
| [R/miscrossprod.R](../R/miscrossprod.R)     |    100.00    |
| [R/shave.R](../R/shave.R)                   |    100.00    |
| [R/soft.R](../R/soft.R)                     |    100.00    |
| [R/soft.threshold.R](../R/soft.threshold.R) |    100.00    |

<br>

Unit Tests
----------

Unit Test summary is created using the [testthat](https://github.com/r-lib/testthat) package.

|                        | file                                                     |    n|    time|  error|  failed|  skipped|  warning|
|------------------------|:---------------------------------------------------------|----:|-------:|------:|-------:|--------:|--------:|
| test\_cov2.R           | [test\_cov2.R](testthat/test_cov2.R)                     |    1|   0.011|      0|       0|        0|        0|
| test\_rgcca.R          | [test\_rgcca.R](testthat/test_rgcca.R)                   |   53|   0.531|      0|       0|        0|        0|
| test\_scale2.R         | [test\_scale2.R](testthat/test_scale2.R)                 |    8|   0.009|      0|       0|        0|        0|
| test\_sgcca.R          | [test\_sgcca.R](testthat/test_sgcca.R)                   |   29|  17.535|      0|       0|        0|        0|
| test\_soft.threshold.R | [test\_soft.threshold.R](testthat/test_soft.threshold.R) |    1|   0.002|      0|       0|        0|        0|
| test\_tau.estimate.R   | [test\_tau.estimate.R](testthat/test_tau.estimate.R)     |    1|   0.003|      0|       0|        0|        0|

| file                                                        | context        | test                                  | status |    n|    time|
|:------------------------------------------------------------|:---------------|:--------------------------------------|:-------|----:|-------:|
| [test\_cov2.R](testthat/test_cov2.R#L5)                     | cov2           | cov2                                  | PASS   |    1|   0.011|
| [test\_rgcca.R](testthat/test_rgcca.R#L16)                  | RGCCA          | Example 1:factorial                   | PASS   |   17|   0.030|
| [test\_rgcca.R](testthat/test_rgcca.R#L59)                  | RGCCA          | Example 2:function                    | PASS   |   18|   0.483|
| [test\_rgcca.R](testthat/test_rgcca.R#L104)                 | RGCCA          | Example 3: factorial                  | PASS   |   18|   0.018|
| [test\_scale2.R](testthat/test_scale2.R#L5_L8)              | scale2         | Works without changing                | PASS   |    8|   0.009|
| [test\_sgcca.R](testthat/test_sgcca.R#L17)                  | Testing sgcca  | sgcca output does what it is expected | PASS   |   29|  17.535|
| [test\_soft.threshold.R](testthat/test_soft.threshold.R#L6) | soft.threshold | works as is                           | PASS   |    1|   0.002|
| [test\_tau.estimate.R](testthat/test_tau.estimate.R#L5)     | tau.estimate   | tau.estimate                          | PASS   |    1|   0.003|

<!--- Final Status : pass --->
