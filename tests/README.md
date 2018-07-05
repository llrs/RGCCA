Tests and Coverage
================
05 julio, 2018 10:17:16

-   [Coverage](#coverage)
-   [Unit Tests](#unit-tests)

This output is created by [covrpage](https://github.com/yonicd/covrpage).

Coverage
--------

Coverage summary is created using the [covr](https://github.com/r-lib/covr) package.

| Object                                      | Coverage (%) |
|:--------------------------------------------|:------------:|
| RGCCA                                       |     75.41    |
| [R/rgccak.R](../R/rgccak.R)                 |     64.77    |
| [R/sgcca.R](../R/sgcca.R)                   |     65.04    |
| [R/norm2.R](../R/norm2.R)                   |     66.67    |
| [R/sgccak.R](../R/sgccak.R)                 |     77.38    |
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
| test\_cov2.R           | [test\_cov2.R](testthat/test_cov2.R)                     |    1|   0.010|      0|       0|        0|        0|
| test\_rgcca.R          | [test\_rgcca.R](testthat/test_rgcca.R)                   |   71|   0.489|      0|       0|        0|        0|
| test\_scale2.R         | [test\_scale2.R](testthat/test_scale2.R)                 |    8|   0.009|      0|       0|        0|        0|
| test\_sgcca.R          | [test\_sgcca.R](testthat/test_sgcca.R)                   |   96|  25.189|      0|       0|        0|        0|
| test\_soft.threshold.R | [test\_soft.threshold.R](testthat/test_soft.threshold.R) |    1|   0.002|      0|       0|        0|        0|
| test\_tau.estimate.R   | [test\_tau.estimate.R](testthat/test_tau.estimate.R)     |    1|   0.002|      0|       0|        0|        0|

| file                                                        | context        | test                   | status |    n|   time|
|:------------------------------------------------------------|:---------------|:-----------------------|:-------|----:|------:|
| [test\_cov2.R](testthat/test_cov2.R#L5)                     | cov2           | cov2                   | PASS   |    1|  0.010|
| [test\_rgcca.R](testthat/test_rgcca.R#L16)                  | RGCCA          | Example 1:factorial    | PASS   |   17|  0.033|
| [test\_rgcca.R](testthat/test_rgcca.R#L59)                  | RGCCA          | Example 2: function    | PASS   |   18|  0.413|
| [test\_rgcca.R](testthat/test_rgcca.R#L104)                 | RGCCA          | Example 3: factorial   | PASS   |   18|  0.020|
| [test\_rgcca.R](testthat/test_rgcca.R#L144)                 | RGCCA          | Example 3: horst       | PASS   |   18|  0.023|
| [test\_scale2.R](testthat/test_scale2.R#L5_L8)              | scale2         | Works without changing | PASS   |    8|  0.009|
| [test\_sgcca.R](testthat/test_sgcca.R#L17)                  | Testing sgcca  | centroid               | PASS   |   32|  8.613|
| [test\_sgcca.R](testthat/test_sgcca.R#L86)                  | Testing sgcca  | horst                  | PASS   |   32|  8.261|
| [test\_sgcca.R](testthat/test_sgcca.R#L151)                 | Testing sgcca  | factorial              | PASS   |   32|  8.315|
| [test\_soft.threshold.R](testthat/test_soft.threshold.R#L6) | soft.threshold | works as is            | PASS   |    1|  0.002|
| [test\_tau.estimate.R](testthat/test_tau.estimate.R#L5)     | tau.estimate   | tau.estimate           | PASS   |    1|  0.002|

<!--- Final Status : pass --->
