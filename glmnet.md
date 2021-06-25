Lasso with Tidymodels(kaggle competition)
================

Let’s apply Lasso model to the data.

# 1 Preparations (준비작업) {.tabset . tabset-fade}

## 1.1 Libraries

``` r
library(tidymodels)
library(tidyverse)
library(magrittr)
library(skimr)
library(knitr)
theme_set(theme_bw())
```

## 1.2 Data load

``` r
file_path <- "../input/house-prices-advanced-regression-techniques/"
files <- list.files(file_path)
files
```

    ## [1] "data_description.txt"  "sample_submission.csv" "test.csv"             
    ## [4] "train.csv"

``` r
train <- read_csv(file.path(file_path, "train.csv"))
test <- read_csv(file.path(file_path, "test.csv"))
```

``` r
train %>% dim()
```

    ## [1] 1460   81

``` r
test %>% dim()
```

    ## [1] 1459   80

``` r
"SalePrice" %in% names(test)
```

    ## [1] FALSE

# 2 Detailed info. `train`

``` r
skim(train)
```

|                                                  |       |
|:-------------------------------------------------|:------|
| Name                                             | train |
| Number of rows                                   | 1460  |
| Number of columns                                | 81    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |       |
| Column type frequency:                           |       |
| character                                        | 43    |
| numeric                                          | 38    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |       |
| Group variables                                  | None  |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:---------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| MSZoning       |          0 |           1.00 |   2 |   7 |     0 |         5 |          0 |
| Street         |          0 |           1.00 |   4 |   4 |     0 |         2 |          0 |
| Alley          |       1369 |           0.06 |   4 |   4 |     0 |         2 |          0 |
| LotShape       |          0 |           1.00 |   3 |   3 |     0 |         4 |          0 |
| LandContour    |          0 |           1.00 |   3 |   3 |     0 |         4 |          0 |
| Utilities      |          0 |           1.00 |   6 |   6 |     0 |         2 |          0 |
| LotConfig      |          0 |           1.00 |   3 |   7 |     0 |         5 |          0 |
| LandSlope      |          0 |           1.00 |   3 |   3 |     0 |         3 |          0 |
| Neighborhood   |          0 |           1.00 |   5 |   7 |     0 |        25 |          0 |
| Condition1     |          0 |           1.00 |   4 |   6 |     0 |         9 |          0 |
| Condition2     |          0 |           1.00 |   4 |   6 |     0 |         8 |          0 |
| BldgType       |          0 |           1.00 |   4 |   6 |     0 |         5 |          0 |
| HouseStyle     |          0 |           1.00 |   4 |   6 |     0 |         8 |          0 |
| RoofStyle      |          0 |           1.00 |   3 |   7 |     0 |         6 |          0 |
| RoofMatl       |          0 |           1.00 |   4 |   7 |     0 |         8 |          0 |
| Exterior1st    |          0 |           1.00 |   5 |   7 |     0 |        15 |          0 |
| Exterior2nd    |          0 |           1.00 |   5 |   7 |     0 |        16 |          0 |
| MasVnrType     |          8 |           0.99 |   4 |   7 |     0 |         4 |          0 |
| ExterQual      |          0 |           1.00 |   2 |   2 |     0 |         4 |          0 |
| ExterCond      |          0 |           1.00 |   2 |   2 |     0 |         5 |          0 |
| Foundation     |          0 |           1.00 |   4 |   6 |     0 |         6 |          0 |
| BsmtQual       |         37 |           0.97 |   2 |   2 |     0 |         4 |          0 |
| BsmtCond       |         37 |           0.97 |   2 |   2 |     0 |         4 |          0 |
| BsmtExposure   |         38 |           0.97 |   2 |   2 |     0 |         4 |          0 |
| BsmtFinType1   |         37 |           0.97 |   3 |   3 |     0 |         6 |          0 |
| BsmtFinType2   |         38 |           0.97 |   3 |   3 |     0 |         6 |          0 |
| Heating        |          0 |           1.00 |   4 |   5 |     0 |         6 |          0 |
| HeatingQC      |          0 |           1.00 |   2 |   2 |     0 |         5 |          0 |
| CentralAir     |          0 |           1.00 |   1 |   1 |     0 |         2 |          0 |
| Electrical     |          1 |           1.00 |   3 |   5 |     0 |         5 |          0 |
| KitchenQual    |          0 |           1.00 |   2 |   2 |     0 |         4 |          0 |
| Functional     |          0 |           1.00 |   3 |   4 |     0 |         7 |          0 |
| FireplaceQu    |        690 |           0.53 |   2 |   2 |     0 |         5 |          0 |
| GarageType     |         81 |           0.94 |   6 |   7 |     0 |         6 |          0 |
| GarageFinish   |         81 |           0.94 |   3 |   3 |     0 |         3 |          0 |
| GarageQual     |         81 |           0.94 |   2 |   2 |     0 |         5 |          0 |
| GarageCond     |         81 |           0.94 |   2 |   2 |     0 |         5 |          0 |
| PavedDrive     |          0 |           1.00 |   1 |   1 |     0 |         3 |          0 |
| PoolQC         |       1453 |           0.00 |   2 |   2 |     0 |         3 |          0 |
| Fence          |       1179 |           0.19 |   4 |   5 |     0 |         4 |          0 |
| MiscFeature    |       1406 |           0.04 |   4 |   4 |     0 |         4 |          0 |
| SaleType       |          0 |           1.00 |   2 |   5 |     0 |         9 |          0 |
| SaleCondition  |          0 |           1.00 |   6 |   7 |     0 |         6 |          0 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |      mean |       sd |    p0 |       p25 |      p50 |       p75 |   p100 | hist  |
|:---------------|-----------:|---------------:|----------:|---------:|------:|----------:|---------:|----------:|-------:|:------|
| Id             |          0 |           1.00 |    730.50 |   421.61 |     1 |    365.75 |    730.5 |   1095.25 |   1460 | ▇▇▇▇▇ |
| MSSubClass     |          0 |           1.00 |     56.90 |    42.30 |    20 |     20.00 |     50.0 |     70.00 |    190 | ▇▅▂▁▁ |
| LotFrontage    |        259 |           0.82 |     70.05 |    24.28 |    21 |     59.00 |     69.0 |     80.00 |    313 | ▇▃▁▁▁ |
| LotArea        |          0 |           1.00 |  10516.83 |  9981.26 |  1300 |   7553.50 |   9478.5 |  11601.50 | 215245 | ▇▁▁▁▁ |
| OverallQual    |          0 |           1.00 |      6.10 |     1.38 |     1 |      5.00 |      6.0 |      7.00 |     10 | ▁▂▇▅▁ |
| OverallCond    |          0 |           1.00 |      5.58 |     1.11 |     1 |      5.00 |      5.0 |      6.00 |      9 | ▁▁▇▅▁ |
| YearBuilt      |          0 |           1.00 |   1971.27 |    30.20 |  1872 |   1954.00 |   1973.0 |   2000.00 |   2010 | ▁▂▃▆▇ |
| YearRemodAdd   |          0 |           1.00 |   1984.87 |    20.65 |  1950 |   1967.00 |   1994.0 |   2004.00 |   2010 | ▅▂▂▃▇ |
| MasVnrArea     |          8 |           0.99 |    103.69 |   181.07 |     0 |      0.00 |      0.0 |    166.00 |   1600 | ▇▁▁▁▁ |
| BsmtFinSF1     |          0 |           1.00 |    443.64 |   456.10 |     0 |      0.00 |    383.5 |    712.25 |   5644 | ▇▁▁▁▁ |
| BsmtFinSF2     |          0 |           1.00 |     46.55 |   161.32 |     0 |      0.00 |      0.0 |      0.00 |   1474 | ▇▁▁▁▁ |
| BsmtUnfSF      |          0 |           1.00 |    567.24 |   441.87 |     0 |    223.00 |    477.5 |    808.00 |   2336 | ▇▅▂▁▁ |
| TotalBsmtSF    |          0 |           1.00 |   1057.43 |   438.71 |     0 |    795.75 |    991.5 |   1298.25 |   6110 | ▇▃▁▁▁ |
| 1stFlrSF       |          0 |           1.00 |   1162.63 |   386.59 |   334 |    882.00 |   1087.0 |   1391.25 |   4692 | ▇▅▁▁▁ |
| 2ndFlrSF       |          0 |           1.00 |    346.99 |   436.53 |     0 |      0.00 |      0.0 |    728.00 |   2065 | ▇▃▂▁▁ |
| LowQualFinSF   |          0 |           1.00 |      5.84 |    48.62 |     0 |      0.00 |      0.0 |      0.00 |    572 | ▇▁▁▁▁ |
| GrLivArea      |          0 |           1.00 |   1515.46 |   525.48 |   334 |   1129.50 |   1464.0 |   1776.75 |   5642 | ▇▇▁▁▁ |
| BsmtFullBath   |          0 |           1.00 |      0.43 |     0.52 |     0 |      0.00 |      0.0 |      1.00 |      3 | ▇▆▁▁▁ |
| BsmtHalfBath   |          0 |           1.00 |      0.06 |     0.24 |     0 |      0.00 |      0.0 |      0.00 |      2 | ▇▁▁▁▁ |
| FullBath       |          0 |           1.00 |      1.57 |     0.55 |     0 |      1.00 |      2.0 |      2.00 |      3 | ▁▇▁▇▁ |
| HalfBath       |          0 |           1.00 |      0.38 |     0.50 |     0 |      0.00 |      0.0 |      1.00 |      2 | ▇▁▅▁▁ |
| BedroomAbvGr   |          0 |           1.00 |      2.87 |     0.82 |     0 |      2.00 |      3.0 |      3.00 |      8 | ▁▇▂▁▁ |
| KitchenAbvGr   |          0 |           1.00 |      1.05 |     0.22 |     0 |      1.00 |      1.0 |      1.00 |      3 | ▁▇▁▁▁ |
| TotRmsAbvGrd   |          0 |           1.00 |      6.52 |     1.63 |     2 |      5.00 |      6.0 |      7.00 |     14 | ▂▇▇▁▁ |
| Fireplaces     |          0 |           1.00 |      0.61 |     0.64 |     0 |      0.00 |      1.0 |      1.00 |      3 | ▇▇▁▁▁ |
| GarageYrBlt    |         81 |           0.94 |   1978.51 |    24.69 |  1900 |   1961.00 |   1980.0 |   2002.00 |   2010 | ▁▁▅▅▇ |
| GarageCars     |          0 |           1.00 |      1.77 |     0.75 |     0 |      1.00 |      2.0 |      2.00 |      4 | ▁▃▇▂▁ |
| GarageArea     |          0 |           1.00 |    472.98 |   213.80 |     0 |    334.50 |    480.0 |    576.00 |   1418 | ▂▇▃▁▁ |
| WoodDeckSF     |          0 |           1.00 |     94.24 |   125.34 |     0 |      0.00 |      0.0 |    168.00 |    857 | ▇▂▁▁▁ |
| OpenPorchSF    |          0 |           1.00 |     46.66 |    66.26 |     0 |      0.00 |     25.0 |     68.00 |    547 | ▇▁▁▁▁ |
| EnclosedPorch  |          0 |           1.00 |     21.95 |    61.12 |     0 |      0.00 |      0.0 |      0.00 |    552 | ▇▁▁▁▁ |
| 3SsnPorch      |          0 |           1.00 |      3.41 |    29.32 |     0 |      0.00 |      0.0 |      0.00 |    508 | ▇▁▁▁▁ |
| ScreenPorch    |          0 |           1.00 |     15.06 |    55.76 |     0 |      0.00 |      0.0 |      0.00 |    480 | ▇▁▁▁▁ |
| PoolArea       |          0 |           1.00 |      2.76 |    40.18 |     0 |      0.00 |      0.0 |      0.00 |    738 | ▇▁▁▁▁ |
| MiscVal        |          0 |           1.00 |     43.49 |   496.12 |     0 |      0.00 |      0.0 |      0.00 |  15500 | ▇▁▁▁▁ |
| MoSold         |          0 |           1.00 |      6.32 |     2.70 |     1 |      5.00 |      6.0 |      8.00 |     12 | ▃▆▇▃▃ |
| YrSold         |          0 |           1.00 |   2007.82 |     1.33 |  2006 |   2007.00 |   2008.0 |   2009.00 |   2010 | ▇▇▇▇▅ |
| SalePrice      |          0 |           1.00 | 180921.20 | 79442.50 | 34900 | 129975.00 | 163000.0 | 214000.00 | 755000 | ▇▅▁▁▁ |

# 3 Detailed info. `test`

``` r
skim(test)
```

|                                                  |      |
|:-------------------------------------------------|:-----|
| Name                                             | test |
| Number of rows                                   | 1459 |
| Number of columns                                | 80   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| character                                        | 43   |
| numeric                                          | 37   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:---------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| MSZoning       |          4 |           1.00 |   2 |   7 |     0 |         5 |          0 |
| Street         |          0 |           1.00 |   4 |   4 |     0 |         2 |          0 |
| Alley          |       1352 |           0.07 |   4 |   4 |     0 |         2 |          0 |
| LotShape       |          0 |           1.00 |   3 |   3 |     0 |         4 |          0 |
| LandContour    |          0 |           1.00 |   3 |   3 |     0 |         4 |          0 |
| Utilities      |          2 |           1.00 |   6 |   6 |     0 |         1 |          0 |
| LotConfig      |          0 |           1.00 |   3 |   7 |     0 |         5 |          0 |
| LandSlope      |          0 |           1.00 |   3 |   3 |     0 |         3 |          0 |
| Neighborhood   |          0 |           1.00 |   5 |   7 |     0 |        25 |          0 |
| Condition1     |          0 |           1.00 |   4 |   6 |     0 |         9 |          0 |
| Condition2     |          0 |           1.00 |   4 |   6 |     0 |         5 |          0 |
| BldgType       |          0 |           1.00 |   4 |   6 |     0 |         5 |          0 |
| HouseStyle     |          0 |           1.00 |   4 |   6 |     0 |         7 |          0 |
| RoofStyle      |          0 |           1.00 |   3 |   7 |     0 |         6 |          0 |
| RoofMatl       |          0 |           1.00 |   7 |   7 |     0 |         4 |          0 |
| Exterior1st    |          1 |           1.00 |   6 |   7 |     0 |        13 |          0 |
| Exterior2nd    |          1 |           1.00 |   5 |   7 |     0 |        15 |          0 |
| MasVnrType     |         16 |           0.99 |   4 |   7 |     0 |         4 |          0 |
| ExterQual      |          0 |           1.00 |   2 |   2 |     0 |         4 |          0 |
| ExterCond      |          0 |           1.00 |   2 |   2 |     0 |         5 |          0 |
| Foundation     |          0 |           1.00 |   4 |   6 |     0 |         6 |          0 |
| BsmtQual       |         44 |           0.97 |   2 |   2 |     0 |         4 |          0 |
| BsmtCond       |         45 |           0.97 |   2 |   2 |     0 |         4 |          0 |
| BsmtExposure   |         44 |           0.97 |   2 |   2 |     0 |         4 |          0 |
| BsmtFinType1   |         42 |           0.97 |   3 |   3 |     0 |         6 |          0 |
| BsmtFinType2   |         42 |           0.97 |   3 |   3 |     0 |         6 |          0 |
| Heating        |          0 |           1.00 |   4 |   4 |     0 |         4 |          0 |
| HeatingQC      |          0 |           1.00 |   2 |   2 |     0 |         5 |          0 |
| CentralAir     |          0 |           1.00 |   1 |   1 |     0 |         2 |          0 |
| Electrical     |          0 |           1.00 |   5 |   5 |     0 |         4 |          0 |
| KitchenQual    |          1 |           1.00 |   2 |   2 |     0 |         4 |          0 |
| Functional     |          2 |           1.00 |   3 |   4 |     0 |         7 |          0 |
| FireplaceQu    |        730 |           0.50 |   2 |   2 |     0 |         5 |          0 |
| GarageType     |         76 |           0.95 |   6 |   7 |     0 |         6 |          0 |
| GarageFinish   |         78 |           0.95 |   3 |   3 |     0 |         3 |          0 |
| GarageQual     |         78 |           0.95 |   2 |   2 |     0 |         4 |          0 |
| GarageCond     |         78 |           0.95 |   2 |   2 |     0 |         5 |          0 |
| PavedDrive     |          0 |           1.00 |   1 |   1 |     0 |         3 |          0 |
| PoolQC         |       1456 |           0.00 |   2 |   2 |     0 |         2 |          0 |
| Fence          |       1169 |           0.20 |   4 |   5 |     0 |         4 |          0 |
| MiscFeature    |       1408 |           0.03 |   4 |   4 |     0 |         3 |          0 |
| SaleType       |          1 |           1.00 |   2 |   5 |     0 |         9 |          0 |
| SaleCondition  |          0 |           1.00 |   6 |   7 |     0 |         6 |          0 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |    mean |      sd |   p0 |     p25 |    p50 |      p75 |  p100 | hist  |
|:---------------|-----------:|---------------:|--------:|--------:|-----:|--------:|-------:|---------:|------:|:------|
| Id             |          0 |           1.00 | 2190.00 |  421.32 | 1461 | 1825.50 | 2190.0 |  2554.50 |  2919 | ▇▇▇▇▇ |
| MSSubClass     |          0 |           1.00 |   57.38 |   42.75 |   20 |   20.00 |   50.0 |    70.00 |   190 | ▇▅▂▁▁ |
| LotFrontage    |        227 |           0.84 |   68.58 |   22.38 |   21 |   58.00 |   67.0 |    80.00 |   200 | ▃▇▁▁▁ |
| LotArea        |          0 |           1.00 | 9819.16 | 4955.52 | 1470 | 7391.00 | 9399.0 | 11517.50 | 56600 | ▇▂▁▁▁ |
| OverallQual    |          0 |           1.00 |    6.08 |    1.44 |    1 |    5.00 |    6.0 |     7.00 |    10 | ▁▁▇▅▁ |
| OverallCond    |          0 |           1.00 |    5.55 |    1.11 |    1 |    5.00 |    5.0 |     6.00 |     9 | ▁▁▇▅▁ |
| YearBuilt      |          0 |           1.00 | 1971.36 |   30.39 | 1879 | 1953.00 | 1973.0 |  2001.00 |  2010 | ▁▂▃▆▇ |
| YearRemodAdd   |          0 |           1.00 | 1983.66 |   21.13 | 1950 | 1963.00 | 1992.0 |  2004.00 |  2010 | ▅▂▂▃▇ |
| MasVnrArea     |         15 |           0.99 |  100.71 |  177.63 |    0 |    0.00 |    0.0 |   164.00 |  1290 | ▇▁▁▁▁ |
| BsmtFinSF1     |          1 |           1.00 |  439.20 |  455.27 |    0 |    0.00 |  350.5 |   753.50 |  4010 | ▇▂▁▁▁ |
| BsmtFinSF2     |          1 |           1.00 |   52.62 |  176.75 |    0 |    0.00 |    0.0 |     0.00 |  1526 | ▇▁▁▁▁ |
| BsmtUnfSF      |          1 |           1.00 |  554.29 |  437.26 |    0 |  219.25 |  460.0 |   797.75 |  2140 | ▇▆▂▁▁ |
| TotalBsmtSF    |          1 |           1.00 | 1046.12 |  442.90 |    0 |  784.00 |  988.0 |  1305.00 |  5095 | ▇▇▁▁▁ |
| 1stFlrSF       |          0 |           1.00 | 1156.53 |  398.17 |  407 |  873.50 | 1079.0 |  1382.50 |  5095 | ▇▃▁▁▁ |
| 2ndFlrSF       |          0 |           1.00 |  325.97 |  420.61 |    0 |    0.00 |    0.0 |   676.00 |  1862 | ▇▃▂▁▁ |
| LowQualFinSF   |          0 |           1.00 |    3.54 |   44.04 |    0 |    0.00 |    0.0 |     0.00 |  1064 | ▇▁▁▁▁ |
| GrLivArea      |          0 |           1.00 | 1486.05 |  485.57 |  407 | 1117.50 | 1432.0 |  1721.00 |  5095 | ▇▇▁▁▁ |
| BsmtFullBath   |          2 |           1.00 |    0.43 |    0.53 |    0 |    0.00 |    0.0 |     1.00 |     3 | ▇▆▁▁▁ |
| BsmtHalfBath   |          2 |           1.00 |    0.07 |    0.25 |    0 |    0.00 |    0.0 |     0.00 |     2 | ▇▁▁▁▁ |
| FullBath       |          0 |           1.00 |    1.57 |    0.56 |    0 |    1.00 |    2.0 |     2.00 |     4 | ▁▇▇▁▁ |
| HalfBath       |          0 |           1.00 |    0.38 |    0.50 |    0 |    0.00 |    0.0 |     1.00 |     2 | ▇▁▅▁▁ |
| BedroomAbvGr   |          0 |           1.00 |    2.85 |    0.83 |    0 |    2.00 |    3.0 |     3.00 |     6 | ▁▃▇▂▁ |
| KitchenAbvGr   |          0 |           1.00 |    1.04 |    0.21 |    0 |    1.00 |    1.0 |     1.00 |     2 | ▁▁▇▁▁ |
| TotRmsAbvGrd   |          0 |           1.00 |    6.39 |    1.51 |    3 |    5.00 |    6.0 |     7.00 |    15 | ▅▇▃▁▁ |
| Fireplaces     |          0 |           1.00 |    0.58 |    0.65 |    0 |    0.00 |    0.0 |     1.00 |     4 | ▇▇▁▁▁ |
| GarageYrBlt    |         78 |           0.95 | 1977.72 |   26.43 | 1895 | 1959.00 | 1979.0 |  2002.00 |  2207 | ▂▇▁▁▁ |
| GarageCars     |          1 |           1.00 |    1.77 |    0.78 |    0 |    1.00 |    2.0 |     2.00 |     5 | ▅▇▂▁▁ |
| GarageArea     |          1 |           1.00 |  472.77 |  217.05 |    0 |  318.00 |  480.0 |   576.00 |  1488 | ▃▇▃▁▁ |
| WoodDeckSF     |          0 |           1.00 |   93.17 |  127.74 |    0 |    0.00 |    0.0 |   168.00 |  1424 | ▇▁▁▁▁ |
| OpenPorchSF    |          0 |           1.00 |   48.31 |   68.88 |    0 |    0.00 |   28.0 |    72.00 |   742 | ▇▁▁▁▁ |
| EnclosedPorch  |          0 |           1.00 |   24.24 |   67.23 |    0 |    0.00 |    0.0 |     0.00 |  1012 | ▇▁▁▁▁ |
| 3SsnPorch      |          0 |           1.00 |    1.79 |   20.21 |    0 |    0.00 |    0.0 |     0.00 |   360 | ▇▁▁▁▁ |
| ScreenPorch    |          0 |           1.00 |   17.06 |   56.61 |    0 |    0.00 |    0.0 |     0.00 |   576 | ▇▁▁▁▁ |
| PoolArea       |          0 |           1.00 |    1.74 |   30.49 |    0 |    0.00 |    0.0 |     0.00 |   800 | ▇▁▁▁▁ |
| MiscVal        |          0 |           1.00 |   58.17 |  630.81 |    0 |    0.00 |    0.0 |     0.00 | 17000 | ▇▁▁▁▁ |
| MoSold         |          0 |           1.00 |    6.10 |    2.72 |    1 |    4.00 |    6.0 |     8.00 |    12 | ▅▆▇▃▃ |
| YrSold         |          0 |           1.00 | 2007.77 |    1.30 | 2006 | 2007.00 | 2008.0 |  2009.00 |  2010 | ▇▇▇▇▃ |

# 4 EDA with visualization (탐색적 데이터 분석)

## 4.1 Distribution of `sale_price`

If we check out the distribution of the house price, it is little bit
skewed to the right.

``` r
train %>%
    ggplot(aes(x=SalePrice)) +
    geom_histogram()
```

<img src="glmnet_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />
Since we want to build a linear regression assume that the noise follows
the normal distribution, let us take a log to `SalePrice` variable.

``` r
train %>%
    ggplot(aes(x=log(SalePrice))) +
    geom_histogram()
```

<img src="glmnet_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />
\#\# `NA`s

There is a nice package for checking out `NA`s. Let’s see how many
variables we have which contains `NA`s

``` r
library(naniar)
train %>% 
    select(where(~sum(is.na(.)) > 0)) %>%
    gg_miss_var()
```

<img src="glmnet_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

We can do more analysis about `NA`s with `upset()` function, which shows
that most of the observations with `NA`s in the data set have `NA`s at
the `PoolQC`, `MiscFeature`, `Alley`, `Fence` at the same time.

``` r
train %>% 
    select(where(~sum(is.na(.)) > 0 )) %>%
    gg_miss_upset()
```

<img src="glmnet_files/figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />
From the abobe, we can have some insights that if a house doesn’t have
Pool, it is likely that it doesn’t have Alley, Fence, and Fireplace too.

# 5 Preprocessing with `recipe` (전처리 레시피 만들기)

First, I would like to clean the variable names with `janitor` package
so that we have consistent variable names.

## 5.1 `all_data` combine and name cleaning with \`janitor

``` r
all_data <- bind_rows(train, test) %>%
    janitor::clean_names()
names(all_data)[1:10]
```

    ##  [1] "id"           "ms_sub_class" "ms_zoning"    "lot_frontage" "lot_area"    
    ##  [6] "street"       "alley"        "lot_shape"    "land_contour" "utilities"

## 5.2 Make recipe

Note that we will use mode imputation for nominal variables for the
baseline, and the mean imputation for the numerical variables. However,
thie should be changed to build a more sensitive model because we have
checked that the `NA` in the nominal variables indicates that cases
where the house doesn’t have the corresponding attributes.

``` r
housing_recipe <- all_data %>%
    recipe(sale_price ~ .) %>% # formula 
    step_rm(id) %>% # remove id column
    step_log(sale_price) %>% # log to sale_price
    step_impute_mode(all_nominal()) %>% # impute nominal variables to mode 
    step_dummy(all_nominal()) %>% # dummy coding
    step_impute_mean(all_predictors()) %>% # impute numeric variables to mean
    step_normalize(all_predictors()) %>% # normalize numeric variables
    prep(training = all_data)

print(housing_recipe)
```

    ## Data Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor         80
    ## 
    ## Training data contained 2919 data points and 2919 incomplete rows. 
    ## 
    ## Operations:
    ## 
    ## Variables removed id [trained]
    ## Log transformation on sale_price [trained]
    ## Mode Imputation for ms_zoning, street, alley, ... [trained]
    ## Dummy variables from ms_zoning, street, alley, lot_shape, ... [trained]
    ## Mean Imputation for ms_sub_class, lot_frontage, ... [trained]
    ## Centering and scaling for ms_sub_class, lot_frontage, ... [trained]

## 5.3 `juice` the all\_data2 and split

``` r
all_data2 <- juice(housing_recipe)
all_data2
```

    ## # A tibble: 2,919 x 246
    ##    ms_sub_class lot_frontage lot_area overall_qual overall_cond year_built
    ##           <dbl>        <dbl>    <dbl>        <dbl>        <dbl>      <dbl>
    ##  1       0.0673      -0.202   -0.218        0.646        -0.507     1.05  
    ##  2      -0.873        0.502   -0.0720      -0.0632        2.19      0.155 
    ##  3       0.0673      -0.0613   0.137        0.646        -0.507     0.980 
    ##  4       0.303       -0.437   -0.0784       0.646        -0.507    -1.86  
    ##  5       0.0673       0.689    0.519        1.36         -0.507     0.947 
    ##  6      -0.168        0.736    0.500       -0.772        -0.507     0.716 
    ##  7      -0.873        0.267   -0.0107       1.36         -0.507     1.08  
    ##  8       0.0673       0        0.0271       0.646         0.391     0.0557
    ##  9      -0.168       -0.859   -0.513        0.646        -0.507    -1.33  
    ## 10       3.12        -0.906   -0.348       -0.772         0.391    -1.07  
    ## # ... with 2,909 more rows, and 240 more variables: year_remod_add <dbl>,
    ## #   mas_vnr_area <dbl>, bsmt_fin_sf1 <dbl>, bsmt_fin_sf2 <dbl>,
    ## #   bsmt_unf_sf <dbl>, total_bsmt_sf <dbl>, x1st_flr_sf <dbl>,
    ## #   x2nd_flr_sf <dbl>, low_qual_fin_sf <dbl>, gr_liv_area <dbl>,
    ## #   bsmt_full_bath <dbl>, bsmt_half_bath <dbl>, full_bath <dbl>,
    ## #   half_bath <dbl>, bedroom_abv_gr <dbl>, kitchen_abv_gr <dbl>,
    ## #   tot_rms_abv_grd <dbl>, fireplaces <dbl>, garage_yr_blt <dbl>,
    ## #   garage_cars <dbl>, garage_area <dbl>, wood_deck_sf <dbl>,
    ## #   open_porch_sf <dbl>, enclosed_porch <dbl>, x3ssn_porch <dbl>,
    ## #   screen_porch <dbl>, pool_area <dbl>, misc_val <dbl>, mo_sold <dbl>,
    ## #   yr_sold <dbl>, sale_price <dbl>, ms_zoning_FV <dbl>, ms_zoning_RH <dbl>,
    ## #   ms_zoning_RL <dbl>, ms_zoning_RM <dbl>, street_Pave <dbl>,
    ## #   alley_Pave <dbl>, lot_shape_IR2 <dbl>, lot_shape_IR3 <dbl>,
    ## #   lot_shape_Reg <dbl>, land_contour_HLS <dbl>, land_contour_Low <dbl>,
    ## #   land_contour_Lvl <dbl>, utilities_NoSeWa <dbl>, lot_config_CulDSac <dbl>,
    ## #   lot_config_FR2 <dbl>, lot_config_FR3 <dbl>, lot_config_Inside <dbl>,
    ## #   land_slope_Mod <dbl>, land_slope_Sev <dbl>, neighborhood_Blueste <dbl>,
    ## #   neighborhood_BrDale <dbl>, neighborhood_BrkSide <dbl>,
    ## #   neighborhood_ClearCr <dbl>, neighborhood_CollgCr <dbl>,
    ## #   neighborhood_Crawfor <dbl>, neighborhood_Edwards <dbl>,
    ## #   neighborhood_Gilbert <dbl>, neighborhood_IDOTRR <dbl>,
    ## #   neighborhood_MeadowV <dbl>, neighborhood_Mitchel <dbl>,
    ## #   neighborhood_NAmes <dbl>, neighborhood_NoRidge <dbl>,
    ## #   neighborhood_NPkVill <dbl>, neighborhood_NridgHt <dbl>,
    ## #   neighborhood_NWAmes <dbl>, neighborhood_OldTown <dbl>,
    ## #   neighborhood_Sawyer <dbl>, neighborhood_SawyerW <dbl>,
    ## #   neighborhood_Somerst <dbl>, neighborhood_StoneBr <dbl>,
    ## #   neighborhood_SWISU <dbl>, neighborhood_Timber <dbl>,
    ## #   neighborhood_Veenker <dbl>, condition1_Feedr <dbl>, condition1_Norm <dbl>,
    ## #   condition1_PosA <dbl>, condition1_PosN <dbl>, condition1_RRAe <dbl>,
    ## #   condition1_RRAn <dbl>, condition1_RRNe <dbl>, condition1_RRNn <dbl>,
    ## #   condition2_Feedr <dbl>, condition2_Norm <dbl>, condition2_PosA <dbl>,
    ## #   condition2_PosN <dbl>, condition2_RRAe <dbl>, condition2_RRAn <dbl>,
    ## #   condition2_RRNn <dbl>, bldg_type_X2fmCon <dbl>, bldg_type_Duplex <dbl>,
    ## #   bldg_type_Twnhs <dbl>, bldg_type_TwnhsE <dbl>, house_style_X1.5Unf <dbl>,
    ## #   house_style_X1Story <dbl>, house_style_X2.5Fin <dbl>,
    ## #   house_style_X2.5Unf <dbl>, house_style_X2Story <dbl>,
    ## #   house_style_SFoyer <dbl>, house_style_SLvl <dbl>, ...

We are done for preprocessing. Let’s split the data set.

``` r
train_index <- seq_len(nrow(train))
train2 <- all_data2[train_index,]
test2 <- all_data2[-train_index,]
```

``` r
train2 %>% head() %>% kable()
```

| ms\_sub\_class | lot\_frontage |  lot\_area | overall\_qual | overall\_cond | year\_built | year\_remod\_add | mas\_vnr\_area | bsmt\_fin\_sf1 | bsmt\_fin\_sf2 | bsmt\_unf\_sf | total\_bsmt\_sf | x1st\_flr\_sf | x2nd\_flr\_sf | low\_qual\_fin\_sf | gr\_liv\_area | bsmt\_full\_bath | bsmt\_half\_bath | full\_bath | half\_bath | bedroom\_abv\_gr | kitchen\_abv\_gr | tot\_rms\_abv\_grd | fireplaces | garage\_yr\_blt | garage\_cars | garage\_area | wood\_deck\_sf | open\_porch\_sf | enclosed\_porch | x3ssn\_porch | screen\_porch | pool\_area |  misc\_val |   mo\_sold |   yr\_sold | sale\_price | ms\_zoning\_FV | ms\_zoning\_RH | ms\_zoning\_RL | ms\_zoning\_RM | street\_Pave | alley\_Pave | lot\_shape\_IR2 | lot\_shape\_IR3 | lot\_shape\_Reg | land\_contour\_HLS | land\_contour\_Low | land\_contour\_Lvl | utilities\_NoSeWa | lot\_config\_CulDSac | lot\_config\_FR2 | lot\_config\_FR3 | lot\_config\_Inside | land\_slope\_Mod | land\_slope\_Sev | neighborhood\_Blueste | neighborhood\_BrDale | neighborhood\_BrkSide | neighborhood\_ClearCr | neighborhood\_CollgCr | neighborhood\_Crawfor | neighborhood\_Edwards | neighborhood\_Gilbert | neighborhood\_IDOTRR | neighborhood\_MeadowV | neighborhood\_Mitchel | neighborhood\_NAmes | neighborhood\_NoRidge | neighborhood\_NPkVill | neighborhood\_NridgHt | neighborhood\_NWAmes | neighborhood\_OldTown | neighborhood\_Sawyer | neighborhood\_SawyerW | neighborhood\_Somerst | neighborhood\_StoneBr | neighborhood\_SWISU | neighborhood\_Timber | neighborhood\_Veenker | condition1\_Feedr | condition1\_Norm | condition1\_PosA | condition1\_PosN | condition1\_RRAe | condition1\_RRAn | condition1\_RRNe | condition1\_RRNn | condition2\_Feedr | condition2\_Norm | condition2\_PosA | condition2\_PosN | condition2\_RRAe | condition2\_RRAn | condition2\_RRNn | bldg\_type\_X2fmCon | bldg\_type\_Duplex | bldg\_type\_Twnhs | bldg\_type\_TwnhsE | house\_style\_X1.5Unf | house\_style\_X1Story | house\_style\_X2.5Fin | house\_style\_X2.5Unf | house\_style\_X2Story | house\_style\_SFoyer | house\_style\_SLvl | roof\_style\_Gable | roof\_style\_Gambrel | roof\_style\_Hip | roof\_style\_Mansard | roof\_style\_Shed | roof\_matl\_CompShg | roof\_matl\_Membran | roof\_matl\_Metal | roof\_matl\_Roll | roof\_matl\_Tar.Grv | roof\_matl\_WdShake | roof\_matl\_WdShngl | exterior1st\_AsphShn | exterior1st\_BrkComm | exterior1st\_BrkFace | exterior1st\_CBlock | exterior1st\_CemntBd | exterior1st\_HdBoard | exterior1st\_ImStucc | exterior1st\_MetalSd | exterior1st\_Plywood | exterior1st\_Stone | exterior1st\_Stucco | exterior1st\_VinylSd | exterior1st\_Wd.Sdng | exterior1st\_WdShing | exterior2nd\_AsphShn | exterior2nd\_Brk.Cmn | exterior2nd\_BrkFace | exterior2nd\_CBlock | exterior2nd\_CmentBd | exterior2nd\_HdBoard | exterior2nd\_ImStucc | exterior2nd\_MetalSd | exterior2nd\_Other | exterior2nd\_Plywood | exterior2nd\_Stone | exterior2nd\_Stucco | exterior2nd\_VinylSd | exterior2nd\_Wd.Sdng | exterior2nd\_Wd.Shng | mas\_vnr\_type\_BrkFace | mas\_vnr\_type\_None | mas\_vnr\_type\_Stone | exter\_qual\_Fa | exter\_qual\_Gd | exter\_qual\_TA | exter\_cond\_Fa | exter\_cond\_Gd | exter\_cond\_Po | exter\_cond\_TA | foundation\_CBlock | foundation\_PConc | foundation\_Slab | foundation\_Stone | foundation\_Wood | bsmt\_qual\_Fa | bsmt\_qual\_Gd | bsmt\_qual\_TA | bsmt\_cond\_Gd | bsmt\_cond\_Po | bsmt\_cond\_TA | bsmt\_exposure\_Gd | bsmt\_exposure\_Mn | bsmt\_exposure\_No | bsmt\_fin\_type1\_BLQ | bsmt\_fin\_type1\_GLQ | bsmt\_fin\_type1\_LwQ | bsmt\_fin\_type1\_Rec | bsmt\_fin\_type1\_Unf | bsmt\_fin\_type2\_BLQ | bsmt\_fin\_type2\_GLQ | bsmt\_fin\_type2\_LwQ | bsmt\_fin\_type2\_Rec | bsmt\_fin\_type2\_Unf | heating\_GasA | heating\_GasW | heating\_Grav | heating\_OthW | heating\_Wall | heating\_qc\_Fa | heating\_qc\_Gd | heating\_qc\_Po | heating\_qc\_TA | central\_air\_Y | electrical\_FuseF | electrical\_FuseP | electrical\_Mix | electrical\_SBrkr | kitchen\_qual\_Fa | kitchen\_qual\_Gd | kitchen\_qual\_TA | functional\_Maj2 | functional\_Min1 | functional\_Min2 | functional\_Mod | functional\_Sev | functional\_Typ | fireplace\_qu\_Fa | fireplace\_qu\_Gd | fireplace\_qu\_Po | fireplace\_qu\_TA | garage\_type\_Attchd | garage\_type\_Basment | garage\_type\_BuiltIn | garage\_type\_CarPort | garage\_type\_Detchd | garage\_finish\_RFn | garage\_finish\_Unf | garage\_qual\_Fa | garage\_qual\_Gd | garage\_qual\_Po | garage\_qual\_TA | garage\_cond\_Fa | garage\_cond\_Gd | garage\_cond\_Po | garage\_cond\_TA | paved\_drive\_P | paved\_drive\_Y | pool\_qc\_Fa | pool\_qc\_Gd | fence\_GdWo | fence\_MnPrv | fence\_MnWw | misc\_feature\_Othr | misc\_feature\_Shed | misc\_feature\_TenC | sale\_type\_Con | sale\_type\_ConLD | sale\_type\_ConLI | sale\_type\_ConLw | sale\_type\_CWD | sale\_type\_New | sale\_type\_Oth | sale\_type\_WD | sale\_condition\_AdjLand | sale\_condition\_Alloca | sale\_condition\_Family | sale\_condition\_Normal | sale\_condition\_Partial |
|---------------:|--------------:|-----------:|--------------:|--------------:|------------:|-----------------:|---------------:|---------------:|---------------:|--------------:|----------------:|--------------:|--------------:|-------------------:|--------------:|-----------------:|-----------------:|-----------:|-----------:|-----------------:|-----------------:|-------------------:|-----------:|----------------:|-------------:|-------------:|---------------:|----------------:|----------------:|-------------:|--------------:|-----------:|-----------:|-----------:|-----------:|------------:|---------------:|---------------:|---------------:|---------------:|-------------:|------------:|----------------:|----------------:|----------------:|-------------------:|-------------------:|-------------------:|------------------:|---------------------:|-----------------:|-----------------:|--------------------:|-----------------:|-----------------:|----------------------:|---------------------:|----------------------:|----------------------:|----------------------:|----------------------:|----------------------:|----------------------:|---------------------:|----------------------:|----------------------:|--------------------:|----------------------:|----------------------:|----------------------:|---------------------:|----------------------:|---------------------:|----------------------:|----------------------:|----------------------:|--------------------:|---------------------:|----------------------:|------------------:|-----------------:|-----------------:|-----------------:|-----------------:|-----------------:|-----------------:|-----------------:|------------------:|-----------------:|-----------------:|-----------------:|-----------------:|-----------------:|-----------------:|--------------------:|-------------------:|------------------:|-------------------:|----------------------:|----------------------:|----------------------:|----------------------:|----------------------:|---------------------:|-------------------:|-------------------:|---------------------:|-----------------:|---------------------:|------------------:|--------------------:|--------------------:|------------------:|-----------------:|--------------------:|--------------------:|--------------------:|---------------------:|---------------------:|---------------------:|--------------------:|---------------------:|---------------------:|---------------------:|---------------------:|---------------------:|-------------------:|--------------------:|---------------------:|---------------------:|---------------------:|---------------------:|---------------------:|---------------------:|--------------------:|---------------------:|---------------------:|---------------------:|---------------------:|-------------------:|---------------------:|-------------------:|--------------------:|---------------------:|---------------------:|---------------------:|------------------------:|---------------------:|----------------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|-------------------:|------------------:|-----------------:|------------------:|-----------------:|---------------:|---------------:|---------------:|---------------:|---------------:|---------------:|-------------------:|-------------------:|-------------------:|----------------------:|----------------------:|----------------------:|----------------------:|----------------------:|----------------------:|----------------------:|----------------------:|----------------------:|----------------------:|--------------:|--------------:|--------------:|--------------:|--------------:|----------------:|----------------:|----------------:|----------------:|----------------:|------------------:|------------------:|----------------:|------------------:|------------------:|------------------:|------------------:|-----------------:|-----------------:|-----------------:|----------------:|----------------:|----------------:|------------------:|------------------:|------------------:|------------------:|---------------------:|----------------------:|----------------------:|----------------------:|---------------------:|--------------------:|--------------------:|-----------------:|-----------------:|-----------------:|-----------------:|-----------------:|-----------------:|-----------------:|-----------------:|----------------:|----------------:|-------------:|-------------:|------------:|-------------:|------------:|--------------------:|--------------------:|--------------------:|----------------:|------------------:|------------------:|------------------:|----------------:|----------------:|----------------:|---------------:|-------------------------:|------------------------:|------------------------:|------------------------:|-------------------------:|
|      0.0673199 |    -0.2020329 | -0.2178414 |     0.6460727 |    -0.5071973 |   1.0460784 |        0.8966793 |      0.5251119 |      0.5808073 |     -0.2930798 |    -0.9347024 |      -0.4442517 |    -0.7737285 |     1.2071717 |         -0.1011797 |     0.4134764 |        1.0868363 |       -0.2498524 |   0.781232 |  1.2323877 |         0.169898 |       -0.2076629 |          0.9866803 | -0.9241529 |       1.0007573 |    0.3064753 |    0.3488399 |     -0.7406335 |       0.1999717 |      -0.3595391 |   -0.1033128 |    -0.2858865 | -0.0631394 | -0.0895766 | -1.5519176 |  0.1576185 |    12.24769 |     -0.2235685 |     -0.0947847 |      0.5351371 |     -0.4324394 |    0.0642383 |  -0.1656675 |      -0.1634722 |       -0.074227 |       0.7549859 |         -0.2070212 |         -0.1448419 |          0.3365019 |         -0.018509 |           -0.2532614 |        -0.173155 |       -0.0694091 |            0.606934 |       -0.2114791 |        -0.074227 |            -0.0586211 |           -0.1018855 |            -0.1959779 |            -0.1236896 |             3.1510604 |            -0.1912176 |            -0.2667738 |            -0.2447291 |           -0.1813765 |            -0.1132868 |            -0.2015634 |          -0.4229141 |            -0.1578646 |            -0.0891026 |            -0.2455142 |           -0.2167279 |            -0.2985775 |           -0.2335237 |            -0.2114791 |            -0.2578243 |            -0.1333279 |          -0.1292795 |           -0.1590004 |            -0.0910347 |        -0.2439421 |        0.4030253 |       -0.0830456 |       -0.1163487 |       -0.0983967 |       -0.1319913 |       -0.0453765 |       -0.0556033 |        -0.0668728 |        0.1018855 |        -0.037037 |        -0.037037 |        -0.018509 |        -0.018509 |       -0.0261802 |          -0.1472876 |         -0.1969181 |        -0.1843766 |         -0.2903361 |            -0.0809289 |            -1.0077380 |            -0.0524143 |            -0.0910347 |             1.5318854 |           -0.1710455 |         -0.2141168 |          0.5133674 |            -0.087129 |       -0.4822925 |           -0.0614929 |        -0.0414158 |           0.1222546 |           -0.018509 |         -0.018509 |        -0.018509 |          -0.0891026 |          -0.0556033 |          -0.0490206 |           -0.0261802 |           -0.0453765 |           -0.1752422 |          -0.0261802 |           -0.2123613 |           -0.4223512 |            -0.018509 |           -0.4268461 |           -0.2861546 |         -0.0261802 |          -0.1222546 |            1.3580858 |           -0.4047462 |           -0.1398328 |            -0.037037 |            -0.087129 |           -0.1279035 |          -0.0320695 |           -0.2123613 |           -0.4018763 |           -0.0718576 |           -0.4251627 |          -0.018509 |           -0.3192027 |         -0.0453765 |          -0.1279035 |            1.3693865 |           -0.3932108 |           -0.1689125 |               1.5231625 |           -1.2373891 |            -0.3053301 |      -0.1101443 |       1.4074569 |      -1.2662447 |      -0.1532457 |      -0.3377618 |      -0.0320695 |       0.3873845 |         -0.8562253 |         1.1096078 |        -0.130642 |        -0.0614929 |       -0.0414158 |     -0.1762775 |      1.1890784 |     -0.9364133 |     -0.2088138 |     -0.0414158 |      0.2931008 |          -0.323096 |         -0.2985775 |          0.6852938 |            -0.3185509 |             1.5611942 |              -0.23596 |            -0.3307969 |             -0.683675 |            -0.1544121 |            -0.1085406 |            -0.1752422 |            -0.1931338 |             0.3666433 |      0.125109 |    -0.0966069 |    -0.0556033 |    -0.0261802 |    -0.0453765 |      -0.1803668 |      -0.4402256 |      -0.0320695 |      -0.6445724 |       0.2682439 |        -0.1319913 |        -0.0524143 |       -0.018509 |         0.3039876 |        -0.1567214 |         1.2391648 |         -1.023047 |       -0.0556033 |       -0.1508882 |       -0.1567214 |      -0.1101443 |      -0.0261802 |       0.2711665 |        -0.1612502 |         0.5905687 |        -0.1265135 |        -0.5042992 |            0.7432834 |            -0.1117261 |            -0.2608328 |            -0.0718576 |           -0.6032363 |           1.6119459 |          -0.9526448 |       -0.2105938 |       -0.0910347 |       -0.0414158 |        0.2375732 |       -0.1612502 |       -0.0718576 |       -0.0694091 |        0.1940858 |      -0.1472876 |       0.3243873 |   -0.0261802 |    -0.037037 |  -0.1997162 |    0.3006139 |  -0.0642383 |           -0.037037 |           0.0586211 |           -0.018509 |      -0.0414158 |        -0.0947847 |        -0.0556033 |        -0.0524143 |      -0.0642383 |      -0.2985775 |      -0.0490206 |      0.3943712 |               -0.0642383 |              -0.0910347 |              -0.1265135 |               0.4638573 |               -0.3026411 |
|     -0.8734664 |     0.5017845 | -0.0720317 |    -0.0631737 |     2.1879039 |   0.1547375 |       -0.3955364 |     -0.5721522 |      1.1779104 |     -0.2930798 |    -0.6297885 |       0.4770294 |     0.2610301 |    -0.7848906 |         -0.1011797 |    -0.4718098 |       -0.8195386 |        3.8217640 |   0.781232 | -0.7561915 |         0.169898 |       -0.2076629 |         -0.2877090 |  0.6235248 |      -0.0849858 |    0.3064753 |   -0.0597822 |      1.6146027 |      -0.7027224 |      -0.3595391 |   -0.1033128 |    -0.2858865 | -0.0631394 | -0.0895766 | -0.4468483 | -0.6028583 |    12.10901 |     -0.2235685 |     -0.0947847 |      0.5351371 |     -0.4324394 |    0.0642383 |  -0.1656675 |      -0.1634722 |       -0.074227 |       0.7549859 |         -0.2070212 |         -0.1448419 |          0.3365019 |         -0.018509 |           -0.2532614 |         5.773193 |       -0.0694091 |           -1.647061 |       -0.2114791 |        -0.074227 |            -0.0586211 |           -0.1018855 |            -0.1959779 |            -0.1236896 |            -0.3172448 |            -0.1912176 |            -0.2667738 |            -0.2447291 |           -0.1813765 |            -0.1132868 |            -0.2015634 |          -0.4229141 |            -0.1578646 |            -0.0891026 |            -0.2455142 |           -0.2167279 |            -0.2985775 |           -0.2335237 |            -0.2114791 |            -0.2578243 |            -0.1333279 |          -0.1292795 |           -0.1590004 |            10.9810599 |         4.0979294 |       -2.4803837 |       -0.0830456 |       -0.1163487 |       -0.0983967 |       -0.1319913 |       -0.0453765 |       -0.0556033 |        -0.0668728 |        0.1018855 |        -0.037037 |        -0.037037 |        -0.018509 |        -0.018509 |       -0.0261802 |          -0.1472876 |         -0.1969181 |        -0.1843766 |         -0.2903361 |            -0.0809289 |             0.9919814 |            -0.0524143 |            -0.0910347 |            -0.6525667 |           -0.1710455 |         -0.2141168 |          0.5133674 |            -0.087129 |       -0.4822925 |           -0.0614929 |        -0.0414158 |           0.1222546 |           -0.018509 |         -0.018509 |        -0.018509 |          -0.0891026 |          -0.0556033 |          -0.0490206 |           -0.0261802 |           -0.0453765 |           -0.1752422 |          -0.0261802 |           -0.2123613 |           -0.4223512 |            -0.018509 |            2.3419622 |           -0.2861546 |         -0.0261802 |          -0.1222546 |           -0.7360782 |           -0.4047462 |           -0.1398328 |            -0.037037 |            -0.087129 |           -0.1279035 |          -0.0320695 |           -0.2123613 |           -0.4018763 |           -0.0718576 |            2.3512352 |          -0.018509 |           -0.3192027 |         -0.0453765 |          -0.1279035 |           -0.7300038 |           -0.3932108 |           -0.1689125 |              -0.6563038 |            0.8078764 |            -0.3053301 |      -0.1101443 |      -0.7102579 |       0.7894662 |      -0.1532457 |      -0.3377618 |      -0.0320695 |       0.3873845 |          1.1675169 |        -0.9009106 |        -0.130642 |        -0.0614929 |       -0.0414158 |     -0.1762775 |      1.1890784 |     -0.9364133 |     -0.2088138 |     -0.0414158 |      0.2931008 |           3.093995 |         -0.2985775 |         -1.4587283 |            -0.3185509 |            -0.6403159 |              -0.23596 |            -0.3307969 |             -0.683675 |            -0.1544121 |            -0.1085406 |            -0.1752422 |            -0.1931338 |             0.3666433 |      0.125109 |    -0.0966069 |    -0.0556033 |    -0.0261802 |    -0.0453765 |      -0.1803668 |      -0.4402256 |      -0.0320695 |      -0.6445724 |       0.2682439 |        -0.1319913 |        -0.0524143 |       -0.018509 |         0.3039876 |        -0.1567214 |        -0.8067187 |          0.977137 |       -0.0556033 |       -0.1508882 |       -0.1567214 |      -0.1101443 |      -0.0261802 |       0.2711665 |        -0.1612502 |        -1.6927029 |        -0.1265135 |         1.9822706 |            0.7432834 |            -0.1117261 |            -0.2608328 |            -0.0718576 |           -0.6032363 |           1.6119459 |          -0.9526448 |       -0.2105938 |       -0.0910347 |       -0.0414158 |        0.2375732 |       -0.1612502 |       -0.0718576 |       -0.0694091 |        0.1940858 |      -0.1472876 |       0.3243873 |   -0.0261802 |    -0.037037 |  -0.1997162 |    0.3006139 |  -0.0642383 |           -0.037037 |           0.0586211 |           -0.018509 |      -0.0414158 |        -0.0947847 |        -0.0556033 |        -0.0524143 |      -0.0642383 |      -0.2985775 |      -0.0490206 |      0.3943712 |               -0.0642383 |              -0.0910347 |              -0.1265135 |               0.4638573 |               -0.3026411 |
|      0.0673199 |    -0.0612694 |  0.1371734 |     0.6460727 |    -0.5071973 |   0.9800531 |        0.8488195 |      0.3347702 |      0.0978563 |     -0.2930798 |    -0.2884670 |      -0.2990251 |    -0.6106138 |     1.2351632 |         -0.1011797 |     0.5636589 |        1.0868363 |       -0.2498524 |   0.781232 |  1.2323877 |         0.169898 |       -0.2076629 |         -0.2877090 |  0.6235248 |       0.9203319 |    0.3064753 |    0.6274459 |     -0.7406335 |      -0.0811953 |      -0.3595391 |   -0.1033128 |    -0.2858865 | -0.0631394 | -0.0895766 |  1.0265775 |  0.1576185 |    12.31717 |     -0.2235685 |     -0.0947847 |      0.5351371 |     -0.4324394 |    0.0642383 |  -0.1656675 |      -0.1634722 |       -0.074227 |      -1.3240743 |         -0.2070212 |         -0.1448419 |          0.3365019 |         -0.018509 |           -0.2532614 |        -0.173155 |       -0.0694091 |            0.606934 |       -0.2114791 |        -0.074227 |            -0.0586211 |           -0.1018855 |            -0.1959779 |            -0.1236896 |             3.1510604 |            -0.1912176 |            -0.2667738 |            -0.2447291 |           -0.1813765 |            -0.1132868 |            -0.2015634 |          -0.4229141 |            -0.1578646 |            -0.0891026 |            -0.2455142 |           -0.2167279 |            -0.2985775 |           -0.2335237 |            -0.2114791 |            -0.2578243 |            -0.1333279 |          -0.1292795 |           -0.1590004 |            -0.0910347 |        -0.2439421 |        0.4030253 |       -0.0830456 |       -0.1163487 |       -0.0983967 |       -0.1319913 |       -0.0453765 |       -0.0556033 |        -0.0668728 |        0.1018855 |        -0.037037 |        -0.037037 |        -0.018509 |        -0.018509 |       -0.0261802 |          -0.1472876 |         -0.1969181 |        -0.1843766 |         -0.2903361 |            -0.0809289 |            -1.0077380 |            -0.0524143 |            -0.0910347 |             1.5318854 |           -0.1710455 |         -0.2141168 |          0.5133674 |            -0.087129 |       -0.4822925 |           -0.0614929 |        -0.0414158 |           0.1222546 |           -0.018509 |         -0.018509 |        -0.018509 |          -0.0891026 |          -0.0556033 |          -0.0490206 |           -0.0261802 |           -0.0453765 |           -0.1752422 |          -0.0261802 |           -0.2123613 |           -0.4223512 |            -0.018509 |           -0.4268461 |           -0.2861546 |         -0.0261802 |          -0.1222546 |            1.3580858 |           -0.4047462 |           -0.1398328 |            -0.037037 |            -0.087129 |           -0.1279035 |          -0.0320695 |           -0.2123613 |           -0.4018763 |           -0.0718576 |           -0.4251627 |          -0.018509 |           -0.3192027 |         -0.0453765 |          -0.1279035 |            1.3693865 |           -0.3932108 |           -0.1689125 |               1.5231625 |           -1.2373891 |            -0.3053301 |      -0.1101443 |       1.4074569 |      -1.2662447 |      -0.1532457 |      -0.3377618 |      -0.0320695 |       0.3873845 |         -0.8562253 |         1.1096078 |        -0.130642 |        -0.0614929 |       -0.0414158 |     -0.1762775 |      1.1890784 |     -0.9364133 |     -0.2088138 |     -0.0414158 |      0.2931008 |          -0.323096 |          3.3480662 |         -1.4587283 |            -0.3185509 |             1.5611942 |              -0.23596 |            -0.3307969 |             -0.683675 |            -0.1544121 |            -0.1085406 |            -0.1752422 |            -0.1931338 |             0.3666433 |      0.125109 |    -0.0966069 |    -0.0556033 |    -0.0261802 |    -0.0453765 |      -0.1803668 |      -0.4402256 |      -0.0320695 |      -0.6445724 |       0.2682439 |        -0.1319913 |        -0.0524143 |       -0.018509 |         0.3039876 |        -0.1567214 |         1.2391648 |         -1.023047 |       -0.0556033 |       -0.1508882 |       -0.1567214 |      -0.1101443 |      -0.0261802 |       0.2711665 |        -0.1612502 |        -1.6927029 |        -0.1265135 |         1.9822706 |            0.7432834 |            -0.1117261 |            -0.2608328 |            -0.0718576 |           -0.6032363 |           1.6119459 |          -0.9526448 |       -0.2105938 |       -0.0910347 |       -0.0414158 |        0.2375732 |       -0.1612502 |       -0.0718576 |       -0.0694091 |        0.1940858 |      -0.1472876 |       0.3243873 |   -0.0261802 |    -0.037037 |  -0.1997162 |    0.3006139 |  -0.0642383 |           -0.037037 |           0.0586211 |           -0.018509 |      -0.0414158 |        -0.0947847 |        -0.0556033 |        -0.0524143 |      -0.0642383 |      -0.2985775 |      -0.0490206 |      0.3943712 |               -0.0642383 |              -0.0910347 |              -0.1265135 |               0.4638573 |               -0.3026411 |
|      0.3025164 |    -0.4366387 | -0.0783713 |     0.6460727 |    -0.5071973 |  -1.8590326 |       -0.6826955 |     -0.5721522 |     -0.4948563 |     -0.2930798 |    -0.0472664 |      -0.6711682 |    -0.5061185 |     0.9785744 |         -0.1011797 |     0.4273090 |        1.0868363 |       -0.2498524 |  -1.027187 | -0.7561915 |         0.169898 |       -0.2076629 |          0.3494857 |  0.6235248 |       0.7996938 |    1.6196836 |    0.7853226 |     -0.7406335 |      -0.1847831 |       3.8743031 |   -0.1033128 |    -0.2858865 | -0.0631394 | -0.0895766 | -1.5519176 | -1.3633351 |    11.84940 |     -0.2235685 |     -0.0947847 |      0.5351371 |     -0.4324394 |    0.0642383 |  -0.1656675 |      -0.1634722 |       -0.074227 |      -1.3240743 |         -0.2070212 |         -0.1448419 |          0.3365019 |         -0.018509 |           -0.2532614 |        -0.173155 |       -0.0694091 |           -1.647061 |       -0.2114791 |        -0.074227 |            -0.0586211 |           -0.1018855 |            -0.1959779 |            -0.1236896 |            -0.3172448 |             5.2278523 |            -0.2667738 |            -0.2447291 |           -0.1813765 |            -0.1132868 |            -0.2015634 |          -0.4229141 |            -0.1578646 |            -0.0891026 |            -0.2455142 |           -0.2167279 |            -0.2985775 |           -0.2335237 |            -0.2114791 |            -0.2578243 |            -0.1333279 |          -0.1292795 |           -0.1590004 |            -0.0910347 |        -0.2439421 |        0.4030253 |       -0.0830456 |       -0.1163487 |       -0.0983967 |       -0.1319913 |       -0.0453765 |       -0.0556033 |        -0.0668728 |        0.1018855 |        -0.037037 |        -0.037037 |        -0.018509 |        -0.018509 |       -0.0261802 |          -0.1472876 |         -0.1969181 |        -0.1843766 |         -0.2903361 |            -0.0809289 |            -1.0077380 |            -0.0524143 |            -0.0910347 |             1.5318854 |           -0.1710455 |         -0.2141168 |          0.5133674 |            -0.087129 |       -0.4822925 |           -0.0614929 |        -0.0414158 |           0.1222546 |           -0.018509 |         -0.018509 |        -0.018509 |          -0.0891026 |          -0.0556033 |          -0.0490206 |           -0.0261802 |           -0.0453765 |           -0.1752422 |          -0.0261802 |           -0.2123613 |           -0.4223512 |            -0.018509 |           -0.4268461 |           -0.2861546 |         -0.0261802 |          -0.1222546 |           -0.7360782 |            2.4698379 |           -0.1398328 |            -0.037037 |            -0.087129 |           -0.1279035 |          -0.0320695 |           -0.2123613 |           -0.4018763 |           -0.0718576 |           -0.4251627 |          -0.018509 |           -0.3192027 |         -0.0453765 |          -0.1279035 |           -0.7300038 |           -0.3932108 |            5.9181952 |              -0.6563038 |            0.8078764 |            -0.3053301 |      -0.1101443 |      -0.7102579 |       0.7894662 |      -0.1532457 |      -0.3377618 |      -0.0320695 |       0.3873845 |         -0.8562253 |        -0.9009106 |        -0.130642 |        -0.0614929 |       -0.0414158 |     -0.1762775 |     -0.8406993 |      1.0675387 |      4.7873140 |     -0.0414158 |     -3.4106271 |          -0.323096 |         -0.2985775 |          0.6852938 |            -0.3185509 |            -0.6403159 |              -0.23596 |            -0.3307969 |             -0.683675 |            -0.1544121 |            -0.1085406 |            -0.1752422 |            -0.1931338 |             0.3666433 |      0.125109 |    -0.0966069 |    -0.0556033 |    -0.0261802 |    -0.0453765 |      -0.1803668 |       2.2707842 |      -0.0320695 |      -0.6445724 |       0.2682439 |        -0.1319913 |        -0.0524143 |       -0.018509 |         0.3039876 |        -0.1567214 |         1.2391648 |         -1.023047 |       -0.0556033 |       -0.1508882 |       -0.1567214 |      -0.1101443 |      -0.0261802 |       0.2711665 |        -0.1612502 |         0.5905687 |        -0.1265135 |        -0.5042992 |           -1.3449209 |            -0.1117261 |            -0.2608328 |            -0.0718576 |            1.6571574 |          -0.6201557 |           1.0493496 |       -0.2105938 |       -0.0910347 |       -0.0414158 |        0.2375732 |       -0.1612502 |       -0.0718576 |       -0.0694091 |        0.1940858 |      -0.1472876 |       0.3243873 |   -0.0261802 |    -0.037037 |  -0.1997162 |    0.3006139 |  -0.0642383 |           -0.037037 |           0.0586211 |           -0.018509 |      -0.0414158 |        -0.0947847 |        -0.0556033 |        -0.0524143 |      -0.0642383 |      -0.2985775 |      -0.0490206 |      0.3943712 |               -0.0642383 |              -0.0910347 |              -0.1265135 |              -2.1550970 |               -0.3026411 |
|      0.0673199 |     0.6894691 |  0.5188142 |     1.3553191 |    -0.5071973 |   0.9470405 |        0.7530998 |      1.3872480 |      0.4688505 |     -0.2930798 |    -0.1610403 |       0.2115370 |    -0.0371639 |     1.6713642 |         -0.1011797 |     1.3778060 |        1.0868363 |       -0.2498524 |   0.781232 |  1.2323877 |         1.385418 |       -0.2076629 |          1.6238750 |  0.6235248 |       0.8801192 |    1.6196836 |    1.6861486 |      0.7768341 |       0.5403318 |      -0.3595391 |   -0.1033128 |    -0.2858865 | -0.0631394 | -0.0895766 |  2.1316468 |  0.1576185 |    12.42922 |     -0.2235685 |     -0.0947847 |      0.5351371 |     -0.4324394 |    0.0642383 |  -0.1656675 |      -0.1634722 |       -0.074227 |      -1.3240743 |         -0.2070212 |         -0.1448419 |          0.3365019 |         -0.018509 |           -0.2532614 |         5.773193 |       -0.0694091 |           -1.647061 |       -0.2114791 |        -0.074227 |            -0.0586211 |           -0.1018855 |            -0.1959779 |            -0.1236896 |            -0.3172448 |            -0.1912176 |            -0.2667738 |            -0.2447291 |           -0.1813765 |            -0.1132868 |            -0.2015634 |          -0.4229141 |             6.3323719 |            -0.0891026 |            -0.2455142 |           -0.2167279 |            -0.2985775 |           -0.2335237 |            -0.2114791 |            -0.2578243 |            -0.1333279 |          -0.1292795 |           -0.1590004 |            -0.0910347 |        -0.2439421 |        0.4030253 |       -0.0830456 |       -0.1163487 |       -0.0983967 |       -0.1319913 |       -0.0453765 |       -0.0556033 |        -0.0668728 |        0.1018855 |        -0.037037 |        -0.037037 |        -0.018509 |        -0.018509 |       -0.0261802 |          -0.1472876 |         -0.1969181 |        -0.1843766 |         -0.2903361 |            -0.0809289 |            -1.0077380 |            -0.0524143 |            -0.0910347 |             1.5318854 |           -0.1710455 |         -0.2141168 |          0.5133674 |            -0.087129 |       -0.4822925 |           -0.0614929 |        -0.0414158 |           0.1222546 |           -0.018509 |         -0.018509 |        -0.018509 |          -0.0891026 |          -0.0556033 |          -0.0490206 |           -0.0261802 |           -0.0453765 |           -0.1752422 |          -0.0261802 |           -0.2123613 |           -0.4223512 |            -0.018509 |           -0.4268461 |           -0.2861546 |         -0.0261802 |          -0.1222546 |            1.3580858 |           -0.4047462 |           -0.1398328 |            -0.037037 |            -0.087129 |           -0.1279035 |          -0.0320695 |           -0.2123613 |           -0.4018763 |           -0.0718576 |           -0.4251627 |          -0.018509 |           -0.3192027 |         -0.0453765 |          -0.1279035 |            1.3693865 |           -0.3932108 |           -0.1689125 |               1.5231625 |           -1.2373891 |            -0.3053301 |      -0.1101443 |       1.4074569 |      -1.2662447 |      -0.1532457 |      -0.3377618 |      -0.0320695 |       0.3873845 |         -0.8562253 |         1.1096078 |        -0.130642 |        -0.0614929 |       -0.0414158 |     -0.1762775 |      1.1890784 |     -0.9364133 |     -0.2088138 |     -0.0414158 |      0.2931008 |          -0.323096 |         -0.2985775 |         -1.4587283 |            -0.3185509 |             1.5611942 |              -0.23596 |            -0.3307969 |             -0.683675 |            -0.1544121 |            -0.1085406 |            -0.1752422 |            -0.1931338 |             0.3666433 |      0.125109 |    -0.0966069 |    -0.0556033 |    -0.0261802 |    -0.0453765 |      -0.1803668 |      -0.4402256 |      -0.0320695 |      -0.6445724 |       0.2682439 |        -0.1319913 |        -0.0524143 |       -0.018509 |         0.3039876 |        -0.1567214 |         1.2391648 |         -1.023047 |       -0.0556033 |       -0.1508882 |       -0.1567214 |      -0.1101443 |      -0.0261802 |       0.2711665 |        -0.1612502 |        -1.6927029 |        -0.1265135 |         1.9822706 |            0.7432834 |            -0.1117261 |            -0.2608328 |            -0.0718576 |           -0.6032363 |           1.6119459 |          -0.9526448 |       -0.2105938 |       -0.0910347 |       -0.0414158 |        0.2375732 |       -0.1612502 |       -0.0718576 |       -0.0694091 |        0.1940858 |      -0.1472876 |       0.3243873 |   -0.0261802 |    -0.037037 |  -0.1997162 |    0.3006139 |  -0.0642383 |           -0.037037 |           0.0586211 |           -0.018509 |      -0.0414158 |        -0.0947847 |        -0.0556033 |        -0.0524143 |      -0.0642383 |      -0.2985775 |      -0.0490206 |      0.3943712 |               -0.0642383 |              -0.0910347 |              -0.1265135 |               0.4638573 |               -0.3026411 |
|     -0.1678767 |     0.7363903 |  0.5004295 |    -0.7724201 |    -0.5071973 |   0.7159521 |        0.5138006 |     -0.5721522 |      0.6378834 |     -0.2930798 |    -1.1303934 |      -0.5804016 |    -0.9266484 |     0.5353755 |         -0.1011797 |    -0.2742013 |        1.0868363 |       -0.2498524 |  -1.027187 |  1.2323877 |        -2.261142 |       -0.2076629 |         -0.9249036 | -0.9241529 |       0.5986302 |    0.3064753 |    0.0330864 |     -0.4244944 |      -0.2587744 |      -0.3595391 |   12.6010642 |    -0.2858865 | -0.0631394 |  1.1441161 |  1.3949339 |  0.9180953 |    11.87060 |     -0.2235685 |     -0.0947847 |      0.5351371 |     -0.4324394 |    0.0642383 |  -0.1656675 |      -0.1634722 |       -0.074227 |      -1.3240743 |         -0.2070212 |         -0.1448419 |          0.3365019 |         -0.018509 |           -0.2532614 |        -0.173155 |       -0.0694091 |            0.606934 |       -0.2114791 |        -0.074227 |            -0.0586211 |           -0.1018855 |            -0.1959779 |            -0.1236896 |            -0.3172448 |            -0.1912176 |            -0.2667738 |            -0.2447291 |           -0.1813765 |            -0.1132868 |             4.9595195 |          -0.4229141 |            -0.1578646 |            -0.0891026 |            -0.2455142 |           -0.2167279 |            -0.2985775 |           -0.2335237 |            -0.2114791 |            -0.2578243 |            -0.1333279 |          -0.1292795 |           -0.1590004 |            -0.0910347 |        -0.2439421 |        0.4030253 |       -0.0830456 |       -0.1163487 |       -0.0983967 |       -0.1319913 |       -0.0453765 |       -0.0556033 |        -0.0668728 |        0.1018855 |        -0.037037 |        -0.037037 |        -0.018509 |        -0.018509 |       -0.0261802 |          -0.1472876 |         -0.1969181 |        -0.1843766 |         -0.2903361 |            -0.0809289 |            -1.0077380 |            -0.0524143 |            -0.0910347 |            -0.6525667 |           -0.1710455 |         -0.2141168 |          0.5133674 |            -0.087129 |       -0.4822925 |           -0.0614929 |        -0.0414158 |           0.1222546 |           -0.018509 |         -0.018509 |        -0.018509 |          -0.0891026 |          -0.0556033 |          -0.0490206 |           -0.0261802 |           -0.0453765 |           -0.1752422 |          -0.0261802 |           -0.2123613 |           -0.4223512 |            -0.018509 |           -0.4268461 |           -0.2861546 |         -0.0261802 |          -0.1222546 |            1.3580858 |           -0.4047462 |           -0.1398328 |            -0.037037 |            -0.087129 |           -0.1279035 |          -0.0320695 |           -0.2123613 |           -0.4018763 |           -0.0718576 |           -0.4251627 |          -0.018509 |           -0.3192027 |         -0.0453765 |          -0.1279035 |            1.3693865 |           -0.3932108 |           -0.1689125 |              -0.6563038 |            0.8078764 |            -0.3053301 |      -0.1101443 |      -0.7102579 |       0.7894662 |      -0.1532457 |      -0.3377618 |      -0.0320695 |       0.3873845 |         -0.8562253 |        -0.9009106 |        -0.130642 |        -0.0614929 |       24.1371155 |     -0.1762775 |      1.1890784 |     -0.9364133 |     -0.2088138 |     -0.0414158 |      0.2931008 |          -0.323096 |         -0.2985775 |          0.6852938 |            -0.3185509 |             1.5611942 |              -0.23596 |            -0.3307969 |             -0.683675 |            -0.1544121 |            -0.1085406 |            -0.1752422 |            -0.1931338 |             0.3666433 |      0.125109 |    -0.0966069 |    -0.0556033 |    -0.0261802 |    -0.0453765 |      -0.1803668 |      -0.4402256 |      -0.0320695 |      -0.6445724 |       0.2682439 |        -0.1319913 |        -0.0524143 |       -0.018509 |         0.3039876 |        -0.1567214 |        -0.8067187 |          0.977137 |       -0.0556033 |       -0.1508882 |       -0.1567214 |      -0.1101443 |      -0.0261802 |       0.2711665 |        -0.1612502 |         0.5905687 |        -0.1265135 |        -0.5042992 |            0.7432834 |            -0.1117261 |            -0.2608328 |            -0.0718576 |           -0.6032363 |          -0.6201557 |           1.0493496 |       -0.2105938 |       -0.0910347 |       -0.0414158 |        0.2375732 |       -0.1612502 |       -0.0718576 |       -0.0694091 |        0.1940858 |      -0.1472876 |       0.3243873 |   -0.0261802 |    -0.037037 |  -0.1997162 |    0.3006139 |  -0.0642383 |           -0.037037 |           0.0586211 |           -0.018509 |      -0.0414158 |        -0.0947847 |        -0.0556033 |        -0.0524143 |      -0.0642383 |      -0.2985775 |      -0.0490206 |      0.3943712 |               -0.0642383 |              -0.0910347 |              -0.1265135 |               0.4638573 |               -0.3026411 |

Set `mixture` is equal to zero refering the Ridge regression in `glmnet`
since the

``` r
lasso_model <- linear_reg(penalty = 0.01, mixture = 1) %>% # lasso : 1, ridge = 0
               set_engine("glmnet")

lasso_fit <- lasso_model %>% fit(sale_price ~., data = train2)

options(max.print=10)

lasso_fit %>% tidy() %>% filter(estimate > 0.001)
```

    ## # A tibble: 34 x 3
    ##    term           estimate penalty
    ##    <chr>             <dbl>   <dbl>
    ##  1 (Intercept)    12.0        0.01
    ##  2 lot_area        0.00900    0.01
    ##  3 overall_qual    0.114      0.01
    ##  4 overall_cond    0.0317     0.01
    ##  5 year_built      0.0412     0.01
    ##  6 year_remod_add  0.0208     0.01
    ##  7 total_bsmt_sf   0.0222     0.01
    ##  8 x1st_flr_sf     0.00271    0.01
    ##  9 gr_liv_area     0.108      0.01
    ## 10 bsmt_full_bath  0.0175     0.01
    ## # ... with 24 more rows

# 6 Prediction and submit (예측 및 평가)

``` r
result <- predict(lasso_fit, test2)
result %>% head()
```

    ## # A tibble: 6 x 1
    ##   .pred
    ##   <dbl>
    ## 1  11.6
    ## 2  11.9
    ## 3  12.0
    ## 4  12.2
    ## 5  12.2
    ## 6  12.1

# 7 submission

``` r
submission <- read_csv(file.path(file_path, "sample_submission.csv"))
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Id = col_double(),
    ##   SalePrice = col_double()
    ## )

``` r
submission$SalePrice <- exp(result$.pred)
write.csv(submission, row.names=FALSE,
          "lasso_regression_0point1.csv")
```

# 8 본 분석은 슬기로운통계생활님의 영상을 참조하여 진행하였습니다.
