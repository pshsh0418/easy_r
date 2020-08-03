나이와 월급의 관계
================
박세화
July 30, 2020

## 3\. 나이와 월급의 관계

나이에 따라 월급이 어떻게 다른지 데이터 분석을 통해 알아본다. 먼저 나이 변수를 검토하고 전처리한다.

### 분석 절차

  - 1단계 : 변수 검토 및 전처리 (나이, 월급)
  - 2단계 : 변수 간 관계 분석 (나이에 따른 월급 평균표 만들기, 그래프 만들기)

#### 1\. 변수 검토하기

한국복지패널데이터에 나이 변수가 없으므로 태어난 연도를 이용하여 나이 변수를 생성한다. 먼저 태어난 연도 변수를 검토한다.

``` r
class(welfare$birth)
```

    ## [1] "numeric"

``` r
summary(welfare$birth)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1907    1946    1966    1968    1988    2014

``` r
qplot(welfare$birth)
```

![](welfare03_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

#### 2\. 전처리

``` r
summary(welfare$birth)                                         # 이상치 확인
table(is.na(welfare$birth))                                    # 결측치 확인
welfare$birth <- ifelse(welfare$birth==9999,NA,welfare$birth)  # 이상치 결측 처리
table(is.na(welfare$birth))
```

#### 3\. 파생변수 만들기 - 나이

2015년에 조사가 진행됐으므로 2015년에서 태어난 연도를 뺀 후 1을 더해 나이를 구한다.

``` r
welfare$age <- 2015 - welfare$birth +1
summary(welfare$age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    2.00   28.00   50.00   48.43   70.00  109.00

``` r
qplot(welfare$age)
```

![](welfare03_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### 나이와 월급의 관계 분석하기

전처리 작업이 끝났으니 나이에 따른 월급을 분석한다.

#### 1\. 나이에 따른 월급 평균표 만들기

먼저 나이별 월급 평균표를 만든다.

``` r
age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income=mean(income))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
head(age_income)
```

    ## # A tibble: 6 x 2
    ##     age mean_income
    ##   <dbl>       <dbl>
    ## 1    20        121.
    ## 2    21        106.
    ## 3    22        130.
    ## 4    23        142.
    ## 5    24        134.
    ## 6    25        145.

#### 2\. 그래프 만들기

``` r
ggplot(data=age_income,aes(x=age,y=mean_income))+geom_line()
```

![](welfare03_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
