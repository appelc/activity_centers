---
title: "Activity center occupancy models"
author: "Cara"
date: "2023-09-19"
output: 
  html_document:
    toc: true
    toc_float: true
    toc_depth: 4
    keep_md: TRUE
---





--------------------------------------------------------------------------------

### Occupancy models for analyzing spotted owl activity center ("big grid") data from 2021 and 2022


--------------------------------------------------------------------------------
##
#### - Load detection histories  

```
##              V1 X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13 X14 X15 X16 X17 X18
##   1: 2021_DC_01 NA NA NA NA  0  0  0  0  0   0   0   0   0   0   0   0   0   0
##   2: 2021_DC_02 NA NA NA NA  0  0  0  0  0   0   0   0   0   0   0   0   0   0
##   3: 2021_DC_03 NA NA NA NA  0  0  0  0  0   0   0   0   0   0   0   0   0   0
##   4: 2021_DC_04 NA NA NA NA  0  0  0  0  0   0   0   0   0   0   0   0   0   0
##   5: 2021_DC_05 NA NA NA NA  0  0  0  0  0   0   0   0   0   0   0   0   0   0
##  ---                                                                          
## 287: 2022_LM_33 NA NA NA NA NA NA  0  0  0   0   0   0   0   0   0   1   1   0
## 288: 2022_LM_34 NA NA NA NA NA NA  0  0  0   0   0   0   0   0   0   0   0   0
## 289: 2022_LM_35 NA NA NA NA NA NA  0  0  0   0   0   0   0   0   0   0   0   0
## 290: 2022_LM_36 NA NA NA NA NA  0  0  0  0   1   1   0   1   0   1   1   1   1
## 291: 2022_LM_37 NA NA NA NA NA  0  0  0  1   1   1   0   1   0   0   1   1   0
##      X19 X20 X21 X22
##   1:   0   0  NA  NA
##   2:   0   0  NA  NA
##   3:   0   0  NA  NA
##   4:   0   0  NA  NA
##   5:   0   0  NA  NA
##  ---                
## 287:   1   0  NA  NA
## 288:   0   0  NA  NA
## 289:   0   0  NA  NA
## 290:   1   1  NA  NA
## 291:   1   1  NA  NA
```
  
  
## 
#### - Load site-level covariate data 

```
##     year_site site_name repro_state nested duration dist_actual
## 1: 2021_DC_01        DC     fledged      y      104    3018.404
## 2: 2021_DC_02        DC     fledged      y      104    2657.769
## 3: 2021_DC_03        DC     fledged      y      104    2605.506
## 4: 2021_DC_04        DC     fledged      y      104    3059.737
## 5: 2021_DC_05        DC     fledged      y      104    2643.332
## 6: 2021_DC_06        DC     fledged      y        7    2000.804
```

```
##     year_site site_name repro_state nested duration dist_actual
## 1: 2022_LM_32        LM        pair      n       97    2058.717
## 2: 2022_LM_33        LM        pair      n       96    2643.504
## 3: 2022_LM_34        LM        pair      n       96    3009.265
## 4: 2022_LM_35        LM        pair      n       96    2666.787
## 5: 2022_LM_36        LM        pair      n       97    2731.669
## 6: 2022_LM_37        LM        pair      n       97    2970.722
```


##
#### - Load observation-level covariate data




```r
  #effort (recording minutes per week)
    head(effort_covar)
```

```
##           1        2        3        4         5         6        7        8
## 1: 133384.9 133384.9 133384.9 133384.9  88109.29 138455.46 138455.5 138455.5
## 2: 133384.9 133384.9 133384.9 133384.9 107888.64 138455.46 138455.5 138455.5
## 3: 133384.9 133384.9 133384.9 133384.9 107888.64 138455.46 138455.5 138455.5
## 4: 133384.9 133384.9 133384.9 133384.9 107888.64 138455.46 138455.5 138455.5
## 5: 133384.9 133384.9 133384.9 133384.9  88109.29 138455.46 138455.5 138455.5
## 6: 133384.9 133384.9 133384.9 133384.9 107888.64  30566.82 133384.9 133384.9
##           9       10       11       12       13       14       15       16
## 1: 138455.5 138455.5 138455.5 139176.3 141754.6 142645.1 142645.1 143485.3
## 2: 138455.5 138455.5 138455.5 139115.9 141754.6 142645.1 142645.1 143485.3
## 3: 138455.5 138455.5 138455.5 139115.9 141754.6 142645.1 142645.1 143485.3
## 4: 138455.5 138455.5 138455.5 139056.0 141754.6 142645.1 142645.1 143545.2
## 5: 138455.5 138455.5 138455.5 139236.2 141753.1 142645.1 142645.1 143485.3
## 6: 133384.9 133384.9 133384.9 133384.9 133384.9 133384.9 133384.9 133384.9
##          17       18       19        20       21       22
## 1: 146658.1 146834.8 146834.8  32362.40 133384.9 133384.9
## 2: 146658.1 146834.8 146834.8  11385.99 133384.9 133384.9
## 3: 146658.1 146834.8 146834.8  11385.99 133384.9 133384.9
## 4: 146658.1 146834.8 146834.8  11385.99 133384.9 133384.9
## 5: 146658.1 146834.8 146834.8  32362.40 133384.9 133384.9
## 6: 133384.9 133384.9 133384.9 133384.94 133384.9 133384.9
```

```r
  #noise (mean SPL per week)
    head(noise_covar)
```

```
##            1         2         3         4         5         6         7
## 1: -61.09267 -61.09267 -61.09267 -61.09267 -49.54984 -49.97800 -45.82109
## 2: -61.09267 -61.09267 -61.09267 -61.09267 -58.50847 -61.93878 -55.10000
## 3: -61.09267 -61.09267 -61.09267 -61.09267 -60.87196 -65.74785 -56.70839
## 4: -61.09267 -61.09267 -61.09267 -61.09267 -59.53730 -62.37211 -56.08639
## 5: -61.09267 -61.09267 -61.09267 -61.09267 -50.04222 -52.46689 -48.75896
## 6: -61.09267 -61.09267 -61.09267 -61.09267 -58.88757 -66.44405 -61.09267
##            8         9        10        11        12        13        14
## 1: -50.11451 -52.10952 -53.80295 -52.50045 -54.06939 -54.28941 -54.68429
## 2: -63.20544 -62.36599 -62.00816 -63.43243 -64.53515 -65.73211 -67.15082
## 3: -65.47098 -66.65125 -64.39093 -65.74036 -64.74104 -65.30467 -70.20082
## 4: -63.17551 -64.06327 -64.51927 -64.47982 -64.33039 -63.89039 -66.96816
## 5: -52.13651 -53.59002 -52.05125 -53.02993 -55.09274 -55.92415 -55.18469
## 6: -61.09267 -61.09267 -61.09267 -61.09267 -61.09267 -61.09267 -61.09267
##           15        16        17        18        19        20        21
## 1: -53.64224 -55.29388 -55.45544 -54.09926 -55.53154 -55.92848 -61.09267
## 2: -66.19673 -67.52143 -67.80056 -66.31967 -67.76865 -68.71633 -61.09267
## 3: -68.03102 -70.19020 -67.15670 -64.67236 -68.98776 -67.15306 -61.09267
## 4: -65.25551 -67.81102 -65.60334 -64.94378 -66.89425 -62.62041 -61.09267
## 5: -54.60204 -56.09122 -57.04267 -56.04230 -55.31002 -55.04351 -61.09267
## 6: -61.09267 -61.09267 -61.09267 -61.09267 -61.09267 -61.09267 -61.09267
##           22
## 1: -61.09267
## 2: -61.09267
## 3: -61.09267
## 4: -61.09267
## 5: -61.09267
## 6: -61.09267
```

```r
  #barred owl (total detections per week)
    head(stva_t_covar)
```

```
##           1        2        3        4   5  6         7        8        9
## 1: 29.04701 29.04701 29.04701 29.04701   0  3  10.00000  7.00000 12.00000
## 2: 29.04701 29.04701 29.04701 29.04701  71 79  25.00000 79.00000 41.00000
## 3: 29.04701 29.04701 29.04701 29.04701  31 26  27.00000 60.00000 30.00000
## 4: 29.04701 29.04701 29.04701 29.04701  45 25  13.00000 73.00000 60.00000
## 5: 29.04701 29.04701 29.04701 29.04701  57 52 110.00000 61.00000  2.00000
## 6: 29.04701 29.04701 29.04701 29.04701 104 30  29.04701 29.04701 29.04701
##          10       11       12       13       14       15       16       17
## 1: 19.00000  1.00000  8.00000  0.00000  0.00000  0.00000  9.00000  3.00000
## 2: 49.00000 68.00000 71.00000 23.00000 35.00000 40.00000 43.00000 63.00000
## 3: 69.00000 93.00000 45.00000 39.00000 46.00000 27.00000 38.00000  6.00000
## 4: 53.00000 33.00000 22.00000 17.00000 23.00000 22.00000 57.00000 26.00000
## 5:  4.00000  3.00000  3.00000  1.00000  7.00000 68.00000 50.00000 61.00000
## 6: 29.04701 29.04701 29.04701 29.04701 29.04701 29.04701 29.04701 29.04701
##          18       19       20       21       22
## 1: 10.00000  4.00000  0.00000 29.04701 29.04701
## 2: 70.00000 51.00000  4.00000 29.04701 29.04701
## 3: 14.00000  8.00000  0.00000 29.04701 29.04701
## 4: 27.00000 76.00000  0.00000 29.04701 29.04701
## 5: 23.00000 16.00000 11.00000 29.04701 29.04701
## 6: 29.04701 29.04701 29.04701 29.04701 29.04701
```

```r
  #barred owl (days with detections per week)
    head(stva_d_covar)
```

```
##           1        2        3        4 5 6        7        8        9       10
## 1: 2.425979 2.425979 2.425979 2.425979 0 3 3.000000 2.000000 2.000000 1.000000
## 2: 2.425979 2.425979 2.425979 2.425979 4 7 4.000000 6.000000 6.000000 4.000000
## 3: 2.425979 2.425979 2.425979 2.425979 3 5 4.000000 6.000000 5.000000 4.000000
## 4: 2.425979 2.425979 2.425979 2.425979 3 5 2.000000 7.000000 6.000000 5.000000
## 5: 2.425979 2.425979 2.425979 2.425979 3 6 5.000000 5.000000 1.000000 3.000000
## 6: 2.425979 2.425979 2.425979 2.425979 5 2 2.425979 2.425979 2.425979 2.425979
##          11       12       13       14       15       16       17       18
## 1: 1.000000 1.000000 0.000000 0.000000 0.000000 1.000000 1.000000 2.000000
## 2: 4.000000 7.000000 6.000000 5.000000 5.000000 5.000000 7.000000 5.000000
## 3: 4.000000 4.000000 6.000000 6.000000 6.000000 6.000000 2.000000 4.000000
## 4: 4.000000 4.000000 4.000000 4.000000 6.000000 4.000000 5.000000 6.000000
## 5: 1.000000 1.000000 1.000000 3.000000 4.000000 4.000000 6.000000 3.000000
## 6: 2.425979 2.425979 2.425979 2.425979 2.425979 2.425979 2.425979 2.425979
##          19       20       21       22
## 1: 1.000000 0.000000 2.425979 2.425979
## 2: 4.000000 1.000000 2.425979 2.425979
## 3: 3.000000 0.000000 2.425979 2.425979
## 4: 7.000000 0.000000 2.425979 2.425979
## 5: 5.000000 2.000000 2.425979 2.425979
## 6: 2.425979 2.425979 2.425979 2.425979
```


##  
#### - Create input files 



## 
#### - Run and compare models (constant psi) 


##### - All detections: no time component
<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:600px; "><table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2181.838 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2190.630 </td>
   <td style="text-align:right;"> 8.792 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> ~scale(dist) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2207.091 </td>
   <td style="text-align:right;"> 25.253 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + site ~ 1 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2216.546 </td>
   <td style="text-align:right;"> 34.708 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~scale(dist) + site ~ 1 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2237.511 </td>
   <td style="text-align:right;"> 55.673 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + site ~ 1 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2238.965 </td>
   <td style="text-align:right;"> 57.127 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2279.626 </td>
   <td style="text-align:right;"> 97.788 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2281.597 </td>
   <td style="text-align:right;"> 99.759 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2281.635 </td>
   <td style="text-align:right;"> 99.797 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + nest ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2301.742 </td>
   <td style="text-align:right;"> 119.904 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2303.950 </td>
   <td style="text-align:right;"> 122.112 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2316.176 </td>
   <td style="text-align:right;"> 134.337 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2337.764 </td>
   <td style="text-align:right;"> 155.925 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> ~scale(dist) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2339.702 </td>
   <td style="text-align:right;"> 157.864 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2352.971 </td>
   <td style="text-align:right;"> 171.132 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~scale(dist) ~ 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2364.041 </td>
   <td style="text-align:right;"> 182.203 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> ~scale(dist) + nest ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2365.948 </td>
   <td style="text-align:right;"> 184.110 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~1 ~ 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2459.959 </td>
   <td style="text-align:right;"> 278.121 </td>
  </tr>
</tbody>
</table></div>

##### - All detections: 'week' as factor
<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:600px; "><table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2189.461 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2202.061 </td>
   <td style="text-align:right;"> 12.600 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + site ~ 1 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2221.171 </td>
   <td style="text-align:right;"> 31.711 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> ~week + scale(dist) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2224.423 </td>
   <td style="text-align:right;"> 34.963 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~week + scale(dist) + site ~ 1 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2248.118 </td>
   <td style="text-align:right;"> 58.658 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + site ~ 1 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2248.688 </td>
   <td style="text-align:right;"> 59.227 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2270.827 </td>
   <td style="text-align:right;"> 81.367 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2285.409 </td>
   <td style="text-align:right;"> 95.949 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2286.039 </td>
   <td style="text-align:right;"> 96.578 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + nest ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2300.763 </td>
   <td style="text-align:right;"> 111.303 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2301.855 </td>
   <td style="text-align:right;"> 112.395 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2332.851 </td>
   <td style="text-align:right;"> 143.390 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2356.721 </td>
   <td style="text-align:right;"> 167.261 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> ~week + scale(dist) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2358.702 </td>
   <td style="text-align:right;"> 169.242 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) ~ 1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2360.320 </td>
   <td style="text-align:right;"> 170.859 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~week + scale(dist) ~ 1 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2375.250 </td>
   <td style="text-align:right;"> 185.789 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> ~week + scale(dist) + nest ~ 1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2377.248 </td>
   <td style="text-align:right;"> 187.787 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~week ~ 1 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2471.580 </td>
   <td style="text-align:right;"> 282.120 </td>
  </tr>
</tbody>
</table></div>

##### - All detections: linear time trend
<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:600px; "><table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2171.524 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2184.096 </td>
   <td style="text-align:right;"> 12.572 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> ~time + scale(dist) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2208.501 </td>
   <td style="text-align:right;"> 36.977 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + site ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2210.782 </td>
   <td style="text-align:right;"> 39.258 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~time + scale(dist) + site ~ 1 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2239.370 </td>
   <td style="text-align:right;"> 67.846 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + site ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2240.819 </td>
   <td style="text-align:right;"> 69.295 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2264.314 </td>
   <td style="text-align:right;"> 92.790 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2266.900 </td>
   <td style="text-align:right;"> 95.376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2267.820 </td>
   <td style="text-align:right;"> 96.296 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + nest ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2290.138 </td>
   <td style="text-align:right;"> 118.614 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2291.677 </td>
   <td style="text-align:right;"> 120.153 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2318.165 </td>
   <td style="text-align:right;"> 146.641 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2339.705 </td>
   <td style="text-align:right;"> 168.181 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> ~time + scale(dist) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2341.618 </td>
   <td style="text-align:right;"> 170.094 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2354.727 </td>
   <td style="text-align:right;"> 183.203 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~time + scale(dist) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2365.714 </td>
   <td style="text-align:right;"> 194.190 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> ~time + scale(dist) + nest ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2367.562 </td>
   <td style="text-align:right;"> 196.038 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~time ~ 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2461.506 </td>
   <td style="text-align:right;"> 289.982 </td>
  </tr>
</tbody>
</table></div>

##### - All detections: quadratic time trend
<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:600px; "><table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2172.286 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2184.440 </td>
   <td style="text-align:right;"> 12.154 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + site ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2204.509 </td>
   <td style="text-align:right;"> 32.223 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2209.370 </td>
   <td style="text-align:right;"> 37.084 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + site ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2233.896 </td>
   <td style="text-align:right;"> 61.610 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + site ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2235.194 </td>
   <td style="text-align:right;"> 62.908 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2256.916 </td>
   <td style="text-align:right;"> 84.630 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2267.365 </td>
   <td style="text-align:right;"> 95.079 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2268.260 </td>
   <td style="text-align:right;"> 95.974 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + nest ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2284.296 </td>
   <td style="text-align:right;"> 112.009 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2285.610 </td>
   <td style="text-align:right;"> 113.324 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2318.808 </td>
   <td style="text-align:right;"> 146.522 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2340.062 </td>
   <td style="text-align:right;"> 167.776 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2342.060 </td>
   <td style="text-align:right;"> 169.774 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2346.863 </td>
   <td style="text-align:right;"> 174.576 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2359.173 </td>
   <td style="text-align:right;"> 186.887 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + nest ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2361.104 </td>
   <td style="text-align:right;"> 188.818 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~poly(time, 2) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2454.408 </td>
   <td style="text-align:right;"> 282.122 </td>
  </tr>
</tbody>
</table></div>

##### - All detections: top models viewed together
<table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2171.524 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2172.286 </td>
   <td style="text-align:right;"> 0.762 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2181.838 </td>
   <td style="text-align:right;"> 10.314 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2189.461 </td>
   <td style="text-align:right;"> 17.937 </td>
  </tr>
</tbody>
</table>


##


##### - Female detections: no time component
<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:600px; "><table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1219.305 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1225.741 </td>
   <td style="text-align:right;"> 6.435 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + site ~ 1 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1236.555 </td>
   <td style="text-align:right;"> 17.249 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> ~scale(dist) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1240.784 </td>
   <td style="text-align:right;"> 21.479 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1250.530 </td>
   <td style="text-align:right;"> 31.225 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~scale(dist) + site ~ 1 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1255.454 </td>
   <td style="text-align:right;"> 36.148 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + site ~ 1 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1256.637 </td>
   <td style="text-align:right;"> 37.332 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1259.133 </td>
   <td style="text-align:right;"> 39.827 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1259.532 </td>
   <td style="text-align:right;"> 40.226 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1269.026 </td>
   <td style="text-align:right;"> 49.721 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + nest ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1269.537 </td>
   <td style="text-align:right;"> 50.232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1277.533 </td>
   <td style="text-align:right;"> 58.227 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> ~scale(dist) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1292.479 </td>
   <td style="text-align:right;"> 73.173 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1293.989 </td>
   <td style="text-align:right;"> 74.684 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1298.989 </td>
   <td style="text-align:right;"> 79.684 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> ~scale(dist) + nest ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1306.333 </td>
   <td style="text-align:right;"> 87.028 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~scale(dist) ~ 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1307.795 </td>
   <td style="text-align:right;"> 88.489 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~1 ~ 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1326.057 </td>
   <td style="text-align:right;"> 106.752 </td>
  </tr>
</tbody>
</table></div>

##### - Female detections: 'week' as factor
<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:600px; "><table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1226.705 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1236.385 </td>
   <td style="text-align:right;"> 9.680 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + site ~ 1 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1241.809 </td>
   <td style="text-align:right;"> 15.104 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1250.600 </td>
   <td style="text-align:right;"> 23.895 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> ~week + scale(dist) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1258.414 </td>
   <td style="text-align:right;"> 31.709 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + site ~ 1 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1265.077 </td>
   <td style="text-align:right;"> 38.372 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~week + scale(dist) + site ~ 1 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1265.788 </td>
   <td style="text-align:right;"> 39.083 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1270.233 </td>
   <td style="text-align:right;"> 43.528 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1270.740 </td>
   <td style="text-align:right;"> 44.035 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1276.044 </td>
   <td style="text-align:right;"> 49.339 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + nest ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1276.741 </td>
   <td style="text-align:right;"> 50.035 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1292.382 </td>
   <td style="text-align:right;"> 65.677 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) ~ 1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1303.801 </td>
   <td style="text-align:right;"> 77.096 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> ~week + scale(dist) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1309.679 </td>
   <td style="text-align:right;"> 82.973 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1311.898 </td>
   <td style="text-align:right;"> 85.193 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> ~week + scale(dist) + nest ~ 1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1316.286 </td>
   <td style="text-align:right;"> 89.581 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~week + scale(dist) ~ 1 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1318.306 </td>
   <td style="text-align:right;"> 91.601 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~week ~ 1 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1336.561 </td>
   <td style="text-align:right;"> 109.856 </td>
  </tr>
</tbody>
</table></div>

##### - Female detections: linear time trend
<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:600px; "><table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1202.562 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1212.182 </td>
   <td style="text-align:right;"> 9.620 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + site ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1224.656 </td>
   <td style="text-align:right;"> 22.094 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1234.994 </td>
   <td style="text-align:right;"> 32.432 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> ~time + scale(dist) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1241.485 </td>
   <td style="text-align:right;"> 38.924 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1245.209 </td>
   <td style="text-align:right;"> 42.647 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1246.062 </td>
   <td style="text-align:right;"> 43.500 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1256.607 </td>
   <td style="text-align:right;"> 54.046 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~time + scale(dist) + site ~ 1 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1257.084 </td>
   <td style="text-align:right;"> 54.522 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + nest ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1257.670 </td>
   <td style="text-align:right;"> 55.108 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + site ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1258.318 </td>
   <td style="text-align:right;"> 55.756 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1279.523 </td>
   <td style="text-align:right;"> 76.962 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> ~time + scale(dist) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1294.469 </td>
   <td style="text-align:right;"> 91.907 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1295.988 </td>
   <td style="text-align:right;"> 93.426 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1300.701 </td>
   <td style="text-align:right;"> 98.139 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> ~time + scale(dist) + nest ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1308.199 </td>
   <td style="text-align:right;"> 105.637 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~time + scale(dist) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1309.629 </td>
   <td style="text-align:right;"> 107.067 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~time ~ 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1328.045 </td>
   <td style="text-align:right;"> 125.483 </td>
  </tr>
</tbody>
</table></div>

##### - Female detections: quadratic time trend
<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:600px; "><table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1201.968 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1210.770 </td>
   <td style="text-align:right;"> 8.802 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + site ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1216.943 </td>
   <td style="text-align:right;"> 14.975 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1227.270 </td>
   <td style="text-align:right;"> 25.302 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1239.813 </td>
   <td style="text-align:right;"> 37.845 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1244.515 </td>
   <td style="text-align:right;"> 42.547 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1245.291 </td>
   <td style="text-align:right;"> 43.324 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + site ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1248.400 </td>
   <td style="text-align:right;"> 46.432 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + site ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1249.205 </td>
   <td style="text-align:right;"> 47.237 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1250.245 </td>
   <td style="text-align:right;"> 48.277 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + nest ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1251.077 </td>
   <td style="text-align:right;"> 49.109 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1276.233 </td>
   <td style="text-align:right;"> 74.265 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1287.465 </td>
   <td style="text-align:right;"> 85.497 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1290.787 </td>
   <td style="text-align:right;"> 88.819 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1292.487 </td>
   <td style="text-align:right;"> 90.519 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + nest ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1296.773 </td>
   <td style="text-align:right;"> 94.805 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1298.605 </td>
   <td style="text-align:right;"> 96.637 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~poly(time, 2) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1316.745 </td>
   <td style="text-align:right;"> 114.777 </td>
  </tr>
</tbody>
</table></div>


##### - Female detections: top models viewed together
<table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1201.968 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1202.562 </td>
   <td style="text-align:right;"> 0.594 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1219.305 </td>
   <td style="text-align:right;"> 17.338 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1226.705 </td>
   <td style="text-align:right;"> 24.737 </td>
  </tr>
</tbody>
</table>


## 

```
## Hessian is singular.
## Hessian is singular.
```

##### - Male detections: no time component
<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:600px; "><table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1884.615 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1888.942 </td>
   <td style="text-align:right;"> 4.327 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> ~scale(dist) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1892.932 </td>
   <td style="text-align:right;"> 8.317 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + site ~ 1 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1906.067 </td>
   <td style="text-align:right;"> 21.452 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~scale(dist) + site ~ 1 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1912.352 </td>
   <td style="text-align:right;"> 27.737 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + site ~ 1 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1913.615 </td>
   <td style="text-align:right;"> 29.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1950.666 </td>
   <td style="text-align:right;"> 66.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1958.532 </td>
   <td style="text-align:right;"> 73.917 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1962.620 </td>
   <td style="text-align:right;"> 78.005 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1965.788 </td>
   <td style="text-align:right;"> 81.173 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> ~scale(dist) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1966.170 </td>
   <td style="text-align:right;"> 81.556 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1966.556 </td>
   <td style="text-align:right;"> 81.941 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) + nest ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1973.232 </td>
   <td style="text-align:right;"> 88.617 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1974.349 </td>
   <td style="text-align:right;"> 89.735 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1977.828 </td>
   <td style="text-align:right;"> 93.213 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> ~scale(dist) + nest ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1982.564 </td>
   <td style="text-align:right;"> 97.949 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~scale(dist) ~ 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1983.071 </td>
   <td style="text-align:right;"> 98.456 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~1 ~ 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2079.177 </td>
   <td style="text-align:right;"> 194.563 </td>
  </tr>
</tbody>
</table></div>

##### - Male detections: 'week' as factor
<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:600px; "><table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1885.954 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1893.908 </td>
   <td style="text-align:right;"> 7.954 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + site ~ 1 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1906.716 </td>
   <td style="text-align:right;"> 20.761 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> ~week + scale(dist) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1909.133 </td>
   <td style="text-align:right;"> 23.178 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~week + scale(dist) + site ~ 1 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1925.807 </td>
   <td style="text-align:right;"> 39.853 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + site ~ 1 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1926.528 </td>
   <td style="text-align:right;"> 40.574 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1962.433 </td>
   <td style="text-align:right;"> 76.479 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1964.820 </td>
   <td style="text-align:right;"> 78.865 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1965.819 </td>
   <td style="text-align:right;"> 79.865 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1968.412 </td>
   <td style="text-align:right;"> 82.458 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) + nest ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1975.728 </td>
   <td style="text-align:right;"> 89.774 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1980.296 </td>
   <td style="text-align:right;"> 94.341 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1983.266 </td>
   <td style="text-align:right;"> 97.312 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> ~week + scale(dist) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1984.364 </td>
   <td style="text-align:right;"> 98.410 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) ~ 1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1985.457 </td>
   <td style="text-align:right;"> 99.502 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~week + scale(dist) ~ 1 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1996.484 </td>
   <td style="text-align:right;"> 110.530 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> ~week + scale(dist) + nest ~ 1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1997.341 </td>
   <td style="text-align:right;"> 111.387 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~week ~ 1 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2093.741 </td>
   <td style="text-align:right;"> 207.787 </td>
  </tr>
</tbody>
</table></div>

##### - Male detections:linear time trend
<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:600px; "><table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1872.466 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1879.975 </td>
   <td style="text-align:right;"> 7.510 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> ~time + scale(dist) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1891.524 </td>
   <td style="text-align:right;"> 19.058 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + site ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1897.782 </td>
   <td style="text-align:right;"> 25.316 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~time + scale(dist) + site ~ 1 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1912.064 </td>
   <td style="text-align:right;"> 39.598 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + site ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1913.285 </td>
   <td style="text-align:right;"> 40.819 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1949.114 </td>
   <td style="text-align:right;"> 76.648 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1949.577 </td>
   <td style="text-align:right;"> 77.111 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1953.170 </td>
   <td style="text-align:right;"> 80.704 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1953.915 </td>
   <td style="text-align:right;"> 81.449 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) + nest ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1965.290 </td>
   <td style="text-align:right;"> 92.824 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1965.826 </td>
   <td style="text-align:right;"> 93.360 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> ~time + scale(dist) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1966.232 </td>
   <td style="text-align:right;"> 93.766 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1969.832 </td>
   <td style="text-align:right;"> 97.366 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1974.199 </td>
   <td style="text-align:right;"> 101.733 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~time + scale(dist) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1983.332 </td>
   <td style="text-align:right;"> 110.866 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> ~time + scale(dist) + nest ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1983.388 </td>
   <td style="text-align:right;"> 110.922 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~time ~ 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2080.243 </td>
   <td style="text-align:right;"> 207.777 </td>
  </tr>
</tbody>
</table></div>

##### - Male detections:quadratic time trend
<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:600px; "><table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1873.652 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1880.852 </td>
   <td style="text-align:right;"> 7.200 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + site + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1892.835 </td>
   <td style="text-align:right;"> 19.184 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + site ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1894.361 </td>
   <td style="text-align:right;"> 20.709 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + site ~ 1 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1909.444 </td>
   <td style="text-align:right;"> 35.793 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + site ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1910.595 </td>
   <td style="text-align:right;"> 36.943 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1950.457 </td>
   <td style="text-align:right;"> 76.805 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1950.490 </td>
   <td style="text-align:right;"> 76.838 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1950.770 </td>
   <td style="text-align:right;"> 77.118 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1954.327 </td>
   <td style="text-align:right;"> 80.676 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) + nest ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1962.771 </td>
   <td style="text-align:right;"> 89.120 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1966.932 </td>
   <td style="text-align:right;"> 93.281 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(noise) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1966.974 </td>
   <td style="text-align:right;"> 93.323 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + nest + scale(effort) ~ 1 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1967.398 </td>
   <td style="text-align:right;"> 93.747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1970.534 </td>
   <td style="text-align:right;"> 96.883 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) ~ 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1980.306 </td>
   <td style="text-align:right;"> 106.654 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + nest ~ 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1980.611 </td>
   <td style="text-align:right;"> 106.959 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~poly(time, 2) ~ 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 2076.593 </td>
   <td style="text-align:right;"> 202.942 </td>
  </tr>
</tbody>
</table></div>
    
##### - Male detections: top models viewed together
<table class="table table-striped table-hover table-responsive" style="font-size: 14px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> formula </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> nPars </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> AIC </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ~time + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1872.466 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> ~poly(time, 2) + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1873.652 </td>
   <td style="text-align:right;"> 1.186 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ~scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1884.615 </td>
   <td style="text-align:right;"> 12.149 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ~week + scale(dist) + scale(bo_d) + scale(noise) + scale(effort) + site ~ 1 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 291 </td>
   <td style="text-align:right;"> 1885.954 </td>
   <td style="text-align:right;"> 13.488 </td>
  </tr>
</tbody>
</table>

## 
#### - View parameter estimates from top models above 
#### (using linear trend on time here --- simpler than quadratic and within 2 delta AIC)

```r
# All detections
    p18_any_time
```

```
## 
## Call:
## occu(formula = ~time + scale(dist) + scale(bo_d) + scale(noise) + 
##     scale(effort) + site ~ 1, data = input_any)
## 
## Occupancy:
##  Estimate    SE     z P(>|z|)
##    -0.187 0.154 -1.21   0.225
## 
## Detection:
##               Estimate     SE       z  P(>|z|)
## (Intercept)    -1.1479 0.1875  -6.122 9.27e-10
## time           -0.0465 0.0134  -3.476 5.09e-04
## scale(dist)    -0.7876 0.0663 -11.885 1.42e-32
## scale(bo_d)    -0.2235 0.0592  -3.777 1.59e-04
## scale(noise)   -0.5078 0.0882  -5.756 8.62e-09
## scale(effort)   0.3702 0.0723   5.122 3.02e-07
## siteCC         -2.8385 0.5631  -5.041 4.64e-07
## siteDC         -0.3425 0.1915  -1.789 7.36e-02
## siteLM          0.4448 0.2082   2.137 3.26e-02
## siteMC          0.0390 0.2936   0.133 8.94e-01
## siteUG          0.3076 0.1801   1.708 8.76e-02
## siteWC         -0.7945 0.2775  -2.863 4.19e-03
## 
## AIC: 2171.524
```

```r
# Females
    p18_fem_time
```

```
## 
## Call:
## occu(formula = ~time + scale(dist) + scale(bo_d) + scale(noise) + 
##     scale(effort) + site ~ 1, data = input_female)
## 
## Occupancy:
##  Estimate    SE     z P(>|z|)
##    -0.895 0.209 -4.29 1.8e-05
## 
## Detection:
##               Estimate     SE      z  P(>|z|)
## (Intercept)    -1.0169 0.2755 -3.691 2.24e-04
## time           -0.0868 0.0205 -4.228 2.35e-05
## scale(dist)    -0.7916 0.0950 -8.331 8.00e-17
## scale(bo_d)    -0.3111 0.0925 -3.365 7.66e-04
## scale(noise)   -0.8195 0.1367 -5.994 2.05e-09
## scale(effort)   0.3702 0.0985  3.759 1.71e-04
## siteCC         -2.2919 0.6941 -3.302 9.59e-04
## siteDC         -0.6474 0.2763 -2.343 1.91e-02
## siteLM         -0.0826 0.2668 -0.310 7.57e-01
## siteMC         -0.4232 0.5597 -0.756 4.50e-01
## siteUG         -0.8692 0.4359 -1.994 4.62e-02
## siteWC         -1.2430 0.4245 -2.928 3.41e-03
## 
## AIC: 1202.562
```

```r
# Males
    p18_mal_time
```

```
## 
## Call:
## occu(formula = ~time + scale(dist) + scale(bo_d) + scale(noise) + 
##     scale(effort) + site ~ 1, data = input_male)
## 
## Occupancy:
##  Estimate    SE      z P(>|z|)
##   -0.0791 0.189 -0.418   0.676
## 
## Detection:
##               Estimate      SE       z  P(>|z|)
## (Intercept)    -1.3507  0.2032  -6.648 2.98e-11
## time           -0.0543  0.0146  -3.721 1.98e-04
## scale(dist)    -0.8375  0.0735 -11.399 4.23e-30
## scale(bo_d)    -0.1981  0.0649  -3.051 2.28e-03
## scale(noise)   -0.4237  0.0980  -4.325 1.53e-05
## scale(effort)   0.3396  0.0815   4.168 3.07e-05
## siteCC        -12.3826 67.3705  -0.184 8.54e-01
## siteDC         -0.5909  0.2215  -2.668 7.63e-03
## siteLM          0.0465  0.2327   0.200 8.42e-01
## siteMC         -0.3140  0.3301  -0.951 3.42e-01
## siteUG          0.4672  0.1883   2.481 1.31e-02
## siteWC         -1.1920  0.3056  -3.900 9.62e-05
## 
## AIC: 1872.466
```


##
#### - Make predictions on covariate values

   
### Create marginal plots (by site): p(time + dist + noise + site + effort)

##

##
#### Create marginal plots (grouped): p(dist)


##
#### Create marginal plots (by nest status): p(dist + nest)


##
#### Create marginal plots (grouped): p(dist + time)
  


## 
#### Create marginal plots (by nest status): p(dist + nest + time)


