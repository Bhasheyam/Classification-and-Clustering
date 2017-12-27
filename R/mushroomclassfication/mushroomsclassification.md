Mushroom classification
================
Bhasheyam Krishnan

loading the dataset

``` r
mushroombase = read.csv(file = "mushrooms.csv" )
```

Basic Info About the dataset

``` r
names(mushroombase)
```

    ##  [1] "class"                    "cap.shape"               
    ##  [3] "cap.surface"              "cap.color"               
    ##  [5] "bruises"                  "odor"                    
    ##  [7] "gill.attachment"          "gill.spacing"            
    ##  [9] "gill.size"                "gill.color"              
    ## [11] "stalk.shape"              "stalk.root"              
    ## [13] "stalk.surface.above.ring" "stalk.surface.below.ring"
    ## [15] "stalk.color.above.ring"   "stalk.color.below.ring"  
    ## [17] "veil.type"                "veil.color"              
    ## [19] "ring.number"              "ring.type"               
    ## [21] "spore.print.color"        "population"              
    ## [23] "habitat"

``` r
dim(mushroombase)
```

    ## [1] 8124   23

The Dataset has 23 Features and 8124 Instance

``` r
sum(is.na(mushroombase))
```

    ## [1] 0

``` r
sum(is.null(mushroombase))
```

    ## [1] 0

There is no NA and null in Dataset, so we dont have missing values

``` r
summary(mushroombase)
```

    ##  class    cap.shape cap.surface   cap.color    bruises       odor     
    ##  e:4208   b: 452    f:2320      n      :2284   f:4748   n      :3528  
    ##  p:3916   c:   4    g:   4      g      :1840   t:3376   f      :2160  
    ##           f:3152    s:2556      e      :1500            s      : 576  
    ##           k: 828    y:3244      y      :1072            y      : 576  
    ##           s:  32                w      :1040            a      : 400  
    ##           x:3656                b      : 168            l      : 400  
    ##                                 (Other): 220            (Other): 484  
    ##  gill.attachment gill.spacing gill.size   gill.color   stalk.shape
    ##  a: 210          c:6812       b:5612    b      :1728   e:3516     
    ##  f:7914          w:1312       n:2512    p      :1492   t:4608     
    ##                                         w      :1202              
    ##                                         n      :1048              
    ##                                         g      : 752              
    ##                                         h      : 732              
    ##                                         (Other):1170              
    ##  stalk.root stalk.surface.above.ring stalk.surface.below.ring
    ##  ?:2480     f: 552                   f: 600                  
    ##  b:3776     k:2372                   k:2304                  
    ##  c: 556     s:5176                   s:4936                  
    ##  e:1120     y:  24                   y: 284                  
    ##  r: 192                                                      
    ##                                                              
    ##                                                              
    ##  stalk.color.above.ring stalk.color.below.ring veil.type veil.color
    ##  w      :4464           w      :4384           p:8124    n:  96    
    ##  p      :1872           p      :1872                     o:  96    
    ##  g      : 576           g      : 576                     w:7924    
    ##  n      : 448           n      : 512                     y:   8    
    ##  b      : 432           b      : 432                               
    ##  o      : 192           o      : 192                               
    ##  (Other): 140           (Other): 156                               
    ##  ring.number ring.type spore.print.color population habitat 
    ##  n:  36      e:2776    w      :2388      a: 384     d:3148  
    ##  o:7488      f:  48    n      :1968      c: 340     g:2148  
    ##  t: 600      l:1296    k      :1872      n: 400     l: 832  
    ##              n:  36    h      :1632      s:1248     m: 292  
    ##              p:3968    r      :  72      v:4040     p:1144  
    ##                        b      :  48      y:1712     u: 368  
    ##                        (Other): 144                 w: 192

From the above it is clear that all the type class. so we need to factorize the dataset to use it. Here the the target is Class.

so we have to train models in Classification and Clustering.

Factorize the Dataset

``` r
for ( n in names(mushroombase)){
  
  mushroombase[n] = as.factor(unlist(mushroombase[n]))
}
str(mushroombase)
```

    ## 'data.frame':    8124 obs. of  23 variables:
    ##  $ class                   : Factor w/ 2 levels "e","p": 2 1 1 2 1 1 1 1 2 1 ...
    ##  $ cap.shape               : Factor w/ 6 levels "b","c","f","k",..: 6 6 1 6 6 6 1 1 6 1 ...
    ##  $ cap.surface             : Factor w/ 4 levels "f","g","s","y": 3 3 3 4 3 4 3 4 4 3 ...
    ##  $ cap.color               : Factor w/ 10 levels "b","c","e","g",..: 5 10 9 9 4 10 9 9 9 10 ...
    ##  $ bruises                 : Factor w/ 2 levels "f","t": 2 2 2 2 1 2 2 2 2 2 ...
    ##  $ odor                    : Factor w/ 9 levels "a","c","f","l",..: 7 1 4 7 6 1 1 4 7 1 ...
    ##  $ gill.attachment         : Factor w/ 2 levels "a","f": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ gill.spacing            : Factor w/ 2 levels "c","w": 1 1 1 1 2 1 1 1 1 1 ...
    ##  $ gill.size               : Factor w/ 2 levels "b","n": 2 1 1 2 1 1 1 1 2 1 ...
    ##  $ gill.color              : Factor w/ 12 levels "b","e","g","h",..: 5 5 6 6 5 6 3 6 8 3 ...
    ##  $ stalk.shape             : Factor w/ 2 levels "e","t": 1 1 1 1 2 1 1 1 1 1 ...
    ##  $ stalk.root              : Factor w/ 5 levels "?","b","c","e",..: 4 3 3 4 4 3 3 3 4 3 ...
    ##  $ stalk.surface.above.ring: Factor w/ 4 levels "f","k","s","y": 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ stalk.surface.below.ring: Factor w/ 4 levels "f","k","s","y": 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ stalk.color.above.ring  : Factor w/ 9 levels "b","c","e","g",..: 8 8 8 8 8 8 8 8 8 8 ...
    ##  $ stalk.color.below.ring  : Factor w/ 9 levels "b","c","e","g",..: 8 8 8 8 8 8 8 8 8 8 ...
    ##  $ veil.type               : Factor w/ 1 level "p": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ veil.color              : Factor w/ 4 levels "n","o","w","y": 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ ring.number             : Factor w/ 3 levels "n","o","t": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ ring.type               : Factor w/ 5 levels "e","f","l","n",..: 5 5 5 5 1 5 5 5 5 5 ...
    ##  $ spore.print.color       : Factor w/ 9 levels "b","h","k","n",..: 3 4 4 3 4 3 3 4 3 3 ...
    ##  $ population              : Factor w/ 6 levels "a","c","n","s",..: 4 3 3 4 1 3 3 4 5 4 ...
    ##  $ habitat                 : Factor w/ 7 levels "d","g","l","m",..: 6 2 4 6 2 2 4 4 2 4 ...

Spliting the Training and Test data in 2:1 (Train:Test)

``` r
set.seed(134)
jumble = runif(nrow(mushroombase))
mushroombase = mushroombase[ordered(jumble),]
sampleindex = sample(2,nrow(mushroombase), replace = TRUE, prob = c(0.67, 0.33))
mushroomtrain = mushroombase[sampleindex == 1,]
mushroomtest = mushroombase[sampleindex == 2,]
```

``` r
table(mushroomtrain$class)
```

    ## 
    ##    e    p 
    ## 2795 2591

From the above we can see the Edible class is postive class

``` r
print(2795 / (2795 + 2591))
```

    ## [1] 0.518938

baseline is considering the positive class as prediticed for all instance which is 52% percent approximately.

``` r
library(mlr)
```

    ## Warning: package 'mlr' was built under R version 3.4.2

    ## Loading required package: ParamHelpers

    ## Warning: package 'ParamHelpers' was built under R version 3.4.2

``` r
mushroomTrain = makeClassifTask(data = mushroomtrain, target = "class")
mushroomTest = makeClassifTask(data = mushroomtest, target = "class")
```

``` r
library(mlr)
mustree = makeLearner("classif.rpart", predict.type = "response")
treecv =  makeResampleDesc("CV",iters = 10L)
param = makeParamSet(
makeIntegerParam("minsplit",lower = 10, upper = 20),
makeIntegerParam("minbucket", lower = 5, upper = 10),
makeNumericParam("cp", lower = 0.001, upper = 0.1)
)

control = makeTuneControlGrid()

treetune <- tuneParams(learner = mustree, resampling = treecv, task = mushroomTrain, par.set = param, control = control, measures = acc)
```

    ## [Tune] Started tuning learner classif.rpart for parameter set:

    ##              Type len Def       Constr Req Tunable Trafo
    ## minsplit  integer   -   -     10 to 20   -    TRUE     -
    ## minbucket integer   -   -      5 to 10   -    TRUE     -
    ## cp        numeric   -   - 0.001 to 0.1   -    TRUE     -

    ## With control class: TuneControlGrid

    ## Imputation value: -0

    ## [Tune-x] 1: minsplit=10; minbucket=5; cp=0.001

    ## [Tune-y] 1: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 2: minsplit=11; minbucket=5; cp=0.001

    ## [Tune-y] 2: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 3: minsplit=12; minbucket=5; cp=0.001

    ## [Tune-y] 3: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 4: minsplit=13; minbucket=5; cp=0.001

    ## [Tune-y] 4: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 5: minsplit=14; minbucket=5; cp=0.001

    ## [Tune-y] 5: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 6: minsplit=16; minbucket=5; cp=0.001

    ## [Tune-y] 6: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 7: minsplit=17; minbucket=5; cp=0.001

    ## [Tune-y] 7: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 8: minsplit=18; minbucket=5; cp=0.001

    ## [Tune-y] 8: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 9: minsplit=19; minbucket=5; cp=0.001

    ## [Tune-y] 9: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 10: minsplit=20; minbucket=5; cp=0.001

    ## [Tune-y] 10: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 11: minsplit=10; minbucket=6; cp=0.001

    ## [Tune-y] 11: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 12: minsplit=11; minbucket=6; cp=0.001

    ## [Tune-y] 12: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 13: minsplit=12; minbucket=6; cp=0.001

    ## [Tune-y] 13: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 14: minsplit=13; minbucket=6; cp=0.001

    ## [Tune-y] 14: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 15: minsplit=14; minbucket=6; cp=0.001

    ## [Tune-y] 15: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 16: minsplit=16; minbucket=6; cp=0.001

    ## [Tune-y] 16: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 17: minsplit=17; minbucket=6; cp=0.001

    ## [Tune-y] 17: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 18: minsplit=18; minbucket=6; cp=0.001

    ## [Tune-y] 18: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 19: minsplit=19; minbucket=6; cp=0.001

    ## [Tune-y] 19: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 20: minsplit=20; minbucket=6; cp=0.001

    ## [Tune-y] 20: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 21: minsplit=10; minbucket=7; cp=0.001

    ## [Tune-y] 21: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 22: minsplit=11; minbucket=7; cp=0.001

    ## [Tune-y] 22: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 23: minsplit=12; minbucket=7; cp=0.001

    ## [Tune-y] 23: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 24: minsplit=13; minbucket=7; cp=0.001

    ## [Tune-y] 24: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 25: minsplit=14; minbucket=7; cp=0.001

    ## [Tune-y] 25: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 26: minsplit=16; minbucket=7; cp=0.001

    ## [Tune-y] 26: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 27: minsplit=17; minbucket=7; cp=0.001

    ## [Tune-y] 27: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 28: minsplit=18; minbucket=7; cp=0.001

    ## [Tune-y] 28: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 29: minsplit=19; minbucket=7; cp=0.001

    ## [Tune-y] 29: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 30: minsplit=20; minbucket=7; cp=0.001

    ## [Tune-y] 30: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 31: minsplit=10; minbucket=8; cp=0.001

    ## [Tune-y] 31: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 32: minsplit=11; minbucket=8; cp=0.001

    ## [Tune-y] 32: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 33: minsplit=12; minbucket=8; cp=0.001

    ## [Tune-y] 33: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 34: minsplit=13; minbucket=8; cp=0.001

    ## [Tune-y] 34: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 35: minsplit=14; minbucket=8; cp=0.001

    ## [Tune-y] 35: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 36: minsplit=16; minbucket=8; cp=0.001

    ## [Tune-y] 36: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 37: minsplit=17; minbucket=8; cp=0.001

    ## [Tune-y] 37: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 38: minsplit=18; minbucket=8; cp=0.001

    ## [Tune-y] 38: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 39: minsplit=19; minbucket=8; cp=0.001

    ## [Tune-y] 39: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 40: minsplit=20; minbucket=8; cp=0.001

    ## [Tune-y] 40: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 41: minsplit=10; minbucket=9; cp=0.001

    ## [Tune-y] 41: acc.test.mean=0.998; time: 0.0 min

    ## [Tune-x] 42: minsplit=11; minbucket=9; cp=0.001

    ## [Tune-y] 42: acc.test.mean=0.998; time: 0.0 min

    ## [Tune-x] 43: minsplit=12; minbucket=9; cp=0.001

    ## [Tune-y] 43: acc.test.mean=0.998; time: 0.0 min

    ## [Tune-x] 44: minsplit=13; minbucket=9; cp=0.001

    ## [Tune-y] 44: acc.test.mean=0.998; time: 0.0 min

    ## [Tune-x] 45: minsplit=14; minbucket=9; cp=0.001

    ## [Tune-y] 45: acc.test.mean=0.998; time: 0.0 min

    ## [Tune-x] 46: minsplit=16; minbucket=9; cp=0.001

    ## [Tune-y] 46: acc.test.mean=0.998; time: 0.0 min

    ## [Tune-x] 47: minsplit=17; minbucket=9; cp=0.001

    ## [Tune-y] 47: acc.test.mean=0.998; time: 0.0 min

    ## [Tune-x] 48: minsplit=18; minbucket=9; cp=0.001

    ## [Tune-y] 48: acc.test.mean=0.998; time: 0.0 min

    ## [Tune-x] 49: minsplit=19; minbucket=9; cp=0.001

    ## [Tune-y] 49: acc.test.mean=0.998; time: 0.0 min

    ## [Tune-x] 50: minsplit=20; minbucket=9; cp=0.001

    ## [Tune-y] 50: acc.test.mean=0.998; time: 0.0 min

    ## [Tune-x] 51: minsplit=10; minbucket=10; cp=0.001

    ## [Tune-y] 51: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 52: minsplit=11; minbucket=10; cp=0.001

    ## [Tune-y] 52: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 53: minsplit=12; minbucket=10; cp=0.001

    ## [Tune-y] 53: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 54: minsplit=13; minbucket=10; cp=0.001

    ## [Tune-y] 54: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 55: minsplit=14; minbucket=10; cp=0.001

    ## [Tune-y] 55: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 56: minsplit=16; minbucket=10; cp=0.001

    ## [Tune-y] 56: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 57: minsplit=17; minbucket=10; cp=0.001

    ## [Tune-y] 57: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 58: minsplit=18; minbucket=10; cp=0.001

    ## [Tune-y] 58: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 59: minsplit=19; minbucket=10; cp=0.001

    ## [Tune-y] 59: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 60: minsplit=20; minbucket=10; cp=0.001

    ## [Tune-y] 60: acc.test.mean=0.999; time: 0.0 min

    ## [Tune-x] 61: minsplit=10; minbucket=5; cp=0.012

    ## [Tune-y] 61: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 62: minsplit=11; minbucket=5; cp=0.012

    ## [Tune-y] 62: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 63: minsplit=12; minbucket=5; cp=0.012

    ## [Tune-y] 63: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 64: minsplit=13; minbucket=5; cp=0.012

    ## [Tune-y] 64: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 65: minsplit=14; minbucket=5; cp=0.012

    ## [Tune-y] 65: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 66: minsplit=16; minbucket=5; cp=0.012

    ## [Tune-y] 66: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 67: minsplit=17; minbucket=5; cp=0.012

    ## [Tune-y] 67: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 68: minsplit=18; minbucket=5; cp=0.012

    ## [Tune-y] 68: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 69: minsplit=19; minbucket=5; cp=0.012

    ## [Tune-y] 69: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 70: minsplit=20; minbucket=5; cp=0.012

    ## [Tune-y] 70: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 71: minsplit=10; minbucket=6; cp=0.012

    ## [Tune-y] 71: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 72: minsplit=11; minbucket=6; cp=0.012

    ## [Tune-y] 72: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 73: minsplit=12; minbucket=6; cp=0.012

    ## [Tune-y] 73: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 74: minsplit=13; minbucket=6; cp=0.012

    ## [Tune-y] 74: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 75: minsplit=14; minbucket=6; cp=0.012

    ## [Tune-y] 75: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 76: minsplit=16; minbucket=6; cp=0.012

    ## [Tune-y] 76: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 77: minsplit=17; minbucket=6; cp=0.012

    ## [Tune-y] 77: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 78: minsplit=18; minbucket=6; cp=0.012

    ## [Tune-y] 78: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 79: minsplit=19; minbucket=6; cp=0.012

    ## [Tune-y] 79: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 80: minsplit=20; minbucket=6; cp=0.012

    ## [Tune-y] 80: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 81: minsplit=10; minbucket=7; cp=0.012

    ## [Tune-y] 81: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 82: minsplit=11; minbucket=7; cp=0.012

    ## [Tune-y] 82: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 83: minsplit=12; minbucket=7; cp=0.012

    ## [Tune-y] 83: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 84: minsplit=13; minbucket=7; cp=0.012

    ## [Tune-y] 84: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 85: minsplit=14; minbucket=7; cp=0.012

    ## [Tune-y] 85: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 86: minsplit=16; minbucket=7; cp=0.012

    ## [Tune-y] 86: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 87: minsplit=17; minbucket=7; cp=0.012

    ## [Tune-y] 87: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 88: minsplit=18; minbucket=7; cp=0.012

    ## [Tune-y] 88: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 89: minsplit=19; minbucket=7; cp=0.012

    ## [Tune-y] 89: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 90: minsplit=20; minbucket=7; cp=0.012

    ## [Tune-y] 90: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 91: minsplit=10; minbucket=8; cp=0.012

    ## [Tune-y] 91: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 92: minsplit=11; minbucket=8; cp=0.012

    ## [Tune-y] 92: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 93: minsplit=12; minbucket=8; cp=0.012

    ## [Tune-y] 93: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 94: minsplit=13; minbucket=8; cp=0.012

    ## [Tune-y] 94: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 95: minsplit=14; minbucket=8; cp=0.012

    ## [Tune-y] 95: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 96: minsplit=16; minbucket=8; cp=0.012

    ## [Tune-y] 96: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 97: minsplit=17; minbucket=8; cp=0.012

    ## [Tune-y] 97: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 98: minsplit=18; minbucket=8; cp=0.012

    ## [Tune-y] 98: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 99: minsplit=19; minbucket=8; cp=0.012

    ## [Tune-y] 99: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 100: minsplit=20; minbucket=8; cp=0.012

    ## [Tune-y] 100: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 101: minsplit=10; minbucket=9; cp=0.012

    ## [Tune-y] 101: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 102: minsplit=11; minbucket=9; cp=0.012

    ## [Tune-y] 102: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 103: minsplit=12; minbucket=9; cp=0.012

    ## [Tune-y] 103: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 104: minsplit=13; minbucket=9; cp=0.012

    ## [Tune-y] 104: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 105: minsplit=14; minbucket=9; cp=0.012

    ## [Tune-y] 105: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 106: minsplit=16; minbucket=9; cp=0.012

    ## [Tune-y] 106: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 107: minsplit=17; minbucket=9; cp=0.012

    ## [Tune-y] 107: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 108: minsplit=18; minbucket=9; cp=0.012

    ## [Tune-y] 108: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 109: minsplit=19; minbucket=9; cp=0.012

    ## [Tune-y] 109: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 110: minsplit=20; minbucket=9; cp=0.012

    ## [Tune-y] 110: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 111: minsplit=10; minbucket=10; cp=0.012

    ## [Tune-y] 111: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 112: minsplit=11; minbucket=10; cp=0.012

    ## [Tune-y] 112: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 113: minsplit=12; minbucket=10; cp=0.012

    ## [Tune-y] 113: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 114: minsplit=13; minbucket=10; cp=0.012

    ## [Tune-y] 114: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 115: minsplit=14; minbucket=10; cp=0.012

    ## [Tune-y] 115: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 116: minsplit=16; minbucket=10; cp=0.012

    ## [Tune-y] 116: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 117: minsplit=17; minbucket=10; cp=0.012

    ## [Tune-y] 117: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 118: minsplit=18; minbucket=10; cp=0.012

    ## [Tune-y] 118: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 119: minsplit=19; minbucket=10; cp=0.012

    ## [Tune-y] 119: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 120: minsplit=20; minbucket=10; cp=0.012

    ## [Tune-y] 120: acc.test.mean=0.995; time: 0.0 min

    ## [Tune-x] 121: minsplit=10; minbucket=5; cp=0.023

    ## [Tune-y] 121: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 122: minsplit=11; minbucket=5; cp=0.023

    ## [Tune-y] 122: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 123: minsplit=12; minbucket=5; cp=0.023

    ## [Tune-y] 123: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 124: minsplit=13; minbucket=5; cp=0.023

    ## [Tune-y] 124: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 125: minsplit=14; minbucket=5; cp=0.023

    ## [Tune-y] 125: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 126: minsplit=16; minbucket=5; cp=0.023

    ## [Tune-y] 126: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 127: minsplit=17; minbucket=5; cp=0.023

    ## [Tune-y] 127: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 128: minsplit=18; minbucket=5; cp=0.023

    ## [Tune-y] 128: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 129: minsplit=19; minbucket=5; cp=0.023

    ## [Tune-y] 129: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 130: minsplit=20; minbucket=5; cp=0.023

    ## [Tune-y] 130: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 131: minsplit=10; minbucket=6; cp=0.023

    ## [Tune-y] 131: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 132: minsplit=11; minbucket=6; cp=0.023

    ## [Tune-y] 132: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 133: minsplit=12; minbucket=6; cp=0.023

    ## [Tune-y] 133: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 134: minsplit=13; minbucket=6; cp=0.023

    ## [Tune-y] 134: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 135: minsplit=14; minbucket=6; cp=0.023

    ## [Tune-y] 135: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 136: minsplit=16; minbucket=6; cp=0.023

    ## [Tune-y] 136: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 137: minsplit=17; minbucket=6; cp=0.023

    ## [Tune-y] 137: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 138: minsplit=18; minbucket=6; cp=0.023

    ## [Tune-y] 138: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 139: minsplit=19; minbucket=6; cp=0.023

    ## [Tune-y] 139: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 140: minsplit=20; minbucket=6; cp=0.023

    ## [Tune-y] 140: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 141: minsplit=10; minbucket=7; cp=0.023

    ## [Tune-y] 141: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 142: minsplit=11; minbucket=7; cp=0.023

    ## [Tune-y] 142: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 143: minsplit=12; minbucket=7; cp=0.023

    ## [Tune-y] 143: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 144: minsplit=13; minbucket=7; cp=0.023

    ## [Tune-y] 144: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 145: minsplit=14; minbucket=7; cp=0.023

    ## [Tune-y] 145: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 146: minsplit=16; minbucket=7; cp=0.023

    ## [Tune-y] 146: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 147: minsplit=17; minbucket=7; cp=0.023

    ## [Tune-y] 147: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 148: minsplit=18; minbucket=7; cp=0.023

    ## [Tune-y] 148: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 149: minsplit=19; minbucket=7; cp=0.023

    ## [Tune-y] 149: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 150: minsplit=20; minbucket=7; cp=0.023

    ## [Tune-y] 150: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 151: minsplit=10; minbucket=8; cp=0.023

    ## [Tune-y] 151: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 152: minsplit=11; minbucket=8; cp=0.023

    ## [Tune-y] 152: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 153: minsplit=12; minbucket=8; cp=0.023

    ## [Tune-y] 153: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 154: minsplit=13; minbucket=8; cp=0.023

    ## [Tune-y] 154: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 155: minsplit=14; minbucket=8; cp=0.023

    ## [Tune-y] 155: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 156: minsplit=16; minbucket=8; cp=0.023

    ## [Tune-y] 156: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 157: minsplit=17; minbucket=8; cp=0.023

    ## [Tune-y] 157: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 158: minsplit=18; minbucket=8; cp=0.023

    ## [Tune-y] 158: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 159: minsplit=19; minbucket=8; cp=0.023

    ## [Tune-y] 159: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 160: minsplit=20; minbucket=8; cp=0.023

    ## [Tune-y] 160: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 161: minsplit=10; minbucket=9; cp=0.023

    ## [Tune-y] 161: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 162: minsplit=11; minbucket=9; cp=0.023

    ## [Tune-y] 162: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 163: minsplit=12; minbucket=9; cp=0.023

    ## [Tune-y] 163: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 164: minsplit=13; minbucket=9; cp=0.023

    ## [Tune-y] 164: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 165: minsplit=14; minbucket=9; cp=0.023

    ## [Tune-y] 165: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 166: minsplit=16; minbucket=9; cp=0.023

    ## [Tune-y] 166: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 167: minsplit=17; minbucket=9; cp=0.023

    ## [Tune-y] 167: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 168: minsplit=18; minbucket=9; cp=0.023

    ## [Tune-y] 168: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 169: minsplit=19; minbucket=9; cp=0.023

    ## [Tune-y] 169: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 170: minsplit=20; minbucket=9; cp=0.023

    ## [Tune-y] 170: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 171: minsplit=10; minbucket=10; cp=0.023

    ## [Tune-y] 171: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 172: minsplit=11; minbucket=10; cp=0.023

    ## [Tune-y] 172: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 173: minsplit=12; minbucket=10; cp=0.023

    ## [Tune-y] 173: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 174: minsplit=13; minbucket=10; cp=0.023

    ## [Tune-y] 174: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 175: minsplit=14; minbucket=10; cp=0.023

    ## [Tune-y] 175: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 176: minsplit=16; minbucket=10; cp=0.023

    ## [Tune-y] 176: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 177: minsplit=17; minbucket=10; cp=0.023

    ## [Tune-y] 177: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 178: minsplit=18; minbucket=10; cp=0.023

    ## [Tune-y] 178: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 179: minsplit=19; minbucket=10; cp=0.023

    ## [Tune-y] 179: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 180: minsplit=20; minbucket=10; cp=0.023

    ## [Tune-y] 180: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 181: minsplit=10; minbucket=5; cp=0.034

    ## [Tune-y] 181: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 182: minsplit=11; minbucket=5; cp=0.034

    ## [Tune-y] 182: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 183: minsplit=12; minbucket=5; cp=0.034

    ## [Tune-y] 183: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 184: minsplit=13; minbucket=5; cp=0.034

    ## [Tune-y] 184: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 185: minsplit=14; minbucket=5; cp=0.034

    ## [Tune-y] 185: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 186: minsplit=16; minbucket=5; cp=0.034

    ## [Tune-y] 186: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 187: minsplit=17; minbucket=5; cp=0.034

    ## [Tune-y] 187: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 188: minsplit=18; minbucket=5; cp=0.034

    ## [Tune-y] 188: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 189: minsplit=19; minbucket=5; cp=0.034

    ## [Tune-y] 189: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 190: minsplit=20; minbucket=5; cp=0.034

    ## [Tune-y] 190: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 191: minsplit=10; minbucket=6; cp=0.034

    ## [Tune-y] 191: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 192: minsplit=11; minbucket=6; cp=0.034

    ## [Tune-y] 192: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 193: minsplit=12; minbucket=6; cp=0.034

    ## [Tune-y] 193: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 194: minsplit=13; minbucket=6; cp=0.034

    ## [Tune-y] 194: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 195: minsplit=14; minbucket=6; cp=0.034

    ## [Tune-y] 195: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 196: minsplit=16; minbucket=6; cp=0.034

    ## [Tune-y] 196: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 197: minsplit=17; minbucket=6; cp=0.034

    ## [Tune-y] 197: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 198: minsplit=18; minbucket=6; cp=0.034

    ## [Tune-y] 198: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 199: minsplit=19; minbucket=6; cp=0.034

    ## [Tune-y] 199: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 200: minsplit=20; minbucket=6; cp=0.034

    ## [Tune-y] 200: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 201: minsplit=10; minbucket=7; cp=0.034

    ## [Tune-y] 201: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 202: minsplit=11; minbucket=7; cp=0.034

    ## [Tune-y] 202: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 203: minsplit=12; minbucket=7; cp=0.034

    ## [Tune-y] 203: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 204: minsplit=13; minbucket=7; cp=0.034

    ## [Tune-y] 204: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 205: minsplit=14; minbucket=7; cp=0.034

    ## [Tune-y] 205: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 206: minsplit=16; minbucket=7; cp=0.034

    ## [Tune-y] 206: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 207: minsplit=17; minbucket=7; cp=0.034

    ## [Tune-y] 207: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 208: minsplit=18; minbucket=7; cp=0.034

    ## [Tune-y] 208: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 209: minsplit=19; minbucket=7; cp=0.034

    ## [Tune-y] 209: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 210: minsplit=20; minbucket=7; cp=0.034

    ## [Tune-y] 210: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 211: minsplit=10; minbucket=8; cp=0.034

    ## [Tune-y] 211: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 212: minsplit=11; minbucket=8; cp=0.034

    ## [Tune-y] 212: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 213: minsplit=12; minbucket=8; cp=0.034

    ## [Tune-y] 213: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 214: minsplit=13; minbucket=8; cp=0.034

    ## [Tune-y] 214: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 215: minsplit=14; minbucket=8; cp=0.034

    ## [Tune-y] 215: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 216: minsplit=16; minbucket=8; cp=0.034

    ## [Tune-y] 216: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 217: minsplit=17; minbucket=8; cp=0.034

    ## [Tune-y] 217: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 218: minsplit=18; minbucket=8; cp=0.034

    ## [Tune-y] 218: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 219: minsplit=19; minbucket=8; cp=0.034

    ## [Tune-y] 219: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 220: minsplit=20; minbucket=8; cp=0.034

    ## [Tune-y] 220: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 221: minsplit=10; minbucket=9; cp=0.034

    ## [Tune-y] 221: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 222: minsplit=11; minbucket=9; cp=0.034

    ## [Tune-y] 222: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 223: minsplit=12; minbucket=9; cp=0.034

    ## [Tune-y] 223: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 224: minsplit=13; minbucket=9; cp=0.034

    ## [Tune-y] 224: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 225: minsplit=14; minbucket=9; cp=0.034

    ## [Tune-y] 225: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 226: minsplit=16; minbucket=9; cp=0.034

    ## [Tune-y] 226: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 227: minsplit=17; minbucket=9; cp=0.034

    ## [Tune-y] 227: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 228: minsplit=18; minbucket=9; cp=0.034

    ## [Tune-y] 228: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 229: minsplit=19; minbucket=9; cp=0.034

    ## [Tune-y] 229: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 230: minsplit=20; minbucket=9; cp=0.034

    ## [Tune-y] 230: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 231: minsplit=10; minbucket=10; cp=0.034

    ## [Tune-y] 231: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 232: minsplit=11; minbucket=10; cp=0.034

    ## [Tune-y] 232: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 233: minsplit=12; minbucket=10; cp=0.034

    ## [Tune-y] 233: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 234: minsplit=13; minbucket=10; cp=0.034

    ## [Tune-y] 234: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 235: minsplit=14; minbucket=10; cp=0.034

    ## [Tune-y] 235: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 236: minsplit=16; minbucket=10; cp=0.034

    ## [Tune-y] 236: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 237: minsplit=17; minbucket=10; cp=0.034

    ## [Tune-y] 237: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 238: minsplit=18; minbucket=10; cp=0.034

    ## [Tune-y] 238: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 239: minsplit=19; minbucket=10; cp=0.034

    ## [Tune-y] 239: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 240: minsplit=20; minbucket=10; cp=0.034

    ## [Tune-y] 240: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 241: minsplit=10; minbucket=5; cp=0.045

    ## [Tune-y] 241: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 242: minsplit=11; minbucket=5; cp=0.045

    ## [Tune-y] 242: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 243: minsplit=12; minbucket=5; cp=0.045

    ## [Tune-y] 243: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 244: minsplit=13; minbucket=5; cp=0.045

    ## [Tune-y] 244: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 245: minsplit=14; minbucket=5; cp=0.045

    ## [Tune-y] 245: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 246: minsplit=16; minbucket=5; cp=0.045

    ## [Tune-y] 246: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 247: minsplit=17; minbucket=5; cp=0.045

    ## [Tune-y] 247: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 248: minsplit=18; minbucket=5; cp=0.045

    ## [Tune-y] 248: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 249: minsplit=19; minbucket=5; cp=0.045

    ## [Tune-y] 249: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 250: minsplit=20; minbucket=5; cp=0.045

    ## [Tune-y] 250: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 251: minsplit=10; minbucket=6; cp=0.045

    ## [Tune-y] 251: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 252: minsplit=11; minbucket=6; cp=0.045

    ## [Tune-y] 252: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 253: minsplit=12; minbucket=6; cp=0.045

    ## [Tune-y] 253: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 254: minsplit=13; minbucket=6; cp=0.045

    ## [Tune-y] 254: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 255: minsplit=14; minbucket=6; cp=0.045

    ## [Tune-y] 255: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 256: minsplit=16; minbucket=6; cp=0.045

    ## [Tune-y] 256: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 257: minsplit=17; minbucket=6; cp=0.045

    ## [Tune-y] 257: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 258: minsplit=18; minbucket=6; cp=0.045

    ## [Tune-y] 258: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 259: minsplit=19; minbucket=6; cp=0.045

    ## [Tune-y] 259: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 260: minsplit=20; minbucket=6; cp=0.045

    ## [Tune-y] 260: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 261: minsplit=10; minbucket=7; cp=0.045

    ## [Tune-y] 261: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 262: minsplit=11; minbucket=7; cp=0.045

    ## [Tune-y] 262: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 263: minsplit=12; minbucket=7; cp=0.045

    ## [Tune-y] 263: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 264: minsplit=13; minbucket=7; cp=0.045

    ## [Tune-y] 264: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 265: minsplit=14; minbucket=7; cp=0.045

    ## [Tune-y] 265: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 266: minsplit=16; minbucket=7; cp=0.045

    ## [Tune-y] 266: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 267: minsplit=17; minbucket=7; cp=0.045

    ## [Tune-y] 267: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 268: minsplit=18; minbucket=7; cp=0.045

    ## [Tune-y] 268: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 269: minsplit=19; minbucket=7; cp=0.045

    ## [Tune-y] 269: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 270: minsplit=20; minbucket=7; cp=0.045

    ## [Tune-y] 270: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 271: minsplit=10; minbucket=8; cp=0.045

    ## [Tune-y] 271: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 272: minsplit=11; minbucket=8; cp=0.045

    ## [Tune-y] 272: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 273: minsplit=12; minbucket=8; cp=0.045

    ## [Tune-y] 273: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 274: minsplit=13; minbucket=8; cp=0.045

    ## [Tune-y] 274: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 275: minsplit=14; minbucket=8; cp=0.045

    ## [Tune-y] 275: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 276: minsplit=16; minbucket=8; cp=0.045

    ## [Tune-y] 276: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 277: minsplit=17; minbucket=8; cp=0.045

    ## [Tune-y] 277: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 278: minsplit=18; minbucket=8; cp=0.045

    ## [Tune-y] 278: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 279: minsplit=19; minbucket=8; cp=0.045

    ## [Tune-y] 279: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 280: minsplit=20; minbucket=8; cp=0.045

    ## [Tune-y] 280: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 281: minsplit=10; minbucket=9; cp=0.045

    ## [Tune-y] 281: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 282: minsplit=11; minbucket=9; cp=0.045

    ## [Tune-y] 282: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 283: minsplit=12; minbucket=9; cp=0.045

    ## [Tune-y] 283: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 284: minsplit=13; minbucket=9; cp=0.045

    ## [Tune-y] 284: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 285: minsplit=14; minbucket=9; cp=0.045

    ## [Tune-y] 285: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 286: minsplit=16; minbucket=9; cp=0.045

    ## [Tune-y] 286: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 287: minsplit=17; minbucket=9; cp=0.045

    ## [Tune-y] 287: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 288: minsplit=18; minbucket=9; cp=0.045

    ## [Tune-y] 288: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 289: minsplit=19; minbucket=9; cp=0.045

    ## [Tune-y] 289: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 290: minsplit=20; minbucket=9; cp=0.045

    ## [Tune-y] 290: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 291: minsplit=10; minbucket=10; cp=0.045

    ## [Tune-y] 291: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 292: minsplit=11; minbucket=10; cp=0.045

    ## [Tune-y] 292: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 293: minsplit=12; minbucket=10; cp=0.045

    ## [Tune-y] 293: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 294: minsplit=13; minbucket=10; cp=0.045

    ## [Tune-y] 294: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 295: minsplit=14; minbucket=10; cp=0.045

    ## [Tune-y] 295: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 296: minsplit=16; minbucket=10; cp=0.045

    ## [Tune-y] 296: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 297: minsplit=17; minbucket=10; cp=0.045

    ## [Tune-y] 297: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 298: minsplit=18; minbucket=10; cp=0.045

    ## [Tune-y] 298: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 299: minsplit=19; minbucket=10; cp=0.045

    ## [Tune-y] 299: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 300: minsplit=20; minbucket=10; cp=0.045

    ## [Tune-y] 300: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 301: minsplit=10; minbucket=5; cp=0.056

    ## [Tune-y] 301: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 302: minsplit=11; minbucket=5; cp=0.056

    ## [Tune-y] 302: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 303: minsplit=12; minbucket=5; cp=0.056

    ## [Tune-y] 303: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 304: minsplit=13; minbucket=5; cp=0.056

    ## [Tune-y] 304: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 305: minsplit=14; minbucket=5; cp=0.056

    ## [Tune-y] 305: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 306: minsplit=16; minbucket=5; cp=0.056

    ## [Tune-y] 306: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 307: minsplit=17; minbucket=5; cp=0.056

    ## [Tune-y] 307: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 308: minsplit=18; minbucket=5; cp=0.056

    ## [Tune-y] 308: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 309: minsplit=19; minbucket=5; cp=0.056

    ## [Tune-y] 309: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 310: minsplit=20; minbucket=5; cp=0.056

    ## [Tune-y] 310: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 311: minsplit=10; minbucket=6; cp=0.056

    ## [Tune-y] 311: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 312: minsplit=11; minbucket=6; cp=0.056

    ## [Tune-y] 312: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 313: minsplit=12; minbucket=6; cp=0.056

    ## [Tune-y] 313: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 314: minsplit=13; minbucket=6; cp=0.056

    ## [Tune-y] 314: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 315: minsplit=14; minbucket=6; cp=0.056

    ## [Tune-y] 315: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 316: minsplit=16; minbucket=6; cp=0.056

    ## [Tune-y] 316: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 317: minsplit=17; minbucket=6; cp=0.056

    ## [Tune-y] 317: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 318: minsplit=18; minbucket=6; cp=0.056

    ## [Tune-y] 318: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 319: minsplit=19; minbucket=6; cp=0.056

    ## [Tune-y] 319: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 320: minsplit=20; minbucket=6; cp=0.056

    ## [Tune-y] 320: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 321: minsplit=10; minbucket=7; cp=0.056

    ## [Tune-y] 321: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 322: minsplit=11; minbucket=7; cp=0.056

    ## [Tune-y] 322: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 323: minsplit=12; minbucket=7; cp=0.056

    ## [Tune-y] 323: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 324: minsplit=13; minbucket=7; cp=0.056

    ## [Tune-y] 324: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 325: minsplit=14; minbucket=7; cp=0.056

    ## [Tune-y] 325: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 326: minsplit=16; minbucket=7; cp=0.056

    ## [Tune-y] 326: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 327: minsplit=17; minbucket=7; cp=0.056

    ## [Tune-y] 327: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 328: minsplit=18; minbucket=7; cp=0.056

    ## [Tune-y] 328: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 329: minsplit=19; minbucket=7; cp=0.056

    ## [Tune-y] 329: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 330: minsplit=20; minbucket=7; cp=0.056

    ## [Tune-y] 330: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 331: minsplit=10; minbucket=8; cp=0.056

    ## [Tune-y] 331: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 332: minsplit=11; minbucket=8; cp=0.056

    ## [Tune-y] 332: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 333: minsplit=12; minbucket=8; cp=0.056

    ## [Tune-y] 333: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 334: minsplit=13; minbucket=8; cp=0.056

    ## [Tune-y] 334: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 335: minsplit=14; minbucket=8; cp=0.056

    ## [Tune-y] 335: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 336: minsplit=16; minbucket=8; cp=0.056

    ## [Tune-y] 336: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 337: minsplit=17; minbucket=8; cp=0.056

    ## [Tune-y] 337: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 338: minsplit=18; minbucket=8; cp=0.056

    ## [Tune-y] 338: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 339: minsplit=19; minbucket=8; cp=0.056

    ## [Tune-y] 339: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 340: minsplit=20; minbucket=8; cp=0.056

    ## [Tune-y] 340: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 341: minsplit=10; minbucket=9; cp=0.056

    ## [Tune-y] 341: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 342: minsplit=11; minbucket=9; cp=0.056

    ## [Tune-y] 342: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 343: minsplit=12; minbucket=9; cp=0.056

    ## [Tune-y] 343: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 344: minsplit=13; minbucket=9; cp=0.056

    ## [Tune-y] 344: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 345: minsplit=14; minbucket=9; cp=0.056

    ## [Tune-y] 345: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 346: minsplit=16; minbucket=9; cp=0.056

    ## [Tune-y] 346: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 347: minsplit=17; minbucket=9; cp=0.056

    ## [Tune-y] 347: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 348: minsplit=18; minbucket=9; cp=0.056

    ## [Tune-y] 348: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 349: minsplit=19; minbucket=9; cp=0.056

    ## [Tune-y] 349: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 350: minsplit=20; minbucket=9; cp=0.056

    ## [Tune-y] 350: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 351: minsplit=10; minbucket=10; cp=0.056

    ## [Tune-y] 351: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 352: minsplit=11; minbucket=10; cp=0.056

    ## [Tune-y] 352: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 353: minsplit=12; minbucket=10; cp=0.056

    ## [Tune-y] 353: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 354: minsplit=13; minbucket=10; cp=0.056

    ## [Tune-y] 354: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 355: minsplit=14; minbucket=10; cp=0.056

    ## [Tune-y] 355: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 356: minsplit=16; minbucket=10; cp=0.056

    ## [Tune-y] 356: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 357: minsplit=17; minbucket=10; cp=0.056

    ## [Tune-y] 357: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 358: minsplit=18; minbucket=10; cp=0.056

    ## [Tune-y] 358: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 359: minsplit=19; minbucket=10; cp=0.056

    ## [Tune-y] 359: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 360: minsplit=20; minbucket=10; cp=0.056

    ## [Tune-y] 360: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 361: minsplit=10; minbucket=5; cp=0.067

    ## [Tune-y] 361: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 362: minsplit=11; minbucket=5; cp=0.067

    ## [Tune-y] 362: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 363: minsplit=12; minbucket=5; cp=0.067

    ## [Tune-y] 363: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 364: minsplit=13; minbucket=5; cp=0.067

    ## [Tune-y] 364: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 365: minsplit=14; minbucket=5; cp=0.067

    ## [Tune-y] 365: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 366: minsplit=16; minbucket=5; cp=0.067

    ## [Tune-y] 366: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 367: minsplit=17; minbucket=5; cp=0.067

    ## [Tune-y] 367: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 368: minsplit=18; minbucket=5; cp=0.067

    ## [Tune-y] 368: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 369: minsplit=19; minbucket=5; cp=0.067

    ## [Tune-y] 369: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 370: minsplit=20; minbucket=5; cp=0.067

    ## [Tune-y] 370: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 371: minsplit=10; minbucket=6; cp=0.067

    ## [Tune-y] 371: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 372: minsplit=11; minbucket=6; cp=0.067

    ## [Tune-y] 372: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 373: minsplit=12; minbucket=6; cp=0.067

    ## [Tune-y] 373: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 374: minsplit=13; minbucket=6; cp=0.067

    ## [Tune-y] 374: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 375: minsplit=14; minbucket=6; cp=0.067

    ## [Tune-y] 375: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 376: minsplit=16; minbucket=6; cp=0.067

    ## [Tune-y] 376: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 377: minsplit=17; minbucket=6; cp=0.067

    ## [Tune-y] 377: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 378: minsplit=18; minbucket=6; cp=0.067

    ## [Tune-y] 378: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 379: minsplit=19; minbucket=6; cp=0.067

    ## [Tune-y] 379: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 380: minsplit=20; minbucket=6; cp=0.067

    ## [Tune-y] 380: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 381: minsplit=10; minbucket=7; cp=0.067

    ## [Tune-y] 381: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 382: minsplit=11; minbucket=7; cp=0.067

    ## [Tune-y] 382: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 383: minsplit=12; minbucket=7; cp=0.067

    ## [Tune-y] 383: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 384: minsplit=13; minbucket=7; cp=0.067

    ## [Tune-y] 384: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 385: minsplit=14; minbucket=7; cp=0.067

    ## [Tune-y] 385: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 386: minsplit=16; minbucket=7; cp=0.067

    ## [Tune-y] 386: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 387: minsplit=17; minbucket=7; cp=0.067

    ## [Tune-y] 387: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 388: minsplit=18; minbucket=7; cp=0.067

    ## [Tune-y] 388: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 389: minsplit=19; minbucket=7; cp=0.067

    ## [Tune-y] 389: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 390: minsplit=20; minbucket=7; cp=0.067

    ## [Tune-y] 390: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 391: minsplit=10; minbucket=8; cp=0.067

    ## [Tune-y] 391: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 392: minsplit=11; minbucket=8; cp=0.067

    ## [Tune-y] 392: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 393: minsplit=12; minbucket=8; cp=0.067

    ## [Tune-y] 393: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 394: minsplit=13; minbucket=8; cp=0.067

    ## [Tune-y] 394: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 395: minsplit=14; minbucket=8; cp=0.067

    ## [Tune-y] 395: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 396: minsplit=16; minbucket=8; cp=0.067

    ## [Tune-y] 396: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 397: minsplit=17; minbucket=8; cp=0.067

    ## [Tune-y] 397: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 398: minsplit=18; minbucket=8; cp=0.067

    ## [Tune-y] 398: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 399: minsplit=19; minbucket=8; cp=0.067

    ## [Tune-y] 399: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 400: minsplit=20; minbucket=8; cp=0.067

    ## [Tune-y] 400: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 401: minsplit=10; minbucket=9; cp=0.067

    ## [Tune-y] 401: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 402: minsplit=11; minbucket=9; cp=0.067

    ## [Tune-y] 402: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 403: minsplit=12; minbucket=9; cp=0.067

    ## [Tune-y] 403: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 404: minsplit=13; minbucket=9; cp=0.067

    ## [Tune-y] 404: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 405: minsplit=14; minbucket=9; cp=0.067

    ## [Tune-y] 405: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 406: minsplit=16; minbucket=9; cp=0.067

    ## [Tune-y] 406: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 407: minsplit=17; minbucket=9; cp=0.067

    ## [Tune-y] 407: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 408: minsplit=18; minbucket=9; cp=0.067

    ## [Tune-y] 408: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 409: minsplit=19; minbucket=9; cp=0.067

    ## [Tune-y] 409: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 410: minsplit=20; minbucket=9; cp=0.067

    ## [Tune-y] 410: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 411: minsplit=10; minbucket=10; cp=0.067

    ## [Tune-y] 411: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 412: minsplit=11; minbucket=10; cp=0.067

    ## [Tune-y] 412: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 413: minsplit=12; minbucket=10; cp=0.067

    ## [Tune-y] 413: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 414: minsplit=13; minbucket=10; cp=0.067

    ## [Tune-y] 414: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 415: minsplit=14; minbucket=10; cp=0.067

    ## [Tune-y] 415: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 416: minsplit=16; minbucket=10; cp=0.067

    ## [Tune-y] 416: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 417: minsplit=17; minbucket=10; cp=0.067

    ## [Tune-y] 417: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 418: minsplit=18; minbucket=10; cp=0.067

    ## [Tune-y] 418: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 419: minsplit=19; minbucket=10; cp=0.067

    ## [Tune-y] 419: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 420: minsplit=20; minbucket=10; cp=0.067

    ## [Tune-y] 420: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 421: minsplit=10; minbucket=5; cp=0.078

    ## [Tune-y] 421: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 422: minsplit=11; minbucket=5; cp=0.078

    ## [Tune-y] 422: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 423: minsplit=12; minbucket=5; cp=0.078

    ## [Tune-y] 423: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 424: minsplit=13; minbucket=5; cp=0.078

    ## [Tune-y] 424: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 425: minsplit=14; minbucket=5; cp=0.078

    ## [Tune-y] 425: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 426: minsplit=16; minbucket=5; cp=0.078

    ## [Tune-y] 426: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 427: minsplit=17; minbucket=5; cp=0.078

    ## [Tune-y] 427: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 428: minsplit=18; minbucket=5; cp=0.078

    ## [Tune-y] 428: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 429: minsplit=19; minbucket=5; cp=0.078

    ## [Tune-y] 429: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 430: minsplit=20; minbucket=5; cp=0.078

    ## [Tune-y] 430: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 431: minsplit=10; minbucket=6; cp=0.078

    ## [Tune-y] 431: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 432: minsplit=11; minbucket=6; cp=0.078

    ## [Tune-y] 432: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 433: minsplit=12; minbucket=6; cp=0.078

    ## [Tune-y] 433: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 434: minsplit=13; minbucket=6; cp=0.078

    ## [Tune-y] 434: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 435: minsplit=14; minbucket=6; cp=0.078

    ## [Tune-y] 435: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 436: minsplit=16; minbucket=6; cp=0.078

    ## [Tune-y] 436: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 437: minsplit=17; minbucket=6; cp=0.078

    ## [Tune-y] 437: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 438: minsplit=18; minbucket=6; cp=0.078

    ## [Tune-y] 438: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 439: minsplit=19; minbucket=6; cp=0.078

    ## [Tune-y] 439: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 440: minsplit=20; minbucket=6; cp=0.078

    ## [Tune-y] 440: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 441: minsplit=10; minbucket=7; cp=0.078

    ## [Tune-y] 441: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 442: minsplit=11; minbucket=7; cp=0.078

    ## [Tune-y] 442: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 443: minsplit=12; minbucket=7; cp=0.078

    ## [Tune-y] 443: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 444: minsplit=13; minbucket=7; cp=0.078

    ## [Tune-y] 444: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 445: minsplit=14; minbucket=7; cp=0.078

    ## [Tune-y] 445: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 446: minsplit=16; minbucket=7; cp=0.078

    ## [Tune-y] 446: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 447: minsplit=17; minbucket=7; cp=0.078

    ## [Tune-y] 447: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 448: minsplit=18; minbucket=7; cp=0.078

    ## [Tune-y] 448: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 449: minsplit=19; minbucket=7; cp=0.078

    ## [Tune-y] 449: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 450: minsplit=20; minbucket=7; cp=0.078

    ## [Tune-y] 450: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 451: minsplit=10; minbucket=8; cp=0.078

    ## [Tune-y] 451: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 452: minsplit=11; minbucket=8; cp=0.078

    ## [Tune-y] 452: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 453: minsplit=12; minbucket=8; cp=0.078

    ## [Tune-y] 453: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 454: minsplit=13; minbucket=8; cp=0.078

    ## [Tune-y] 454: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 455: minsplit=14; minbucket=8; cp=0.078

    ## [Tune-y] 455: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 456: minsplit=16; minbucket=8; cp=0.078

    ## [Tune-y] 456: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 457: minsplit=17; minbucket=8; cp=0.078

    ## [Tune-y] 457: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 458: minsplit=18; minbucket=8; cp=0.078

    ## [Tune-y] 458: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 459: minsplit=19; minbucket=8; cp=0.078

    ## [Tune-y] 459: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 460: minsplit=20; minbucket=8; cp=0.078

    ## [Tune-y] 460: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 461: minsplit=10; minbucket=9; cp=0.078

    ## [Tune-y] 461: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 462: minsplit=11; minbucket=9; cp=0.078

    ## [Tune-y] 462: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 463: minsplit=12; minbucket=9; cp=0.078

    ## [Tune-y] 463: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 464: minsplit=13; minbucket=9; cp=0.078

    ## [Tune-y] 464: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 465: minsplit=14; minbucket=9; cp=0.078

    ## [Tune-y] 465: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 466: minsplit=16; minbucket=9; cp=0.078

    ## [Tune-y] 466: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 467: minsplit=17; minbucket=9; cp=0.078

    ## [Tune-y] 467: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 468: minsplit=18; minbucket=9; cp=0.078

    ## [Tune-y] 468: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 469: minsplit=19; minbucket=9; cp=0.078

    ## [Tune-y] 469: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 470: minsplit=20; minbucket=9; cp=0.078

    ## [Tune-y] 470: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 471: minsplit=10; minbucket=10; cp=0.078

    ## [Tune-y] 471: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 472: minsplit=11; minbucket=10; cp=0.078

    ## [Tune-y] 472: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 473: minsplit=12; minbucket=10; cp=0.078

    ## [Tune-y] 473: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 474: minsplit=13; minbucket=10; cp=0.078

    ## [Tune-y] 474: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 475: minsplit=14; minbucket=10; cp=0.078

    ## [Tune-y] 475: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 476: minsplit=16; minbucket=10; cp=0.078

    ## [Tune-y] 476: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 477: minsplit=17; minbucket=10; cp=0.078

    ## [Tune-y] 477: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 478: minsplit=18; minbucket=10; cp=0.078

    ## [Tune-y] 478: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 479: minsplit=19; minbucket=10; cp=0.078

    ## [Tune-y] 479: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 480: minsplit=20; minbucket=10; cp=0.078

    ## [Tune-y] 480: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 481: minsplit=10; minbucket=5; cp=0.089

    ## [Tune-y] 481: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 482: minsplit=11; minbucket=5; cp=0.089

    ## [Tune-y] 482: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 483: minsplit=12; minbucket=5; cp=0.089

    ## [Tune-y] 483: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 484: minsplit=13; minbucket=5; cp=0.089

    ## [Tune-y] 484: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 485: minsplit=14; minbucket=5; cp=0.089

    ## [Tune-y] 485: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 486: minsplit=16; minbucket=5; cp=0.089

    ## [Tune-y] 486: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 487: minsplit=17; minbucket=5; cp=0.089

    ## [Tune-y] 487: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 488: minsplit=18; minbucket=5; cp=0.089

    ## [Tune-y] 488: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 489: minsplit=19; minbucket=5; cp=0.089

    ## [Tune-y] 489: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 490: minsplit=20; minbucket=5; cp=0.089

    ## [Tune-y] 490: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 491: minsplit=10; minbucket=6; cp=0.089

    ## [Tune-y] 491: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 492: minsplit=11; minbucket=6; cp=0.089

    ## [Tune-y] 492: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 493: minsplit=12; minbucket=6; cp=0.089

    ## [Tune-y] 493: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 494: minsplit=13; minbucket=6; cp=0.089

    ## [Tune-y] 494: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 495: minsplit=14; minbucket=6; cp=0.089

    ## [Tune-y] 495: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 496: minsplit=16; minbucket=6; cp=0.089

    ## [Tune-y] 496: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 497: minsplit=17; minbucket=6; cp=0.089

    ## [Tune-y] 497: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 498: minsplit=18; minbucket=6; cp=0.089

    ## [Tune-y] 498: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 499: minsplit=19; minbucket=6; cp=0.089

    ## [Tune-y] 499: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 500: minsplit=20; minbucket=6; cp=0.089

    ## [Tune-y] 500: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 501: minsplit=10; minbucket=7; cp=0.089

    ## [Tune-y] 501: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 502: minsplit=11; minbucket=7; cp=0.089

    ## [Tune-y] 502: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 503: minsplit=12; minbucket=7; cp=0.089

    ## [Tune-y] 503: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 504: minsplit=13; minbucket=7; cp=0.089

    ## [Tune-y] 504: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 505: minsplit=14; minbucket=7; cp=0.089

    ## [Tune-y] 505: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 506: minsplit=16; minbucket=7; cp=0.089

    ## [Tune-y] 506: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 507: minsplit=17; minbucket=7; cp=0.089

    ## [Tune-y] 507: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 508: minsplit=18; minbucket=7; cp=0.089

    ## [Tune-y] 508: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 509: minsplit=19; minbucket=7; cp=0.089

    ## [Tune-y] 509: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 510: minsplit=20; minbucket=7; cp=0.089

    ## [Tune-y] 510: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 511: minsplit=10; minbucket=8; cp=0.089

    ## [Tune-y] 511: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 512: minsplit=11; minbucket=8; cp=0.089

    ## [Tune-y] 512: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 513: minsplit=12; minbucket=8; cp=0.089

    ## [Tune-y] 513: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 514: minsplit=13; minbucket=8; cp=0.089

    ## [Tune-y] 514: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 515: minsplit=14; minbucket=8; cp=0.089

    ## [Tune-y] 515: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 516: minsplit=16; minbucket=8; cp=0.089

    ## [Tune-y] 516: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 517: minsplit=17; minbucket=8; cp=0.089

    ## [Tune-y] 517: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 518: minsplit=18; minbucket=8; cp=0.089

    ## [Tune-y] 518: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 519: minsplit=19; minbucket=8; cp=0.089

    ## [Tune-y] 519: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 520: minsplit=20; minbucket=8; cp=0.089

    ## [Tune-y] 520: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 521: minsplit=10; minbucket=9; cp=0.089

    ## [Tune-y] 521: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 522: minsplit=11; minbucket=9; cp=0.089

    ## [Tune-y] 522: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 523: minsplit=12; minbucket=9; cp=0.089

    ## [Tune-y] 523: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 524: minsplit=13; minbucket=9; cp=0.089

    ## [Tune-y] 524: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 525: minsplit=14; minbucket=9; cp=0.089

    ## [Tune-y] 525: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 526: minsplit=16; minbucket=9; cp=0.089

    ## [Tune-y] 526: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 527: minsplit=17; minbucket=9; cp=0.089

    ## [Tune-y] 527: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 528: minsplit=18; minbucket=9; cp=0.089

    ## [Tune-y] 528: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 529: minsplit=19; minbucket=9; cp=0.089

    ## [Tune-y] 529: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 530: minsplit=20; minbucket=9; cp=0.089

    ## [Tune-y] 530: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 531: minsplit=10; minbucket=10; cp=0.089

    ## [Tune-y] 531: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 532: minsplit=11; minbucket=10; cp=0.089

    ## [Tune-y] 532: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 533: minsplit=12; minbucket=10; cp=0.089

    ## [Tune-y] 533: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 534: minsplit=13; minbucket=10; cp=0.089

    ## [Tune-y] 534: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 535: minsplit=14; minbucket=10; cp=0.089

    ## [Tune-y] 535: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 536: minsplit=16; minbucket=10; cp=0.089

    ## [Tune-y] 536: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 537: minsplit=17; minbucket=10; cp=0.089

    ## [Tune-y] 537: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 538: minsplit=18; minbucket=10; cp=0.089

    ## [Tune-y] 538: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 539: minsplit=19; minbucket=10; cp=0.089

    ## [Tune-y] 539: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 540: minsplit=20; minbucket=10; cp=0.089

    ## [Tune-y] 540: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 541: minsplit=10; minbucket=5; cp=0.1

    ## [Tune-y] 541: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 542: minsplit=11; minbucket=5; cp=0.1

    ## [Tune-y] 542: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 543: minsplit=12; minbucket=5; cp=0.1

    ## [Tune-y] 543: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 544: minsplit=13; minbucket=5; cp=0.1

    ## [Tune-y] 544: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 545: minsplit=14; minbucket=5; cp=0.1

    ## [Tune-y] 545: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 546: minsplit=16; minbucket=5; cp=0.1

    ## [Tune-y] 546: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 547: minsplit=17; minbucket=5; cp=0.1

    ## [Tune-y] 547: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 548: minsplit=18; minbucket=5; cp=0.1

    ## [Tune-y] 548: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 549: minsplit=19; minbucket=5; cp=0.1

    ## [Tune-y] 549: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 550: minsplit=20; minbucket=5; cp=0.1

    ## [Tune-y] 550: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 551: minsplit=10; minbucket=6; cp=0.1

    ## [Tune-y] 551: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 552: minsplit=11; minbucket=6; cp=0.1

    ## [Tune-y] 552: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 553: minsplit=12; minbucket=6; cp=0.1

    ## [Tune-y] 553: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 554: minsplit=13; minbucket=6; cp=0.1

    ## [Tune-y] 554: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 555: minsplit=14; minbucket=6; cp=0.1

    ## [Tune-y] 555: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 556: minsplit=16; minbucket=6; cp=0.1

    ## [Tune-y] 556: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 557: minsplit=17; minbucket=6; cp=0.1

    ## [Tune-y] 557: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 558: minsplit=18; minbucket=6; cp=0.1

    ## [Tune-y] 558: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 559: minsplit=19; minbucket=6; cp=0.1

    ## [Tune-y] 559: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 560: minsplit=20; minbucket=6; cp=0.1

    ## [Tune-y] 560: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 561: minsplit=10; minbucket=7; cp=0.1

    ## [Tune-y] 561: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 562: minsplit=11; minbucket=7; cp=0.1

    ## [Tune-y] 562: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 563: minsplit=12; minbucket=7; cp=0.1

    ## [Tune-y] 563: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 564: minsplit=13; minbucket=7; cp=0.1

    ## [Tune-y] 564: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 565: minsplit=14; minbucket=7; cp=0.1

    ## [Tune-y] 565: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 566: minsplit=16; minbucket=7; cp=0.1

    ## [Tune-y] 566: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 567: minsplit=17; minbucket=7; cp=0.1

    ## [Tune-y] 567: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 568: minsplit=18; minbucket=7; cp=0.1

    ## [Tune-y] 568: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 569: minsplit=19; minbucket=7; cp=0.1

    ## [Tune-y] 569: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 570: minsplit=20; minbucket=7; cp=0.1

    ## [Tune-y] 570: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 571: minsplit=10; minbucket=8; cp=0.1

    ## [Tune-y] 571: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 572: minsplit=11; minbucket=8; cp=0.1

    ## [Tune-y] 572: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 573: minsplit=12; minbucket=8; cp=0.1

    ## [Tune-y] 573: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 574: minsplit=13; minbucket=8; cp=0.1

    ## [Tune-y] 574: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 575: minsplit=14; minbucket=8; cp=0.1

    ## [Tune-y] 575: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 576: minsplit=16; minbucket=8; cp=0.1

    ## [Tune-y] 576: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 577: minsplit=17; minbucket=8; cp=0.1

    ## [Tune-y] 577: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 578: minsplit=18; minbucket=8; cp=0.1

    ## [Tune-y] 578: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 579: minsplit=19; minbucket=8; cp=0.1

    ## [Tune-y] 579: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 580: minsplit=20; minbucket=8; cp=0.1

    ## [Tune-y] 580: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 581: minsplit=10; minbucket=9; cp=0.1

    ## [Tune-y] 581: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 582: minsplit=11; minbucket=9; cp=0.1

    ## [Tune-y] 582: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 583: minsplit=12; minbucket=9; cp=0.1

    ## [Tune-y] 583: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 584: minsplit=13; minbucket=9; cp=0.1

    ## [Tune-y] 584: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 585: minsplit=14; minbucket=9; cp=0.1

    ## [Tune-y] 585: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 586: minsplit=16; minbucket=9; cp=0.1

    ## [Tune-y] 586: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 587: minsplit=17; minbucket=9; cp=0.1

    ## [Tune-y] 587: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 588: minsplit=18; minbucket=9; cp=0.1

    ## [Tune-y] 588: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 589: minsplit=19; minbucket=9; cp=0.1

    ## [Tune-y] 589: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 590: minsplit=20; minbucket=9; cp=0.1

    ## [Tune-y] 590: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 591: minsplit=10; minbucket=10; cp=0.1

    ## [Tune-y] 591: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 592: minsplit=11; minbucket=10; cp=0.1

    ## [Tune-y] 592: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 593: minsplit=12; minbucket=10; cp=0.1

    ## [Tune-y] 593: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 594: minsplit=13; minbucket=10; cp=0.1

    ## [Tune-y] 594: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 595: minsplit=14; minbucket=10; cp=0.1

    ## [Tune-y] 595: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 596: minsplit=16; minbucket=10; cp=0.1

    ## [Tune-y] 596: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 597: minsplit=17; minbucket=10; cp=0.1

    ## [Tune-y] 597: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 598: minsplit=18; minbucket=10; cp=0.1

    ## [Tune-y] 598: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 599: minsplit=19; minbucket=10; cp=0.1

    ## [Tune-y] 599: acc.test.mean=0.985; time: 0.0 min

    ## [Tune-x] 600: minsplit=20; minbucket=10; cp=0.1

    ## [Tune-y] 600: acc.test.mean=0.985; time: 0.0 min

    ## [Tune] Result: minsplit=12; minbucket=7; cp=0.001 : acc.test.mean=0.999

``` r
treetune$y
```

    ## acc.test.mean 
    ##     0.9992575

``` r
treetune$x
```

    ## $minsplit
    ## [1] 12
    ## 
    ## $minbucket
    ## [1] 7
    ## 
    ## $cp
    ## [1] 0.001

``` r
tree = setHyperPars(mustree, par.vals = treetune$x)
traintree = train(tree, mushroomTrain)
predicttree = predict(traintree, mushroomTest)
table(mushroomtest$class,predicttree$data$response)
```

    ##    
    ##        e    p
    ##   e 1413    0
    ##   p    4 1321

``` r
library(rattle) 
```

    ## Warning: package 'rattle' was built under R version 3.4.2

    ## Rattle: A free graphical interface for data science with R.
    ## Version 5.1.0 Copyright (c) 2006-2017 Togaware Pty Ltd.
    ## Type 'rattle()' to shake, rattle, and roll your data.

``` r
fancyRpartPlot(traintree$learner.model)
```

![](mushroomsclassification_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-15-1.png)

From the above the Decression Tree generationg result close to 100 which look like a over fit. we can club some more entires from orginal set and try to test the model

``` r
nsample = sample(2,nrow(mushroombase),replace = TRUE)

test1mushroom = mushroombase[nsample == 1,]
test2mushroom = mushroombase[nsample == 2,]
mushroomTest1 = makeClassifTask(data= test1mushroom,target = "class")
```

    ## Warning in makeTask(type = type, data = data, weights = weights, blocking =
    ## blocking, : Empty factor levels were dropped for columns: cap.surface

``` r
mushroomTest2 = makeClassifTask(data= test2mushroom,target = "class")
```

``` r
traintree1 = train(tree, mushroomTrain)
predicttree1 = predict(traintree1, mushroomTest1)
table(test1mushroom$class,predicttree1$data$response)
```

    ##    
    ##        e    p
    ##   e 2053    0
    ##   p    3 1991

``` r
library(ggplot2)
nsample = sample(2,nrow(mushroombase),replace = TRUE)

test1mushroom = mushroombase[nsample == 1,]
test2mushroom = mushroombase[nsample == 2,]
mushroomTest1 = makeClassifTask(data= test1mushroom,target = "class")
```

    ## Warning in makeTask(type = type, data = data, weights = weights, blocking =
    ## blocking, : Empty factor levels were dropped for columns: cap.surface

``` r
mushroomTest2 = makeClassifTask(data= test2mushroom,target = "class")
traintree2 = train(tree, mushroomTrain)
predicttree2 = predict(traintree2, mushroomTest2)
table(test2mushroom$class,predicttree2$data$response)
```

    ##    
    ##        e    p
    ##   e 2139    0
    ##   p    8 1939

``` r
#From the Above the given dataset works well for the Decision Tree classification as for all the three test it is #producing more than 98% so we can use this model .


library(pROC)
```

    ## Warning: package 'pROC' was built under R version 3.4.3

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
library(ROCR)
```

    ## Warning: package 'ROCR' was built under R version 3.4.2

    ## Loading required package: gplots

    ## Warning: package 'gplots' was built under R version 3.4.2

    ## 
    ## Attaching package: 'gplots'

    ## The following object is masked from 'package:stats':
    ## 
    ##     lowess

    ## 
    ## Attaching package: 'ROCR'

    ## The following object is masked from 'package:mlr':
    ## 
    ##     performance

``` r
library(gplots)
actual = c()
predicted1 = c()
for(n in test2mushroom$class ){
  if ( n == "e"){
    actual = c(actual,0)
  }
  else{
    actual = c(actual,1)
  }
  
}
for(n1 in predicttree2$data$response ){
  if ( n1 == "e"){
    predicted1 = c(predicted1,0)
  }
  else{
    predicted1 = c(predicted1,1)
  }
  
}
destreepredition = prediction(actual,predicted1)

perform = performance(destreepredition, measure = "tpr", x.measure = "fpr")

auc <- performance(destreepredition, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perform@x.values),
                       tpr=unlist(perform@y.values))
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr),color= "blue") +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))
```

![](mushroomsclassification_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-18-1.png) The above is almost a ideal perfect ROC Curve which is not possible in many cases.

Naive bayes:

``` r
library(e1071)
```

    ## Warning: package 'e1071' was built under R version 3.4.2

    ## 
    ## Attaching package: 'e1071'

    ## The following object is masked from 'package:mlr':
    ## 
    ##     impute

``` r
naivemodel = naiveBayes(class ~ . ,data = mushroomtrain)
summary(naivemodel)
```

    ##         Length Class  Mode     
    ## apriori  2     table  numeric  
    ## tables  22     -none- list     
    ## levels   2     -none- character
    ## call     4     -none- call

``` r
str(naivemodel)
```

    ## List of 4
    ##  $ apriori: 'table' int [1:2(1d)] 2795 2591
    ##   ..- attr(*, "dimnames")=List of 1
    ##   .. ..$ Y: chr [1:2] "e" "p"
    ##  $ tables :List of 22
    ##   ..$ cap.shape               : table [1:2, 1:6] 0.100179 0.011193 0 0.000772 0.374955 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y        : chr [1:2] "e" "p"
    ##   .. .. ..$ cap.shape: chr [1:6] "b" "c" "f" "k" ...
    ##   ..$ cap.surface             : table [1:2, 1:4] 0.378533 0.189502 0 0.000386 0.26619 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y          : chr [1:2] "e" "p"
    ##   .. .. ..$ cap.surface: chr [1:4] "f" "g" "s" "y"
    ##   ..$ cap.color               : table [1:2, 1:10] 0.01181 0.03088 0.00537 0.00232 0.1424 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y        : chr [1:2] "e" "p"
    ##   .. .. ..$ cap.color: chr [1:10] "b" "c" "e" "g" ...
    ##   ..$ bruises                 : table [1:2, 1:2] 0.347 0.842 0.653 0.158
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y      : chr [1:2] "e" "p"
    ##   .. .. ..$ bruises: chr [1:2] "f" "t"
    ##   ..$ odor                    : table [1:2, 1:9] 0.0916 0 0 0.0506 0 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y   : chr [1:2] "e" "p"
    ##   .. .. ..$ odor: chr [1:9] "a" "c" "f" "l" ...
    ##   ..$ gill.attachment         : table [1:2, 1:2] 0.04687 0.00502 0.95313 0.99498
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y              : chr [1:2] "e" "p"
    ##   .. .. ..$ gill.attachment: chr [1:2] "a" "f"
    ##   ..$ gill.spacing            : table [1:2, 1:2] 0.7141 0.9726 0.2859 0.0274
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y           : chr [1:2] "e" "p"
    ##   .. .. ..$ gill.spacing: chr [1:2] "c" "w"
    ##   ..$ gill.size               : table [1:2, 1:2] 0.936 0.431 0.064 0.569
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y        : chr [1:2] "e" "p"
    ##   .. .. ..$ gill.size: chr [1:2] "b" "n"
    ##   ..$ gill.color              : table [1:2, 1:12] 0 0.4404 0.0204 0 0.0565 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y         : chr [1:2] "e" "p"
    ##   .. .. ..$ gill.color: chr [1:12] "b" "e" "g" "h" ...
    ##   ..$ stalk.shape             : table [1:2, 1:2] 0.38 0.492 0.62 0.508
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y          : chr [1:2] "e" "p"
    ##   .. .. ..$ stalk.shape: chr [1:2] "e" "t"
    ##   ..$ stalk.root              : table [1:2, 1:5] 0.172 0.447 0.456 0.474 0.121 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y         : chr [1:2] "e" "p"
    ##   .. .. ..$ stalk.root: chr [1:5] "?" "b" "c" "e" ...
    ##   ..$ stalk.surface.above.ring: table [1:2, 1:4] 0.0923 0.0351 0.0358 0.5681 0.8683 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y                       : chr [1:2] "e" "p"
    ##   .. .. ..$ stalk.surface.above.ring: chr [1:4] "f" "k" "s" "y"
    ##   ..$ stalk.surface.below.ring: table [1:2, 1:4] 0.1088 0.0332 0.0354 0.5535 0.8068 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y                       : chr [1:2] "e" "p"
    ##   .. .. ..$ stalk.surface.below.ring: chr [1:4] "f" "k" "s" "y"
    ##   ..$ stalk.color.above.ring  : table [1:2, 1:9] 0 0.10845 0 0.00926 0.02218 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y                     : chr [1:2] "e" "p"
    ##   .. .. ..$ stalk.color.above.ring: chr [1:9] "b" "c" "e" "g" ...
    ##   ..$ stalk.color.below.ring  : table [1:2, 1:9] 0 0.10266 0 0.00926 0.02075 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y                     : chr [1:2] "e" "p"
    ##   .. .. ..$ stalk.color.below.ring: chr [1:9] "b" "c" "e" "g" ...
    ##   ..$ veil.type               : table [1:2, 1] 1 1
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y        : chr [1:2] "e" "p"
    ##   .. .. ..$ veil.type: chr "p"
    ##   ..$ veil.color              : table [1:2, 1:4] 0.0243 0 0.0225 0 0.9531 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y         : chr [1:2] "e" "p"
    ##   .. .. ..$ veil.color: chr [1:4] "n" "o" "w" "y"
    ##   ..$ ring.number             : table [1:2, 1:3] 0 0.00926 0.87478 0.9699 0.12522 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y          : chr [1:2] "e" "p"
    ##   .. .. ..$ ring.number: chr [1:3] "n" "o" "t"
    ##   ..$ ring.type               : table [1:2, 1:5] 0.2361 0.4489 0.0107 0 0 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y        : chr [1:2] "e" "p"
    ##   .. .. ..$ ring.type: chr [1:5] "e" "f" "l" "n" ...
    ##   ..$ spore.print.color       : table [1:2, 1:9] 0.0118 0 0.0107 0.4006 0.3821 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y                : chr [1:2] "e" "p"
    ##   .. .. ..$ spore.print.color: chr [1:9] "b" "h" "k" "n" ...
    ##   ..$ population              : table [1:2, 1:6] 0.0898 0 0.068 0.0124 0.0952 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y         : chr [1:2] "e" "p"
    ##   .. .. ..$ population: chr [1:6] "a" "c" "n" "s" ...
    ##   ..$ habitat                 : table [1:2, 1:7] 0.4494 0.325 0.3363 0.1868 0.0562 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ Y      : chr [1:2] "e" "p"
    ##   .. .. ..$ habitat: chr [1:7] "d" "g" "l" "m" ...
    ##  $ levels : chr [1:2] "e" "p"
    ##  $ call   : language naiveBayes.default(x = X, y = Y, laplace = laplace)
    ##  - attr(*, "class")= chr "naiveBayes"

``` r
library(ggplot2)
naivepredict = predict(naivemodel,mushroomtest[,2:23])
table(mushroomtest$class ,naivepredict)
```

    ##    naivepredict
    ##        e    p
    ##   e 1402   11
    ##   p  146 1179

``` r
actual1 = c()
predicted2 = c()
for(n in mushroomtest$class ){
  if ( n == "e"){
    actual1 = c(actual1,0)
  }
  else{
    actual1 = c(actual1,1)
  }
  
}
for(n1 in naivepredict ){
  if ( n1 == "e"){
    predicted2 = c(predicted2,0)
  }
  else{
    predicted2 = c(predicted2,1)
  }
  
}

naivepredicted = prediction(actual1,predicted2)

perform1 = performance(naivepredicted, measure = "tpr", x.measure = "fpr")
naiveperform = performance(naivepredicted, measure = "auc")
auc = naiveperform@y.values[[1]]


roc.data1 <- data.frame(fpr=unlist(perform1@x.values),
                       tpr=unlist(perform1@y.values))
ggplot(roc.data1, aes(x = fpr,  ymin=0, ymax=tpr))+ geom_ribbon(alpha=0.5) +
    geom_line(aes(y=tpr)) + 
    ggtitle(paste0("ROC Curve w/ AUC=", auc))
```

![](mushroomsclassification_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-21-1.png)

From the above we can observe decsition tree perfom well.
