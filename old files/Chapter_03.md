Chapter 03
================
A Solomon Kurz
2018-07-01

3.2. Example with dichotomous *X*: The influence of presumed media influence
----------------------------------------------------------------------------

Here we load a couple necessary packages, load the data, and take a peek at them.

``` r
library(readr)
library(tidyverse)

pmi <- read_csv("data/pmi/pmi.csv")

glimpse(pmi)
```

    ## Observations: 123
    ## Variables: 6
    ## $ cond     <int> 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0...
    ## $ pmi      <dbl> 7.0, 6.0, 5.5, 6.5, 6.0, 5.5, 3.5, 6.0, 4.5, 7.0, 1.0, 6.0, 5.0, 7.0, 7.0, 7.0, 4.5, 3.5...
    ## $ import   <int> 6, 1, 6, 6, 5, 1, 1, 6, 6, 6, 3, 3, 4, 7, 1, 6, 3, 3, 2, 4, 4, 6, 7, 4, 5, 4, 6, 5, 5, 7...
    ## $ reaction <dbl> 5.25, 1.25, 5.00, 2.75, 2.50, 1.25, 1.50, 4.75, 4.25, 6.25, 1.25, 2.75, 3.75, 5.00, 4.00...
    ## $ gender   <int> 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1...
    ## $ age      <dbl> 51.0, 40.0, 26.0, 21.0, 27.0, 25.0, 23.0, 25.0, 22.0, 24.0, 22.0, 21.0, 23.0, 21.0, 22.0...

You can get the male/female split like so:

``` r
pmi %>% 
  group_by(gender) %>% 
  count()
```

    ## # A tibble: 2 x 2
    ## # Groups:   gender [2]
    ##   gender     n
    ##    <int> <int>
    ## 1      0    80
    ## 2      1    43

Here is the split by `condition`:

``` r
pmi %>% 
  group_by(cond) %>% 
  count()
```

    ## # A tibble: 2 x 2
    ## # Groups:   cond [2]
    ##    cond     n
    ##   <int> <int>
    ## 1     0    65
    ## 2     1    58

Here is how to get the ungrouped mean and *SD* values for `reaction` and `pmi`, as presented in Table 3.1,

``` r
pmi %>% 
  select(reaction, pmi) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 2 x 3
    ##   key       mean    sd
    ##   <chr>    <dbl> <dbl>
    ## 1 pmi       5.60  1.32
    ## 2 reaction  3.48  1.55

You might get the mean and *SD* values for `reaction` and `pmi` grouped by `cond` like this:

``` r
pmi %>% 
  select(reaction, pmi, cond) %>% 
  gather(key, value, -cond) %>% 
  group_by(cond, key) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 4 x 4
    ## # Groups:   cond [2]
    ##    cond key       mean    sd
    ##   <int> <chr>    <dbl> <dbl>
    ## 1     0 pmi       5.38  1.34
    ## 2     0 reaction  3.25  1.61
    ## 3     1 pmi       5.85  1.27
    ## 4     1 reaction  3.75  1.45

Let's load our primary statistical package.

``` r
library(brms)
```

Before we begin, I should acknowledge that I greatly benefited by [this great blog post](http://www.imachordata.com/bayesian-sem-with-brms/) by Jarrett Byrnes.

In brms, we handle mediation models using the [multivariate syntax](https://cran.r-project.org/web/packages/brms/vignettes/brms_multivariate.html). There are a few ways to do this. Let's start simple.

If you look at the path model in Figure 3.3, you'll note that `reaction` is predicted by `pmi` and `cond`. `pmi`, in turn, is predicted solely by `cond`. So we have two regression models, which is just the kind of thing the brms multivariate syntax is for. So first let's specify both models, which we'll nest in `bf()` functions and save as objects.

``` r
y_model <- bf(reaction ~ 1 + pmi + cond)
m_model <- bf(pmi ~ 1 + cond)
```

Now we have our `bf()` objects in hand, we'll combine them with the `+` operator within the `brm()` function. We'll also specify `set_rescor(FALSE)`--we're not interested in adding a residual correlation between `reaction` and `pmi`.

``` r
model1 <-
  brm(data = pmi, family = gaussian,
      y_model + m_model + set_rescor(FALSE),
      chains = 4, cores = 4)
```

Here are our results.

``` r
print(model1)
```

    ##  Family: MV(gaussian, gaussian) 
    ##   Links: mu = identity; sigma = identity
    ##          mu = identity; sigma = identity 
    ## Formula: reaction ~ 1 + pmi + cond 
    ##          pmi ~ 1 + cond 
    ##    Data: pmi (Number of observations: 123) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##                    Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## reaction_Intercept     0.53      0.56    -0.57     1.60       4000 1.00
    ## pmi_Intercept          5.38      0.17     5.06     5.71       4000 1.00
    ## reaction_pmi           0.50      0.10     0.32     0.70       4000 1.00
    ## reaction_cond          0.26      0.26    -0.25     0.76       4000 1.00
    ## pmi_cond               0.47      0.24     0.01     0.94       4000 1.00
    ## 
    ## Family Specific Parameters: 
    ##                Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## sigma_reaction     1.41      0.09     1.24     1.60       4000 1.00
    ## sigma_pmi          1.32      0.09     1.17     1.50       4000 1.00
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

If you compare our model summary with the coefficients in the path model in Figure 3.3, you'll see our coefficients are the same. The brms summary also includes intercepts and residual variances, which are typically omitted in path diagrams even though they're still part of the model.

If you're getting lost in all the model output, try this.

``` r
fixef(model1)[3:5, ] %>% round(digits = 3)
```

    ##               Estimate Est.Error   Q2.5 Q97.5
    ## reaction_pmi     0.505     0.099  0.318 0.704
    ## reaction_cond    0.259     0.262 -0.254 0.764
    ## pmi_cond         0.472     0.236  0.006 0.941

Also note that Hayes tends to refer to the intercepts as constants.

In his Table 3.2, he also included the *R*<sup>2</sup> values. Here are ours.

``` r
bayes_R2(model1) %>% round(digits = 3)
```

    ##             Estimate Est.Error Q2.5 Q97.5
    ## R2_reaction    0.209     0.057  0.1 0.322
    ## R2_pmi         0.038     0.030  0.0 0.113

It's worth it to actually plot the *R*<sup>2</sup> distributions.

``` r
# we'll get our color palette from ggthemes
library(ggthemes)

bayes_R2(model1, summary = F) %>% 
  as_tibble() %>% 
  gather() %>% 
  
  ggplot(aes(x = value, fill = key)) +
  geom_density(color = "transparent", alpha = 2/3) +
  scale_fill_colorblind() +  # we got this color palette from the ggthemes package
  coord_cartesian(xlim = 0:1) +
  labs(title = expression(paste("The ", italic("R")^{2}, " distributions for fit0")),
       x = NULL) +
  theme_classic()
```

![](Chapter_03_files/figure-markdown_github/unnamed-chunk-12-1.png)

We went through the trouble of plotting the *R*<sup>2</sup> distributions because it’s useful to understand that they won’t often be symmetric when they’re near their logical boundaries (i.e., 0 and 1). This is where asymmetric Bayesian credible intervals can really shine.

Let's get down to business and examine the indirect effect, the *ab* pathway. In our model:

-   *a* = `pmi_cond`
-   *b* = `reaction_pmi`

You can isolate them with `fixef()[i]`.

``` r
fixef(model1)[5 , ]
```

    ##    Estimate   Est.Error        Q2.5       Q97.5 
    ## 0.472438370 0.235809136 0.005505107 0.941285023

``` r
fixef(model1)[3 , ]
```

    ##   Estimate  Est.Error       Q2.5      Q97.5 
    ## 0.50497430 0.09943277 0.31789622 0.70386003

So the naive approach would be to just multiply them.

``` r
(fixef(model1)[5 , ] * fixef(model1)[3 , ]) %>% round(digits = 3)
```

    ##  Estimate Est.Error      Q2.5     Q97.5 
    ##     0.239     0.023     0.002     0.663

Now, this does get us the correct 'Estimate' (i.e., posterior mean). However, the posterior *SD* and 95% intervals are off. If you want to do this properly, you need to work with the poster samples themselves. Here they are:

``` r
post <- posterior_samples(model1)

glimpse(post)
```

    ## Observations: 4,000
    ## Variables: 8
    ## $ b_reaction_Intercept <dbl> 0.292197477, 0.342669227, 0.366377734, 0.358973771, 0.255912282, 0.843884219...
    ## $ b_pmi_Intercept      <dbl> 5.517267, 5.103582, 5.616524, 5.103351, 5.656004, 5.516082, 5.304989, 5.2830...
    ## $ b_reaction_pmi       <dbl> 0.5690514, 0.5561447, 0.5150801, 0.5173845, 0.5785413, 0.4575690, 0.3318920,...
    ## $ b_reaction_cond      <dbl> -0.0447590060, -0.0009895494, 0.5323765893, 0.1898111591, 0.2625103858, 0.23...
    ## $ b_pmi_cond           <dbl> 0.48323687, 0.45754768, 0.53488279, 0.78870808, 0.27422206, 0.34882640, 0.66...
    ## $ sigma_reaction       <dbl> 1.389594, 1.431525, 1.424760, 1.415241, 1.353212, 1.371856, 1.483129, 1.5377...
    ## $ sigma_pmi            <dbl> 1.288783, 1.300165, 1.337792, 1.148611, 1.596948, 1.160362, 1.287942, 1.2057...
    ## $ lp__                 <dbl> -432.8805, -434.9034, -434.6133, -435.8771, -437.8934, -433.6470, -434.2474,...

Here we compute the indirect effect, `ab`.

``` r
post <-
  post %>% 
  mutate(ab = b_pmi_cond*b_reaction_pmi)
```

Now we have `ab` as a properly computed vector, we can summarize it with the `quantile()` function.

``` r
quantile(post$ab, probs = c(.5, .025, .975)) %>% 
  round(digits = 3)
```

    ##   50%  2.5% 97.5% 
    ## 0.230 0.003 0.513

And we can even visualize it as a density.

``` r
post %>% 
  
  ggplot(aes(x = ab)) +
  geom_density(color = "transparent", 
               fill = colorblind_pal()(3)[3]) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(paste("Our indirect effect, the ", italic("ab"), " pathway")),
       x = NULL) +
  theme_classic()
```

![](Chapter_03_files/figure-markdown_github/unnamed-chunk-18-1.png)

It's also worth pointing out that as the indirect effect isn't perfectly symmetric, its mean and median aren't quite the same.

``` r
post %>% 
  summarize(mean = mean(ab),
            median = median(ab)) %>% 
  round(digits = 3)
```

    ##    mean median
    ## 1 0.238   0.23

Their magnitudes are similar, but this asymmetry will be a source of contrast to our estimates and those in the text. This is also something to consider when reporting on central tendency. When the indirect effect--or any other parameter, for that matter--is quite asymmetric, you might prefer reporting the median rather than the mean.

On page 90, Hayes computed the *adjusted means* for *Y*. For both `cond == 1` and `cond == 0`, he computed the expected values for `reaction` when `pmi` was at its mean. A natural way to do that in brms is with `fitted()`. First we'll put our input values for `cond` and `pmi` in a tibble, which we'll call `nd`. Then we'll feed `nd` into the `newdata` argument within the `fitted()` function.

``` r
nd <-
  tibble(cond = 1:0,
         pmi = mean(pmi$pmi))

fitted(model1, newdata = nd)
```

    ## , , reaction
    ## 
    ##      Estimate Est.Error     Q2.5    Q97.5
    ## [1,] 3.621590 0.1882552 3.253196 3.989656
    ## [2,] 3.362723 0.1769166 3.019732 3.711897
    ## 
    ## , , pmi
    ## 
    ##      Estimate Est.Error     Q2.5    Q97.5
    ## [1,] 5.852737 0.1742271 5.510209 6.191928
    ## [2,] 5.380298 0.1653719 5.058686 5.705105

Because `model1` is a multivariate model, `fitted()` returns the model-implied summaries for both `reaction` and `pmi`. If you just want the adjusted means for `reaction`, you can use the `resp` argument within `fitted()`.

``` r
fitted(model1, newdata = nd, resp = "reaction") %>% round(digits = 3)
```

    ##      Estimate Est.Error  Q2.5 Q97.5
    ## [1,]    3.622     0.188 3.253 3.990
    ## [2,]    3.363     0.177 3.020 3.712

Note how this is where the two values in the *Y* adjusted column in Table 3.1 came from.

However, if we want to reproduce how Hayes computed the total effect (i.e. *c*<sup>′</sup> + *ab*), we'll need to work with the posterior itself, `post`. Recall, we've already saved the indirect effect as a vector, `ab`. The direct effect, *c*<sup>′</sup>, is labeled `b_reaction_cond` within `post`. in order to get the total effect, *c*, all we need to is add those vectors together.

``` r
post <-
  post %>% 
  mutate(total_effect = b_reaction_cond + ab)
```

Here's the posterior mean with its 95% intervals

``` r
post %>% 
  summarize(mean = mean(total_effect),
            ll = quantile(total_effect, prob = .025),
            ul = quantile(total_effect, prob = .975))
```

    ##        mean        ll       ul
    ## 1 0.4971552 -0.049025 1.055737

### ~~Estimation of the model in PROCESS for SPSS and SAS.~~

Nothing new for us, here.

3.4 Statistical inference
-------------------------

### Inference about the direct effect of *X* on *Y*.

In this section, Hayes provides a *t*-test and corresponding *p*-value for the direct effect (i.e., *c*<sup>′</sup>, `b_reaction_cond`). Instead of the *t*-test, we can just look at the posterior distribution.

``` r
post %>% 
  
  ggplot(aes(x = b_reaction_cond)) +
  geom_density(color = "transparent", 
               fill = colorblind_pal()(4)[4]) +
  geom_vline(xintercept = 0, color = "white", linetype = 2) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(paste("Yep, 0 is a credible value for ", italic("c"), ".")),
       x = NULL) +
  theme_classic()
```

![](Chapter_03_files/figure-markdown_github/unnamed-chunk-24-1.png)

If you wanted to quantify what proportion of the density was less than 0, you could do:

``` r
post %>% 
  summarize(proportion_below_zero = filter(., b_reaction_cond < 0) %>% nrow()/nrow(.))
```

    ##   proportion_below_zero
    ## 1               0.15825

This is something like a Bayesian *p*-value. But of course, you could always just look at the posterior intervals.

``` r
posterior_interval(model1)["b_reaction_cond", ]
```

    ##       2.5%      97.5% 
    ## -0.2536169  0.7643558

### Inference about the indirect of *X* on *Y* through *M*.

#### The normal theory approach.

This is not our approach.

#### Bootstrap confidence interval.

This is not our approach.

However, Markov chain Monte Carlo (MCMC) methods are iterative and share some characteristics with boostrapping. On page 98, Hayes outlined 6 steps for constructing the *ab* bootstrap confidence interval. Here are our responses to those steps w/r/t Bayes with MCMC--or in our case HMC (i.e., Hamiltonian Monte Carlo).

If HMC or MCMC, in general, are new to you, you might check out [this lecture](https://www.youtube.com/watch?v=BWEtS3HuU5A&t=7s) or the [Stan reference manual](http://mc-stan.org/users/documentation/) if you're more technically oriented.

Anyway, Hayes's 6 steps:

##### 1.

With HMC we do not take random samples of the data themselves. Rather, we take random draws from the posterior distribution. The posterior distribution is the joint probability distribution of our model.

##### 2.

After we fit our model with the `brm()` function and save our posterior draws in a data frame (i.e., `post <- posterior_samples(my_model_fit)`), we then make a new column (a.k.a. vector, variable) that is the product of our coefficients for the *a* and *b* pathways. In the example above, this looked like `post %>% mutate(ab = b_pmi_cond*b_reaction_pmi)`. Let's take a look at those columns.

``` r
post %>% 
  select(b_pmi_cond, b_reaction_pmi, ab) %>% 
  slice(1:10)
```

    ##    b_pmi_cond b_reaction_pmi        ab
    ## 1   0.4832369      0.5690514 0.2749866
    ## 2   0.4575477      0.5561447 0.2544627
    ## 3   0.5348828      0.5150801 0.2755075
    ## 4   0.7887081      0.5173845 0.4080653
    ## 5   0.2742221      0.5785413 0.1586488
    ## 6   0.3488264      0.4575690 0.1596122
    ## 7   0.6657279      0.3318920 0.2209498
    ## 8   0.8835803      0.3417820 0.3019918
    ## 9   0.5584497      0.4324235 0.2414868
    ## 10  0.8112304      0.4950180 0.4015737

Our data frame, `post`, has 4000 rows. Why 4000? By default, brms runs 4 HMC chains. Each chain has 2000 iterations, 1000 of which are warmups, which we always discard. As such, there are 1000 good iterations left in each chain and 1000\*4 = 4000. We can change these defaults as needed. At this point in the project, the default settings have been fine.

Each row in `post` contains the parameter values based on one of those draws. And again, these are draws from the posterior distribution. They are not draws from the data.

##### 3.

We don't refit the model *k* times based on the samples from the data. We take a number of draws from the posterior distribution. Hayes likes to take 5000 samples when he bootstraps. Happily, that number is quite similar to our 4000 HMC draws. Whether 5000, 4000 or 10,000, these are all large enough numbers that the distributions become fairly stable. With HMC, however, you might want to increase the number of iterations if the effective sample size, 'Eff.Sample' in the `print()` output, is substantially smaller than the number of iterations.

##### 4.

When we use the `quantile()` function to compute our Bayesian credible intervals, we've sorted. Conceptually, we've done this:

``` r
post %>% 
  select(ab) %>% 
  arrange(ab) %>% 
  slice(1:10)
```

    ##            ab
    ## 1  -0.2139011
    ## 2  -0.1519141
    ## 3  -0.1477587
    ## 4  -0.1405426
    ## 5  -0.1344720
    ## 6  -0.1225225
    ## 7  -0.1159545
    ## 8  -0.1153202
    ## 9  -0.1086523
    ## 10 -0.1078631

##### 5.

Yes, this is what we do, too.

``` r
ci <- 95

.5*(100 - ci)
```

    ## [1] 2.5

##### 6.

This is also what we do.

``` r
ci <- 95

(100 - .5*(100 - ci))
```

    ## [1] 97.5

Also, notice the headers in the rightmost two columns in our `posterior_summary()` output:

``` r
posterior_summary(model1)
```

    ##                          Estimate  Est.Error          Q2.5        Q97.5
    ## b_reaction_Intercept    0.5340456 0.56303432 -5.689811e-01    1.6026955
    ## b_pmi_Intercept         5.3802984 0.16537191  5.058686e+00    5.7051051
    ## b_reaction_pmi          0.5049743 0.09943277  3.178962e-01    0.7038600
    ## b_reaction_cond         0.2588670 0.26158002 -2.536169e-01    0.7643558
    ## b_pmi_cond              0.4724384 0.23580914  5.505107e-03    0.9412850
    ## sigma_reaction          1.4070774 0.09081949  1.243940e+00    1.5984746
    ## sigma_pmi               1.3198903 0.08643984  1.165573e+00    1.5025414
    ## lp__                 -434.8793782 1.93948210 -4.396101e+02 -432.2107376

Those .025 and .975 quantiles from above are just what brms is giving us in our 95% Bayesian credible intervals.

Here's our version of Figure 3.5

``` r
# these will come in handy in the subtitle
ll <- quantile(post$ab, probs = .025) %>% round(digits = 3)
ul <- quantile(post$ab, probs = .975) %>% round(digits = 3)

post %>% 
  
  ggplot(aes(x = ab)) +
  geom_histogram(color = "white", size = .25, 
               fill = colorblind_pal()(5)[5],
               binwidth = .025, boundary = 0) +
  geom_vline(xintercept = quantile(post$ab, probs = c(.025, .975)),
             linetype = 3, color = colorblind_pal()(6)[6]) +
  labs(x = expression(paste("Indirect effect (", italic("ab"), ")")),
       y = "Frequency in 4,000 HMC posterior draws",
       subtitle = paste("95% of the posterior draws are between", ll, "and", ul)) +
  theme_classic()
```

![](Chapter_03_files/figure-markdown_github/unnamed-chunk-32-1.png)

Again, as Hayes discussed how to specify different types of intervals in PROCESS on page 102, you can ask for different kinds of intervals in your `print()` or `summary()` output with the `probs` argument, just as you can with `quantile()` when working directly with the posterior draws.

Hayes discussed setting the seed in PROCESS on page 104. You can do this with the `seed` argument in the `brm()` function, too.

#### Alternative "asymmetric" confidence interval approaches.

This section does not quite refer to us. I'm a little surprised Hayes didn't at least dedicate a paragraph or two mentioning Bayesian estimation. Sure, he mentioned Monte Carlo, but not within the context of Bayes. So it goes...

3.5 An example with continuous *X*: Economic stress among small-business owners
-------------------------------------------------------------------------------

Here's the `estress` data.

``` r
estress <- read_csv("data/estress/estress.csv")

glimpse(estress)
```

    ## Observations: 262
    ## Variables: 7
    ## $ tenure   <dbl> 1.67, 0.58, 0.58, 2.00, 5.00, 9.00, 0.00, 2.50, 0.50, 0.58, 9.00, 1.92, 2.00, 1.42, 0.92...
    ## $ estress  <dbl> 6.0, 5.0, 5.5, 3.0, 4.5, 6.0, 5.5, 3.0, 5.5, 6.0, 5.5, 4.0, 3.0, 2.5, 3.5, 6.0, 4.0, 6.0...
    ## $ affect   <dbl> 2.60, 1.00, 2.40, 1.16, 1.00, 1.50, 1.00, 1.16, 1.33, 3.00, 3.00, 2.00, 1.83, 1.16, 1.16...
    ## $ withdraw <dbl> 3.00, 1.00, 3.66, 4.66, 4.33, 3.00, 1.00, 1.00, 2.00, 4.00, 4.33, 1.00, 5.00, 1.66, 4.00...
    ## $ sex      <int> 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1...
    ## $ age      <int> 51, 45, 42, 50, 48, 48, 51, 47, 40, 43, 57, 36, 33, 29, 33, 48, 40, 45, 37, 42, 54, 57, ...
    ## $ ese      <dbl> 5.33, 6.05, 5.26, 4.35, 4.86, 5.05, 3.66, 6.13, 5.26, 4.00, 2.53, 6.60, 5.20, 5.66, 5.66...

The model set up is just like before. There are no complications switching from a binary *X* variable to a continuous one.

``` r
y_model <- bf(withdraw ~ 1 + estress + affect)
m_model <- bf(affect ~ 1 + estress)
```

With our `y_model` and `m_model` defined, we're ready to fit.

``` r
model2 <-
  brm(data = estress, family = gaussian,
      y_model + m_model + set_rescor(FALSE),
      chains = 4, cores = 4)
```

Let's take a look.

``` r
print(model2, digits = 3)
```

    ##  Family: MV(gaussian, gaussian) 
    ##   Links: mu = identity; sigma = identity
    ##          mu = identity; sigma = identity 
    ## Formula: withdraw ~ 1 + estress + affect 
    ##          affect ~ 1 + estress 
    ##    Data: estress (Number of observations: 262) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##                    Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## withdraw_Intercept    1.451     0.248    0.968    1.938       4000 1.000
    ## affect_Intercept      0.803     0.144    0.514    1.090       4000 1.000
    ## withdraw_estress     -0.077     0.051   -0.174    0.025       4000 1.000
    ## withdraw_affect       0.767     0.101    0.565    0.960       4000 1.000
    ## affect_estress        0.172     0.029    0.114    0.232       4000 1.000
    ## 
    ## Family Specific Parameters: 
    ##                Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## sigma_withdraw    1.140     0.052    1.043    1.247       4000 1.000
    ## sigma_affect      0.685     0.030    0.629    0.747       4000 0.999
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

The 'Eff.Sample' and 'Rhat' values look great. Happily, the values in our summary cohere well with those Hayes reported in Table 3.5. Here are our *R*<sup>2</sup> values.

``` r
bayes_R2(model2)
```

    ##              Estimate  Est.Error       Q2.5     Q97.5
    ## R2_withdraw 0.1816900 0.03759369 0.10692794 0.2538932
    ## R2_affect   0.1159291 0.03400884 0.05316538 0.1884766

These are also quite similar to those in the text. Here's our indirect effect.

``` r
# putting the posterior draws into a data frame
post <- posterior_samples(model2)

# computing the ab coefficient with multiplication
post <-
  post %>% 
  mutate(ab = b_affect_estress*b_withdraw_affect)

# getting the posterior median and 95% intervals with `quantile()`
quantile(post$ab, probs = c(.5, .025, .975)) %>% round(digits = 3)
```

    ##   50%  2.5% 97.5% 
    ## 0.131 0.079 0.193

We can visualize its shape, median, and 95% intervals in a density plot.

``` r
post %>% 
  ggplot(aes(x = ab)) +
  geom_density(color = "transparent", 
               fill = colorblind_pal()(7)[7]) +
  geom_vline(xintercept = quantile(post$ab, probs = c(.025, .5, .975)), 
             color = "white", linetype = c(2, 1, 2), size = c(.5, .8, .5)) +
  scale_x_continuous(breaks = quantile(post$ab, probs = c(.025, .5, .975)),
                     labels = quantile(post$ab, probs = c(.025, .5, .975)) %>% round(2) %>% as.character()) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(paste("Behold our ", italic("ab"), "!")),
       x = NULL) +
  theme_classic()
```

![](Chapter_03_files/figure-markdown_github/unnamed-chunk-39-1.png)

Here's *c*<sup>′</sup>, the direct effect of `esterss` predicting `withdraw`.

``` r
posterior_summary(model2)["b_withdraw_estress", ]
```

    ##    Estimate   Est.Error        Q2.5       Q97.5 
    ## -0.07682641  0.05135193 -0.17391731  0.02481739

It has wide flapping intervals which do straddle zero. A little addition will give us the direct effect, *c*.

``` r
post <-
  post %>% 
  mutate(c = b_withdraw_estress + ab)

quantile(post$c, probs = c(.5, .025, .975)) %>% round(digits = 3)
```

    ##    50%   2.5%  97.5% 
    ##  0.057 -0.050  0.159

Note. The analyses in this document were done with:

-   R 3.4.4
-   RStudio 1.1.442
-   rmarkdown 1.9
-   readr 1.1.1
-   tidyverse 1.2.1
-   rstan 2.17.3
-   brms 2.3.2
-   ggthemes 3.5.0

Reference
---------

Hayes, A. F. (2018). *Introduction to mediation, moderation, and conditional process analysis: A regression-based approach.* (2nd ed.). New York, NY, US: The Guilford Press.
