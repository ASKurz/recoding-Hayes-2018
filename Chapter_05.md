Chapter 05
================
A Solomon Kurz
2018-05-30

5.2 Example using the presumed media influence study
----------------------------------------------------

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

``` r
library(brms)

fit0 <- 
  brm(data = pmi, family = gaussian,
      cbind(pmi, import) ~ 1,
      chains = 4, cores = 4)
```

Here's the Bayesian correlation with its posterior *SD* and intervals.

``` r
posterior_summary(fit0)["rescor__pmi__import", ] %>% round(digits = 3)
```

    ##  Estimate Est.Error      Q2.5     Q97.5 
    ##     0.277     0.083     0.108     0.433

``` r
y_model  <- bf(reaction ~ 1 + import + pmi + cond)
m1_model <- bf(import   ~ 1 + cond)
m2_model <- bf(pmi      ~ 1 + cond)
```

Now we have our `bf()` objects in hand, we'll combine them with the `+` operator within the `brm()` function. We'll also specify `set_rescor(FALSE)`--we're not interested in adding a residual correlation between `reaction` and `pmi`.

``` r
fit1 <-
  brm(data = pmi, family = gaussian,
      y_model + m1_model + m2_model + set_rescor(FALSE),
      chains = 4, cores = 4)
```

``` r
print(fit1)
```

    ##  Family: MV(gaussian, gaussian, gaussian) 
    ##   Links: mu = identity; sigma = identity
    ##          mu = identity; sigma = identity
    ##          mu = identity; sigma = identity 
    ## Formula: reaction ~ 1 + import + pmi + cond 
    ##          import ~ 1 + cond 
    ##          pmi ~ 1 + cond 
    ##    Data: pmi (Number of observations: 123) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##                    Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## reaction_Intercept    -0.15      0.53    -1.15     0.87       4000 1.00
    ## import_Intercept       3.91      0.22     3.49     4.33       4000 1.00
    ## pmi_Intercept          5.38      0.16     5.05     5.69       4000 1.00
    ## reaction_import        0.33      0.07     0.18     0.47       4000 1.00
    ## reaction_pmi           0.40      0.10     0.21     0.58       4000 1.00
    ## reaction_cond          0.11      0.24    -0.37     0.59       4000 1.00
    ## import_cond            0.63      0.32    -0.01     1.25       4000 1.00
    ## pmi_cond               0.48      0.24     0.01     0.96       4000 1.00
    ## 
    ## Family Specific Parameters: 
    ##                Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## sigma_reaction     1.30      0.09     1.15     1.49       4000 1.00
    ## sigma_import       1.73      0.11     1.53     1.97       4000 1.00
    ## sigma_pmi          1.32      0.09     1.16     1.50       4000 1.00
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Now we have three criteria, we'll have three Bayesian *R*<sup>2</sup> posteriors.

``` r
library(ggthemes)

bayes_R2(fit1, summary = F) %>% 
  as_tibble() %>% 
  gather() %>% 
  mutate(key = str_remove(key, "R2_")) %>% 
  
  ggplot(aes(x = value, color = key, fill = key)) +
  geom_density(alpha = .5) +
  scale_color_ptol() +
  scale_fill_ptol() +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = 0:1) +
  labs(title = expression(paste("Our ", italic("R")^{2}, " distributions")),
       subtitle = "The densities for import and pmi are asymmetric, small, and largely overlapping. The density for reaction is Gaussian and\nmore impressive in magnitude.",
       x = NULL) +
  theme_minimal() +
  theme(legend.title = element_blank())
```

![](Chapter_05_files/figure-markdown_github/unnamed-chunk-7-1.png)

It'll take a bit of data wrangling to rename/configure our model parameters to the *a*, *b*...configuration.

``` r
post <- posterior_samples(fit1)

post<-
  post %>% 
  mutate(a1 = b_import_cond,
         a2 = b_pmi_cond,
         b1 = b_reaction_import,
         b2 = b_reaction_pmi,
         c_prime = b_reaction_cond) %>% 
  mutate(a1b1 = a1*b1,
         a2b2 = a2*b2) %>% 
  mutate(c = c_prime + a1b1 + a2b2)
```

Here are their summaries, this time using the posterior medians instead of the means.

``` r
post %>% 
  select(a1:c) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(median = median(value), 
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 3)
```

    ## # A tibble: 8 x 4
    ##   key     median       ll    ul
    ##   <chr>    <dbl>    <dbl> <dbl>
    ## 1 a1       0.635 -0.0120  1.25 
    ## 2 a1b1     0.195 -0.00400 0.444
    ## 3 a2       0.479  0.00600 0.957
    ## 4 a2b2     0.179  0.00300 0.436
    ## 5 b1       0.324  0.181   0.470
    ## 6 b2       0.395  0.209   0.579
    ## 7 c        0.502 -0.00800 1.04 
    ## 8 c_prime  0.107 -0.369   0.588

``` r
post %>% 
  mutate(dif = a1b1*b1) %>% 
  summarize(median = median(dif), 
            ll = quantile(dif, probs = .025),
            ul = quantile(dif, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 3)
```

    ##   median     ll    ul
    ## 1  0.061 -0.001 0.189

In the middle paragraph of page 158, Hayes shows how the mean difference in `imprt` between the two `cond` groups multiplied by `b1`, the coefficient of `import` predicting `reaction`, is equal to the `a1b1` indirect effect. He does this with simple algebra using the group means and the point estimates.

Let's follow along. First, here we'll get those two group means and save them as numbers to arbitrary precision.

``` r
(
  means <-
    pmi %>%
    group_by(cond) %>% 
    summarize(mean = mean(import))
 )
```

    ## # A tibble: 2 x 2
    ##    cond  mean
    ##   <int> <dbl>
    ## 1     0  3.91
    ## 2     1  4.53

``` r
(cond_0_mean <- means[1, 2] %>% pull())
```

    ## [1] 3.907692

``` r
(cond_1_mean <- means[2, 2] %>% pull())
```

    ## [1] 4.534483

Here we follow the formula in the last sentence of the paragraph and then compare the results to the posterior for `a1b1`.

``` r
post %>% 
  # Using his formula to make our new vector, `hand_made_a1b1` 
  mutate(hand_made_a1b1 = (cond_1_mean - cond_0_mean)*b1) %>% 
  # Here's all the usual data wrangling
  select(a1b1, hand_made_a1b1) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(mean = mean(value), 
            median = median(value), 
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 3)
```

    ## # A tibble: 2 x 5
    ##   key             mean median       ll    ul
    ##   <chr>          <dbl>  <dbl>    <dbl> <dbl>
    ## 1 a1b1           0.204  0.195 -0.00400 0.444
    ## 2 hand_made_a1b1 0.204  0.203  0.114   0.294

Yep, at the mean, Hayes's formula is spot on. But the distributions are distinct. They differ slightly at the median and vastly in the widths of the posterior intervals. I’m no mathematician, so take this with a grain of salt, but I suspect this has to do with how we used fixed values (i.e., the difference of the subsample means) to compute `hand_made_a1b1`, but all the components in `a1b1` were estimated.

``` r
# nd <- 
#   tibble(cond = 0:1,
#          import = mean(pmi$import),
#          pmi = 0)
# 
# fitted(fit1, 
#        newdata = nd, 
#        resp = "reaction",
#        summary = F) %>% 
#   as_tibble() %>% 
#   rename(cond_0 = V1,
#          cond_1 = V2) %>% 
#   mutate(difference = cond_1 - cond_0) %>% 
#   gather() %>% 
#   group_by(key) %>% 
#   summarize(median = median(value), 
#             ll = quantile(value, probs = .025),
#             ul = quantile(value, probs = .975)) %>% 
#   mutate_if(is_double, round, digits = 3)
```

**More to come...**

Note. The analyses in this document were done with:

-   R 3.4.4
-   RStudio 1.1.442
-   rmarkdown 1.9
-   readr 1.1.1
-   tidyverse 1.2.1
-   rstan 2.17.3
-   brms 2.3.1
-   ggthemes 3.5.0

Reference
---------

Hayes, A. F. (2018). *Introduction to mediation, moderation, and conditional process analysis: A regression-based approach.* (2nd ed.). New York, NY, US: The Guilford Press.
