Chapter 06
================
A Solomon Kurz
2018-07-01

6.2 An example: Sex discrimination in the workplace
---------------------------------------------------

Here we load a couple necessary packages, load the data, and take a `glimpse()`.

``` r
library(readr)
library(tidyverse)

protest <- read_csv("data/protest/protest.csv")

glimpse(protest)
```

    ## Observations: 129
    ## Variables: 6
    ## $ subnum   <int> 209, 44, 124, 232, 30, 140, 27, 64, 67, 182, 85, 109, 122, 69, 45, 28, 170, 66, 168, 97,...
    ## $ protest  <int> 2, 0, 2, 2, 2, 1, 2, 0, 0, 0, 2, 2, 0, 1, 1, 0, 1, 2, 2, 1, 2, 1, 1, 2, 2, 0, 1, 1, 0, 1...
    ## $ sexism   <dbl> 4.87, 4.25, 5.00, 5.50, 5.62, 5.75, 5.12, 6.62, 5.75, 4.62, 4.75, 6.12, 4.87, 5.87, 4.87...
    ## $ angry    <int> 2, 1, 3, 1, 1, 1, 2, 1, 6, 1, 2, 5, 2, 1, 1, 1, 2, 1, 3, 4, 1, 1, 1, 5, 1, 5, 1, 1, 2, 1...
    ## $ liking   <dbl> 4.83, 4.50, 5.50, 5.66, 6.16, 6.00, 4.66, 6.50, 1.00, 6.83, 5.00, 5.66, 5.83, 6.50, 4.50...
    ## $ respappr <dbl> 4.25, 5.75, 4.75, 7.00, 6.75, 5.50, 5.00, 6.25, 3.00, 5.75, 5.25, 7.00, 4.50, 6.25, 5.00...

Here are the ungrouped means and *SD*s for `respappr` and `liking` shown at the bottom of Table 6.1.

``` r
protest %>%
  select(liking:respappr) %>% 
  gather(key, value) %>% 
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 2 x 3
    ##   key       mean    sd
    ##   <chr>    <dbl> <dbl>
    ## 1 liking    5.64  1.05
    ## 2 respappr  4.87  1.35

We compute the summaries for `respappr` and `liking`, grouped by `protest`, like so.

``` r
protest %>%
  select(protest, liking:respappr) %>% 
  gather(key, value, -protest) %>% 
  group_by(protest, key) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 6 x 4
    ## # Groups:   protest [3]
    ##   protest key       mean    sd
    ##     <int> <chr>    <dbl> <dbl>
    ## 1       0 liking    5.31 1.30 
    ## 2       0 respappr  3.88 1.46 
    ## 3       1 liking    5.83 0.819
    ## 4       1 respappr  5.14 1.08 
    ## 5       2 liking    5.75 0.936
    ## 6       2 respappr  5.49 0.936

It looks like Hayes has a typo in the *SD* for `liking` when `protest == 0`. It seemed he accidentally entered the value for when `protest == 1` in that slot.

You'll have to wait a minute to see where the adjusted *Y* values came.

With a little `ifelse()`, computing the dummies `D1` and `D2` is easy enough.

``` r
protest <-
  protest %>% 
  mutate(D1 = ifelse(protest == 1, 1, 0),
         D2 = ifelse(protest == 2, 1, 0))
```

This is the first time we've had a simple univariate regression model in a while. No special `cbind()` syntax or multiple `bf()` formulas. Just straight up `brm()`.

``` r
library(brms)

model1 <-
  brm(data = protest, family = gaussian,
      liking ~ 1 + D1 + D2,
      chains = 4, cores = 4)
```

``` r
fixef(model1)
```

    ##            Estimate Est.Error        Q2.5     Q97.5
    ## Intercept 5.3118620 0.1625518  4.99764595 5.6295263
    ## D1        0.5140292 0.2327801  0.03843715 0.9411392
    ## D2        0.4415353 0.2305622 -0.01087532 0.8898664

Our *R*<sup>2</sup> differes a bit from the OLS version in the text. This shouldn't be surprising when it's near the boundary.

``` r
bayes_R2(model1)
```

    ##      Estimate  Est.Error        Q2.5     Q97.5
    ## R2 0.05839651 0.03574557 0.004527587 0.1362534

Here's its shape. For the plots in this chapter, we'll take a few formatting cues from [Edward Tufte](https://www.edwardtufte.com/tufte/books_vdqi), curtesy of the [ggthemes package](https://cran.r-project.org/web/packages/ggthemes/index.html). The `theme_tufte()` function will change the default font and remove some chart junk. We will take our color palette from [Pokemon](http://pokemonbyreview.blogspot.com/2017/02/311-312-plusle-minun.html) via the [palettetown package](https://cran.r-project.org/web/packages/palettetown/index.html).

``` r
library(ggthemes)
library(palettetown)

bayes_R2(model1, summary = F) %>% 
  as_tibble() %>% 
  
  ggplot(aes(x = R2)) +
  geom_density(size = 0, fill = pokepal(pokemon = "plusle")[2]) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = 0:1) +
  xlab(expression(italic(R)^{2})) +
  theme_tufte() +
  theme(legend.title = element_blank(),
        plot.background = element_rect(fill = pokepal(pokemon = "plusle")[8]))
```

![](Chapter_06_files/figure-markdown_github/unnamed-chunk-8-1.png)

To use the model-implied equations to compute the means for each group on the criterion, we'll extract the posterior samples.

``` r
post <- posterior_samples(model1)

post %>% 
  transmute(Y_np = b_Intercept + b_D1*0 + b_D2*0,
            Y_ip = b_Intercept + b_D1*1 + b_D2*0,
            Y_cp = b_Intercept + b_D1*0 + b_D2*1) %>% 
  gather() %>%
  # this line will order our output the same way Hayes did in the text (p. 197)
  mutate(key = factor(key, levels = c("Y_np", "Y_ip", "Y_cp"))) %>% 
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value))
```

    ## # A tibble: 3 x 3
    ##   key    mean    sd
    ##   <fct> <dbl> <dbl>
    ## 1 Y_np   5.31 0.163
    ## 2 Y_ip   5.83 0.162
    ## 3 Y_cp   5.75 0.161

What Hayes called the "relative total effects" *c*<sub>1</sub> and *c*<sub>2</sub> are the `D1` and `D2` lines in our `fixef()` output, above.

Here's the mediation model.

``` r
m_model <- bf(respappr ~ 1 + D1 + D2)
y_model <- bf(liking   ~ 1 + D1 + D2 + respappr)

model2 <-
  brm(data = protest, family = gaussian,
      m_model + y_model + set_rescor(FALSE),
      chains = 4, cores = 4)
```

``` r
print(model2)
```

    ##  Family: MV(gaussian, gaussian) 
    ##   Links: mu = identity; sigma = identity
    ##          mu = identity; sigma = identity 
    ## Formula: respappr ~ 1 + D1 + D2 
    ##          liking ~ 1 + D1 + D2 + respappr 
    ##    Data: protest (Number of observations: 129) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##                    Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## respappr_Intercept     3.88      0.18     3.51     4.25       4000 1.00
    ## liking_Intercept       3.71      0.31     3.10     4.32       4000 1.00
    ## respappr_D1            1.26      0.26     0.75     1.80       4000 1.00
    ## respappr_D2            1.61      0.26     1.10     2.10       4000 1.00
    ## liking_D1             -0.00      0.22    -0.44     0.42       4000 1.00
    ## liking_D2             -0.22      0.23    -0.67     0.23       4000 1.00
    ## liking_respappr        0.41      0.07     0.27     0.55       4000 1.00
    ## 
    ## Family Specific Parameters: 
    ##                Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## sigma_respappr     1.18      0.08     1.04     1.34       4000 1.00
    ## sigma_liking       0.93      0.06     0.82     1.05       4000 1.00
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

The Bayesian *R*<sup>2</sup> posteriors:

``` r
bayes_R2(model2, summary = F) %>% 
  as_tibble() %>% 
  gather() %>% 
  
  ggplot(aes(x = value, fill = key)) +
  geom_density(size = 0, alpha = 2/3) +
  annotate("text", x = .18, y = 6.75, label = "liking", color = pokepal(pokemon = "plusle")[2], family = "Times") +
  annotate("text", x = .355, y = 6.75, label = "respappr", color = pokepal(pokemon = "plusle")[6], family = "Times") +
  scale_y_continuous(NULL, breaks = NULL) +
  scale_fill_manual(values = pokepal(pokemon = "plusle")[c(2, 6)]) +
  coord_cartesian(xlim = 0:1) +
  labs(title = expression(paste("The ", italic(R)^{2}, " densities overlap near perfectly, both hovering around .25.")),
       x = NULL) +
  theme_tufte() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = pokepal(pokemon = "plusle")[8]))
```

![](Chapter_06_files/figure-markdown_github/unnamed-chunk-12-1.png)

To get the model summaries as presented in the second two columns in Table 6.2, we use `posterior_samples()`, rename a bit, and `summarize()` as usual.

``` r
post <-
  posterior_samples(model2) %>% 
  mutate(a1 = b_respappr_D1,
         a2 = b_respappr_D2,
         b = b_liking_respappr,
         c1_prime = b_liking_D1,
         c2_prime = b_liking_D2,
         i_m = b_respappr_Intercept,
         i_y = b_liking_Intercept)

post %>% 
  select(a1:i_y) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 7 x 5
    ##   key        mean    sd     ll    ul
    ##   <chr>     <dbl> <dbl>  <dbl> <dbl>
    ## 1 a1        1.26  0.262  0.75  1.80 
    ## 2 a2        1.61  0.257  1.10  2.10 
    ## 3 b         0.412 0.071  0.273 0.549
    ## 4 c1_prime -0.002 0.218 -0.438 0.419
    ## 5 c2_prime -0.22  0.229 -0.667 0.234
    ## 6 i_m       3.88  0.185  3.51  4.25 
    ## 7 i_y       3.71  0.312  3.10  4.32

Working with the *M*-bar formulas in page 199 is quite similar to what we did above.

``` r
post %>% 
  transmute(M_np = b_respappr_Intercept + b_respappr_D1*0 + b_respappr_D2*0,
            M_ip = b_respappr_Intercept + b_respappr_D1*1 + b_respappr_D2*0,
            M_cp = b_respappr_Intercept + b_respappr_D1*0 + b_respappr_D2*1) %>% 
  gather() %>%
  # this line will order our output the same way Hayes did in the text (p. 199)
  mutate(key = factor(key, levels = c("M_np", "M_ip", "M_cp"))) %>% 
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value))
```

    ## # A tibble: 3 x 3
    ##   key    mean    sd
    ##   <fct> <dbl> <dbl>
    ## 1 M_np   3.88 0.185
    ## 2 M_ip   5.15 0.178
    ## 3 M_cp   5.49 0.174

The *Y*-bar formulas are more of the same.

``` r
post <-
  post %>% 
  mutate(Y_np = b_liking_Intercept + b_liking_D1*0 + b_liking_D2*0 + b_liking_respappr*mean(protest$respappr),
         Y_ip = b_liking_Intercept + b_liking_D1*1 + b_liking_D2*0 + b_liking_respappr*mean(protest$respappr),
         Y_cp = b_liking_Intercept + b_liking_D1*0 + b_liking_D2*1 + b_liking_respappr*mean(protest$respappr))

post %>% 
  select(starts_with("Y_")) %>% 
  gather() %>%
  mutate(key = factor(key, levels = c("Y_np", "Y_ip", "Y_cp"))) %>% 
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value))
```

    ## # A tibble: 3 x 3
    ##   key    mean    sd
    ##   <fct> <dbl> <dbl>
    ## 1 Y_np   5.72 0.157
    ## 2 Y_ip   5.71 0.143
    ## 3 Y_cp   5.50 0.147

Note, these are where the adjusted *Y* values came from in Table 6.1.

This is as fine a spot as any to introduce coefficient plots. Both brms and the [bayesplot package](https://github.com/stan-dev/bayesplot) offer convenience functions for coefficient plots. It's good to know how to make them by hand. Here's ours for those last three *Y*-values.

``` r
post %>% 
  select(starts_with("Y_")) %>% 
  gather() %>% 
  
  ggplot(aes(x = key, y = value, color = key)) +
  stat_summary(geom = "pointrange",
               fun.y = median,
               fun.ymin = function(x){quantile(x, probs = .025)},
               fun.ymax = function(x){quantile(x, probs = .975)},
               size = .75) +
  stat_summary(geom = "linerange",
               fun.ymin = function(x){quantile(x, probs = .25)},
               fun.ymax = function(x){quantile(x, probs = .75)},
               size = 1.5) +
  scale_color_manual(values = pokepal(pokemon = "plusle")[c(3, 7, 9)]) +
  coord_flip() +
  theme_tufte() +
  labs(x = NULL, y = NULL) +
  theme(axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = pokepal(pokemon = "plusle")[8]))
```

![](Chapter_06_files/figure-markdown_github/unnamed-chunk-16-1.png)

The points are the posterior medians, the thick inner lines the 50% intervals, and the thinner outer lines the 95% intervals. For kicks, we distinguished the three values by color.

If we want to examine *R*<sup>2</sup> change for dropping the dummy variables, we'll first fit a model that omits them.

``` r
model3 <-
  brm(data = protest, family = gaussian,
      liking ~ 1 + respappr,
      chains = 4, cores = 4)
```

Here are the competing *R*<sup>2</sup> distributions.

``` r
R2s <-
  bayes_R2(model2, resp = "liking", summary = F) %>%
  as_tibble() %>% 
  rename(R2 = R2_liking) %>% 
  bind_rows(
    bayes_R2(model3, summary = F) %>% 
      as_tibble()
  ) %>% 
  mutate(fit = rep(c("model2", "model3"), each = 4000))

R2s %>% 
  ggplot(aes(x = R2, fill = fit)) +
  geom_density(size = 0, alpha = 2/3) +
  scale_fill_manual(values = pokepal(pokemon = "plusle")[c(6, 7)]) +
  annotate("text", x = .15, y = 6.75, label = "model3", color = pokepal(pokemon = "plusle")[7], family = "Times") +
  annotate("text", x = .35, y = 6.75, label = "model2", color = pokepal(pokemon = "plusle")[6], family = "Times") +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = 0:1) +
  labs(title = expression(paste("The ", italic(R)^{2}, " densities for LIKING substantially overlap.")),
       x = NULL) +
  theme_tufte() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = pokepal(pokemon = "plusle")[8]))
```

![](Chapter_06_files/figure-markdown_github/unnamed-chunk-18-1.png)

If you want to compare then with a change score, do something like this.

``` r
R2s %>%
  mutate(iter = rep(1:4000, times = 2)) %>% 
  spread(key = fit, value = R2) %>% 
  mutate(difference = model2 - model3) %>% 
  
  ggplot(aes(x = difference)) +
  geom_density(size = 0, fill = pokepal(pokemon = "plusle")[4]) +
  geom_vline(xintercept = 0, color = pokepal(pokemon = "plusle")[8]) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(paste("The ", Delta, italic(R)^{2}, " distribution")),
       subtitle = "Doesn't appear we have a lot of change.",
       x = NULL) +
  theme_tufte() +
  theme(legend.title = element_blank(),
        plot.background = element_rect(fill = pokepal(pokemon = "plusle")[8]))
```

![](Chapter_06_files/figure-markdown_github/unnamed-chunk-19-1.png)

Here's how to compute the summaries for *a*<sub>1</sub>*b* and *a*<sub>2</sub>*b*, including the Bayesian HMC intervals.

``` r
post %>% 
  mutate(a1b = a1*b,
         a2b = a2*b) %>%
  select(a1b:a2b) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 2 x 5
    ##   key    mean    sd    ll    ul
    ##   <chr> <dbl> <dbl> <dbl> <dbl>
    ## 1 a1b   0.519 0.14  0.267 0.819
    ## 2 a2b   0.663 0.155 0.386 0.988

6.3 Using a different group coding system
-----------------------------------------

Here we'll make our alternative dummies, what we'll call `D_1` and `D_2`, with orthogonal contrast coding.

``` r
protest <-
  protest %>% 
  mutate(D_1 = ifelse(protest == 0, -2/3, 1/3),
         D_2 = ifelse(protest == 0, 0, 
                      ifelse(protest == 1, -1/2, 1/2)))
```

Here's the model.

``` r
m_model <- bf(respappr ~ 1 + D_1 + D_2)
y_model <- bf(liking   ~ 1 + D_1 + D_2 + respappr)

model4 <-
  brm(data = protest, family = gaussian,
      m_model + y_model + set_rescor(FALSE),
      chains = 4, cores = 4)
```

Here are our intercepts and regression coefficient summaries.

``` r
fixef(model4)
```

    ##                      Estimate  Est.Error       Q2.5     Q97.5
    ## respappr_Intercept  4.8411006 0.10531269  4.6322267 5.0433757
    ## liking_Intercept    3.6422991 0.35360058  2.9417111 4.3312647
    ## respappr_D_1        1.4365959 0.21469495  1.0068888 1.8541240
    ## respappr_D_2        0.3501275 0.25182620 -0.1536161 0.8427628
    ## liking_D_1         -0.1122553 0.20840927 -0.5142453 0.3040818
    ## liking_D_2         -0.2159270 0.20069743 -0.6144085 0.1746028
    ## liking_respappr     0.4101460 0.07121553  0.2725024 0.5493263

It's important to note that these will not correspond to the "TOTAL EFFECT MODEL" section of the PROCESS output of Figure 6.3. Hayes's PROCESS has the `mcx=3` command which tells the program to reparametrize the orthogonal contrasts. brms doesn't have such a command.

For now, we'll have to jump to equation 6.8 towards the bottom of page 207. Those parameters are evident in our output.

``` r
fixef(model4)[c(1, 3:4) , ] %>% round(digits = 3)
```

    ##                    Estimate Est.Error   Q2.5 Q97.5
    ## respappr_Intercept    4.841     0.105  4.632 5.043
    ## respappr_D_1          1.437     0.215  1.007 1.854
    ## respappr_D_2          0.350     0.252 -0.154 0.843

Thus it's easy to get the *M*-bar means with a little posterior manipulation.

``` r
post <- posterior_samples(model4)

post <-
  post %>% 
  mutate(M_np = b_respappr_Intercept + b_respappr_D_1*-2/3 + b_respappr_D_2*0,
         M_ip = b_respappr_Intercept + b_respappr_D_1*1/3 + b_respappr_D_2*-1/2,
         M_cp = b_respappr_Intercept + b_respappr_D_1*1/3 + b_respappr_D_2*1/2)

post %>% 
  select(starts_with("M_")) %>% 
  gather() %>%
  mutate(key = factor(key, levels = c("M_np", "M_ip", "M_cp"))) %>% 
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value))
```

    ## # A tibble: 3 x 3
    ##   key    mean    sd
    ##   <fct> <dbl> <dbl>
    ## 1 M_np   3.88 0.182
    ## 2 M_ip   5.14 0.178
    ## 3 M_cp   5.50 0.176

With these in hand, we can compute *a*<sub>1</sub> and *a*<sub>2</sub>.

``` r
post <-
  post %>% 
  mutate(a1 = (M_ip + M_cp)/2 - M_np,
         a2 = M_cp - M_ip)

post %>% 
  select(a1:a2) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value))
```

    ## # A tibble: 2 x 3
    ##   key    mean    sd
    ##   <chr> <dbl> <dbl>
    ## 1 a1    1.44  0.215
    ## 2 a2    0.350 0.252

Happily, our model output will allow us to work with Hayes's *Y*-bar equations in the middle of page 210.

``` r
post <-
  post %>% 
  mutate(Y_np = b_liking_Intercept + b_liking_D_1*-2/3 + b_liking_D_2*0 + b_liking_respappr*mean(protest$respappr),
         Y_ip = b_liking_Intercept + b_liking_D_1*1/3 + b_liking_D_2*-1/2 + b_liking_respappr*mean(protest$respappr),
         Y_cp = b_liking_Intercept + b_liking_D_1*1/3 + b_liking_D_2*1/2 + b_liking_respappr*mean(protest$respappr))

post %>% 
  select(starts_with("Y_")) %>% 
  gather() %>%
  mutate(key = factor(key, levels = c("Y_np", "Y_ip", "Y_cp"))) %>% 
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value))
```

    ## # A tibble: 3 x 3
    ##   key    mean    sd
    ##   <fct> <dbl> <dbl>
    ## 1 Y_np   5.71 0.165
    ## 2 Y_ip   5.71 0.145
    ## 3 Y_cp   5.49 0.145

And with these in hand, we can compute *c*'<sub>1</sub> and *c*'<sub>2</sub>.

``` r
post <-
  post %>% 
  mutate(c1_prime = (Y_ip + Y_cp)/2 - Y_np,
         c2_prime = Y_cp - Y_ip)

post %>% 
  select(c1_prime:c2_prime) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value))
```

    ## # A tibble: 2 x 3
    ##   key        mean    sd
    ##   <chr>     <dbl> <dbl>
    ## 1 c1_prime -0.112 0.208
    ## 2 c2_prime -0.216 0.201

It appears Hayes has a typo in the formula for *c*'<sub>2</sub> on page 211. The value he has down for *Y*\_bar<sub>ip</sub>, 5.145, is incorrect. It's not the one he displayed at the bottom of the previous page and it also contradicts the analyses herein. So it goes... These things happen.

We haven't spelled out, but the *b* parameter is currently labeled `b_liking_respappr` in our `post` object. Here we'll make a `b` column to make things easier. While we're at it, we'll compute the indirect effects, too.

``` r
post <-
  post %>%
  mutate(b = b_liking_respappr) %>% 
  mutate(a1b = a1*b,
         a2b = a2*b)

post %>% 
  select(a1b:a2b) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 2 x 5
    ##   key    mean    sd     ll    ul
    ##   <chr> <dbl> <dbl>  <dbl> <dbl>
    ## 1 a1b   0.589 0.136  0.351 0.873
    ## 2 a2b   0.144 0.109 -0.058 0.376

Now we can compute and `summarize()` our *c*<sub>1</sub> and *c*<sub>2</sub>.

``` r
post <-
  post %>% 
  mutate(c1 = c1_prime + a1b,
         c2 = c2_prime + a2b)

post %>% 
  select(c1:c2) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value))
```

    ## # A tibble: 2 x 3
    ##   key      mean    sd
    ##   <chr>   <dbl> <dbl>
    ## 1 c1     0.477  0.200
    ## 2 c2    -0.0717 0.223

Note. The analyses in this document were done with:

-   R 3.4.4
-   RStudio 1.1.442
-   rmarkdown 1.9
-   readr 1.1.1
-   tidyverse 1.2.1
-   rstan 2.17.3
-   brms 2.3.2
-   ggthemes 3.5.0
-   palettetown 0.1.1

Reference
---------

Hayes, A. F. (2018). *Introduction to mediation, moderation, and conditional process analysis: A regression-based approach.* (2nd ed.). New York, NY, US: The Guilford Press.
