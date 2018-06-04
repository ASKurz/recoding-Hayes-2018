Chapter 06
================
A Solomon Kurz
2018-06-03

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
    ## # Groups: protest [3]
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

fit0 <-
  brm(data = protest, family = gaussian,
      liking ~ 1 + D1 + D2,
      chains = 4, cores = 4)
```

``` r
fixef(fit0)
```

    ##            Estimate Est.Error        Q2.5     Q97.5
    ## Intercept 5.3133508 0.1626005 4.994257106 5.6355566
    ## D1        0.5127446 0.2285454 0.069990909 0.9700469
    ## D2        0.4366860 0.2261788 0.001375586 0.8820305

Our *R*<sup>2</sup> differes a bit from the OLS version in the text. This shouldn't be surprising when it's near the boundary.

``` r
bayes_R2(fit0)
```

    ##      Estimate  Est.Error        Q2.5     Q97.5
    ## R2 0.05752817 0.03551413 0.005679464 0.1407828

Here's it's shape. For the plots in this chapter, we'll take a few formatting cues from [Edward Tufte](https://www.edwardtufte.com/tufte/books_vdqi), curtesy of the [ggthemes package](https://cran.r-project.org/web/packages/ggthemes/index.html). The `theme_tufte()` function will change the default font and remove some chart junk. We will take our color palette from [Pokemon](http://pokemonbyreview.blogspot.com/2017/02/311-312-plusle-minun.html) via the [palettetown package](https://cran.r-project.org/web/packages/palettetown/index.html).

``` r
library(ggthemes)
library(palettetown)

# pokepal(pokemon = "plusle")[1]

bayes_R2(fit0, summary = F) %>% 
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
post <- posterior_samples(fit0)

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
    ##   key     mean    sd
    ##   <fctr> <dbl> <dbl>
    ## 1 Y_np    5.31 0.163
    ## 2 Y_ip    5.83 0.161
    ## 3 Y_cp    5.75 0.158

What Hayes called the "relative total effects" *c*<sub>1</sub> and *c*<sub>2</sub> are the 'D1' and 'D2' lines in our `fixef()` output, above.

Here's the mediation model.

``` r
m_model <- bf(respappr ~ 1 + D1 + D2)
y_model <- bf(liking   ~ 1 + D1 + D2 + respappr)

fit1 <-
  brm(data = protest, family = gaussian,
      m_model + y_model + set_rescor(FALSE),
      chains = 4, cores = 4)
```

``` r
print(fit1)
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
    ## respappr_Intercept     3.88      0.19     3.52     4.25       4000 1.00
    ## liking_Intercept       3.70      0.32     3.07     4.31       4000 1.00
    ## respappr_D1            1.26      0.27     0.74     1.79       4000 1.00
    ## respappr_D2            1.61      0.26     1.10     2.11       4000 1.00
    ## liking_D1             -0.01      0.23    -0.44     0.44       4000 1.00
    ## liking_D2             -0.22      0.23    -0.68     0.22       3572 1.00
    ## liking_respappr        0.41      0.07     0.28     0.56       4000 1.00
    ## 
    ## Family Specific Parameters: 
    ##                Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## sigma_respappr     1.18      0.07     1.04     1.33       4000 1.00
    ## sigma_liking       0.93      0.06     0.82     1.05       4000 1.00
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

The Bayesian *R*<sup>2</sup> posteriors:

``` r
bayes_R2(fit1, summary = F) %>% 
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
  posterior_samples(fit1) %>% 
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
    ##   key         mean     sd     ll    ul
    ##   <chr>      <dbl>  <dbl>  <dbl> <dbl>
    ## 1 a1        1.26   0.265   0.741 1.79 
    ## 2 a2        1.61   0.257   1.10  2.11 
    ## 3 b         0.414  0.0730  0.276 0.561
    ## 4 c1_prime -0.0100 0.226  -0.442 0.444
    ## 5 c2_prime -0.224  0.234  -0.684 0.223
    ## 6 i_m       3.88   0.187   3.52  4.25 
    ## 7 i_y       3.70   0.318   3.07  4.31

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
    ##   key     mean    sd
    ##   <fctr> <dbl> <dbl>
    ## 1 M_np    3.88 0.187
    ## 2 M_ip    5.15 0.182
    ## 3 M_cp    5.49 0.174

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
    ##   key     mean    sd
    ##   <fctr> <dbl> <dbl>
    ## 1 Y_np    5.72 0.163
    ## 2 Y_ip    5.71 0.144
    ## 3 Y_cp    5.49 0.147

Note, these are where the adjusted *Y* values came from in Table 6.1.

This is as fine a spot as any to introduce coefficient plots. Both brms and the [bayesplot package](https://github.com/stan-dev/bayesplot) offer convenience funtions for coefficient plots. It's good to know how to make them by hand. Here's ours for those last three *Y*-values.

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
fit2 <-
  brm(data = protest, family = gaussian,
      liking ~ 1 + respappr,
      chains = 4, cores = 4)
```

Here are the competing *R*<sup>2</sup> distributions.

``` r
R2s <-
  bayes_R2(fit1, resp = "liking", summary = F) %>%
  as_tibble() %>% 
  rename(R2 = R2_liking) %>% 
  bind_rows(
    bayes_R2(fit2, summary = F) %>% 
      as_tibble()
  ) %>% 
  mutate(fit = rep(c("fit1", "fit2"), each = 4000))

R2s %>% 
  ggplot(aes(x = R2, fill = fit)) +
  geom_density(size = 0, alpha = 2/3) +
  scale_fill_manual(values = pokepal(pokemon = "plusle")[c(6, 7)]) +
  annotate("text", x = .19, y = 6.75, label = "fit2", color = pokepal(pokemon = "plusle")[7], family = "Times") +
  annotate("text", x = .31, y = 6.75, label = "fit1", color = pokepal(pokemon = "plusle")[6], family = "Times") +
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
  mutate(difference = fit1 - fit2) %>% 
  
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
    ## 1 a1b   0.522 0.142 0.269 0.839
    ## 2 a2b   0.666 0.159 0.381 1.01

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

fit3 <-
  brm(data = protest, family = gaussian,
      m_model + y_model + set_rescor(FALSE),
      chains = 4, cores = 4)
```

Here are our intercepts and regression coefficient summaries.

``` r
fixef(fit3)
```

    ##                      Estimate  Est.Error       Q2.5     Q97.5
    ## respappr_Intercept  4.8412168 0.10388981  4.6383129 5.0494489
    ## liking_Intercept    3.6349424 0.34558342  2.9555718 4.2996627
    ## respappr_D_1        1.4336107 0.22300580  1.0004290 1.8693855
    ## respappr_D_2        0.3461242 0.25048684 -0.1463369 0.8373058
    ## liking_D_1         -0.1114302 0.19970882 -0.5013271 0.2822665
    ## liking_D_2         -0.2155757 0.19696670 -0.6057279 0.1579487
    ## liking_respappr     0.4120523 0.06903237  0.2802028 0.5476141

It's important to note that these will not correspond to the "TOTAL EFFECT MODEL" section of the PROCESS output of Figure 6.3. Hayes's PROCESS has the `mcx=3` command which tells the program to reparameterize the orthogonal contrasts. brms doesn't have such a command.

For now, we'll have to jump to equation 6.8 towards the bottom of page 207. Those parameters are evident in our output.

``` r
fixef(fit3)[c(1, 3:4) , ] %>% round(digits = 3)
```

    ##                    Estimate Est.Error   Q2.5 Q97.5
    ## respappr_Intercept    4.841     0.104  4.638 5.049
    ## respappr_D_1          1.434     0.223  1.000 1.869
    ## respappr_D_2          0.346     0.250 -0.146 0.837

Thus it's easy to get the *M*-bar means with a little posterior manipulation.

``` r
post <- posterior_samples(fit3)

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
    ##   key     mean    sd
    ##   <fctr> <dbl> <dbl>
    ## 1 M_np    3.89 0.185
    ## 2 M_ip    5.15 0.179
    ## 3 M_cp    5.49 0.175

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
    ## 1 a1    1.43  0.223
    ## 2 a2    0.346 0.250

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
    ##   key     mean    sd
    ##   <fctr> <dbl> <dbl>
    ## 1 Y_np    5.71 0.161
    ## 2 Y_ip    5.71 0.143
    ## 3 Y_cp    5.50 0.146

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
    ## 1 c1_prime -0.111 0.200
    ## 2 c2_prime -0.216 0.197

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
    ##   key    mean    sd      ll    ul
    ##   <chr> <dbl> <dbl>   <dbl> <dbl>
    ## 1 a1b   0.591 0.137  0.347  0.880
    ## 2 a2b   0.142 0.106 -0.0590 0.365

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
    ## 1 c1     0.480  0.198
    ## 2 c2    -0.0739 0.224

Note. The analyses in this document were done with:

-   R 3.4.4
-   RStudio 1.1.442
-   rmarkdown 1.9
-   readr 1.1.1
-   tidyverse 1.2.1
-   rstan 2.17.3
-   brms 2.3.1
-   ggthemes 3.5.0
-   palettetown 0.1.1

Reference
---------

Hayes, A. F. (2018). *Introduction to mediation, moderation, and conditional process analysis: A regression-based approach.* (2nd ed.). New York, NY, US: The Guilford Press.
