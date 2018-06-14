Chapter 08
================
A Solomon Kurz
2018-06-14

8.1 Moderation with a dichotomous moderator
-------------------------------------------

Here we load a couple necessary packages, load the data, and take a `glimpse()`.

``` r
library(readr)
library(tidyverse)

disaster <- read_csv("data/disaster/disaster.csv")

glimpse(disaster)
```

    ## Observations: 211
    ## Variables: 5
    ## $ id      <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25...
    ## $ frame   <int> 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1,...
    ## $ donate  <dbl> 5.6, 4.2, 4.2, 4.6, 3.0, 5.0, 4.8, 6.0, 4.2, 4.4, 5.8, 6.2, 6.0, 4.2, 4.4, 5.8, 5.4, 3.4,...
    ## $ justify <dbl> 2.95, 2.85, 3.00, 3.30, 5.00, 3.20, 2.90, 1.40, 3.25, 3.55, 1.55, 1.60, 1.65, 2.65, 3.15,...
    ## $ skeptic <dbl> 1.8, 5.2, 3.2, 1.0, 7.6, 4.2, 4.2, 1.2, 1.8, 8.8, 1.0, 5.4, 2.2, 3.6, 7.8, 1.6, 1.0, 6.4,...

Our first moderation model is:

``` r
library(brms)

fit0 <-
  brm(data = disaster, family = gaussian,
      justify ~ 1 + skeptic + frame + frame:skeptic,
      chains = 4, cores = 4)
```

``` r
print(fit0)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: justify ~ 1 + skeptic + frame + frame:skeptic 
    ##    Data: disaster (Number of observations: 211) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## Intercept         2.45      0.15     2.15     2.75       1988 1.00
    ## skeptic           0.11      0.04     0.03     0.18       2147 1.00
    ## frame            -0.56      0.22    -0.98    -0.14       1861 1.00
    ## skeptic:frame     0.20      0.05     0.09     0.31       1772 1.00
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## sigma     0.82      0.04     0.74     0.90       2805 1.00
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

We'll compute our Bayeisan *R*<sup>2</sup> in the typical way.

``` r
bayes_R2(fit0) %>% round(digits = 3)
```

    ##    Estimate Est.Error  Q2.5 Q97.5
    ## R2    0.248     0.045 0.158 0.331

For the plots in this chapter, we'll take our color palette from the [ochRe package](https://github.com/ropenscilabs/ochRe), which provides Australia-inspired colors. We'll also use a few theme settings from good-old [ggthemes](https://cran.r-project.org/web/packages/ggthemes/vignettes/ggthemes.html). As in the last chapter, we'll save our adjusted theme settings as an object, `theme_08`.

``` r
library(ggthemes)
library(ochRe)

theme_08 <-
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = ochre_palettes[["olsen_seq"]][8],
                                       color = "transparent"))
```

Happily, the ochRe package has a handy convenience function, `viz_palette()`, that makes it easy to get a glimpse of the colors available in a given palette. We'll be using "olsen\_qual" and "olsen\_seq".

``` r
viz_palette(ochre_palettes[["olsen_qual"]])
```

![](Chapter_08_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
viz_palette(ochre_palettes[["olsen_seq"]])
```

![](Chapter_08_files/figure-markdown_github/unnamed-chunk-6-2.png)

Here's our Figure 8.3.

``` r
# these will come in handy with `geom_text()`, below
green_slope <- (fixef(fit0)["skeptic", 1] + fixef(fit0)[4, 1]) %>% round(digits = 3)
blue_slope  <- fixef(fit0)["skeptic", 1] %>% round(digits = 3)

(
  nd <-
  tibble(frame = rep(0:1, times = 2),
         skeptic = rep(c(0, 7), each = 2))
  )
```

    ## # A tibble: 4 x 2
    ##   frame skeptic
    ##   <int>   <dbl>
    ## 1     0    0   
    ## 2     1    0   
    ## 3     0    7.00
    ## 4     1    7.00

``` r
fitted(fit0, newdata = nd, 
       summary = F) %>% 
  as_tibble() %>% 
  gather() %>% 
  mutate(iter = rep(1:4000, times = 4),
         frame = rep(rep(0:1, each = 4000),
                     times = 2),
         skeptic = rep(c(0, 7), each = 4000*2)) %>% 
  
  ggplot(aes(x = skeptic, y = value, 
             group = interaction(frame, iter),
             color = frame %>% as.character())) +
  geom_line(aes(color = frame %>% as.character()),
            size = 1/6, alpha = 1/25) +
  geom_text(data = tibble(skeptic = c(4, 4.6),
                          value   = c(3.5, 2.6),
                          frame   = 1:0,
                          iter    = 0,
                          label   = c(paste("the slope when frame = 1 is about", green_slope),
                                      paste("the slope when frame = 0 is about", blue_slope)),
                          angle   = c(28, 6)),
            aes(label = label, angle = angle)) +
  scale_color_manual(NULL, values = ochre_palettes[["olsen_qual"]][(5:6)]) +
  scale_x_continuous(breaks = 1:6) +
  coord_cartesian(xlim = 1:6,
                  ylim = 2:4) +
  labs(title = "The moderation effect in a spaghetti plot",
       subtitle = "Using brms defaults, we have 4000 posterior draws. Much like we have 4000\ndraws for each model parameter, when we combine those draws across\nparameters, we end up with 4000 model equations. The consequence is we can\nalso express the line plot as 4000 semitransparent lines, one for each level of\nour moderator, frame.",
       x = expression(paste("Climate Change Skepticism (", italic("X"), ")")),
       y = "Strength of Justification for Withholding Aid") +
  theme_08 +
  theme(legend.position = "none")
```

![](Chapter_08_files/figure-markdown_github/unnamed-chunk-7-1.png)

In addition to our fancy Australia-inspired colors, we'll also play around a bit with spaghetti plots in this chapter. To my knowledge, this use of spaghetti plots is uniquely Bayesian. If you're trying to wrap your head around what on earth we just did, take a look at the first few rows from `posterior_samples()` object, `post`.

``` r
post <- posterior_samples(fit0)

head(post)
```

    ##   b_Intercept  b_skeptic    b_frame b_skeptic:frame     sigma      lp__
    ## 1    2.398227 0.09767384 -0.3946772       0.1809662 0.8202004 -260.6487
    ## 2    2.551179 0.08840025 -0.4507650       0.1646336 0.8151730 -261.0351
    ## 3    2.379841 0.12762162 -0.6066723       0.1869119 0.7206065 -263.7262
    ## 4    2.488578 0.11446067 -0.3760598       0.1440574 0.7494242 -262.6839
    ## 5    2.415064 0.11617831 -0.6350767       0.2184922 0.8386429 -260.4019
    ## 6    2.394783 0.11041747 -0.2624935       0.1329494 0.7987664 -261.3201

We've got six rows, each one corresponding to the credible parameter values from a given posterior draw. The `lp__` is uniquely Bayesian and beyond the scope of this project. You might think of `sigma` as the Bayesian analogue to what the OLS folks often refer to as error or the residual variance. Hayes doesn't tend to emphasize it in this text, but it's something you'll want to pay increasing attention to as you move along in your Bayesian career. All the columns starting with `b_` are the regression parameters, the model coefficients or the fixed effects. But anyways, notice that those `b_` columns correspond to the four parameter values in formula 8.2 on page 270. Here they are, but reformatted to more closely mimic the text:

1.  *Y*\_hat = 2.398 + 0.098*X* + -0.395*W* + 0.181*XW*
2.  *Y*\_hat = 2.551 + 0.088*X* + -0.451*W* + 0.165*XW*
3.  *Y*\_hat = 2.38 + 0.128*X* + -0.607*W* + 0.187*XW*
4.  *Y*\_hat = 2.489 + 0.114*X* + -0.376*W* + 0.144*XW*
5.  *Y*\_hat = 2.415 + 0.116*X* + -0.635*W* + 0.218*XW*
6.  *Y*\_hat = 2.395 + 0.11*X* + -0.262*W* + 0.133*XW*

Each row of `post`, each iteration or posterior draw, yields a full model equation that is a credible description of the data—or at least a credible as we can get within the limits of the model we have specified, our priors (which we typically cop out on and just use defaults in this project), and how well those fit when applied to the data at hand. So when we use brms convenience functions like `fitted()`, we pass specific predictor values through those 4000 unique model equations, which produces 4000 similar but distinct expected *Y*-values. So although a nice way to summarize those 4000 values is with summaries such as the posterior mean/median and 95% intervals, another way is to just plot an individual regression line for each of the iterations. That is what’s going on when we depict out models with a spaghetti plot.

The thing I like about spaghetti plots is that they give a three-dimensional sense of the posterior. Note that each individual line is very skinny and semitransparent. When you pile a whole bunch of them atop each other, the peaked or most credible regions of the posterior are the most saturated in color. Less credible posterior regions almost seamlessly merge into the background. Also, note how the combination of many similar but distinct straight lines results in a bowtie shape. Hopefully this clarifies where that shape’s been coming from in our simple summary plots.

But anyways, you could recode `frame` in a number of ways, including `ifelse()` or, in this case, by simple arithmetic. With `frame_ep` in hand, we’re ready to refit the model.

``` r
disaster <-
  disaster %>% 
  mutate(frame_ep = 1 - frame)

fit1 <-
  update(fit0, newdata = disaster,
         formula = justify ~ 1 + skeptic + frame_ep + frame_ep:skeptic,
         chains = 4, cores = 4)
```

``` r
print(fit1)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: justify ~ skeptic + frame_ep + skeptic:frame_ep 
    ##    Data: disaster (Number of observations: 211) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##                  Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## Intercept            1.88      0.16     1.56     2.19       1930 1.00
    ## skeptic              0.31      0.04     0.23     0.39       1975 1.00
    ## frame_ep             0.57      0.23     0.13     1.02       1790 1.00
    ## skeptic:frame_ep    -0.20      0.06    -0.32    -0.09       1743 1.00
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## sigma     0.82      0.04     0.74     0.90       2824 1.00
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Our results match nicely with the formula on page 275.

If you want to follow along with Hayes on pate 276 and isolate the 95% credible intervals for the `skeptic` parameter, you can use `posterior_interval()`.

``` r
posterior_interval(fit1)["b_skeptic", ] %>% round(digits = 3)
```

    ##  2.5% 97.5% 
    ## 0.229 0.390

8.2 Interaction between two quantitative variables
--------------------------------------------------

Here's the `glbwarm` data.

``` r
glbwarm <- read_csv("data/glbwarm/glbwarm.csv")

glimpse(glbwarm)
```

    ## Observations: 815
    ## Variables: 7
    ## $ govact   <dbl> 3.6, 5.0, 6.6, 1.0, 4.0, 7.0, 6.8, 5.6, 6.0, 2.6, 1.4, 5.6, 7.0, 3.8, 3.4, 4.2, 1.0, 2.6...
    ## $ posemot  <dbl> 3.67, 2.00, 2.33, 5.00, 2.33, 1.00, 2.33, 4.00, 5.00, 5.00, 1.00, 4.00, 1.00, 5.67, 3.00...
    ## $ negemot  <dbl> 4.67, 2.33, 3.67, 5.00, 1.67, 6.00, 4.00, 5.33, 6.00, 2.00, 1.00, 4.00, 5.00, 4.67, 2.00...
    ## $ ideology <int> 6, 2, 1, 1, 4, 3, 4, 5, 4, 7, 6, 4, 2, 4, 5, 2, 6, 4, 2, 4, 4, 2, 6, 4, 4, 3, 4, 5, 4, 5...
    ## $ age      <int> 61, 55, 85, 59, 22, 34, 47, 65, 50, 60, 71, 60, 71, 59, 32, 36, 69, 70, 41, 48, 38, 63, ...
    ## $ sex      <int> 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1...
    ## $ partyid  <int> 2, 1, 1, 1, 1, 2, 1, 1, 2, 3, 2, 1, 1, 1, 1, 1, 2, 3, 1, 3, 2, 1, 3, 2, 1, 1, 1, 3, 1, 1...

Although Hayes made a distinction between the *X*, *M*, and *C* variables in the text. That distinction is conceptual and doesn't impact the way we enter them into `brm()`. Rather, the `brm()` formula clarifies they're all just predictors.

``` r
fit2 <- 
  brm(data = glbwarm, family = gaussian,
      govact ~ 1 + negemot + age + negemot:age + posemot + ideology + sex,
      chains = 4, cores = 4)
```

Our results cohere nicely with the Hayes's formula in the middle of page 278 or in Table 8.2.

``` r
print(fit2, digits = 3)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: govact ~ 1 + negemot + age + negemot:age + posemot + ideology + sex 
    ##    Data: glbwarm (Number of observations: 815) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##             Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## Intercept      5.181     0.328    4.549    5.829       2416 1.001
    ## negemot        0.119     0.080   -0.037    0.275       2245 1.002
    ## age           -0.024     0.006   -0.035   -0.013       2246 1.001
    ## posemot       -0.022     0.027   -0.076    0.032       3841 1.001
    ## ideology      -0.212     0.027   -0.265   -0.160       3525 1.000
    ## sex           -0.011     0.077   -0.161    0.140       3504 1.000
    ## negemot:age    0.006     0.001    0.003    0.009       2217 1.001
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## sigma    1.059     0.027    1.008    1.112       3421 1.002
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Here's the *R*<sup>2</sup>.

``` r
bayes_R2(fit2) %>% round(digits = 3)
```

    ##    Estimate Est.Error  Q2.5 Q97.5
    ## R2    0.401     0.021 0.358 0.439

As the *R*<sup>2</sup> is a good bit away from the boundaries, it's nicely Gaussian.

``` r
bayes_R2(fit2, summary = F) %>% 
  as_tibble() %>% 

  ggplot(aes(x = R2)) +
  geom_density(size = 0, fill = ochre_palettes[["olsen_qual"]][(1)]) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = 0:1) +
  labs(title = expression(paste("Loot at how Gaussian our ", italic(R)^2, " is!")),
       subtitle = expression(paste("No need to put a ", italic(p), "-value on that sucker.")),
       x = NULL) +
  theme_08
```

![](Chapter_08_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
(
  nd <-
  tibble(negemot = rep(c(1, 6), each = 3),
         age = rep(c(30, 50, 70), times = 2),
         posemot = mean(glbwarm$posemot),
         ideology = mean(glbwarm$ideology),
         sex = mean(glbwarm$sex))
  )
```

    ## # A tibble: 6 x 5
    ##   negemot   age posemot ideology   sex
    ##     <dbl> <dbl>   <dbl>    <dbl> <dbl>
    ## 1    1.00  30.0    3.13     4.08 0.488
    ## 2    1.00  50.0    3.13     4.08 0.488
    ## 3    1.00  70.0    3.13     4.08 0.488
    ## 4    6.00  30.0    3.13     4.08 0.488
    ## 5    6.00  50.0    3.13     4.08 0.488
    ## 6    6.00  70.0    3.13     4.08 0.488

``` r
# these will come in handy with `geom_text()`, below
slope_30 <- (fixef(fit2)["negemot", 1] + fixef(fit2)["negemot:age", 1]*30) %>% round(digits = 3)
slope_50 <- (fixef(fit2)["negemot", 1] + fixef(fit2)["negemot:age", 1]*50) %>% round(digits = 3)
slope_70 <- (fixef(fit2)["negemot", 1] + fixef(fit2)["negemot:age", 1]*70) %>% round(digits = 3)

fitted(fit2, newdata = nd, 
       summary = F) %>% 
  as_tibble() %>% 
  gather() %>% 
  mutate(iter = rep(1:4000, times = 6),
         negemot = rep(rep(c(1, 6), each = 3),
                       each = 4000),
         age = rep(rep(c(30, 50, 70), times = 2),
                   each = 4000)) %>% 
  # here we'll reduce our spaghetti plot lines to 100 per age category
  filter(iter <= 100) %>% 
  
  ggplot(aes(x = negemot, y = value, 
             group = interaction(age, iter),
             color = age %>% as.character())) +
  geom_line(aes(color = age %>% as.character()),
            size = 3/4, alpha = 1/8) +
  geom_text(data = tibble(negemot = 6.1,
                          value   = c(5.4, 5.7, 6),
                          age     = c(30, 50, 70),
                          iter    = 0,
                          label   = c(paste("the slope for 30 year olds is about", slope_30),
                                      paste("the slope for 50 year olds is about", slope_50),
                                      paste("the slope for 70 year olds is about", slope_70))),
            aes(label = label),
            hjust = 0) +
  scale_color_manual(NULL, values = ochre_palettes[["olsen_qual"]][c(4, 1, 3)]) +
  scale_x_continuous(breaks = 1:6) +
  coord_cartesian(xlim = 1:9,
                  ylim = c(2.9, 6.1)) +
  labs(title = "The moderation effect of age on negemot",
       x = expression(paste("Negative Emotions about Climate Change (", italic("X"), ")")),
       y = "Support for Government Action") +
  theme_08 +
  theme(legend.position = "none")
```

![](Chapter_08_files/figure-markdown_github/unnamed-chunk-18-1.png)

We'll continue with our spaghetti plot approach for Figure 8.7. Again, when we did the JN technique plot for Chapter 7, we computed values for the posterior mean and the 95% intervals. Because the intervals follow a bowtie shape, we had to compute the y-values for many values across the x-axis in order to make the curve look smooth. But as long as we stick with the spaghetti plot approach, all we need are the values at the endpoints of each iteration. Although each line is straight, the combination of many lines is what produces the bowtie effect.

``` r
# here is our primary data object
post <-
  posterior_samples(fit2) %>% 
  transmute(at_15 = b_negemot + `b_negemot:age`*15,
            at_90 = b_negemot + `b_negemot:age`*90,
            iter = 1:n()) %>% 
  gather(key, value, -iter) %>% 
  rename(age = key) %>% 
  mutate(age = str_remove(age, "at_") %>% as.double())

# Here we compute the points for the posterior mean
post_means <-
  post %>% 
  group_by(age) %>% 
  summarize(value = mean(value)) %>% 
  mutate(iter = 0)

# the plot
post %>% 
  filter(iter < 501) %>% 

  ggplot(aes(x = age, y = value, group = iter)) +
  geom_line(color = ochre_palettes[["olsen_qual"]][1],
            alpha = 1/12) +
  geom_line(data = post_means,
            color = ochre_palettes[["olsen_qual"]][3],
            size = 1.1) +
  scale_y_continuous(breaks = seq(from = -.25, to = 1, by = .25)) +
  coord_cartesian(xlim = c(20, 85),
                  ylim = c(-.25, 1)) +
  labs(subtitle = "Each orange line is the consequence of one of 1000 posterior draws. All are credible\nregression lines for the data, but because they are semitransparent, the most\ncredible regions are the ones with the darkest color. The line corresponding to the\nposterior mean is in red-orange.",
       x = expression(paste("Age (", italic("W"), ")")),
       y = "Conditional Effects of Negative Emotions on\nSupport for Government Action") +
  theme_08
```

![](Chapter_08_files/figure-markdown_github/unnamed-chunk-20-1.png)

8.3 Hierarchical versus simultaneous entry
------------------------------------------

``` r
fit3 <-
  update(fit0, 
         formula = justify ~ 1 + skeptic + frame,
         chains = 4, cores = 4)
```

``` r
# the moderation model's R2
R2s <-
  bayes_R2(fit0, summary = F) %>% 
  as_tibble() %>% 
  rename(moderation_model = R2) %>% 
  # here we add the multivaraible model's R2
  bind_cols(
    bayes_R2(fit3, summary = F) %>% 
      as_tibble() %>% 
      rename(multivariable_model = R2)
  ) %>% 
  # we'll need a difference score
  mutate(difference = moderation_model - multivariable_model) %>% 
  # putting the data in the long format and grouping will make summarizing easier
  gather(R2, value)

R2s %>% 
  group_by(R2) %>% 
  summarize(median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 3 x 4
    ##   R2                  median      ll    ul
    ##   <chr>                <dbl>   <dbl> <dbl>
    ## 1 difference          0.0480 -0.0740 0.170
    ## 2 moderation_model    0.249   0.158  0.331
    ## 3 multivariable_model 0.200   0.116  0.285

Note that the Bayesian *R*<sup>2</sup> performed differently than the *F*-test in the text.

``` r
R2s %>% 
  filter(R2 == "difference") %>% 

  ggplot(aes(x = value)) +
  geom_density(aes(fill = model), size = 0, fill = ochre_palettes[["olsen_seq"]][14]) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(paste("The Bayesian ", Delta, italic(R)^2, " distribution")),
       subtitle = "Although most of the posterior mass is positive--suggesting the moderation model accounted for more variance than the simple\nmultivariable model--, a substantial portion of the postrior is within the negative parameter space. Sure, if we had to bet, the safer\nbet is on the moderation model. But that bet wouled be quite uncertain and we might well loose our shirts. Also, note the width of\nthe distribution; credible values range from -0.1 to nearly 0.2.",
       x = NULL) +
  coord_cartesian(xlim = c(-.4, .4)) +
  theme_08
```

![](Chapter_08_files/figure-markdown_github/unnamed-chunk-23-1.png)

We can also compare these with the LOO, which, as is typical of information criteria, corrects for model coplexity.

``` r
(l_fit0 <- loo(fit0))
```

    ## 
    ## Computed from 4000 by 211 log-likelihood matrix
    ## 
    ##          Estimate   SE
    ## elpd_loo   -259.1 10.8
    ## p_loo         5.5  0.9
    ## looic       518.2 21.6
    ## ------
    ## Monte Carlo SE of elpd_loo is 0.0.
    ## 
    ## All Pareto k estimates are good (k < 0.5).
    ## See help('pareto-k-diagnostic') for details.

``` r
(l_fit3 <- loo(fit3))
```

    ## 
    ## Computed from 4000 by 211 log-likelihood matrix
    ## 
    ##          Estimate   SE
    ## elpd_loo   -264.7 11.2
    ## p_loo         4.7  0.9
    ## looic       529.5 22.4
    ## ------
    ## Monte Carlo SE of elpd_loo is 0.0.
    ## 
    ## All Pareto k estimates are good (k < 0.5).
    ## See help('pareto-k-diagnostic') for details.

The LOO values aren't of interest in and of themselves. However, the bottom of the `loo()` output was useful because for both models we learned that "All Pareto k estimates are good (k &lt; 0.5).", which assures us that we didn't have a problem with overly-influential outlier values. But even though the LOO values weren't interesting themselves, their difference score is. We'll use `compare_ic()` to get that.

``` r
compare_ic(l_fit0, l_fit3)
```

    ##              LOOIC    SE
    ## fit0        518.22 21.63
    ## fit3        529.46 22.38
    ## fit0 - fit3 -11.24  8.17

As a reminder, we generally prefer models with lower information criteria, which in this case is clearly the moderation model (i.e., `fit0`). However, the standard error value for the difference is quite large, which suggests that the model with the lowest value isn't the clear winner. Happily, these results match nicely with the Bayesian *R*<sup>2</sup> difference score. The moderation model appears somewhat better than the multivariable model, but its superiority is hardly decisive.

8.4 The equivalence between moderated regression analysis and a 2 X 2 factorial analysis of variance
----------------------------------------------------------------------------------------------------

I'm just not going to encourage ANOVA *F*-testing methodology.

However, I will show the Bayesian regression model. First, here are the data.

``` r
caskets <- read_csv("data/caskets/caskets.csv")

glimpse(caskets)
```

    ## Observations: 541
    ## Variables: 7
    ## $ policy   <int> 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0...
    ## $ interest <dbl> 4.0, 2.0, 3.0, 1.0, 1.0, 2.0, 1.0, 2.5, 3.0, 1.0, 2.0, 3.5, 1.0, 1.0, 1.5, 3.0, 1.0, 2.0...
    ## $ age      <int> 39, 57, 63, 56, 50, 87, 33, 64, 82, 28, 18, 52, 42, 39, 64, 72, 54, 84, 55, 27, 42, 77, ...
    ## $ educ     <int> 3, 3, 2, 5, 3, 2, 7, 2, 3, 3, 1, 1, 5, 4, 3, 2, 3, 4, 7, 2, 3, 5, 4, 5, 5, 3, 3, 2, 2, 5...
    ## $ male     <int> 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0...
    ## $ conserv  <int> 4, 3, 6, 3, 3, 5, 6, 3, 6, 7, 4, 2, 7, 6, 5, 6, 6, 3, 7, 6, 5, 5, 3, 4, 6, 2, 7, 6, 2, 6...
    ## $ kerry    <int> 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0...

The model:

``` r
fit4 <-
  brm(data = caskets, family = gaussian,
      interest ~ 1 + policy + kerry + policy:kerry,
      chains = 4, cores = 4)
```

``` r
print(fit4)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: interest ~ 1 + policy + kerry + policy:kerry 
    ##    Data: caskets (Number of observations: 541) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## Intercept        1.78      0.09     1.61     1.96       2032 1.00
    ## policy          -0.38      0.13    -0.65    -0.13       1781 1.00
    ## kerry            0.60      0.13     0.35     0.85       1880 1.00
    ## policy:kerry     0.36      0.18    -0.00     0.72       1761 1.00
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## sigma     1.04      0.03     0.97     1.10       3136 1.00
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Those results don't look anything like what Hayes reported in Tables 8.3 or 8.4. However, we a little deft manipulation of the posterior samples can yield equivalent results to Hayes's Table 8.3.

``` r
post <- 
  posterior_samples(fit4) %>% 
  mutate(Y_bar_1 = b_Intercept + b_policy*0 + b_kerry*0 + `b_policy:kerry`*0*0,
         Y_bar_3 = b_Intercept + b_policy*0 + b_kerry*1 + `b_policy:kerry`*0*1,
         Y_bar_2 = b_Intercept + b_policy*1 + b_kerry*0 + `b_policy:kerry`*1*0,
         Y_bar_4 = b_Intercept + b_policy*1 + b_kerry*1 + `b_policy:kerry`*1*1,
         
         Y_bar_12 = b_Intercept + b_policy*.5 + b_kerry*0 + `b_policy:kerry`*.5*0,
         Y_bar_34 = b_Intercept + b_policy*.5 + b_kerry*1 + `b_policy:kerry`*.5*1,
         Y_bar_13 = b_Intercept + b_policy*0 + b_kerry*.5 + `b_policy:kerry`*0*.5,
         Y_bar_24 = b_Intercept + b_policy*1 + b_kerry*.5 + `b_policy:kerry`*1*.5)
```

Here are the cell-specific means in Table 8.3.

``` r
post %>% 
  select(Y_bar_1:Y_bar_4) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 4 x 4
    ##   key     median    ll    ul
    ##   <chr>    <dbl> <dbl> <dbl>
    ## 1 Y_bar_1   1.78  1.61  1.96
    ## 2 Y_bar_2   1.40  1.21  1.58
    ## 3 Y_bar_3   2.38  2.21  2.56
    ## 4 Y_bar_4   2.35  2.19  2.53

And here are the marginal means from Table 8.3.

``` r
post %>% 
  select(Y_bar_12:Y_bar_24) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 4 x 4
    ##   key      median    ll    ul
    ##   <chr>     <dbl> <dbl> <dbl>
    ## 1 Y_bar_12   1.59  1.46  1.72
    ## 2 Y_bar_13   2.08  1.96  2.20
    ## 3 Y_bar_24   1.88  1.75  2.00
    ## 4 Y_bar_34   2.37  2.25  2.49

For kicks and giggles, here are what the cell-specific means look like in box plots.

``` r
post %>% 
  select(Y_bar_1:Y_bar_4) %>% 
  gather() %>% 
  
  ggplot(aes(x = key, y = value, fill = key)) +
  geom_boxplot(size = 1/3) +
  scale_fill_manual(values = ochre_palettes[["olsen_qual"]][c(5, 6, 4, 3)]) +
  labs(title = "Cell-specific effects",
       x = NULL, 
       y = "interest") +
  theme_08 +
  theme(legend.position = "none")
```

![](Chapter_08_files/figure-markdown_github/unnamed-chunk-32-1.png)

And here are the same for the marginal means. This time we'll show the shapes of the posteriors with violin plots with horizontal lines depicting the median and interquartile ranges.

``` r
post %>% 
  select(Y_bar_12:Y_bar_24) %>% 
  gather() %>% 
  
  ggplot(aes(x = key, y = value, fill = key)) +
  geom_violin(draw_quantiles = c(.25, .5, .75),
              color = ochre_palettes[["olsen_seq"]][8]) +
  scale_fill_manual(values = ochre_palettes[["olsen_qual"]][c(5, 6, 4, 3)]) +
  labs(title = "Marginal means",
       x = NULL, 
       y = "interest") +
  theme_08 +
  theme(legend.position = "none")
```

![](Chapter_08_files/figure-markdown_github/unnamed-chunk-33-1.png)

On page 294, Hayes used point estimates to compute the *simple effect* of policy information among Kerry supporters and then the same thing among Bush supporters. Here's how we'd do that when working with the full vector of posterior iterations:

``` r
post %>% 
  transmute(simple_effect_Kerry = Y_bar_4 - Y_bar_3,
            simple_effect_Bush = Y_bar_2 - Y_bar_1) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 2 x 4
    ##   key                  median     ll     ul
    ##   <chr>                 <dbl>  <dbl>  <dbl>
    ## 1 simple_effect_Bush  -0.384  -0.648 -0.133
    ## 2 simple_effect_Kerry -0.0250 -0.268  0.219

So then computing the main effect for policy information using the simple effects is little more than an extension of those steps.

``` r
post %>% 
  transmute(main_effect = ((Y_bar_4 - Y_bar_3) + (Y_bar_2 - Y_bar_1))/2) %>% 
  summarize(median = median(main_effect),
            ll = quantile(main_effect, probs = .025),
            ul = quantile(main_effect, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ##   median     ll     ul
    ## 1 -0.206 -0.386 -0.036

And we get the same results by strategically subtracting the marginal means.

``` r
post %>% 
  transmute(main_effect = Y_bar_24 - Y_bar_13) %>% 
  summarize(median = median(main_effect),
            ll = quantile(main_effect, probs = .025),
            ul = quantile(main_effect, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ##   median     ll     ul
    ## 1 -0.206 -0.386 -0.036

So then the main effect of for candidate is similarly computed using either approach:

``` r
post %>% 
  transmute(main_effect = ((Y_bar_4 - Y_bar_2) + (Y_bar_3 - Y_bar_1))/2) %>% 
  summarize(median = median(main_effect),
            ll = quantile(main_effect, probs = .025),
            ul = quantile(main_effect, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ##   median    ll    ul
    ## 1  0.778 0.601 0.955

``` r
post %>% 
  transmute(main_effect = Y_bar_34 - Y_bar_12) %>% 
  summarize(median = median(main_effect),
            ll = quantile(main_effect, probs = .025),
            ul = quantile(main_effect, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ##   median    ll    ul
    ## 1  0.778 0.601 0.955

We don't have an *F*-test for our Bayesian moderation model. But we do have an interaction term. Here's it's distribution:

``` r
post %>% 
  ggplot(aes(x = `b_policy:kerry`)) +
  geom_density(size = 0,
               fill = ochre_palettes[["olsen_qual"]][2]) +
  geom_vline(xintercept = fixef(fit4)["policy:kerry", c(1, 3, 4)],
             color = ochre_palettes[["olsen_seq"]][8], linetype = c(1, 2, 2)) +
  scale_x_continuous(breaks = fixef(fit4)["policy:kerry", c(1, 3, 4)],
                     labels = fixef(fit4)["policy:kerry", c(1, 3, 4)] %>% round(digits = 2)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "The interaction term, `policy:kerry`",
       subtitle = "The solid vertical line is the posterior mean and the\ndashed lines to either end denote the percentile-\nbased 95% intervals.",
       x = NULL) +
  theme_08 +
  theme(legend.position = "none")
```

![](Chapter_08_files/figure-markdown_github/unnamed-chunk-38-1.png)

Following Hayes's work on the bottom of page 295, here's how you'd reproduce that by manipulating our *Y*-bar vectors.

``` r
post %>% 
  transmute(reproduced_interaction_term = (Y_bar_4 - Y_bar_3) - (Y_bar_2 - Y_bar_1)) %>% 
  summarize(median = median(reproduced_interaction_term),
            ll = quantile(reproduced_interaction_term, probs = .025),
            ul = quantile(reproduced_interaction_term, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 2)
```

    ##   median ll   ul
    ## 1   0.36  0 0.72

Extending that logic, we also get:

``` r
post %>% 
  transmute(reproduced_interaction_term = (Y_bar_4 - Y_bar_2) - (Y_bar_3 - Y_bar_1)) %>% 
  summarize(median = median(reproduced_interaction_term),
            ll = quantile(reproduced_interaction_term, probs = .025),
            ul = quantile(reproduced_interaction_term, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 2)
```

    ##   median ll   ul
    ## 1   0.36  0 0.72

### Simple effects parameterization.

``` r
caskets <- read_csv("data/caskets/caskets.csv")

glimpse(caskets)
```

    ## Observations: 541
    ## Variables: 7
    ## $ policy   <int> 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0...
    ## $ interest <dbl> 4.0, 2.0, 3.0, 1.0, 1.0, 2.0, 1.0, 2.5, 3.0, 1.0, 2.0, 3.5, 1.0, 1.0, 1.5, 3.0, 1.0, 2.0...
    ## $ age      <int> 39, 57, 63, 56, 50, 87, 33, 64, 82, 28, 18, 52, 42, 39, 64, 72, 54, 84, 55, 27, 42, 77, ...
    ## $ educ     <int> 3, 3, 2, 5, 3, 2, 7, 2, 3, 3, 1, 1, 5, 4, 3, 2, 3, 4, 7, 2, 3, 5, 4, 5, 5, 3, 3, 2, 2, 5...
    ## $ male     <int> 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0...
    ## $ conserv  <int> 4, 3, 6, 3, 3, 5, 6, 3, 6, 7, 4, 2, 7, 6, 5, 6, 6, 3, 7, 6, 5, 5, 3, 4, 6, 2, 7, 6, 2, 6...
    ## $ kerry    <int> 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0...

The model:

``` r
fit4 <-
  brm(data = caskets, family = gaussian,
      interest ~ 1 + policy + kerry + policy:kerry,
      chains = 4, cores = 4)
```

``` r
post %>% 
  transmute(b1 = b_policy,
            `Y_bar_2 - Y_bar_1` = Y_bar_2 - Y_bar_1) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 2 x 3
    ##   key                 mean    sd
    ##   <chr>              <dbl> <dbl>
    ## 1 b1                -0.385 0.132
    ## 2 Y_bar_2 - Y_bar_1 -0.385 0.132

``` r
post %>% 
  transmute(b2 = b_kerry,
            `Y_bar_3 - Y_bar_1` = Y_bar_3 - Y_bar_1) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 2 x 3
    ##   key                mean    sd
    ##   <chr>             <dbl> <dbl>
    ## 1 b2                0.600 0.130
    ## 2 Y_bar_3 - Y_bar_1 0.600 0.130

Here we compute *b*\[3\] with the difference between the simple effects of *X* at levels of *W*.

``` r
post %>% 
  transmute(b3 = `b_policy:kerry`,
            `(Y_bar_4 - Y_bar_3) - (Y_bar_2 - Y_bar_1)` = (Y_bar_4 - Y_bar_3) - (Y_bar_2 - Y_bar_1)) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 2 x 3
    ##   key                                        mean    sd
    ##   <chr>                                     <dbl> <dbl>
    ## 1 (Y_bar_4 - Y_bar_3) - (Y_bar_2 - Y_bar_1) 0.358 0.184
    ## 2 b3                                        0.358 0.184

And now *b*\[3\] with the difference between the simple effects of *W* at levels of *X*.

``` r
post %>% 
  transmute(b3 = `b_policy:kerry`,
            `(Y_bar_4 - Y_bar_2) - (Y_bar_3 - Y_bar_1)` = (Y_bar_4 - Y_bar_2) - (Y_bar_3 - Y_bar_1)) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 2 x 3
    ##   key                                        mean    sd
    ##   <chr>                                     <dbl> <dbl>
    ## 1 (Y_bar_4 - Y_bar_2) - (Y_bar_3 - Y_bar_1) 0.358 0.184
    ## 2 b3                                        0.358 0.184

Here's our analogue to the "Model 1" portion of Table 8.5.

``` r
fixef(fit4) %>% round(digits = 3)
```

    ##              Estimate Est.Error   Q2.5  Q97.5
    ## Intercept       1.784     0.087  1.618  1.954
    ## policy         -0.387     0.122 -0.630 -0.153
    ## kerry           0.602     0.122  0.363  0.837
    ## policy:kerry    0.360     0.171  0.026  0.697

``` r
bayes_R2(fit4) %>% round(digits = 3)
```

    ##    Estimate Est.Error  Q2.5 Q97.5
    ## R2    0.141     0.025 0.093 0.194

### Main effects parameterization.

A nice feature of brms is you can do data transformations right within the `brm()` or `update()` functions. Here we'll make our two new main-effects-coded variables, `policy_me` and `kerry_me`, with the `mutate()` function right within `update()`.

``` r
fit5 <-
  update(fit4,
         newdata = caskets %>%
           mutate(policy_me = policy - .5,
                  kerry_me = kerry - .5), 
         family = gaussian,
         interest ~ 1 + policy_me + kerry_me + policy_me:kerry_me,
         chains = 4, cores = 4)
```

Here's our analogue to the "Model 2" portion of Table 8.5.

``` r
fixef(fit5) %>% round(digits = 3)
```

    ##                    Estimate Est.Error   Q2.5  Q97.5
    ## Intercept             1.980     0.044  1.893  2.066
    ## policy_me            -0.208     0.089 -0.379 -0.036
    ## kerry_me              0.782     0.091  0.601  0.966
    ## policy_me:kerry_me    0.365     0.179  0.017  0.719

``` r
bayes_R2(fit5) %>% round(digits = 3)
```

    ##    Estimate Est.Error  Q2.5 Q97.5
    ## R2    0.141     0.026 0.091 0.194

Like with `fit4`, above, we'll need a bit of algebra to compute our *Y*\_bar\[*i*\] vectors.

``` r
post <- 
  posterior_samples(fit5) %>% 
  mutate(Y_bar_1 = b_Intercept + b_policy_me*-.5 + b_kerry_me*-.5 + `b_policy_me:kerry_me`*-.5*-.5,
         Y_bar_3 = b_Intercept + b_policy_me*-.5 + b_kerry_me*.5 + `b_policy_me:kerry_me`*-.5*.5,
         Y_bar_2 = b_Intercept + b_policy_me*.5 + b_kerry_me*-.5 + `b_policy_me:kerry_me`*.5*-.5,
         Y_bar_4 = b_Intercept + b_policy_me*.5 + b_kerry_me*.5 + `b_policy_me:kerry_me`*.5*.5)
```

With our `post` for `fit5` in hand, we'll follow the formulas at the top of page 298 to compute our *b*\[1\] and *b*\[2\] distributions.

``` r
post %>% 
  transmute(b1 = ((Y_bar_4 - Y_bar_3) + (Y_bar_2 - Y_bar_1))/2,
            b2 = ((Y_bar_4 - Y_bar_2) + (Y_bar_3 - Y_bar_1))/2) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 2 x 3
    ##   key     mean     sd
    ##   <chr>  <dbl>  <dbl>
    ## 1 b1    -0.208 0.0890
    ## 2 b2     0.782 0.0910

Hayes pointed out that the interaction effect, *b*\[3\], is the same across models his OLS Models 1 and 2. This is largely true for our Bayesian HMC `fit4` adn `fit5` models:

``` r
fixef(fit4)[4, ] %>% round(digits = 3)
```

    ##  Estimate Est.Error      Q2.5     Q97.5 
    ##     0.360     0.171     0.026     0.697

``` r
fixef(fit5)[4, ] %>% round(digits = 3)
```

    ##  Estimate Est.Error      Q2.5     Q97.5 
    ##     0.365     0.179     0.017     0.719

However, the results aren’t exactly the same because of simulation error. If you were working on a project requiring high precision, increase the number of posterior iterations. To demonstrate, here we'll increase each chain's post-warmup iteration count by an order of magnitude, resulting in 80,000 post-warmup iterations rather than the defuault 4,000.

``` r
fit4 <-
  update(fit4,
         chains = 4, cores = 4, warmup = 1000, iter = 21000)

fit5 <-
  update(fit5,
         chains = 4, cores = 4, warmup = 1000, iter = 21000)
```

Now there quite a bit closer.

``` r
fixef(fit4)[4, ] %>% round(digits = 3)
```

    ##  Estimate Est.Error      Q2.5     Q97.5 
    ##     0.360     0.179     0.012     0.712

``` r
fixef(fit5)[4, ] %>% round(digits = 3)
```

    ##  Estimate Est.Error      Q2.5     Q97.5 
    ##     0.361     0.179     0.010     0.712

And before you get fixate on how there are still differences after 80,000 iterations, each, consider comparing the two density plots:

``` r
posterior_samples(fit4) %>% 
  as_tibble() %>% 
  select(`b_policy:kerry`) %>% 
  rename(iteraction = `b_policy:kerry`) %>% 
  bind_rows(
    posterior_samples(fit5) %>% 
      as_tibble() %>% 
      select(`b_policy_me:kerry_me`) %>% 
      rename(iteraction = `b_policy_me:kerry_me`)
  ) %>% 
  mutate(model = rep(c("fit4", "fit5"), each = 80000)) %>% 
  
  ggplot(aes(x = iteraction, fill = model)) +
  geom_density(size = 0, alpha = 1/2) +
  scale_fill_manual(values = ochre_palettes[["olsen_qual"]][c(3, 6)]) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "The interaction densities, by model",
       subtitle = "Yes, they are indeed different. And yet that difference is so\ntrivial that we'd expect greater variability from measurement\nerror than we still have from simulation error.",
       x = NULL) +
  theme_08 
```

![](Chapter_08_files/figure-markdown_github/unnamed-chunk-55-1.png)

### Conducting ~~a 2 X 2 between-participants factorial ANOVA using PROCESS~~ another regression model with brms.

Since we're square in regression land with brms, there's no direct analogue for us, here. However, notice all those post-ANOVA *t*-tests Hayes presented on page 300. If we just want to consider the 2 X 2 structure of our two dummy variables as indicative of four groups, we have one more coding system up ourselves. With the handy `str_c()` function, we'll concatenate the `policy` and `kerry` values into an unordered categorical variable, `policy_kerry`. Here's what that looks like:

``` r
caskets <-
  caskets %>% 
  mutate(policy_kerry = str_c(policy, kerry))

head(caskets)
```

    ## # A tibble: 6 x 8
    ##   policy interest   age  educ  male conserv kerry policy_kerry
    ##    <int>    <dbl> <int> <int> <int>   <int> <int> <chr>       
    ## 1      1     4.00    39     3     1       4     1 11          
    ## 2      0     2.00    57     3     1       3     1 01          
    ## 3      1     3.00    63     2     0       6     1 11          
    ## 4      1     1.00    56     5     1       3     1 11          
    ## 5      1     1.00    50     3     0       3     1 11          
    ## 6      0     2.00    87     2     1       5     0 00

Now check out what happens if we reformat our formula as `interest ~ 0 + policy_kerry`.

``` r
fit6 <-
  brm(data = caskets, family = gaussian,
      interest ~ 0 + policy_kerry,
      chains = 4, cores = 4)
```

The `brm()` function recnognized `policy_kerry` was a character vector and treated it as a nominal variable. The `0 +` part of the fucntion removed the model intercept. Here's how that effects the output:

``` r
print(fit6)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: interest ~ 0 + policy_kerry 
    ##    Data: caskets (Number of observations: 541) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##                Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## policy_kerry00     1.78      0.09     1.61     1.96       4000 1.00
    ## policy_kerry01     2.38      0.09     2.21     2.56       4000 1.00
    ## policy_kerry10     1.40      0.09     1.22     1.58       4000 1.00
    ## policy_kerry11     2.35      0.08     2.19     2.51       4000 1.00
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## sigma     1.04      0.03     0.97     1.10       4000 1.00
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Without the typical interecept, `brm()` estimated the means for each of the four `policy_kerry` groups. Here's what their densities look like:

``` r
posterior_samples(fit6) %>% 
  select(b_policy_kerry00:b_policy_kerry11) %>% 
  gather() %>%
  mutate(key = str_remove(key, "b_")) %>% 
  
  ggplot(aes(x = value, fill = key)) +
  geom_density(color = "transparent", alpha = 2/3) +
  scale_fill_manual(NULL,
                    values = ochre_palettes[["olsen_qual"]][c(5, 6, 4, 3)]) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "Group means",
       x = NULL) +
  theme_08
```

![](Chapter_08_files/figure-markdown_github/unnamed-chunk-59-1.png)

Since each of the four primary vectors returned by `posterior_samples(fit6)` is of a group mean, it's trivial to compute difference scores. To compute the difference score analogous to Hayes's two *t*-tests, we'd do the following.

``` r
posterior_samples(fit6) %>% 
  transmute(difference_1 = b_policy_kerry10 - b_policy_kerry00,
            difference_2 = b_policy_kerry11 - b_policy_kerry01) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 2 x 4
    ##   key           median     ll     ul
    ##   <chr>          <dbl>  <dbl>  <dbl>
    ## 1 difference_1 -0.389  -0.635 -0.139
    ## 2 difference_2 -0.0300 -0.265  0.205

Note. The analyses in this document were done with:

-   R 3.4.4
-   RStudio 1.1.442
-   rmarkdown 1.9
-   ggthemes 3.5.0
-   ochRe 1.0.0
-   tidyverse 1.2.1
-   readr 1.1.1
-   rstan 2.17.3
-   brms 2.3.1

Reference
---------

Hayes, A. F. (2018). *Introduction to mediation, moderation, and conditional process analysis: A regression-based approach.* (2nd ed.). New York, NY, US: The Guilford Press.
