Chapter 12
================
A Solomon Kurz
2018-06-28

11.3 Example: Hiding your feelings from your work team
------------------------------------------------------

Here we load a couple necessary packages, load the data, and take a `glimpse()`.

``` r
library(readr)
library(tidyverse)

disaster <- read_csv("data/disaster/disaster.csv")

glimpse(disaster)
```

    ## Observations: 211
    ## Variables: 5
    ## $ id      <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, ...
    ## $ frame   <int> 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0...
    ## $ donate  <dbl> 5.6, 4.2, 4.2, 4.6, 3.0, 5.0, 4.8, 6.0, 4.2, 4.4, 5.8, 6.2, 6.0, 4.2, 4.4, 5.8,...
    ## $ justify <dbl> 2.95, 2.85, 3.00, 3.30, 5.00, 3.20, 2.90, 1.40, 3.25, 3.55, 1.55, 1.60, 1.65, 2...
    ## $ skeptic <dbl> 1.8, 5.2, 3.2, 1.0, 7.6, 4.2, 4.2, 1.2, 1.8, 8.8, 1.0, 5.4, 2.2, 3.6, 7.8, 1.6,...

Our `model1` is the simple moderation model.

``` r
library(brms)

model1 <-
  brm(data = disaster, family = gaussian,
      donate ~ 1 + frame + skeptic + frame:skeptic,
      chains = 4, cores = 4)
```

Our `model1` summary matches nicely with the text.

``` r
print(model1, digits = 3)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: donate ~ 1 + frame + skeptic + frame:skeptic 
    ##    Data: disaster (Number of observations: 211) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## Intercept        5.026     0.229    4.583    5.481       2404 1.000
    ## frame            0.677     0.328    0.036    1.314       2154 1.001
    ## skeptic         -0.139     0.058   -0.252   -0.025       2195 1.001
    ## frame:skeptic   -0.170     0.084   -0.329   -0.007       2040 1.001
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## sigma    1.242     0.062    1.124    1.369       3122 1.001
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

For the figures in this chapter, we'll take cues from Matthew Kay's [tidybayes package](https://github.com/mjskay/tidybayes). Otherwise, our Figure 12.2 is business as usual at this point.

``` r
theme_set(theme_light())

nd <-
  tibble(frame = rep(0:1, each = 30),
         skeptic = rep(seq(from = 0, to = 7, length.out = 30),
                       times = 2))

fitted(model1, newdata = nd) %>% 
  as_tibble() %>% 
  bind_cols(nd) %>% 
  mutate(frame = ifelse(frame == 0, str_c("Natural causes (X = ", frame, ")"),
                        str_c("Climate change (X = ", frame, ")"))) %>% 
  
  ggplot(aes(x = skeptic, y = Estimate)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = frame),
              alpha = 1/3) +
  geom_line(aes(color = frame)) +
  scale_fill_brewer(type = "qual") +
  scale_color_brewer(type = "qual") +
  coord_cartesian(xlim = 1:6) +
  labs(x = expression(paste("Climate Change Skepticism (", italic(W), ")")),
       y = "Willingness to Donate to Victims") +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank())
```

![](Chapter_12_files/figure-markdown_github/unnamed-chunk-4-1.png)

In Hayes's Figure 12.2, he emphasized the differences at the three levels of `skeptic`. If you want the full difference score distributions in a pick-a-point-approach sort of way, you might plot the densities with `tidybayes::geom_halfeyeh()`, which places coefficient plots at the base of the densities. In this case, we show the posterior medians with the dots, the 50% intervals with the thick horizontal lines, and the 95% intervals with the thinner horizontal lines.

``` r
library(tidybayes)
nd <-
  tibble(frame = rep(0:1, times = 3),
         skeptic = rep(quantile(disaster$skeptic, probs = c(.16, .5, .86)),
                                times = 2))

fitted(model1, summary = F,
       newdata = nd) %>% 
  as_tibble() %>% 
  gather() %>% 
  mutate(frame = rep(rep(0:1, times = 3),
                     each = 4000),
         skeptic = rep(rep(quantile(disaster$skeptic, probs = c(.16, .5, .86)),
                                times = 2),
                       each = 4000),
         iter = rep(1:4000, times = 6)) %>% 
  select(-key) %>% 
  spread(key = frame, value = value) %>% 
  mutate(difference = `1` - `0`) %>% 
  
  ggplot(aes(x = difference, y = skeptic, group = skeptic, fill = skeptic %>% as.character())) +
  geom_halfeyeh(.prob = c(0.95, 0.5)) +
  scale_fill_brewer() +
  scale_y_continuous(breaks = quantile(disaster$skeptic, probs = c(.16, .5, .86)),
                     labels = quantile(disaster$skeptic, probs = c(.16, .5, .86)) %>% round(2)) +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank())
```

![](Chapter_12_files/figure-markdown_github/unnamed-chunk-5-1.png)

Here's our simple mediation model, `model2`, using the multivariate syntax right in the `brm()` function.

``` r
model2 <-
  brm(data = disaster, family = gaussian,
      bf(justify ~ 1 + frame) +
        bf(donate ~ 1 + frame + justify) +
        set_rescor(FALSE),
      chains = 4, cores = 4)
```

``` r
print(model2, digits = 3)
```

    ##  Family: MV(gaussian, gaussian) 
    ##   Links: mu = identity; sigma = identity
    ##          mu = identity; sigma = identity 
    ## Formula: justify ~ 1 + frame 
    ##          donate ~ 1 + frame + justify 
    ##    Data: disaster (Number of observations: 211) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##                   Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## justify_Intercept    2.803     0.090    2.627    2.978       4000 1.000
    ## donate_Intercept     7.236     0.233    6.791    7.695       4000 1.000
    ## justify_frame        0.135     0.131   -0.127    0.385       4000 1.001
    ## donate_frame         0.215     0.137   -0.056    0.479       4000 0.999
    ## donate_justify      -0.954     0.074   -1.100   -0.810       4000 1.000
    ## 
    ## Family Specific Parameters: 
    ##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## sigma_justify    0.935     0.045    0.851    1.025       4000 1.000
    ## sigma_donate     0.986     0.048    0.895    1.086       4000 0.999
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

If you want the indirect effect with its intervals, you use `posterior_samples()` and data wrangle, as usual.

``` r
posterior_samples(model2) %>% 
  mutate(ab = b_justify_frame*b_donate_justify) %>% 
  summarize(mean = mean(ab),
            ll = quantile(ab, probs = .025),
            ul = quantile(ab, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ##     mean     ll   ul
    ## 1 -0.129 -0.378 0.12

12.2 Moderation of the direct and indirect effects in a conditional process model
---------------------------------------------------------------------------------

We don't need to do anything particularly special to fit a model like this in brms. It just requires we do a careful job specifying the formulas in our `bf()` arguments. If you find this syntax a little too cumbersome, you can always specify the formulas outside of `brm()`, save them as one or multiple objects, and plug those objects into `brm()`.

``` r
model3 <-
  brm(data = disaster, family = gaussian,
      bf(justify ~ 1 + frame + skeptic + frame:skeptic) +
        bf(donate ~ 1 + frame + justify + skeptic + frame:skeptic) +
        set_rescor(FALSE),
      chains = 4, cores = 4)
```

The model summary:

``` r
print(model3, digits = 3)
```

    ##  Family: MV(gaussian, gaussian) 
    ##   Links: mu = identity; sigma = identity
    ##          mu = identity; sigma = identity 
    ## Formula: justify ~ 1 + frame + skeptic + frame:skeptic 
    ##          donate ~ 1 + frame + justify + skeptic + frame:skeptic 
    ##    Data: disaster (Number of observations: 211) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##                       Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## justify_Intercept        2.453     0.149    2.159    2.739       3900 0.999
    ## donate_Intercept         7.292     0.272    6.744    7.834       4000 0.999
    ## justify_frame           -0.565     0.219   -0.999   -0.133       3545 1.000
    ## justify_skeptic          0.105     0.038    0.032    0.181       3590 0.999
    ## justify_frame:skeptic    0.201     0.055    0.089    0.309       3270 1.000
    ## donate_frame             0.155     0.267   -0.350    0.676       3240 1.001
    ## donate_justify          -0.923     0.085   -1.092   -0.756       4000 0.999
    ## donate_skeptic          -0.043     0.047   -0.133    0.050       3565 1.000
    ## donate_frame:skeptic     0.016     0.068   -0.120    0.150       2974 1.001
    ## 
    ## Family Specific Parameters: 
    ##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## sigma_justify    0.818     0.040    0.742    0.902       4000 1.000
    ## sigma_donate     0.989     0.049    0.901    1.092       4000 1.000
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

### Quantifying direct and indirect effects.

Here are *a*<sub>1</sub> through *a*<sub>3</sub>.

``` r
fixef(model3)[c(3:5), ] %>% round(digits = 3)
```

    ##                       Estimate Est.Error   Q2.5  Q97.5
    ## justify_frame           -0.565     0.219 -0.999 -0.133
    ## justify_skeptic          0.105     0.038  0.032  0.181
    ## justify_frame:skeptic    0.201     0.055  0.089  0.309

This is *b*.

``` r
fixef(model3)[7, ] %>% round(digits = 3)
```

    ##  Estimate Est.Error      Q2.5     Q97.5 
    ##    -0.923     0.085    -1.092    -0.756

We'll need to employ `posterior_samples()` to compute (*a*<sub>1</sub> + *a*<sub>3</sub>*W*)*b*.

``` r
post <- 
  posterior_samples(model3) %>% 
  mutate(`indirect effect when W is 1.592` = (b_justify_frame + `b_justify_frame:skeptic`*1.592)*b_donate_justify,
         `indirect effect when W is 2.800` = (b_justify_frame + `b_justify_frame:skeptic`*2.800)*b_donate_justify,
         `indirect effect when W is 5.200` = (b_justify_frame + `b_justify_frame:skeptic`*5.200)*b_donate_justify) 

post %>% 
  select(starts_with("indirect")) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 3 x 4
    ##   key                             median     ll     ul
    ##   <chr>                            <dbl>  <dbl>  <dbl>
    ## 1 indirect effect when W is 1.592  0.227 -0.045  0.504
    ## 2 indirect effect when W is 2.800  0.003 -0.211  0.211
    ## 3 indirect effect when W is 5.200 -0.444 -0.73  -0.173

### The conditional direct effect of X.

This process is very similar.

``` r
post <- 
  post %>% 
  mutate(`direct effect when W is 1.592` = b_donate_frame + `b_donate_frame:skeptic`*1.592,
         `direct effect when W is 2.800` = b_donate_frame + `b_donate_frame:skeptic`*2.800,
         `direct effect when W is 5.200` = b_donate_frame + `b_donate_frame:skeptic`*5.200)

post %>% 
  select(starts_with("direct")) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 3 x 4
    ##   key                           median     ll    ul
    ##   <chr>                          <dbl>  <dbl> <dbl>
    ## 1 direct effect when W is 1.592  0.179 -0.169 0.534
    ## 2 direct effect when W is 2.800  0.202 -0.072 0.482
    ## 3 direct effect when W is 5.200  0.235 -0.116 0.618

### Visualizing the direct and indirect effects.

In order to make Figure 12.7, we'll use `sapply()` to get the conditional effects for `justify` and `donate`.

``` r
justify_effects <-
  sapply(seq(from = 0, to = 6, length.out = 30), function(w){
    (post$b_justify_frame + post$`b_justify_frame:skeptic`*w)*post$b_donate_justify
    }) %>% 
  as_tibble() %>% 
  gather() %>% 
  select(-key) %>% 
  mutate(skeptic = seq(from = 0, to = 6, length.out = 30) %>% rep(., each = 4000)) %>% 
  group_by(skeptic) %>% 
  summarize(median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975))

donate_effects <-
  sapply(seq(from = 0, to = 6, length.out = 30), function(w){
    post$b_donate_frame + post$`b_donate_frame:skeptic`*w
    }) %>% 
  as_tibble() %>% 
  gather() %>% 
  select(-key) %>% 
  mutate(skeptic = seq(from = 0, to = 6, length.out = 30) %>% rep(., each = 4000)) %>% 
  group_by(skeptic) %>% 
  summarize(median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975))

# here's what they look like:
glimpse(justify_effects)
```

    ## Observations: 30
    ## Variables: 4
    ## $ skeptic <dbl> 0.0000000, 0.2068966, 0.4137931, 0.6206897, 0.8275862, 1.0344828, 1.2413793, 1....
    ## $ median  <dbl> 0.51994437, 0.48122918, 0.44308125, 0.40480865, 0.36744617, 0.32995722, 0.29189...
    ## $ ll      <dbl> 0.120363185, 0.102272788, 0.083276892, 0.065407997, 0.042008590, 0.021865977, -...
    ## $ ul      <dbl> 0.933347495, 0.878190871, 0.822235540, 0.764233193, 0.705737438, 0.650791681, 0...

``` r
glimpse(donate_effects)
```

    ## Observations: 30
    ## Variables: 4
    ## $ skeptic <dbl> 0.0000000, 0.2068966, 0.4137931, 0.6206897, 0.8275862, 1.0344828, 1.2413793, 1....
    ## $ median  <dbl> 0.1512183, 0.1544502, 0.1581665, 0.1621030, 0.1653814, 0.1676131, 0.1710118, 0....
    ## $ ll      <dbl> -0.35010589, -0.32419515, -0.30001969, -0.27399908, -0.25086959, -0.22986830, -...
    ## $ ul      <dbl> 0.6760396, 0.6608650, 0.6405930, 0.6219335, 0.6020020, 0.5852079, 0.5604962, 0....

Here we'll combine those two tibbles by stacking `donate_effects` underneath `justify_effects` and then indexing them by `effect`. Then we're ready to plot.

``` r
# combining the tibbles
figure_12.7 <-
  justify_effects %>% 
  bind_rows(donate_effects) %>% 
  mutate(effect = rep(c("Indirect effect", "Direct effect"), each = nrow(justify_effects)))
  
# we'll need this for `geom_text()`
text_tibble <-
  tibble(x = c(4.2, 4.7),
         y = c(.28, -.28),
         angle = c(3.6, 335),
         effect = c("Direct effect", "Indirect effect"))

# the plot
figure_12.7 %>% 
  ggplot(aes(x = skeptic, group = effect)) +
  geom_ribbon(aes(ymin = ll, ymax = ul, fill = effect),
              alpha = 1/3) +
  geom_line(aes(y = median, color = effect)) +
  geom_text(data = text_tibble,
            aes(x = x, y = y,
                angle = angle, 
                color = effect,
                label = effect),
            size = 5) +
  scale_fill_brewer(type = "qual") +
  scale_color_brewer(type = "qual") +
  coord_cartesian(xlim = c(1, 5.5),
                  ylim = c(-.6, .4)) +
  labs(x = expression(paste("Climate Change Skepticism (", italic(W), ")")),
       y = "Effects of Disaster Frame on Willingness to Donate") +
  theme(legend.position = "none")
```

![](Chapter_12_files/figure-markdown_github/unnamed-chunk-16-1.png)

Note how wide those 95% intervals are relative to the scale of the y-axis. I specifically kept the y-axis within the same range as Figure 12.7 in the text. To me the message is clear: include credible-interval ribbons in your regression slope plots. They help depict how uncertain the posterior is in a way a simple line slopes just don't.

### **Bonus**: Let's replace `sapply()` with `map()`.

Good old `sapply()` worked just fine for our purposes, above. However, we can use `map()` to accomplish those goals in a more tidyverse-consistent fashion. First we'll define two custom functions to do what our two `sapply()` statements did for us.

``` r
# defining two custom functions
make_justify <- function(w){
    (post$b_justify_frame + post$`b_justify_frame:skeptic`*w)*post$b_donate_justify
  }

make_donate <-function(w){
    post$b_donate_frame + post$`b_donate_frame:skeptic`*w
  }
```

Next, we'll make a 30-row tibble with each row a value for `skeptic`, ranging from 0 ot 6, just like what we did with `sapply()`, above. Because we'll be performing a nested operation for each value of `skeptic`, we'll group the tibble by `skeptic`. Then with the `mutate()` function, we'll use `map()` to apply our custom `make_justify` and `make_donate` functions to each of the 30 `skeptic` values.

``` r
tidyverse_style_tibble <-
  tibble(skeptic = seq(from = 0, to = 6, length.out = 30)) %>% 
  group_by(skeptic) %>% 
  mutate(`indirect effect` = map(skeptic, make_justify),
         `direct effect` = map(skeptic, make_donate))

tidyverse_style_tibble
```

    ## # A tibble: 30 x 3
    ## # Groups:   skeptic [30]
    ##    skeptic `indirect effect` `direct effect`
    ##      <dbl> <list>            <list>         
    ##  1   0     <dbl [4,000]>     <dbl [4,000]>  
    ##  2   0.207 <dbl [4,000]>     <dbl [4,000]>  
    ##  3   0.414 <dbl [4,000]>     <dbl [4,000]>  
    ##  4   0.621 <dbl [4,000]>     <dbl [4,000]>  
    ##  5   0.828 <dbl [4,000]>     <dbl [4,000]>  
    ##  6   1.03  <dbl [4,000]>     <dbl [4,000]>  
    ##  7   1.24  <dbl [4,000]>     <dbl [4,000]>  
    ##  8   1.45  <dbl [4,000]>     <dbl [4,000]>  
    ##  9   1.66  <dbl [4,000]>     <dbl [4,000]>  
    ## 10   1.86  <dbl [4,000]>     <dbl [4,000]>  
    ## # ... with 20 more rows

This yielded a nested tibble. At one level of investigation, we have 30 rows--one for each of the 30 `skeptic` values. However, for both the `idirect effect` and `direct effect` columns, we've packed an entire 4000-row list into each of those rows. The lists are 4000-rows long because both of our custom functions entailed pushing those `skeptic` values through the posterior, which itself had 4000 iterations. Next we'll use `unnest()` to unnest the tibble.

``` r
tidyverse_style_tibble <-
  tidyverse_style_tibble %>% 
  unnest()

head(tidyverse_style_tibble)
```

    ## # A tibble: 6 x 3
    ## # Groups:   skeptic [1]
    ##   skeptic `indirect effect` `direct effect`
    ##     <dbl>             <dbl>           <dbl>
    ## 1       0             0.613         0.0267 
    ## 2       0             0.794        -0.254  
    ## 3       0             0.707        -0.336  
    ## 4       0             0.637         0.00447
    ## 5       0             0.477        -0.510  
    ## 6       0             0.286         0.590

After un-nesting, the tibble is now 4000\*30 = 120,000 rows long. With just a little more wrangling, we'll have our familiar summaries for each level of `skeptic`.

``` r
tidyverse_style_tibble <-
  tidyverse_style_tibble %>% 
  ungroup() %>% 
  mutate(iter = rep(1:4000, times = 30)) %>% 
  gather(effect, value, -skeptic, -iter) %>% 
  group_by(effect, skeptic) %>% 
  summarize(median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975))

head(tidyverse_style_tibble)
```

    ## # A tibble: 6 x 5
    ## # Groups:   effect [1]
    ##   effect        skeptic median     ll    ul
    ##   <chr>           <dbl>  <dbl>  <dbl> <dbl>
    ## 1 direct effect   0      0.151 -0.350 0.676
    ## 2 direct effect   0.207  0.154 -0.324 0.661
    ## 3 direct effect   0.414  0.158 -0.300 0.641
    ## 4 direct effect   0.621  0.162 -0.274 0.622
    ## 5 direct effect   0.828  0.165 -0.251 0.602
    ## 6 direct effect   1.03   0.168 -0.230 0.585

Now we have 60 row, 30 for `direct effect` and another 30 for `indirect effect`. Each has the typical summary values for all 30 levels of `skeptic`. Now we're ready to plot.

``` r
tidyverse_style_tibble %>% 
 ggplot(aes(x = skeptic, group = effect)) +
  geom_ribbon(aes(ymin = ll, ymax = ul, fill = effect),
              alpha = 1/3) +
  geom_line(aes(y = median, color = effect)) +
  scale_fill_brewer(type = "qual", palette = 2) +
  scale_color_brewer(type = "qual", palette = 2) +
  coord_cartesian(xlim = c(1, 5.5),
                  ylim = c(-.6, .4)) +
  labs(x = expression(paste("Climate Change Skepticism (", italic(W), ")")),
       y = "Effects of Disaster Frame on Willingness to Donate") +
  theme(legend.position = "none")
```

![](Chapter_12_files/figure-markdown_github/unnamed-chunk-21-1.png)

To learn more about nested data and using the `map()` function, check out [this subsection](http://r4ds.had.co.nz/many-models.html#nested-data) of Grolemund and Wickham's [*R4DS*](http://r4ds.had.co.nz) or [starting from this point on](https://www.youtube.com/watch?v=rz3_FDVt9eg&t=824s&frags=pl%2Cwn) in this video of one of Wickham's workshops.

12.3 Statistical inference
--------------------------

### Inference about the direct effect.

We've already computed the 95% intervals for these. Here they are as pointrange plots.

``` r
post %>% 
  select(starts_with("direct")) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(median = median(value),
            ll_50 = quantile(value, probs = .25),
            ul_50 = quantile(value, probs = .75),
            ll_95 = quantile(value, probs = .025),
            ul_95 = quantile(value, probs = .975)) %>% 
  mutate(key = str_remove(key, "direct effect when W is ") %>% as.double()) %>% 
  
  ggplot(aes(x = key)) +
  geom_pointrange(aes(y = median, ymin = ll_95, ymax = ul_95)) +
  geom_linerange(aes(ymin = ll_50, ymax = ul_50), size = 1.25) +
  coord_cartesian(xlim = c(1, 5.5)) +
  labs(x = expression(paste("Climate Change Skepticism (", italic(W), ")")),
       y = "Conditional Direct Effect of Disaster Frame on\nWillingness to Donate")
```

![](Chapter_12_files/figure-markdown_github/unnamed-chunk-22-1.png)

If you really wanted our analogue to the standard error, the Bayesian posterior *SD*, you can always use `sd()` in the `summarize()` portion of the code.

### Inference about the indirect effect.

#### A statistical test of moderated mediation.

To get a sense of *a*<sub>3</sub>*b*, we just:

``` r
post <- 
  post %>% 
  mutate(a3b = `b_justify_frame:skeptic`*b_donate_justify) 

post %>% 
  select(a3b) %>% 
  summarize(median = median(a3b),
            sd = sd(a3b),
            ll = quantile(a3b, probs = .025),
            ul = quantile(a3b, probs = .975)) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ##   median    sd     ll     ul
    ## 1 -0.185 0.054 -0.293 -0.081

We might visualize with a coefficient plot.

``` r
post %>% 
  ggplot(aes(x = factor(0), y = a3b)) +
  stat_summary(fun.y = median,
               fun.ymin = function(x){quantile(x, probs = .025)},
               fun.ymax = function(x){quantile(x, probs = .975)}) +
  stat_summary(geom = "linerange",
               fun.ymin = function(x){quantile(x, probs = .25)},
               fun.ymax = function(x){quantile(x, probs = .75)},
               size = 1.25) +
  scale_x_discrete(NULL, breaks = NULL) +
  coord_flip(ylim = c(-.5, 0)) +
  labs(title = expression(paste("Coefficient plot for ", italic(a)[3], italic(b), " (i.e., the index of moderated mediation)")),
       y = NULL)
```

![](Chapter_12_files/figure-markdown_github/unnamed-chunk-24-1.png)

#### Probing moderation of mediation.

As discussed in my manuscript for Chapter 11, our Bayesian version of the JN technique should be fine because HMC does not impose the normality assumption on the parameter posteriors. In this instance, I'll leave the JN technique plot as an exercise for the interested reader. Here we'll just follow along with the text and pick a few points.

We computed and inspected these 95% intervals, above. Here we look at the entire densities with `geom_halfeyeh()`.

``` r
post %>% 
  select(starts_with("indirect")) %>% 
  gather() %>% 
  rename(`indirect effect` = value) %>% 
  mutate(W = str_remove(key, "indirect effect when W is ") %>% as.double()) %>% 
  
  
  ggplot(aes(x = `indirect effect`, y = W, group = W, fill = W %>% as.character())) +
  geom_halfeyeh(.prob = c(0.95, 0.5)) +
  scale_fill_brewer() +
  scale_y_continuous(breaks = c(1.592, 2.8, 5.2),
                     labels = c(1.6, 2.8, 5.2)) +
  coord_cartesian(xlim = -1:1) +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank())
```

![](Chapter_12_files/figure-markdown_github/unnamed-chunk-25-1.png)

### Pruning the model.

Fitting the model without the interaction term is just a small change to one of our `formula` arguments.

``` r
model4 <-
  brm(data = disaster, family = gaussian,
      bf(justify ~ 1 + frame + skeptic + frame:skeptic) +
        bf(donate ~ 1 + frame + justify + skeptic) +
        set_rescor(FALSE),
      chains = 4, cores = 4)
```

Here are the results.

``` r
print(model4, digits = 3)
```

    ##  Family: MV(gaussian, gaussian) 
    ##   Links: mu = identity; sigma = identity
    ##          mu = identity; sigma = identity 
    ## Formula: justify ~ 1 + frame + skeptic + frame:skeptic 
    ##          donate ~ 1 + frame + justify + skeptic 
    ##    Data: disaster (Number of observations: 211) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##                       Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## justify_Intercept        2.452     0.148    2.158    2.739       3801 0.999
    ## donate_Intercept         7.261     0.228    6.825    7.718       4000 1.000
    ## justify_frame           -0.564     0.218   -0.993   -0.138       3523 0.999
    ## justify_skeptic          0.105     0.038    0.031    0.179       3567 0.999
    ## justify_frame:skeptic    0.201     0.055    0.094    0.312       3179 0.999
    ## donate_frame             0.208     0.136   -0.058    0.476       4000 1.000
    ## donate_justify          -0.919     0.081   -1.080   -0.763       4000 1.000
    ## donate_skeptic          -0.036     0.036   -0.106    0.034       4000 1.000
    ## 
    ## Family Specific Parameters: 
    ##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## sigma_justify    0.818     0.040    0.746    0.898       4000 1.000
    ## sigma_donate     0.987     0.049    0.897    1.085       4000 1.000
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Since we're altering the model, we may as well use information criteria to compare the two versions.

``` r
loo(model3, model4)
```

    ##                   LOOIC    SE
    ## model3          1117.42 33.15
    ## model4          1115.03 33.04
    ## model3 - model4    2.39  0.53

The difference in LOO-CV values for the two models was modest. There's little predictive reason to choose one over the other. You could argue that `model4` is simpler than `model3`. Since we've got a complex model either way, one might also consider which one was of primary theoretical interest.

12.4 Mediated moderation
------------------------

### Mediated moderation as the indirect effect of a product.

Hayes explains this in the next subsection, but we've already fit this model, which we called `model3`. Here's the summary.

``` r
print(model3, digits = 3)
```

    ##  Family: MV(gaussian, gaussian) 
    ##   Links: mu = identity; sigma = identity
    ##          mu = identity; sigma = identity 
    ## Formula: justify ~ 1 + frame + skeptic + frame:skeptic 
    ##          donate ~ 1 + frame + justify + skeptic + frame:skeptic 
    ##    Data: disaster (Number of observations: 211) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##                       Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## justify_Intercept        2.453     0.149    2.159    2.739       3900 0.999
    ## donate_Intercept         7.292     0.272    6.744    7.834       4000 0.999
    ## justify_frame           -0.565     0.219   -0.999   -0.133       3545 1.000
    ## justify_skeptic          0.105     0.038    0.032    0.181       3590 0.999
    ## justify_frame:skeptic    0.201     0.055    0.089    0.309       3270 1.000
    ## donate_frame             0.155     0.267   -0.350    0.676       3240 1.001
    ## donate_justify          -0.923     0.085   -1.092   -0.756       4000 0.999
    ## donate_skeptic          -0.043     0.047   -0.133    0.050       3565 1.000
    ## donate_frame:skeptic     0.016     0.068   -0.120    0.150       2974 1.001
    ## 
    ## Family Specific Parameters: 
    ##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## sigma_justify    0.818     0.040    0.742    0.902       4000 1.000
    ## sigma_donate     0.989     0.049    0.901    1.092       4000 1.000
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

### Why mediated moderation is neither interesting nor meaningful.

If it helps interpret this section, take a long look at the model formula.

``` r
model3$formula
```

    ## justify ~ 1 + frame + skeptic + frame:skeptic 
    ## donate ~ 1 + frame + justify + skeptic + frame:skeptic

Note. The analyses in this document were done with:

-   R 3.4.4
-   RStudio 1.1.442
-   rmarkdown 1.9
-   tidyverse 1.2.1
-   readr 1.1.1
-   rstan 2.17.3
-   brms 2.3.2
-   tidybayes 0.12.1.9000

Reference
---------

Hayes, A. F. (2018). *Introduction to mediation, moderation, and conditional process analysis: A regression-based approach.* (2nd ed.). New York, NY, US: The Guilford Press.
