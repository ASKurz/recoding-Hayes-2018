Chapter 12
================
A Solomon Kurz
2018-07-06

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
    ## Intercept        5.028     0.231    4.580    5.484       2240 0.999
    ## frame            0.681     0.339    0.009    1.321       2020 1.000
    ## skeptic         -0.140     0.059   -0.259   -0.026       2252 0.999
    ## frame:skeptic   -0.171     0.086   -0.334   -0.003       1967 1.000
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## sigma    1.242     0.061    1.131    1.369       3292 1.000
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

For the figures in this chapter, we'll take theme cues from Matthew Kay's [tidybayes package](https://github.com/mjskay/tidybayes). Otherwise, our Figure 12.2 is business as usual at this point.

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
  geom_halfeyeh(point_interval = median_qi, .prob = c(0.95, 0.5)) +
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
    ## justify_Intercept    2.801     0.088    2.628    2.971       4000 1.000
    ## donate_Intercept     7.239     0.228    6.798    7.692       4000 1.000
    ## justify_frame        0.134     0.126   -0.108    0.380       4000 1.000
    ## donate_frame         0.212     0.136   -0.055    0.486       4000 1.000
    ## donate_justify      -0.954     0.074   -1.101   -0.812       4000 1.000
    ## 
    ## Family Specific Parameters: 
    ##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## sigma_justify    0.935     0.046    0.849    1.030       4000 1.000
    ## sigma_donate     0.986     0.048    0.896    1.090       4000 1.000
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

    ##     mean     ll    ul
    ## 1 -0.128 -0.362 0.107

We might also streamline our code a touch using `tidybayes::mean_qi()` in place of `tidyverse::summarize()`.

``` r
posterior_samples(model2) %>% 
  mutate(ab = b_justify_frame*b_donate_justify) %>% 
  mean_qi(ab, .prob = .95) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 1 x 4
    ##       ab conf.low conf.high .prob
    ##    <dbl>    <dbl>     <dbl> <dbl>
    ## 1 -0.128   -0.362     0.107  0.95

Note that the last column explicates what interval level we used.

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
    ## justify_Intercept        2.450     0.146    2.169    2.740       3471 1.000
    ## donate_Intercept         7.295     0.273    6.761    7.826       4000 1.000
    ## justify_frame           -0.562     0.218   -0.986   -0.144       3139 1.000
    ## justify_skeptic          0.105     0.038    0.031    0.178       3543 1.000
    ## justify_frame:skeptic    0.201     0.056    0.092    0.308       3096 1.000
    ## donate_frame             0.159     0.270   -0.383    0.678       3139 1.000
    ## donate_justify          -0.924     0.084   -1.092   -0.762       4000 0.999
    ## donate_skeptic          -0.042     0.048   -0.134    0.054       4000 1.001
    ## donate_frame:skeptic     0.015     0.069   -0.120    0.150       2947 1.001
    ## 
    ## Family Specific Parameters: 
    ##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## sigma_justify    0.818     0.041    0.746    0.905       4000 1.000
    ## sigma_donate     0.989     0.049    0.899    1.087       4000 0.999
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
    ## justify_frame           -0.562     0.218 -0.986 -0.144
    ## justify_skeptic          0.105     0.038  0.031  0.178
    ## justify_frame:skeptic    0.201     0.056  0.092  0.308

This is *b*.

``` r
fixef(model3)[7, ] %>% round(digits = 3)
```

    ##  Estimate Est.Error      Q2.5     Q97.5 
    ##    -0.924     0.084    -1.092    -0.762

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
  median_qi(value, .prob = .95) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 3 x 5
    ## # Groups:   key [3]
    ##   key                              value conf.low conf.high .prob
    ##   <chr>                            <dbl>    <dbl>     <dbl> <dbl>
    ## 1 indirect effect when W is 1.592  0.223   -0.043     0.499  0.95
    ## 2 indirect effect when W is 2.800  0       -0.211     0.21   0.95
    ## 3 indirect effect when W is 5.200 -0.444   -0.733    -0.168  0.95

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
  median_qi(value, .prob = .95) %>% 
  mutate_if(is.double, round, digits = 3)
```

    ## # A tibble: 3 x 5
    ## # Groups:   key [3]
    ##   key                           value conf.low conf.high .prob
    ##   <chr>                         <dbl>    <dbl>     <dbl> <dbl>
    ## 1 direct effect when W is 1.592 0.183   -0.181     0.533  0.95
    ## 2 direct effect when W is 2.800 0.2     -0.077     0.468  0.95
    ## 3 direct effect when W is 5.200 0.237   -0.123     0.583  0.95

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
    ## $ median  <dbl> 0.51674735, 0.47876931, 0.44137940, 0.40397112, 0.36509797, 0.32670971, 0.28745...
    ## $ ll      <dbl> 0.1273677362, 0.1102148045, 0.0855316463, 0.0623580439, 0.0429606471, 0.0213024...
    ## $ ul      <dbl> 0.927671789, 0.871825923, 0.815031497, 0.754776962, 0.694773413, 0.640567459, 0...

``` r
glimpse(donate_effects)
```

    ## Observations: 30
    ## Variables: 4
    ## $ skeptic <dbl> 0.0000000, 0.2068966, 0.4137931, 0.6206897, 0.8275862, 1.0344828, 1.2413793, 1....
    ## $ median  <dbl> 0.1648400, 0.1681041, 0.1703780, 0.1739510, 0.1763307, 0.1782794, 0.1793318, 0....
    ## $ ll      <dbl> -0.38309533, -0.35707699, -0.33114537, -0.30110940, -0.27593712, -0.24907860, -...
    ## $ ul      <dbl> 0.6779338, 0.6563455, 0.6353144, 0.6150369, 0.5936870, 0.5799982, 0.5606131, 0....

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

![](Chapter_12_files/figure-markdown_github/unnamed-chunk-17-1.png)

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
    ## 1       0             0.638          0.159 
    ## 2       0             0.730          0.140 
    ## 3       0             0.818         -0.0547
    ## 4       0             0.603         -0.273 
    ## 5       0             0.763         -0.592 
    ## 6       0             0.771         -0.666

After un-nesting, the tibble is now 4000\*30 = 120,000 rows long. With just a little more wrangling, we'll have our familiar summaries for each level of `skeptic`.

``` r
tidyverse_style_tibble <-
  tidyverse_style_tibble %>% 
  ungroup() %>% 
  mutate(iter = rep(1:4000, times = 30)) %>% 
  gather(effect, value, -skeptic, -iter) %>% 
  group_by(effect, skeptic) %>% 
  median_qi(value, .prob = .95)
  
head(tidyverse_style_tibble)
```

    ## # A tibble: 6 x 6
    ## # Groups:   effect, skeptic [6]
    ##   effect        skeptic value conf.low conf.high .prob
    ##   <chr>           <dbl> <dbl>    <dbl>     <dbl> <dbl>
    ## 1 direct effect   0     0.165   -0.383     0.678  0.95
    ## 2 direct effect   0.207 0.168   -0.357     0.656  0.95
    ## 3 direct effect   0.414 0.170   -0.331     0.635  0.95
    ## 4 direct effect   0.621 0.174   -0.301     0.615  0.95
    ## 5 direct effect   0.828 0.176   -0.276     0.594  0.95
    ## 6 direct effect   1.03  0.178   -0.249     0.580  0.95

Now we have 60 row, 30 for `direct effect` and another 30 for `indirect effect`. Each has the typical summary values for all 30 levels of `skeptic`. Now we're ready to plot.

``` r
tidyverse_style_tibble %>% 
 ggplot(aes(x = skeptic, group = effect)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = effect),
              alpha = 1/3) +
  geom_line(aes(y = value, color = effect)) +
  scale_fill_brewer(type = "qual", palette = 2) +
  scale_color_brewer(type = "qual", palette = 2) +
  coord_cartesian(xlim = c(1, 5.5),
                  ylim = c(-.6, .4)) +
  labs(x = expression(paste("Climate Change Skepticism (", italic(W), ")")),
       y = "Effects of Disaster Frame on Willingness to Donate") +
  theme(legend.position = "none")
```

![](Chapter_12_files/figure-markdown_github/unnamed-chunk-22-1.png)

Do note how, in our plot above, we used tidybayes terms `value` (i.e., median--the specified measure of central tendency), `conf.low` and `conf.high`, the lower- and upper-levels of the 95% interval.

To learn more about nested data and using the `map()` function, check out [this subsection](http://r4ds.had.co.nz/many-models.html#nested-data) of Grolemund and Wickham's [*R4DS*](http://r4ds.had.co.nz) or [starting from this point on](https://www.youtube.com/watch?v=rz3_FDVt9eg&t=824s&frags=pl%2Cwn) in this video of one of Wickham's workshops.

12.3 Statistical inference
--------------------------

### Inference about the direct effect.

We've already computed the 95% intervals for these. Here they are as `stat_pointinterval()` plots.

``` r
post %>% 
  select(starts_with("direct")) %>% 
  gather() %>% 
  mutate(key = str_remove(key, "direct effect when W is ") %>% as.double()) %>% 
  
  ggplot(aes(x = key, y = value, group = key)) +
  stat_pointinterval(point_interval = median_qi, .prob = c(.95, .5)) +
  coord_cartesian(xlim = c(1, 5.5)) +
  labs(x = expression(paste("Climate Change Skepticism (", italic(W), ")")),
       y = "Conditional Direct Effect of Disaster Frame on\nWillingness to Donate")
```

![](Chapter_12_files/figure-markdown_github/unnamed-chunk-23-1.png)

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
    ## 1 -0.183 0.054 -0.292 -0.083

We might use `stat_pointintervalh()` to visualize *a*<sub>3</sub>*b* with a coefficient plot.

``` r
post %>% 
  ggplot(aes(x = a3b, y = 1)) +
  stat_pointintervalh(point_interval = median_qi, .prob = c(.95, .5)) +
  scale_y_discrete(NULL, breaks = NULL) +
  coord_cartesian(xlim = c(-.5, 0)) +
  labs(title = expression(paste("Coefficient plot for ", italic(a)[3], italic(b), " (i.e., the index of moderated mediation)")),
       x = NULL)
```

![](Chapter_12_files/figure-markdown_github/unnamed-chunk-25-1.png)

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
  geom_halfeyeh(point_interval = median_qi, .prob = c(0.95, 0.5)) +
  scale_fill_brewer() +
  scale_y_continuous(breaks = c(1.592, 2.8, 5.2),
                     labels = c(1.6, 2.8, 5.2)) +
  coord_cartesian(xlim = -1:1) +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank())
```

![](Chapter_12_files/figure-markdown_github/unnamed-chunk-26-1.png)

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
    ## justify_Intercept        2.456     0.153    2.160    2.749       2919 1.000
    ## donate_Intercept         7.254     0.227    6.794    7.698       4000 1.001
    ## justify_frame           -0.569     0.220   -0.997   -0.146       2671 1.000
    ## justify_skeptic          0.105     0.039    0.026    0.182       3002 1.000
    ## justify_frame:skeptic    0.202     0.056    0.097    0.314       2548 1.000
    ## donate_frame             0.214     0.138   -0.056    0.477       4000 1.000
    ## donate_justify          -0.918     0.084   -1.083   -0.752       3553 1.000
    ## donate_skeptic          -0.036     0.037   -0.109    0.036       3828 1.000
    ## 
    ## Family Specific Parameters: 
    ##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## sigma_justify    0.817     0.040    0.744    0.899       4000 1.000
    ## sigma_donate     0.987     0.049    0.898    1.091       4000 1.001
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Since we're altering the model, we may as well use information criteria to compare the two versions.

``` r
loo(model3, model4)
```

    ##                   LOOIC    SE
    ## model3          1117.21 33.18
    ## model4          1115.66 33.08
    ## model3 - model4    1.55  0.49

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
    ## justify_Intercept        2.450     0.146    2.169    2.740       3471 1.000
    ## donate_Intercept         7.295     0.273    6.761    7.826       4000 1.000
    ## justify_frame           -0.562     0.218   -0.986   -0.144       3139 1.000
    ## justify_skeptic          0.105     0.038    0.031    0.178       3543 1.000
    ## justify_frame:skeptic    0.201     0.056    0.092    0.308       3096 1.000
    ## donate_frame             0.159     0.270   -0.383    0.678       3139 1.000
    ## donate_justify          -0.924     0.084   -1.092   -0.762       4000 0.999
    ## donate_skeptic          -0.042     0.048   -0.134    0.054       4000 1.001
    ## donate_frame:skeptic     0.015     0.069   -0.120    0.150       2947 1.001
    ## 
    ## Family Specific Parameters: 
    ##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
    ## sigma_justify    0.818     0.041    0.746    0.905       4000 1.000
    ## sigma_donate     0.989     0.049    0.899    1.087       4000 0.999
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
