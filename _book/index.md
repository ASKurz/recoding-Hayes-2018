--- 
title: "recoding *Introduction to Mediation, Moderation, and Conditional Process Analysis*"
author: ["A Solomon Kurz"]
date: "2018-07-15"
site: bookdown::bookdown_site
output: bookdown::gitbook
documentclass: book
link-citations: yes
github-repo: ASKURZ/recoding-Hayes-2018
twitter-handle: SolomonKurz
description: "This project is an effort to connect his Hayes’s conditional process analysis work with the Bayesian paradigm. Herein I refit his models with my favorite R package for Bayesian regression, Bürkner’s [brms](https://github.com/paul-buerkner/brms). I use syntax based on sensibilities from the [tidyverse](https://www.tidyverse.org) and plot with Wickham’s [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html)."
---

# Welcome {-}

Andrew Hayes’s *Introduction to Mediation, Moderation, and Conditional Process Analysis*  text, [the second edition of which just came out](http://afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html), has become a staple in social science graduate education. Both editions of his text have been from a frequentist OLS perspective. This project is an effort to connect his work with the Bayesian paradigm. Herein I refit his models with my favorite [R](https://www.r-bloggers.com/why-use-r-five-reasons/) package for Bayesian regression, Bürkner’s [brms](https://github.com/paul-buerkner/brms). I also prefer plotting with Wickham's [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), and recently converted to using [tidyverse](https://www.tidyverse.org)-style syntax (which you might learn about [here](http://r4ds.had.co.nz/transform.html) or [here](http://style.tidyverse.org)). But to be clear, this project is not meant to stand alone. It's a supplement to the textbook. 

**Disclaimer**: brms is a rapidly-evolving package. To get a sense, see how frequently Bürkner has updated some of the most [recent versions](https://cran.r-project.org/src/contrib/Archive/brms/). I also have a lot to learn as a Bayesian and as an R user. So some of the code may appear dated or inelegant. Which is all to say, *suggestions on how to improve my code are welcome*.

**If you’re totally new to Bayesian regression and HMC estimation**, you might want to look at [this paper](https://cran.r-project.org/web/packages/brms/vignettes/brms_overview.pdf), watch a few of [these](https://www.youtube.com/channel/UCNJK6_DZvcMqNSzQdEkzvzA/playlists), or even start with my other [repo](https://github.com/ASKurz/Statistical_Rethinking_with_brms_ggplot2_and_the_tidyverse). That said, you do not need to be totally fluent in Bayes or brms. Otherwise why would you need this project, anyway? IMO, the most important things are curiosity, a willingness to try, and persistent tinkering. I love this stuff. Hopefully you will, too.

Happy modeling!

The source code of the project is available [here](https://github.com/ASKurz/recoding_Introduction_to_Mediation_Moderation_and_Conditional_Process_Analysis). This project is powered by Yihui Xie's [bookdown package](https://bookdown.org) which makes it easy to turn R markdown files into HTML, PDF, and EPUB. You can learn more [here](https://bookdown.org/yihui/bookdown/).
