--- 
title: "Recoding *Introduction to Mediation, Moderation, and Conditional Process Analysis*"
subtitle: "version 1.1.0"
author: ["A Solomon Kurz"]
date: "2019-12-20"
site: bookdown::bookdown_site
output: bookdown::gitbook
documentclass: book
geometry:
  margin = 0.5in
urlcolor: blue
highlight: tango
header-includes:
  \usepackage{underscore}
  \usepackage[T1]{fontenc}
link-citations: yes
github-repo: ASKURZ/recoding-Hayes-2018
twitter-handle: SolomonKurz
description: "This project is an effort to connect his Hayes's conditional process analysis work with the Bayesian paradigm. Herein I refit his models with my favorite R package for Bayesian regression, Bürkner's brms, and use the tidyverse for data manipulation and plotting."
---

# Welcome {-}

Andrew Hayes's *Introduction to Mediation, Moderation, and Conditional Process Analysis*  text, [the second edition of which just came out](http://afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html), has become a staple in social science graduate education. Both editions of his text have been from a frequentist OLS perspective. This project is an effort to connect his work with the Bayesian paradigm. Herein I refit his models with my favorite [**R**](https://www.r-bloggers.com/why-use-r-five-reasons/) package for Bayesian regression, Bürkner's [**brms**](https://github.com/paul-buerkner/brms). I also prefer data wrangling and plotting using the [**tidyverse**](https://www.tidyverse.org), which you might learn about [here](http://r4ds.had.co.nz/transform.html) or [here](http://style.tidyverse.org)). But to be clear, this project is not meant to stand alone. It's a supplement to the textbook. 

The source code of the project is available [here](https://github.com/ASKurz/recoding_Introduction_to_Mediation_Moderation_and_Conditional_Process_Analysis). This project is powered by the great [Yihui Xie](https://twitter.com/xieyihui)'s [**bookdown** package](https://bookdown.org) which makes it easy to turn **R** markdown files into HTML, PDF, and EPUB. You can learn more [here](https://bookdown.org/yihui/bookdown/). While you're at it, also check out [this great guide to R Markdown](https://bookdown.org/yihui/rmarkdown/).

## Why this? {-}

I'm not a statistician and I have no formal background in computer science. But I met a great [statistics mentor](https://pharmacy.olemiss.edu/blog/team/dr-john-p-bentley/) in grad school who was enthusiastic, knowledgeable, and very generous with his time. In one of his stats electives, we used Hayes's first edition text and I learned a lot in that semester. 

Yet a large portion of my training has been out of the classroom, working with messy real-world data, and looking online for help. One of the great resources I happened on was [idre, the UCLA Institute for Digital Education](https://stats.idre.ucla.edu). They perform a variety of services, but I benefited the most from was their portfolio of [richly annotated textbook examples](https://stats.idre.ucla.edu/other/examples/). Their online tutorials are among the earliest inspirations for this project. More so than my old statistics seminar lecture notes, high-quality and freely-available resources like this are where most of my day-to-day data analysis skills come from. We need more resources like this.

Hayes's work has become influential in many corners of the academy, including my own--psychology. His PROCESS macro has been aimed at SPSS and SAS users, which is understandable given their popularity in the social sciences. Over the past few years, I've moved away from proprietary programs like SPSS to **R**. **R** free, open source, and I find it a more flexible and useful tool for data analysis. In fairness, Hayes expanded his second edition to include **R** code, which is a great move forward. But his work is done from a frequentist OLS perspective and there have been a lot of exciting developments in the world of applied Bayesian statistics. If you're an **R** user and want to learn about Bayesian data analysis, I think Bürkner's [**brms**](https://github.com/paul-buerkner/brms) is the best package around. It's flexible, uses reasonably-approachable syntax, has sensible defaults, and offers a wide array of post-processing convenience functions. In addition, the **R** code in Hayes's second edition does not leverage the power of the [**tidyverse**](https://www.tidyverse.org). The purpose of this project is to connect Hayes's insights into regression with the Bayesian paradigm. We'll do so within the free and open-source **R** ecosystem, highlighting the Bayesian **brms** package, and using functions from the **tidyverse** and friends (e.g., [**tidybayes**](https://mjskay.github.io/tidybayes/index.html)) to streamline our code. 

## My assumptions about you {-}

If you're looking at this project, I'm guessing you're either a graduate student or a post-graduate academic or researcher of some sort, which comes with the presumption you have at least a 101-level foundation in statistics. If you're rusty, check out Legler and Roback's free **bookdown** text, [*Broadening Your Statistical Horizons*](https://bookdown.org/roback/bookdown-bysh/) or Navarro's free [*Learning Statistics with R*](https://learningstatisticswithr.com/). I'm also presuming you are at least vaguely familiar with Bayesian statistics. For those totally new to Bayesian regression and HMC estimation, look at [this paper](https://cran.r-project.org/web/packages/brms/vignettes/brms_overview.pdf), watch a few of [these engaging lectures](https://www.youtube.com/channel/UCNJK6_DZvcMqNSzQdEkzvzA/playlists), or even start with my other [project](https://github.com/ASKurz/Statistical_Rethinking_with_brms_ggplot2_and_the_tidyverse) based on [this excellent text](https://xcelab.net/rm/statistical-rethinking/). I'm also presuming a basic working fluency in **R** and a vague idea about what the **tidyverse** is. If you're totally new to **R**, consider starting with Peng's [*R Programming for Data Science*](https://bookdown.org/rdpeng/rprogdatascience/). And the best introduction to the **tidyvese**-style of data analysis I've found is Grolemund and Wickham's [*R for Data Science*](http://r4ds.had.co.nz).

That said, you do not need to be totally fluent in statistics or **R**. Otherwise why would you need this project, anyway? IMO, the most important things are curiosity, a willingness to try, and persistent tinkering. I love this stuff. Hopefully you will, too.

## How to use and understand this project {-}

This project is not meant to stand alone. It's a supplement to the second edition of [Hayes's text](http://afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html). I follow the structure of his text, chapter by chapter, translating his analyses into **brms** and **tidyverse** code. However, many of the sections in the text are composed entirely of equations and prose, leaving us nothing to translate. When we run into those sections, the corresponding sections in this project will be sparse or blank. I imagine students might reference this project as they progress through Hayes's text. I also imagine working data analysts might use this project in conjunction with Hayes's text as they flip to the specific sections that seem relevant to solving their data challenges. 

I reproduce the bulk of the figures in the text, too. The plots in the first few chapters are the closest to those in the text. However, I'm passionate about data visualization and like to play around with [color palettes](https://github.com/EmilHvitfeldt/r-color-palettes), formatting templates, and other conventions quite a bit. As a result, the plots in each chapter have their own look and feel. I also like to highlight some of the unique strengths Bayesian analyses brings to the table, such as the ease with which you can depict uncertainty with density plots, interval ribbons around regression lines, and spaghetti plots. For more on some of these topics, chapters [3](http://r4ds.had.co.nz/data-visualisation.html), [7](http://r4ds.had.co.nz/exploratory-data-analysis.html), and [28](http://r4ds.had.co.nz/graphics-for-communication.html) in *R4DS*, Healy's [*Data Visualization: A practical introduction*](https://socviz.co), or Wickham's [*ggplot2: Elegant Graphics for Data Analysis*](https://ggplot2-book.org/).

In this project, I use a handful of formatting conventions gleaned from [*R4DS*](http://r4ds.had.co.nz/introduction.html#running-r-code) and [*R Markdown: The Definitive Guide*](https://bookdown.org/yihui/rmarkdown/software-info.html). 

* I put **R** and **R** packages (e.g., **tidybayes**) in **boldface**.
* **R** code blocks and their output appear in a gray background. E.g., 


```r
2 + 2
```

```
## [1] 4
```

* Functions are in a typewriter font and followed by parentheses, all atop a gray background (e.g., `brm()`).
* When I want to make explicit what packages a given function comes from, I insert the double-colon operator `::` between the package name and the function (e.g., `tidyr::gather()`).
* **R** objects, such as data or function arguments, are in typewriter font atop a gray background (e.g., `d` or `size = 2`).
* Hyperlinks are denoted by their typical [blue-colored font](https://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html).

## Times change and so do we {-}

I released the 1.0.0 version of this book in July of 2018. This 1.1.0 version includes a lot of changes. Most notably,

* all models have been refit with **brms** 2.10.3,
* the introduction to Bayesian information criteria now reflects the updates introduced in **brms** 2.10.0,
* I have updated the multivariate model code with the `mvbind()` and `mvbf()` functions,
* I have made greater use of the handy **tidybayes** package for post-processing models,
* I have updated and simplified a lot of the **tidyverse** code,
* all fit objects have sequential names and are now saved in `.rda` files in the [`fits` folder](https://github.com/ASKurz/recoding-Hayes-2018/tree/master/fits) on GitHub,
* I have substantially increased the amount of Hayes quotes,
* I added more statistical formulas throughout the text,
* I added a version of Chapter 14, which includes warnings about relying on cross-sectional mediation, and
* the various **R** objects enjoy simpler and more standardized names.

It's unclear what future versions of this book may hold. Candidates include

* more talk about model priors,
* examples of multilevel mediation,
* examples of longitudinal mediation, and
* sterner warnings against cross-sectional mediation.

We'll see. In the meantime, feel free to make suggestions or report typos and coding goofs [here](https://github.com/ASKurz/recoding-Hayes-2018/issues).

