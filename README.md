
# Evaluating of machine learning and artificial intelligence algorithms (Data Train Course 2023)

<!-- badges: start -->
<!-- badges: end -->

## Contents

This repository contains course materials from the

<center>

**“Evaluating machine learning and artificial intelligence algorithms”**
</br> [Data Train Course
2023](https://www.bremen-research.de/data-train/courses/course-catalogue/course-details?event_id=45).

</center>
The rendered html slides can be accessed here:
<center>

**<https://maxwestphal.github.io/evaluation_in_ml_datatrain_2023/>**

</center>

Author: [**Max Westphal**](https://www.linkedin.com/in/maxwestphal/)
(<max.westphal@mevis.fraunhofer.de>)

Course instructors:

- Max Westphal
- Werner Brannath
- Pascal Rink

## For Course Participants

To prepare for the course, please download R and RStudio from
<https://posit.co/download/rstudio-desktop/> and then

- clone this repository:
  `git clone https://github.com/maxwestphal/evaluation_in_ml_cen_2023.git`
  \[terminal\],
- install renv: `install.packages("renv")` \[R\],
- install dependencies: `renv::restore()` \[R\].

While it is possible to work with another IDE, we recommend to use
RStudio.

In principle, hands-on assignments may also be tackled in Python instead
of R. However, solutions will only be supplied in R.

## License

This work is released under a [CC BY-SA
4.0](https://creativecommons.org/licenses/by-sa/4.0/) license.

## Issues

If you find an error or have suggestions for improvements, please create
a new issue here:

<https://github.com/maxwestphal/evaluation_in_ml_datatrain_2023/issues>

## Reproduction

If you are interested in reproducing the course materials (i.e. train
prediction models and produce the evaluation data), please conduct the
following steps:

- re-produce pre-computed results: `source("scripts/_run.R)` \[R\]
- render the file “slides.qmd”: `quarto::quarto_render()` \[R\]

## R Version Info

``` r
str(R.Version())
```

    ## List of 15
    ##  $ platform      : chr "x86_64-w64-mingw32"
    ##  $ arch          : chr "x86_64"
    ##  $ os            : chr "mingw32"
    ##  $ crt           : chr "ucrt"
    ##  $ system        : chr "x86_64, mingw32"
    ##  $ status        : chr ""
    ##  $ major         : chr "4"
    ##  $ minor         : chr "3.1"
    ##  $ year          : chr "2023"
    ##  $ month         : chr "06"
    ##  $ day           : chr "16"
    ##  $ svn rev       : chr "84548"
    ##  $ language      : chr "R"
    ##  $ version.string: chr "R version 4.3.1 (2023-06-16 ucrt)"
    ##  $ nickname      : chr "Beagle Scouts"
