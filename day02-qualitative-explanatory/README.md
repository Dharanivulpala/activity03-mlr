Day 2 - Assessing a MLR Model
================

In this repository/directory you should see two items:

- `README.md` - this document.

You will continue working in your `day01-fitting/activity03.Rmd` file
that you started on [Day 1](../day01-fitting).

## Task 1: Pull your changes from GitHub into RStudio

You successfully updated your GitHub repo from the main
[`README`](../README), but we still need to update your RStudio files.
Read these directions first, then work through them.

- In the **Git** pane of RStudio (upper right-hand area), locate and
  click on the
  <img src="../README-img/pull-icon.png" alt="knit" width = "20"/>
  **Pull** icon.
- After a few moments (you might need to provide your GitHub username
  and PAT), your **Files** pane will be updated with the new items.

Since you are likely looking at the `README` (this page) on GitHub,
nothing of importance for your work today was brought in. However, it is
always a good habit to **pull** from GitHub before starting to work
after taking a break. For example, if you were collaborating on a
project with others, they might do work at different times than you (or
even at the same time). It is always good to solve problems on your end
before **push**ing to GitHub and causing more problems.

## Some Important Questions

From [Section 3.2.2](https://hastie.su.domains/ISLR2/ISLRv2_website.pdf)
of *ISL* (p. 75), you read of a series important questions that we
should consider when performing multiple linear regression (copied here
for ease of access):

1.  Is at least one of the $p$ predictors $X_1$, $X_2$, $\ldots$, $X_p$
    useful in predicting the response $Y$?
2.  Do all the predictors help to explain $Y$, or is only a subset of
    the predictors useful?
3.  How well does the model fit the data?
4.  Given a set of predictor values, what response value should we
    predict and how accurate is our prediction?

We will circle back to these questions and update our modeling process
to include the above questions from *ISL* during Day 3. For the time
being, we are practicing including different types of data in our
models.

During Day 1, you fit a model with one quantitative response variable
and two quantitative explanatory variables. In this activity, we will
instead look at one quantitative explanatory variable and one
qualitative explanatory variable.

## Task 2: Overall model - is at least one predictor useful?

### Fitting the overall model

This is similar to what we have already been doing - fitting our desired
model. For today’s activity, we will fit something like:

$$
y = \beta_0 + \beta_1 \times \text{qualitative_variable} + \beta_2 \times \text{quantitative_variable} + \varepsilon
$$

where $y$, $\text{qualitative_variable}$, and
$\text{quantitative_variable}$ are from your data. Note that the two
explanatory variables can be entered in whatever order.

**I strongly recommend that you choose a qualitative explanatory
variable with only two levels (or force it to have only two levels).**
If your dataset does not meet this, get a hold of me so I can provide
you with a dataset that does.

- Open your `day01-quantitative-explanatory/activity03.Rmd` file and
  <img src="../README-img/knit-icon.png" alt="knit" width = "20"/>
  **knit** it to run the work you completed during Day 1 of this
  activity.

- Create a new R code chunk and type the following (replacing
  information as appropriate for your data), then run your code chunk or
  knit your document.

  ``` r
  # review any visual patterns
  data %>% 
    select(response, qualitative, quantitative) %>% 
    ggpairs()

  #fit the mlr model
  lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

  mlr_mod <- lm_spec %>% 
  fit(response ~ qualitative + quantitatative, data = data)

  tidy(mlr_mod)
  ```

When looking at your `ggpairs` output, remember to ask yourself, “does
it make sense to include all of these variables?” Specifically, if you
notice that the response variables are highly correlated (collinear),
including both does not necessarily add much value as they are
essentially saying the same thing. Note: There are more advanced methods
to include the variability within a rater for our model - this is beyond
STA 631. If this sounds of interest to you, explore *generalized
estimating equations* (GEE) or *generalized linear mixed models* (GLMM).
However, there are often times when we choose to include variables in
our model because it is important to us - for various reasons.
Regardless, I encourage you to keep your readings of *DF* in mind - who
will benefit by including this information; who will be hurt by
including this information?

Also, when looking at your model (`tidy`) output, the `term` label for
your qualitative explanatory variable look odd. Answer the following
questions:

1.  What is the label that R assigned to this explanatory variable
    `term`?
2.  What information is represented here?
3.  What information is missing here?

Your are essentially fitting two models (or $k$ models, where $k$ is the
number of levels in your qualitative variable). From your reading, you
learned that R is creating an indicator variable (see p. 83). If you
have 3 levels in your qualitative variable, you would have 2 (3 - 1)
indicator variables. If you have $k$ levels in your qualitative
variable, you would have $k - 1$ indicator variables.

The decision for R to call the indicator variable by one of your levels
instead of the other has no deeper meaning. R simply codes the level
that comes first alphabetically as a $0$ for your indicator variable.
You can change this reference level of a categorical variable, which is
the level that is coded as a 0, using the `relevel` function. Use
`?relevel` to learn more.

4.  For each level of your qualitative variable, write the simplified
    equation of the estimated line for that level. Note that if your
    qualitative variable has two levels, you should have two simplified
    equations.

The interpretation of the coefficients (parameter estimates) in multiple
regression is slightly different from that of simple regression. The
estimate for the indicator variable reflects how much more a group is
expected to be if something has that quality, *while holding all other
variables constant*. The estimate for the quantitative variable reflects
how much change in the response variable occurs due to a 1-unit increase
in the quantitative variable, *while holding all other variables
constant*.

5.  Interpret the parameter estimate for the reference level of your
    categorical variable in the context of your problem. Page 83 of the
    text can help here (or have me come chat with you).

6.  Interpret the parameter estimate for your quantitative variable in
    the context of your problem.

7.  Create a new model with the same response and quantitative
    explanatory variable, but now choose a qualitative variable with
    more than two (but, say, less than 5) levels and obtain the `tidy`
    model output. How does R appear to handle categorical variables with
    more than two levels?

## What is next?

We will explore interaction terms (Section 3.3.2) and ways to work
around problems that arise in linear regression models (Section 3.3.3)
while answering the important modeling questions (Section 3.2.2).
