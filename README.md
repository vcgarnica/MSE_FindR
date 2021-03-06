# MSE FindR Tutorial

`MSE FindR` is a user-friendly R shiny web app tool to estimate the pooled variance ($S_p^2$; i.e., $MSE$) from ANOVA experiments using information such as treatment means, $\alpha$ significance level, number of replicates, and post hoc test results in balanced, randomized trials. The application is hosted at [https://garnica.shinyapps.io/MSE_FindR/](https://garnica.shinyapps.io/MSE_FindR/).

The tool expands on concepts published in [Ngugi et al., 2011](https://apsjournals.apsnet.org/doi/abs/10.1094/PHYTO-08-10-0221) by incorporating additional post hoc tests (Tukey’s $HSD$, Bonferroni and Šidák correction for multiple comparisons, and Scheffé’s test obtained from `agricolae`, `emmeans`, and `multcomp` R packages) and a variety of experimental designs commonly used in agricultural sciences. 


`MSE FindR` comprises four main modules (Disclosure, Upload file, Separate means and letters, and Estimator) arranged in an intuitive interface that guides the user through the analysis (Figure 1). 

![legend](https://github.com/vcgarnica/MSE_FindR/blob/main/Paper/diagram.png)
Figure 1. MSE FindR conceptual flowchart highlighting the inputs and estimation for randomized complete block designs analyzed with Fisher's LSD test.

In its current version, the tool supports the estimation of $\hat\sigma^2$ for randomized, balanced trials arranged in the following experimental designs:

* Completely randomized design (CRD)
* Randomized complete block design (RCBD)
* Latin Square
* Two-way complete factorial as CRD
* Two-way complete factorial as RCBD
* Split-plot arranged as CRD
* Split-plot arranged as RCBD

and post hoc tests:

* Fisher's $LSD$
* Tukey's $HSD$
* Bonferroni correction for multiple comparisons
* Šidák correction for multiple comparisons
* Scheffé's test


## `MSE FindR`: Disclosure

Contains a walk-through tutorial, downloadable example files, information about developers, and reference for citation.

## `MSE FindR`: Upload csv file

Only standard CSV formats are accepted by the application. The default is comma separator and double quote and settings can be modified to match the data column separation form.


## `MSE FindR`: Separate means and letters

This module explores potential caveats during data management that can be addressed by `MSE FindR` functionalities. Often, treatment means and post hoc test results are combined in the same column of a CSV file. This is a common issue when the original trial report is available as a PDF file and users convert it to XLSX to optimize data imputation time. Additionally, there have been situations where authors report abbreviated post hoc letters (ex. “a-c” instead of “abc” or “f-j” instead of “fghij”) that could be expanded for more clarity on statistically similar results within a trial.

![Legend](https://github.com/vcgarnica/MSE_FindR/blob/main/Paper/sep.png)
Figure 2. Column 'y' before and after treatment via separate means and letters module. Note new clean_means and clean_letters columns.


`MSE FindR` can handle both issues by splitting a single treatment mean and post hoc test column and expanding abbreviated letters into two new columns, named clean_means (numerical) and clean_letters (categorical) (Figure 2). The task is achieved via the separate button in the module Separate means and letters. Noteworthy, the newly created columns should be selected in future steps in $\hat\sigma^2$ estimation.


## `MSE FindR`: Estimator

### Input

When preparing the CSV input file to be uploaded into `MSE FindR`, all trials assigned must share the same *experimental design*, *post hoc test*, $\alpha$ *significance level*, and *treatment structure*. Incorrect $\hat\sigma^2$ estimations can result from assigning trials with incompatible information to the same CSV file.

Additionally, the number of columns required for $\hat\sigma^2$ estimation differs between experimental designs. Consider a Latin square design, for example. In this design, the number of columns, rows, and treatments is equal and there is only one factor of interest under evaluation. Therefore, the CSV input file should contain at least the trial identifier number, treatment list, means, and post hoc test results columns. For other one-way designs such as CRD or RCBD, the amount of information for $\hat\sigma^2$ estimation depends also on the number of replicates or blocks, which is not necessarily equal to the number of treatment levels. Therefore, compared to the Latin square design, the CSV input files for CRDs and RCBDs must include an additional column containing the number of replicates or blocks for each trial. Further information is required when estimating $\hat\sigma^2$ from trials with more complex experimental designs, such as two-way factorials or split-plot designs. In these scenarios, users are interested in obtaining $\hat\sigma^2$ for either factor A, B, or the interaction A × B. 

When the collection of scientific reports or studies is arranged as a two-way factorial design and contains only means and post hoc tests for a single factor, herein called factor A, users must include information concerning the **number of levels for the omitted factor**, herein called factor B. This is often the case when A × B interaction is not significant in a study and authors include means and post hoc test results for only one factor. Conversely, in scenarios where users are interested in estimating $\hat\sigma^2$ for trials with significant A × B interaction and both means and post hoc results are reported, two columns respective to factors A and B should be included for all trials in the input CSV file.

Basic knowledge about experimental designs shows that one-way designs (CDR and RCBD) and two-way factorials are structurally similar because there is only one error variance in ANOVA. That is not the case with split-plot designs in which involve at least two error terms (see, [Montgomery, 2017](https://www.google.com/books/edition/Design_and_Analysis_of_Experiments/Py7bDgAAQBAJ?hl=en&gbpv=0)). Five distinct $\hat\sigma^2$ estimations could arise from split-plot designs, depending on the results and treatment structure of manuscripts at hand:

When the A × B interaction is not significant:

1. Estimation of $\hat\sigma^2$ for main plot level (herein called factor A – main plot)
2. Estimation of $\hat\sigma^2$ for subplot level (herein called factor B – subplot)

When A × B interaction is statistically significant:

3. Estimation of $\hat\sigma^2$ for subplot level within a main plot level (B within A)
4. Estimation of $\hat\sigma^2$ for main plot level within subplot level (A within B)
5. Estimation of $\hat\sigma^2$ for subplot level across different main plot level (A × B)

`MSE FindR` is able to estimate $\hat\sigma^2$ for scenarios 1, 2, and 3. Noteworthy, $\hat\sigma^2$ estimation in 5 consists of a weighted average between $\hat\sigma^2_A$ (main plot error) and $\hat\sigma^2_B$ (subplot error), which is considerably more complex to estimate than other scenarios. 

Example files pertaining to the minimum required information for major experimental designs are available within the Shiny application. Users are encouraged to download example files and familiarize themselves with the required information before proceeding with CSV input file assembly.


### Selections

In the Estimator module, select columns box, users can interactively choose columns from the uploaded CSV file to match specific criteria required by the tool. Erroneous assignment of columns in this step will result in incorrect $\hat\sigma^2$ estimations. 

For Latin square designs, the CSV input file should contain the following columns, named at user's preferences:

* **Unique trial identifier number:** contains trial identification number — allows $\hat\sigma^2$ estimation for multiple trials simultaneously. 
* **Factor A:** represents the treatment list for factor A – either numerical or categorical.
* **Means:** represents treatment means – numeric.
* **Post hoc test letters:** contains letters of post hoc tests – categorical.

For one-way CRD and RCBD designs, besides columns mentioned above, the following is one should be included in the input file:

* **Number of replicates:** represents the total number of replicates or blocks for each trial — assumes a balanced trial (i.e., the same number of replicates for each treatment) – numerical.

As mentioned before, in two-way complete designs, there are two factors of interest in ANOVA and authors can either report information about A, B, or A × B interaction. This selection is done via:

* **Source of variation:** represents treatment structure of studies. Select A when only means for one factor are reported and A × B when the interaction is reported. In case A is selected, users must specify a column with the factor of interest (A) and the number of levels for the committed factor (B).
* **Number of levels of factor B:** represents the number of levels for factor B. – numerical.

Alternatively, when A × B interaction means and post hoc tests are reported, users must specify a column for factor B via:

* **Factor B:** represents the treatment list for factor B – either numerical or categorical.

For split-plot designs, in addition to the source of variation selection tab, a level selection field is displayed:

* **Level:** represents the level (main or subplot) when only a single factor is reported. Users must select one of the options to distinguish $\hat\sigma^2$ estimation between 3 and 4. 

Conversely, when the A × B interaction is statistically significant, both factors are present in the trial report and should be included as factor A and factor B columns in the CSV input file. Changing the **source of variation** from A to B within A will result in the respective selection fields, which must follow the structure of A being the main plot level and B the subplot level, respectively.

After column selections have been made and the estimate button pressed, a download button along with the results window will pop up. Users can export the file containing `MSE FindR` estimates of degrees of freedom and $\hat\sigma^2$ for each trial along with information previously included in the original CSV file. Obs.: double-check the degrees of freedom for the residual by hand for a few trials to ensure reliable results. At this point, users should know that only one $\hat\sigma^2$ value should be estimated for each trial.


# Rationale

In section, we explore the mechanisms of $\hat\sigma^2$ estimation via `MSE FindR`. As previously discussed, `MSE FindR` computes two finite boundaries (the largest non-significant and the smallest significant differences) for all mean pairwise combinations of treatments and averages those values for a parameter called $ELSD$ ([Ngugi et al., 2011](https://apsjournals.apsnet.org/doi/abs/10.1094/PHYTO-08-10-0221)). 

Table 1 outlines formulas used to obtain $\hat\sigma^2$ for one-way designs with the $n.replicates$ consisting of a unique number of repetitions or blocks per trial, $ELSD$ consisting of the parameter estimated by the algorithm, $\alpha$ consisting of the significance level, $n.levels.A$ consisting of the number of levels in factor A, $m$ consisting of the total number of null hypotheses or comparisons per trial, and $df_{error}$ consisting of the degrees of freedom for $\hat\sigma^2$. $m$ is given by $\binom{n.levels.A}{2}$.


Table 1. Expressions for estimation of $\hat\sigma^2$ based on post hoc test for one-way factor designs.

| Post hoc test          | Expression                                                                                   |
| ---------------------- | ------------------------------------------------------------------------------------------- |
| Fisher's LSD           | $\hat\sigma^2 = 0.5 \cdot n.replicates \cdot \biggl(\frac{ELSD}{qt(1-\alpha/2,df_{error})}\biggr)^2$ |
| Tukey's HSD            | $\hat\sigma^2 = n.replicates \cdot \biggl(\frac{ELSD}{qtukey(1-\alpha,n.levels.A,df_{error})}\biggr)^2$ |
| Bonferroni adjustment  | $\hat\sigma^2 = 0.5 \cdot n.replicates \cdot \biggl(\frac{ELSD}{qt(1-(\alpha/2m),df_{error})}\biggr)^2$ |
| Šidák adjustment       | $\hat\sigma^2 = 0.5 \cdot n.replicates \cdot \biggl( \frac{ELSD}{qt(1-(1-\alpha)^{1/m},df_{error})}\biggr)^2$ |
| Scheffé's              | $\hat\sigma^2 = \frac{n.replicates \cdot ELSD^2}{2 \cdot (n.level.A-1) \cdot qf(1-\alpha,n.level.A-1,df_{error})}$ |

Similar expressions are used for $\hat\sigma^2$ estimation in two-way designs, except that they include the number of levels for factor B, which are either reported by the user (via number of levels of factor B column) or calculated by the app directly (from factor B column) from the CSV input file.

## References

Ngugi, H.K., Lehman, B.L. and Madden, L.V., 2011. Multiple treatment meta-analysis of products evaluated for control of fire blight in the eastern United States. Phytopathology, 101(5), pp.512-522.

Montgomery, D.C., 2017. Design and analysis of experiments. John wiley & sons.

