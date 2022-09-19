# MSE FindR Tutorial

## Introduction

Meta-analysis is a methodology used to synthesize the results of multiple, independent studies to provide an overall estimate of a treatment effect along with a measure of its precision. In the absence of individual participant data (a.k.a. raw data), standard meta-analytic approaches on continuous outcomes rely on reported summary metrics, such as the mean and a corresponding measure of variability (e.g., the sample standard deviation and standard error values for each treatment group). This is because effect sizes in meta-analyses are normally weighted by study precisions, most commonly by the inverse of study variance. A common complication arises when none of these summary variability metrics are explicitly included in the primary studies.

`MSE FindR` is a user-friendly R shiny web app tool developed to help researchers obtain a measurement of variability from studies lacking summary metrics, and that, instead, report treatment means, alpha significance level ($\alpha$), number of replicates, and post hoc test results. `MSE FindR` estimates the pooled variance (i.e., MSE; the estimator of population variance $\sigma^2$, and thus $\hat\sigma^2$) in replicated, randomized studies analyzed with ANOVA, conditional that trials are balanced (i.e., the same number of replicates per treatment). Once the MSE has been estimated, one can then calculate the variability measurements required in meta-analysis studies.

The application is hosted at [https://garnica.shinyapps.io/MSE_FindR/](https://garnica.shinyapps.io/MSE_FindR/).

`MSE FindR` expands on concepts published in [Ngugi et al., 2011](https://apsjournals.apsnet.org/doi/abs/10.1094/PHYTO-08-10-0221) by incorporating additional post hoc tests (a.k.a. multiple comparison tests obtained from `agricolae`, `emmeans`, and `multcomp` R packages) and a variety of experimental designs commonly used in the agricultural sciences. 

In its current version, `MSE FindR` supports the estimation of $\hat\sigma^2$ for the following experimental designs:

* Completely randomized design (CRD)
* Randomized complete block design (RCBD)
* Latin Square
* Two-way complete factorial as CRD
* Two-way complete factorial as RCBD
* Split-plot arranged as CRD
* Split-plot arranged as RCBD

And post hoc tests:

* Fisher's LSD
* Tukey's HSD
* Bonferroni correction for multiple comparisons
* Šidák correction for multiple comparisons
* Scheffé's test


`MSE FindR` comprises four main modules (Disclosure, Upload file, Separate means and letters, and Estimator). These modules aim to help users navigate through the tool functionalities and conveniently obtain an estimate of $\hat\sigma^2$ for multiple trials at a time.

`MSE FindR` uses an intuitive interface that guides the user through the analysis (Figure 1). 

![legend](https://github.com/vcgarnica/MSE_FindR/blob/main/Images/workflow.png)
Figure 1. MSE FindR conceptual flowchart highlighting the inputs and estimation for randomized complete block designs analyzed with Fisher's LSD test.


## `MSE FindR`: Disclosure

Contains a walk-through tutorial, downloadable example files, information about developers, and citation.


## `MSE FindR`: Upload CSV file

In this module, users upload the CSV file containing the trial-specific information, which may vary depending on the experimental design and treatment structure of compiled studies. Instructions on how to collate trials into the CSV input file are presented below. Only standard CSV formats are accepted by the application with default file configurations set with comma separators and double quotes. Settings can be modified to match the CSV data file.


## `MSE FindR`: Separate means and letters

Occasionally, treatment means and post hoc test results are combined in the same column. This is a common issue, for example, when the original trial report is available as a PDF file and users convert it to XLSX to optimize data digitalization. Additionally, there have been situations where post hoc letter results are reported in an abbreviated form (ex. “a-c” instead of “abc” or “f-j” instead of “fghij”) (Figure 2).


![Legend](https://github.com/vcgarnica/MSE_FindR/blob/main/Images/sep.png)
Figure 2. Column 'y' before and after treatment via separate means and letters module. Note the newly created clean_means and clean_letters columns.


This optional module is equipped with a function that enables users to split a single column containing both treatment means and post hoc test results into two new columns, named ‘clean_means’ and ‘clean_letters’. This function can also simultaneously expand abbreviated letters into a complete list of elements (Figure 2). Note that the newly created columns (‘clean_means’ and ‘clean_letters’) should be selected in the next steps of $\hat\sigma^2$ estimation if the user opt to use this function.


## `MSE FindR`: Estimator

After the CSV file has been pre-processed according to `MSE FindR` tutorial and uploaded into the application, users may proceed with $\hat\sigma^2$ estimation in this module by assigning the columns of CSV input file, named at the user's preferences, to the respective selection fields in this module. Proper assignment of columns to the selection fields is a critical step for correct $\hat\sigma^2$ estimation. Following $\hat\sigma^2$ estimation via the estimate button, a drop-in download button will appear where `MSE FindR` results can be exported as a CSV file (Figure 1). `MSE FindR` obtains a trial-specific estimate of $\hat\sigma^2$ and its respective degrees of freedom.


## Procedure

### Step 1 - Compile trials with the same information

First, users must only assign trials with the same experimental configuration, more specifically, the same **experimental design**, **post hoc test**, **$\alpha$ significance level**, and **treatment structure (for two-way designs)** to the CVS input file (Figure 1). Incorrect $\hat\sigma^2$ estimations can result from assigning trials with incompatible information to the same CVS file (Figure 1). 

*Example 1*: A systematic reviewer collects ten trial reports investigating the effect of four commercially available fertilizers, with yield as response. Half of these trials were analyzed with Fisher's LSD and the other half were analyzed with Tukey's HSD. In addition, two trials of Tukey's group were arranged as CDR and the remaining three were arranged as RCBD. In this case, users must separate trials in three different folders, one for Fisher's LSD, another for Tukey's HSD arranged as CRD, and a third for Tukey's HSD arranged as RCBD, assuming all the other information is the same. This is an example where the **experimental design** and **post hoc test** differed between trials. Each folder will have its CSV input file.

*Example 2*: It was later found that four trials in the Fisher's group were analyzed with $\alpha$ set to 0.05 and one trial was analyzed with $\alpha$ set to 0.10. Then, a fourth folder is needed, instead of three, to account for the difference in **$\alpha$ significance level** among Fisher's trials.  

*Example 3*: The systematic reviewer selected five independent trial reports containing each plant experiment arranged as a two-way factorial RCBD and testing combinations of six-to-eight commercially available fertilizers and three-to-five soybean cultivars, with yield as response. Note that, the original authors may choose to publish results for either fertilizer (factor A), cultivar (factor B), or the interaction (A × B), depending on research goals and ANOVA results. The systematic reviewer is interested in A and while two trial reports contain means/post hoc tests for A, the remaining three report means of A × B interaction. This is a scenario where the experimental design is the same but the **treatment structure** of trials at hand is different. Two folders, one for each treatment structure, must be created, assuming all the information is the same. We will discuss what information is needed between different treatment structures at later stages.

*Example 4*: The systematic reviewer now selects two nearly identical trial reports with plant experiments arranged as a split-plot RCBD testing combinations of four-to-five fertilizers (factor A) and three-to-five cultivars (factor B), with plant height as response. Notably, the trial reports have been published by the same authors and only means (and post hoc test results) for factor A are available. Factor B and A × B were omitted because no significant effects were observed during ANOVA. All the other experimental information is the same, except that A treatments have been allocated to main plots in the first trial report and to subplots in the second trial report. Conversely, B treatments have been allocated to subplots and main plots for the first and second trials, respectively. This is a scenario where treatments are reported similarly but are still **structurally** different, and thus, trials must be compiled separately. 

Therefore, users must compile trial reports that share the same *experimental design*, *post hoc test*, $\alpha$ *significance level*, and *treatment structure (for two-way designs)* configuration. 


### Step 2 - Determine information needed for CSV input file collation

As one may anticipate, the number of columns required for $\hat\sigma^2$ estimation by `MSE FindR` may differ between experimental designs. For example, consider a Latin square design, the simplest experimental design available in `MSE FindR`. In this design the number of columns, rows, and treatments is equal and there is only one treatment factor under evaluation (see, [Montgomery, 2017](https://www.google.com/books/edition/Design_and_Analysis_of_Experiments/Py7bDgAAQBAJ?hl=en&gbpv=0)), herein called factor A. Therefore, `MSE FindR` will count the number of treatments and automatically assign that value for the number of columns and rows, so the number of degrees of freedom for the error variance can be calculated. 

Alternatively, for other one-way designs such as CRD or RCBD, the information required for $\hat\sigma^2$ estimation depends also on the number of replicates or blocks, which is not necessarily equal to the number of treatment levels. Therefore, the CSV input files for CRDs and RCBDs must also include the number of replicates or blocks for each trial. `MSE FindR` will count the number of distinct treatments, once the column has been correctly specified in the tool, and compute the correct number of degrees of freedom based on both replicates/blocks and treatment numbers. 

Additional information is required when estimating $\hat\sigma^2$ from trials arranged in more complex experimental designs, such as two-way factorials or split-plot designs. In these scenarios, original researchers wanted also to investigate a second treatment factor, herein called factor B, which may or may not interact (significantly) with A, and therefore, may or may not be included in the original publication. When the collection of scientific reports or studies is arranged as two-way factorial or split-plot designs and only includes means and post hoc results for A, users must provide a column in the CSV input file with the **number of levels for the omitted factor B**. Alternatively, if A × B means have been reported, then the systematic reviewer must list A and B treatment columns independently in the CSV input file. Note: more precise $\hat\sigma^2$ estimations are observed when information includes A × B interaction rather than A levels only, in two-way designs. Additionally, note that in factorial designs, A and B can be assigned interchangeably, meaning any treatment factor can be named A or B – which is not the case with split-plot designs because of the hierarchical structure present in split-plot designs.

There are some similarities in ANOVA of two-way factorial designs and CRDs and RCBDs, being the presence of a single error term describing the variance of all treatments, which differs from split-plot designs where at least two error terms are present, one for factors assigned to the main plot and a second for subplot factors (see, [Montgomery, 2017](https://www.google.com/books/edition/Design_and_Analysis_of_Experiments/Py7bDgAAQBAJ?hl=en&gbpv=0)). Knowing whether A, the factor of interest, was placed in the main or subplot levels matters because it affects how the number of degrees of freedom of $\hat\sigma^2$ is calculated. Five distinct $\hat\sigma^2$ estimations could arise, depending on whether factor A is allocated on main versus subplot levels and the presence/absence of significant interactions:

When the A × B interaction **is not significant** in primary studies:

1. Estimation of $\hat\sigma^2$ for A in the main plot level
2. Estimation of $\hat\sigma^2$ for A in the subplot level

When A × B interaction **is statistically significant** in primary studies:

3. Estimation of $\hat\sigma^2$ for A in the subplot level within the main plot level (A within B)
4. Estimation of $\hat\sigma^2$ for A in the main plot level within the subplot level (B within A)
5. Estimation of $\hat\sigma^2$ for A in the subplot level across different main plot levels (A × B)

`MSE FindR` is able to estimate $\hat\sigma^2$ for scenarios 1, 2, and 3. For 5, $\hat\sigma^2$ estimation consists of a weighted average between $\hat\sigma^2_A$ (main plot error) and $\hat\sigma^2_B$ (subplot error), which is considerably more complex to carry. The collation of trials into the input CSV file follows the same systematic idea of two-way factorial designs, however, for split-plot designs, users need to be mindful (and later correctly specify such information) of whether factor A was assigned to main or subplot levels. Incorrect $\hat\sigma^2$ results from assigning factor A to subplot level when it was supposed to be assigned to the main plot level and vice-versa. 

We go into detail about the information needed for each experimental design below and later on how to specify experimental configurations in the tool. Downloadable example files are also available to aim in the collation of trials to the CSV input file.


### Step 3 - Create a CSV file

After trial reports with similar experimental configurations have been organized, then it is time to collate them into the CSV input file. This is accomplished by defining what `MSE FindR` needs, based on the *experimental design* and *treatment structure (for two-way designs)*. There is no need to include the post hoc test, experimental design, and $\alpha$ significance level as columns in the CSV file because that information will be provided at later stages (Step 4) via field selections (Figure 1).

#### 3.1. Latin square

The CSV input file should contain columns specifying the trial identifier number (*trial_id*), treatment list for factor A (*fertilizer*), treatment means (*yield*), and the post hoc test results (*letters*) columns, named at user's preferences:

* **Unique trial identifier number:** contains trial identification number — numerical. Allows $\hat\sigma^2$ estimation for multiple trials simultaneously.
* **Factor A:** represents the treatment list for factor A – either numerical or categorical.
* **Means:** represents treatment means – numeric.
* **Post hoc test letters:** contains letters of post hoc tests – categorical.


Table 1. Example of columns in the CSV file for $\hat\sigma^2$ estimation by `MSE FindR`in Latin square designs. 

| trial_id | fertilizer |  yield  | letters|
|:---------|-----:|----:|-------:|
|1         | A    | 60.2|  a     |
|1         | B    | 45.2|  c     |
|1         | C    | 55.4|  b     |
|1         | D    | 40.0|  b     |
|2         | A    | 58.2|  a     |
|2         | B    | 57.2|  a     |
|2         | C    | 55.4|  a     |
|...       | ...  | ... | ...    |


#### 3.2. One-way CRD and RCBD designs

For one-way CRD and RCBD designs, in addition to the columns mentioned above, a replicate or block column (*rep*) should be included in the CSV input file:

* **Number of replicates:** represents the total number of replicates or blocks for each trial — assumes a balanced trial (i.e., the same number of replicates for each treatment) – numerical.

Table 2. Example of columns in the CSV file for $\hat\sigma^2$ estimation by `MSE FindR`in completely randomized and randomized complete block designs. 

| trial_id | fertilizer | rep |  yield  | letters|
|:---------|-----:|----:|----:|-------:|
|1         | A    | 3   | 60.2|  a     |
|1         | B    | 3   | 45.2|  c     |
|1         | C    | 3   | 55.4|  b     |
|1         | D    | 3   | 40.0|  b     |
|2         | A    | 5   | 58.2|  a     |
|2         | B    | 5   | 57.2|  a     |
|2         | C    | 5   | 55.4|  a     |
|...       | ...  | ... | ... |  ...   |


#### 3.3. Two-way factorial and split plot designs

As mentioned earlier, additional information is required when estimating $\hat\sigma^2$ from trials with more complex experimental designs, such as two-way factorials or split-plot designs. In these scenarios, users are interested in obtaining $\hat\sigma^2$ for either factor A or the interaction A × B.

##### 3.3.1. *One treatment factor omitted*

When the collection of scientific reports or studies is arranged as two-way factorial or split-plot designs and contains only means and post hoc results for factor A (fertilizer), users must include the list treatment for A and the **number of levels for the omitted factor B** (cultivar) (Table 3). This is often the case when A × B interaction is not significant in a primary study and authors decide to omit insignificant results from the original publication. In this case, add a column in the CSV input file with the number of levels for the omitted factor B:

* **Number of levels of factor B:** represents the number of levels for factor B – numerical.


Table 3. Example of columns in the CSV file for $\hat\sigma^2$ estimation by `MSE FindR`in factorial designs with one factor omitted. 

| trial_id | fertilizer | n.cultivars | rep |  yield  | letters|
|:---------|-----:|--------:|----:|----:|-------:|
|1         | A    |   5     |  3   | 60.2|  a     |
|1         | B    |   5     |  3   | 45.2|  c     |
|1         | C    |   5     |  3   | 55.4|  b     |
|1         | D    |   5     |  3   | 40.0|  b     |
|2         | A    |   4     |  5   | 58.2|  a     |
|2         | B    |   4     |  5   | 57.2|  a     |
|2         | C    |   4     |  5   | 55.4|  a     |
|...       | ...  | ... | ... | ... |  ...   |


##### 3.3.2. *Both treatment factors present*

Conversely, in scenarios where users are interested in estimating $\hat\sigma^2$ for trials with a significant A × B interaction, two independent columns respective to factors A and B should be included for all trials in the input CSV file (Table 4). In scenarios where A × B interaction means are reported, users must specify a column for factor B treatment list via:

* **Factor B:** represents the treatment list for factor B – either numerical or categorical.

Table 4. Example of columns in the CSV file for $\hat\sigma^2$ estimation by `MSE FindR`in factorial designs with two factors present. 

| trial_id | fertilizer | cultivars | rep |  yield  | letters|
|:---------|-----:|------:|----:|----:|-------:|
|1         | A    |   Z     |  3   | 60.2|  a     |
|1         | B    |   Z     |  3   | 45.2|  c     |
|1         | C    |   Z     |  3   | 55.4|  b     |
|1         | A    |   Y     |  3   | 40.0|  b     |
|1         | B    |   Y     |  3   | 58.2|  a     |
|1         | C    |   Y     |  3   | 57.2|  a     |
|2         | A    |   H     |  6   | 55.4|  a     |
|...       | ...  | ... | ... | ... |  ...   |


Note that in factorial designs, A and B can be used interchangeably because there is only one error variance in ANOVA, which is not the case for split-plot designs. Users must specify whether factor A was allocated in the main or subplot levels in split-plot designs at later field selection steps. However, only trials with A allocated to either the main or subplot levels must be compiled together, as in example 4 of the tutorial. 


### Step 4 - Upload the CSV file

In this step, users upload the pre-processed CSV file containing the required information. Users are encouraged to familiarize themselves with example files and follow instructions for input CSV collation. 


### Step 5 - Make field selections and estimate

After trial-specific information has been correctly organized into the CSV input file, users will proceed with $\hat\sigma^2$ estimation in the Estimator module. In this module, users will indicate the experimental design, post hoc test, and $\alpha$ significance level for all underlying trials in the CSV input file in the design box (Figure 1). The selection fields in specify column box are updated as experimental designs are selected in the design box. Users must match the CSV input file columns with their respective fields during this step.

For two-way factorial designs, additional selection field options are displayed to indicate whether the treatment structure of compiled trials has the factor B omitted or A × B interaction present. That is accomplished via the field **source of variation** in the estimator module. Select `A` when only means for one treatment factor `A` is present and `A × B` when both factors are reported. 

* **Source of variation:** represents the treatment structure of studies. Select `A` when only means for one factor are reported and `A × B` when both treatment lists are available.

Alternatively, if the collection of trials was arranged as a two-way split-plot design, users must also indicate whether `A` was allocated to main or subplot levels via the **level** selection button. Note that when `A × B` is selected in the source of variation for split-plot designs, `A` must be assigned to the subplot level, unconditionally. As mentioned earlier, `MSE FindR` can provide $\hat\sigma^2$ estimations for scenarios 1, 2, and 3 of split-plot designs (Step 2).

* **Level:** represents the level (main or subplot) when only a single factor is reported. Users must select one of the options to distinguish $\hat\sigma^2$ estimation between 1 and 2.


### Step 6 - Export file

After column selections have been made and the estimate button pressed, a download button along with the results will be displayed. Users can export the file containing `MSE FindR` estimates of degrees of freedom and $\hat\sigma^2$ for each trial along with information previously included in the original CSV file. 

Obs.: users may check whether $\hat\sigma^2$ has been correctly estimated by hand-calculating the degrees of freedom for a few trials and comparing that value to `MSE FindR` degrees of freedom estimates. At this point, users should know that only one $\hat\sigma^2$ value is estimated for each trial. Additionally, `MSE FindR` returns `NA` for non-significant post hoc test results (e.g. all letters are the same in a trial).


# Technical details

In this section, we provide some details on $\hat\sigma^2$ estimation. `MSE FindR` algorithms compute two finite boundaries (the largest non-significant and the smallest significant differences) for all mean pairwise combinations of treatments and average those two values for a variable called ELSD ([Ngugi et al., 2011](https://apsjournals.apsnet.org/doi/abs/10.1094/PHYTO-08-10-0221)). 

Table 1 outlines formulas used to obtain $\hat\sigma^2$ for one-way designs (see, [Millien & Johnson, 2009](https://www.taylorfrancis.com/books/mono/10.1201/EBK1584883340/analysis-messy-data-volume-1-george-milliken-dallas-johnson)).

$n.replicates$ = the number of repetitions or blocks per trial,

$ELSD$ = estimated average of two finite boundaries,

$\alpha$ = the significance level of the post hoc test,

$n.levels.A$ = the number of levels in factor A,

$m$ = the total number of pairwise(?) comparisons per trial. m is given by $\binom{n.levels.A}{2}$,

$df_{error}$ = the degrees of freedom for $\hat\sigma^2$,

$qt$, $qtukey$, and $qf$ = r functions used to compute the value of quantile function over Student t, Tukey, and F distributions, respectively.


Table 1. Expressions for estimation of $\hat\sigma^2$ based on post hoc test for one-way factor designs.

| Post hoc test          | Expression                                                                                   |
| ---------------------- | ------------------------------------------------------------------------------------------- |
| Fisher's LSD           | $\hat\sigma^2 = 0.5 \cdot n.replicates \cdot \biggl(\frac{ELSD}{qt(1-\alpha/2,df_{error})}\biggr)^2$ |
| Tukey's HSD            | $\hat\sigma^2 = n.replicates \cdot \biggl(\frac{ELSD}{qtukey(1-\alpha,n.levels.A,df_{error})}\biggr)^2$ |
| Bonferroni adjustment  | $\hat\sigma^2 = 0.5 \cdot n.replicates \cdot \biggl(\frac{ELSD}{qt(1-(\alpha/2m),df_{error})}\biggr)^2$ |
| Šidák adjustment       | $\hat\sigma^2 = 0.5 \cdot n.replicates \cdot \biggl( \frac{ELSD}{qt(1-(1-\alpha)^{1/m},df_{error})}\biggr)^2$ |
| Scheffé's              | $\hat\sigma^2 = \frac{n.replicates \cdot ELSD^2}{2 \cdot (n.level.A-1) \cdot qf(1-\alpha,n.level.A-1,df_{error})}$ |

Similar expressions are used for $\hat\sigma^2$ estimation in two-way designs, except that they include the number of levels for factor B, which are either reported by the user (via the number of levels of factor B column) or calculated by the app directly (from factor B column) from the CSV input file (see, [Millien & Johnson, 2009](https://www.taylorfrancis.com/books/mono/10.1201/EBK1584883340/analysis-messy-data-volume-1-george-milliken-dallas-johnson)).



## References

Ngugi, H.K., Lehman, B.L. and Madden, L.V., 2011. Multiple treatment meta-analysis of products evaluated for control of fire blight in the eastern United States. Phytopathology, 101, 512-522.

Milliken, G.A., & Johnson, D.E., 2009. Analysis of Messy Data Volume 1: Designed Experiments, Second Edition. Chapman and Hall/CRC

Montgomery, D.C., 2017. Design and analysis of experiments. John Wiley & Sons.

