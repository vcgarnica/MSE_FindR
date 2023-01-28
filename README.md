# MSE FindR Tutorial

## Introduction

Meta-analysis is a methodology used to synthesize the results of multiple independent studies to provide an overall estimate of a treatment effect, along with a measure of its precision. In the absence of individual participant data (a.k.a. raw data), standard meta-analytic approaches for continuous outcomes rely on reported summary metrics, such as the mean and a corresponding measure of variability (e.g., the sample standard deviation and standard error values for each treatment group). This is because effect sizes in meta-analyses are typically weighted by the study's precision, most commonly by the inverse of the study variance. A common complication arises when none of these summary variability metrics are explicitly included in the primary studies.

`MSE FindR` is a user-friendly R Shiny web app tool developed to help researchers obtain a measurement of variability from studies lacking summary metrics. Instead, it utilizes treatment means, alpha significance level ($\alpha$), the number of replicates, and post-hoc test results. `MSE FindR` estimates the pooled variance (i.e., MSE; mean square error), the estimator of population variance $\sigma^2$, and thus $\hat\sigma^2$, in replicated, randomized studies analyzed with ANOVA. The tool extracts $\hat\sigma^2$ using basic experimental information, provided that the trials are balanced (i.e., the same number of replicates per treatment). Once the MSE has been estimated, one can then calculate the variability measurements required in meta-analysis studies. MSE and $\hat\sigma^2$ will be used interchangeably in the tutorial.

The application is hosted at [https://garnica.shinyapps.io/MSE_FindR/](https://garnica.shinyapps.io/MSE_FindR/).

`MSE FindR` expands on concepts published in [Ngugi et al., 2011](https://apsjournals.apsnet.org/doi/abs/10.1094/PHYTO-08-10-0221) by incorporating additional post hoc tests (a.k.a. multiple comparison tests obtained from `agricolae`, `emmeans`, and `multcomp` R packages) and a variety of experimental designs commonly used in the agricultural sciences. 

In its current version, `MSE FindR` supports the extraction $\hat\sigma^2$ for the following experimental designs:

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


`MSE FindR` contains three main modules: Disclosure, Upload file, and Estimator, to help users navigate through the tool's functionalities and conveniently obtain $\hat\sigma^2$ for multiple trials at a time (Figure 1). 

![legend](https://github.com/vcgarnica/MSE_FindR/blob/main/Images/workflow.png)
Figure 1. MSE FindR conceptual flowchart highlighting the inputs and estimation for randomized complete block designs analyzed with Fisher's LSD test.


## `MSE FindR`: Disclosure

Contains a walk-through tutorial, downloadable example files, information about developers, and citation.


## `MSE FindR`: Upload CSV file

In this module, users can upload a CSV file that contains information specific to a trial. The information in the file may vary depending on the design and structure of the study. Below, you will find instructions on how to gather trial data and organize it into a CSV file. The application only accepts standard CSV formats, and the default settings are set to use commas as separators and double quotes. However, these settings can be modified to match the format of your CSV data file.

## `MSE FindR`: Estimator

After pre-processing a CSV file using the `MSE FindR` tutorial and uploading it into the application, users can calculate $\hat\sigma^2$ by assigning columns of the CSV file, named at the user's preferences, to the appropriate fields. Note that a proper assignment of columns to the respective selection fields is a critical step for correct $\hat\sigma^2$ extraction.  Once columns have been properly assigned and the estimate button is clicked, a download button will appear allowing users to export the results as a CSV file. The `MSE FindR` method calculates a trial-specific value for $\hat\sigma^2$ and its associated degrees of freedom.


## Procedure

### Step 1 - Compile trials with the same information

First, users must only assign trials with the same experimental configuration, meaning the same **experimental design**, **post hoc test**, **$\alpha$ significance level**, and **treatment structure (for two-way designs)** to the CVS input file (Figure 1). Incorrect $\hat\sigma^2$ can result from assigning trials with incompatible information to the same CVS file. 

*Example 1*: A systematic reviewer has collected data from ten trials that examine the impact of four different commercial fertilizers on yield. Half of these trials were analyzed using Fisher's LSD and the other half were analyzed using Tukey's HSD. Additionally, two trials that used Tukey's HSD were arranged as CRD and the remaining three were arranged as RCBD. In order to properly organize this data, the user should create three separate folders, one for Fisher's LSD, one for Tukey's HSD arranged as CRD, and one for Tukey's HSD arranged as RCBD. This is an example where the **experimental design** and **post-hoc test** used in the trials are different. Each folder should contain a CSV input file with trials with similar criteria.

*Example 2*: It was later found that four trials in the Fisher's group were analyzed with $\alpha$ set to 0.05 and one trial was analyzed with $\alpha$ set to 0.10. In this case, a fourth folder is needed, instead of three, to account for the difference in **$\alpha$ significance level** among Fisher's trials.  

*Example 3*: The systematic reviewer has selected five independent trial reports that each conduct a plant experiment arranged as a two-way factorial RCBD, testing combinations of six to eight commercially available fertilizers and three to five soybean cultivars, with yield as the response. The original authors of the trials may choose to publish results for either the fertilizer (factor A), cultivar (factor B), or the interaction (A x B), depending on their research goals and ANOVA results. The systematic reviewer is interested in obtaining $\hat\sigma^2$ for the fertilizer response, but two trial reports contain means/post-hoc tests for A, while the remaining three reports contain means/post-hoc tests for the A x B interaction. This is a scenario where the experimental design is the same, but the **treatment structure** of the trials (whether data is presented as factor A or A x B) is different. Two folders, one for each treatment structure, should be created, assuming all other information is the same. We will discuss the information needed for different experimental designs and treatment structures at later stages.

*Example 4*: The researcher has selected two nearly identical trial reports that conduct plant experiments arranged as a split-plot RCBD, testing combinations of four to five fertilizers (factor A) and three to five cultivars (factor B), with plant height as the response. Notably, the trial reports have been published by the same authors and only means/post-hoc test results for factor A are available. Factors B and A x B were omitted as there were no significant effects for these factors during ANOVA. All other experimental information is the same, except that in the first trial report, fertilizers treatments were allocated to main plots and cultivars treatments to subplots, while in the second trial report, fertilizers treatments were allocated to subplots and cultivar treatments to main plots. This is a scenario where the treatments are reported similarly (only fertilizer means/post hoc) but are structurally/spatially different, and thus, the trials must be collated separately. Assigning trials with fertilizers means/post-hoc test to incorrect hierarchical level (main or subplot) will generate incorrect calculations and degree of freedom estimations.

Therefore, users must compile trial reports that share the same *experimental design*, *post hoc test*, $\alpha$ *significance level*, and *treatment structure (for two-way designs)* configurations. 


### Step 2 - Determine information needed for CSV input file collation

`MSE FindR` is a tool that can be used to extract the value of $\hat\sigma^2$ for multiple experimental designs. As one may anticipate, $\hat\sigma^2$ extraction will be different across experimental designs. More complex (2-way) designs will require more information than simpler (1-way) designs.

For example, consider a Latin square design, the simplest experimental design available in `MSE FindR`. In this design the number of columns, rows, and treatments is equal and there is only one factor under evaluation (see, [Montgomery, 2017](https://www.google.com/books/edition/Design_and_Analysis_of_Experiments/Py7bDgAAQBAJ?hl=en&gbpv=0)), herein called factor A. Thus, users do not need to specify the number of columns and rows in each trial analyzed. `MSE FindR` will count the number of treatments and automatically assign that value to calculate the number of degrees of freedom for the error variance. 

When using other experimental designs, such as CRD or RCBD, the information needed to calculate $\hat\sigma^2$ also depends on factors such as the number of replicates or blocks in the trial. These numbers may not be equal to the number of treatment levels. As a result, users who want to calculate $\hat\sigma^2$ for these designs must include information about the number of replicates or blocks in the CSV input file used with `MSE FindR`. The tool will then use this information, along with the number of distinct treatments, to compute the correct number of degrees of freedom for the error variance.

In some cases, researchers may be interested in studying a second treatment factor (factor B) in addition to factor A, and this factor may or may not interact with factor A. This information may not be included in the original publication as per author's decision. When the data is arranged as two-way factorial or split-plot designs and only treatment means/post-hoc results for factor A are included, users must provide a column in the CSV input file specifying the **number of levels for the omitted factor B**. Alternatively, if means/post hoc results for the A x B interaction have been reported, then the systematic reviewer must list the treatment columns for factors A and B independently in the CSV input file. It's worth noting that the calculation of $\hat\sigma^2$ is more precise when information about the A x B interaction is included, rather than just the levels of factor A or B for two-way designs (check [Ngugi et al., 2011](https://apsjournals.apsnet.org/doi/abs/10.1094/PHYTO-08-10-0221)). Additionally, it's important to note that in factorial designs, factors A and B can be assigned interchangeably, meaning any treatment factor can be named A or B. However, this is not the case in split-plot designs due to their hierarchical/spatial structure.

There are some similarities between the ANOVA of two-way factorial designs and designs such as CRD and RCBD, in that there is a single error term describing the variance of all treatments. This contrasts with split-plot designs, where at least two error terms are present, one for factors assigned to the main plot and a second for subplot factors (see, [Montgomery, 2017](https://www.google.com/books/edition/Design_and_Analysis_of_Experiments/Py7bDgAAQBAJ?hl=en&gbpv=0)). Knowing whether factor A, the factor of interest, was placed in the main or subplot levels is important because it affects how the number of degrees of freedom of $\hat\sigma^2$ is calculated. Depending on whether factor A is allocated on main or subplot levels and the presence/absence of significant factor interactions, there could be five distinct values of $\hat\sigma^2$:

When the A × B interaction **is not significant** in primary studies:

1. Obtaining $\hat\sigma^2$ for A in the main plot level
2. Obtaining $\hat\sigma^2$ for A in the subplot level

When A × B interaction **is statistically significant** in primary studies:

3. Obtaining $\hat\sigma^2$ for A in the subplot level within the main plot level (A within B)
4. Obtaining $\hat\sigma^2$ for A in the main plot level within the subplot level (B within A)
5. Obtaining $\hat\sigma^2$ for A in the subplot level across different main plot levels (A × B)

`MSE FindR` is able to compute $\hat\sigma^2$ for scenarios 1, 2, and 3. For 5, $\hat\sigma^2$ consists of a weighted average between $\hat\sigma^2_A$ (main plot error) and $\hat\sigma^2_B$ (subplot error), which is considerably more complex. The process of collating trial data into the input CSV file for split-plot designs is similar to that of two-way factorial designs. However, users must be mindful of whether factor A was allocated to the main or subplot levels, and specify this information correctly in later steps. Incorrect $\hat\sigma^2$ results may occur if factor A is wrongly assigned to the subplot level instead of the main plot level or vice versa. 

More information on the data requirements and how to specify experimental configurations for all trials in the CSV file can be found below. Downloadable example files to assist with the collation process can be found in the `MSE FindR` app.

### Step 3 - Create a CSV file

The next step after organizing trial reports with similar experimental configurations is to compile the data into a CSV input file. This process involves specifying the necessary information based on the experimental design and treatment structure (for two-way designs) for `MSE FindR` to function properly. It is important to note that the **post-hoc test** **experimental design**, and **\alpha significance level** info do not necessarily need to be included in the CSV file as users will provide that information at later stages through field selections in the Estimator module (Figure 1). One could though add that information to keep files more traceable. 

Below you can find **experimental design** specific information for CSV input file processing. Multiple trials cna be collated to the same CSV input file, as long as they meet the criteria discussed above.

#### 3.1. Latin square

The CSV input file should include columns that specify the trial identifier number (e.g., *trial_id*), treatment list for factor A (e.g.,*fertilizer*), treatment means (e.g.,*yield*), and the post hoc test results (e.g., *letters*). These columns can be named according to the user's preference.

In the application, at Estimator tab, Select columns box, there will be the following fields in Latin square design:
* **Unique trial identifier number:** allows extraction of $\hat\sigma^2$ for multiple trials at once by including a unique numerical identification number for each trial.
* **Factor A:** represents the treatment list for factor A – either numerical or categorical.
* **Means:** represents treatment means – numeric.
* **Post hoc test letters:** contains letters of post hoc tests results – categorical.


Table 1. Example of columns in the CSV file for obtaining $\hat\sigma^2$ via `MSE FindR`in Latin square designs. 

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

For one-way CRD and RCBD designs, the number of replicates (e.g., *rep*) or blocks will need to be included in the CSV input file:

* **Number of replicates:** represents the total number of replicates or blocks for each trial — assumes a balanced trial (i.e., the same number of replicates for each treatment) – numerical.

Table 2. Example of columns in the CSV file for obtaining $\hat\sigma^2$ via `MSE FindR`in completely randomized and randomized complete block designs. 

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

As mentioned earlier, additional information is required when calculating $\hat\sigma^2$ from trials with two-way factorials or split-plot designs. In these scenarios, users are interested in obtaining $\hat\sigma^2$ for either factor A (which may be placed in main or subplot for split-plots) or the interaction A × B. We will cover scenarios where A or A × B are of interest separately below. Note that users will need to specify whether `MSE FindR` is handling A or the interaction A × B in the app.

##### 3.3.1. *One treatment factor omitted*

When the collection of scientific reports or studies is arranged as two-way factorial or split-plot designs and only treatment means and post hoc results for factor A (e.g., *fertilizer*) are included, users must provide a column in the CSV input file specifying the **number of levels for the omitted factor B (e.g., cultivar)**. This is often the case when A × B interaction is not significant in primary studies and authors decide to omit its results from the original publication. In this case, add a column in the CSV input file with the number of levels for the omitted factor B:

* **Number of levels of factor B:** represents the number of levels for factor B – numerical.


Table 3. Example of columns in the CSV file for obtaining $\hat\sigma^2$ via `MSE FindR`in factorial designs with one factor omitted. 

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

In contrast, if the primary studies show a significant A x B interaction, then it is necessary to include two separate columns in the input CSV file for the treatment lists of both factors A and B (Table 4). Factor B treatment column is specified via:

* **Factor B:** represents the treatment list for factor B – either numerical or categorical.

Table 4. Example of columns in the CSV file for obtaining $\hat\sigma^2$ via `MSE FindR`in factorial designs with A × B interaction present. 

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


Final note: In factorial designs, A and B can be used interchangeably because there is only one error variance in ANOVA, which is not the case for split-plot designs. In split-plot designs, users must specify whether factor A was allocated in the main or subplot levels by making the selection in the proper field within `MSE FindR` Estimate tab. We reinforce that the collation of trials data must obey the same treatment structure rule, meaning only trials with A allocated to either the main or subplot levels can be compiled together, as in example 4 of the tutorial. 


### Step 4 - Upload the CSV file

In this step, users upload the pre-processed CSV file containing the required information. 


### Step 5 - Make field selections and estimate

After the trial data has been correctly organized in the CSV input file and uploaded, users can proceed with the calculation of $\hat\sigma^2$ in the Estimator module. In this module, users need to select the experimental design, post hoc test, and $\alpha$ significance level for all trials in the CSV input file using the design box (Figure 1). The selection fields in the specify column box will be updated dynamically as the experimental design is selected in the design box. Users must match the columns in the CSV input file with their corresponding selection fields during this step.

In two-way factorial designs, additional selection field options are available to indicate whether the treatment structure of the compiled trials represents the A × B interaction or the omitted factor B. This is done through the field **source of variation** in the Estimator module. Select **A** when only means for one treatment factor are present and **A × B** when both factor means are reported.

* **Source of variation:** represents the treatment structure of studies. Select `A` when only means for one factor are reported and `A × B` when both treatment lists are available.

Alternatively, if the collection of trials was arranged as a two-way split-plot design, users must also indicate whether `A` was allocated to main or subplot levels via the **level** selection button. 

* **Level:** represents the level (main or subplot) when only a single factor is reported. Users must select one of the options to distinguish between 1 and 2 $\hat\sigma^2$ calculations in split-plot designs.

### Step 6 - Export file

After clicking on the estimate button, a download button along with the results table will be displayed. Users can export the file containing `MSE FindR` estimates of degrees of freedom and $\hat\sigma^2$ for each trial along with information previously included in the CSV input file. 

Obs.: Users may check whether $\hat\sigma^2$ has been correctly calculated by hand-calculating the degrees of freedom for a few trials and comparing that value to `MSE FindR` estimates. At this point, one should know that only one $\hat\sigma^2$ value is calculated for each trial. `MSE FindR` returns `NA` when non-significant post hoc test results are included (e.g. all letters are the same for a trial).


# Technical details

In this section, we provide some details on $\hat\sigma^2$ calculations. `MSE FindR` algorithms compute two finite boundaries (the largest non-significant and the smallest significant differences) for all mean pairwise combinations of treatments and average those two values for a variable called ELSD ([Ngugi et al., 2011](https://apsjournals.apsnet.org/doi/abs/10.1094/PHYTO-08-10-0221)). 

Table 1 outlines formulas used to obtain $\hat\sigma^2$ for one-way designs (see, [Millien & Johnson, 2009](https://www.taylorfrancis.com/books/mono/10.1201/EBK1584883340/analysis-messy-data-volume-1-george-milliken-dallas-johnson)).

$n.replicates$ = the number of repetitions or blocks per trial.

$ELSD$ = estimated average of two finite boundaries.

$\alpha$ = the significance level of the post hoc test.

$n.levels.A$ = the number of levels in factor A.

$m$ = the total number of pairwise comparisons per trial. m is given by $\binom{n.levels.A}{2}$,

$df_{error}$ = the degrees of freedom for $\hat\sigma^2$.

$qt$, $qtukey$, and $qf$ = r functions used to compute the value of quantile function over Student t, Tukey, and F distributions, respectively.


Table 1. Expressions for obtaining $\hat\sigma^2$ based on post hoc test for one-way factor designs.

| Post hoc test          | Expression                                                                                   |
| ---------------------- | ------------------------------------------------------------------------------------------- |
| Fisher's LSD           | $\hat\sigma^2 = 0.5 \cdot n.replicates \cdot \biggl(\frac{ELSD}{qt(1-\alpha/2,df_{error})}\biggr)^2$ |
| Tukey's HSD            | $\hat\sigma^2 = n.replicates \cdot \biggl(\frac{ELSD}{qtukey(1-\alpha,n.levels.A,df_{error})}\biggr)^2$ |
| Bonferroni adjustment  | $\hat\sigma^2 = 0.5 \cdot n.replicates \cdot \biggl(\frac{ELSD}{qt(1-(\alpha/2m),df_{error})}\biggr)^2$ |
| Šidák adjustment       | $\hat\sigma^2 = 0.5 \cdot n.replicates \cdot \biggl( \frac{ELSD}{qt(1-(1-\alpha)^{1/m},df_{error})}\biggr)^2$ |
| Scheffé's              | $\hat\sigma^2 = \frac{n.replicates \cdot ELSD^2}{2 \cdot (n.level.A-1) \cdot qf(1-\alpha,n.level.A-1,df_{error})}$ |

Similar expressions are used for $\hat\sigma^2$ extraction in two-way designs, except that they include the number of levels for factor B, which are either reported by the user (via the number of levels of factor B column) or calculated by the app directly (from counting factor B levels in the treatment B column).


## References

Ngugi, H.K., Lehman, B.L. and Madden, L.V., 2011. Multiple treatment meta-analysis of products evaluated for control of fire blight in the eastern United States. Phytopathology, 101, 512-522.

Milliken, G.A., & Johnson, D.E., 2009. Analysis of Messy Data Volume 1: Designed Experiments, Second Edition. Chapman and Hall/CRC

Montgomery, D.C., 2017. Design and analysis of experiments. John Wiley & Sons.

