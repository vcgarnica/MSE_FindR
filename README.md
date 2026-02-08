#  `MSE FINDR` 

This repository contains the companion code and the associated shiny app `MSE FINDR` of Garnica et al. (2024). It contains the associated figures of the paper, example files, instructions, as well as the simulation code used in the paper.


## Citation

This repository is associated with the following article:

> Garnica, V. C., Shah, D. A., Esker, P. D., Ojiambo, P. S. (2024). MSE FINDR: A Shiny R Application to Estimate Mean Square Error Using Treatment Means and Post-hoc Test Results. Plant Disease. doi: [10.1094/PDIS-11-23-2519-SR](https://apsjournals.apsnet.org/doi/epdf/10.1094/PDIS-11-23-2519-SR).

## Introduction

Meta-analysis and multi-environmental trial analysis are methodologies used to synthesize the results of multiple independent studies to provide an overall estimate of a treatment effect. In the absence of individual study data (also known as raw data), standard meta-analytic approaches for continuous outcomes rely on reported summary metrics, such as treatment mean and a corresponding measure of variability (e.g., the sample standard deviation and standard error values for each treatment group). This is because effect sizes in these analyses are typically weighted by the study's precision, most commonly by the inverse of the study variance or other variability metric. A common complication arises when none of these summary variability metrics are explicitly included in the primary studies.

`MSE FINDR` is a user-friendly R Shiny web app tool developed to help researchers obtain a measurement of variability from studies lacking summary metrics. Instead, it utilizes treatment means, significance level, the number of replicates, and post-hoc test results as input information. `MSE FINDR` estimates the pooled residual variance (i.e., MSE; mean square error) in replicated, randomized studies analyzed via ANOVA. The tool extracts $\hat\sigma^2$ using basic experimental information, as long as trials are balanced (i.e., the same number of replicates per treatment). Once the $\hat\sigma^2$ has been estimated, one can then calculate the variability measurements required in meta-analysis or multi-environmental trial studies. MSE and $\hat\sigma^2$ will be used interchangeably in the tutorial.

The application is hosted at [https://garnica.shinyapps.io/MSE_FindR/](https://garnica.shinyapps.io/MSE_FindR/).

`MSE FINDR` expands on concepts published in [Ngugi et al., 2011](https://apsjournals.apsnet.org/doi/abs/10.1094/PHYTO-08-10-0221) by incorporating additional post-hoc tests (also known as multiple comparison tests obtained from `agricolae`, `emmeans`, and `multcomp` R packages) and a variety of experimental designs commonly used in ecology and agricultural sciences. It also improves on [Ngugi et al., 2011](https://apsjournals.apsnet.org/doi/abs/10.1094/PHYTO-08-10-0221) by incorporating the correct distribution and degrees of freedom for underlying post-hoc test used in primary study analysis.

In its current version, `MSE FINDR` supports the extraction of $\hat\sigma^2$ for the following experimental designs:

- Completely randomized design (CRD)
- Randomized complete block design (RCBD)
- Latin Square
- Two-way factorial as CRD
- Two-way factorial as RCBD
- Split-plot arranged as CRD
- Split-plot arranged as RCBD

And post-hoc tests:

- Fisher's LSD
- Tukey's HSD
- Bonferroni correction for multiple comparisons
- Šidák correction for multiple comparisons
- Scheffé's test

`MSE FINDR` contains four main modules: **Disclosure**, **Upload file**, **Estimator**, and **Usage Statistics**, to help users navigate through the tool's functionalities and conveniently obtain $\hat\sigma^2$ for multiple trials at a time (Figure 1).

![legend](https://github.com/vcgarnica/MSE_FindR/blob/main/Images/workflow.png)
Figure 1. `MSE FINDR` conceptual flowchart highlighting the inputs and estimation for randomized complete block designs analyzed with Fisher's LSD test.

# Tutorial 

## `MSE FINDR`: Disclosure

The Disclosure module serves as the landing page of the application and provides:

- A **Quick Start Guide** with step-by-step instructions and links to tutorial sections
- An overview of **Key Features**, including supported experimental designs and post-hoc tests
- **Downloadable example files** for all supported design/test combinations
- A **"Load Example Data"** button that allows users to preview and load example datasets directly into the application without downloading and re-uploading CSV files
- Citation information, development team contacts, and licensing details

## `MSE FINDR`: Upload CSV file

In this module, users can upload a CSV file that contains trial-specific information. Columns in the CSV input file may vary depending on the design and structure of the study. Below, you will find detailed instructions on how to gather trial data and collate into the CSV input file. The application exclusively supports standard CSV formats, with default settings utilizing commas as separators and double quotes. Nevertheless, these settings can be adjusted to align with the format of your CSV data file.

Users can also arrive at this module automatically after loading an example dataset from the Disclosure page, in which case the example data will already be displayed in the data table.

## `MSE FINDR`: Estimator

After pre-processing a CSV file using the `MSE FINDR` tutorial below and uploading it into the application, users can calculate $\hat\sigma^2$ by assigning columns of the CSV file, named at the user's preferences, to the appropriate selection fields. Note that a proper assignment of columns to the respective selection fields is a critical step for correct $\hat\sigma^2$ extraction. Once columns have been properly assigned and the estimate button is clicked, a download button will appear allowing users to export the results in a CSV output file. The `MSE FINDR` application calculates a trial-specific value for $\hat\sigma^2$ along with its associated degrees of freedom.

The Estimator module now includes a dynamic **Instructions panel** that updates based on the selected experimental design. This panel displays the required CSV columns for the chosen design and provides important warnings — particularly for split-plot designs, where misspecification of the source of variation (main-plot vs. sub-plot) will result in incorrect MSE estimates.

## `MSE FINDR`: Usage Statistics

The Usage Statistics module provides real-time information about the reach and impact of the tool, including:

- **Total visitor count** — a persistent counter tracking the number of unique sessions since January 2026
- **Live citation count** — powered by the [Dimensions.ai](https://www.dimensions.ai/) badge, displaying the current number of citations for the associated publication in real-time

## Workflow

### Step 1 - Compile trials with the same information

First, users should collect all primary reports that share the same experimental configuration, meaning the same **experimental design**, **post-hoc test**, **significance level**, and **treatment structure** (for two-way designs). These reports should be organized within a designated folder (refer to Figure 1). It is crucial that all trials within the designated folder have the same experimental configuration as mentioned above. By doing this, `MSE FINDR` can process a large number of trials simultaneously and return correct values for $\hat\sigma^2$ and its associated degrees of freedom. Incorrect $\hat\sigma^2$ can result from assigning trials with incompatible information to the same CVS input file. 

Here are some examples to help users better understand how studies with different experimental configurations should be organized:

*Example 1*: Let's consider a scenario where a systematic reviewer has obtained means/post-hoc test results from ten studies aimed to investigate the effects of four different commercial fertilizers on crop yield. The reviewer found that half of the studies used Fisher's LSD as the post-hoc test, while the other half used Tukey's HSD. Furthermore, two of the trials that utilized Tukey's HSD were arranged as CRD design, and the remaining three trials were arranged as RCBD design.

To properly organize this data for `MSE FINDR`, the user should create three separate folders. The first folder will contain the primary studies that used Fisher's LSD. The second folder will include the trials that used Tukey's HSD and were arranged as CRD. Finally, the third folder will contain the trials that used Tukey's HSD and were arranged as RCBD. This separation is necessary because the **experimental design** and **post-hoc test** used in the trial analysis differ across the folders.

Each folder should result in a CSV input file with similar content that will be covered in Step 2.

*Example 1a*: Suppose it was discovered that among the trials analyzed with Fisher's LSD, four trials were conducted with a significance level of 0.05, while one trial was analyzed with a significance level of 0.10. In this situation, an additional folder is required to account for the difference in **significance level** among the Fisher's LSD analyzed trials. By now, you should note that the systematic reviewer must create four separate folders: one folder for the trials analyzed with Fisher's LSD at = 0.05, another folder for the trial analyzed with Fisher's LSD at = 0.10, and the remaining two folders for the trials arranged as CRD and RCBD, which use Tukey's HSD. Each folder should contain the relevant trial information in a CSV input file.

*Example 2*: Another systematic reviewer has chosen five independent trial reports that involve plant experiments arranged as a RCBD two-way factorial design. These trials examine the effects of different commercially available fertilizers (factor `A`) and cultivars (factor `B`) on soybean yield. The trials vary in the number of fertilizers tested (ranging from six to eight) and the number of soybean cultivars (ranging from three to five). Yield is the response variable.

The primary authors of the studies can choose to publish results (means and post-hoc tests results) for either main effect `A`, `B`, or the interaction effect `A x B`, depending on their research goals and significant results during ANOVA. However, the systematic reviewer is interested in obtaining $\hat\sigma^2$ for only the fertilizer treatments. Furthermore, two of the trial reports provide means and post-hoc test results for main effect, while the remaining three reports provide means and post-hoc test results for the `A x B` interaction. This represents a situation where the **experimental design** is the same, but the **treatment structure** (the reported treatment means/post-hoc test result) of the trials differs.

Two folders, one for each treatment structure, should be created, assuming all other information is the same. Again, information needed for different experimental designs and treatment structures will be discussed shortly.

*Example 3*: A third researcher has chosen two nearly identical trial reports that involve plant experiments arranged as a RCBD split-plot design. These trials investigate the effects of different fungicides and cultivars on disease severity. Trials vary in the number of fungicides tested (ranging from four to five) and the number of cultivars (ranging from three to five).

The primary reports have been published by the same authors, and only means and post-hoc test results for the fungicide factor are available in the primary reports. The cultivar factors and interaction were omitted from the reports as they were found to have no significant effects during ANOVA. All other experimental information, such as the experimental design, post-hoc test, and significance level, remains the same for both groups. Upon closer examination, it was observed that in the first trial report, the fungicide treatments were assigned to the main-plots, while the cultivar treatments were assigned to the sub-plots. Conversely, in the second trial report, the fungicide treatments were allocated to the sub-plots, and the cultivar treatments were assigned to the main-plots. Should we assign these two trials to the same folder? Definitely, not...

Because split-plot designs contain two error sources, it is important to recognize and take this difference in account when estimating $\hat\sigma^2$. This represents a scenario where the treatments are reported similarly (only fungicide means and post-hoc test results), but the structural and spatial arrangement of the treatments within the experimental design (**treatment structure**)  differs between the two trial reports. Consequently, it is necessary to collate these trials into separate CSV input files. Assigning trials with fungicide means and post-hoc tests to the incorrect hierarchical level (main-plot or sub-plot and vice-versa) in split-plot designs would lead to erroneous estimations of $\hat\sigma^2$ and the associated degrees of freedom.

Therefore, users must compile trial reports that share the same **experimental design**, **post-hoc test**, **significance level**, and **treatment structure (for two-way designs)** configurations. 


### Step 2 - Understanding how information is organized and creating the CSV input file

With the appropriately organized folders in place, it is now time to understand how the CSV input file will consolidate information from all the trials within that folder.

`MSE FINDR` was designed to extract $\hat\sigma^2$ for various experimental designs. As a result, the extraction process may differ depending on the complexity of the study and its treatment design. More intricate designs, such as two-way designs, require additional information about the other treatment factor compared to simpler one-way designs. However, the CSV input file maintains a basic column structure throughout:

* Trial identification number
* Treatment list 
* Treatment means
* Number of replicates or blocks (if applicable based on the experimental design)
* Corresponding post-hoc test letter results

In the next sections, we explain how this basic structure changes across experimental designs.

> **Tip:** You can use the **"Load Example Data"** button on the Disclosure page to load and preview example datasets for each supported design directly within the application, without needing to download and re-upload files.

#### 2.1. Latin square

Let's take the Latin square design as an example, which is the most straightforward experimental design supported by `MSE FINDR`. In this design, the number of columns, rows, and treatments within a study is equal. In addition, comparisons are made for only one treatment factor, referred to as factor `A`. Thus, when using `MSE FINDR` with a Latin square design, users don't need to specify the number of columns and rows in each trial, the tool automatically determines these values based on the number of treatments for each trial. `MSE FINDR` will calculate the number of treatments and determine the correct number of degrees of freedom for the residual variance for each trial. 

The CSV input file compiling trials arranged in Latin square design should include four information columns, named at user's preference: trial identification number (e.g. labeled as *trial_id*, numerical), treatment list for factor `A` (e.g., *fertilizer*, either numerical or categorical), treatment means (e.g.,*yield*, numerical), and the post-hoc test results (e.g., *fisher_letters*, categorical) (Table 1). 

Looking at Table 1, the first 5 fertilizer treatments correspond to treatments evaluated in the first scientific report. Then, we included 3 fertilizer treatments for the second trial, with respective `MSE FINDR` supporting information. This is done for all primary reports in the designated folder until all trials are included in the CSV input file. In this example, we are interested in obtaining variability metrics for fertilizer treatments.

Table 1. Example of columns in the CSV file for obtaining $\hat\sigma^2$ via `MSE FINDR`in Latin square designs. 

| trial_id | fertilizer | yield  | fisher_letters|
|:---------|-----:|----:|-------:|
|1         | A    | 60.2|  a     |
|1         | B    | 45.2|  c     |
|1         | C    | 55.4|  b     |
|1         | D    | 40.0|  b     |
|2         | A    | 58.2|  a     |
|2         | J    | 57.2|  a     |
|2         | K    | 55.4|  a     |
|...       | ...  | ... | ...    |


#### 2.2. One-way CRD and RCBD designs

For studies arranged as CRD or RCBD, calculating $\hat\sigma^2$ requires an additional piece of information: the number of replicates or blocks in each trial. Unlike the Latin square design, the number of replicates or blocks may or may not be the same as the number of treatments in each trial. As a result, users need to include the number of replicates or blocks as a column in the CSV input file (e.g., *rep*, numerical), as illustrated in Table 2. This ensures that the tool accurately calculates the appropriate number of degrees of freedom for $\hat\sigma^2$. In this example, we are interested in obtaining variability metrics for fertilizer treatments.

Table 2. Example of columns in the CSV file for obtaining $\hat\sigma^2$ via `MSE FINDR`in completely randomized and randomized complete block designs. 

| trial_id | fertilizer | rep |  yield  | fisher_letters|
|:---------|-----:|----:|----:|-------:|
|7         | A    | 3   | 60.2|  a     |
|7         | B    | 3   | 45.2|  c     |
|7         | C    | 3   | 55.4|  b     |
|7         | D    | 3   | 40.0|  b     |
|9         | A    | 5   | 58.2|  a     |
|9         | O    | 5   | 57.2|  a     |
|9         | T    | 5   | 55.4|  a     |
|...       | ...  | ... | ... |  ...   |



#### 2.3. Two-way factorial designs
In certain cases, primary researchers may wish to examine the influence of a second treatment factor on a response variable, as exemplified in step 1's example 2. A two-way design is an experimental setup where data is gathered for all possible combinations of the levels of the two factors of interest. The simplest version of this design is the full factorial design, which explores two main effects, `A` and `B`, along with the interaction effect `A x B`.

Based on the ANOVA results, primary studies may present means and post-hoc test outcomes for either `A`, `B`, or `A × B`. The main effect of `A` or `B` represents the influence of one independent variable on the dependent variable, disregarding the effects of all other independent variables. When an interaction effect exists, authors of primary reports generally provide means and post-hoc test results for the combined treatment factors `A × B`. However, in the absence of an interaction effect, authors may choose to compare only the factors `A` or `B` or sometimes both independently. 

When the `A × B` interaction **is significant** in primary studies and we want to obtain:

1. $\hat\sigma^2$ from mixed comparisons: Comparisons between `A × B`.

When the `A × B` interaction **is not significant** in primary studies and we want to obtain:

2. $\hat\sigma^2$ from comparisons among `A` or `B` levels (used interchangeably in two-way factorial designs)

In the following two sections we explain how the CSV input file should be assembled for two-way designs with complete or omitted treatment information.

##### 2.3.2. `A × B` interaction **is significant** (i.e., both factors included)
Here comparisons were performed for the `A × B` interaction, so both main effects `A` (e.g., *fertilizer*) and `B` (e.g., *cultivar*) need to be included as independent columns in the CSV input file, as shown in Table 4. They can take either numerical or categorical values. All the other information (*trial_id*, *rep*, etc.) remains the same as in one-way CRD or RCBD. In this example, we are interested in obtaining variability metrics for fertilizer x cultivar treatments and as a result, we must include information about both main effects.

Table 4. Example of columns in the CSV file for obtaining $\hat\sigma^2$ via `MSE FINDR`in factorial designs with A × B interaction present. 

| trial_id | fertilizer | cultivars | rep |  yield  | fisher_letters|
|:---------|-----:|------:|----:|----:|-------:|
|1         | A    |   Z     |  3   | 60.2|  a     |
|1         | B    |   Z     |  3   | 45.2|  c     |
|1         | C    |   Z     |  3   | 55.4|  b     |
|1         | A    |   Y     |  3   | 40.0|  b     |
|1         | B    |   Y     |  3   | 58.2|  a     |
|1         | C    |   Y     |  3   | 57.2|  a     |
|2         | A    |   H     |  6   | 55.4|  a     |
|...       | ...  | ... | ... | ... |  ...   |


##### 2.3.1. `A × B` interaction **is not significant** (i.e., one of the factors is omitted)

When comparisons were performed only among main effects `A` or `B`, users still must provide an additional information regarding the omitted factor. This is achieved by specifying the **number of levels for the omitted factor B (e.g., n.cultivar)** in the in the CSV input file (Table 3). Ignoring this will lead to incorrect calculation of degrees of freedom. In Table 3, the **n.cultivars** represents the number of levels of the omitted factor, which is typically informed in material and methods section of primary reports. In this example, we are interested in obtaining variability metrics for fertilizer treatments and not the interaction, however, because this is a two-way design study, we must include information about cultivar treatment levels in numerical values, as omitted factor. 

Table 3. Example of columns in the CSV file for obtaining $\hat\sigma^2$ via `MSE FINDR`in factorial designs with a factor `B` omitted. 

| trial_id | fertilizer | n.cultivars | rep |  yield  | fisher_letters|
|:---------|-----:|--------:|----:|----:|-------:|
|1         | A    |   5     |  3   | 60.2|  a     |
|1         | B    |   5     |  3   | 45.2|  c     |
|1         | C    |   5     |  3   | 55.4|  b     |
|1         | D    |   5     |  3   | 40.0|  b     |
|2         | A    |   4     |  5   | 58.2|  a     |
|2         | B    |   4     |  5   | 57.2|  a     |
|2         | C    |   4     |  5   | 55.4|  a     |
|...       | ...  | ... | ... | ... |  ...   |


Final note on two-way factorial designs: `A` and `B` can be used interchangeably for $\hat\sigma^2$ recovery purposes because there is only one residual variance in ANOVA table. This is NOT the case for split-plot designs. However, the CSV input file assembly is identical between these two designs. Users must know (and later specify in the tool) whether main effect `A` or `B` were assigned to main-plot or sub-plot units in split-plot designs, as explained below.

#### 2.4. Two-way split plot designs

The CSV input file is assembled identically for two-way factorial and split-plot designs. However, there are fundamental differences during the estimation steps. For split-plot designs, $\hat\sigma^2$ extraction depends on two aspects: whether there are significant `A x B` interactions & whether the comparisons which we want to obtain $\hat\sigma^2$ were assigned to main-plot or sub-plot units. 

Throughout for split-plot designs, we will refer to `A` treatments located in the main-plot and `B` to treatments assigned to sub-plot units. Here the arrangement of treatments to `A` and `B` matters because there are two variances in split-plot ANOVA, and so, the main effects cannot be used interchangeably. This nomenclature is also used in the `MSE FINDR`. 

Because this hierarchical structure of main- and sub-plots & in contrast to factorial designs, split-plot designs could result in five distinct estimation procedures: 

When the `A × B` interaction **is not significant** in primary studies and we want to obtain:

1. $\hat\sigma^2$ from comparisons among `A` (main-plot) levels
2. $\hat\sigma^2$ from comparisons among `B` (sub-plot) levels

When the `A × B` interaction **is significant** in primary studies and we want to obtain:

3. $\hat\sigma^2$ from comparisons among B levels (sub-plot) within A levels (main-plot) - (`B within A`)
4. $\hat\sigma^2$ from comparisons among A levels (main-plot) within B levels (sub-plot) - (`A within B`)
5. $\hat\sigma^2$ from mixed comparisons: Comparisons between B (sub-plot) levels across different A levels (main-plot) - `mixed`


`MSE FINDR` CAN ONLY compute $\hat\sigma^2$ for scenarios **1, 2, and 3** . For scenario 5, $\hat\sigma^2$ consists of a weighted average between $\hat\sigma^2_A$ (main-plot error) and $\hat\sigma^2_B$ (sub-plot error), which is considerably more complex to obtain. We do not cover technical details of scenarios 4 and 5, however, we urge users to be aware whether $\hat\sigma^2$ is being obtained from comparisons in main or sub-plot because mispecifications can result in severely wrong $\hat\sigma^2$ calculations. A good resource to understand more of these designs can be found [here](https://psfaculty.plantsciences.ucdavis.edu/agr205/Lectures/2011_Lectures/L12a_SplitPlot.pdf).

> **Note:** The Estimator module includes a dynamic Instructions panel that displays a prominent warning when split-plot designs are selected, reminding users that main-plot and sub-plot assignments are not interchangeable and that misspecification will result in incorrect MSE estimates.

As mentioned before, collating trial data into the CSV input file for split-plot designs is identical to that of two-way factorial designs. However, users must be mindful of whether the main effect comparisons or source of variation was assigned to factor `A` (main-plot) or `B` (sub-plot) units. When `A × B` interaction **is significant**, only one scenario for $\hat\sigma^2$ extraction can be performed.

Downloadable example files to assist with the collation process can be found in the `MSE FINDR` app. Again, for additional information on estimating for split-plot designs, check [this](https://psfaculty.plantsciences.ucdavis.edu/agr205/Lectures/2011_Lectures/L12a_SplitPlot.pdf). 


### Step 3 - Upload the CSV file

In this step, users upload the processed CSV input file containing the all required information, as described above. 

Alternatively, users can skip this step by using the **"Load Example Data"** button on the Disclosure page to load a pre-formatted example dataset directly into the application. This is useful for exploring the tool's functionality before preparing your own CSV file.


### Step 4 - Make field selections and estimate

Once the trial data has been appropriately organized in the CSV input file and uploaded, users can proceed with the calculation of $\hat\sigma^2$ using the Estimator module. In this module, users are required to specify the experimental design, post-hoc test, and significance level for all trials in the CSV input file using the design box (Figure 1).

As users select different experimental designs in the design box, the selection fields in the column assignment box will be updated dynamically. This ensures that the appropriate options are presented based on the chosen experimental design. To proceed successfully, users must ensure that the columns in the CSV input file are correctly matched with their corresponding selection fields in the column assignment box.

To assist users with the matching process, info signs are available in the tool, providing additional information on column matching and ensuring a smooth and accurate setup.

### Step 6 - Export file

Once the estimate button is clicked, a download button will appear alongside a results table. Users can export the file containing the estimates of degrees of freedom and $\hat\sigma^2$ generated by  `MSE FINDR` for each trial, along with the information previously included in the CSV input file.

Note: Users can opt to manually calculate the degrees of freedom for a few trials and compare them to the  `MSE FINDR` estimates to verify the accuracy of the calculated $\hat\sigma^2$. It is important to note that only one $\hat\sigma^2$ value is calculated for each trial. In cases where non-significant post-hoc test results are included (e.g., all letters are the same for a trial),  `MSE FINDR` returns "".


## References

Ngugi, H.K., Lehman, B.L. and Madden, L.V., 2011. Multiple treatment meta-analysis of products evaluated for control of fire blight in the eastern United States. Phytopathology, 101, 512-522.

Milliken, G.A., & Johnson, D.E., 2009. Analysis of Messy Data Volume 1: Designed Experiments, Second Edition. Chapman and Hall/CRC

Montgomery, D.C., 2017. Design and analysis of experiments. John Wiley & Sons.

## License

MIT License

Copyright (c) 2022–2026 Vinicius Garnica and others

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
