# Survey data analysis

This is the code for the data analysis of the UU GSNS PhD survey from November 
2025. 

## Installation

In your terminal, navigate to the location you want this repository to be, and 
clone it with the following command.

```
git clone git@github.com:tjvanleeuwen/survey.git
```

Move your `csv` datasets to the directory `data`. Open the `survey.Rproj` 
Rproject in Rstudio, and open the files `main.R` and `custom.R` found in the
directory `scripts`. They can now be run to generate some figures, which will 
end up in the directory `figures/`.

## Contents

There is one main R script that generates figures, `main.R`. It makes use of
functions from the following supplementary files found in the directory `R`.

- `utils.R` contains utility functions.
- `functions.R` contains longer functions that do not generate functions.
- `clean.R` contains functions used for cleaning the datasets, and is thus
highly specific to the data.
- `statistics.R` contains functions that run a Wilcox test on the answers to
a yes/no style question, disaggregated by a category.
- `settings.R` contains some global settings for visualisation purposes.
- `mc_byquestion.R` contains functions that calculate the averages for a set
of Likert style multiple choice functions, and then generate a patchwork plot. 
Each subplot is a plot of the averaged answers disaggregated by a certain 
category (or the total).
- `mc_bybase.R` contains functions that generate a patchwork plot for a set of
Likert style multiple choice questions, disaggregated by a given category. 
Each subplot is a plot of the answers to the questions, given a particular
answer to the category.
- `custom.R` contains some functions that generate a patchwork plot for a 
non-Likert style multiple choice question.

## Usage

The file `main.R` cleans the data using `clean.R`, and runs the Wilcox test 
from `statistics.R` to investigate gender and nationality biases within the
department / institute groups. Then the figures for the Likert style questions 
are generated, using `mc_byquestion.R` and `mc_bybase.R`. Finally, a custom
figure is generated for each of two non-Likert style multiple choice questions.

The figures are saved twice, once numbered in `figures/ordered/`, and once 
named in `figures/named/`. A final directory `figures/test/` exists for testing 
purposes.







