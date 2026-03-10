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
end up in the directory `figures`.

## Usage

There are two R scripts that generate figures, `main.R` and `custom.R`. They 
both use functions from the supplementary files found in the directory `R`.

- `utils.R` contains utility functions.
- `clean.R` contains functions used for cleaning the datasets, and is thus
highly specific to the data.
- `settings.R` contains some global settings for visualisation purposes.
- `mc_byquestion.R` contains functions that generate a patchwork plot for a 
single Likert-style multiple choice question. Each subplot is a plot of the
answers to the question disaggregated by a certain category (or the total).
- `mc_bybase.R` contains functions that generate a patchwork plot for a set of
Likert-style multiple choice questions, disaggregated by a given category. 
Each subplot is a plot of the answers to the questions, given a particular
answer to the category.

The file `main.R` cleans the data using `clean.R`, and then generates some 
figures using `mc_byquestion.R` and `mc_bybase.R`. The file `custom.R` also
cleans the data, and then generates some custom figures for specific questions.







