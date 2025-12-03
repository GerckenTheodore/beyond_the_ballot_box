# Beyond the Ballot Box: Towards A Comprehensive Measure of Minor Party Success

*Theodore Gercken - Hamilton College*

This repository contains the code used to write the paper "Beyond the Ballot Box: Towards A Comprehensive Measure of Minor Party Success," which proposes a new measure of minor party success: I-Scores, which are a quantitative measure of the extent to which a minor party influences the major parties in its political environment. The functionality to calculate I-Scores is published in the [`minorparties`](https://gerckentheodore.github.io/minorparties/) R package, which this code is built around.

The `analysis.R` script builds the paper's data set from the raw platforms in `/platforms`, saves the data it generates in `data`, and saves the plots it creates in `plots`. `analysis.R`:

- Calculates and plots the I-Scores (along with confidence intervals) for minor parties that won more than 0.5% of the presidential vote in a given election between 1948 and 2024.

- Creates tables to summarize Ross Perot's I-Score calculations and display the I-Scores of all studied minor parties.

- Calculates the correlation between parties' I-Scores and the number of votes and legislative seats they won.

- Tests the method's robustness by recalculating I-Scores for a wide range of possible values for the arbitrarily chosen variables, calculating the significance of the choices and plotting their effect, and carrying out an ANOVA test to determine what proportion of variance in the scores was explained by the choices.

- Demonstrates how to test for causality by plotting position scores on "Economic Orthodoxy" over time and conducting an interrupted time series analysis to determine whether Ross Perot's 1992-1996 movement had a statistically significant effect on these trends.
