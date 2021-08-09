# Sleep quality prediction in caregivers using physiological signals

Last modification: 07/01/2021 by Charan Pinninty (pinninty.2@wright.edu)

Sleep quality in caregivers

Changes in code:
For all NA values are filled with the mean of the other row values.
Added lines for generating confusion matrix, precision, sensitivity, specificity and F1 score for all three classifiers
Added lines for generating accuracy and confusion matrix in evaluating measures – sleep quality and feeling rested
Added lines for generating pictures of plots in the code to save in local disk in .png format

New code:
Lasso regression on baseline models
Finding best seed value to find maximum accuracy from seed value 1-1000 

Analysis:
Accuracies and F1 scores for all 21 experiments
Accuracies and F1 scores for all participants
Mean absolute error, mean squared error and Root mean squared error for linear sleep measure
Accuracies and F1 scores for lasso regularization

Experiments:
With all 18 participants’ data, each experiment has one participant excluded and the data is trained on other 17 participants and tested on the excluded participant.
After 18 experiments, the 19th experiment is on the data which is collected only in participants of Spring 2021 and Summer 2021.
The 20th experiment is about including all the 18 participants' data, trained and tested at 80:20 split.
The 21st experiment is about regrouping the classes in sleep quality from 5 classes to 3 classes, from {0,1,2,3,4} to {0,1,2}

The source code of paper:

R. Sadeghi, T. Banerjee, J.C. Hughes, L.W. Lawhorne, Sleep quality prediction in caregivers using physiological signals, Computers in Biology and Medicine (2019), doi:https://doi.org/10.1016/j.compbiomed.2019.05.010.

URL: https://www.sciencedirect.com/science/article/pii/S001048251930160X#!

Video: https://youtu.be/D7P9eoM4KOo

Featured in: https://www.empatica.com/blog/using-the-e4-to-assess-sleep-in-caregivers-of-people-with-dementia.html

Implementation details: This project has been implemented by **R version 3.5.0** and **MATLAB R2018b**

This repository contains the source code, final models, and demos of this project. Due to the IRB restrictions, we are limited to publishing the orignal time series. However, we release the final models with the aim of reproducibility and comparability of our models for researchers.

Please, cite the paper and star this implementation if you find this implementation useful. Also, feel free to contact me if you need any further information via the following link.

https://rezasadeghiwsu.github.io/Website/
