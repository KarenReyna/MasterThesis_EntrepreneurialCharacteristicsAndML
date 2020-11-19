# Advanced Statistical Approaches to Analyse Entrepreneurial Characteristics: A Machine Learning Approach
Master Thesis

This is a repository for my master thesis for the [Master of Science in Management](https://www.wi.tum.de/programs/master-in-management/). My thesis was supvervised by M.Sc. Leidy Cubillos from the Chair of Research and Science Management at the Technical University of Munich.

## Abstract

**Background:** Previous studies have shown the important role of individuals' characteristics in Entrepreneurial Intention and that specific subforms of rule breaking favour it. Demographic and entrepreneurship features have been considered an important success factor in Entrepreneurial Intention. It is important to develop a measurement system for predicting Entrepreneurial Intention based on the individual’s entrepreneurial, psychological, and demographic characteristics.

**Objectives:** The aim of this paper is to assess the classification problem of recognizing Entrepreneurial Intention of Unconditional Rule Followers and Conditional Rule Breakers individuals by ML methods.

**Methods/Approach:** Three methods were tested in both types of rule breakers: DT, RF, and KNN on the same dataset in order to compare their efficiency in the sense of classification accuracy. Each model was validated using a combination of the train/test split procedure and the 10-fold Cross Validation procedure in order to avoid getting biased measures of the performance of the models. The performance of the models was evaluated using accuracy, sensitivity and specificity measures; and it was improved using the hyperparameter tuning procedure.

**Results:** The DT model yielded a higher classification accuracy than the models produced by other methods. For Unconditional Rule Followers, the best method had an accuracy of 75\% with a 66:34 split. For Conditional Rule Breakers, the best method had an accuracy of 71.43\% with a 74:24 split. Both types of rule breakers differ in the individuals' characteristics and values used when defining high Entrepreneurial Intention. The results also showed that the algorithms display a chaotic response to the percentage of training and testing.

**Conclusion:** Tested ML algorithms are able to learn fast and predict the Entrepreneurial Intention in individuals. However, further work has to be done by testing the methodology on a bigger dataset in order to generalize my conclusions.

## Licence

This source code is licensed under the BSD-style license found in the LICENSE file in the root directory of this source tree.
