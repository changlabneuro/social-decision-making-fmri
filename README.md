Social Decision-Making Task ReadMe

The Matlab files take fit parameters and test behavioral models for either intertemporal choice (examples included) or risky choice. For these, run “social_decision_model_main_script.m” which calls the others. This code is heavily adapted from code developed in Lockwood et al., Nature Human Behaviour, 2017.

Lockwood, P. L. et al. Prosocial apathy for helping others when effort is required. Nat Hum Behav 1, 0131, doi:10.1038/s41562-017-0131 (2017).

The R files take betas derived from fMRI along with associated trial labels to train/test classifiers for MVPA. These files also allow for permutation of labels to creat a null distribution and then compare the actual decoding accuracy to that null distribution. For this analysis, run “social_decision_MVPA.R” and “social_decision_MVPA_permutations_pt1.R” first (you can run these concurrently). Once both are finished, you can get a P-value (because I know you want that) from “social_decision_MVPA_permutations_pt2.R” which compares the actual with the null distribution. My thanks to Kayla Velnoskey who wrote the code on which these are based in 2017.

If you have any questions, look within yourself. You’ll find that you already have the answers.
