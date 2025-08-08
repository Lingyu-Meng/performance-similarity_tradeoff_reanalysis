Deployed as github pages at [https://lingyu-meng.github.io/performance-similarity_tradeoff_reanalysis/](https://lingyu-meng.github.io/performance-similarity_tradeoff_reanalysis/)

# Data and Code

Please download the data from [here](https://figshare.com/articles/dataset/Stan_object_of_the_winning_model_M6b_Zhang_Gl_scher_2020_/12824309) and place it in the same directory as this README file.

[m7b_model_fit.R](m7b_model_fit.R) is the code to fit the model. It requires the `rstan` package, which can be installed when you run the script.

[sit_m7b.stan](sit_m7b.stan) is the Stan model file that defines the model structure. This model is based on the M2b model from the original paper by Zhang and Gläscher (2020). The initial commit of this file was made by copying the M2b model from the original paper. Therefore, the revision can be traced.

The fitted model object is saved as `m7b.RData`. This file is not included in the repository to reduce the size of the repository. You can run the `m7b_model_fit.R` script to fit the model and save the fitted model object.

[plot_m7b.R](plot_m7b.R) is the code to plot the results of the fitted model.