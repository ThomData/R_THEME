# R_THEME
THEmatic Model Exploration is a both exploratory and predictive multiblock-multiequation-technique.
The dataset is partitioned by the user into thematic blocks of variables. The thematic blocks are linked by a thematic model consisting of one or several equations.
Each equation links a dependent theme to explanatory themes. THEME searches each block for a given number of components capturing the theme's information that best serves the overall model.
Then, THEME can perform cross-validation-based backward component-selection to identify the number of really useful components in themes.
This backward selection produces a decreasing sequence of models, each of which is associated with a vector of component-numbers in themes.
THEME outputs the prediction-error rates of dependent variables for each model in the sequence, so that the user can compare the performance of the models in detail.
THEME also outputs the loadings of variables on components in all themes, the coefficients of the component-regression models, among other relevant numeric information.
