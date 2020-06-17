## Standard Error efficacy and its influence on reliability in clustered data - Comparing linear models and hierarchical linear Models
This Master thesis is split in two parts. In the first part multilevel theory is explained on a very basic level. Goal of this part is that psychology students can get an easy understanding of multilevel analysis and it's application. 

The second part of my master thesis contains a simulation study. The main point of this study is to show that if effects of clustering is present within the data, it's advisable to use hierarical linear modelling (HLM) instead of normal multiple linear modelling (MLM). The level of clustering in the simulated data was manipulated through the intraclass correlation (ICC), where an ICC of 0 describes a sample with no effects of clustering and an ICC of 1 describes a sample with high effects of clustering. ICCs used in this study ranged from 0 to 0.5.

Two outcome measures where calculated for each condition: 
* **Standard Error (SE) Efficacy:** Precision of the SE estimation of a given method. Underestimating tends to result in higher type 1 errors and overestimating tends to result in higher type 2 errors.
* **Statistical Power:** The probabillity to reject the null hypothesis when an alternative hypothesis is true.

Results showed that HLM had good SE Efficacy even with high ICC. Wheres MLM showed under- and overestimating of SE with increasing ICCs. Concerning Statistical Power HLM showed again better power as MLM. The difference in power grew with increasing ICC.

In combination with this master theses a Shiny App was written for an easy and more interactive way for students to understand Multilevel Analysis. Additionally the results of my simulation study are also presented in the Shiny App.

Link to the Shiny App: https://noboss.shinyapps.io/master_thesis_app/
