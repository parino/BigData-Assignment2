# Assignment 2: Predictive modeling

### The second assignment consists of the construction of a predictive model.

The data set hmeq.csv (attached) reports characteristics and delinquency information for home equity loans. A home equity loan is a loan where the obligor uses the equity of his or her home as the underlying collateral. (Data set thanks to http://www.creditriskanalytics.net.)

Your task is to construct a predictive model in order to predict whether applicant defaulted on loan (or seriously delinquent) or not

Your model needs to be build using R or Python
*	As an environment, you can use Jupyter (Lab), RStudio, Google Colaboratory or Microsoft Azure Machine Learning Studio
*	Other environments are fine too, but it needs to be based on R or Python
*	The data set is already relatively clean, and a binary label has been constructed for you
*	R and RStudio can be downloaded from https://cran.r-project.org/ and https://www.rstudio.com/. Python and Jupyter can be installed using the Anaconda Distribution
*	A description of the data set follows below

The second part of your lab report should contain:
*	A clear overview of your whole modeling pipeline, including approach, exploratory analysis (if any), preprocessing (if any), construction of model, set-up of validation and results of the model
*	Feel free to use code fragments, tables, visualisations, ...
*	You can use any predictive technique you want. The focus is not on the absolute outcome, but rather on the general setup, critical thinking, and the ability to get and validate an outcome
*	You can use the code fragments from the slides to get started (not required, however!), but do keep in mind that many things are still missing there, so Google around, read documentation, find tutorials, etc!
*	Take care of: performing a thorough validation, treating categorical variables if you need to, dealing with class imbalance, selecting an appropriate metric
*	You're free to use unsupervised technique for your data exploration part, too. When you decide to build a black box model, you're free to experiment and include ideas regarding making the result more understandable
*	Any other assumptions or insights are thoughts can be included as well: the idea is to take what we've seen in class, get your hands dirty and reflect on what we've seen

As discussed in class: you're free to schedule your planning and division of work. You do not hand in this assignment separately, but hand in your completed lab report containing all four assignments (including this one) on Sunday June 3th .
The following metadata is provided for the data set:

*	BAD: 1 = applicant defaulted on loan or seriously delinquent; 0 = applicant paid loan
*	LOAN: Amount of the loan request
*	MORTDUE: Amount due on existing mortgage
*	VALUE: Value of current property
*	REASON: DebtCon = debt consolidation; HomeImp = home improvement
*	JOB: Occupational categories
*	YOJ: Years at present job
*	DEROG: Number of major derogatory reports
*	DELINQ: Number of delinquent credit lines
*	CLAGE: Age of oldest credit line in months
*	NINQ: Number of recent credit inquiries
*	CLNO: Number of credit lines
*	DEBTINC: Debt-to-income ratio
