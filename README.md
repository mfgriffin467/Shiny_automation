# How vulnerable are US jobs to automation in the medium-term?

“47% could be done by machines “over the next decade or two””
Carl Benedikt Frey and Michael Osborne of Oxford University

See blog post here: TBC

See Shiny IO here: https://mfgriffin.shinyapps.io/Shiny/


## Project aims:

1) Provide a dynamic tool for visualisation research on the possible impact of automation/computerisation of jobs over the medium-term
2) Update the original 2012 research to latest available information


The dashboard enables users to explore a variety of questions which could be of use to individuals, organisations and policy-makers:"),em(
1) How many jobs are at risk of computerisation?"
2) How are high risk roles distributed across states and industries?"
3)  Is my current / future role likley to be replaced?"
4)  Which skills are more or less likley to be automated"
5)  How has the landscape changed since the 2012 research?"


## Key caveats:

This analysis narrowly focusses on the theoretical potential for job substitution in the US, looking at a single measure of the estimated 
probability that job roles can be fully automated. 

This approach therefore several real-life factors:
- New jobs: in the past, technology has replaced roles but new positions have been created through the creative destruction process. This will surely continue
- Job growth: US labour does produce near-term projections for the growth of current job categories; these are excluded
- Job augmentation: the likelihood of computerisation is based on an assessment of current skills and abilities required for roles - technology may change these skillsets over time
- Policy response: action could be taken to protect roles
- Resources: in a world of limited finances and technical resource (robust data, workforce, compute etc), not all that can be automated will be automated


## Approach

a) Estimates of the likelihood of automation are based on the 2012 research available here:
https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf

b) Job breakdowns across role and state are taken for 2010, 2012, 2014, 2016 and 2018
https://www.bls.gov/oes/tables.htm

Tables are enriched to cover skill/ability/knowledge data for "bottleneck variables" identified in (a)
O*NET

c) The shiny application shows the mapping of estimates from (a) across the datasets in (b)

d) Ongoing work - Probability estimates are recalculated using the latest skill, accoutning for changes in job capabilities and new job profiles.
The Gaussian Process Classifier implemented in sklearn is used to calculate probability estimates using the Rational Quadratic kernel 
Retraining on the 2010 labels produce similar results so new estimates are obtained using latest available 2018 data


## Original research methodology

1) Machine learning exerts held a workshop to hand-label around 70 of ~ 700 occupations by eye-balling the skills requirements  collected in the O*NET database. Specifically they answered yes/no to the question:
“Can the tasks of this job be sufficiently specified, conditional on the availability of big data, to be performed by state-of-the-art computer-controlled equipment”
2) Nine variables are proposed as features to explain the likelihood of automation – these are the bottleneck variables and correspond to e level of perception and manipulation, creativity, and social intelligence.  The “level” variable is used (rather than “importance”) 
3) Probabilistic classification is used to fit the labels given the proposed features with testing on logistic regression (which uses linear modelling)  and Gaussian process classifiers (specially using the GPML toolbox for exponentiated quadratic, rational quadratic and linear covariances) .  The area under the ROC curve was used to select he best model which was found to be the exponentiated quadratic using a cross-validation approach
4) The chosen Gaussian approach accounts for non-linearities between variables



## Ongoing work

- Further work to align Gaussian modelling for 2010 datasets
- Extend to other countries?

MAJOR
Re-run ML for 2018

MINOR
Draft bog post in Word
Hide drop downs for specific tabs
Fix ranking for main visuals
