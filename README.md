# fuzzymineR #  

#### R Package to do Fuzzy Process Mining ####  

[![Travis-CI Build Status](https://travis-ci.org/nirmalpatel/fuzzymineR.svg?branch=master)](https://travis-ci.org/nirmalpatel/fuzzymineR)  


### About fuzzymineR ### 

fuzzymineR package consists of functions which are helpful when a user wants to 
build process model from event log data.It contains functions by which a user has to only provide a CSV file and by applying a series of functions one can mine process models.fuzzymineR provides a way to create Adaptive Process Models in which you can configure the simplification level by setting the parameters. Hence users can get the derired level of abstraction that they did like to work with. For process discovery, fuzzy mining algorithm is considered one of the best algorithms because it can deal with less-structered event data(which is the case with real world data) very well and saves you from creating Spaghetti models.


### Requirments ###  

System should have **java 8** or higher version. The fuzzy algorithm is written in java and hence it is required to install java on your system.

### About Functions ###  

**Create Eventlog :** creates eventlog object from a CSV file  

**mine_fuzzy_model :** applies fuzzy mining algorithm on the eventlog object and returns a list of metrics  

**viz_fuzzy_model :**creates process model after processing the list of metrics  


### Example ###  

`library(fuzzymineR)`  
`data("artificial_loan_process")`  
`log <- create_eventlog(artificial_loan_process,`  
`                       case_id = "case",`  
`                       activity_id = "event",`  
`                       timestamp = "completeTime")`  
`metrics <- mine_fuzzy_model(log)`  
`viz_fuzzy_model(metrics = metrics,`  
`    node_sig_threshold = 0,`  
`    edge_sig_threshold = 0.3,`  
`    sig_weights = c(1,0),`  
`    corr_weights = c(0,0,0,0,0))`  

    
This creates a process model simply based on the frequency of the ordering of the
activities i.e just based on Freqency Binary Significance.  


### Useful Links ###  

[Fuzzy Mining - Adaptive Process Simplification  Based on Multi-Perspective Metrics](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.81.1207&rep=rep1&type=pdf)  

