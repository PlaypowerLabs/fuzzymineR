# fuzzymineR #  

#### R Package to do Fuzzy Process Mining ####  

[![Travis-CI Build Status](https://travis-ci.org/nirmalpatel/fuzzymineR.svg?branch=master)](https://travis-ci.org/nirmalpatel/fuzzymineR)  

### What is Process Mining? ###

Process Mining is a data mining technique that allows us to build process models from event logs. For example, we can use audit trail logs, transaction logs, or any other kind of event logs to build models that approximate the process behind the series of events. Process models give us a compact end-to-end representation of the processes at play that are 'causing' the observed logs. Process mining started in the business process domain, but it has since been used in various other domains such as health and education. For more info, visit the [Process Mining website](http://www.processmining.org).

### About fuzzymineR ### 

There are a few different tools available to do process modeling. Two of the most famous ones are [ProM](http://promtools.org) and [Disco](https://fluxicon.com/disco/). ProM is free and open source, and contains a plethora of process mining algorithms. We figured out that out of all of the available algorithms, Fuzzy Process Mining was probably the most useful process modeling algorithm when it came to mining real world data as this algorithm provided many different abstractions over the process. Fuzzy Process Mining algorithm was not available in R, so we built fuzzymineR to use the Fuzzy Process Mining algorithm available in ProM.

In fuzzymineR, to build a process model from event log data, a user has to only provide a `data.frame` that contains an event log. fuzzymineR provides a way to create an object of class [bupaR::eventlog](https://rdrr.io/cran/bupaR/man/eventlog.html) (or you can create the object directly yourself) and that object can be used to create a Fuzzy Process Model. Once the model is mined, you can visualize the model by setting the parameters of the visualization function and get the desired level of abstraction that you would like to work with. For process discovery, fuzzy mining algorithm is considered one of the best algorithms because it can deal with less-structered event data and it allows you do look at the process from different abstraction levels. Data complexity is a typical issue and fuzzy mining algorithm often saves you from creating extremely complex *spaghetti models*.

### ProM Version ###

Our package carries ProM JAR files within it. Right now, we are using ProM 6.10 JAR files.

### System Requirments ###  

Your system should have **Java 8**. We have not tested this package on other Java versions.

### Functions ###  

* `create_eventlog`: Creates eventlog object from a `data.frame`  
* `mine_fuzzy_model`: Applies fuzzy mining algorithm on the eventlog object and returns a list of metrics  
* `viz_fuzzy_model`: Creates process model after processing the list of metrics  

### Installation ###

To install the package, use the following command:

```r
devtools::install_github("nirmalpatel/fuzzymineR")
```

### Example ###  

After installing the package, please run the following example code:

```r
library(fuzzymineR)

# Load the sample dataset
data("artificial_loan_process")

# Create an eventlog object
log <- create_eventlog(artificial_loan_process,
                      case_id = "case",
                      activity_id = "event",
                      timestamp = "completeTime")
                      
# Mine the fuzzy model
metrics <- mine_fuzzy_model(log)

# Visualize the fuzzy model for a given set of
# parameters
viz_fuzzy_model(metrics = metrics,
   node_sig_threshold = 0,
   edge_sig_threshold = 0.3,
   edge_sig_to_corr_ratio = 0.75)
```

Pleae read the documentation or the research paper linked below for understanding the different metrics in the `viz_fuzzy_model` function.

### Useful Links ###  

[Fuzzy Mining - Adaptive Process Simplification  Based on Multi-Perspective Metrics](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.81.1207&rep=rep1&type=pdf)

### Authors ###

1. Tirth Shah (Playpower Labs)
2. Nirmal Patel (Playpower Labs)

