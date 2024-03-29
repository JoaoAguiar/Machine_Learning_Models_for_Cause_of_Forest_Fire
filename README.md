# Machine Learning Models for Cause of Forest Fire
> Multiple Machine Learning Models that make their predictions about the cause type (intentional or non-intentional) of the various forest fires in Portugal between 2014 and 2015

## About
Forest fires are a very important issue that negatively affects climate change. Typically, the causes of forest fires are those oversights, accidents and negligence committed by individuals, intentional acts and natural causes. The latter is the root cause for only a minority of the fires.

Their harmful impacts and effects on ecosystems can be major ones. Among them, we can mention the disappearance of native species, the increase in levels of carbon dioxide in the atmosphere, earth’s nutrients destroyed by the ashes, and the massive loss of wildlife.

Data mining techniques can help in the prediction of the cause of the fire and, thus, better support the decision of taking preventive measures in order to avoid tragedy. In effect, this can play a major role in resource allocation, mitigation and recovery efforts.

The ICFN - Nature and Forest Conservation Institute has the record of the list of forest fires occurred in Portugal for several years. In the file fires2015_train.csv, you have data on reported forest fires during 2015, for which the cause is known.

The goal of this is to find and build the most suitable machine learning model to predict the cause type (intentional or non-intentional) of a forest fire in Portugal.

## Attributes
The attributes, of the fires2015_train.csv, have information regarding the forest fire’s alarm point and the total affected area:

* id
* region
* district
* municipality
* parish
* lat
* lon
* origin
* alert_date
* alert_hour
* extinction_date
* extinction_hour
* firstInterv_date
* firstInterv_hour
* alert_source
* village_area
* vegetation_area
* farming_area
* village_veget_area
* total_area
* cause_type
* intentional_cause
* max_temperature
* min_temperature
* avg_temperature
* precipitation

## Future Work
In order to further improve our work, we would like to do more experiments, try more algorithms, more variables and more configurations, in particular we would like to explore Artificial Neural Networks.
