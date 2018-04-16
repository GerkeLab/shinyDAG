# shinyDAG
___

shinyDAG is a web application and R program that creates and assesses directed acyclic graphs (DAGs). This program is built upon the previous work of dagR for evaluating DAGs and Latex for producing publication quality DAG images. The web-based application can be accessed here  [https://gerkelab.shinyapps.io/shinyDAG/](https://gerkelab.shinyapps.io/shinyDAG/).

## Citing shinyDAG
___

Coming soon!

## Example of shinyDAG
___

The following DAG was reproduced from "A structural approach to selection bias" by Hernan, Hernandez-Diaz, and Robins (Figure 6A) using the shinyDAG web app.

![alt text](https://github.com/tgerke/ShinyDAG/raw/master/Figures/example1.png "Hernan Example")

## Accessing shinyDAG 
___

### Web-based version

Simply access [https://gerkelab.shinyapps.io/shinyDAG/](https://gerkelab.shinyapps.io/shinyDAG/) through the web browser of your choosing!

### Locale version

To access the locale version simply run the following code in your R console.

```R
library(shiny)
runGitHub("ShinyDAG", "tgerke", subdir = "locale/")
```

In order to run the locale version users should have Latex already installed on their machine. For optimal performance, users should install the necessary packages themselves to avoid duplicates or issues with dependencies. 

## Using shinyDAG 
___

### Web-based

The web based tool allows users to build their DAG based completely on user inputs. The resulting DAG aesthetics can then be altered to meet the users preferences. After selecting for the exposure and outcome, all open and closed paths are displayed as well as minimal adjustment sets. 

### Locale 

The locale version allows users to building their DAG from pre-existing data. Users can edit names obtained from the data that the user uploads, which are then used to create the DAG. Editing and assessing the DAG works the same as in the web-based version. The locale version also has the additional capability of running models based on the minimal adjustment sets on the provided data.  

## Additional Examples
___

![alt text](https://github.com/tgerke/ShinyDAG/raw/master/Figures/buildView.png "Build View")

![alt text](https://github.com/tgerke/ShinyDAG/raw/master/Figures/modelView.png "Model View")

## About Gerke Lab 
___

Check us out at [http://www.travisgerke.com](http://www.travisgerke.com)


