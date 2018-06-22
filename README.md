# shinyDAG

shinyDAG is a web application that uses R and Latex to create publication-quality images of directed acyclic graphs (DAGs). Additionally, the application leverages complementary R packages to evaluate correlational structures and identify minimal adjustment sets for estimating causal effects<sup>1-4</sup>. The web-based application can be accessed here  [https://gerkelab.shinyapps.io/shinyDAG/](https://gerkelab.shinyapps.io/shinyDAG/).

## Using shinyDAG

### Adding nodes and edges

![Alt Text](https://github.com/GerkeLab/ShinyDAG/raw/master/Figures/AddNodeEdge.gif)

### Editing DAG aesthetics

![Alt Text](https://github.com/GerkeLab/ShinyDAG/raw/master/Figures/editEdge.gif)

## Example of shinyDAG

The following DAG was reproduced from "A structural approach to selection bias" by Hernan, Hernandez-Diaz, and Robins (Figure 6A) using the shinyDAG web app.

![alt text](https://github.com/tgerke/ShinyDAG/raw/master/Figures/example1.png "Hernan Example")

The DAG represents a study on the effects of antiretroviral therapy (E) on AIDS risk (D), where immunosuppression (U) is unmeasured. L represents presense of symptoms (such as fever, weight loss, CD4 count ...) and C represents censoring.

ShinyDAG provides the user with the information that in order to estimate the direct the direct effect of E on D the user should adjust for none of the variables provided.

![alt text](https://github.com/tgerke/ShinyDAG/raw/master/Figures/adjustSets.png "shinyDAG output")

The DAG from the original article is shown below.

![alt text](https://github.com/tgerke/ShinyDAG/raw/master/Figures/example1_hernan.png "Hernan Original")

## Accessing shinyDAG

Simply access [https://gerkelab.shinyapps.io/shinyDAG/](https://gerkelab.shinyapps.io/shinyDAG/) through the web browser of your choosing!

## Using shinyDAG

The web based tool allows users to build their DAG based completely on user inputs. The resulting DAG aesthetics can then be altered to meet the users preferences. After selecting for the exposure and outcome, all open and closed paths are displayed as well as minimal adjustment sets and conditional independencies.

## Citing shinyDAG

Concept DOI: 10.5281/zenodo.1288712

v0.0.0 DOI: 10.5281/zenodo.1288713

## References

1. Richard Iannone (NA). DiagrammeR: Graph/Network Visualization. R package version 1.0.0.
  [https://github.com/rich-iannone/DiagrammeR](https://github.com/rich-iannone/DiagrammeR).
1. Johannes Textor and Benito van der Zander (2016). dagitty: Graphical Analysis of Structural Causal
  Models. R package version 0.2-2. [https://CRAN.R-project.org/package=dagitty](https://CRAN.R-project.org/package=dagitty).
1. Malcolm Barrett (2018). ggdag: Analyze and Create Elegant Directed Acyclic Graphs. R package
  version 0.1.0. [https://CRAN.R-project.org/package=ggdag](https://CRAN.R-project.org/package=ggdag).
1. Csardi G, Nepusz T: The igraph software package for complex network research, InterJournal,
  Complex Systems 1695. 2006. [http://igraph.org](http://igraph.org).

## About Gerke Lab

Check us out at [http://www.travisgerke.com](http://www.travisgerke.com)

![alt text](https://github.com/GerkeLab/hexLogoGerke/raw/master/GerkeLab-1200dpi-square.png "Gerke Logo")

<!--![Alt Text](https://media.giphy.com/media/vFKqnCdLPNOKc/giphy.gif)-->

