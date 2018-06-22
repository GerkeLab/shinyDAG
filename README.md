# shinyDAG

shinyDAG is a web application that uses R and Latex to create publication-quality images of directed acyclic graphs (DAGs). Additionally, the application leverages complementary R packages to evaluate correlational structures and identify minimal adjustment sets for estimating causal effects<sup>1-5</sup>. The web-based application can be accessed here  [https://gerkelab.shinyapps.io/shinyDAG/](https://gerkelab.shinyapps.io/shinyDAG/).

## Citing shinyDAG

Concept DOI: 10.5281/zenodo.1288712

v0.0.0 DOI: 10.5281/zenodo.1288713

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

## References
1. Richard Iannone (NA). DiagrammeR: Graph/Network Visualization. R package version 1.0.0.
  [https://github.com/rich-iannone/DiagrammeR](https://github.com/rich-iannone/DiagrammeR).

## About Gerke Lab

Check us out at [http://www.travisgerke.com](http://www.travisgerke.com)

![alt text](https://github.com/GerkeLab/hexLogoGerke/raw/master/GerkeLab-1200dpi-square.png "Gerke Logo")

<!--![Alt Text](https://media.giphy.com/media/vFKqnCdLPNOKc/giphy.gif)-->

