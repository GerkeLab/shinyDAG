# shinyDAG

shinyDAG is a web application that uses R and LaTeX to create publication-quality images of directed acyclic graphs (DAGs). Additionally, the application leverages complementary R packages to evaluate correlational structures and identify appropriate adjustment sets for estimating causal effects<sup>1-4</sup>. The web-based application can be accessed at [https://apps.gerkelab.com/shinyDAG/](https://apps.gerkelab.com/shinyDAG/).

## Key operations

### Adding nodes and edges

![Alt Text](https://github.com/GerkeLab/ShinyDAG/raw/master/Figures/AddNodeEdge.gif)

### Editing DAG aesthetics

![Alt Text](https://github.com/GerkeLab/ShinyDAG/raw/master/Figures/editEdge.gif)

## Examplary usage

The following DAG was reproduced from "A structural approach to selection bias"<sup>5</sup> (Figure 6A) using the shinyDAG web app.

![alt text](https://github.com/tgerke/ShinyDAG/raw/master/Figures/example1.png "Hernan Example")

For comparison, the DAG from the original article is shown below.

![alt text](https://github.com/tgerke/ShinyDAG/raw/master/Figures/example1_hernan.png "Hernan Original")

The DAG represents a study on the effects of antiretroviral therapy (E) on AIDS risk (D), where immunosuppression (U) is unmeasured. L represents presence of symptoms (such as fever, weight loss, and diarrhea) and C represents censoring. A spurious path exists between E and D due to selection bias. We can see this in shinyDAG by ensuring that we've selected E as the exposure, D as the outcome, adjusted for C, and then toggling the "Examine DAG elements" button in the bottom left corner. The spurious open path is displayed as D <- U -> L -> C <- E.

![alt text](https://github.com/tgerke/ShinyDAG/raw/master/Figures/paths.png "shinyDAG path output")

One possible resolution for this bias is to adjust for L. After toggling L in the "Select nodes to adjust" section, we see that all spurious E to D paths are now closed.

![alt text](https://github.com/tgerke/ShinyDAG/raw/master/Figures/paths2.png "shinyDAG final path output")

## Other features

In addition PDF and PNG exports, users can download R objects in `ggdag` or `daggity` formats, as well as the source LaTeX code. The "Edit LaTeX" pane permits in-app modification of the LaTeX code with a preview window; however, users should be aware that the information in "Examine DAG elements" is not responsive to changes in the Edit LaTeX pane.

shinyDAG should work in most modern web browsers, however, we have observed optimal performance in Chrome. The most notable difference across OS/browsers is likely to be in display handling for the PDF preview in the main panel: various user or browser-specific settings will determine the default zoom level.

## Citing shinyDAG

shinyDAG was developed by Jordan Creed and Travis Gerke.

Concept DOI: 10.5281/zenodo.1288712

v0.1.0 DOI: 10.5281/zenodo.1296477

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
1. Hernan MA, Hernandez-Díaz S, Robins JM. A structural approach to selection bias. Epidemiology 2004;15:615-625.
