# rrisk: Risk modelling and auto-reporting in R

## Introduction

rrisk is an R package that has been developed at the BfR (Federal Institute for Risk Assessment) as a prototype of a programme to support the risk assessor in the definition and reporting of quantitative risk models. The programme is based on the open source software "R" and therefore falls under the GNU-licence (General Public License) for free distribution. rrisk is a project under development. Please note the conditions of use.

## Features and developments goals of rrisk

The special features and development goals of the software are:

* Reproducibility, version control and portability are secured.
* Good practice of comprehensive and consistent documentation is embedded in user guidance.
* Transparency is ensured through identity between the model and its documentation and auto-reporting.
* State-of-art risk modelling methodology is available through rich and extensible functionality.
* The programme is freely availability and open for further participatory development.

## Download of the software

The program and documentation can be downloaded from this repository. Use the following in R

````
if(!require(devtools))
{
  install.packages("devtools")
  library(devtools)
}

install_github("STAT-UP/rrisk", auth_token = "YOUR PERSONAL ACCESS TOKEN")
````

## Further required or recommended software to run rrisk

| Software                                | Use                                                              |
|-----------------------------------------|------------------------------------------------------------------|
| [R](https://cran.r-project.org/)        | required to run rrisk                                            |
| [JabRef](http://www.jabref.org/)        | recommended to integrate a personal BibTeX reference data base   |
| [MikTeX](http://miktex.org/)            | recommended TeX-Software for generation of Modell-Reports        |
