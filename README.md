Nomogram for Predicting AD Conversion

Overview

This repository contains a Shiny web application that provides an interactive nomogram for predicting the probability of conversion from mild cognitive impairment (MCI) to Alzheimer's disease (AD). The model integrates plasma biomarkers (GFAP, NFL, pTau181, pTau217), demographic factors, and cognitive scores to enhance predictive accuracy.

Features

Interactive Nomogram: Users can input biomarker levels and demographic details to estimate AD conversion risk.
Statistical Model: Built using logistic regression, Cox proportional hazards analysis, and multiple imputation techniques.
Shiny App Integration: Developed with shiny, rms, and Hmisc for dynamic user interaction.
Public Access: Hosted on ShinyApps.io for online accessibility.

Installation

To run the app locally, install the required R packages and execute app.R:
install.packages(c("shiny", "rms", "Hmisc", "rsconnect"))
library(shiny)
library(rms)
library(Hmisc)
shiny::runApp()

Deployment

To deploy the app to ShinyApps.io:
library(rsconnect)
rsconnect::deployApp("path/to/app")
Alternatively, the app can be hosted via GitHub Pages for R Shiny applications.

