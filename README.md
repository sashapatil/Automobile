# Automobile Price Analysis Project
## Overview
This project aims to analyze the factors affecting automobile prices by utilizing a dataset from the UCI Machine Learning Repository. We explore how various features such as engine type, car dimensions, and fuel efficiency influence the price. The study is divided into three primary research questions focusing on different sets of features that could impact automobile pricing.

## Project Objective
The goal of the project is to determine which factors have a statistically significant effect on car prices. Our analysis includes:

- How car dimensions (height, width, length) affect price.
- How engine-related features (engine size, number of cylinders, engine type) influence pricing.
- How fuel efficiency (city MPG, highway MPG) and related features impact automobile price.

## Dataset
The project uses the Automobile dataset from the UCI Machine Learning Repository. It includes 26 variables across multiple categories, such as car dimensions, engine characteristics, and performance metrics.

## Key variables:

Price: The price of the automobile (dependent variable).
Engine Type: Type of engine used in the car.
Dimensions: Car’s height, width, and length.
Fuel Efficiency: City and highway miles-per-gallon (MPG).
Cylinders: Number of engine cylinders.

## Analysis

Research Question 1: How do car dimensions influence price? We built a regression model using height, width, and length as predictors and found that these features significantly impact the price.

Research Question 2: How does engine type affect price? Engine-related features such as engine size, number of cylinders, and engine type were evaluated, revealing that they moderately influence the car price, though the model could benefit from additional features.

Research Question 3: How do fuel efficiency and weight impact price? A combination of MPG, compression ratio, and curb weight significantly influenced pricing, with performance improving after applying a Box-Cox transformation.

## Techniques Used
- Linear Regression
- Cross-validation
- Bootstrapping
- Hypothesis Testing
- Box-Cox Transformation
  
## Key Findings
Car dimensions, specifically height and width, significantly influence the price.
Engine size and number of cylinders have a moderate impact on car price, but additional features could enhance the model.
Fuel efficiency and weight-related features, when combined with compression ratio, are strong predictors of price, especially after data transformations.
