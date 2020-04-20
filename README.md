# `TermProject: Dashboard in shiny` ![shiny-logo, width = 2cm, height = 2cm](/shiny-logo.png)
This repository is an application to visualize IMF data in a strucured way using shiny.

<br>
<br>
See a running example [here](https://max-franke.shinyapps.io/IMF_analysis/).

## Prerequisites for using this dashboard
This dashboard was built in [R](https://www.r-project.org), an open source programming language using the [Shiny package](https://shiny.rstudio.com), a web application framework for R. Users will need to download [R](https://cran.uni-muenster.de/) in order to use this dashboard and also it is suggested to use [RStudio](https://www.rstudio.com). R is completely free to use. All required code can be found in this github repositroy.

## Input type for calculations
This dashboard works with standard csv-files (.csv), which were extracted from the IMF API with following package: [imf](https://cran.r-project.org/web/packages/imfr/imfr.pdf). The variables needed for this dashboard are as follows:


### Input variables for **this dashboard**
#### Country group data
| Variable             	| Detail                                                                           	|
|----------------------	|----------------------------------------------------------------------------------	|
| Country Group Name | Group Name of countries (e.g. world, Euro area) |
| Subject Descriptor | Different categories (e.g. Gross domestic product, Investment) |
| Units | Units (e.g. Percent change) |
| Years | 1980 - 2020  |

#### Country data
| Variable             	| Detail                                                                           	|
|----------------------	|----------------------------------------------------------------------------------	|
| WEO Country Code | Code for countries  |
| ISO | Specific ISO number from IMF |
| WEO Subject Code | Specific Code for Subject Descriptor |
| Country | Countries as character |
| Subject Descriptor | Different categories (e.g. Gross domestic product, Investment) |
| Subject Notes | Explanation and notes of the subject |
| Units | Units (e.g. Percent change) |
| Scale | Scale of units |
| Country/Series-specific Notes | Notes |
| Years | 1980 - 2020  |

These data will be loaded, merged, and transformed for analysis upon start of the dashboard

## Story of dashboard

The Idea of the dashboard
The dashboard is structured into three areas (channels):

1. National Account

2. Trade

3. People

In the first area, the following variables are included:

Gross domestic product, constant prices
Gross domestic product, constant prices (percent changes)
Inflation, average consumer prices
Total investment
In the second area, the following variables are included:

Volume of imports of goods and services
Volume of exports of goods and services
Volume of Imports of goods
Volume of exports of goods
In the third area, the following variables are included:

Unemployment rate
Employment
Population
Current account balance
In the following chapters, the visualization will be produced, which will be implemented into the dashboard with shiny.

The dashboard can be found with the following link:

https://max-franke.shinyapps.io/IMF_analysis/

An example of an image:![gdp](/03_Images/01_NationalAccount/GDP.gif)

## Privacy and storage
This dashboard works with open source data. All data for the running [example](https://max-franke.shinyapps.io/IMF_analysis/) was collected from the official IMF database and API.
<br>
This dashboard can be run locally (for example: [Shiny server](https://www.rstudio.com/products/shiny/shiny-server/)) or on personal machines (mac, windows).

## Author

This dashboard was created at the School of Science of the [St. Thomas University](http://www.stu.edu) by Max Franke in the class CIS-546 DATA VISUALIZATION