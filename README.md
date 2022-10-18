# Joint Research Center European Commission - Food Price Crowdsourcing Africa (FPCA) project


R programming code for updating and expanding to new regions and other data the real-time quality check algorithms and procedure for crowdsourced price data.
[Link to the dashboard](https://datam.jrc.ec.europa.eu/datam/mashup/FP_NGA/)

## Project description
Timely and reliable monitoring of commodity food prices is an essential requirement for the assessment of market and food security risks and the establishment of early warning systems, especially in developing economies. However, data from regional or national systems for tracking changes of food prices in sub-Saharan Africa lacks the temporal or spatial richness and is often insufficient to inform targeted interventions. In addition to limited opportunity for [near-]real-time assessment of food prices, various stages in the commodity supply chain are mostly unrepresented, thereby limiting insights on stage-related price evolution. Yet, governments and market stakeholders rely on commodity price data to make decisions on appropriate interventions or commodity-focused investments. Recent rapid technological development indicates that digital devices and connectivity services are becoming affordable for many, including in remote areas of developing economies. This offers a great opportunity both for the harvesting of price data (via new data collection methodologies, such as crowdsourcing/crowdsensing — i.e. citizen-generated data — using mobile apps/devices), and for disseminating it (via web dashboards or other means) to provide real-time data that can support decisions at various levels and related policy-making processes. However, market information that aims at improving the functioning of markets and supply chains requires a continuous data flow as well as quality, accessibility and trust. More data does not necessarily translate into better information. Citizen-based data-generation systems are often confronted by challenges related to data quality and citizen participation, which may be further complicated by the volume of data generated compared to traditional approaches. Following the food price hikes during the first noughties of the 21st century, the European Commission's Joint Research Centre (JRC) started working on innovative methodologies for real-time food price data collection and analysis in developing countries. The work carried out so far includes a pilot initiative to crowdsource data from selected markets across several African countries, two workshops (with relevant stakeholders and experts), and the development of a spatial statistical quality methodology to facilitate the best possible exploitation of geo-located data. Based on the latter, the JRC designed the Food Price Crowdsourcing Africa (FPCA) project and implemented it within two states in Northern Nigeria. The FPCA is a credible methodology, based on the voluntary provision of data by a crowd (people living in urban, suburban, and rural areas) using a mobile app, leveraging monetary and non-monetary incentives to enhance contribution, which makes it possible to collect, analyse and validate, and disseminate staple food price data in real time across market segments. The granularity and high frequency of the crowdsourcing data open the door to real-time space-time analysis, which can be essential for policy and decision making and rapid response on specific geographic regions.
[Link to the project](https://datam.jrc.ec.europa.eu/datam/perm/news/870?rdr=1666109837893)


## R project guidelines
All the analysis script R should be placed in the main folder. Other resources must be positioned according to the structure of the subfolder that follows.

- R folder contains the main function file (functions.R) and the others custom functions (one per file)
- data folder contains all the raw data (CSV is preferred), maps and other input files
- output folder contains output data (.Rdata and .csv)

### functions.R
The functions.R file is useful to load all packages needed for the analysis. If a packages isn’t installed on the local machine, the script will install it avoiding the errors. Moreover, it loads all the additional functions placed in the R folder. To make sure that a new function is loaded and “propagated” in all analyses files, simply put a new R file named as the name of the function in the R subfolder.

### code.R
The code.R file contains all the scripts for running the automatic analysis in the server. In the fist part of the script, there is the parameter definition. Secondly, there is the generation datasets from step 0 to step 3.
In order to correct for non-sampling error in data submissions, such as measurement errors or possible fraudulent activities, we run a pre-processing routine. The pre-processing routine consisted of extracting and validating the raw data reaching the crowdsourcing platform from the mobile app in real time. This phase consists of four steps: (1) the automatic data retrieved from the digital platform through the API and conversion of the json into structured data, (2) data transformation (e.g. standardisation of measurement units), (3) data geo-location to different levels of administrative sub-division and finally, (4) outlier detection. First three steps are executed through the function dataload(), while the outlier detection is performed using the function outlier_detection().

### Extended documentation
Extended documentation is available in documentation.pdf file.
