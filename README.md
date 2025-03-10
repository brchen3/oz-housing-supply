<h1>The Impact of Opportunity Zones on Housing Supply</h1>
This repository includes methods and documents to support the paper [<em>The Impact of Opportunity Zones on Housing Supply</em>](https://eig.org/oppotunity-zones-housing-supply), published March 11th, 2025.

Note that this is a living repository and adjustments to the data and results that come from the code in this analysis may precede changes to the published working draft.

Contact Ben Glasner (benjamin@eig.org) with any questions or concerns regarding inconsistencies.

***
<h2>Abstract</h2>
The United States suffers from a large and persistent shortfall in housing supply. While housing regulations are primarily decided at the local level, the federal government has for many years attempted to boost supply and affordability through a variety of policy interventions. Opportunity Zones (OZs) are one among many such policies, but only recently has enough time passed since implementation that their impact can be observed on an important indicator of local revitalization: growth in local housing stock. We measure this effect using net residential address growth from the United States Department of Housing and Urban Development’s Aggregated United States Postal Service Administrative Data on Address Vacancies. The results offer insight into the effectiveness of a novel capital gains tax incentive to drive investment activity to areas with significantly higher economic need than the typical U.S. community. Using modern difference-in-differences methods, we find that OZs roughly doubled the total amount of new housing added to low-income communities from Q3 2019 to Q3 2024, at a fiscal cost of about $26,000 per new residential address. Our findings show that the causal effect of OZs on housing supply is positive, substantial, and has continued to grow through the end of 2024. The results furthermore show that these effects are geographically widespread and cost-efficient.

***

<h2>Data</h2>
<h3>Administrative Data on Address Vacancies</h3>
United States Department of Housing and Urban Development’s (HUD) Aggregated United States Postal Service (USPS) Administrative Data on Address Vacancies is the primary dataset for analysis and is used to measure net address growth. This data is collected by USPS postal workers to facilitate mail delivery and provides quarterly counts of addresses serviced by USPS. These addresses are categorized as one of three types: residential, business, and other. An address within these three types can be coded as active, vacant, or no-stat. The address is considered vacant if delivery staff on urban routes have identified that mail has not been collected for 90 days or longer. Some addresses are considered to be "no-stat" (neither occupied nor non-occupied) for many reasons, including (1) being on a rural route that has been vacant for 90 days or longer, (2) the address is for a structure under construction and is not yet occupied, and (3) being identified by a carrier as not likely to be active for some time.

This analysis is built on active and vacant residential addresses.

Due to issues with the data available online, the analysis uses the 2020 standardized file received directly from HUD. Contact [Alexander Din](https://www.alexdin.com/) at Alexander.M.Din@hud.gov for questions related to this file.  

<h3>Eligible and Designated OZ Tracts</h3>
Tracts were categorized as being OZ eligible using 2010 census tract boundary definitions. Our data is defined using 2020 census tract boundaries. To make these compatible, we walk 2010 definitions forward to 2020 definitions using HUD's crosswalk system. We only include tracts whose treatment status remained the same using 2020 boundaries. This means that a tract's area had a single OZ designation status through the boundary change. Eligibility categories include Low Income Community (LIC) tracts, contiguous tracts, and ineligible tracts.

<h3>Socioeconomic outcomes from the ACS</h3>
From the 5-year American Community Survey samples from 2012–2023, we extract tract-level poverty rates, median household income, unemployment rates, and the share of prime-age adult residents.

<h3>NCES locale classifications</h3>
Block group population data from the 2017–2021 American Community Survey is paired with the 2021 locale classifications from the National Center for Education Statistics (NCES) to calculate the locale where most people in a tract live. We then determine where a tract sits on the urban–rural spectrum according to the community characteristics where residents tend to live. The definitions are as follows:

<b>Large urban</b>

At least 50 percent of the tract population is in a large urban area, and the tract is in a large urban county. Classified as suburban if at least 50 percent of the tract population is in a large urban area but the tract is not in a large urban county.

<b>Mid-sized urban</b>

At least 50 percent of the tract population is in a mid-sized urban area, and the tract is in a mid-sized urban county.

<b>Small urban</b>

At least 50 percent of the tract population is in a small urban area, and the tract is not classified as large or mid-sized urban.

<b>Suburban</b>

If at least 50 percent of the tract population is in an urban suburban area of any size, the tract is classified as suburban (excluding those already classified as urban) regardless of county type. If at least 50 percent of the tract population is in a small town area, the tract is classified as suburban if in an urban or suburban county; otherwise, it is classified as small town.

<b>Small town</b>

At least 50 percent of the tract population lives in a small town of any size, and the tract is not classified as suburban or urban.

<b>Rural</b>

More than 50 percent of the tract population lives in a rural or small town area of any size, and the tract is not classified as urban, suburban, or small town.

<h3>LODES</h3>
The total number of jobs in each tract and the total number of people who live in each tract who have jobs anywhere are estimated using the EHD Origin-Destination Employment Statistics from 2002–2022.

<h3>Additional data</h3>
We use the 2010 and 2020 Census data for a robustness check to compare tract-level housing units in the census to the count of active addresses from USPS.

Crosswalks are from HUD and from [MCDC](https://mcdc.missouri.edu/applications/geocorr2022.html)

***

<h2>Methodology</h2>

Due to the geographic variation in OZ designation, the simultaneous designation of tracts and finalization of regulations, and the characteristic-dependent selection of eligibility, we favor a difference-in-differences approach that is flexible enough to incorporate conditional parallel trends and produce dynamic effect estimates over the observed post-treatment period. 

In particular, we use the doubly-robust difference-in-differences estimator from Callaway and Sant’Anna (2021) (CSDID). The sample for the CSDID estimates is all low-income community (LIC) tracts that were eligible to be designated as OZs. Those that were designated OZs are the treated group, and those that were not designated are the control group. We exclude contiguous and ineligible tracts from the analysis at this stage. To help ensure against a violation of the parallel trends assumption, we construct conditional parallel trends using a tracts' poverty rate, median household income, unemployment rate, and share of the population who are of prime age (25 to 54) in 2016, before OZs were enacted.

When considering the effect estimate on residential address counts at the tract level, it is important to note the relationship between treatment and our outcomes of interest. OZs are being utilized first and foremost to increase local construction. The construction of new buildings takes time, often with long lags between planning and due diligence, purchase, land development and permitting, construction, sale, and finally, occupancy. This implies a lag between treatment assignment and changes in the count of residential addresses. As such, an average treatment effect over the entire post-treatment period will likely underestimate the true effect of the policy. In fact, the latest available data in the post-period will show the most accurate picture of the effect of OZs up to this point. In our case, we report effect estimates in the third quarter of 2024.
