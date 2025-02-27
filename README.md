<h1>The Impact of Opportunity Zones on Housing Supply</h1>

This repository includes methods and documents to support the paper <em>The Impact of Opportunity Zones on Housing Supply</em>, published [MONTH],[DAY],2025.

Contact Ben Glasner (ben@eig.org) or Adam Ozimek (adam@eig.org) with any questions.

***

<h2>Data</h2>

<h3>Administrative Data on Address Vacancies</h3>

United States Department of Housing and Urban Development’s (HUD) Aggregated United States Postal Service (USPS) Administrative Data on Address Vacancies is the primary dataset for analysis and is used to measure net address growth. This data is collected by USPS postal workers to facilitate mail delivery, and provides quarterly counts of addresses serviced by USPS. These addresses are categorized by residential, business, and other. An address is considered vacant if delivery staff on urban routes have identified that mail has not been collected for 90 days or longer. Some addresses are consitered to be "no-stat" (neither occupied nor non-occupied) for many reasons, including (1) being on a rural route that has been vacant for 90 days or longer, (2) the address is for a structure under construction and is not yet occupied, (3) identified by a carrier as not likely to be active for some time.

<h3>Eligible and Designated OZ Tracts</h3>

Tracts were categorized as being OZ eligible using 1010 census tract boundary definitions. To  make these compatible, we walk 2010 definitions forward to 2020 definitions using HUD's crosswalk system. We only include tracts whose treatment status remained the same using 2020 boundaries. This means that a tract's area had a single OZ designation status through the boundary change.

<h3>Socioeconomic outcomes from the ACS</h3>

From the 5-year American Community Survey samples from 2012-2023, we extract tract-level poverty rates, median household income, unemployment rates, and the share of prime-age adult residents. 

<h3>NCES locale classifications</h3>

Block group population data from the 2017-2021 American Community Survey is paired with the 2021 locale classifications from the National Center for Education Statistics (NCES)  to calculate the locale where most people in a tract live. We then determine where a tract sits on the urban-rural spectrum according to the community characteristics where residents tend to live.


Mid-sized urban
At least 50 percent of the tract population is in a mid-sized urban area, and the tract is in a mid-sized urban county.

Small urban
At least 50 percent of the tract population is in a small urban area, and the tract is not classified as large or mid-sized urban.

Suburban
If at least 50 percent of the tract population is in an urban suburban area of any size the tract is classified as suburban (excluding those already classified as urban) regardless of county type. If at least 50 percent of the tract population is in a small town area the tract is classified as suburban if in an urban or suburban county, otherwise classified as small town. The tract definitions are as follows:

<b>Large urban</b>

      At least 50 percent of the tract population is in a large urban area, and the tract is in a large urban county. Classified as suburban if at least 50 percent of the tract population is in a large urban area but the tract is not in a large urban county.

Small town
At least 50 percent of the tract population lives in a small town of any size, and the tract is not classified as suburban or urban.

Rural
More than 50 percent of the tract population lives in a rural or small town area of any size, and the tract is not classified as urban, suburban, or small town.

