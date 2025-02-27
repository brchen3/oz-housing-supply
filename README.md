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

<h3>National Center for Education Statistics locale classifications</h3>

