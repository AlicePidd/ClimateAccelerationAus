# ClimateAccelerationAus

This repo contains R code underpinning the analyses for my fourth thesis chapter:

> ### Climate acceleration highlights regions of intensifying exposure to warming
>
> *Alice M. Pidd*<sup>*1*</sup>*, David S. Schoeman*<sup>*1,2*</sup>*, Anthony J. Richardson*<sup>*3-5*</sup>*, Kylie L. Scales*<sup>*1*</sup>
>
> ##### <sup>1</sup> Ocean Futures Research Cluster, Global-Change Ecology Research Group, School of Science, Technology and Engineering, University of the Sunshine Coast.
>
> ##### <sup>2</sup> Centre for African Conservation Ecology, Department of Zoology, Nelson Mandela University, Gqeberha, South Africa
>
> ##### <sup>3</sup> Centre for Biodiversity and Conservation Science (CBCS), The University of Queensland, Brisbane, Queensland, Australia
>
> ##### <sup>4</sup> School of the Environment, The University of Queensland, Brisbane, Queensland, Australia
>
> ##### <sup>5</sup> Commonwealth Scientific and Industrial Research Organization (CSIRO) Environment, Queensland Biosciences Precinct (QBP), Queensland, Australia

## Contents

```         
ClimateConnectivity
├── figures_tables      <--- .pdf files of figures and tables in the main text
├── masks               <--- .RDS files of masks used in computation and spatial plotting
├── helpers             <--- helper files used in computation and spatial plotting
└── supplementary       <--- supplementary materials for the manuscript
```

## Overview

Here, we explore the utility of our novel climate metric, **climate acceleration**, as a complementary metric of exposure in addition to existing approaches for climate-smart conservation planning — for example, using climate velocity to identify climate refugia (areas of the least/slowest change in climate).

We used estimates of local climate velocity to compute **climate acceleration** as the slope of velocities per 20-year IPCC period from 1995-2090, under four IPCC AR6 emissions scenarios. We use Australian waters as a case study, applying the **climate velocity-acceleration** fields in tandem to yield a combined exposure landscape across the continental EEZ, and in relation to marine protected areas.

## Workflow

Scripts included in this repo reflect the entire code base for computing gradient-based climate velocity (km decade^-1^), climate acceleration (km decade^-2^), and for plotting outputs that appear in my final thesis chapter.

Climate velocity computations followed the workflow found in the `VoCC` R package (Molinos et al. 2019) at <https://github.com/JorGarMol/VoCC>, reworked slightly for `terra`.

Workflow regarding the Earth System Model (ESM) outputs used to compute climate velocity are not included in this repo. ESMs of sea surface temperature (SST) are publicly available from data nodes via the Earth System Grid Federation MetaGrid (<https://esgf.nci.org.au/search>).

Workflow for downloading, wrangling, and processing ESMs can be followed in the `hotrstuff` package and GitHub repo (Buenafe, Schoeman, & Everett 2024) at <https://github.com/SnBuenafe/hotrstuff>.

Background data and shapefiles included relate specifically to the case study region (here, continental Australia).

## Machine specifications

All analyses were run on a machine with the following specifications:

```         
Model Name:     MacBook Pro
Chip:           Apple M3 Max
Cores:          16 (12 performance and 4 efficiency)
Memory:         64 GB
OS:             Tahoe Version 26.3.1 (a) (25D771280a)
R version:      4.5.2 (2025-10-31) -- "[Not] Part in a Rumble"
GitHub:         Version 3.5.8 (arm64)
```

## Questions or feedback?

Please submit an issue, or email your questions to A.Pidd: alicempidd(at)gmail(dot)com

## References

Buenafe, K., Schoeman, D., & Everett, J. (2024). hotrstuff: Facilitate the rapid download, wrangling and processing of Earth System Model (ESM) outputs from the Coupled Model Intercomparison Project (CMIP). R package version 0.0.2. <https://github.com/SnBuenafe/hotrstuff>

Molinos, J. G., Schoeman, D. S., Brown, C. J., & Burrows, M. T. (2019). VoCC: An r package for calculating the velocity of climate change and related climatic metrics. Methods in Ecology and Evolution, 10(12), 2195–2202. <https://doi.org/10.1111/2041-210x.13295>
