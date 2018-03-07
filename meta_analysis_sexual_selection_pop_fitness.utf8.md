---
title: 'Effects of sexual selection on non-sexual fitness components: a meta-analysis'
author: "Justin G. Cally^1^, Luke Holman^1^, Devi Stuart-Fox^1^ <br></br> <br></br> ^1^The University of Melbourne"
subtitle: Supplementary Material
output:
  html_document:
    toc: true # table of content true
    toc_float: true # make 
    depth: 3  # upto three depths of headings (specified by #, ## and ###)
    number_sections: false  ## if you want number sections at each table header
    theme: yeti # lovely fonts and colours
    code_folding: hide # awesome buttons to show/hide the code
  pdf_document: default
---


```r
library(knitr)
library(pander)
library(compute.es)
library(metafor)
library(dplyr)
library(lme4)
library(forestplot)
library(ggplot2)
library(kableExtra)
library(ggrepel)
library(reshape2)
library(RColorBrewer)
source("Vdodge_function.R") # nice function for ggplot
```




## Supplementary Methods

Our aim was to investigate the effects of sexual selection on population fitness by conducting a meta-analysis on studies that measured fitness related outcomes after experimentally evolving a population under varying levels of opportunity for sexual selection. Here we describe the process of the literature search, data extraction, effect size calculation, formulation of multilevel models and assessing publication bias.

### Literature Search

**The literature search was conducted under the following conditions:**

1. We searched ISI Web of Science and Scopus on 9th June 2017. The two search engines produded a somewhat different set of papers (**PRISMA Figure**).

2. Studies were restricted to those from peer-reviewed and in the English language.

3. We devised a search strategy that sought to find studies which manipulated the presence or strength of sexual selection using experimental evolution, and then measured some proxy of population fitness. As such the search terms were as follows: 

<br></br>

**_ISI Web of Science_**

We used the following search on ISI Web of Science:

Topic (TS) = “Sexual Selection” OR Promisc* OR Monogam* OR Polygam* OR Polyandr* OR Polygyn* OR “Mate choice”

AND

Topic (TS) = Fitness OR “Population Fitness” OR Deleterious OR “Male Strength” OR Fecund* OR Viability OR Productiv* OR “Reproductive Success” OR “Reproductive Rate” OR Surviv* OR | “Development Rate” OR Extinct* OR “Competitive Success” OR Mortality OR Mass OR “Body Size” OR “Wing Size” OR Emergence OR Mating Rate OR “Mating Propensity” OR Adapt* OR “Novel | Environment” OR “Sexual Conflict” OR “Sexual Antagonis*”

AND

Topic (TS) = Generations OR “Experimental evolution” OR “mutation load”

AND

Research Area (SU) = “Evolutionary Biology”

<br></br>

**_Scopus_**

We used the following search on Scopus:

TITLE-ABS-KEY = “Sexual Selection” OR Promisc* OR Monogam* OR Polygam* OR Polyandr* OR Polygyn* OR “Mate choice”

AND

TITLE-ABS-KEY = Fitness OR “Population Fitness” OR Deleterious OR “Male Strength” OR Fecund* OR Viability OR Productiv* OR “Reproductive Success” OR “Reproductive Rate” OR Surviv* | OR “Development Rate” OR Extinct* OR “Competitive Success” OR Mortality OR Mass OR “Body Size” OR “Wing Size” OR Emergence OR Mating Rate OR “Mating Propensity” OR Adapt* OR | “Novel Environment” OR “Sexual Conflict” OR “Sexual Antagonis*”

AND

TITLE-ABS-KEY = Generations OR “Experimental evolution” OR “mutation load”

### Additions to the Literature Search

In addition to studies found from the literature search we also included three relevant studies that we found, which were not picked up in the subsequent formal searches (Partridge 1980; Price et al. 2010; Savic Veselinovic et al. 2013; **PRISMA Figure**). 

### Data extraction

After removing duplicates papers recovered from both ISI and Scopus, we read the titles and abstracts of the remaining 1015 papers, and removed papers that were not relevant (typically because they were not an empirical study using experimental evolution). This left 130 papers, for which we read the full text and applied the following selection criteria: 

  + **(1: Study Design)** The study was an experimental evolution study lasting >1 generation
  + **(1: Population)** a) The study was conducted using an animal species that was b) diecious
  + **(1: Intervention and Control)** The study experimentally manipulated the strength of sexual selection (e.g. via enforced monogamy or an altered sex ratio)
  + **(1: Outcomes)** The study measured a trait that we judged to be a potential correlate of population fitness. 
  
This latter criterion is likely to be contentious, because there is rarely enough data justify the assumption that a particular trait is (or is not) correlated with population fitness. We therefore relied on our best judgement when deciding which studies to exclude (see **Table S1**). The inclusion/exlusion critera as applied to each study are detailed in **Table S2**.

**Table S1:** We classed each of the twenty fitness related outcomes into three broad groups of direct, indirect and ambiguous based on the established link with population fitness, the directionality of the measure. Here we detailed how these outcomes were measured in the studies of this meta-analysis. In the accompanying box we provide a legend to the references cited in the table.

 <br/><br/>

```r
outcome.descriptions <- read.csv('data/outcome.descriptions.csv', 
                                 fileEncoding="UTF-8")
kable(outcome.descriptions, "html") %>%
  kable_styling() %>%
  scroll_box(width = "800px", height = "500px")
```

<div style="border: 1px solid #ddd; padding: 5px; overflow-y: scroll; height:500px; overflow-x: scroll; width:800px; "><table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Outcome </th>
   <th style="text-align:left;"> Classification </th>
   <th style="text-align:left;"> Explanation </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Behavioural Plasticity </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Female kicking against male harassment in different sociosexual contexts for the beetle Callosobruchus maculatus (1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Body size was often recorded to correct for other morphometric traits (e.g. body condition, strength or testes weight (2, 3). It was measured as either length or dry mass. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Development Rate </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Egg-to adult development time was recorded in several studies (4-6) and often alongside traits other life-history traits suspected to impact fitness. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Early fecundity was measured (alongside lifetime fecundity) as a life-history trait that may impact lifetime reproductive success. It was defined as either the total or proportional reproductive output in earlier stages of maturity (e.g. within the first 7 days) (7, 8). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Phenoloxidase (PO) activity or parasite load (6, 9-11). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mating Duration </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Mating duration may have variable fitness impacts based on the soiciosexual conditions and extent of sexual conflict. It may be beneficial to have longer mating bouts for a male in a competitive environment however it may be damaging for a female under benign conditions (e.g. 1, 12, 13, 14). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pesticide Resistance </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Pesticide resistance was measured both in the presence and absence of pesticides for the insect Tribolium castaneum, it was a binary measure of resistance to knockdown that was incorporated into generalized linear mixed models (15), </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mutant Frequency </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Allele and mutant frequency measured at the population level (Arbuthnott and Rundle (16), Hollis, Fierst (17)) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Body Condition </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Mean body weight of Onthophagus Taurus adjusted for body size (thorax width) (Simmons and Garcia-Gonzalez (2). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fitness Senescence </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Rate of decline in survival probability across lifespan (5, 18). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Longevity or survival across the entire lifespan (e.g. 19) or from a given point once under stressful conditions, such as starvation or after females mated in different operational sex ratios (e.g. 20). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Male Attractiveness </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Inferred from female preference tests in mice (21, 22) and male ornament size (coloration) in guppies (23). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mating Frequency </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Number of mounts by males on females in Tribolium castaneum and Drosophila melanogaster (13, 19). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mating Latency </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Time taken for a male to undertake their first copulatory mount from the time of being first put together with female/s (1, 12-14, 24, 25). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Male mating success measured males ability to successfully mate with females. Often in the presence of other males (e.g. 8, 24, 26). Mating success of a male against a rival male can be determined via competing a focal male against an irradiated (infertile) competitor, the resulting proportion of eggs hatching are then determined to be a measure of the focal males success. Mating success also included measurements of mating capacity where males were continually presented with females until exhausted, the number of sequential matings were then recorded (27) and mating offence and defence ability (14). The mating offence and defence capability was estimated via paternity share of a male when in the first mating position (P1) or the second (P2). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Strength </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Male pulling strength in the dung beetle, Onthophagus Taurus, measured by attaching weights and measuring the weight the beetle was able to pull (Almbro and Simmons (3) . </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Sperm quality and production grouped multiple measured outcomes together, both within a study (28) and during the meta-analysis. This includes sperm size, plug size, testes size, soporific effect, ejaculate weight, accessory gland size, motility, path velocity, sperm longevity (e.g. 1, 29, 30, 31) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Extinction Rate </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Extinction rate was measured at the population level, either via recording the proportion of extinct lines after a given number of generations (32, 33) or via analysis of extinction rate over consecutive generations via the Weibull baseline hazard distribution (34). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Offspring viability, also recorded as egg-to-adult viability or embryonic viability, was measured as survival to a certain age (e.g. 1 year (23)) or life stage (e.g. hatching (35)). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> A measure of the number of offspring produced by an individual. Reproductive success was also described as fecundity (e.g. 36), number of offspring produced (e.g. 37), fertility (e.g. 12) in females and proportion or total progeny sired in males (e.g. 5). </td>
  </tr>
</tbody>
</table></div>
  <br/><br/> 
<input type=button class=hideshow></input>

```r
outcome.references <- read.csv('data/references.tableS1.csv', 
                                 fileEncoding="UTF-8")
kable(outcome.references, "html") %>%
  kable_styling() %>%
  scroll_box(width = "800px", height = "250px")
```

<div style="border: 1px solid #ddd; padding: 5px; overflow-y: scroll; height:250px; overflow-x: scroll; width:800px; "><table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> Citation </th>
   <th style="text-align:left;"> Reference </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> van Lieshout E, McNamara KB, Simmons LW. Rapid Loss of Behavioral Plasticity and Immunocompetence under Intense Sexual Selection. Evolution. 2014;68(9):2550-8. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Simmons LW, Garcia-Gonzalez F. Evolutionary Reduction in Testes Size and Competitive Fertilization Success in Response to the Experimental Removal of Sexual Selection in Dung Beetles. Evolution. 2008;62(10):2580-91. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> Almbro M, Simmons LW. Sexual Selection Can Remove an Experimentally Induced Mutation Load. Evolution. 2014;68(1):295-300. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> Fricke C, Arnqvist G. Rapid adaptation to a novel host in a seed beetle (Callosobruchus maculatus): The role of sexual selection. Evolution. 2007;61(2):440-54. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> Hollis B, Keller L, Kawecki TJ. Sexual selection shapes development and maturation rates in Drosophila. Evolution. 2017;71(2):304-14. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> McKean KA, Nunney L. Sexual selection and immune function in Drosophila melanogaster. Evolution. 2008;62(2):386-400. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> Crudgington HS, Fellows S, Snook RR. Increased opportunity for sexual conflict promotes harmful males with elevated courtship frequencies. Journal of Evolutionary Biology. 2010;23(2):440-6. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:left;"> Tilszer, M. Antoszczyk, K. Salek, N. Zajac, E. Radwan, J.. Evolution under relaxed sexual conflict in the bulb mite Rhizoglyphus robini. Evolution. 2006;60(9):1868-73. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> Hangartner S, Michalczyk L, Gage MJG, Martin OY. Experimental removal of sexual selection leads to decreased investment in an immune component in female Tribolium castaneum. Infection, Genetics and Evolution. 2015;33:212-8. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Hangartner S, Sbilordo SH, Michalczyk L, Gage MJG, Martin OY. Are there genetic trade-offs between immune and reproductive investments in Tribolium castaneum? Infection, Genetics and Evolution. 2013;19:45-50. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> McNamara KB, van Lieshout E, Simmons LW. A test of the sexy-sperm and good-sperm hypotheses for the evolution of polyandry. Behavioral Ecology. 2014;25(4):989-95. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:left;"> Edward DA, Fricke C, Chapman T. Adaptations to sexual selection and sexual conflict: insights from experimental evolution and artificial selection. Philosophical Transactions of the Royal Society B-Biological Sciences. 2010;365(1552):2541-8. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:left;"> Michalczyk L, Millard AL, Martin OY, Lumley AJ, Emerson BC, Gage MJG. Experimental Evolution Exposes Female and Male Responses to Sexual Selection and Conflict in Tribolium Castaneum. Evolution. 2011;65(3):713-24. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:left;"> Nandy B, Chakraborty P, Gupta V, Ali SZ, Prasad NG. Sperm Competitive Ability Evolves in Response to Experimental Alteration of Operational Sex Ratio. Evolution. 2013;67(7):2133-41. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Jacomb F, Marsh J, Holman L. Sexual selection expedites the evolution of pesticide resistance. Evolution. 2016;70(12):2746-51. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:left;"> Arbuthnott D, Rundle HD. Sexual Selection Is Ineffectual or Inhibits the Purging of Deleterious Mutations in Drosophila Melanogaster. Evolution. 2012;66(7):2127-37. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:left;"> Hollis B, Fierst JL, Houle D. Sexual Selection Accelerates the Elimination of a Deleterious Mutant in Drosophila Melanogaster. Evolution. 2009;63(2):324-33. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Archer CR, Duffy E, Hosken DJ, Mokkonen M, Okada K, Oku K, et al. Sex-specific effects of natural and sexual selection on the evolution of life span and ageing in Drosophila simulans. Functional Ecology. 2015;29(4):562-9. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:left;"> Wigby S, Chapman T. Female resistance to male harm evolves in response to manipulation of sexual conflict. Evolution. 2004;58(5):1028-37. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Martin OY, Hosken DJ. Costs and benefits of evolving under experimentally enforced polyandry or monogamy. Evolution. 2003;57(12):2765-72. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:left;"> Firman RC. Female social preference for males that have evolved via monogamy: evidence of a trade-off between pre- and post-copulatory sexually selected traits? Biology Letters. 2014;10(10). </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> Nelson AC, Colson KE, Harmon S, Potts WK. Rapid adaptation to mammalian sociality via sexually selected traits. Bmc Evolutionary Biology. 2013;13. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:left;"> Pelabon C, Larsen LK, Bolstad GH, Viken A, Fleming IA, Rosenqvist G. The effects of sexual selection on life-history traits: An experimental study on guppies. Journal of Evolutionary Biology. 2014;27(2):404-16. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Debelle A, Ritchie MG, Snook RR. Sexual selection and assortative mating: an experimental test. Journal of Evolutionary Biology. 2016;29(7):1307-16. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:left;"> Hollis B, Kawecki TJ. Male cognitive performance declines in the absence of sexual selection. Proceedings of the Royal Society B-Biological Sciences. 2014;281(1781). </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:left;"> McGuigan K, Petfield D, Blows MW. Reducing mutation load through sexual selection on males. Evolution. 2011;65(10):2816-29. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Crudgington HS, Fellows S, Badcock NS, Snook RR. Experimental Manipulation of Sexual Selection Promotes Greater Male Mating Capacity but Does Not Alter Sperm Investment. Evolution. 2009;63(4):926-38. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:left;"> Firman RC, Simmons LW. Experimental Evolution of Sperm Quality Via Postcopulatory Sexual Selection in House Mice. Evolution. 2010;64(5):1245-56. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Fritzsche K, Timmermeyer N, Wolter M, Michiels NK. Female, but not male, nematodes evolve under experimental sexual coevolution. Proceedings of the Royal Society B-Biological Sciences. 2014;281(1796). </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> Gay L, Hosken DJ, Vasudev R, Tregenza T, Eady PE. Sperm competition and maternal effects differentially influence testis and sperm size in Callosobruchus maculatus. Journal of Evolutionary Biology. 2009;22(5):1143-50. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:left;"> McNamara KB, Robinson SP, Rosa ME, Sloan NS, van Lieshout E, Simmons LW. Male-biased sex ratio does not promote increased sperm competitiveness in the seed beetle, Callosobruchus maculatus. Scientific Reports. 2016;6. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:left;"> Jarzebowska M, Radwan J. Sexual Selection Counteracts Extinction of Small Populations of the Bulb Mites. Evolution. 2010;64(5):1283-9. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:left;"> Plesnar-Bielak A, Skrzynecka AM, Prokop ZM, Radwan J. Mating system affects population performance and extinction risk under environmental challenge. Proceedings of the Royal Society B-Biological Sciences. 2012;279(1747):4661-7. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:left;"> Lumley AJ, Michalczyk L, Kitson JJN, Spurgin LG, Morrison CA, Godwin JL, et al. Sexual selection protects against extinction. Nature. 2015;522(7557):470-+. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:left;"> Plesnar A, Konior M, Radwan J. The role of sexual selection in purging the genome of induced mutations in the bulb mite (Rizoglyphus robini). Evolutionary Ecology Research. 2011;13(2):209-16. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:left;"> Firman RC. Polyandrous females benefit by producing sons that achieve high reproductive success in a competitive environment. Proceedings of the Royal Society B-Biological Sciences. 2011;278(1719):2823-31. </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:left;"> Bernasconi G, Keller L. Female polyandry affects their sons' reproductive success in the red flour beetle Tribolium castaneum. Journal of Evolutionary Biology. 2001;14(1):186-93. </td>
  </tr>
</tbody>
</table></div>
  <br/><br/> 

**Table S2:** An eligibility criteria was based on four features a study needed to include (discussed above), to be eligable for inclusion in the meta-analysis the study needed to satisfy all criteria. Here we applied a step-wise process to the studies that had their full-text read and excluded them when they first failed to meet the criteria. Additional notes documenting reasons behind exclusion were also taken.
 <br/><br/> 
<input type=button class=hideshow></input>

```r
Eligibility.criteria <- read.csv('data/Eligibility Workbook(22.02).csv', 
                                 fileEncoding="UTF-8")
kable(Eligibility.criteria, "html") %>%
  kable_styling() %>%
  scroll_box(width = "800px", height = "500px")
```

<div style="border: 1px solid #ddd; padding: 5px; overflow-y: scroll; height:500px; overflow-x: scroll; width:800px; "><table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Authors </th>
   <th style="text-align:right;"> Year </th>
   <th style="text-align:left;"> Title </th>
   <th style="text-align:left;"> Study.Design </th>
   <th style="text-align:left;"> Population </th>
   <th style="text-align:left;"> Intervention.and.Control </th>
   <th style="text-align:left;"> Outcomes </th>
   <th style="text-align:left;"> Included </th>
   <th style="text-align:left;"> Exclusion.Reason </th>
   <th style="text-align:left;"> Notes </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Aguirre, J. D. and D. J. Marshall </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Does Genetic Diversity Reduce Sibling Competition? </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ahuja, A. and R. S. Singh </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Variation and evolution of male sex combs in Drosophila: Nature of selection response and theories of genetic variation for sexual traits </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Almbro, M. and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Sexual Selection Can Remove an Experimentally Induced Mutation Load </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Male strength is important in competition </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Amitin, E. G. and S. Pitnick </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Influence of developmental environment on male- and female-mediated sperm precedence in Drosophila melanogaster </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Antolin, M. F., P. J. Ode, G. E. Heimpel, R. B. O'Hara and M. R. Strand </td>
   <td style="text-align:right;"> 2003 </td>
   <td style="text-align:left;"> Population structure, mating system, and sex-determining allele diversity of the parasitoid wasp Habrobracon hebetor </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Arbuthnott, D., E. M. Dutton, A. F. Agrawal and H. D. Rundle </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> The ecology of sexual conflict: ecologically dependent parallel evolution of male harm and female resistance in Drosophila melanogaster </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Arbuthnott, D. and H. D. Rundle </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Sexual Selection Is Ineffectual or Inhibits the Purging of Deleterious Mutations in Drosophila Melanogaster </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Natural selection acted against tested alleles, thus indicate fitness aspect </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Arbuthnott, D. and H. D. Rundle </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Misalignment of natural and sexual selection among divergently adapted Drosophila melanogaster populations </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Archer, C. R., E. Duffy, D. J. Hosken, M. Mokkonen, K. Okada, K. Oku, M. D. Sharma and J. Hunt </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Sex-specific effects of natural and sexual selection on the evolution of life span and ageing in Drosophila simulans </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Natural selection was measured simultanous and thus provides measurement of fitness </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Artieri, C. G., W. Haerty, B. P. Gupta and R. S. Singh </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Sexual selection and maintenance of sex: Evidence from comparisons of rates of genomic accumulation of mutations and divergence of sex-related genes in sexual and hermaphroditic species of Caenorhabditis </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bacigalupe, L. D., H. S. Crudgington, F. Hunter, A. J. Moore and R. R. Snook </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Sexual conflict does not drive reproductive isolation in experimental populations of Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Viability and sterility were measured + Mating Speed, However these were in crosses, refer to 2008 study for beater outcomes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bacigalupe, L. D., H. S. Crudgington, J. Slate, A. J. Moore and R. R. Snook </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Sexual selection and interacting phenotypes in experimental evolution: A study of Drosophila pseudoobscura mating behavior </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Not Suitable </td>
   <td style="text-align:left;"> Mating speed cited as a measure of fitness. Uses both male and female. Doesn't really work. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Barbosa, M., S. R. Connolly, M. Hisano, M. Dornelas and A. E. Magurran </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Fitness consequences of female multiple mating: A direct test of indirect benefits </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Measures multiple mating not choice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bernasconi, G. and L. Keller </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Female polyandry affects their sons' reproductive success in the red flour beetle Tribolium castaneum </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Unsure </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Polyandry was done sequentially with postcop mate choice, however the experimental approach and measurement of outcomes meant that extracting SS+ vs SS- was not possible </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bielak, A. P., A. M. Skrzynecka, K. Miler and J. Radwan </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Selection for alternative male reproductive tactics alters intralocus sexual conflict </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Artificial selection was conducted </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Blows, M. W. </td>
   <td style="text-align:right;"> 2002 </td>
   <td style="text-align:left;"> Interaction between natural and sexual selection during the evolution of mate recognition </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Hybrid Drosophilia used, indirect fitness was measured (mate recognition system) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Brommer, J. E., C. Fricke, D. A. Edward and T. Chapman </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Interactions between Genotype and Sexual Conflict Environment Influence Transgenerational Fitness in Drosophila Melanogaster </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Multiple males but only one at a time ( could be post copulatory SS) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Castillo, D. M., M. K. Burger, C. M. Lively and L. F. Delph </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Experimental evolution: Assortative mating and sexual selection, independent of local adaptation, lead to reproductive isolation in the nematode Caenorhabditis remanei </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> No SS lines </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cayetano, L., A. A. Maklakov, R. C. Brooks and R. Bonduriansky </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Evolution of Male and Female Genitalia Following Release from Sexual Selection </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Conflict I burdensome and defensive / offensive traits have fitness costs and benefits: Removing as too difficult to see clear fitness of measurements </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Chandler, C. H., C. Ofria and I. Dworkin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Runaway Sexual Selection Leads to Good Genes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 2a </td>
   <td style="text-align:left;"> Digital organisms used </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Chenoweth, S. F., N. C. Appleton, S. L. Allen and H. D. Rundle </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Genomic Evidence that Sexual Selection Impedes Adaptation to a Novel Environment </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Alongside direct fitness, SNPs also used. This paper reports SNPs while Rundle (2006) reports fitness measures. Thus data is extracted from that paper, not this one </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Chenoweth, S. F., D. Petfield, P. Doughty and M. W. Blows </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Male choice generates stabilizing sexual selection on a female fecundity correlate </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Behavioural mate choice experiment </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Chenoweth, S. F., H. D. Rundle and M. W. Blows </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Genetic constraints and the evolution of display trait sexual dimorphism by natural and sexual selection </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Natural selection was also measured and CHCs provide an indirect fitness aspect </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Chenoweth, S. F., H. D. Rundle and M. W. Blows </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Experimental evidence for the evolution of indirect genetic effects: changes in the interaction effect coefficient, psi (_), due to sexual selection </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> CHCs may provide indirect fitness aspect </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Crudgington, H. S., A. P. Beckerman, L. Brustle, K. Green and R. R. Snook </td>
   <td style="text-align:right;"> 2005 </td>
   <td style="text-align:left;"> Experimental removal and elevation of sexual selection: Does sexual selection generate manipulative males and resistant females? </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Experimental Manipulation of Sexual Selection Promotes Greater Male Mating Capacity but Does Not Alter Sperm Investment </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Direct and indirect outcomes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Increased opportunity for sexual conflict promotes harmful males with elevated courtship frequencies </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Debelle, A., M. G. Ritchie and R. R. Snook </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Sexual selection and assortative mating: an experimental test </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Demont, M., V. M. Grazer, L. Michalczyk, A. L. Millard, S. H. Sbilordo, B. C. Emerson, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Experimental Removal of Sexual Selection Reveals Adaptations to Polyandry in Both Sexes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Edward, D. A., C. Fricke and T. Chapman </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Adaptations to sexual selection and sexual conflict: insights from experimental evolution and artificial selection </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fava, G. </td>
   <td style="text-align:right;"> 1975 </td>
   <td style="text-align:left;"> Studies on the selective agents operating in experimental populations of Tisbe clodiensis (Copepoda, Harpacticoida) </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Firman, R. C. </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Polyandrous females benefit by producing sons that achieve high reproductive success in a competitive environment </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> It looks like post copulatory selection was enabled here </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Firman, R. C. </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Female social preference for males that have evolved via monogamy: evidence of a trade-off between pre- and post-copulatory sexually selected traits? </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> The outcome measured was female preference and male scent marking rate. May have a role in fitness but not explicitly stated </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Firman, R. C., L. Y. Cheam and L. W. Simmons </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Sperm competition does not influence sperm hook morphology in selection lines of house mice </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Sperm quality was measured </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Firman, R. C., F. Garcia-Gonzalez, E. Thyer, S. Wheeler, Z. Yamin, M. Yuan and L. W. Simmons </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Evolutionary change in testes tissue composition among experimental populations of house mice </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Amount of sperm producing tissue was measured as it provides an advantage in sperm competition </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Firman, R. C., M. Gomendio, E. R. S. Roldan and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> The Coevolution of Ova Defensiveness with Sperm Competitiveness in House Mice </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ova defensivenenss can bias fertilization to a more specific type of sperm and thus be a fitness adavantage </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Firman, R. C. and L. W. Simmons </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Experimental Evolution of Sperm Quality Via Postcopulatory Sexual Selection in House Mice </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Polygamous lines have only post-copulatory selection </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Firman, R. C. and L. W. Simmons </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Experimental evolution of sperm competitiveness in a mammal </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Sperm competition is a fitness advantage </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Firman, R. C. and L. W. Simmons </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Male house mice evolving with post-copulatory sexual selection sire embryos with increased viability </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Post cop SS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fricke, C., C. Andersson and G. Arnqvist </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Natural selection hampers divergence of reproductive traits in a seed beetle </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Could not use the broad outcome of reproductive characteristics as it is not directional </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fricke, C. and G. Arnqvist </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Rapid adaptation to a novel host in a seed beetle (Callosobruchus maculatus): The role of sexual selection </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Also post cop </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fritzsche, K., I. Booksmythe and G. Arnqvist </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Sex Ratio Bias Leads to the Evolution of Sex Role Reversal in Honey Locust Beetles </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Male bias and female bias setups without monogamus/lack of SS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fritzsche, K., N. Timmermeyer, M. Wolter and N. K. Michiels </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Female, but not male, nematodes evolve under experimental sexual coevolution </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Male bias and female bias setups without monogamus/lack of SS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Garcia-Gonzalez, F., Y. Yasui and J. P. Evans </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Mating portfolios: bet-hedging, sexual selection and female multiple mating </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Not Suitable </td>
   <td style="text-align:left;"> Experiments run alongside bet-hedging, perhaps confounding, need to look at data </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gay, L., P. E. Eady, R. Vasudev, D. J. Hosken and T. Tregenza </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Does reproductive isolation evolve faster in larger populations via sexually antagonistic coevolution? </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Generations of monoandry were replaced by polyandry (not done simultaneously ), Not sure whether the monogamous lines were maintained. This experiment was focussed on reproductive isolation anyway </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gay, L., D. J. Hosken, P. Eady, R. Vasudev and T. Tregenza </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> The Evolution of Harm-Effect of Sexual Conflicts and Population Size </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Generations of monoandry were replaced by polyandry (not done simultaneously ), Not sure whether the monogamous lines were maintained. Also, did not directly look at SS+ vs SS- </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gay, L., D. J. Hosken, R. Vasudev, T. Tregenza and P. E. Eady </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Sperm competition and maternal effects differentially influence testis and sperm size in Callosobruchus maculatus </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Appears to be direct comparison bw mono and poly </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Grazer, V. M., M. Demont, L. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Environmental quality alters female costs and benefits of evolving under enforced monogamy </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Direct Measures of fitness in environments that had standard and sub-standard food quality </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Grieshop, K., J. Stangberg, I. Martinossi-Allibert, G. Arnqvist and D. Berger </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Strong sexual selection in males against a mutation load that reduces offspring production in seed beetles </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Different mating systems/ opportunity for SS were not imposed </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hall, M. D., L. F. Bussiere and R. Brooks </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Diet-dependent female evolution influences male lifespan in a nuptial feeding insect </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Different mating systems/ opportunity for SS were not imposed </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hangartner, S., L. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Experimental removal of sexual selection leads to decreased investment in an immune component in female Tribolium castaneum </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, L. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Are there genetic trade-offs between immune and reproductive investments in Tribolium castaneum? </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Different levels of SS, but none with enforced monogamy (no choice) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hicks, S. K., K. L. Hagenbuch and L. M. Meffert </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Variable costs of mating, longevity, and starvation resistance in Musca domestica (Diptera: Muscidae) </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Study on environmental conditions not SS treatment </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Holland, B. </td>
   <td style="text-align:right;"> 2002 </td>
   <td style="text-align:left;"> Sexual selection fails to promote adaptation to a new environment </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Also looks at thermal stress </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Holland, B. and W. R. Rice </td>
   <td style="text-align:right;"> 1999 </td>
   <td style="text-align:left;"> Experimental removal of sexual selection reverses intersexual antagonistic coevolution and removes a reproductive load </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hollis, B., J. L. Fierst and D. Houle </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Sexual Selection Accelerates the Elimination of a Deleterious Mutant in Drosophila Melanogaster </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> looked at the purging of a deleterious allele </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hollis, B. and D. Houle </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Populations with elevated mutation load do not benefit from the operation of sexual selection </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Mutagenesis took place and direct fitness measurements were made </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hollis, B., D. Houle and T. J. Kawecki </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Evolution of reduced post-copulatory molecular interactions in Drosophila populations lacking sperm competition </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Seminal fluid proteins have a fitness advantage in a polygamous setting, thus is favoured. Feels like a bit of a circular argument </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hollis, B., D. Houle, Z. Yan, T. J. Kawecki and L. Keller </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Evolution under monogamy feminizes gene expression in Drosophila melanogaster </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Sex biased gene expression was measured, showing sexual antagonism </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hollis, B. and T. J. Kawecki </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Male cognitive performance declines in the absence of sexual selection </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Cognitive ability measured in both male and female </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hollis, B., L. Keller and T. J. Kawecki </td>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:left;"> Sexual selection shapes development and maturation rates in Drosophila </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Development and fitness measured </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hosken, D. J., O. Y. Martin, S. Wigby, T. Chapman and D. J. Hodgson </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Sexual conflict and reproductive isolation in flies </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Reproductive isolation measured without fitness components </td>
  </tr>
  <tr>
   <td style="text-align:left;"> House, C. M., Z. Lewis, D. J. Hodgson, N. Wedell, M. D. Sharma, J. Hunt and D. J. Hosken </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Sexual and Natural Selection Both Influence Male Genital Evolution </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Indirect fitness component of male genitalia with citation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hunt, J., R. R. Snook, C. Mitchell, H. S. Crudgington and A. J. Moore </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Sexual selection and experimental evolution of chemical signals in Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Body size measured as well as CHC, like other studies may confer fitness advantage </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Immonen, E., R. R. Snook and M. G. Ritchie </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Mating system variation drives rapid evolution of the female transcriptome in Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> While transcriptome outcomes not exclusively measuring fitness they also measures aspects of fecundity </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Innocenti, P., I. Flis and E. H. Morrow </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Female responses to experimental removal of sexual selection components in Drosophila melanogaster </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> To some extent the nature of SS treatment is unclear. Gene expression and fecundity are measured </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Jacomb, F., J. Marsh and L. Holman </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Sexual selection expedites the evolution of pesticide resistance </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Pesticide Resistance as an environmental condition that needs to be adapted to </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Janicke, T., P. Sandner, S. A. Ramm, D. B. Vizoso and L. Schaerer </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Experimentally evolved and phenotypically plastic responses to enforced monogamy in a hermaphroditic flatworm </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 2b </td>
   <td style="text-align:left;"> Hermaphroditic </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Jarzebowska, M. and J. Radwan </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Sexual Selection Counteracts Extinction of Small Populations of the Bulb Mites </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Direct fitness measurements over several generations </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Klemme, I. and R. C. Firman </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Male house mice that have evolved with sperm competition have increased mating duration and paternity success </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Paternity Success measured </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Long, T. A. F., A. F. Agrawal and L. Rowe </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> The Effect of Sexual Selection on Offspring Fitness Depends on the Nature of Genetic Variation </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> No enforced SS regimes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lumley, A. J., L. Michalczyk, J. J. N. Kitson, L. G. Spurgin, C. A. Morrison, J. L. Godwin, M. E. Dickinson, O. Y. Martin, B. C. Emerson, T. Chapman and M. J. G. Gage </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Sexual selection protects against extinction </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Reproductive fitness and time to extinction measured </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MacLellan, K., L. Kwan, M. C. Whitlock and H. D. Rundle </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Dietary stress does not strengthen selection against single deleterious mutations in Drosophila melanogaster </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Selection based experiment rather than EE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MacLellan, K., M. C. Whitlock and H. D. Rundle </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Sexual selection against deleterious mutations via variable male search success </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Selection based experiment rather than EE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Maklakov, A. A., R. Bonduriansky and R. C. Brooks </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Sex Differences, Sexual Selection, and Ageing: An Experimental Evolution Approach </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Life History traits were measured </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Maklakov, A. A. and C. Fricke </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Sexual selection did not contribute to the evolution of male lifespan under curtailed age at reproduction in a seed beetle </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Life History traits were measured </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Maklakov, A. A., C. Fricke and G. Arnqvist </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Sexual selection affects lifespan and aging in the seed beetle </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Life History traits were measured </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mallet, M. A., J. M. Bouchard, C. M. Kimber and A. K. Chippindale </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Experimental mutation-accumulation on the X chromosome of Drosophila melanogaster reveals stronger selection on males than females </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> No SS+ and SS- treatments </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mallet, M. A. and A. K. Chippindale </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Inbreeding reveals stronger net selection on Drosophila melanogaster males: implications for mutation load and the fitness of sexual females </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Mutation levels analysed </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Martin, O. Y. and D. J. Hosken </td>
   <td style="text-align:right;"> 2003 </td>
   <td style="text-align:left;"> Costs and benefits of evolving under experimentally enforced polyandry or monogamy </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Crossing took place after Gen 29, results still contain fitness components though </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Martin, O. Y. and D. J. Hosken </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Reproductive consequences of population divergence through sexual conflict </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Crossing also took place, it should still be fine as they some populations were not crossed </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Matsuyama, T. and H. Kuba </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Mating time and call frequency of males between mass-reared and wild strains of melon fly, Bactrocera cucurbitae (Coquillett) (Diptera: Tephritidae) </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Mate choice in different populations </td>
  </tr>
  <tr>
   <td style="text-align:left;"> McGuigan, K., D. Petfield and M. W. Blows </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> REDUCING MUTATION LOAD THROUGH SEXUAL SELECTION ON MALES </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> The control line was not enforced monomagous (did not remove SS)., it was just a control where the population was mutagenised. No clear SS treatment as level of selection varied across the generations. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> McKean, K. A. and L. Nunney </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Sexual selection and immune function in Drosophila melanogaster </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> The control line was a 1:1 SR but not enforced monogamy </td>
  </tr>
  <tr>
   <td style="text-align:left;"> McLain, D. K. </td>
   <td style="text-align:right;"> 1992 </td>
   <td style="text-align:left;"> Population density and the intensity of sexual selection on body length in spatially or temporally restricted natural populations of a seed bug </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Field study </td>
  </tr>
  <tr>
   <td style="text-align:left;"> McNamara, K. B., S. P. Robinson, M. E. Rosa, N. S. Sloan, E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Male-biased sex ratio does not promote increased sperm competitiveness in the seed beetle, Callosobruchus maculatus </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No SS- (enforced monogamy) just altered SR </td>
  </tr>
  <tr>
   <td style="text-align:left;"> McNamara, K. B., E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> A test of the sexy-sperm and good-sperm hypotheses for the evolution of polyandry </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Polygamy was still randomly done meaning post-cop SS is only available. Numorous measures of fitness conducted </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Meffert, L. M., J. L. Regan, S. K. Hicks, N. Mukana and S. B. Day </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Testing alternative methods for purging genetic load using the housefly (Musca domestica L.) </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> No tsts of SS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson, T. Chapman and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Inbreeding Promotes Female Promiscuity </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> It does not appear the SS regimes were enforced (fig 1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Experimental Evolution Exposes Female and Male Responses to Sexual Selection and Conflict in Tribolium Castaneum </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No enforced monogamy (no SS-), but different OSR </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Morrow, E. H., A. D. Stewart and W. R. Rice </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Assessing the extent of genome-wide intralocus sexual conflict via experimentally enforced gender-limited selection </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Not assessing SS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Sperm Competitive Ability Evolves in Response to Experimental Alteration of Operational Sex Ratio </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Use an OSR of male and female bias </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Experimental Evolution of Female Traits under Different Levels of Intersexual Conflict in Drosophila Melanogaster </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Use an OSR of male and female bias </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nelson, A. C., K. E. Colson, S. Harmon and W. K. Potts </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Rapid adaptation to mammalian sociality via sexually selected traits </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 3 generations in mice with direct fitness outcomes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nie, H. and K. Kaneshiro </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Sexual selection and incipient speciation in Hawaiian Drosophila </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Artificial selection was conducted alongside mate choice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Palopoli, M. F., C. Peden, C. Woo, K. Akiha, M. Ary, L. Cruze, J. L. Anderson and P. C. Phillips </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Natural and experimental evolution of sexual conflict within Caenorhabditis nematodes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 2b </td>
   <td style="text-align:left;"> Hermaphroditic, also competition not SS was modulated </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Partridge, L. </td>
   <td style="text-align:right;"> 1980 </td>
   <td style="text-align:left;"> Mate Choice Increases a Component of Offspring Fitness in Fruit-Flies </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Competitive success from 1 generation of populations with and without mate choice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pelabon, C., L. K. Larsen, G. H. Bolstad, A. Viken, I. A. Fleming and G. Rosenqvist </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> The effects of sexual selection on life-history traits: An experimental study on guppies </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Direct and indirect outcomes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Perry, J. C., R. Joag, D. J. Hosken, N. Wedell, J. Radwan and S. Wigby </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Experimental evolution under hyper-promiscuity in Drosophila melanogaster </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> SS was manipulated with SPR not enforced selection conditions </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pischedda, A. and A. Chippindale </td>
   <td style="text-align:right;"> 2005 </td>
   <td style="text-align:left;"> Sex, mutation and fitness: asymmetric costs and routes to recovery through compensatory evolution </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Effect of mutation in diff populations </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pischedda, A. and A. K. Chippindale </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Intralocus sexual conflict diminishes the benefits of sexual selection </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Focussed on fitness effects of conflict </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pitnick, S., W. D. Brown and G. T. Miller </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Evolution of female remating behaviour following experimental removal of sexual selection </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Body size and number of progeny measured. Not purpose of study though </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pitnick, S., G. T. Miller, J. Reagan and B. Holland </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Males' evolutionary responses to experimental removal of sexual selection </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> male and population fitness outcomes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Plesnar, A., M. Konior and J. Radwan </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> The role of sexual selection in purging the genome of induced mutations in the bulb mite (Rizoglyphus robini) </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Plesnar-Bielak, A., A. M. Skrzynecka, Z. M. Prokop, M. Kolasa, M. Dzia_o and J. Radwan </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> No Evidence for Reproductive Isolation through Sexual Conflict in the Bulb Mite Rhizoglyphus robini </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Reproductive isolation measured without fitness components </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Plesnar-Bielak, A., A. M. Skrzynecka, Z. M. Prokop and J. Radwan </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Mating system affects population performance and extinction risk under environmental challenge </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Power, D. J. and L. Holman </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Polyandrous females found fitter populations </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Remating was presented to the females 72 hours after first mating. Measuring effects of polyandry, thus multiple mating has more of an effect. Post copulatory selection will take place though. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Power, D. J. and L. Holman </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Assessing the alignment of sexual and natural selection using radiomutagenized seed beetles </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Experiment 2 Measures affect of SS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Price, T. A. R., G. D. D. Hurst and N. Wedell </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Polyandry Prevents Extinction </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Appears that individuals that only mated once still had a choice, post cop SS would be enacted then. Interested in mating freq over choice </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Prokop, Z. M., M. A. Prus, T. S. Gaczorek, K. Sychta, J. K. Palka, A. Plesnar-Bielak and M. Skarbo_ </td>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:left;"> Do males pay for sex? Sex-specific selection coefficients suggest not </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> SS was estimated using models </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Promislow, D. E. L., E. A. Smith and L. Pearse </td>
   <td style="text-align:right;"> 1998 </td>
   <td style="text-align:left;"> Adult fitness consequences of sexual selection in Drosophila melanogaster </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Radwan, J. </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Effectiveness of sexual selection in removing mutations induced with ionizing radiation </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Direct fitness outcomes measured </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Radwan, J., J. Unrug, K. Snigorska and K. Gawronska </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Effectiveness of sexual selection in preventing fitness deterioration in bulb mite populations under relaxed natural selection </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Direct fitness outcomes measured </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rundle, H. D., S. F. Chenoweth and M. W. Blows </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> The roles of natural and sexual selection during adaptation to a novel environment </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Direct fitness outcomes measured </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rundle, H. D., S. F. Chenoweth and M. W. Blows </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> The diversification of mate preferences by natural and sexual selection </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> CHCs / mate preference outcome measured alongside natural selection </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rundle, H. D., A. Odeen and A. O. Mooers </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> An experimental test for indirect benefits in Drosophila melanogaster </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Between studs and duds not SS+ / SS- </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Savic Veselinovic, M., S. Pavkovic-Lucic, Z. Kurbalija Novicic, M. Jelic and M. Andelkovic </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Sexual Selection Can Reduce Mutational Load in Drosophila Subobscura </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No ES </td>
   <td style="text-align:left;"> Irradiated and direct fitness outcomes measured </td>
  </tr>
  <tr>
   <td style="text-align:left;"> _e_lija, D., I. Marecko and N. Tucic </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Sexual selection and senescence: Do seed beetle males (Acanthoscelides obtectus, Bruchidae, Coleoptera) shape the longevity of their mates? </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> While there is monoandrous lines, these lines were not enforced and choice still existed. Put post-cop choice may be stronger in other lines. This is a strange setup and may be hard to compare with other studies </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sharma, M. D., J. Hunt and D. J. Hosken </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Antagonistic Responses to Natural and Sexual Selection and the Sex-Specific Evolution of Cuticular Hydrocarbons in Drosophila Simulans </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> CHCs / mate preference outcome measured alongside natural selection </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sharp, N. P. and A. F. Agrawal </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Mating density and the strength of sexual selection against deleterious alleles in Drosophila melanogaster </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> One generation w/ gene freq. Also no enforced monogamy </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sharp, N. P. and A. F. Agrawal </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Sexual Selection and the Random Union of Gametes: Testing for a Correlation in Fitness between Mates in Drosophila melanogaster </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Assortive mating study </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Simmons, L. W. and R. C. Firman </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Experimental Evidence for the Evolution of the Mammalian Baculum by Sexual Selection </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> States that "Far less is known of the fitness consequences of variation in baculum morphology for mammals." - No direct link with fitness advantage. However the size is cited as having a fitness advantage </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Simmons, L. W. and F. Garcia-Gonzalez </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Evolutionary Reduction in Testes Size and Competitive Fertilization Success in Response to the Experimental Removal of Sexual Selection in Dung Beetles </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Direct fitness outcomes measured </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Simmons, L. W. and F. Garcia-Gonzalez </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Experimental coevolution of male and female genital morphology </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Genital morphology has conflicting fitness outcomes for males and females </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Simmons, L. W., C. M. House, J. Hunt and F. Garcia-Gonzalez </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Evolutionary Response to Sexual Selection in Male Genital Morphology </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Genital Morphology </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Snook, R. R., N. A. Gidaszewski, T. Chapman and L. W. Simmons </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Sexual selection and the evolution of secondary sexual traits: sex comb evolution in Drosophila </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> In D. pseudo monogamy was enforced. Sex combs are cited as having  positive fitness effects at high and low numbers. Would not give an accurate representation of a fitness comparison </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tilszer, M., K. Antoszczyk, N. Sa_ek, E. Zajac and J. Radwan </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Evolution under relaxed sexual conflict in the bulb mite Rhizoglyphus robini </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> van Lieshout, E., K. B. McNamara and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Rapid Loss of Behavioral Plasticity and Immunocompetence under Intense Sexual Selection </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Did not use enforced monogamy but had different OSR </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Whitlock, M. C. and D. Bourguet </td>
   <td style="text-align:right;"> 2000 </td>
   <td style="text-align:left;"> Factors affecting the genetic load in Drosophila: Synergistic epistasis and correlations among fitness components </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> No manipulation of SS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Female resistance to male harm evolves in response to manipulation of sexual conflict </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Did not use enforced monogamy but had different SR </td>
  </tr>
</tbody>
</table></div>



### Effect Size Calculation

A spreadsheet describing the data that was extracted from each of the included studies is included as supplementary material. It details the type of data collected for each study (arithmatic means, SD, n, F-statistic, chi-squared, proportion etc.). The rules utilised were as follows: 

1. Arithmatic means, standard deviations/errors and sample sizes were extracted from a paper, supplementary material or a linked data repository (e.g. Data Dryad). This was possible when means and SD were reported in text or in a table. We would preferentially extract data for each experimental evolution line/replicat/family if possible and only extract data for the final reported generation (which was noted down).

2. If we could not find the means and SD in text format we used web-plot digitizer (v.3.12) to extract data from graphs. 

3. If means were not reported then we extracted a summary statistic or proportion value, which we could later convert to Hedges g' using the _compute.es_ package. Summary statistics included _F_, _z_, _t_ and _chi^2^_. These conversions still required providing sample sizes for each treatment so these needed to be extractable from the study. Some summary statistics were obtained from generalized linear model summary tabels, others from straight forward ANOVAs and then some from more complex analysis such as proportional hazards statistical tests. 

4. We also collected various covariates for some of the studies (**Table S3**), which are discussed later.

______________

## The Effect Size Dataset

### Table of Effect Sizes

>Justin Says: I think I need to clean up some of the notes and make them more readable

**Table S3:** Table of effect sizes included in our meta-analysis. See the text following the table for an explanation of each column.
 <br/><br/> 
<input type=button class=hideshow></input>

```r
# Load the data and clean up the variable formats
prelim.data <- read.csv('data/Preliminary data frame 22.2.18.csv')
prelim.data$Study.ID <- prelim.data$Study.ID %>% factor()
prelim.data$Taxon <- prelim.data$Taxon %>% factor()
prelim.data$Group.ID <- prelim.data$Group.ID %>% factor()
prelim.data$Authors <- prelim.data$Authors %>% factor()
prelim.data$Environment <- prelim.data$Environment %>% factor %>% relevel(ref="Unstressed")
prelim.data$Sex <- prelim.data$Sex %>% factor %>% relevel(ref="B")
prelim.data$Ambiguous <- prelim.data$Ambiguous %>% factor()
prelim.data$Species <- prelim.data$Species %>% factor()
#Outcome.Class.2 is using the categories that were decided by survey. I am keeping both just to check them against each other (how much of a difference it makes)
prelim.data$Outcome.Class <- prelim.data$Outcome.Class %>% factor() %>% relevel(ref="Indirect")
prelim.data$Enforced.Monogamy <- prelim.data$Enforced.Monogamy %>% factor() %>% relevel(ref="NO")
prelim.data$Pre.cop <- prelim.data$Pre.cop %>% factor() %>% relevel(ref="0")
prelim.data$Post.cop <- prelim.data$Post.cop %>% factor() %>% relevel(ref="0")
prelim.data$Blinding <- prelim.data$Blinding %>% factor()

kable(prelim.data, "html") %>%
  kable_styling() %>%
  scroll_box(width = "800px", height = "500px")
```

<div style="border: 1px solid #ddd; padding: 5px; overflow-y: scroll; height:500px; overflow-x: scroll; width:800px; "><table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Study.ID </th>
   <th style="text-align:left;"> Group.ID </th>
   <th style="text-align:left;"> Authors </th>
   <th style="text-align:right;"> Year </th>
   <th style="text-align:left;"> Species </th>
   <th style="text-align:left;"> Taxon </th>
   <th style="text-align:right;"> SS.density.high.to.low </th>
   <th style="text-align:right;"> SS.ratio.high </th>
   <th style="text-align:right;"> SS.density.high </th>
   <th style="text-align:left;"> Pre.cop </th>
   <th style="text-align:left;"> Post.cop </th>
   <th style="text-align:left;"> Blinding </th>
   <th style="text-align:right;"> Generations </th>
   <th style="text-align:left;"> Enforced.Monogamy </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:left;"> Outcome </th>
   <th style="text-align:left;"> Sex </th>
   <th style="text-align:left;"> Ambiguous </th>
   <th style="text-align:left;"> Outcome.Class </th>
   <th style="text-align:left;"> Environment </th>
   <th style="text-align:right;"> g </th>
   <th style="text-align:right;"> var.g </th>
   <th style="text-align:right;"> mean.low </th>
   <th style="text-align:right;"> sd.low </th>
   <th style="text-align:right;"> n.low </th>
   <th style="text-align:right;"> mean.high </th>
   <th style="text-align:right;"> sd.high </th>
   <th style="text-align:right;"> n.high </th>
   <th style="text-align:right;"> JIF </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> Almbro, M. and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Onthophagus taurus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 182 </td>
   <td style="text-align:left;"> Strength </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.385 </td>
   <td style="text-align:right;"> 0.022 </td>
   <td style="text-align:right;"> 0.0470000 </td>
   <td style="text-align:right;"> 0.0572364 </td>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:right;"> 0.0940000 </td>
   <td style="text-align:right;"> 0.1621697 </td>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> Almbro, M. and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Onthophagus taurus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 182 </td>
   <td style="text-align:left;"> Strength </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 0.022 </td>
   <td style="text-align:right;"> 0.1170000 </td>
   <td style="text-align:right;"> 0.1717091 </td>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:right;"> 0.1170000 </td>
   <td style="text-align:right;"> 0.1717091 </td>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> Almbro, M. and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Onthophagus taurus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 222 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.172 </td>
   <td style="text-align:right;"> 0.018 </td>
   <td style="text-align:right;"> 1.8920000 </td>
   <td style="text-align:right;"> 0.9060662 </td>
   <td style="text-align:right;"> 111 </td>
   <td style="text-align:right;"> 2.0510000 </td>
   <td style="text-align:right;"> 0.9376732 </td>
   <td style="text-align:right;"> 111 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> Almbro, M. and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Onthophagus taurus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 222 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.204 </td>
   <td style="text-align:right;"> 0.018 </td>
   <td style="text-align:right;"> 2.1900000 </td>
   <td style="text-align:right;"> 0.9692801 </td>
   <td style="text-align:right;"> 111 </td>
   <td style="text-align:right;"> 2.3820000 </td>
   <td style="text-align:right;"> 0.9060662 </td>
   <td style="text-align:right;"> 111 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> Almbro, M. and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Onthophagus taurus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 414 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> 0.258 </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> 15.4000000 </td>
   <td style="text-align:right;"> 10.0712462 </td>
   <td style="text-align:right;"> 207 </td>
   <td style="text-align:right;"> 18.0000000 </td>
   <td style="text-align:right;"> 10.0712462 </td>
   <td style="text-align:right;"> 207 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Arbuthnott, D. and H. D. Rundle </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 60.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 400 </td>
   <td style="text-align:left;"> Mutant Frequency </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.011 </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.864 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Arbuthnott, D. and H. D. Rundle </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 60.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 400 </td>
   <td style="text-align:left;"> Mutant Frequency </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.434 </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.864 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Arbuthnott, D. and H. D. Rundle </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 60.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 400 </td>
   <td style="text-align:left;"> Mutant Frequency </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.064 </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.864 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Arbuthnott, D. and H. D. Rundle </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 60.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 400 </td>
   <td style="text-align:left;"> Mutant Frequency </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.037 </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.864 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Arbuthnott, D. and H. D. Rundle </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 60.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 400 </td>
   <td style="text-align:left;"> Mutant Frequency </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.129 </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.864 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Arbuthnott, D. and H. D. Rundle </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 60.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 400 </td>
   <td style="text-align:left;"> Mutant Frequency </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.032 </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.864 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> Archer, C. R., E. Duffy, D. J. Hosken, M. Mokkonen, K. Okada, K. Oku, M. D. Sharma and J. Hunt </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Drosophila simulans </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.500 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.971 </td>
   <td style="text-align:right;"> 0.005 </td>
   <td style="text-align:right;"> 30.5200000 </td>
   <td style="text-align:right;"> 5.9396970 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 24.2100000 </td>
   <td style="text-align:right;"> 7.0003571 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 5.210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> Archer, C. R., E. Duffy, D. J. Hosken, M. Mokkonen, K. Okada, K. Oku, M. D. Sharma and J. Hunt </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Drosophila simulans </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.500 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.154 </td>
   <td style="text-align:right;"> 0.004 </td>
   <td style="text-align:right;"> 34.2800000 </td>
   <td style="text-align:right;"> 26.9407684 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 31.3200000 </td>
   <td style="text-align:right;"> 4.0305087 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 5.210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> Archer, C. R., E. Duffy, D. J. Hosken, M. Mokkonen, K. Okada, K. Oku, M. D. Sharma and J. Hunt </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Drosophila simulans </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.500 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Fitness Senescence </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.074 </td>
   <td style="text-align:right;"> 0.004 </td>
   <td style="text-align:right;"> 3.6300000 </td>
   <td style="text-align:right;"> 1.2727922 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 3.4300000 </td>
   <td style="text-align:right;"> 3.6062446 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 5.210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> Archer, C. R., E. Duffy, D. J. Hosken, M. Mokkonen, K. Okada, K. Oku, M. D. Sharma and J. Hunt </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Drosophila simulans </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.500 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Fitness Senescence </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.087 </td>
   <td style="text-align:right;"> 0.004 </td>
   <td style="text-align:right;"> 3.9200000 </td>
   <td style="text-align:right;"> 1.4849242 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 4.3500000 </td>
   <td style="text-align:right;"> 6.7882251 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 5.210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> Archer, C. R., E. Duffy, D. J. Hosken, M. Mokkonen, K. Okada, K. Oku, M. D. Sharma and J. Hunt </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Drosophila simulans </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.500 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.868 </td>
   <td style="text-align:right;"> 0.005 </td>
   <td style="text-align:right;"> 0.0295858 </td>
   <td style="text-align:right;"> 0.0063640 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 0.0372000 </td>
   <td style="text-align:right;"> 0.0106066 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 5.210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> Archer, C. R., E. Duffy, D. J. Hosken, M. Mokkonen, K. Okada, K. Oku, M. D. Sharma and J. Hunt </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Drosophila simulans </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.500 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.148 </td>
   <td style="text-align:right;"> 0.004 </td>
   <td style="text-align:right;"> 0.0264000 </td>
   <td style="text-align:right;"> 0.0254558 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 0.0291000 </td>
   <td style="text-align:right;"> 0.0042426 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 5.210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> Archer, C. R., E. Duffy, D. J. Hosken, M. Mokkonen, K. Okada, K. Oku, M. D. Sharma and J. Hunt </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Drosophila simulans </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.500 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.780 </td>
   <td style="text-align:right;"> 0.005 </td>
   <td style="text-align:right;"> 35.5500000 </td>
   <td style="text-align:right;"> 11.0308658 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 26.9400000 </td>
   <td style="text-align:right;"> 11.0308658 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 5.210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> Archer, C. R., E. Duffy, D. J. Hosken, M. Mokkonen, K. Okada, K. Oku, M. D. Sharma and J. Hunt </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Drosophila simulans </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.500 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.146 </td>
   <td style="text-align:right;"> 0.004 </td>
   <td style="text-align:right;"> 34.2800000 </td>
   <td style="text-align:right;"> 26.9407684 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 30.1900000 </td>
   <td style="text-align:right;"> 28.8499567 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 5.210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> Archer, C. R., E. Duffy, D. J. Hosken, M. Mokkonen, K. Okada, K. Oku, M. D. Sharma and J. Hunt </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Drosophila simulans </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.500 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Fitness Senescence </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.021 </td>
   <td style="text-align:right;"> 0.004 </td>
   <td style="text-align:right;"> 4.5000000 </td>
   <td style="text-align:right;"> 6.1518290 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 4.3300000 </td>
   <td style="text-align:right;"> 9.9702056 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 5.210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> Archer, C. R., E. Duffy, D. J. Hosken, M. Mokkonen, K. Okada, K. Oku, M. D. Sharma and J. Hunt </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Drosophila simulans </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.500 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Fitness Senescence </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.038 </td>
   <td style="text-align:right;"> 0.004 </td>
   <td style="text-align:right;"> 4.8200000 </td>
   <td style="text-align:right;"> 5.9396970 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 5.1500000 </td>
   <td style="text-align:right;"> 10.8187337 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 5.210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> Archer, C. R., E. Duffy, D. J. Hosken, M. Mokkonen, K. Okada, K. Oku, M. D. Sharma and J. Hunt </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Drosophila simulans </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.500 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.855 </td>
   <td style="text-align:right;"> 0.005 </td>
   <td style="text-align:right;"> 0.2580000 </td>
   <td style="text-align:right;"> 0.0042426 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 0.0339000 </td>
   <td style="text-align:right;"> 0.0127279 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 5.210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> Archer, C. R., E. Duffy, D. J. Hosken, M. Mokkonen, K. Okada, K. Oku, M. D. Sharma and J. Hunt </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Drosophila simulans </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.500 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.176 </td>
   <td style="text-align:right;"> 0.004 </td>
   <td style="text-align:right;"> 0.0267000 </td>
   <td style="text-align:right;"> 0.0169706 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 0.0305000 </td>
   <td style="text-align:right;"> 0.0254558 </td>
   <td style="text-align:right;"> 450 </td>
   <td style="text-align:right;"> 5.210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Bernasconi, G. and L. Keller </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.533 </td>
   <td style="text-align:right;"> 0.242 </td>
   <td style="text-align:right;"> 0.5600000 </td>
   <td style="text-align:right;"> 0.3600000 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 0.9700000 </td>
   <td style="text-align:right;"> 0.0400000 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2.673 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Bernasconi, G. and L. Keller </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.123 </td>
   <td style="text-align:right;"> 0.184 </td>
   <td style="text-align:right;"> 63.0000000 </td>
   <td style="text-align:right;"> 27.0000000 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 60.0000000 </td>
   <td style="text-align:right;"> 19.0000000 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2.673 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Brommer, J. E., C. Fricke, D. A. Edward and T. Chapman </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.378 </td>
   <td style="text-align:right;"> 0.043 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> 0.3316625 </td>
   <td style="text-align:right;"> 44 </td>
   <td style="text-align:right;"> 0.8700000 </td>
   <td style="text-align:right;"> 0.3500000 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 4.864 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., A. P. Beckerman, L. Br_stle, K. Green and R. R. Snook </td>
   <td style="text-align:right;"> 2005 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 200 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.216 </td>
   <td style="text-align:right;"> 0.020 </td>
   <td style="text-align:right;"> 76.9000000 </td>
   <td style="text-align:right;"> 47.0000000 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 66.4000000 </td>
   <td style="text-align:right;"> 50.0000000 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 4.464 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., A. P. Beckerman, L. Br_stle, K. Green and R. R. Snook </td>
   <td style="text-align:right;"> 2005 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 200 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.280 </td>
   <td style="text-align:right;"> 0.020 </td>
   <td style="text-align:right;"> 120.6000000 </td>
   <td style="text-align:right;"> 119.0000000 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 153.6000000 </td>
   <td style="text-align:right;"> 116.0000000 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 4.464 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., A. P. Beckerman, L. Br_stle, K. Green and R. R. Snook </td>
   <td style="text-align:right;"> 2005 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 200 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.365 </td>
   <td style="text-align:right;"> 0.045 </td>
   <td style="text-align:right;"> 0.7810000 </td>
   <td style="text-align:right;"> 0.0700000 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 0.8740000 </td>
   <td style="text-align:right;"> 0.0500000 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 4.464 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., A. P. Beckerman, L. Br_stle, K. Green and R. R. Snook </td>
   <td style="text-align:right;"> 2005 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 200 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.244 </td>
   <td style="text-align:right;"> 0.020 </td>
   <td style="text-align:right;"> 108.5000000 </td>
   <td style="text-align:right;"> 44.0000000 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 97.9000000 </td>
   <td style="text-align:right;"> 43.0000000 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 4.464 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., A. P. Beckerman, L. Br_stle, K. Green and R. R. Snook </td>
   <td style="text-align:right;"> 2005 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 200 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.281 </td>
   <td style="text-align:right;"> 0.020 </td>
   <td style="text-align:right;"> 164.1000000 </td>
   <td style="text-align:right;"> 119.0000000 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 197.5000000 </td>
   <td style="text-align:right;"> 119.0000000 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 4.464 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., A. P. Beckerman, L. Br_stle, K. Green and R. R. Snook </td>
   <td style="text-align:right;"> 2005 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 200 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.311 </td>
   <td style="text-align:right;"> 0.155 </td>
   <td style="text-align:right;"> 0.9680000 </td>
   <td style="text-align:right;"> 0.0400000 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 0.9450000 </td>
   <td style="text-align:right;"> 0.0400000 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 4.464 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.168 </td>
   <td style="text-align:right;"> 0.184 </td>
   <td style="text-align:right;"> 15.7249071 </td>
   <td style="text-align:right;"> 1.9984654 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 15.3903346 </td>
   <td style="text-align:right;"> 1.7633519 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.576 </td>
   <td style="text-align:right;"> 0.192 </td>
   <td style="text-align:right;"> 15.3903346 </td>
   <td style="text-align:right;"> 2.1160222 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 14.3122677 </td>
   <td style="text-align:right;"> 1.4106815 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.311 </td>
   <td style="text-align:right;"> 0.226 </td>
   <td style="text-align:right;"> 15.0185874 </td>
   <td style="text-align:right;"> 1.0580111 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 16.3940520 </td>
   <td style="text-align:right;"> 0.9404543 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.512 </td>
   <td style="text-align:right;"> 0.190 </td>
   <td style="text-align:right;"> 15.7992565 </td>
   <td style="text-align:right;"> 1.6457951 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 16.6542751 </td>
   <td style="text-align:right;"> 1.5282383 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.373 </td>
   <td style="text-align:right;"> 0.231 </td>
   <td style="text-align:right;"> 15.7249071 </td>
   <td style="text-align:right;"> 1.9984654 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 18.0669145 </td>
   <td style="text-align:right;"> 1.1755679 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.190 </td>
   <td style="text-align:right;"> 0.219 </td>
   <td style="text-align:right;"> 15.3903346 </td>
   <td style="text-align:right;"> 2.1160222 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 17.6208178 </td>
   <td style="text-align:right;"> 1.4106815 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.305 </td>
   <td style="text-align:right;"> 0.226 </td>
   <td style="text-align:right;"> 15.0185874 </td>
   <td style="text-align:right;"> 1.0580111 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 17.1003718 </td>
   <td style="text-align:right;"> 1.8809086 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.928 </td>
   <td style="text-align:right;"> 0.276 </td>
   <td style="text-align:right;"> 15.7992565 </td>
   <td style="text-align:right;"> 1.6457951 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 18.5873606 </td>
   <td style="text-align:right;"> 1.0580111 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.750 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.713 </td>
   <td style="text-align:right;"> 0.257 </td>
   <td style="text-align:right;"> 15.3903346 </td>
   <td style="text-align:right;"> 1.7633519 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 18.0700000 </td>
   <td style="text-align:right;"> 1.1755679 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.750 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 2.248 </td>
   <td style="text-align:right;"> 0.310 </td>
   <td style="text-align:right;"> 14.3122677 </td>
   <td style="text-align:right;"> 1.4106815 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 17.6200000 </td>
   <td style="text-align:right;"> 1.4106815 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.750 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.458 </td>
   <td style="text-align:right;"> 0.189 </td>
   <td style="text-align:right;"> 16.3940520 </td>
   <td style="text-align:right;"> 0.9404543 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 17.1000000 </td>
   <td style="text-align:right;"> 1.8809086 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.750 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.414 </td>
   <td style="text-align:right;"> 0.233 </td>
   <td style="text-align:right;"> 16.6542751 </td>
   <td style="text-align:right;"> 1.5282383 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 18.5900000 </td>
   <td style="text-align:right;"> 1.0580111 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.060 </td>
   <td style="text-align:right;"> 0.096 </td>
   <td style="text-align:right;"> 622.3853211 </td>
   <td style="text-align:right;"> 367.6177813 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 642.9357798 </td>
   <td style="text-align:right;"> 301.9717489 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.089 </td>
   <td style="text-align:right;"> 0.096 </td>
   <td style="text-align:right;"> 760.3669725 </td>
   <td style="text-align:right;"> 407.0054007 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 733.9449541 </td>
   <td style="text-align:right;"> 354.4885748 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.214 </td>
   <td style="text-align:right;"> 0.097 </td>
   <td style="text-align:right;"> 728.0733945 </td>
   <td style="text-align:right;"> 407.0054007 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 648.8073394 </td>
   <td style="text-align:right;"> 315.1009554 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.515 </td>
   <td style="text-align:right;"> 0.099 </td>
   <td style="text-align:right;"> 622.4000000 </td>
   <td style="text-align:right;"> 367.6177800 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 819.0825688 </td>
   <td style="text-align:right;"> 380.7469877 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.768 </td>
   <td style="text-align:right;"> 0.103 </td>
   <td style="text-align:right;"> 760.4000000 </td>
   <td style="text-align:right;"> 407.0054000 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 1200.7339450 </td>
   <td style="text-align:right;"> 682.7187366 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.068 </td>
   <td style="text-align:right;"> 0.110 </td>
   <td style="text-align:right;"> 728.1000000 </td>
   <td style="text-align:right;"> 407.0054000 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 1150.8256880 </td>
   <td style="text-align:right;"> 367.6177813 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.750 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.503 </td>
   <td style="text-align:right;"> 0.099 </td>
   <td style="text-align:right;"> 642.9357798 </td>
   <td style="text-align:right;"> 301.9717489 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 819.0825688 </td>
   <td style="text-align:right;"> 380.7469877 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.750 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.841 </td>
   <td style="text-align:right;"> 0.105 </td>
   <td style="text-align:right;"> 733.9449541 </td>
   <td style="text-align:right;"> 354.4885748 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 1200.7339450 </td>
   <td style="text-align:right;"> 682.7187366 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows, N. S. Badcock and R. R. Snook </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.750 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.437 </td>
   <td style="text-align:right;"> 0.122 </td>
   <td style="text-align:right;"> 648.8073394 </td>
   <td style="text-align:right;"> 315.1009554 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 1150.8256880 </td>
   <td style="text-align:right;"> 367.6177813 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.861 </td>
   <td style="text-align:right;"> 0.111 </td>
   <td style="text-align:right;"> 237.3000000 </td>
   <td style="text-align:right;"> 55.0072700 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 169.5000000 </td>
   <td style="text-align:right;"> 95.8836795 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.655 </td>
   <td style="text-align:right;"> 0.118 </td>
   <td style="text-align:right;"> 210.6000000 </td>
   <td style="text-align:right;"> 67.6189300 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 170.5000000 </td>
   <td style="text-align:right;"> 50.7141992 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.026 </td>
   <td style="text-align:right;"> 0.123 </td>
   <td style="text-align:right;"> 0.5230000 </td>
   <td style="text-align:right;"> 0.1833600 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 0.5350000 </td>
   <td style="text-align:right;"> 0.2460732 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.360 </td>
   <td style="text-align:right;"> 0.140 </td>
   <td style="text-align:right;"> 0.4960000 </td>
   <td style="text-align:right;"> 0.1772900 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 0.6590000 </td>
   <td style="text-align:right;"> 0.2680019 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -1.447 </td>
   <td style="text-align:right;"> 0.132 </td>
   <td style="text-align:right;"> 237.3000000 </td>
   <td style="text-align:right;"> 55.0072700 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 150.0000000 </td>
   <td style="text-align:right;"> 63.4958266 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.739 </td>
   <td style="text-align:right;"> 0.114 </td>
   <td style="text-align:right;"> 210.6000000 </td>
   <td style="text-align:right;"> 67.6189300 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 154.1000000 </td>
   <td style="text-align:right;"> 80.6396305 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.620 </td>
   <td style="text-align:right;"> 0.160 </td>
   <td style="text-align:right;"> 0.5230000 </td>
   <td style="text-align:right;"> 0.1833600 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 0.7750000 </td>
   <td style="text-align:right;"> 0.2185246 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.450 </td>
   <td style="text-align:right;"> 0.140 </td>
   <td style="text-align:right;"> 0.4960000 </td>
   <td style="text-align:right;"> 0.1772900 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 0.6930000 </td>
   <td style="text-align:right;"> 0.2310216 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.750 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.233 </td>
   <td style="text-align:right;"> 0.110 </td>
   <td style="text-align:right;"> 169.5000000 </td>
   <td style="text-align:right;"> 95.8836795 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 150.0000000 </td>
   <td style="text-align:right;"> 63.4958266 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.750 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.235 </td>
   <td style="text-align:right;"> 0.107 </td>
   <td style="text-align:right;"> 170.5000000 </td>
   <td style="text-align:right;"> 50.7141992 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 154.1000000 </td>
   <td style="text-align:right;"> 80.6396305 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.750 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.590 </td>
   <td style="text-align:right;"> 0.160 </td>
   <td style="text-align:right;"> 0.5350000 </td>
   <td style="text-align:right;"> 0.2460732 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 0.7750000 </td>
   <td style="text-align:right;"> 0.2185246 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.750 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.080 </td>
   <td style="text-align:right;"> 0.150 </td>
   <td style="text-align:right;"> 0.6590000 </td>
   <td style="text-align:right;"> 0.2680019 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 0.6930000 </td>
   <td style="text-align:right;"> 0.2310216 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.520 </td>
   <td style="text-align:right;"> 0.105 </td>
   <td style="text-align:right;"> 500.3000000 </td>
   <td style="text-align:right;"> 174.4133000 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 403.8000000 </td>
   <td style="text-align:right;"> 261.3466663 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.843 </td>
   <td style="text-align:right;"> 0.123 </td>
   <td style="text-align:right;"> 474.7000000 </td>
   <td style="text-align:right;"> 195.0229000 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 315.4000000 </td>
   <td style="text-align:right;"> 173.5827468 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -2.487 </td>
   <td style="text-align:right;"> 0.188 </td>
   <td style="text-align:right;"> 403.8000000 </td>
   <td style="text-align:right;"> 261.3466663 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 228.1000000 </td>
   <td style="text-align:right;"> 152.5549081 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -1.065 </td>
   <td style="text-align:right;"> 0.122 </td>
   <td style="text-align:right;"> 315.4000000 </td>
   <td style="text-align:right;"> 173.5827468 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 266.1000000 </td>
   <td style="text-align:right;"> 188.3044344 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.750 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.796 </td>
   <td style="text-align:right;"> 0.118 </td>
   <td style="text-align:right;"> 500.3000000 </td>
   <td style="text-align:right;"> 174.4133000 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 228.1000000 </td>
   <td style="text-align:right;"> 152.5549081 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Crudgington, H. S., S. Fellows and R. R. Snook </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.750 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.266 </td>
   <td style="text-align:right;"> 0.108 </td>
   <td style="text-align:right;"> 474.7000000 </td>
   <td style="text-align:right;"> 195.0229000 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 266.1000000 </td>
   <td style="text-align:right;"> 188.3044344 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 3.636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Debelle, A., M. G. Ritchie and R. R. Snook </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 2038 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.555 </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 2.2200000 </td>
   <td style="text-align:right;"> 0.0730000 </td>
   <td style="text-align:right;"> 1019 </td>
   <td style="text-align:right;"> 2.2600000 </td>
   <td style="text-align:right;"> 0.0710000 </td>
   <td style="text-align:right;"> 1019 </td>
   <td style="text-align:right;"> 2.792 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Debelle, A., M. G. Ritchie and R. R. Snook </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 2038 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.111 </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 2.4500000 </td>
   <td style="text-align:right;"> 0.0820000 </td>
   <td style="text-align:right;"> 1019 </td>
   <td style="text-align:right;"> 2.4600000 </td>
   <td style="text-align:right;"> 0.0980000 </td>
   <td style="text-align:right;"> 1019 </td>
   <td style="text-align:right;"> 2.792 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Debelle, A., M. G. Ritchie and R. R. Snook </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 2038 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.663 </td>
   <td style="text-align:right;"> 0.004 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 2.792 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Debelle, A., M. G. Ritchie and R. R. Snook </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 2038 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.655 </td>
   <td style="text-align:right;"> 0.004 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 2.792 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Debelle, A., M. G. Ritchie and R. R. Snook </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 2038 </td>
   <td style="text-align:left;"> Mating Latency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.197 </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 126.5000000 </td>
   <td style="text-align:right;"> 15.8000000 </td>
   <td style="text-align:right;"> 1019 </td>
   <td style="text-align:right;"> 129.4000000 </td>
   <td style="text-align:right;"> 13.5000000 </td>
   <td style="text-align:right;"> 1019 </td>
   <td style="text-align:right;"> 2.792 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Debelle, A., M. G. Ritchie and R. R. Snook </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 2038 </td>
   <td style="text-align:left;"> Mating Latency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 2.486 </td>
   <td style="text-align:right;"> 0.003 </td>
   <td style="text-align:right;"> 153.8000000 </td>
   <td style="text-align:right;"> 19.7000000 </td>
   <td style="text-align:right;"> 1019 </td>
   <td style="text-align:right;"> 113.6000000 </td>
   <td style="text-align:right;"> 11.6000000 </td>
   <td style="text-align:right;"> 1019 </td>
   <td style="text-align:right;"> 2.792 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Demont, M., V. M. Grazer, L. Michalczyk, A. L. Millard, S. H. Sbilordo, B. C. Emerson, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.810 </td>
   <td style="text-align:right;"> 0.144 </td>
   <td style="text-align:right;"> 91.7000000 </td>
   <td style="text-align:right;"> 9.4400000 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 105.7700000 </td>
   <td style="text-align:right;"> 5.1700000 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 2.606 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Demont, M., V. M. Grazer, L. Michalczyk, A. L. Millard, S. H. Sbilordo, B. C. Emerson, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.299 </td>
   <td style="text-align:right;"> 0.102 </td>
   <td style="text-align:right;"> 93.9700000 </td>
   <td style="text-align:right;"> 21.3500000 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 101.2400000 </td>
   <td style="text-align:right;"> 26.0600000 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 2.606 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Demont, M., V. M. Grazer, L. Michalczyk, A. L. Millard, S. H. Sbilordo, B. C. Emerson, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.222 </td>
   <td style="text-align:right;"> 0.156 </td>
   <td style="text-align:right;"> 106.8500000 </td>
   <td style="text-align:right;"> 6.2000000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 108.6500000 </td>
   <td style="text-align:right;"> 9.2000000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 2.606 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Demont, M., V. M. Grazer, L. Michalczyk, A. L. Millard, S. H. Sbilordo, B. C. Emerson, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.279 </td>
   <td style="text-align:right;"> 0.209 </td>
   <td style="text-align:right;"> 0.3000000 </td>
   <td style="text-align:right;"> 0.0500000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0.4200000 </td>
   <td style="text-align:right;"> 0.0500000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 2.606 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Demont, M., V. M. Grazer, L. Michalczyk, A. L. Millard, S. H. Sbilordo, B. C. Emerson, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 44 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.415 </td>
   <td style="text-align:right;"> 0.090 </td>
   <td style="text-align:right;"> 24.0000000 </td>
   <td style="text-align:right;"> 8.9442719 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 27.0000000 </td>
   <td style="text-align:right;"> 4.8989795 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 2.606 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Demont, M., V. M. Grazer, L. Michalczyk, A. L. Millard, S. H. Sbilordo, B. C. Emerson, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.407 </td>
   <td style="text-align:right;"> 0.088 </td>
   <td style="text-align:right;"> 23.0000000 </td>
   <td style="text-align:right;"> 9.3808315 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 26.0000000 </td>
   <td style="text-align:right;"> 4.7958315 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 2.606 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> Edward, D. A., C. Fricke and T. Chapman </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 0.760 </td>
   <td style="text-align:right;"> 75.00 </td>
   <td style="text-align:right;"> 76.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:left;"> Mating Latency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.324 </td>
   <td style="text-align:right;"> 0.020 </td>
   <td style="text-align:right;"> 6.5230000 </td>
   <td style="text-align:right;"> 5.4190000 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 5.0170000 </td>
   <td style="text-align:right;"> 3.6600000 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 8.090 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> Edward, D. A., C. Fricke and T. Chapman </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 0.760 </td>
   <td style="text-align:right;"> 75.00 </td>
   <td style="text-align:right;"> 76.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:left;"> Mating Duration </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.219 </td>
   <td style="text-align:right;"> 0.020 </td>
   <td style="text-align:right;"> 11.9500000 </td>
   <td style="text-align:right;"> 2.9810000 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 12.6440000 </td>
   <td style="text-align:right;"> 3.3310000 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 8.090 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> Edward, D. A., C. Fricke and T. Chapman </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 0.760 </td>
   <td style="text-align:right;"> 75.00 </td>
   <td style="text-align:right;"> 76.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:left;"> Mating Latency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.099 </td>
   <td style="text-align:right;"> 0.019 </td>
   <td style="text-align:right;"> 5.5121951 </td>
   <td style="text-align:right;"> 3.5893711 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 5.8885017 </td>
   <td style="text-align:right;"> 3.9412702 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 8.090 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> Edward, D. A., C. Fricke and T. Chapman </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 0.760 </td>
   <td style="text-align:right;"> 75.00 </td>
   <td style="text-align:right;"> 76.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:left;"> Mating Duration </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.393 </td>
   <td style="text-align:right;"> 0.020 </td>
   <td style="text-align:right;"> 9.1892361 </td>
   <td style="text-align:right;"> 2.5424101 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 10.4565972 </td>
   <td style="text-align:right;"> 3.7697805 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 8.090 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> Edward, D. A., C. Fricke and T. Chapman </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 0.760 </td>
   <td style="text-align:right;"> 75.00 </td>
   <td style="text-align:right;"> 76.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.070 </td>
   <td style="text-align:right;"> 0.019 </td>
   <td style="text-align:right;"> 72.3810000 </td>
   <td style="text-align:right;"> 35.0550000 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 74.8857645 </td>
   <td style="text-align:right;"> 35.9428779 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 8.090 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> Edward, D. A., C. Fricke and T. Chapman </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 0.760 </td>
   <td style="text-align:right;"> 75.00 </td>
   <td style="text-align:right;"> 76.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.015 </td>
   <td style="text-align:right;"> 0.019 </td>
   <td style="text-align:right;"> 0.6410000 </td>
   <td style="text-align:right;"> 0.5090000 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 0.6491071 </td>
   <td style="text-align:right;"> 0.5545710 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 8.090 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> Edward, D. A., C. Fricke and T. Chapman </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 0.760 </td>
   <td style="text-align:right;"> 75.00 </td>
   <td style="text-align:right;"> 76.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.001 </td>
   <td style="text-align:right;"> 0.019 </td>
   <td style="text-align:right;"> 0.7750000 </td>
   <td style="text-align:right;"> 0.6600000 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 0.7759516 </td>
   <td style="text-align:right;"> 0.7391160 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 8.090 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> Edward, D. A., C. Fricke and T. Chapman </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 0.760 </td>
   <td style="text-align:right;"> 75.00 </td>
   <td style="text-align:right;"> 76.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.312 </td>
   <td style="text-align:right;"> 0.020 </td>
   <td style="text-align:right;"> 81.9595782 </td>
   <td style="text-align:right;"> 34.6116602 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 71.0632689 </td>
   <td style="text-align:right;"> 35.0553994 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 8.090 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> Edward, D. A., C. Fricke and T. Chapman </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 0.760 </td>
   <td style="text-align:right;"> 75.00 </td>
   <td style="text-align:right;"> 76.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.099 </td>
   <td style="text-align:right;"> 0.019 </td>
   <td style="text-align:right;"> 0.5946429 </td>
   <td style="text-align:right;"> 0.5004665 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 0.6446429 </td>
   <td style="text-align:right;"> 0.5049752 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 8.090 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> Edward, D. A., C. Fricke and T. Chapman </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 0.760 </td>
   <td style="text-align:right;"> 75.00 </td>
   <td style="text-align:right;"> 76.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.049 </td>
   <td style="text-align:right;"> 0.019 </td>
   <td style="text-align:right;"> 0.7157439 </td>
   <td style="text-align:right;"> 0.6919384 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 0.7510381 </td>
   <td style="text-align:right;"> 0.7495999 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 8.090 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.396 </td>
   <td style="text-align:right;"> 0.080 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.248 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -1.258 </td>
   <td style="text-align:right;"> 0.114 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.248 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.352 </td>
   <td style="text-align:right;"> 0.076 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.248 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.316 </td>
   <td style="text-align:right;"> 0.146 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.248 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.196 </td>
   <td style="text-align:right;"> 0.132 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.248 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.142 </td>
   <td style="text-align:right;"> 0.104 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.248 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.131 </td>
   <td style="text-align:right;"> 0.072 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.248 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.747 </td>
   <td style="text-align:right;"> 0.360 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.248 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> Male Attractiveness </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -1.177 </td>
   <td style="text-align:right;"> 0.149 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.248 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C., L. Y. Cheam and L. W. Simmons </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> 0.303 </td>
   <td style="text-align:right;"> 0.073 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.276 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C., L. Y. Cheam and L. W. Simmons </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> 1.844 </td>
   <td style="text-align:right;"> 0.105 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.276 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C., F. Garcia-Gonzalez, E. Thyer, S. Wheeler, Z. Yamin, M. Yuan and L. W. Simmons </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> 1.003 </td>
   <td style="text-align:right;"> 0.073 </td>
   <td style="text-align:right;"> 0.7010000 </td>
   <td style="text-align:right;"> 0.0492950 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 0.7470000 </td>
   <td style="text-align:right;"> 0.0438178 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 4.007 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C., M. Gomendio, E. R. S. Roldan and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.963 </td>
   <td style="text-align:right;"> 0.068 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.832 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C., M. Gomendio, E. R. S. Roldan and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -1.733 </td>
   <td style="text-align:right;"> 0.349 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.832 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C., M. Gomendio, E. R. S. Roldan and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -1.717 </td>
   <td style="text-align:right;"> 0.111 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.832 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C., M. Gomendio, E. R. S. Roldan and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.974 </td>
   <td style="text-align:right;"> 0.115 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.832 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C., M. Gomendio, E. R. S. Roldan and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 86 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.599 </td>
   <td style="text-align:right;"> 0.102 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.832 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C., M. Gomendio, E. R. S. Roldan and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.904 </td>
   <td style="text-align:right;"> 0.159 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.832 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C., M. Gomendio, E. R. S. Roldan and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.504 </td>
   <td style="text-align:right;"> 0.199 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.832 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. and L. W. Simmons </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> 0.399 </td>
   <td style="text-align:right;"> 0.026 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.521 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. and L. W. Simmons </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.564 </td>
   <td style="text-align:right;"> 0.100 </td>
   <td style="text-align:right;"> 17.5500000 </td>
   <td style="text-align:right;"> 5.4112845 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 14.4500000 </td>
   <td style="text-align:right;"> 5.3665631 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 3.521 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. and L. W. Simmons </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.328 </td>
   <td style="text-align:right;"> 0.097 </td>
   <td style="text-align:right;"> 16.1500000 </td>
   <td style="text-align:right;"> 4.1143651 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 14.5500000 </td>
   <td style="text-align:right;"> 5.3665631 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 3.521 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. and L. W. Simmons </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> 0.668 </td>
   <td style="text-align:right;"> 0.029 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.521 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. and L. W. Simmons </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 128 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.364 </td>
   <td style="text-align:right;"> 0.031 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.521 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. and L. W. Simmons </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 128 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -1.008 </td>
   <td style="text-align:right;"> 0.035 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.521 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. and L. W. Simmons </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.784 </td>
   <td style="text-align:right;"> 0.030 </td>
   <td style="text-align:right;"> 4.9400000 </td>
   <td style="text-align:right;"> 2.2910260 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 6.6500000 </td>
   <td style="text-align:right;"> 2.0364675 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 3.521 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. and L. W. Simmons </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 128 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.213 </td>
   <td style="text-align:right;"> 0.031 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.521 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. and L. W. Simmons </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 128 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.416 </td>
   <td style="text-align:right;"> 0.032 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.521 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. and L. W. Simmons </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 128 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 0.031 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.521 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Firman, R. C. and L. W. Simmons </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 128 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.408 </td>
   <td style="text-align:right;"> 0.032 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.521 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Fricke, C. and G. Arnqvist </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 155 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> 0.080 </td>
   <td style="text-align:right;"> 0.026 </td>
   <td style="text-align:right;"> 0.0011635 </td>
   <td style="text-align:right;"> 0.0001053 </td>
   <td style="text-align:right;"> 77 </td>
   <td style="text-align:right;"> 0.0011720 </td>
   <td style="text-align:right;"> 0.0001073 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 4.502 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Fricke, C. and G. Arnqvist </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 155 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> 0.102 </td>
   <td style="text-align:right;"> 0.026 </td>
   <td style="text-align:right;"> 0.0009178 </td>
   <td style="text-align:right;"> 0.0000963 </td>
   <td style="text-align:right;"> 77 </td>
   <td style="text-align:right;"> 0.0009283 </td>
   <td style="text-align:right;"> 0.0001084 </td>
   <td style="text-align:right;"> 77 </td>
   <td style="text-align:right;"> 4.502 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Fricke, C. and G. Arnqvist </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:left;"> Development Rate </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.453 </td>
   <td style="text-align:right;"> 0.053 </td>
   <td style="text-align:right;"> 0.8289099 </td>
   <td style="text-align:right;"> 0.0286151 </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 0.8135570 </td>
   <td style="text-align:right;"> 0.0377825 </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 4.502 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Fricke, C. and G. Arnqvist </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:left;"> Development Rate </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.772 </td>
   <td style="text-align:right;"> 0.053 </td>
   <td style="text-align:right;"> 0.8251609 </td>
   <td style="text-align:right;"> 0.0378719 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 0.8534363 </td>
   <td style="text-align:right;"> 0.0346177 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 4.502 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Fricke, C. and G. Arnqvist </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.579 </td>
   <td style="text-align:right;"> 0.054 </td>
   <td style="text-align:right;"> 419.3947368 </td>
   <td style="text-align:right;"> 34.3546995 </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 397.4210526 </td>
   <td style="text-align:right;"> 40.5573545 </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 4.502 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Fricke, C. and G. Arnqvist </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.185 </td>
   <td style="text-align:right;"> 0.050 </td>
   <td style="text-align:right;"> 292.2051282 </td>
   <td style="text-align:right;"> 55.6900159 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 301.0500000 </td>
   <td style="text-align:right;"> 37.3678526 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 4.502 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Fricke, C. and G. Arnqvist </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.476 </td>
   <td style="text-align:right;"> 0.053 </td>
   <td style="text-align:right;"> 0.5462428 </td>
   <td style="text-align:right;"> 0.0780889 </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 0.5107408 </td>
   <td style="text-align:right;"> 0.0691831 </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 4.502 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Fricke, C. and G. Arnqvist </td>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.543 </td>
   <td style="text-align:right;"> 0.052 </td>
   <td style="text-align:right;"> 0.4180605 </td>
   <td style="text-align:right;"> 0.0784570 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 0.4575765 </td>
   <td style="text-align:right;"> 0.0652579 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 4.502 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> Fritzsche, K., I. Booksmythe and G. Arnqvist </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Megabruchidius dorsalis </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 150.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 1200 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.056 </td>
   <td style="text-align:right;"> 0.003 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 8.851 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> Fritzsche, K., I. Booksmythe and G. Arnqvist </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Megabruchidius dorsalis </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 150.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 1200 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.031 </td>
   <td style="text-align:right;"> 0.003 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 8.851 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> Fritzsche, K., I. Booksmythe and G. Arnqvist </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Megabruchidius dorsalis </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 150.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 1200 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.066 </td>
   <td style="text-align:right;"> 0.003 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 8.851 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> Fritzsche, K., I. Booksmythe and G. Arnqvist </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Megabruchidius dorsalis </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 150.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 1200 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.083 </td>
   <td style="text-align:right;"> 0.003 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 8.851 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> Fritzsche, K., N. Timmermeyer, M. Wolter and N. K. Michiels </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Caenorhabditis remanei </td>
   <td style="text-align:left;"> Nematode </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 90 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.197 </td>
   <td style="text-align:right;"> 0.044 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> Fritzsche, K., N. Timmermeyer, M. Wolter and N. K. Michiels </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Caenorhabditis remanei </td>
   <td style="text-align:left;"> Nematode </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 256 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.041 </td>
   <td style="text-align:right;"> 0.016 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> Fritzsche, K., N. Timmermeyer, M. Wolter and N. K. Michiels </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Caenorhabditis remanei </td>
   <td style="text-align:left;"> Nematode </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 256 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.065 </td>
   <td style="text-align:right;"> 0.016 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> Fritzsche, K., N. Timmermeyer, M. Wolter and N. K. Michiels </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Caenorhabditis remanei </td>
   <td style="text-align:left;"> Nematode </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 256 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.078 </td>
   <td style="text-align:right;"> 0.016 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> Fritzsche, K., N. Timmermeyer, M. Wolter and N. K. Michiels </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Caenorhabditis remanei </td>
   <td style="text-align:left;"> Nematode </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 256 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.267 </td>
   <td style="text-align:right;"> 0.016 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> Fritzsche, K., N. Timmermeyer, M. Wolter and N. K. Michiels </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Caenorhabditis remanei </td>
   <td style="text-align:left;"> Nematode </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 184 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> 0.095 </td>
   <td style="text-align:right;"> 0.022 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> Fritzsche, K., N. Timmermeyer, M. Wolter and N. K. Michiels </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Caenorhabditis remanei </td>
   <td style="text-align:left;"> Nematode </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 184 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> 0.407 </td>
   <td style="text-align:right;"> 0.022 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> Fritzsche, K., N. Timmermeyer, M. Wolter and N. K. Michiels </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Caenorhabditis remanei </td>
   <td style="text-align:left;"> Nematode </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 392 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> 0.059 </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> Fritzsche, K., N. Timmermeyer, M. Wolter and N. K. Michiels </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Caenorhabditis remanei </td>
   <td style="text-align:left;"> Nematode </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 60.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 392 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> 0.219 </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> Gay, L., D. J. Hosken, R. Vasudev, T. Tregenza and P. E. Eady </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 60.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 90 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.971 </td>
   <td style="text-align:right;"> 0.073 </td>
   <td style="text-align:right;"> 1.8700000 </td>
   <td style="text-align:right;"> 0.0822192 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 2.0400000 </td>
   <td style="text-align:right;"> 0.0885438 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 3.816 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> Gay, L., D. J. Hosken, R. Vasudev, T. Tregenza and P. E. Eady </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 60.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 90 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.385 </td>
   <td style="text-align:right;"> 0.061 </td>
   <td style="text-align:right;"> 0.4500000 </td>
   <td style="text-align:right;"> 0.1201666 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 0.6400000 </td>
   <td style="text-align:right;"> 0.1517893 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 3.816 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> Gay, L., D. J. Hosken, R. Vasudev, T. Tregenza and P. E. Eady </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 60.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 90 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.661 </td>
   <td style="text-align:right;"> 0.052 </td>
   <td style="text-align:right;"> 0.1571000 </td>
   <td style="text-align:right;"> 0.0059000 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 0.1626000 </td>
   <td style="text-align:right;"> 0.0103000 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 3.816 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Grazer, V. M., M. Demont, L. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 228 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.211 </td>
   <td style="text-align:right;"> 0.018 </td>
   <td style="text-align:right;"> 149.9000000 </td>
   <td style="text-align:right;"> 174.9000000 </td>
   <td style="text-align:right;"> 114 </td>
   <td style="text-align:right;"> 181.6000000 </td>
   <td style="text-align:right;"> 119.5000000 </td>
   <td style="text-align:right;"> 114 </td>
   <td style="text-align:right;"> 3.368 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Grazer, V. M., M. Demont, L. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 240 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.214 </td>
   <td style="text-align:right;"> 0.017 </td>
   <td style="text-align:right;"> 240.6000000 </td>
   <td style="text-align:right;"> 189.7000000 </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:right;"> 291.5000000 </td>
   <td style="text-align:right;"> 275.6000000 </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:right;"> 3.368 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., L. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.141 </td>
   <td style="text-align:right;"> 0.059 </td>
   <td style="text-align:right;"> 6.9700000 </td>
   <td style="text-align:right;"> 1.7400000 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 6.7000000 </td>
   <td style="text-align:right;"> 2.0300000 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 2.591 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., L. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.848 </td>
   <td style="text-align:right;"> 0.065 </td>
   <td style="text-align:right;"> 6.3300000 </td>
   <td style="text-align:right;"> 1.3400000 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 7.7900000 </td>
   <td style="text-align:right;"> 2.0000000 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 2.591 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., L. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.175 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 80.8600000 </td>
   <td style="text-align:right;"> 41.5400000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 87.9200000 </td>
   <td style="text-align:right;"> 39.1000000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 2.591 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., L. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.089 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 85.0000000 </td>
   <td style="text-align:right;"> 41.5400000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 88.9400000 </td>
   <td style="text-align:right;"> 46.4300000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 2.591 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., L. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.097 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 92.8100000 </td>
   <td style="text-align:right;"> 35.8400000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 89.2100000 </td>
   <td style="text-align:right;"> 38.2800000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 2.591 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., L. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.070 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 87.9900000 </td>
   <td style="text-align:right;"> 35.8400000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 90.2900000 </td>
   <td style="text-align:right;"> 29.3200000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 2.591 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.107 </td>
   <td style="text-align:right;"> 0.054 </td>
   <td style="text-align:right;"> 6.0300000 </td>
   <td style="text-align:right;"> 2.5400000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 5.7500000 </td>
   <td style="text-align:right;"> 2.6200000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.121 </td>
   <td style="text-align:right;"> 0.054 </td>
   <td style="text-align:right;"> 6.8100000 </td>
   <td style="text-align:right;"> 2.6200000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 7.1300000 </td>
   <td style="text-align:right;"> 2.6200000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.281 </td>
   <td style="text-align:right;"> 0.055 </td>
   <td style="text-align:right;"> 6.8400000 </td>
   <td style="text-align:right;"> 3.6700000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 5.8200000 </td>
   <td style="text-align:right;"> 3.5000000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.043 </td>
   <td style="text-align:right;"> 0.054 </td>
   <td style="text-align:right;"> 6.1400000 </td>
   <td style="text-align:right;"> 2.5400000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 5.7500000 </td>
   <td style="text-align:right;"> 2.6200000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.203 </td>
   <td style="text-align:right;"> 0.055 </td>
   <td style="text-align:right;"> 7.3400000 </td>
   <td style="text-align:right;"> 2.5400000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 7.1300000 </td>
   <td style="text-align:right;"> 2.6200000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.074 </td>
   <td style="text-align:right;"> 0.054 </td>
   <td style="text-align:right;"> 7.1100000 </td>
   <td style="text-align:right;"> 3.5000000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 5.8200000 </td>
   <td style="text-align:right;"> 3.5000000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.150 </td>
   <td style="text-align:right;"> 0.055 </td>
   <td style="text-align:right;"> 6.1400000 </td>
   <td style="text-align:right;"> 2.5400000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 6.0300000 </td>
   <td style="text-align:right;"> 2.5400000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.081 </td>
   <td style="text-align:right;"> 0.054 </td>
   <td style="text-align:right;"> 7.3400000 </td>
   <td style="text-align:right;"> 2.5400000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 6.8100000 </td>
   <td style="text-align:right;"> 2.6200000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.361 </td>
   <td style="text-align:right;"> 0.394 </td>
   <td style="text-align:right;"> 7.1100000 </td>
   <td style="text-align:right;"> 3.5000000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 6.8400000 </td>
   <td style="text-align:right;"> 3.6700000 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.073 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 1.5000000 </td>
   <td style="text-align:right;"> 2.6800000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 1.7100000 </td>
   <td style="text-align:right;"> 3.0500000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.035 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 1.4600000 </td>
   <td style="text-align:right;"> 2.9600000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 1.3600000 </td>
   <td style="text-align:right;"> 2.7800000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.164 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 2.1100000 </td>
   <td style="text-align:right;"> 3.3300000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 1.6200000 </td>
   <td style="text-align:right;"> 2.5900000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.022 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 2.6900000 </td>
   <td style="text-align:right;"> 4.3500000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 2.7900000 </td>
   <td style="text-align:right;"> 4.8100000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.013 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 1.6700000 </td>
   <td style="text-align:right;"> 2.9600000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 1.7100000 </td>
   <td style="text-align:right;"> 3.0500000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.025 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 1.4300000 </td>
   <td style="text-align:right;"> 2.7800000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 1.3600000 </td>
   <td style="text-align:right;"> 2.7800000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.029 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 2.2100000 </td>
   <td style="text-align:right;"> 3.5200000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 1.6200000 </td>
   <td style="text-align:right;"> 2.5900000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.068 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 2.4100000 </td>
   <td style="text-align:right;"> 3.8900000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 2.7900000 </td>
   <td style="text-align:right;"> 4.8100000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.060 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 1.6700000 </td>
   <td style="text-align:right;"> 2.9600000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 1.5000000 </td>
   <td style="text-align:right;"> 2.6800000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 1.4300000 </td>
   <td style="text-align:right;"> 2.7800000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 1.4600000 </td>
   <td style="text-align:right;"> 2.9600000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 288 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.190 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 2.2100000 </td>
   <td style="text-align:right;"> 3.5200000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 2.1100000 </td>
   <td style="text-align:right;"> 3.3300000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Hangartner, S., S. H. Sbilordo, _. Michalczyk, M. J. G. Gage and O. Y. Martin </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.087 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 2.4100000 </td>
   <td style="text-align:right;"> 3.8900000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 2.6900000 </td>
   <td style="text-align:right;"> 4.3500000 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 3.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> Holland, B. </td>
   <td style="text-align:right;"> 2002 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.500 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 89 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.116 </td>
   <td style="text-align:right;"> 0.015 </td>
   <td style="text-align:right;"> 11.5900000 </td>
   <td style="text-align:right;"> 10.1800000 </td>
   <td style="text-align:right;"> 133 </td>
   <td style="text-align:right;"> 10.6600000 </td>
   <td style="text-align:right;"> 4.9500000 </td>
   <td style="text-align:right;"> 133 </td>
   <td style="text-align:right;"> 3.516 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> Holland, B. </td>
   <td style="text-align:right;"> 2002 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.500 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 89 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.070 </td>
   <td style="text-align:right;"> 0.015 </td>
   <td style="text-align:right;"> 14.4300000 </td>
   <td style="text-align:right;"> 3.2800000 </td>
   <td style="text-align:right;"> 133 </td>
   <td style="text-align:right;"> 14.8100000 </td>
   <td style="text-align:right;"> 6.9300000 </td>
   <td style="text-align:right;"> 133 </td>
   <td style="text-align:right;"> 3.516 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> Holland, B. and W. R. Rice </td>
   <td style="text-align:right;"> 1999 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.305 </td>
   <td style="text-align:right;"> 0.018 </td>
   <td style="text-align:right;"> 11.2400000 </td>
   <td style="text-align:right;"> 10.6600000 </td>
   <td style="text-align:right;"> 114 </td>
   <td style="text-align:right;"> 8.9300000 </td>
   <td style="text-align:right;"> 3.6000000 </td>
   <td style="text-align:right;"> 114 </td>
   <td style="text-align:right;"> 10.260 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B., J. L. Fierst and D. Houle </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Mutant Frequency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.807 </td>
   <td style="text-align:right;"> 0.053 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B., J. L. Fierst and D. Houle </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Mutant Frequency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.237 </td>
   <td style="text-align:right;"> 0.049 </td>
   <td style="text-align:right;"> 0.9410000 </td>
   <td style="text-align:right;"> 1.7760000 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 0.3990000 </td>
   <td style="text-align:right;"> 2.6590000 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 33 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B. and D. Houle </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.304 </td>
   <td style="text-align:right;"> 0.011 </td>
   <td style="text-align:right;"> 126.5900000 </td>
   <td style="text-align:right;"> 28.9794410 </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:right;"> 117.6000000 </td>
   <td style="text-align:right;"> 29.1136051 </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:right;"> 3.276 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 33 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B. and D. Houle </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 164 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.031 </td>
   <td style="text-align:right;"> 0.008 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.276 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 33 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B. and D. Houle </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 164 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.064 </td>
   <td style="text-align:right;"> 0.008 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.276 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 34 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B. and T. J. Kawecki </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:left;"> Mating Latency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.038 </td>
   <td style="text-align:right;"> 0.062 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 34 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B. and T. J. Kawecki </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 90 </td>
   <td style="text-align:left;"> Mating Latency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.194 </td>
   <td style="text-align:right;"> 0.043 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 34 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B. and T. J. Kawecki </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.216 </td>
   <td style="text-align:right;"> 0.091 </td>
   <td style="text-align:right;"> 0.6010000 </td>
   <td style="text-align:right;"> 0.2950000 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 0.8760000 </td>
   <td style="text-align:right;"> 0.1380000 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 5.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 34 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B. and T. J. Kawecki </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.659 </td>
   <td style="text-align:right;"> 0.066 </td>
   <td style="text-align:right;"> 0.5530000 </td>
   <td style="text-align:right;"> 0.3660000 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 0.7710000 </td>
   <td style="text-align:right;"> 0.2860000 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 5.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 34 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B. and T. J. Kawecki </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.830 </td>
   <td style="text-align:right;"> 0.090 </td>
   <td style="text-align:right;"> 0.6100000 </td>
   <td style="text-align:right;"> 0.3400000 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 0.8530000 </td>
   <td style="text-align:right;"> 0.2300000 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 5.051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B., L. Keller and T. J. Kawecki </td>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 139 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:left;"> Development Rate </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.482 </td>
   <td style="text-align:right;"> 0.028 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.201 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B., L. Keller and T. J. Kawecki </td>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 139 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:left;"> Development Rate </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.414 </td>
   <td style="text-align:right;"> 0.028 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.201 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B., L. Keller and T. J. Kawecki </td>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 162 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 0.022 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.201 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B., L. Keller and T. J. Kawecki </td>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 162 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.238 </td>
   <td style="text-align:right;"> 0.022 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.201 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B., L. Keller and T. J. Kawecki </td>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 117 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 44 </td>
   <td style="text-align:left;"> Fitness Senescence </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.500 </td>
   <td style="text-align:right;"> 0.031 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.201 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Hollis, B., L. Keller and T. J. Kawecki </td>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 117 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> Fitness Senescence </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.017 </td>
   <td style="text-align:right;"> 0.030 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.201 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 36 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> Immonen, E., R. R. Snook and M. G. Ritchie </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila pseudoobscura </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.500 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 7.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.636 </td>
   <td style="text-align:right;"> 0.046 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 2.320 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Innocenti, P., I. Flis and E. H. Morrow </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 96.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 110 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.306 </td>
   <td style="text-align:right;"> 0.120 </td>
   <td style="text-align:right;"> 780.1295325 </td>
   <td style="text-align:right;"> 24.5249313 </td>
   <td style="text-align:right;"> 169 </td>
   <td style="text-align:right;"> 773.1405125 </td>
   <td style="text-align:right;"> 20.8806168 </td>
   <td style="text-align:right;"> 160 </td>
   <td style="text-align:right;"> 3.368 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Innocenti, P., I. Flis and E. H. Morrow </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 96.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 107 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.290 </td>
   <td style="text-align:right;"> 0.013 </td>
   <td style="text-align:right;"> 879.0553188 </td>
   <td style="text-align:right;"> 25.2349182 </td>
   <td style="text-align:right;"> 160 </td>
   <td style="text-align:right;"> 870.6142500 </td>
   <td style="text-align:right;"> 32.5085570 </td>
   <td style="text-align:right;"> 160 </td>
   <td style="text-align:right;"> 3.368 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Innocenti, P., I. Flis and E. H. Morrow </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 96.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.745 </td>
   <td style="text-align:right;"> 0.053 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.368 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Innocenti, P., I. Flis and E. H. Morrow </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 96.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.490 </td>
   <td style="text-align:right;"> 0.051 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.368 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Innocenti, P., I. Flis and E. H. Morrow </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 96.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.545 </td>
   <td style="text-align:right;"> 0.051 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.368 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Innocenti, P., I. Flis and E. H. Morrow </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 96.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.379 </td>
   <td style="text-align:right;"> 0.050 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.368 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Innocenti, P., I. Flis and E. H. Morrow </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 96.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.228 </td>
   <td style="text-align:right;"> 0.049 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.368 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Innocenti, P., I. Flis and E. H. Morrow </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 96.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.300 </td>
   <td style="text-align:right;"> 0.050 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.368 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Innocenti, P., I. Flis and E. H. Morrow </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 96.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.108 </td>
   <td style="text-align:right;"> 0.049 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.368 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Innocenti, P., I. Flis and E. H. Morrow </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 96.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.080 </td>
   <td style="text-align:right;"> 0.049 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.368 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 38 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Jacomb, F., J. Marsh and L. Holman </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 320 </td>
   <td style="text-align:left;"> Pesticide Resistance </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.246 </td>
   <td style="text-align:right;"> 0.005 </td>
   <td style="text-align:right;"> 0.8560000 </td>
   <td style="text-align:right;"> 0.0210000 </td>
   <td style="text-align:right;"> 480 </td>
   <td style="text-align:right;"> 0.8920000 </td>
   <td style="text-align:right;"> 0.0350000 </td>
   <td style="text-align:right;"> 480 </td>
   <td style="text-align:right;"> 4.201 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 38 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Jacomb, F., J. Marsh and L. Holman </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 176 </td>
   <td style="text-align:left;"> Pesticide Resistance </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -1.001 </td>
   <td style="text-align:right;"> 0.005 </td>
   <td style="text-align:right;"> 0.0880000 </td>
   <td style="text-align:right;"> 0.0850000 </td>
   <td style="text-align:right;"> 480 </td>
   <td style="text-align:right;"> 0.0270000 </td>
   <td style="text-align:right;"> 0.0140000 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 4.201 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 39 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Jarzebowska, M. and J. Radwan </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.390 </td>
   <td style="text-align:right;"> 0.019 </td>
   <td style="text-align:right;"> 0.7390000 </td>
   <td style="text-align:right;"> 0.5240000 </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 0.8080000 </td>
   <td style="text-align:right;"> 0.4010000 </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:right;"> 5.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 39 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Jarzebowska, M. and J. Radwan </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.190 </td>
   <td style="text-align:right;"> 0.020 </td>
   <td style="text-align:right;"> 0.8350000 </td>
   <td style="text-align:right;"> 0.4730000 </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 0.7880000 </td>
   <td style="text-align:right;"> 0.5730000 </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:right;"> 5.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 39 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Jarzebowska, M. and J. Radwan </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> Extinction Rate </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.752 </td>
   <td style="text-align:right;"> 0.133 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 39 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Jarzebowska, M. and J. Radwan </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.150 </td>
   <td style="text-align:right;"> 0.019 </td>
   <td style="text-align:right;"> 37.9100000 </td>
   <td style="text-align:right;"> 27.9900000 </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 51.3200000 </td>
   <td style="text-align:right;"> 38.5200000 </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:right;"> 5.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 39 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Jarzebowska, M. and J. Radwan </td>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.088 </td>
   <td style="text-align:right;"> 0.019 </td>
   <td style="text-align:right;"> 70.4400000 </td>
   <td style="text-align:right;"> 32.3000000 </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 61.8700000 </td>
   <td style="text-align:right;"> 54.1700000 </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:right;"> 5.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Klemme, I. and R. C. Firman </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.769 </td>
   <td style="text-align:right;"> 0.114 </td>
   <td style="text-align:right;"> 0.2800000 </td>
   <td style="text-align:right;"> 0.4200000 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 0.7200000 </td>
   <td style="text-align:right;"> 0.6700000 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 3.068 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Klemme, I. and R. C. Firman </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Mus domesticus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.946 </td>
   <td style="text-align:right;"> 0.119 </td>
   <td style="text-align:right;"> 0.3400000 </td>
   <td style="text-align:right;"> 0.3900000 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 0.7900000 </td>
   <td style="text-align:right;"> 0.5300000 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 3.068 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 41 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Lumley, A. J., L. Michalczyk, J. J. N. Kitson, L. G. Spurgin, C. A. Morrison, J. L. Godwin, M. E. Dickinson, O. Y. Martin, B. C. Emerson, T. Chapman and M. J. G. Gage </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 9.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.576 </td>
   <td style="text-align:right;"> 0.025 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 38.138 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 41 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Lumley, A. J., L. Michalczyk, J. J. N. Kitson, L. G. Spurgin, C. A. Morrison, J. L. Godwin, M. E. Dickinson, O. Y. Martin, B. C. Emerson, T. Chapman and M. J. G. Gage </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.559 </td>
   <td style="text-align:right;"> 0.084 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 38.138 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 41 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Lumley, A. J., L. Michalczyk, J. J. N. Kitson, L. G. Spurgin, C. A. Morrison, J. L. Godwin, M. E. Dickinson, O. Y. Martin, B. C. Emerson, T. Chapman and M. J. G. Gage </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 9.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:left;"> Extinction Rate </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.522 </td>
   <td style="text-align:right;"> 0.024 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 38.138 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 41 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Lumley, A. J., L. Michalczyk, J. J. N. Kitson, L. G. Spurgin, C. A. Morrison, J. L. Godwin, M. E. Dickinson, O. Y. Martin, B. C. Emerson, T. Chapman and M. J. G. Gage </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:left;"> Extinction Rate </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.798 </td>
   <td style="text-align:right;"> 0.087 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 38.138 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 42 </td>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> Maklakov, A. A., R. Bonduriansky and R. C. Brooks </td>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 50.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Not Stated </td>
   <td style="text-align:right;"> -0.958 </td>
   <td style="text-align:right;"> 0.133 </td>
   <td style="text-align:right;"> 155.1200000 </td>
   <td style="text-align:right;"> 37.7600000 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 118.0000000 </td>
   <td style="text-align:right;"> 37.7600000 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 5.429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 43 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Martin, O. Y. and D. J. Hosken </td>
   <td style="text-align:right;"> 2003 </td>
   <td style="text-align:left;"> Sepsis cynipsea </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 25.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 50.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.841 </td>
   <td style="text-align:right;"> 0.138 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.833 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 43 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Martin, O. Y. and D. J. Hosken </td>
   <td style="text-align:right;"> 2003 </td>
   <td style="text-align:left;"> Sepsis cynipsea </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 25.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 50.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.920 </td>
   <td style="text-align:right;"> 0.140 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.833 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 43 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Martin, O. Y. and D. J. Hosken </td>
   <td style="text-align:right;"> 2003 </td>
   <td style="text-align:left;"> Sepsis cynipsea </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 25.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 50.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.038 </td>
   <td style="text-align:right;"> 0.144 </td>
   <td style="text-align:right;"> 28.2000000 </td>
   <td style="text-align:right;"> 15.4532035 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 49.2000000 </td>
   <td style="text-align:right;"> 23.1604404 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.833 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 43 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Martin, O. Y. and D. J. Hosken </td>
   <td style="text-align:right;"> 2003 </td>
   <td style="text-align:left;"> Sepsis cynipsea </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 25.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 50.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -1.314 </td>
   <td style="text-align:right;"> 0.155 </td>
   <td style="text-align:right;"> 2.2130508 </td>
   <td style="text-align:right;"> 0.0600641 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 2.1161864 </td>
   <td style="text-align:right;"> 0.0817265 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.833 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Martin, O. Y. and D. J. Hosken </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Sepsis cynipsea </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 25.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 50.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.421 </td>
   <td style="text-align:right;"> 0.159 </td>
   <td style="text-align:right;"> 34.9043478 </td>
   <td style="text-align:right;"> 21.5075526 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 42.9391304 </td>
   <td style="text-align:right;"> 14.7600851 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 3.833 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Martin, O. Y. and D. J. Hosken </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Sepsis cynipsea </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 250.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 500.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.075 </td>
   <td style="text-align:right;"> 0.155 </td>
   <td style="text-align:right;"> 34.9043478 </td>
   <td style="text-align:right;"> 21.5075526 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 33.4434783 </td>
   <td style="text-align:right;"> 15.6035186 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 3.833 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Martin, O. Y. and D. J. Hosken </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Sepsis cynipsea </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 500.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.603 </td>
   <td style="text-align:right;"> 0.163 </td>
   <td style="text-align:right;"> 42.9391304 </td>
   <td style="text-align:right;"> 14.7600851 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 33.4434783 </td>
   <td style="text-align:right;"> 15.6035186 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 3.833 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Martin, O. Y. and D. J. Hosken </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Sepsis cynipsea </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 25.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 50.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.405 </td>
   <td style="text-align:right;"> 0.082 </td>
   <td style="text-align:right;"> 17.3460898 </td>
   <td style="text-align:right;"> 2.4943223 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 16.3327787 </td>
   <td style="text-align:right;"> 2.4698682 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 3.833 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Martin, O. Y. and D. J. Hosken </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Sepsis cynipsea </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 250.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 500.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.638 </td>
   <td style="text-align:right;"> 0.085 </td>
   <td style="text-align:right;"> 17.3460898 </td>
   <td style="text-align:right;"> 2.4943223 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 15.8086522 </td>
   <td style="text-align:right;"> 2.2497809 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 3.833 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Martin, O. Y. and D. J. Hosken </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Sepsis cynipsea </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 500.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.216 </td>
   <td style="text-align:right;"> 0.081 </td>
   <td style="text-align:right;"> 16.3327787 </td>
   <td style="text-align:right;"> 2.4698682 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 15.8086522 </td>
   <td style="text-align:right;"> 2.2497809 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 3.833 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 45 </td>
   <td style="text-align:left;"> 33 </td>
   <td style="text-align:left;"> McGuigan, K., D. Petfield and M. W. Blows </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Drosophila serrata </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.405 </td>
   <td style="text-align:right;"> 3.81 </td>
   <td style="text-align:right;"> 4.81 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 292 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.034 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 0.4997000 </td>
   <td style="text-align:right;"> 0.3400000 </td>
   <td style="text-align:right;"> 146 </td>
   <td style="text-align:right;"> 0.5097000 </td>
   <td style="text-align:right;"> 0.2460000 </td>
   <td style="text-align:right;"> 146 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 45 </td>
   <td style="text-align:left;"> 33 </td>
   <td style="text-align:left;"> McGuigan, K., D. Petfield and M. W. Blows </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Drosophila serrata </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.405 </td>
   <td style="text-align:right;"> 3.81 </td>
   <td style="text-align:right;"> 4.81 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 208 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.114 </td>
   <td style="text-align:right;"> 0.019 </td>
   <td style="text-align:right;"> 49.9300000 </td>
   <td style="text-align:right;"> 22.7000000 </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 52.1740000 </td>
   <td style="text-align:right;"> 16.1000000 </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> McKean, K. A. and L. Nunney </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.700 </td>
   <td style="text-align:right;"> 2.40 </td>
   <td style="text-align:right;"> 170.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.528 </td>
   <td style="text-align:right;"> 0.242 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.737 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> McKean, K. A. and L. Nunney </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.700 </td>
   <td style="text-align:right;"> 2.40 </td>
   <td style="text-align:right;"> 170.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> Development Rate </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.853 </td>
   <td style="text-align:right;"> 0.105 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.737 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> McKean, K. A. and L. Nunney </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.700 </td>
   <td style="text-align:right;"> 2.40 </td>
   <td style="text-align:right;"> 170.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> Development Rate </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 3.124 </td>
   <td style="text-align:right;"> 0.218 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.737 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> McKean, K. A. and L. Nunney </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.700 </td>
   <td style="text-align:right;"> 2.40 </td>
   <td style="text-align:right;"> 170.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:left;"> Development Rate </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 2.655 </td>
   <td style="text-align:right;"> 0.184 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.737 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> McKean, K. A. and L. Nunney </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.700 </td>
   <td style="text-align:right;"> 2.40 </td>
   <td style="text-align:right;"> 170.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.839 </td>
   <td style="text-align:right;"> 0.081 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.737 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> McKean, K. A. and L. Nunney </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.700 </td>
   <td style="text-align:right;"> 2.40 </td>
   <td style="text-align:right;"> 170.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.598 </td>
   <td style="text-align:right;"> 0.099 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.737 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> McKean, K. A. and L. Nunney </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.700 </td>
   <td style="text-align:right;"> 2.40 </td>
   <td style="text-align:right;"> 170.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.907 </td>
   <td style="text-align:right;"> 0.110 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.737 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> McKean, K. A. and L. Nunney </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.700 </td>
   <td style="text-align:right;"> 2.40 </td>
   <td style="text-align:right;"> 170.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.911 </td>
   <td style="text-align:right;"> 0.054 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.737 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> McNamara, K. B., S. P. Robinson, M. E. Rosa, N. S. Sloan, E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 153 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.106 </td>
   <td style="text-align:right;"> 0.027 </td>
   <td style="text-align:right;"> 2.7000000 </td>
   <td style="text-align:right;"> 0.8442748 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 2.6000000 </td>
   <td style="text-align:right;"> 1.0480935 </td>
   <td style="text-align:right;"> 65 </td>
   <td style="text-align:right;"> 4.259 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> McNamara, K. B., S. P. Robinson, M. E. Rosa, N. S. Sloan, E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 145 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.157 </td>
   <td style="text-align:right;"> 0.028 </td>
   <td style="text-align:right;"> 0.1600000 </td>
   <td style="text-align:right;"> 0.0728835 </td>
   <td style="text-align:right;"> 83 </td>
   <td style="text-align:right;"> 0.1500000 </td>
   <td style="text-align:right;"> 0.0472440 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 4.259 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> McNamara, K. B., S. P. Robinson, M. E. Rosa, N. S. Sloan, E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 202 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.100 </td>
   <td style="text-align:right;"> 0.020 </td>
   <td style="text-align:right;"> 0.5700000 </td>
   <td style="text-align:right;"> 0.1014889 </td>
   <td style="text-align:right;"> 103 </td>
   <td style="text-align:right;"> 0.5800000 </td>
   <td style="text-align:right;"> 0.0994987 </td>
   <td style="text-align:right;"> 99 </td>
   <td style="text-align:right;"> 4.259 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> McNamara, K. B., S. P. Robinson, M. E. Rosa, N. S. Sloan, E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 101 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.280 </td>
   <td style="text-align:right;"> 0.039 </td>
   <td style="text-align:right;"> 0.8600000 </td>
   <td style="text-align:right;"> 0.1428286 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 0.8200000 </td>
   <td style="text-align:right;"> 0.1414214 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 4.259 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> McNamara, K. B., S. P. Robinson, M. E. Rosa, N. S. Sloan, E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 127 </td>
   <td style="text-align:left;"> Mating Duration </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.371 </td>
   <td style="text-align:right;"> 0.032 </td>
   <td style="text-align:right;"> 534.6200000 </td>
   <td style="text-align:right;"> 204.1600000 </td>
   <td style="text-align:right;"> 64 </td>
   <td style="text-align:right;"> 466.0200000 </td>
   <td style="text-align:right;"> 160.0944118 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 4.259 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> McNamara, K. B., S. P. Robinson, M. E. Rosa, N. S. Sloan, E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 127 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.156 </td>
   <td style="text-align:right;"> 0.031 </td>
   <td style="text-align:right;"> 34.2600000 </td>
   <td style="text-align:right;"> 18.7200000 </td>
   <td style="text-align:right;"> 64 </td>
   <td style="text-align:right;"> 37.1700000 </td>
   <td style="text-align:right;"> 18.2556840 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 4.259 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> McNamara, K. B., S. P. Robinson, M. E. Rosa, N. S. Sloan, E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 125 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.315 </td>
   <td style="text-align:right;"> 0.032 </td>
   <td style="text-align:right;"> 0.6700000 </td>
   <td style="text-align:right;"> 0.3149603 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 0.7700000 </td>
   <td style="text-align:right;"> 0.3174902 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 4.259 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> McNamara, K. B., E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Teleogryllus oceanicus </td>
   <td style="text-align:left;"> Cricket </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 351 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.568 </td>
   <td style="text-align:right;"> 0.012 </td>
   <td style="text-align:right;"> 0.9400000 </td>
   <td style="text-align:right;"> 0.3200000 </td>
   <td style="text-align:right;"> 179 </td>
   <td style="text-align:right;"> 1.0800000 </td>
   <td style="text-align:right;"> 0.1300000 </td>
   <td style="text-align:right;"> 172 </td>
   <td style="text-align:right;"> 3.177 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> McNamara, K. B., E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Teleogryllus oceanicus </td>
   <td style="text-align:left;"> Cricket </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 336 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 0.012 </td>
   <td style="text-align:right;"> 1.6500000 </td>
   <td style="text-align:right;"> 3.4000000 </td>
   <td style="text-align:right;"> 175 </td>
   <td style="text-align:right;"> 1.6500000 </td>
   <td style="text-align:right;"> 3.2000000 </td>
   <td style="text-align:right;"> 161 </td>
   <td style="text-align:right;"> 3.177 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> McNamara, K. B., E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Teleogryllus oceanicus </td>
   <td style="text-align:left;"> Cricket </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 413 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.050 </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> 80.2000000 </td>
   <td style="text-align:right;"> 21.8000000 </td>
   <td style="text-align:right;"> 203 </td>
   <td style="text-align:right;"> 79.0500000 </td>
   <td style="text-align:right;"> 20.3000000 </td>
   <td style="text-align:right;"> 210 </td>
   <td style="text-align:right;"> 3.177 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> McNamara, K. B., E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Teleogryllus oceanicus </td>
   <td style="text-align:left;"> Cricket </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 788 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.106 </td>
   <td style="text-align:right;"> 0.005 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 401 </td>
   <td style="text-align:right;"> 3.177 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> McNamara, K. B., E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Teleogryllus oceanicus </td>
   <td style="text-align:left;"> Cricket </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 335 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.108 </td>
   <td style="text-align:right;"> 0.012 </td>
   <td style="text-align:right;"> 0.5300000 </td>
   <td style="text-align:right;"> 0.1650000 </td>
   <td style="text-align:right;"> 173 </td>
   <td style="text-align:right;"> 0.5100000 </td>
   <td style="text-align:right;"> 0.2050000 </td>
   <td style="text-align:right;"> 162 </td>
   <td style="text-align:right;"> 3.177 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> McNamara, K. B., E. van Lieshout and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Teleogryllus oceanicus </td>
   <td style="text-align:left;"> Cricket </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 406 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.098 </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> 0.5500000 </td>
   <td style="text-align:right;"> 0.2040000 </td>
   <td style="text-align:right;"> 202 </td>
   <td style="text-align:right;"> 0.5300000 </td>
   <td style="text-align:right;"> 0.2050000 </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:right;"> 3.177 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:left;"> Mating Latency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.556 </td>
   <td style="text-align:right;"> 0.086 </td>
   <td style="text-align:right;"> 358.9000000 </td>
   <td style="text-align:right;"> 494.4000000 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 143.4000000 </td>
   <td style="text-align:right;"> 203.6000000 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:left;"> Mating Latency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.470 </td>
   <td style="text-align:right;"> 0.070 </td>
   <td style="text-align:right;"> 294.7000000 </td>
   <td style="text-align:right;"> 313.6000000 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 158.0000000 </td>
   <td style="text-align:right;"> 259.1000000 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:left;"> Mating Duration </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.987 </td>
   <td style="text-align:right;"> 0.112 </td>
   <td style="text-align:right;"> 73.5000000 </td>
   <td style="text-align:right;"> 67.7000000 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 483.4000000 </td>
   <td style="text-align:right;"> 299.5000000 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> Mating Duration </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.551 </td>
   <td style="text-align:right;"> 0.070 </td>
   <td style="text-align:right;"> 181.8000000 </td>
   <td style="text-align:right;"> 198.5000000 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 323.3000000 </td>
   <td style="text-align:right;"> 298.6000000 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:left;"> Mating Frequency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.982 </td>
   <td style="text-align:right;"> 0.112 </td>
   <td style="text-align:right;"> 2.1000000 </td>
   <td style="text-align:right;"> 2.2000000 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 22.2000000 </td>
   <td style="text-align:right;"> 15.0000000 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:left;"> Mating Frequency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.929 </td>
   <td style="text-align:right;"> 0.075 </td>
   <td style="text-align:right;"> 4.2000000 </td>
   <td style="text-align:right;"> 4.1000000 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 15.0000000 </td>
   <td style="text-align:right;"> 15.7000000 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.852 </td>
   <td style="text-align:right;"> 0.183 </td>
   <td style="text-align:right;"> 183.8000000 </td>
   <td style="text-align:right;"> 80.6000000 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 409.5000000 </td>
   <td style="text-align:right;"> 147.0000000 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.061 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 346.1000000 </td>
   <td style="text-align:right;"> 255.8000000 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 366.3000000 </td>
   <td style="text-align:right;"> 378.7000000 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.614 </td>
   <td style="text-align:right;"> 0.193 </td>
   <td style="text-align:right;"> 0.4570000 </td>
   <td style="text-align:right;"> 0.3580000 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 0.6320000 </td>
   <td style="text-align:right;"> 0.1450000 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.931 </td>
   <td style="text-align:right;"> 0.205 </td>
   <td style="text-align:right;"> 0.5290000 </td>
   <td style="text-align:right;"> 0.2500000 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 0.7200000 </td>
   <td style="text-align:right;"> 0.1210000 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.319 </td>
   <td style="text-align:right;"> 0.186 </td>
   <td style="text-align:right;"> 0.5700000 </td>
   <td style="text-align:right;"> 0.0640000 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 0.6210000 </td>
   <td style="text-align:right;"> 0.2070000 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.219 </td>
   <td style="text-align:right;"> 0.156 </td>
   <td style="text-align:right;"> 0.4530000 </td>
   <td style="text-align:right;"> 0.3920000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0.3610000 </td>
   <td style="text-align:right;"> 0.4180000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.178 </td>
   <td style="text-align:right;"> 0.156 </td>
   <td style="text-align:right;"> 0.4450000 </td>
   <td style="text-align:right;"> 0.4240000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0.5140000 </td>
   <td style="text-align:right;"> 0.3180000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.025 </td>
   <td style="text-align:right;"> 0.155 </td>
   <td style="text-align:right;"> 0.4210000 </td>
   <td style="text-align:right;"> 0.3560000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0.4300000 </td>
   <td style="text-align:right;"> 0.3340000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.110 </td>
   <td style="text-align:right;"> 0.156 </td>
   <td style="text-align:right;"> 0.7970000 </td>
   <td style="text-align:right;"> 0.3520000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0.8330000 </td>
   <td style="text-align:right;"> 0.2730000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.724 </td>
   <td style="text-align:right;"> 0.166 </td>
   <td style="text-align:right;"> 0.6940000 </td>
   <td style="text-align:right;"> 0.3350000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0.9010000 </td>
   <td style="text-align:right;"> 0.2000000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.389 </td>
   <td style="text-align:right;"> 0.159 </td>
   <td style="text-align:right;"> 0.8390000 </td>
   <td style="text-align:right;"> 0.2080000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0.7280000 </td>
   <td style="text-align:right;"> 0.3300000 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.211 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 8.3000000 </td>
   <td style="text-align:right;"> 2.5500000 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 8.9000000 </td>
   <td style="text-align:right;"> 2.9700000 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Michalczyk, L., A. L. Millard, O. Y. Martin, A. J. Lumley, B. C. Emerson and M. J. G. Gage </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Tribolium castaneum </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:right;"> 105.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.677 </td>
   <td style="text-align:right;"> 0.138 </td>
   <td style="text-align:right;"> 8.8000000 </td>
   <td style="text-align:right;"> 2.7200000 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 10.3000000 </td>
   <td style="text-align:right;"> 1.4400000 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 5.146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.657 </td>
   <td style="text-align:right;"> 0.076 </td>
   <td style="text-align:right;"> 0.2000000 </td>
   <td style="text-align:right;"> 0.1120000 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 0.2540000 </td>
   <td style="text-align:right;"> 0.1140000 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.041 </td>
   <td style="text-align:right;"> 0.069 </td>
   <td style="text-align:right;"> 0.8890000 </td>
   <td style="text-align:right;"> 0.0740000 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 0.8920000 </td>
   <td style="text-align:right;"> 0.0700000 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.211 </td>
   <td style="text-align:right;"> 0.063 </td>
   <td style="text-align:right;"> 0.8760000 </td>
   <td style="text-align:right;"> 0.1030000 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 0.9010000 </td>
   <td style="text-align:right;"> 0.1290000 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 145 </td>
   <td style="text-align:left;"> Mating Latency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.062 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 2.9400000 </td>
   <td style="text-align:right;"> 1.8800000 </td>
   <td style="text-align:right;"> 149 </td>
   <td style="text-align:right;"> 3.1200000 </td>
   <td style="text-align:right;"> 3.6900000 </td>
   <td style="text-align:right;"> 143 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 145 </td>
   <td style="text-align:left;"> Mating Duration </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.918 </td>
   <td style="text-align:right;"> 0.015 </td>
   <td style="text-align:right;"> 11.7400000 </td>
   <td style="text-align:right;"> 2.3700000 </td>
   <td style="text-align:right;"> 149 </td>
   <td style="text-align:right;"> 14.0500000 </td>
   <td style="text-align:right;"> 2.6500000 </td>
   <td style="text-align:right;"> 143 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 122 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.153 </td>
   <td style="text-align:right;"> 0.016 </td>
   <td style="text-align:right;"> 0.0771000 </td>
   <td style="text-align:right;"> 0.1630000 </td>
   <td style="text-align:right;"> 122 </td>
   <td style="text-align:right;"> 0.1023000 </td>
   <td style="text-align:right;"> 0.1650000 </td>
   <td style="text-align:right;"> 121 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.314 </td>
   <td style="text-align:right;"> 0.073 </td>
   <td style="text-align:right;"> 0.1700000 </td>
   <td style="text-align:right;"> 0.0720000 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 0.2000000 </td>
   <td style="text-align:right;"> 0.1120000 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.613 </td>
   <td style="text-align:right;"> 0.073 </td>
   <td style="text-align:right;"> 0.8350000 </td>
   <td style="text-align:right;"> 0.0980000 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 0.8890000 </td>
   <td style="text-align:right;"> 0.0740000 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.178 </td>
   <td style="text-align:right;"> 0.064 </td>
   <td style="text-align:right;"> 0.8970000 </td>
   <td style="text-align:right;"> 0.1290000 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 0.8760000 </td>
   <td style="text-align:right;"> 0.1030000 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 145 </td>
   <td style="text-align:left;"> Mating Latency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.159 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 3.5600000 </td>
   <td style="text-align:right;"> 5.2200000 </td>
   <td style="text-align:right;"> 142 </td>
   <td style="text-align:right;"> 2.9400000 </td>
   <td style="text-align:right;"> 1.8800000 </td>
   <td style="text-align:right;"> 149 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 145 </td>
   <td style="text-align:left;"> Mating Duration </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.471 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 12.8800000 </td>
   <td style="text-align:right;"> 2.4600000 </td>
   <td style="text-align:right;"> 142 </td>
   <td style="text-align:right;"> 11.7400000 </td>
   <td style="text-align:right;"> 2.3700000 </td>
   <td style="text-align:right;"> 149 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 122 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.170 </td>
   <td style="text-align:right;"> 0.016 </td>
   <td style="text-align:right;"> 0.0543000 </td>
   <td style="text-align:right;"> 0.0961000 </td>
   <td style="text-align:right;"> 122 </td>
   <td style="text-align:right;"> 0.0771000 </td>
   <td style="text-align:right;"> 0.1630000 </td>
   <td style="text-align:right;"> 122 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.868 </td>
   <td style="text-align:right;"> 0.079 </td>
   <td style="text-align:right;"> 0.1700000 </td>
   <td style="text-align:right;"> 0.0720000 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 0.2540000 </td>
   <td style="text-align:right;"> 0.1140000 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.660 </td>
   <td style="text-align:right;"> 0.073 </td>
   <td style="text-align:right;"> 0.8350000 </td>
   <td style="text-align:right;"> 0.0980000 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 0.8920000 </td>
   <td style="text-align:right;"> 0.0700000 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.031 </td>
   <td style="text-align:right;"> 0.064 </td>
   <td style="text-align:right;"> 0.8970000 </td>
   <td style="text-align:right;"> 0.1290000 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 0.9010000 </td>
   <td style="text-align:right;"> 0.1290000 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 145 </td>
   <td style="text-align:left;"> Mating Latency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.097 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 3.5600000 </td>
   <td style="text-align:right;"> 5.2200000 </td>
   <td style="text-align:right;"> 142 </td>
   <td style="text-align:right;"> 3.1200000 </td>
   <td style="text-align:right;"> 3.6900000 </td>
   <td style="text-align:right;"> 143 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 145 </td>
   <td style="text-align:left;"> Mating Duration </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.456 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 12.8800000 </td>
   <td style="text-align:right;"> 2.4600000 </td>
   <td style="text-align:right;"> 142 </td>
   <td style="text-align:right;"> 14.0500000 </td>
   <td style="text-align:right;"> 2.6500000 </td>
   <td style="text-align:right;"> 143 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 122 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.355 </td>
   <td style="text-align:right;"> 0.017 </td>
   <td style="text-align:right;"> 0.0543000 </td>
   <td style="text-align:right;"> 0.0961000 </td>
   <td style="text-align:right;"> 122 </td>
   <td style="text-align:right;"> 0.1023000 </td>
   <td style="text-align:right;"> 0.1650000 </td>
   <td style="text-align:right;"> 121 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 1440 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.088 </td>
   <td style="text-align:right;"> 0.001 </td>
   <td style="text-align:right;"> 0.8700000 </td>
   <td style="text-align:right;"> 0.3415260 </td>
   <td style="text-align:right;"> 1440 </td>
   <td style="text-align:right;"> 0.9000000 </td>
   <td style="text-align:right;"> 0.3415260 </td>
   <td style="text-align:right;"> 1440 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 1440 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.088 </td>
   <td style="text-align:right;"> 0.001 </td>
   <td style="text-align:right;"> 0.9000000 </td>
   <td style="text-align:right;"> 0.3415260 </td>
   <td style="text-align:right;"> 1440 </td>
   <td style="text-align:right;"> 0.8700000 </td>
   <td style="text-align:right;"> 0.3415260 </td>
   <td style="text-align:right;"> 1440 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., P. Chakraborty, V. Gupta, S. Z. Ali and N. G. Prasad </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 1440 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 0.001 </td>
   <td style="text-align:right;"> 0.9000000 </td>
   <td style="text-align:right;"> 0.3415260 </td>
   <td style="text-align:right;"> 1440 </td>
   <td style="text-align:right;"> 0.9000000 </td>
   <td style="text-align:right;"> 0.3415260 </td>
   <td style="text-align:right;"> 1440 </td>
   <td style="text-align:right;"> 4.659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.089 </td>
   <td style="text-align:right;"> 0.072 </td>
   <td style="text-align:right;"> 0.2826667 </td>
   <td style="text-align:right;"> 0.0124900 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 0.2822222 </td>
   <td style="text-align:right;"> 0.0101274 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.981 </td>
   <td style="text-align:right;"> 0.081 </td>
   <td style="text-align:right;"> 0.2943519 </td>
   <td style="text-align:right;"> 0.0103532 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 0.2826667 </td>
   <td style="text-align:right;"> 0.0124900 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -1.183 </td>
   <td style="text-align:right;"> 0.085 </td>
   <td style="text-align:right;"> 0.2943519 </td>
   <td style="text-align:right;"> 0.0103532 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 0.2822222 </td>
   <td style="text-align:right;"> 0.0101274 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> Mating Frequency </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.090 </td>
   <td style="text-align:right;"> 0.065 </td>
   <td style="text-align:right;"> 6.7439524 </td>
   <td style="text-align:right;"> 3.1492291 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 7.0200794 </td>
   <td style="text-align:right;"> 2.9816484 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> Mating Frequency </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.284 </td>
   <td style="text-align:right;"> 0.066 </td>
   <td style="text-align:right;"> 7.6940238 </td>
   <td style="text-align:right;"> 3.4353720 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 6.7439524 </td>
   <td style="text-align:right;"> 3.1492291 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> Mating Frequency </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.205 </td>
   <td style="text-align:right;"> 0.065 </td>
   <td style="text-align:right;"> 7.6940238 </td>
   <td style="text-align:right;"> 3.4353720 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 7.0200794 </td>
   <td style="text-align:right;"> 2.9816484 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.185 </td>
   <td style="text-align:right;"> 0.067 </td>
   <td style="text-align:right;"> 50.3663793 </td>
   <td style="text-align:right;"> 7.6774347 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 51.5985906 </td>
   <td style="text-align:right;"> 5.1763110 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.890 </td>
   <td style="text-align:right;"> 0.075 </td>
   <td style="text-align:right;"> 56.1414116 </td>
   <td style="text-align:right;"> 4.7002918 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 50.3663793 </td>
   <td style="text-align:right;"> 7.6774347 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.905 </td>
   <td style="text-align:right;"> 0.075 </td>
   <td style="text-align:right;"> 56.1414116 </td>
   <td style="text-align:right;"> 4.7002918 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 51.5985906 </td>
   <td style="text-align:right;"> 5.1763110 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.771 </td>
   <td style="text-align:right;"> 0.076 </td>
   <td style="text-align:right;"> 47.9508929 </td>
   <td style="text-align:right;"> 7.0792749 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 52.6177249 </td>
   <td style="text-align:right;"> 4.5419731 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.168 </td>
   <td style="text-align:right;"> 0.067 </td>
   <td style="text-align:right;"> 42.6208333 </td>
   <td style="text-align:right;"> 8.3991535 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 47.9508929 </td>
   <td style="text-align:right;"> 7.0792749 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.439 </td>
   <td style="text-align:right;"> 0.087 </td>
   <td style="text-align:right;"> 42.6208333 </td>
   <td style="text-align:right;"> 8.3991535 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 52.6177249 </td>
   <td style="text-align:right;"> 4.5419731 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.315 </td>
   <td style="text-align:right;"> 0.081 </td>
   <td style="text-align:right;"> 33.4558333 </td>
   <td style="text-align:right;"> 4.1586802 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 38.8756979 </td>
   <td style="text-align:right;"> 3.9717452 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.189 </td>
   <td style="text-align:right;"> 0.074 </td>
   <td style="text-align:right;"> 33.2781463 </td>
   <td style="text-align:right;"> 4.5549330 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 33.4558333 </td>
   <td style="text-align:right;"> 4.1586802 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.041 </td>
   <td style="text-align:right;"> 0.067 </td>
   <td style="text-align:right;"> 33.2781463 </td>
   <td style="text-align:right;"> 4.5549330 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 38.8756979 </td>
   <td style="text-align:right;"> 3.9717452 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.669 </td>
   <td style="text-align:right;"> 0.078 </td>
   <td style="text-align:right;"> 56.2509143 </td>
   <td style="text-align:right;"> 5.8375189 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 57.2156463 </td>
   <td style="text-align:right;"> 4.2069037 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.295 </td>
   <td style="text-align:right;"> 0.083 </td>
   <td style="text-align:right;"> 60.1348639 </td>
   <td style="text-align:right;"> 5.1204446 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 56.2509143 </td>
   <td style="text-align:right;"> 5.8375189 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Nandy, B., V. Gupta, N. Udaykumar, M. A. Samant, S. Sen and N. G. Prasad </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 32.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.612 </td>
   <td style="text-align:right;"> 0.073 </td>
   <td style="text-align:right;"> 60.1348639 </td>
   <td style="text-align:right;"> 5.1204446 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 57.2156463 </td>
   <td style="text-align:right;"> 4.2069037 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> Nelson, A. C., K. E. Colson, S. Harmon and W. K. Potts </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Mus musculus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 15.000 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 30.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.831 </td>
   <td style="text-align:right;"> 0.201 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.407 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> Nelson, A. C., K. E. Colson, S. Harmon and W. K. Potts </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Mus musculus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 15.000 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 30.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.831 </td>
   <td style="text-align:right;"> 0.201 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.407 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> Nelson, A. C., K. E. Colson, S. Harmon and W. K. Potts </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Mus musculus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 15.000 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 30.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> Male Attractiveness </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.999 </td>
   <td style="text-align:right;"> 0.283 </td>
   <td style="text-align:right;"> 0.3850000 </td>
   <td style="text-align:right;"> 0.1090000 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 0.6210000 </td>
   <td style="text-align:right;"> 0.1170000 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 3.407 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> Nelson, A. C., K. E. Colson, S. Harmon and W. K. Potts </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Mus musculus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 15.000 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 30.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.415 </td>
   <td style="text-align:right;"> 0.040 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.407 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> Nelson, A. C., K. E. Colson, S. Harmon and W. K. Potts </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Mus musculus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 15.000 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 30.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 200 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.118 </td>
   <td style="text-align:right;"> 0.020 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.407 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> Nelson, A. C., K. E. Colson, S. Harmon and W. K. Potts </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Mus musculus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 15.000 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 30.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.835 </td>
   <td style="text-align:right;"> 0.313 </td>
   <td style="text-align:right;"> 4.5300000 </td>
   <td style="text-align:right;"> 3.5000000 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 9.5400000 </td>
   <td style="text-align:right;"> 7.0100000 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3.407 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> Nelson, A. C., K. E. Colson, S. Harmon and W. K. Potts </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Mus musculus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 15.000 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 30.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.849 </td>
   <td style="text-align:right;"> 0.314 </td>
   <td style="text-align:right;"> 13.5000000 </td>
   <td style="text-align:right;"> 11.5700000 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 23.1800000 </td>
   <td style="text-align:right;"> 9.3500000 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3.407 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> Nelson, A. C., K. E. Colson, S. Harmon and W. K. Potts </td>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> Mus musculus </td>
   <td style="text-align:left;"> Mouse </td>
   <td style="text-align:right;"> 15.000 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 30.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.304 </td>
   <td style="text-align:right;"> 0.041 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 3.407 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 53 </td>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> Partridge, L. </td>
   <td style="text-align:right;"> 1980 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 100.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 200.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.773 </td>
   <td style="text-align:right;"> 0.103 </td>
   <td style="text-align:right;"> 48.9000000 </td>
   <td style="text-align:right;"> 2.9495762 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 51.1000000 </td>
   <td style="text-align:right;"> 2.6645825 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 53 </td>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> Partridge, L. </td>
   <td style="text-align:right;"> 1980 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 100.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 200.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.874 </td>
   <td style="text-align:right;"> 0.125 </td>
   <td style="text-align:right;"> 48.1000000 </td>
   <td style="text-align:right;"> 2.4083189 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 49.8000000 </td>
   <td style="text-align:right;"> 1.4832397 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 53 </td>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> Partridge, L. </td>
   <td style="text-align:right;"> 1980 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 100.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 200.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.707 </td>
   <td style="text-align:right;"> 0.069 </td>
   <td style="text-align:right;"> 49.4400000 </td>
   <td style="text-align:right;"> 1.4142136 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 50.4500000 </td>
   <td style="text-align:right;"> 1.4142136 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> Pelabon, C., L. K. Larsen, G. H. Bolstad, A. Viken, I. A. Fleming and G. Rosenqvist </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Poecilia reticulata </td>
   <td style="text-align:left;"> Guppy </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 171 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.080 </td>
   <td style="text-align:right;"> 0.023 </td>
   <td style="text-align:right;"> 25.0000000 </td>
   <td style="text-align:right;"> 4.3826932 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 25.3600000 </td>
   <td style="text-align:right;"> 4.5789082 </td>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:right;"> 3.232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> Pelabon, C., L. K. Larsen, G. H. Bolstad, A. Viken, I. A. Fleming and G. Rosenqvist </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Poecilia reticulata </td>
   <td style="text-align:left;"> Guppy </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 284 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.019 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 16.1800000 </td>
   <td style="text-align:right;"> 1.6099182 </td>
   <td style="text-align:right;"> 127 </td>
   <td style="text-align:right;"> 16.2100000 </td>
   <td style="text-align:right;"> 1.5982097 </td>
   <td style="text-align:right;"> 157 </td>
   <td style="text-align:right;"> 3.232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> Pelabon, C., L. K. Larsen, G. H. Bolstad, A. Viken, I. A. Fleming and G. Rosenqvist </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Poecilia reticulata </td>
   <td style="text-align:left;"> Guppy </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 284 </td>
   <td style="text-align:left;"> Male Attractiveness </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.120 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 1.5900000 </td>
   <td style="text-align:right;"> 0.8624562 </td>
   <td style="text-align:right;"> 127 </td>
   <td style="text-align:right;"> 1.7000000 </td>
   <td style="text-align:right;"> 0.9589258 </td>
   <td style="text-align:right;"> 157 </td>
   <td style="text-align:right;"> 3.232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> Pelabon, C., L. K. Larsen, G. H. Bolstad, A. Viken, I. A. Fleming and G. Rosenqvist </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Poecilia reticulata </td>
   <td style="text-align:left;"> Guppy </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 284 </td>
   <td style="text-align:left;"> Male Attractiveness </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 3.1300000 </td>
   <td style="text-align:right;"> 0.1437427 </td>
   <td style="text-align:right;"> 127 </td>
   <td style="text-align:right;"> 3.1300000 </td>
   <td style="text-align:right;"> 0.1278568 </td>
   <td style="text-align:right;"> 157 </td>
   <td style="text-align:right;"> 3.232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> Pelabon, C., L. K. Larsen, G. H. Bolstad, A. Viken, I. A. Fleming and G. Rosenqvist </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Poecilia reticulata </td>
   <td style="text-align:left;"> Guppy </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 284 </td>
   <td style="text-align:left;"> Male Attractiveness </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.193 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 0.1600000 </td>
   <td style="text-align:right;"> 0.8624562 </td>
   <td style="text-align:right;"> 127 </td>
   <td style="text-align:right;"> 0.3300000 </td>
   <td style="text-align:right;"> 0.8949974 </td>
   <td style="text-align:right;"> 157 </td>
   <td style="text-align:right;"> 3.232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> Pelabon, C., L. K. Larsen, G. H. Bolstad, A. Viken, I. A. Fleming and G. Rosenqvist </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Poecilia reticulata </td>
   <td style="text-align:left;"> Guppy </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 284 </td>
   <td style="text-align:left;"> Male Attractiveness </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.055 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 150.8900000 </td>
   <td style="text-align:right;"> 7.9058485 </td>
   <td style="text-align:right;"> 127 </td>
   <td style="text-align:right;"> 151.3400000 </td>
   <td style="text-align:right;"> 8.2900267 </td>
   <td style="text-align:right;"> 157 </td>
   <td style="text-align:right;"> 3.232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> Pelabon, C., L. K. Larsen, G. H. Bolstad, A. Viken, I. A. Fleming and G. Rosenqvist </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Poecilia reticulata </td>
   <td style="text-align:left;"> Guppy </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 174 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.277 </td>
   <td style="text-align:right;"> 0.023 </td>
   <td style="text-align:right;"> 1.5900000 </td>
   <td style="text-align:right;"> 0.7244860 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 1.3820000 </td>
   <td style="text-align:right;"> 0.7659334 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 3.232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> Pelabon, C., L. K. Larsen, G. H. Bolstad, A. Viken, I. A. Fleming and G. Rosenqvist </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Poecilia reticulata </td>
   <td style="text-align:left;"> Guppy </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 173 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.621 </td>
   <td style="text-align:right;"> 0.024 </td>
   <td style="text-align:right;"> 6.9400000 </td>
   <td style="text-align:right;"> 0.5992662 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 7.3200000 </td>
   <td style="text-align:right;"> 0.6171936 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 3.232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> Pelabon, C., L. K. Larsen, G. H. Bolstad, A. Viken, I. A. Fleming and G. Rosenqvist </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Poecilia reticulata </td>
   <td style="text-align:left;"> Guppy </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 145 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> 0.027 </td>
   <td style="text-align:right;"> 3.3200000 </td>
   <td style="text-align:right;"> 2.8195212 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 3.3500000 </td>
   <td style="text-align:right;"> 2.9698485 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 3.232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 55 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., W. D. Brown and G. T. Miller </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 228 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.973 </td>
   <td style="text-align:right;"> 0.020 </td>
   <td style="text-align:right;"> 0.8790000 </td>
   <td style="text-align:right;"> 0.0427083 </td>
   <td style="text-align:right;"> 114 </td>
   <td style="text-align:right;"> 0.9210000 </td>
   <td style="text-align:right;"> 0.0427083 </td>
   <td style="text-align:right;"> 114 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 55 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., W. D. Brown and G. T. Miller </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 234 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.763 </td>
   <td style="text-align:right;"> 0.018 </td>
   <td style="text-align:right;"> 0.8950000 </td>
   <td style="text-align:right;"> 0.0432666 </td>
   <td style="text-align:right;"> 117 </td>
   <td style="text-align:right;"> 0.9240000 </td>
   <td style="text-align:right;"> 0.0324500 </td>
   <td style="text-align:right;"> 117 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 55 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., W. D. Brown and G. T. Miller </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 230 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.363 </td>
   <td style="text-align:right;"> 0.018 </td>
   <td style="text-align:right;"> 129.1000000 </td>
   <td style="text-align:right;"> 80.4285397 </td>
   <td style="text-align:right;"> 115 </td>
   <td style="text-align:right;"> 99.0000000 </td>
   <td style="text-align:right;"> 84.7180618 </td>
   <td style="text-align:right;"> 115 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 55 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., W. D. Brown and G. T. Miller </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 236 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.246 </td>
   <td style="text-align:right;"> 0.017 </td>
   <td style="text-align:right;"> 122.0000000 </td>
   <td style="text-align:right;"> 86.9022439 </td>
   <td style="text-align:right;"> 118 </td>
   <td style="text-align:right;"> 101.2000000 </td>
   <td style="text-align:right;"> 81.4708537 </td>
   <td style="text-align:right;"> 118 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., G. T. Miller, J. Reagan and B. Holland </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 2.115 </td>
   <td style="text-align:right;"> 0.062 </td>
   <td style="text-align:right;"> 233.1300000 </td>
   <td style="text-align:right;"> 16.9400000 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 270.8300000 </td>
   <td style="text-align:right;"> 18.4100000 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., G. T. Miller, J. Reagan and B. Holland </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.346 </td>
   <td style="text-align:right;"> 0.048 </td>
   <td style="text-align:right;"> 211.6700000 </td>
   <td style="text-align:right;"> 19.8900000 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 237.1900000 </td>
   <td style="text-align:right;"> 17.6800000 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., G. T. Miller, J. Reagan and B. Holland </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 2.886 </td>
   <td style="text-align:right;"> 0.081 </td>
   <td style="text-align:right;"> 8.7307692 </td>
   <td style="text-align:right;"> 1.5410000 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 13.7564103 </td>
   <td style="text-align:right;"> 1.9037490 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., G. T. Miller, J. Reagan and B. Holland </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.596 </td>
   <td style="text-align:right;"> 0.041 </td>
   <td style="text-align:right;"> 7.7820513 </td>
   <td style="text-align:right;"> 2.2663679 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 9.0897436 </td>
   <td style="text-align:right;"> 2.0850585 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., G. T. Miller, J. Reagan and B. Holland </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.069 </td>
   <td style="text-align:right;"> 0.145 </td>
   <td style="text-align:right;"> 25.5723951 </td>
   <td style="text-align:right;"> 4.4651987 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 30.4600812 </td>
   <td style="text-align:right;"> 4.4023085 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., G. T. Miller, J. Reagan and B. Holland </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.484 </td>
   <td style="text-align:right;"> 0.163 </td>
   <td style="text-align:right;"> 27.4722598 </td>
   <td style="text-align:right;"> 3.3331765 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 32.9769959 </td>
   <td style="text-align:right;"> 3.8991876 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., G. T. Miller, J. Reagan and B. Holland </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.175 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 177.4228571 </td>
   <td style="text-align:right;"> 4.2492160 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 178.1600000 </td>
   <td style="text-align:right;"> 3.9836400 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., G. T. Miller, J. Reagan and B. Holland </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -1.448 </td>
   <td style="text-align:right;"> 0.161 </td>
   <td style="text-align:right;"> 179.7885714 </td>
   <td style="text-align:right;"> 3.1869120 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 174.8857143 </td>
   <td style="text-align:right;"> 3.3860940 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., G. T. Miller, J. Reagan and B. Holland </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 178 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.015 </td>
   <td style="text-align:right;"> 0.022 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., G. T. Miller, J. Reagan and B. Holland </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.148 </td>
   <td style="text-align:right;"> 0.022 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., G. T. Miller, J. Reagan and B. Holland </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 140 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.436 </td>
   <td style="text-align:right;"> 0.029 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., G. T. Miller, J. Reagan and B. Holland </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 315 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.022 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 0.5878581 </td>
   <td style="text-align:right;"> 0.3673826 </td>
   <td style="text-align:right;"> 112 </td>
   <td style="text-align:right;"> 0.5976808 </td>
   <td style="text-align:right;"> 0.4837226 </td>
   <td style="text-align:right;"> 203 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Pitnick, S., G. T. Miller, J. Reagan and B. Holland </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 344 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.327 </td>
   <td style="text-align:right;"> 0.012 </td>
   <td style="text-align:right;"> 0.4503411 </td>
   <td style="text-align:right;"> 0.4170165 </td>
   <td style="text-align:right;"> 162 </td>
   <td style="text-align:right;"> 0.5968622 </td>
   <td style="text-align:right;"> 0.4754863 </td>
   <td style="text-align:right;"> 182 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 57 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Plesnar, A., M. Konior and J. Radwan </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.060 </td>
   <td style="text-align:right;"> 0.049 </td>
   <td style="text-align:right;"> 0.7700000 </td>
   <td style="text-align:right;"> 0.1700000 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 0.7800000 </td>
   <td style="text-align:right;"> 0.1600000 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 1.029 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 57 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Plesnar, A., M. Konior and J. Radwan </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.094 </td>
   <td style="text-align:right;"> 0.049 </td>
   <td style="text-align:right;"> 0.9500000 </td>
   <td style="text-align:right;"> 0.1100000 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 0.9400000 </td>
   <td style="text-align:right;"> 0.1000000 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 1.029 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 58 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Plesnar-Bielak, A., A. M. Skrzynecka, Z. M. Prokop and J. Radwan </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 20.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 40.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.504 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 31.0909091 </td>
   <td style="text-align:right;"> 15.1950949 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 92.8571429 </td>
   <td style="text-align:right;"> 43.4161068 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 5.683 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 58 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Plesnar-Bielak, A., A. M. Skrzynecka, Z. M. Prokop and J. Radwan </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 20.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 40.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 95 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.171 </td>
   <td style="text-align:right;"> 0.071 </td>
   <td style="text-align:right;"> 134.5000000 </td>
   <td style="text-align:right;"> 48.3000374 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 143.1428571 </td>
   <td style="text-align:right;"> 49.8409939 </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 5.683 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 58 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Plesnar-Bielak, A., A. M. Skrzynecka, Z. M. Prokop and J. Radwan </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 20.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 40.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.174 </td>
   <td style="text-align:right;"> 0.038 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.683 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 58 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Plesnar-Bielak, A., A. M. Skrzynecka, Z. M. Prokop and J. Radwan </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 20.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 40.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 117 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.526 </td>
   <td style="text-align:right;"> 0.120 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.683 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 58 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Plesnar-Bielak, A., A. M. Skrzynecka, Z. M. Prokop and J. Radwan </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 20.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 40.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> Extinction Rate </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.510 </td>
   <td style="text-align:right;"> 0.740 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 5.683 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 59 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Power, D. J. and L. Holman </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.331 </td>
   <td style="text-align:right;"> 0.148 </td>
   <td style="text-align:right;"> 741.0000000 </td>
   <td style="text-align:right;"> 154.4321210 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 948.0000000 </td>
   <td style="text-align:right;"> 147.7954668 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 2.747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 59 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Power, D. J. and L. Holman </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.339 </td>
   <td style="text-align:right;"> 0.149 </td>
   <td style="text-align:right;"> 37.0000000 </td>
   <td style="text-align:right;"> 7.6367532 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 47.4000000 </td>
   <td style="text-align:right;"> 7.4833148 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 2.747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 59 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Power, D. J. and L. Holman </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.242 </td>
   <td style="text-align:right;"> 0.128 </td>
   <td style="text-align:right;"> 602.0000000 </td>
   <td style="text-align:right;"> 143.8255193 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 752.0000000 </td>
   <td style="text-align:right;"> 84.8528137 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 2.747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 59 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Power, D. J. and L. Holman </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.240 </td>
   <td style="text-align:right;"> 0.128 </td>
   <td style="text-align:right;"> 30.1000000 </td>
   <td style="text-align:right;"> 7.2124892 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 37.6000000 </td>
   <td style="text-align:right;"> 4.2426407 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 2.747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 59 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Power, D. J. and L. Holman </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.465 </td>
   <td style="text-align:right;"> 0.154 </td>
   <td style="text-align:right;"> 765.0000000 </td>
   <td style="text-align:right;"> 156.9777054 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 978.0000000 </td>
   <td style="text-align:right;"> 118.9847049 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 2.747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 59 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Power, D. J. and L. Holman </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.428 </td>
   <td style="text-align:right;"> 0.153 </td>
   <td style="text-align:right;"> 38.3000000 </td>
   <td style="text-align:right;"> 8.0610173 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 48.9000000 </td>
   <td style="text-align:right;"> 5.9866518 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 2.747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 59 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Power, D. J. and L. Holman </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 1.017 </td>
   <td style="text-align:right;"> 0.137 </td>
   <td style="text-align:right;"> 70.4000000 </td>
   <td style="text-align:right;"> 9.7580736 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 79.1000000 </td>
   <td style="text-align:right;"> 5.9866518 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 2.747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 59 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Power, D. J. and L. Holman </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.194 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 674.0000000 </td>
   <td style="text-align:right;"> 181.5850214 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 852.0000000 </td>
   <td style="text-align:right;"> 97.5807358 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 2.747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 59 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Power, D. J. and L. Holman </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.223 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 33.7000000 </td>
   <td style="text-align:right;"> 8.9095454 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 42.6000000 </td>
   <td style="text-align:right;"> 4.6669048 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 2.747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 59 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Power, D. J. and L. Holman </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.500 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.050 </td>
   <td style="text-align:right;"> 0.122 </td>
   <td style="text-align:right;"> 73.0000000 </td>
   <td style="text-align:right;"> 7.6367532 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 79.8000000 </td>
   <td style="text-align:right;"> 4.6669048 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 2.747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Power, D. J. and L. Holman </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.160 </td>
   <td style="text-align:right;"> 0.099 </td>
   <td style="text-align:right;"> 0.6091667 </td>
   <td style="text-align:right;"> 0.1700941 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 0.5425014 </td>
   <td style="text-align:right;"> 0.1557092 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 2.747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Power, D. J. and L. Holman </td>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 2.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Blind </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.396 </td>
   <td style="text-align:right;"> 0.100 </td>
   <td style="text-align:right;"> 39.4500000 </td>
   <td style="text-align:right;"> 15.0559483 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 41.7368421 </td>
   <td style="text-align:right;"> 12.7446813 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 2.747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 61 </td>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> Promislow, D. E. L., E. A. Smith and L. Pearse </td>
   <td style="text-align:right;"> 1998 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 150 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.100 </td>
   <td style="text-align:right;"> 0.026 </td>
   <td style="text-align:right;"> -0.0125000 </td>
   <td style="text-align:right;"> 0.2600000 </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 0.0168000 </td>
   <td style="text-align:right;"> 0.3190000 </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 9.821 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 61 </td>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> Promislow, D. E. L., E. A. Smith and L. Pearse </td>
   <td style="text-align:right;"> 1998 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 150 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.449 </td>
   <td style="text-align:right;"> 0.027 </td>
   <td style="text-align:right;"> 0.0950000 </td>
   <td style="text-align:right;"> 0.1750000 </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> -0.0870000 </td>
   <td style="text-align:right;"> 0.5430000 </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 9.821 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 61 </td>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> Promislow, D. E. L., E. A. Smith and L. Pearse </td>
   <td style="text-align:right;"> 1998 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 3.000 </td>
   <td style="text-align:right;"> 5.00 </td>
   <td style="text-align:right;"> 6.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 10182 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.006 </td>
   <td style="text-align:right;"> 0.001 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 9.821 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 62 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Radwan, J. </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> 0.739 </td>
   <td style="text-align:right;"> 0.118 </td>
   <td style="text-align:right;"> 42.1100000 </td>
   <td style="text-align:right;"> 32.8700000 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 65.3900000 </td>
   <td style="text-align:right;"> 22.6900000 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 3.914 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 63 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Radwan, J., J. Unrug, K. Sigorska and K. Gawronska </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 92 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.142 </td>
   <td style="text-align:right;"> 0.043 </td>
   <td style="text-align:right;"> 112.7000000 </td>
   <td style="text-align:right;"> 25.1624442 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 108.7000000 </td>
   <td style="text-align:right;"> 30.3170150 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 2.893 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 63 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Radwan, J., J. Unrug, K. Sigorska and K. Gawronska </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.123 </td>
   <td style="text-align:right;"> 0.059 </td>
   <td style="text-align:right;"> 0.6170000 </td>
   <td style="text-align:right;"> 0.7180703 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 0.5430000 </td>
   <td style="text-align:right;"> 0.4423313 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 2.893 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 63 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Radwan, J., J. Unrug, K. Sigorska and K. Gawronska </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 106 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.106 </td>
   <td style="text-align:right;"> 0.037 </td>
   <td style="text-align:right;"> 0.7030000 </td>
   <td style="text-align:right;"> 0.1965630 </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 0.7610000 </td>
   <td style="text-align:right;"> 0.7425712 </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 2.893 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 63 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Radwan, J., J. Unrug, K. Sigorska and K. Gawronska </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 90 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.085 </td>
   <td style="text-align:right;"> 0.044 </td>
   <td style="text-align:right;"> 25.3700000 </td>
   <td style="text-align:right;"> 21.1979244 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 23.7400000 </td>
   <td style="text-align:right;"> 16.4350996 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 2.893 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 64 </td>
   <td style="text-align:left;"> 34 </td>
   <td style="text-align:left;"> Rundle, H. D., S. F. Chenoweth and M. W. Blows </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Drosophila serrata </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 55.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 110.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 552 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Stressed </td>
   <td style="text-align:right;"> -0.067 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 30.4100000 </td>
   <td style="text-align:right;"> 40.5200000 </td>
   <td style="text-align:right;"> 276 </td>
   <td style="text-align:right;"> 27.6800000 </td>
   <td style="text-align:right;"> 40.5200000 </td>
   <td style="text-align:right;"> 276 </td>
   <td style="text-align:right;"> 4.292 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 64 </td>
   <td style="text-align:left;"> 34 </td>
   <td style="text-align:left;"> Rundle, H. D., S. F. Chenoweth and M. W. Blows </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Drosophila serrata </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 55.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 110.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 552 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.028 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 19.5700000 </td>
   <td style="text-align:right;"> 23.5600000 </td>
   <td style="text-align:right;"> 276 </td>
   <td style="text-align:right;"> 18.8300000 </td>
   <td style="text-align:right;"> 28.2700000 </td>
   <td style="text-align:right;"> 276 </td>
   <td style="text-align:right;"> 4.292 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> Simmons, L. W. and F. Garcia-Gonzalez </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Onthophagus taurus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.918 </td>
   <td style="text-align:right;"> 0.049 </td>
   <td style="text-align:right;"> 2.1300000 </td>
   <td style="text-align:right;"> 0.5969925 </td>
   <td style="text-align:right;"> 44 </td>
   <td style="text-align:right;"> 2.6000000 </td>
   <td style="text-align:right;"> 0.3979950 </td>
   <td style="text-align:right;"> 44 </td>
   <td style="text-align:right;"> 4.737 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> Simmons, L. W. and F. Garcia-Gonzalez </td>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Onthophagus taurus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 10.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 20.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:left;"> Body Condition </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.727 </td>
   <td style="text-align:right;"> 0.048 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 4.737 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 67 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Tilszer, M., K. Antoszczyk, N. Sa_ek, E. Zaj__c and J. Radwan </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.205 </td>
   <td style="text-align:right;"> 0.033 </td>
   <td style="text-align:right;"> 86.7000000 </td>
   <td style="text-align:right;"> 40.2790268 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 95.5000000 </td>
   <td style="text-align:right;"> 44.9266068 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 4.292 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 67 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Tilszer, M., K. Antoszczyk, N. Sa_ek, E. Zaj__c and J. Radwan </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:left;"> Early Fecundity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.259 </td>
   <td style="text-align:right;"> 0.033 </td>
   <td style="text-align:right;"> 90.7000000 </td>
   <td style="text-align:right;"> 52.6725735 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 102.4000000 </td>
   <td style="text-align:right;"> 35.6314468 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 4.292 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 67 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Tilszer, M., K. Antoszczyk, N. Sa_ek, E. Zaj__c and J. Radwan </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 1.768 </td>
   <td style="text-align:right;"> 0.046 </td>
   <td style="text-align:right;"> 0.4310000 </td>
   <td style="text-align:right;"> 0.1006976 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 0.6170000 </td>
   <td style="text-align:right;"> 0.1084435 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 4.292 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 67 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Tilszer, M., K. Antoszczyk, N. Sa_ek, E. Zaj__c and J. Radwan </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:left;"> Mating Success </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.282 </td>
   <td style="text-align:right;"> 0.033 </td>
   <td style="text-align:right;"> 0.4760000 </td>
   <td style="text-align:right;"> 0.5654556 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 0.6340000 </td>
   <td style="text-align:right;"> 0.5499636 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 4.292 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 67 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Tilszer, M., K. Antoszczyk, N. Sa_ek, E. Zaj__c and J. Radwan </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.022 </td>
   <td style="text-align:right;"> 0.033 </td>
   <td style="text-align:right;"> 284.3000000 </td>
   <td style="text-align:right;"> 105.3451470 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 286.8000000 </td>
   <td style="text-align:right;"> 120.8370804 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 4.292 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 67 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Tilszer, M., K. Antoszczyk, N. Sa_ek, E. Zaj__c and J. Radwan </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.123 </td>
   <td style="text-align:right;"> 0.033 </td>
   <td style="text-align:right;"> 278.0000000 </td>
   <td style="text-align:right;"> 61.1931369 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 284.4000000 </td>
   <td style="text-align:right;"> 39.5044301 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 4.292 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 67 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Tilszer, M., K. Antoszczyk, N. Sa_ek, E. Zaj__c and J. Radwan </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.287 </td>
   <td style="text-align:right;"> 0.093 </td>
   <td style="text-align:right;"> 97.8000000 </td>
   <td style="text-align:right;"> 0.4582576 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 97.5000000 </td>
   <td style="text-align:right;"> 1.3747727 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 4.292 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 67 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> Tilszer, M., K. Antoszczyk, N. Sa_ek, E. Zaj__c and J. Radwan </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Rhizoglyphus robini </td>
   <td style="text-align:left;"> Mite </td>
   <td style="text-align:right;"> 5.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.199 </td>
   <td style="text-align:right;"> 0.092 </td>
   <td style="text-align:right;"> 97.4000000 </td>
   <td style="text-align:right;"> 0.9165151 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 96.6000000 </td>
   <td style="text-align:right;"> 5.4990908 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 4.292 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> van Lieshout, E., K. B. McNamara and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 99 </td>
   <td style="text-align:left;"> Behavioural Plasticity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.018 </td>
   <td style="text-align:right;"> 0.040 </td>
   <td style="text-align:right;"> 285.4200000 </td>
   <td style="text-align:right;"> 196.1144075 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 282.2448980 </td>
   <td style="text-align:right;"> 155.8838417 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> van Lieshout, E., K. B. McNamara and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:left;"> Behavioural Plasticity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.132 </td>
   <td style="text-align:right;"> 0.040 </td>
   <td style="text-align:right;"> 393.4166667 </td>
   <td style="text-align:right;"> 153.0849901 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 371.1800000 </td>
   <td style="text-align:right;"> 179.6165633 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> van Lieshout, E., K. B. McNamara and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 99 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.155 </td>
   <td style="text-align:right;"> 0.040 </td>
   <td style="text-align:right;"> 3.4253300 </td>
   <td style="text-align:right;"> 0.5533225 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 3.5132898 </td>
   <td style="text-align:right;"> 0.4702925 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> van Lieshout, E., K. B. McNamara and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:left;"> Body Size </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.259 </td>
   <td style="text-align:right;"> 0.041 </td>
   <td style="text-align:right;"> 4.4623021 </td>
   <td style="text-align:right;"> 0.6760828 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 4.6295060 </td>
   <td style="text-align:right;"> 0.6208700 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> van Lieshout, E., K. B. McNamara and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 99 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.116 </td>
   <td style="text-align:right;"> 0.040 </td>
   <td style="text-align:right;"> 0.2007420 </td>
   <td style="text-align:right;"> 0.0585906 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 0.2075663 </td>
   <td style="text-align:right;"> 0.0648678 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> van Lieshout, E., K. B. McNamara and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:left;"> Ejaculate Quality and Production </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.022 </td>
   <td style="text-align:right;"> 0.040 </td>
   <td style="text-align:right;"> 0.1668542 </td>
   <td style="text-align:right;"> 0.0523804 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 0.1663250 </td>
   <td style="text-align:right;"> 0.0433648 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> van Lieshout, E., K. B. McNamara and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 99 </td>
   <td style="text-align:left;"> Mating Latency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.084 </td>
   <td style="text-align:right;"> 0.040 </td>
   <td style="text-align:right;"> 49.2000000 </td>
   <td style="text-align:right;"> 73.2039365 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 44.1836735 </td>
   <td style="text-align:right;"> 40.4874844 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> van Lieshout, E., K. B. McNamara and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:left;"> Mating Latency </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.105 </td>
   <td style="text-align:right;"> 0.040 </td>
   <td style="text-align:right;"> 69.6458333 </td>
   <td style="text-align:right;"> 86.1992964 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 61.4200000 </td>
   <td style="text-align:right;"> 68.2764758 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> van Lieshout, E., K. B. McNamara and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.373 </td>
   <td style="text-align:right;"> 0.042 </td>
   <td style="text-align:right;"> 12.7920000 </td>
   <td style="text-align:right;"> 0.3350000 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 12.6780000 </td>
   <td style="text-align:right;"> 0.2580000 </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> van Lieshout, E., K. B. McNamara and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:left;"> Immunity </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.564 </td>
   <td style="text-align:right;"> 0.044 </td>
   <td style="text-align:right;"> 12.9760000 </td>
   <td style="text-align:right;"> 0.2400000 </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 12.8530000 </td>
   <td style="text-align:right;"> 0.1880000 </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> van Lieshout, E., K. B. McNamara and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 99 </td>
   <td style="text-align:left;"> Mating Duration </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.029 </td>
   <td style="text-align:right;"> 0.040 </td>
   <td style="text-align:right;"> 565.1000000 </td>
   <td style="text-align:right;"> 277.6167708 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 572.7551020 </td>
   <td style="text-align:right;"> 244.4586307 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> van Lieshout, E., K. B. McNamara and L. W. Simmons </td>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> Callosobruchus maculatus </td>
   <td style="text-align:left;"> Beetle </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 2.00 </td>
   <td style="text-align:right;"> 120.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:left;"> Mating Duration </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Ambiguous </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.354 </td>
   <td style="text-align:right;"> 0.041 </td>
   <td style="text-align:right;"> 616.3958333 </td>
   <td style="text-align:right;"> 261.4579206 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 530.8400000 </td>
   <td style="text-align:right;"> 217.4206268 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 4.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:left;"> Mating Frequency </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.236 </td>
   <td style="text-align:right;"> 0.011 </td>
   <td style="text-align:right;"> 0.3000000 </td>
   <td style="text-align:right;"> 0.5366563 </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:right;"> 0.6700000 </td>
   <td style="text-align:right;"> 2.1466253 </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Mating Frequency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.161 </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 0.0390000 </td>
   <td style="text-align:right;"> 0.0900000 </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:right;"> 0.0650000 </td>
   <td style="text-align:right;"> 0.2100000 </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:left;"> Mating Frequency </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.178 </td>
   <td style="text-align:right;"> 0.011 </td>
   <td style="text-align:right;"> 0.2300000 </td>
   <td style="text-align:right;"> 0.1341641 </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:right;"> 0.3000000 </td>
   <td style="text-align:right;"> 0.5366563 </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Mating Frequency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.077 </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 0.0530000 </td>
   <td style="text-align:right;"> 0.2400000 </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:right;"> 0.2300000 </td>
   <td style="text-align:right;"> 0.1341641 </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:left;"> Mating Frequency </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.288 </td>
   <td style="text-align:right;"> 0.011 </td>
   <td style="text-align:right;"> 0.2300000 </td>
   <td style="text-align:right;"> 0.1341641 </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:right;"> 0.6700000 </td>
   <td style="text-align:right;"> 2.1466253 </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:left;"> Mating Frequency </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> YES </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.053 </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 0.0530000 </td>
   <td style="text-align:right;"> 0.2400000 </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:right;"> 0.0650000 </td>
   <td style="text-align:right;"> 0.2100000 </td>
   <td style="text-align:right;"> 900 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.009 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 91.0000000 </td>
   <td style="text-align:right;"> 68.9050989 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 91.5000000 </td>
   <td style="text-align:right;"> 35.8880723 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.235 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 83.0000000 </td>
   <td style="text-align:right;"> 54.5498700 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 68.0000000 </td>
   <td style="text-align:right;"> 68.9050989 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.259 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 98.0000000 </td>
   <td style="text-align:right;"> 54.5498700 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 81.0000000 </td>
   <td style="text-align:right;"> 71.7761447 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.094 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 90.5000000 </td>
   <td style="text-align:right;"> 49.5255398 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 86.0000000 </td>
   <td style="text-align:right;"> 43.0656868 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.119 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 76.0000000 </td>
   <td style="text-align:right;"> 85.4136122 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 84.5000000 </td>
   <td style="text-align:right;"> 48.8077784 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.397 </td>
   <td style="text-align:right;"> 0.129 </td>
   <td style="text-align:right;"> 96.5000000 </td>
   <td style="text-align:right;"> 68.9050989 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 73.5000000 </td>
   <td style="text-align:right;"> 40.1946410 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.057 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 88.0000000 </td>
   <td style="text-align:right;"> 22.9683663 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 91.0000000 </td>
   <td style="text-align:right;"> 68.9050989 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.201 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 92.0000000 </td>
   <td style="text-align:right;"> 28.7104579 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 83.0000000 </td>
   <td style="text-align:right;"> 54.5498700 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.132 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 88.0000000 </td>
   <td style="text-align:right;"> 89.0024194 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 98.0000000 </td>
   <td style="text-align:right;"> 54.5498700 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.006 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 91.0000000 </td>
   <td style="text-align:right;"> 114.8418315 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 90.5000000 </td>
   <td style="text-align:right;"> 49.5255398 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.171 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 89.0000000 </td>
   <td style="text-align:right;"> 60.2919615 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 76.0000000 </td>
   <td style="text-align:right;"> 85.4136122 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.118 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 88.5000000 </td>
   <td style="text-align:right;"> 63.1630073 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 96.5000000 </td>
   <td style="text-align:right;"> 68.9050989 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.113 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 88.0000000 </td>
   <td style="text-align:right;"> 22.9683663 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 91.5000000 </td>
   <td style="text-align:right;"> 35.8880723 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.442 </td>
   <td style="text-align:right;"> 0.129 </td>
   <td style="text-align:right;"> 92.0000000 </td>
   <td style="text-align:right;"> 28.7104579 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 68.0000000 </td>
   <td style="text-align:right;"> 68.9050989 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.084 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 88.0000000 </td>
   <td style="text-align:right;"> 89.0024194 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 81.0000000 </td>
   <td style="text-align:right;"> 71.7761447 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.056 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 91.0000000 </td>
   <td style="text-align:right;"> 114.8418315 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 86.0000000 </td>
   <td style="text-align:right;"> 43.0656868 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.080 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 89.0000000 </td>
   <td style="text-align:right;"> 60.2919615 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 84.5000000 </td>
   <td style="text-align:right;"> 48.8077784 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Reproductive Success </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.276 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 88.5000000 </td>
   <td style="text-align:right;"> 63.1630073 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 73.5000000 </td>
   <td style="text-align:right;"> 40.1946410 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.174 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 25.5400000 </td>
   <td style="text-align:right;"> 9.6994845 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 27.2600000 </td>
   <td style="text-align:right;"> 10.0458947 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.091 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 24.0500000 </td>
   <td style="text-align:right;"> 10.3923049 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 25.0300000 </td>
   <td style="text-align:right;"> 11.0851252 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.251 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 24.9300000 </td>
   <td style="text-align:right;"> 10.3923049 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 27.5900000 </td>
   <td style="text-align:right;"> 10.7387150 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.390 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 42.3600000 </td>
   <td style="text-align:right;"> 16.8008928 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 36.3500000 </td>
   <td style="text-align:right;"> 13.8564065 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.485 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 33.3700000 </td>
   <td style="text-align:right;"> 18.5329436 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 43.3200000 </td>
   <td style="text-align:right;"> 22.3434554 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.227 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 38.4700000 </td>
   <td style="text-align:right;"> 23.2094808 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 43.8100000 </td>
   <td style="text-align:right;"> 23.7290961 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.164 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 0.8200000 </td>
   <td style="text-align:right;"> 0.4880778 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 0.9000000 </td>
   <td style="text-align:right;"> 0.4593673 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.548 </td>
   <td style="text-align:right;"> 0.131 </td>
   <td style="text-align:right;"> 0.9100000 </td>
   <td style="text-align:right;"> 0.2583941 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 0.7200000 </td>
   <td style="text-align:right;"> 0.4019464 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.298 </td>
   <td style="text-align:right;"> 0.128 </td>
   <td style="text-align:right;"> 0.8800000 </td>
   <td style="text-align:right;"> 0.2583941 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 0.7600000 </td>
   <td style="text-align:right;"> 0.4880778 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 22.2400000 </td>
   <td style="text-align:right;"> 10.0458947 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 22.3100000 </td>
   <td style="text-align:right;"> 10.2190998 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.251 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 23.9000000 </td>
   <td style="text-align:right;"> 11.9511506 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 21.1700000 </td>
   <td style="text-align:right;"> 9.6994845 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.070 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 24.2800000 </td>
   <td style="text-align:right;"> 10.7387150 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 23.5500000 </td>
   <td style="text-align:right;"> 10.2190998 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.124 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 24.2700000 </td>
   <td style="text-align:right;"> 10.2190998 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 25.5400000 </td>
   <td style="text-align:right;"> 9.6994845 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.140 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 22.7300000 </td>
   <td style="text-align:right;"> 8.8334591 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 24.0500000 </td>
   <td style="text-align:right;"> 10.3923049 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.095 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 24.0100000 </td>
   <td style="text-align:right;"> 9.1798693 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 24.9300000 </td>
   <td style="text-align:right;"> 10.3923049 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.210 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 38.2400000 </td>
   <td style="text-align:right;"> 22.3434554 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 42.3600000 </td>
   <td style="text-align:right;"> 16.8008928 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.457 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 41.2900000 </td>
   <td style="text-align:right;"> 16.1080725 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 33.3700000 </td>
   <td style="text-align:right;"> 18.5329436 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.202 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 42.9300000 </td>
   <td style="text-align:right;"> 20.6114046 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 38.4700000 </td>
   <td style="text-align:right;"> 23.2094808 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.069 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 0.8600000 </td>
   <td style="text-align:right;"> 0.6316301 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 0.8200000 </td>
   <td style="text-align:right;"> 0.4880778 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.040 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 0.9000000 </td>
   <td style="text-align:right;"> 0.2296837 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 0.9100000 </td>
   <td style="text-align:right;"> 0.2583941 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.058 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 0.9200000 </td>
   <td style="text-align:right;"> 0.9187347 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 0.8800000 </td>
   <td style="text-align:right;"> 0.2583941 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.061 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 21.6300000 </td>
   <td style="text-align:right;"> 10.0458947 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 22.2400000 </td>
   <td style="text-align:right;"> 10.0458947 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.159 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 22.1700000 </td>
   <td style="text-align:right;"> 9.6994845 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 23.9000000 </td>
   <td style="text-align:right;"> 11.9511506 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.141 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 22.7800000 </td>
   <td style="text-align:right;"> 10.5655099 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 24.2800000 </td>
   <td style="text-align:right;"> 10.7387150 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.292 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 24.2700000 </td>
   <td style="text-align:right;"> 10.2190998 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 27.2600000 </td>
   <td style="text-align:right;"> 10.0458947 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.232 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 22.7300000 </td>
   <td style="text-align:right;"> 8.8334591 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 25.0300000 </td>
   <td style="text-align:right;"> 11.0851252 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.359 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 24.0100000 </td>
   <td style="text-align:right;"> 9.1798693 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 27.5900000 </td>
   <td style="text-align:right;"> 10.7387150 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.100 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 38.2400000 </td>
   <td style="text-align:right;"> 22.3434554 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 36.3500000 </td>
   <td style="text-align:right;"> 13.8564065 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.104 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 41.2900000 </td>
   <td style="text-align:right;"> 16.1080725 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 43.3200000 </td>
   <td style="text-align:right;"> 22.3434554 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Lifespan </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Indirect </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.041 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 42.9300000 </td>
   <td style="text-align:right;"> 20.6114046 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 43.8100000 </td>
   <td style="text-align:right;"> 23.7290961 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.071 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 0.8600000 </td>
   <td style="text-align:right;"> 0.6316301 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 0.9000000 </td>
   <td style="text-align:right;"> 0.4593673 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.537 </td>
   <td style="text-align:right;"> 0.131 </td>
   <td style="text-align:right;"> 0.9000000 </td>
   <td style="text-align:right;"> 0.2296837 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 0.7200000 </td>
   <td style="text-align:right;"> 0.4019464 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.211 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 0.9200000 </td>
   <td style="text-align:right;"> 0.9187347 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 0.7600000 </td>
   <td style="text-align:right;"> 0.4880778 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.067 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 21.6300000 </td>
   <td style="text-align:right;"> 10.0458947 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 22.3100000 </td>
   <td style="text-align:right;"> 10.2190998 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> -0.103 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 22.1700000 </td>
   <td style="text-align:right;"> 9.6994845 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 21.1700000 </td>
   <td style="text-align:right;"> 9.6994845 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> Wigby, S. and T. Chapman </td>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:left;"> Drosophila melanogaster </td>
   <td style="text-align:left;"> Fly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 3.00 </td>
   <td style="text-align:right;"> 100.00 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Not Blind </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:left;"> Offspring Viability </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Direct </td>
   <td style="text-align:left;"> Unstressed </td>
   <td style="text-align:right;"> 0.074 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 22.7800000 </td>
   <td style="text-align:right;"> 10.5655099 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 23.5500000 </td>
   <td style="text-align:right;"> 10.2190998 </td>
   <td style="text-align:right;"> 300 </td>
   <td style="text-align:right;"> 3.719 </td>
  </tr>
</tbody>
</table></div>
<br></br>

**Study ID**: An ID given to the published paper the effect size is sourced from (n=65).

**Group ID**: An ID given to the research group that may have published several papers on the same species usuing the same or very similar experimental setup. [Was not use in analysis]

**Species**: The species used in the experimental evolution procedure (n = 15).

**Taxon**: The taxon to which the species belongs. One of the following: Beetle, fly, mouse, nematode, guppy, mite and cricket (taxa were selected arbitrarily based on the available data). 

**SS Strength, Ratios and SS Density's (Column 7-9)**: Various ratios of the number of males to females and the total number of individuals kept together in an experiment [Was not used in any analysis]

**Post cop and Pre cop**: Whether a study allowed Pre/Post-copulatory sexual selection (1) or not (0).

**Blinding**: A binary classification, describing whether blind protocols were used during the experiment. Papers were assumed to be not blind unless declared otherwise.

**Generations**: The number of generations that the species was subject to differing levels of sexual selection, ranging from 1 to 162. 

**Enforced Monogamy**: Whether the study had the low sexual selection treatment as enforced monogamy (YES) or not (NO). Not all studies compared enforced monogamy and SS+ treatments. Some used FB vs MB, where FB is the SS (low intensity). 

**n**: Pooled sample size of the paired treatments.

**Outcome**: The fitness related outcome that was measured, e.g. fecundity, survival, or mating success (see Table S1 for all 20 categories). We applied our own classifications rather than relying on those provided by the authors, because different papers sometimes used different names for the same trait. 

**Outcome Class**: To help guide analysis the outcomes were classed into three categories; ambiguous, indirect and direct (see Table S1).

**Sex**: A moderator variable with three levels, describing whether the effect size in question comes from a measurement of males (M), females (F), or individuals of both sexes (B).

**Ambiguous**: Is the fitness outcome ambiguous (YES) or not ambigous (NO). Ambiguous outcomes may be those that may not necessarily be directional, that is to say they may be a life history trait. 

**Environment**: In the methods of the papers included in this study it was usually stated whether additional modifications to the experimental lines were made. Briefly, this was usually a modification that made conditions more stressful such as using a novel food source or elevated mutation load, the effect sizes from these experimental lines are labelled as 'Stressed'. If it was clearly stated that there was no such modification it is labelled 'Unstressed'. However, sometimes the paper was ambiguous in what lines had added stress or the results from stressed and unstressed lines were pooled together, in this case we label it as 'Not Stated'.

**g**: Hedge's g calculated using the compute.es package.

**var.g**: The within study variance associated with the effect size, g.

**Positive Fitness**: Whether the measurment used in the study is beneficial for fitness (1) or not (0). Note that g has already been multiplied by this column. 

**mean/sd/n.low/high**: The means, standard deviation and sample size for the low or high sexual selection treatments, used to calculate lnCVR (meta-analysis of variance). Rows without these values had hedges g' derived from summary statistics (F, z, chi-square etc.).

**JIF**: Journal Impact factor at year of publication. Several impact factors were unable to be determined/found and are NA.We obtained the journal impact factor for each effect size at the time of publication using InCites Journal Citation Reports.
<br></br>

### Table of sample sizes

The number of effect sizes, publications, blind experiments, effect sizes in stressed conditions, male, female and both measures and different species used, with the number of effect sizes per taxon also reported. 

**Table S4:** Table of effect sizes included in our meta-analysis. See the text following the data table for an explanation of each column.
 <br/><br/> 
<input type=button class=hideshow></input>

```r
n.blind.ones <- (sum(prelim.data$Blind == "Blind"))
prelim.data %>% 
  summarise(
    Effect_sizes_.Totalq = n(), 
    Publications = prelim.data$Study.ID %>% unique() %>% length(),
    Blind_experiments = n.blind.ones,
    Effect_sizes_.Stressedq = (sum(prelim.data$Environment == "Stressed")),
    Effect_sizes_.Unstressedq = (sum(prelim.data$Environment == "Unstressed")),         
    Effect_sizes_.Maleq = (sum(prelim.data$Sex == "M")),
    Effect_sizes_.Femaleq = (sum(prelim.data$Sex == "F")),
    Effect_sizes_.Both_sexesq = (sum(prelim.data$Sex == "B")),
    Different_species =  prelim.data$Species %>% unique() %>% length(),
    Effect_sizes_.Beetleq = sum(Taxon == "Beetle"),
    Effect_sizes_.Flyq = sum(Taxon == "Fly"),
    Effect_sizes_.Mouseq = sum(Taxon == "Mouse"),
    Effect_sizes_.Nematodeq = sum(Taxon == "Nematode"),
    Effect_sizes_.Miteq = sum(Taxon == "Mite"),
    Effect_sizes_.Cricketq = sum(Taxon == "Cricket"),
    Effect_sizes_.Guppyq = sum(Taxon == "Guppy")) %>% melt() %>%
  mutate(variable = gsub("_", " ", variable),
         variable = gsub("[.]", "(", variable),
         variable = gsub("q", ")", variable)) %>% 
  rename_("n" = "value", " " = "variable") %>% 
  pander(split.cell = 40, split.table = Inf)
```


---------------------------------
                              n  
--------------------------- -----
   Effect sizes (Total)      459 

       Publications          65  

     Blind experiments       54  

  Effect sizes (Stressed)    94  

 Effect sizes (Unstressed)   335 

    Effect sizes (Male)      189 

   Effect sizes (Female)     219 

 Effect sizes (Both sexes)   51  

     Different species       15  

   Effect sizes (Beetle)     116 

    Effect sizes (Fly)       254 

   Effect sizes (Mouse)      40  

  Effect sizes (Nematode)     9  

    Effect sizes (Mite)      25  

  Effect sizes (Cricket)      6  

   Effect sizes (Guppy)       9  
---------------------------------
 <br/><br/> 
**Table S5:** Table of fitness outcomes included in our meta-analysis by sex.
 <br/><br/> 
<input type=button class=hideshow></input>

```r
Outcome_and_sex <- as.data.frame.matrix(table(prelim.data$Outcome, prelim.data$Sex))
colnames(Outcome_and_sex) <- cbind("Both", "Female", "Male")

Outcome_and_sex %>% pander(split.cell = 40, split.table = Inf)
```


-------------------------------------------------------------
                &nbsp;                  Both   Female   Male 
-------------------------------------- ------ -------- ------
      **Behavioural Plasticity**         0       2       0   

          **Body Condition**             0       0       1   

            **Body Size**                2       13      11  

         **Development Rate**            5       1       1   

         **Early Fecundity**             0       14      0   

 **Ejaculate Quality and Production**    0       0       23  

         **Extinction Rate**             4       0       0   

        **Fitness Senescence**           0       3       3   

             **Immunity**                5       15      15  

             **Lifespan**                0       35      3   

       **Male Attractiveness**           0       0       6   

         **Mating Duration**             0       1       9   

         **Mating Frequency**            0       6       5   

          **Mating Latency**             0       1       12  

          **Mating Success**             0       0       39  

         **Mutant Frequency**            6       0       2   

       **Offspring Viability**           15      26      15  

       **Pesticide Resistance**          2       0       0   

       **Reproductive Success**          12     102      42  

             **Strength**                0       0       2   
-------------------------------------------------------------
<br></br>

### Forest plot

Here we show the residual effect sizes for each outcome measured. The following forest plot is based on the residual effect sizes. The model is an intercept only model with standard random effects structure.

> Although I am not sure whether having the raw values would be better. In addition whether it is worth it to add in summary polygons (effect sizes) for a grouped outcome

> Luke says: if you are plotting RESIDUAL effect sizes, doesn't that mean this plots shows each effect size, relative to the grand mean effect size? Like, -0.05 means it is below average, but the effect is still positive since the grand mean is +0.1. This is probably not what we want. 

> I have modified the code below so that it plots the resid + the grand mean. But I think since model is intercept only, this might be the same thing as the raw data... not sure. Have a think, or just plot all the raw effect sizes +/- their 95% CIs, instead of the residuals.

> Also try to make it clear what positive g means (I guess positive g means higher fitness when SS is present/strong?)



```r
# Run standard random effects model
# forest.model <- rma.mv(g, var.g, 
#                        mods = ~ 1 + Sex * Environment + Taxon,
#                        random = list(~ 1 | Study.ID, 
#                                        ~ 1 | Outcome),
#                        method = "REML", 
#                        data = prelim.data)
# 
# # Obtain residuals
# resstandards <- (rstandard.rma.mv(forest.model, 
#                                    type="response"))
# 
# # Obtain grand mean effect size    <- ADDED BY LUKE
# grand.mean <- as.numeric(forest.model$b)
# 
# # Create new df with residuals replacing raw
# df.forest.model <- prelim.data                     # <- Luke says: you could just use prelim.data itself, not the residuals?
# df.forest.model$g <- resstandards$resid + grand.mean   # <- ADDED BY LUKE
# df.forest.model$sei <- resstandards$se

# Create new factor to order factors in a way where Ambig, Indirect and Direct are Grouped
prelim.data$Outcome_f = factor(prelim.data$Outcome, levels = c('Behavioural Plasticity', 'Body Size', 'Development Rate', 'Early Fecundity', 'Immunity', 'Mating Duration', 'Pesticide Resistance', 'Mutant Frequency', 'Body Condition', 'Fitness Senescence', 'Lifespan', 'Male Attractiveness', 'Mating Frequency', 'Mating Latency', 'Mating Success', 'Strength', 'Ejaculate Quality and Production', 'Extinction Rate', 'Offspring Viability', 'Reproductive Success'))

# define upper and lower bounds
prelim.data$lowerci <- prelim.data$g - 1.96*(sqrt(prelim.data$var.g))
prelim.data$upperci <- prelim.data$g + 1.96*(sqrt(prelim.data$var.g))

# Get author and year in one
short.author.names <- data.frame(long = prelim.data$Authors %>% unique(),
                                 short = prelim.data$Authors %>% unique() %>% 
  as.character() %>% 
  strsplit(split = ",") %>% 
  sapply(FUN=function(x) x[1])) %>%
  left_join(prelim.data %>% select(Authors, Year) %>% distinct(), by = c("long" = "Authors")) %>%
  mutate(suffix = "")

# Sometimes there are multiple papers with the same first author from teh same year. Name these 2010a, 2010b etc
dups <- with(short.author.names, table(short, Year)) %>% melt()
dups <- dups[dups$value > 1, ]
for(i in 1:nrow(dups)){
  short.author.names$suffix[
    short.author.names$short == dups$short[i] &
      short.author.names$Year == dups$Year[i]] <- letters[1:dups$value[i]]
}

# Add the AuthorYear column to the dataframe of effect sizes
short.author.names <- short.author.names %>%
  mutate(AuthorYear = paste(short, " ", Year, suffix, sep="")) %>%
  select(long, AuthorYear)
prelim.data <- prelim.data %>% 
  left_join(short.author.names, by = c("Authors" = "long")) 

#Generate a plot
p.meta <- prelim.data %>% 
  mutate(Sex = replace(as.character(Sex), Sex == "B", "Both"),
         Sex = replace(Sex, Sex == "M", "Male"),
         Sex = replace(Sex, Sex == "F", "Female")) %>%
  ggplot(aes(y=reorder(AuthorYear, -g), x = g, shape = Sex, color = Outcome.Class)) +
  geom_errorbarh(aes(xmin = lowerci, 
                     xmax = upperci), height = 0.1) +
  geom_point(size = 1.5) +
  scale_x_continuous(limits=c(-5, 5), name='Standardized Mean Difference (g) \n[positive values indicate sexual selection improves fitness components]') +
  scale_color_discrete(name = "Relationship\nto fitness") + 
  ylab('Reference') + 
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  facet_grid(Outcome_f~., scales= 'free', space='free')+
  theme(strip.text.y = element_text(angle = 0))
ggsave(plot = p.meta, filename = "figures/Big_forest_plot.eps", height = 25, width = 12)
p.meta
```

<img src="meta_analysis_sexual_selection_pop_fitness_files/figure-html/unnamed-chunk-9-1.png" width="960" />
<br></br>
**Figure S1:** Forest plot of raw effect sizes and their 95% confidence intervals, grouped according to environment (stressed or unstressed) and the sex of the individuals whose fitness trait was measured (male, female, or both sexes mixed together). Rows with multiple data points denote studies that provided multiple effect sizes. Positive values indicate fitness benefits of sexual selection. 

###Quantitative summary using bootstrapped means

We can obtain a bootstrap weighted mean for the entire dataset. Although this does not take into account any moderator variables it does provide a rough guide on where most of the datapoints sit: 


```r
#Run function
get.CIs.for.weighted.mean <- function(x, weights){
  total.resamples <- 10000
  num.chunks <- ceiling(length(x)*total.resamples / 10^7) # To be nice to RAM, let's keep these to ten million or less 
  if(num.chunks == 1){
    rand <- sample(length(x), length(x)*total.resamples, replace = TRUE)
    resampled.weighted.means <- data.frame(x = x[rand], w = weights[rand], replicate = rep(1:total.resamples, each = length(x))) %>% 
      group_by(replicate) %>% 
      summarise(w.mean = weighted.mean(x, w)) %>% .$w.mean
  }
  
  else{
    resampled.weighted.means <- vector(mode = "list", length = 0)
    number.of.resamples <- floor(total.resamples/num.chunks)
    final.chunk.size <- total.resamples - number.of.resamples * (num.chunks-1)
    for(i in 1:num.chunks){
      if(i == num.chunks) number.of.resamples <- final.chunk.size
      rand <- sample(length(x), length(x) * number.of.resamples, replace = TRUE)
      resampled.weighted.means[[i]] <- data.frame(x = x[rand], w = weights[rand], replicate = rep(1:number.of.resamples, each = length(x))) %>% 
        group_by(replicate) %>% 
        summarise(w.mean = weighted.mean(x, w)) %>% .$w.mean
    }
    resampled.weighted.means <- unlist(resampled.weighted.means)
  }
  
  data.frame(w.mean = median(resampled.weighted.means),
             lowerCI = quantile(resampled.weighted.means, probs = 0.025) %>% as.numeric(),
             upperCI = quantile(resampled.weighted.means, probs = 0.975) %>% as.numeric())
}

#Get weights
prelim.data$Standard.weights <- (1/prelim.data$var.g)

#Run function with weights
get.CIs.for.weighted.mean(prelim.data$g, prelim.data$Standard.weights) %>% pander()
```


-----------------------------
 w.mean    lowerCI   upperCI 
--------- --------- ---------
 0.04317   -0.0199   0.1307  
-----------------------------

>Could this be used as a line in the forest plot showing the average (or as a polygon at the bottom), or done for each outcome?

___________________

## Multilevel Meta-Analysis

> Let's just run one big model then cull from there

<!-- >The following documents how I could run individual models for each outcome or outcome.class of interest and then plot them. This may be good to add to the above forest plot (in the form of a polygon; how many studies report their estimates) but at the same time, it just adds results from different subsetted models rather than one combined one (as we use when investigating sex and environment) -->

<!-- > Nice idea, but I think the correct way to do this is to fit Outcome.class as a moderator, and then use predict() to find the average effect size within each of the 3 levels, as you have done below. I coded something below, but it's wrong: -->

<!-- ```{r} -->
<!-- model.outcome.class <- rma(g, var.g, -->
<!--                            mods = ~ Outcome.Class,  -->
<!--                            method = "REML",  -->
<!--                            intercept = TRUE,  -->
<!--                            data = prelim.data) -->

<!-- # Get the intercept (i.e. the grand mean for 'ambiguous', and the effects for the others) -->
<!-- mod.table <- do.call("cbind", model.outcome.class[names(model.outcome.class) %in% c("b", "ci.lb", "ci.ub")]) -->

<!-- data.frame(Outcome.class = c("Ambiguous", "Direct", "Indirect"), -->
<!--            mod.table) %>% -->
<!--   ggplot(aes(y = Outcome.class, x = V1))  + -->
<!--   geom_vline(xintercept = 0, linetype = 2) + -->
<!--   geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height=0) + -->
<!--   geom_point() +  -->
<!--   xlab("Effect size and 95% CIs") -->
<!-- ``` -->


<!-- There is not really enough direct outcomes to analyse by itself so hence we combine with indirect later -->
<!-- ```{r} -->
<!-- #Ambiguous outcomes -->

<!-- model.Ambiguous <- rma(g, var.g, -->
<!--                        mods = ~ 1,  -->
<!--                        method = "REML",  -->
<!--                        subset = (Outcome.Class == "Ambiguous"),  -->
<!--                        intercept = TRUE,  -->
<!--                        data = prelim.data) -->

<!-- summary(model.Ambiguous) -->
<!-- ``` -->

<!-- ```{r} -->
<!-- #Indirect outcomes -->

<!-- model.Indirect <- rma(g, var.g, mods = ~ 1, method = "REML", subset = (Outcome.Class == "Indirect"), intercept = T ,data = prelim.data) -->

<!-- summary(model.Indirect) -->
<!-- ``` -->

<!-- ```{r} -->
<!-- #Direct outcomes -->

<!-- model.Direct <- rma(g, var.g, mods = ~ 1, method = "REML", subset = (Outcome.Class == "Direct"), data = prelim.data) -->

<!-- summary(model.Direct) -->
<!-- ``` -->

<!-- Now let's combine into a data frame and plot. The following plot could then be added to the large forest plot as summary polygons or summary points with error bars.  -->

<!-- ```{r} -->
<!-- #Data frame -->
<!-- Outcome.category <- c('Ambiguous', 'Indirect', 'Direct') -->
<!-- Estimate <- c(model.Ambiguous$b, model.Indirect$b, model.Direct$b) -->
<!-- l.ci <- c(model.Ambiguous$ci.lb, model.Indirect$ci.lb, model.Direct$ci.lb) -->
<!-- u.ci <- c(model.Ambiguous$ci.ub, model.Indirect$ci.ub, model.Direct$ci.ub) -->
<!-- data.1 <- data.frame(Outcome.category, Estimate, l.ci, u.ci) -->

<!-- #plot -->

<!-- data.1 %>% ggplot(aes(x = Estimate, y= Outcome.category)) +  -->
<!--   geom_point() + -->
<!--   geom_vline(xintercept = 0, linetype = 2, colour = "grey70") +  -->
<!--   geom_errorbarh(aes(xmin = l.ci, xmax = u.ci), height = 0, size=1) + -->
<!--   ylab("Outcome Category")+ -->
<!--   xlab("Effect Size")+ -->
<!--   xlim(-.2, .5)+ -->
<!--   ggtitle('Meta-Analysis Results') -->
<!-- ``` -->

<!-- From the previous forest plot and models we see that overall sexual selection is beneficial towards population fitness. Although this is heavily modulated by the individual outcome and the outcome class. We explore heterogeneity in more depth later in this document.  -->

### Models With Many Covariates

We collected data from fitness components that were deemed ambiguous as well as unambiguous. The ambiguous outcomes are likely to add in heterogeneity to the models and not help us in answering questions of fitness effects of sexual selection. A model utilising our complete dataset with many moderator variables would thus be: 


```r
model.preliminary <- rma.mv(g, var.g, 
                         mods = ~ 1 + Sex * Environment + Taxon + Outcome.Class + Generations + Blinding, # << ----- Add big model, then cull predictors to this one
                         random = list(~ 1 | Study.ID, 
                                       ~ 1 | Outcome), 
                         method = "REML", 
                         data = prelim.data)

summary(model.preliminary)
```

```
## 
## Multivariate Meta-Analysis Model (k = 512; method: REML)
## 
##     logLik    Deviance         AIC         BIC        AICc  
## -1959.0332   3918.0663   3960.0663   4048.2770   3962.0281  
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed    factor
## sigma^2.1  0.2645  0.5143     65     no  Study.ID
## sigma^2.2  0.1240  0.3521     20     no   Outcome
## 
## Test for Residual Heterogeneity: 
## QE(df = 493) = 6182.5248, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2:19): 
## QM(df = 18) = 203.3356, p-val < .0001
## 
## Model Results:
## 
##                             estimate      se      zval    pval    ci.lb
## intrcpt                       0.0029  0.2606    0.0112  0.9910  -0.5079
## SexF                          0.1920  0.0428    4.4912  <.0001   0.1082
## SexM                          0.1776  0.0414    4.2898  <.0001   0.0965
## EnvironmentNot Stated         0.2175  0.1136    1.9139  0.0556  -0.0052
## EnvironmentStressed           0.6721  0.0560   11.9934  <.0001   0.5622
## TaxonCricket                  0.4236  0.5689    0.7447  0.4565  -0.6913
## TaxonFly                     -0.1789  0.1686   -1.0610  0.2887  -0.5093
## TaxonGuppy                   -0.1945  0.5362   -0.3627  0.7168  -1.2453
## TaxonMite                    -0.0392  0.2584   -0.1518  0.8793  -0.5456
## TaxonMouse                   -0.3095  0.2151   -1.4387  0.1502  -0.7311
## TaxonNematode                -0.1818  0.5393   -0.3371  0.7360  -1.2388
## Outcome.ClassAmbiguous       -0.0856  0.1883   -0.4547  0.6493  -0.4547
## Outcome.ClassDirect          -0.0034  0.2418   -0.0142  0.9886  -0.4774
## Generations                   0.0019  0.0016    1.1898  0.2341  -0.0012
## BlindingNot Blind             0.0287  0.2222    0.1290  0.8974  -0.4069
## SexF:EnvironmentNot Stated    0.1321  0.0922    1.4329  0.1519  -0.0486
## SexM:EnvironmentNot Stated    0.0756  0.0817    0.9254  0.3548  -0.0845
## SexF:EnvironmentStressed     -0.5414  0.0618   -8.7568  <.0001  -0.6626
## SexM:EnvironmentStressed     -0.7242  0.0642  -11.2806  <.0001  -0.8501
##                               ci.ub     
## intrcpt                      0.5137     
## SexF                         0.2759  ***
## SexM                         0.2587  ***
## EnvironmentNot Stated        0.4402    .
## EnvironmentStressed          0.7819  ***
## TaxonCricket                 1.5385     
## TaxonFly                     0.1516     
## TaxonGuppy                   0.8564     
## TaxonMite                    0.4672     
## TaxonMouse                   0.1121     
## TaxonNematode                0.8752     
## Outcome.ClassAmbiguous       0.2834     
## Outcome.ClassDirect          0.4705     
## Generations                  0.0050     
## BlindingNot Blind            0.4642     
## SexF:EnvironmentNot Stated   0.3127     
## SexM:EnvironmentNot Stated   0.2358     
## SexF:EnvironmentStressed    -0.4202  ***
## SexM:EnvironmentStressed    -0.5984  ***
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
<br></br>
**A note on random effects** Here we utilise crossed random effects of Study.ID and Outcome. Nesting Outcomes within Study.ID can also be achieved through ``random = ~ 1 | Study.ID/Outcome``. However given that the same outcome is expected to be measured in the same/very similar way between studies we used a crossed random effects design. 

From this model we can see that there are several redundant moderators: Blinding, Generations and the Outcome class (direct, indirect and ambiguous) show little effect and are not key to our research question (like sex and environment are). However, because taxa is a likely source of heterogeneity and effect size could reasonably be expected to differ between taxa, we investigate this fixed effect further...

__________________________________

### Sexual Selection Amongst Taxa

First we want to run the model using a restricted dataset where we remove effect sizes with Ambiguous outcomes (directionless or variable in their relation to fitness) or environments that were not stated whether they were stressed or unstressed (confusing and confounding). In this model we use Sex, Environment, Taxon and the interaction between sex and environment as we hypothesise that the a stressful enviornment may be of greater importance to the female sex due to 'female demographic dominance', which essentially states that female fitness is more important to the overall population demographics and that most benefits or conversely costs will accrue to female fitness components. 


```r
#Restrict the dataset for unambiguous outcomes and environments 
restricted.data <- prelim.data %>% 
  filter(Outcome.Class != "Ambiguous" & Environment != "Not Stated") %>% 
  mutate(Sex = as.character(Sex), 
         Environment = as.character(Environment), 
         Outcome.Class.2 = as.character(Outcome.Class), 
         Enforced.Monogamy = as.character(Enforced.Monogamy))

# Make sure the factors are leveled in the same order as we write our prediction function (below)
restricted.data$Environment <- restricted.data$Environment %>% factor() %>% relevel(ref="Unstressed")
restricted.data$Sex <- restricted.data$Sex %>% factor() %>% relevel(ref="M")
restricted.data$Outcome.Class <- restricted.data$Outcome.Class %>% factor() %>% relevel(ref="Indirect")
restricted.data$Taxon <- relevel(restricted.data$Taxon, ref = "Beetle") 

model.complete <- rma.mv(g, var.g, 
                         mods = ~ 1 + Sex * Environment + Taxon, # << ----- Add big model, then cull predictors to this one
                         random = list(~ 1 | Study.ID, 
                                       ~ 1 | Outcome), 
                         method = "REML", 
                         data = restricted.data)

summary(model.complete) 
```

```
## 
## Multivariate Meta-Analysis Model (k = 383; method: REML)
## 
##     logLik    Deviance         AIC         BIC        AICc  
## -1483.9916   2967.9833   2993.9833   3044.9289   2995.0000  
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed    factor
## sigma^2.1  0.2266  0.4760     56     no  Study.ID
## sigma^2.2  0.1161  0.3407     13     no   Outcome
## 
## Test for Residual Heterogeneity: 
## QE(df = 372) = 4739.8953, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2:11): 
## QM(df = 10) = 46.7148, p-val < .0001
## 
## Model Results:
## 
##                           estimate      se     zval    pval    ci.lb
## intrcpt                     0.3402  0.1700   2.0012  0.0454   0.0070
## SexB                       -0.0574  0.0594  -0.9658  0.3341  -0.1738
## SexF                        0.0593  0.0281   2.1075  0.0351   0.0041
## EnvironmentStressed        -0.0992  0.0410  -2.4194  0.0155  -0.1796
## TaxonCricket               -0.0294  0.5109  -0.0575  0.9542  -1.0307
## TaxonFly                   -0.1797  0.1687  -1.0655  0.2866  -0.5104
## TaxonGuppy                 -0.2022  0.5043  -0.4010  0.6884  -1.1906
## TaxonMite                   0.0273  0.2486   0.1097  0.9127  -0.4600
## TaxonMouse                 -0.3796  0.2360  -1.6085  0.1077  -0.8422
## SexB:EnvironmentStressed    0.1222  0.0806   1.5152  0.1297  -0.0359
## SexF:EnvironmentStressed    0.1923  0.0490   3.9262  <.0001   0.0963
##                             ci.ub     
## intrcpt                    0.6733    *
## SexB                       0.0590     
## SexF                       0.1144    *
## EnvironmentStressed       -0.0188    *
## TaxonCricket               0.9720     
## TaxonFly                   0.1509     
## TaxonGuppy                 0.7861     
## TaxonMite                  0.5145     
## TaxonMouse                 0.0830     
## SexB:EnvironmentStressed   0.2802     
## SexF:EnvironmentStressed   0.2883  ***
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The result is a model with estimates for various taxa, species, sexes and environments. To make sense of these estimates we should obtain average predictions for each moderator variable class of interest. We can do that by using a modified version version of a function used by Holman 2017. Here it alows us to cluster predictions for the different moderators of interest: Sex, environment, taxon etc. This is done by obtaining predictions using the base ``predict()`` function for the ``rma.mv()`` objects that have been previously created

```r
# function that makes predict.rma work like a normal predict() function, instead of the idiosyncratic way that it works by default.
get.predictions.complete <- function(newdata){
  B<-0; F<-0; Stressed<-0; Cricket<-0; Fly<-0; Guppy<-0; Mite<-0; Mouse<-0; interaction1<-0; interaction2<-0; interaction3<-0
  if(newdata[1] == "B") B<-1 
  if(newdata[1] == "F") F<-1 
  if(newdata[2] == "Stressed") Unstressed<-1
  if(newdata[3] == "Cricket") Cricket<-1
  if(newdata[3] == "Fly") Fly<-1
  if(newdata[3] == "Guppy") Guppy<-1
  if(newdata[3] == "Mite") Mite<-1
  if(newdata[3] == "Mouse") Mouse<-1
  if(newdata[1] == "B" & newdata[2] == "Stressed") interaction1<-1
  if(newdata[1] == "F" & newdata[2] == "Stressed") interaction2<-1

  predict(model.complete, newmods=c(B, F, Stressed, Cricket, Fly, Guppy, Mite, Mouse, interaction1=interaction1, interaction2=interaction2))
}
# Get the predictions for each combination of moderators
predictions.complete <- as.data.frame(expand.grid(Sex = c("M", "B", "F"),
                           Environment = c("Unstressed", "Stressed"),
                           Taxon = c("Beetle", "Cricket", "Fly", "Guppy", "Mite", "Mouse")))
predictions.complete <- cbind(predictions.complete, do.call("rbind", apply(predictions.complete, 1, get.predictions.complete))) %>%
  select(Sex, Environment, Taxon, pred, se, ci.lb, ci.ub) 
for(i in 4:7) predictions.complete[,i] <- unlist(predictions.complete[,i])
```


Thirdly, plot the model predictions for effect size (Hedges' g) for male, female and both sexes under both stressed and unstressed condition and faceted for each taxon. 

<!-- > I would like to know how to join a table with mean, and CI values to the forest plots I am generating. -->

<!-- > You can make tables with ggplot, and bind them to graphs using the grid and gridExtra packages. It's pretty hard! See ?gridExtra::tableGrob.  -->
<!-- > Honestly might be easier to do it manually with e.g. Inkscape or Illustrator.	 -->


```r
pd <- position_dodgev(height = .7)
Taxon.metaanlysis <- predictions.complete %>% 
    mutate(Sex = replace(as.character(Sex), Sex == "B", "Both"),
         Sex = replace(Sex, Sex == "M", "Male"),
         Sex = replace(Sex, Sex == "F", "Female")) %>%
  ggplot(aes(x = pred, y= Environment, fill = Sex)) + 
  geom_vline(xintercept = 0, linetype = 2, colour = "grey70") + 
  geom_errorbarh(aes(xmin = predictions.complete$ci.lb, 
                     xmax = predictions.complete$ci.ub,
                     color= Sex), 
                 height = 0, position = pd, show.legend = F) +
  geom_point(position = pd, size=3, shape=21, color = "grey20") + 
  facet_grid(Taxon ~.)+
  ylab("Environment \n")+
  xlab("\nModel Prediction (Hedges g)")+
  xlim(-2, 2)+
  ggtitle('Effects of Sex and Stress on \nPopulation Fitness for Each Taxon')+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")+
  guides(fill = guide_legend(reverse=T))


ggsave(plot = Taxon.metaanlysis, filename = "figures/Taxon_metaanalysis_plot.eps", height = 7.5, width = 10)
Taxon.metaanlysis
```

<img src="meta_analysis_sexual_selection_pop_fitness_files/figure-html/unnamed-chunk-14-1.png" width="960" />
<br></br>
**Figure S2:** The predictions from this model indicate some heterogeneity between taxon. However, the most apparent difference between taxa is that confidence bands increase for taxa with low sample size. As previously shown, the beetle and fly taxa are the most heavily sampled and in the above figure have the narrowest confidence bands. Importantly, the overall direction of effect does not change between taxon, although guppies and mice show near zero effect sizes. Here we see that under stressed environments, females from all taxa appear to have greater fitness increase than males or 'both'. 

____________

###Sexual Selection, Environment and Sex

Here we ask two key questions: Does sexual selection benefit populations in stressed environments more than unstressed environments? **AND** Do the benefits of sexual selection accrue more for female fitness components?

We run a three level model where the outcome is nested within a study (Study.ID). Other potential random effects include Species and Group.ID. However the estimate (variance from random effect) of these two other potential random effects tended towards zero and were dropped from the model. Additionally the model could be run with just Study.ID, but from our exploration of heterogeneity (below) we see that there is sufficient correlation (but not ICC = 1) between outcomes to include it as a random effect within the model. 


```r
#run model without taxon: same random effects as above
model.complete2 <- rma.mv(g, var.g, 
                          mods = ~ 1 + Sex * Environment, 
                          random = list(~ 1 | Study.ID, 
                                       ~ 1 | Outcome), 
                          method = "REML", 
                          data = restricted.data)
summary(model.complete2)
```

```
## 
## Multivariate Meta-Analysis Model (k = 383; method: REML)
## 
##     logLik    Deviance         AIC         BIC        AICc  
## -1490.3090   2980.6181   2996.6181   3028.0761   2997.0094  
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed    factor
## sigma^2.1  0.2173  0.4661     56     no  Study.ID
## sigma^2.2  0.1131  0.3364     13     no   Outcome
## 
## Test for Residual Heterogeneity: 
## QE(df = 377) = 4919.8185, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2:6): 
## QM(df = 5) = 43.2063, p-val < .0001
## 
## Model Results:
## 
##                           estimate      se     zval    pval    ci.lb
## intrcpt                     0.2036  0.1179   1.7266  0.0842  -0.0275
## SexB                       -0.0562  0.0592  -0.9489  0.3427  -0.1723
## SexF                        0.0589  0.0281   2.0975  0.0360   0.0039
## EnvironmentStressed        -0.1023  0.0410  -2.4985  0.0125  -0.1826
## SexB:EnvironmentStressed    0.1305  0.0804   1.6226  0.1047  -0.0271
## SexF:EnvironmentStressed    0.1950  0.0489   3.9847  <.0001   0.0991
##                             ci.ub     
## intrcpt                    0.4347    .
## SexB                       0.0599     
## SexF                       0.1140    *
## EnvironmentStressed       -0.0221    *
## SexB:EnvironmentStressed   0.2882     
## SexF:EnvironmentStressed   0.2909  ***
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



```r
#Generate predictions without taxon utilising the previously described function

get.predictions.complete2 <- function(newdata){
  B<-0; F<-0; Stressed<-0; interaction1<-0; interaction2<-0; interaction3<-0
  if(newdata[1] == "B") B<-1 
  if(newdata[1] == "F") F<-1 
  if(newdata[2] == "Stressed") Unstressed<-1
  if(newdata[1] == "B" & newdata[2] == "Stressed") interaction1<-1
  if(newdata[1] == "F" & newdata[2] == "Stressed") interaction2<-1

  predict(model.complete2, newmods=c(B, F, Stressed, interaction1=interaction1, interaction2=interaction2))
}
# Get the predictions for each combination of moderators
predictions.complete2 <- as.data.frame(expand.grid(Sex = c("M", "B", "F"),
                           Environment = c("Unstressed", "Stressed")))
predictions.complete2 <- cbind(predictions.complete2, do.call("rbind", apply(predictions.complete2, 1, get.predictions.complete2))) %>%
  select(Sex, Environment, pred, se, ci.lb, ci.ub) 
for(i in 3:6) predictions.complete2[,i] <- unlist(predictions.complete2[,i])

#And plot the results
pd <- position_dodgev(height = .5)
EnvSex.metaanalysis <- predictions.complete2 %>% 
      mutate(Sex = replace(as.character(Sex), Sex == "B", "Both"),
         Sex = replace(Sex, Sex == "M", "Male"),
         Sex = replace(Sex, Sex == "F", "Female")) %>%
  ggplot(aes(x = pred, y= Environment, fill = Sex)) + 
  geom_vline(xintercept = 0, linetype = 2, colour = "grey70") + 
  geom_errorbarh(aes(xmin = predictions.complete2$ci.lb, 
                     xmax = predictions.complete2$ci.ub,
                     color = Sex), height = 0, position = pd, show.legend = F) +
  geom_point(position = pd, size=3, shape=21, color = "grey20") + 
  ylab("Environment\n")+
  xlab("\nEffect Size (Hedges g)")+
  xlim(-.75, .75)+
  ggtitle('Effects of Sex and Stress \non Population Fitness')+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")+
  guides(fill = guide_legend(reverse=T))

ggsave(plot = EnvSex.metaanalysis, filename = "figures/Environment_Sex_metaanalysis_plot.eps", height = 7.5, width = 10)
EnvSex.metaanalysis
```

<img src="meta_analysis_sexual_selection_pop_fitness_files/figure-html/unnamed-chunk-16-1.png" width="960" />
<br></br>
**Figure S3:** Sexual selection generally increases population fitness, especially for females under stressed conditions. The benefits of sexual selection on fitness for females under stressed conditions are small-medium according to Cohen's interperetation of effect sizes. 

We see that female fitness in stressed environments is greater than the other measurements. For outcomes that were measured for both female and males we see a greater uncertainty in the estimate. It is not obviously clear why this is. The 'both' outcomes are restricted to extinction rate, offspring viability, mutant frequency and reproductive success. However, the shift from 'both' being not significant in unstressed to significant in stressed may reflect the dampening of the negative correlations (sexual antagonism).

**Comparisons of Stress vs Unstresss through anovas**

Using an anova we can test to see if a female in a stressed environment gains significantly greater fitness benefits than in an unstressed environment:


```r
anova(model.complete2, L=c(0, 0, 1, -1, 0, 0)) 
```

```
## 
## Hypothesis:                                 
## 1: SexF - EnvironmentStressed = 0
## 
## Results:
##    estimate     se   zval   pval
## 1:   0.1613 0.0414 3.8919 <.0001
## 
## Test of Hypothesis:
## QM(df = 1) = 15.1472, p-val < .0001
```


```r
anova(model.complete2, L=c(0, 1, 0, -1, 0, 0)) 
```

```
## 
## Hypothesis:                                 
## 1: SexB - EnvironmentStressed = 0
## 
## Results:
##    estimate     se   zval   pval
## 1:   0.0461 0.0693 0.6652 0.5059
## 
## Test of Hypothesis:
## QM(df = 1) = 0.4425, p-val = 0.5059
```


__________________________________
###Estimating Heterogeneity Using _I^2^_


Let's obtain a _I^2^_ statistic for the model above using the formulas presented here: http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate


There are different methods to obtain estimates of _I^2^_, they should be pretty similar though. Here we obtain an overall value of _I^2^_ that is weighted based on variance where estimates of heterogeneity are sourced from sigma^2^ of the respective models. 

```r
#This is for the model with outcome and study as crossed random effects
W <- diag(1/restricted.data$var.g)
X <- model.matrix(model.complete2)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(model.complete2$sigma2) / (sum(model.complete2$sigma2) + (model.complete2$k-model.complete2$p)/sum(diag(P)))
```

```
## [1] 94.54655
```
This is a reasonably high _I^2^_ value but is relatively common in Ecology and Evolution (Nakagawa 2017).

To investigate the sources of heterogeneity we can obtain a breakdown of the heterogeneity for the model.

```r
100 * model.complete2$sigma2 / (sum(model.complete2$sigma2) + (model.complete2$k-model.complete2$p)/sum(diag(P)))
```

```
## [1] 62.17215 32.37441
```

This indicates that 32.37 % of total heterogeneity is due to the **between study** heterogeneity and 62.17 % for **between outcome** heterogeneity between different outcomes. With the remaining 5.5 % due to sampling variance. Interestingly this might indicate that _I^2^_ would be largely reduced for a model restricted to a single outcome... Let's test this with our most common outcome...Reproductive Success


```r
#Reproductive Success Restriction 
restricted.dataRS <- restricted.data %>% filter(restricted.data$Outcome == "Reproductive Success")

#Run reproductive success only model 
model.completeRS <- rma.mv(g, var.g, mods = ~ 1 + Sex + Environment + Sex:Environment, random = ~ 1 | Study.ID, method = "REML", data = restricted.dataRS)


#Run estimate of heterogeneity
W2 <- diag(1/restricted.dataRS$var.g)
X2 <- model.matrix(model.completeRS)
P2 <- W2 - W2 %*% X2 %*% solve(t(X2) %*% W2 %*% X2) %*% t(X2) %*% W2
100 * sum(model.completeRS$sigma2) / (sum(model.completeRS$sigma2) + (model.completeRS$k-model.completeRS$p)/sum(diag(P2)))
```

```
## [1] 80.97317
```

So if we look at an individual outcomes such as reproductive success our I^2 is lower (80.97 %). Which is still high as it comes from 39 studies but lower than others. If we wanted to run models independently we could do it for those with a large enough sample size (k>10).

> Justin Says:  I am no longer sure we can use ICC as the model is not nested now, instead the random effects are crossed

Furthermore, we can obtain estimates of the intra-class correlation (ICC) within a study via: 

```r
round(model.complete2$sigma2[1] / sum(model.complete2$sigma2), 3)
```

```
## [1] 0.658
```

This means that within a study, between different outcomes, there is a correlation of 26.2 % (low-medium). This justifies including outcome as a level, as without it we would be assuming ICC = 1. We can also gain an estimate of the total heterogeniety, as the sum of the sigma componenets: 

```r
round(sum(model.complete2$sigma2), 3)
```

```
## [1] 0.33
```



<!-- #### Multilevel model using metafors alternative random effect structure  -->

<!-- > This inner, outer factor stuff from the metafor package is a bit strange. There is a description in ``?rma.mv()`` but still unsure how it differs from the above model. It seems that it is useful to breakdown variance-covariance matrix but unsure how that would benefit our analysis. -->

<!-- ```{r} -->
<!-- #Just to check, how about with outcome as the inner factor  -->
<!-- model.complete2.2 <- rma.mv(g, var.g, mods = ~ 1 + Sex + Environment + Sex:Environment, random = ~ factor(Outcome) | Study.ID, method = "REML", data = restricted.data) -->

<!-- summary(model.complete2.2) -->

<!-- #Now with a slightly different structure (HCS) -->

<!-- model.complete2.3 <- rma.mv(g, var.g, mods = ~ 1 + Sex + Environment + Sex:Environment, random = ~ Outcome | Study.ID, struct = "HCS", method = "REML", data = restricted.data) -->

<!-- summary(model.complete2.3) -->
<!-- ``` -->

<!-- ```{r, fig.height= 7, fig.width=10} -->
<!-- #Generate predictions without taxon -->

<!-- get.predictions.complete2 <- function(newdata){ -->
<!--   B<-0; F<-0; Stressed<-0; interaction1<-0; interaction2<-0; interaction3<-0 -->
<!--   if(newdata[1] == "B") B<-1  -->
<!--   if(newdata[1] == "F") F<-1  -->
<!--   if(newdata[2] == "Stressed") Unstressed<-1 -->
<!--   if(newdata[1] == "B" & newdata[2] == "Stressed") interaction1<-1 -->
<!--   if(newdata[1] == "F" & newdata[2] == "Stressed") interaction2<-1 -->

<!--   predict(model.complete2.2, newmods=c(B, F, Stressed, interaction1=interaction1, interaction2=interaction2)) -->
<!-- } -->
<!-- # Get the predictions for each combination of moderators -->
<!-- predictions.complete2 <- as.data.frame(expand.grid(Sex = c("M", "B", "F"), -->
<!--                            Environment = c("Unstressed", "Stressed"))) -->
<!-- predictions.complete2 <- cbind(predictions.complete2, do.call("rbind", apply(predictions.complete2, 1, get.predictions.complete2))) %>% -->
<!--   select(Sex, Environment, pred, se, ci.lb, ci.ub)  -->
<!-- for(i in 3:6) predictions.complete2[,i] <- unlist(predictions.complete2[,i]) -->

<!-- #And plot the results -->

<!-- pd <- position_dodgev(height = .3) -->
<!-- predictions.complete2 %>% ggplot(aes(x = pred, y= Environment, colour = Sex)) +  -->
<!--   geom_vline(xintercept = 0, linetype = 2, colour = "grey70") +  -->
<!--   geom_errorbarh(aes(xmin = predictions.complete2$ci.lb, xmax = predictions.complete2$ci.ub), height = 0, position = pd) + -->
<!--   geom_point(position = pd) +  -->
<!--   ylab("Environment")+ -->
<!--   xlab("Effect Size (Hedges g)")+ -->
<!--   xlim(-.75, .75)+ -->
<!--   ggtitle('Effects of Sex, Stress \non Population Fitness (OUTCOME = INNER FACTOR)') -->
<!-- ``` -->




____________________

## Multilevel Meta-Analysis on Variance

This meta-analysis on variation utilises previously described and utilised methods devoleped (Nakagawa et al. 2015; Senior et al. 2016). Our goal is to determine whether the phenotypic variance in fitness related traits is impacted by sexual selection. We would assume that if selection is occuring not only would the trait mean shift in a certain direction but the variance associated with those changes to the mean would also decrease. In this case we use an effect size statistic known as the natural log of the coefficient of variation ratio (lnCVR)


**Firstly**, we setup our calculation by creating a a restricted dataset with only unabmiguous fitness outcomes and running the functions developed by Nakagawa et al. 2015: 


```r
#Setup restricted data
prelim.data2 <- (prelim.data %>% filter(Outcome.Class != "Ambiguous",  Environment != "Not Stated"))

#Run function for lnCVR and associated variance of lnCVR

#for lnCVR


Calc.lnCVR<-function(CMean, CSD, CN, EMean, ESD, EN){
	
	ES<-log(ESD) - log(EMean) + 1 / (2*(EN - 1)) - (log(CSD) - log(CMean) + 1 / (2*(CN - 1)))
	
	return(ES)
	
}

#for variance of lnCVR

Calc.var.lnCVR<-function(CMean, CSD, CN, EMean, ESD, EN, Equal.E.C.Corr=T){
	
	if(Equal.E.C.Corr==T){
	
		mvcorr<-cor.test(log(c(CMean, EMean)), log(c(CSD, ESD)))$estimate
	
		S2<- CSD^2 / (CN * (CMean^2)) + 1 / (2 * (CN - 1)) - 2 * mvcorr * sqrt((CSD^2 / (CN * (CMean^2))) * (1 / (2 * (CN - 1)))) + ESD^2 / (EN * (EMean^2)) + 1 / (2 * (EN - 1)) - 2 * mvcorr * sqrt((ESD^2 / (EN * (EMean^2))) * (1 / (2 * (EN - 1))))
	
	}
	else{
		
		Cmvcorr<-cor.test(log(CMean), log(CSD))$estimate
		Emvcorr<-cor.test(log(EMean), (ESD))$estimate
	
		S2<- CSD^2 / (CN * (CMean^2)) + 1 / (2 * (CN - 1)) - 2 * Cmvcorr * sqrt((CSD^2 / (CN * (CMean^2))) * (1 / (2 * (CN - 1)))) + ESD^2 / (EN * (EMean^2)) + 1 / (2 * (EN - 1)) - 2 * Emvcorr * sqrt((ESD^2 / (EN * (EMean^2))) * (1 / (2 * (EN - 1))))		
		
		
	}
	return(S2)
	
}
```


> Justin Says: Scrap subsetting, just use exact same model as we used for Hedges' g

**Secondly**, we utilise those formulas to obtain lnCVR and var.CVR for all applicable effect sizes. Noting that not all of the dataset has means, SD and n; some were calculated from summary statistics and are not able to have lnCVR calculated:



```r
#Calculate lnCVr and var.lnCVr
#for lnCVR
prelim.data2$lnCVr <- Calc.lnCVR(prelim.data2$mean.low, prelim.data2$sd.low, prelim.data2$n.low, prelim.data2$mean.high, prelim.data2$sd.high, prelim.data2$n.high)

#for variance in lnCVR
prelim.data2$var.lnCVr <- Calc.var.lnCVR(prelim.data2$mean.low, prelim.data2$sd.low, prelim.data2$n.low, prelim.data2$mean.high, prelim.data2$sd.high, prelim.data2$n.high, Equal.E.C.Corr=F)
```

<!-- #Run simple models subsetted for each environment/sex (this is perhaps a clunky way so we also use predictions shown below) -->

<!-- # For stressed environment and females -->
<!-- varSF <- rma.mv(lnCVr, var.lnCVr, data = prelim.data2, mods = ~ 1, random = ~ 1 | Study.ID/Outcome, subset = (Environment == "Stressed" & Sex == "F")) -->

<!-- # For stressed environment and females -->
<!-- varSM <- rma.mv(lnCVr, var.lnCVr, data = prelim.data2, mods = ~ 1, random = ~ 1 | Study.ID/Outcome, subset = (Environment == "Stressed" & Sex == "M")) -->

<!-- # For stressed environment and females -->
<!-- varSB <- rma.mv(lnCVr, var.lnCVr, data = prelim.data2, mods = ~ 1, random = ~ 1 | Study.ID/Outcome, subset = (Environment == "Stressed" & Sex == "B")) -->

<!-- #For Benign environment and females -->
<!-- varUF<- rma.mv(lnCVr, var.lnCVr, data = prelim.data2, mods = ~1, random = ~ 1 | Study.ID/Outcome, subset = (Environment == "Unstressed" & Sex == "F")) -->

<!-- #For Benign environment and males -->
<!-- varUM<- rma.mv(lnCVr, var.lnCVr, data = prelim.data2, mods = ~1, random = ~ 1 | Study.ID/Outcome, subset = (Environment == "Unstressed" & Sex == "M")) -->

<!-- #For Benign environment and both -->
<!-- varUB <- rma.mv(lnCVr, var.lnCVr, data = prelim.data2, mods = ~1, random = ~ 1 | Study.ID/Outcome, subset = (Environment == "Unstressed" & Sex == "B")) -->

<!-- #Create dataframe of estimates and confidence intervals -->
<!-- lnCVR <- c(varSF$b, varSM$b, varSB$b, varUF$b, varUM$b, varUB$b) -->
<!-- l.ci <- c(varSF$ci.lb, varSM$ci.lb, varSB$ci.lb, varUF$ci.lb, varUM$ci.lb, varUB$ci.lb) -->
<!-- u.ci <- c(varSF$ci.ub, varSM$ci.ub, varSB$ci.ub, varUF$ci.ub, varUM$ci.ub, varUB$ci.ub) -->
<!-- Environment <- c("Stressed", "Stressed", "Stressed", "Unstressed", "Unstressed", "Unstressed") -->
<!-- Sex <- c("Female", "Male", "Both", "Female", "Male", "Both") -->
<!-- k <- c(varSF$k, varSM$k, varSB$k, varUF$k, varUM$k, varUB$k) -->

<!-- var.data <- data.frame(lnCVR, l.ci, u.ci, Environment, Sex, k) -->

<!-- #Releveling the factors to make sure it aligns with other formatted graphs -->
<!-- var.data$Environment <- var.data$Environment %>% factor %>% relevel(ref="Unstressed") -->
<!-- var.data$Sex <- var.data$Sex %>% factor %>% relevel(ref="Male") -->

<!-- #Plot subseted model estimates -->

<!-- var.data %>% ggplot(aes(x=lnCVR, y = Environment, colour = Sex))+ -->
<!--   geom_vline(xintercept = 0, linetype = 2, colour = "grey70") + -->
<!--   geom_errorbarh(aes(xmin = l.ci, xmax = u.ci), height = 0, position = pd) + -->
<!--   geom_point(position = pd) + -->
<!--   ylab("Environment")+ -->
<!--   xlab("lnCVR")+ -->
<!--   xlim(-.75, .75)+ -->
<!--   ggtitle('Meta-Analysis of Variance (Using Subsetting)') -->





**Thirdly**, although not previously done extensively, it seems that the best way to conduct this analysis is not through subsetting but through utilising model predictions as we did with Hedges' g previously, that way we retain the same methodology in model structure and test the same hypotheses. This can be done be utilising the same predict function but for lnCVR and var.lnCVR. 

Multilevel-model using lnCVR:


```r
#Now try with multilevel model 
variance.model <- rma.mv(lnCVr, var.lnCVr, mods = ~ 1 + Sex*Environment, 
                          random = list(~ 1 | Study.ID, 
                                       ~ 1 | Outcome), 
                         method = "REML", data = prelim.data2)
summary(variance.model)
```

```
## 
## Multivariate Meta-Analysis Model (k = 303; method: REML)
## 
##     logLik    Deviance         AIC         BIC        AICc  
## -4589.1975   9178.3950   9194.3950   9223.9448   9194.8950  
## 
## Variance Components: 
## 
##             estim    sqrt  nlvls  fixed    factor
## sigma^2.1  0.2450  0.4950     46     no  Study.ID
## sigma^2.2  0.1848  0.4299     11     no   Outcome
## 
## Test for Residual Heterogeneity: 
## QE(df = 297) = 13424.1916, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2:6): 
## QM(df = 5) = 3827.7822, p-val < .0001
## 
## Model Results:
## 
##                           estimate      se      zval    pval    ci.lb
## intrcpt                    -0.0138  0.1602   -0.0858  0.9316  -0.3278
## SexF                        0.0204  0.0503    0.4050  0.6855  -0.0783
## SexM                        0.2340  0.0484    4.8318  <.0001   0.1391
## EnvironmentStressed        -0.4672  0.0577   -8.0908  <.0001  -0.5803
## SexF:EnvironmentStressed   -0.7294  0.0616  -11.8430  <.0001  -0.8501
## SexM:EnvironmentStressed    0.2300  0.0622    3.7012  0.0002   0.1082
##                             ci.ub     
## intrcpt                    0.3003     
## SexF                       0.1190     
## SexM                       0.3289  ***
## EnvironmentStressed       -0.3540  ***
## SexF:EnvironmentStressed  -0.6087  ***
## SexM:EnvironmentStressed   0.3519  ***
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Plotted predictions of lnCVR for various moderators: 


```r
#Generate predictions
get.predictions.variance <- function(newdata){
  F<-0; M<-0; Stressed<-0; interaction1<-0; interaction2<-0; interaction3<-0
  if(newdata[1] == "F") F<-1 
  if(newdata[1] == "M") M<-1 
  if(newdata[2] == "Stressed") Unstressed<-1
  if(newdata[1] == "F" & newdata[2] == "Stressed") interaction1<-1
  if(newdata[1] == "M" & newdata[2] == "Stressed") interaction2<-1

  predict(variance.model, newmods=c(F, M, Stressed, interaction1=interaction1, interaction2=interaction2))
}
# Get the predictions for each combination of moderators
predictions.variance <- as.data.frame(expand.grid(Sex = c("M", "B", "F"),
                           Environment = c("Unstressed", "Stressed")))
predictions.variance <- cbind(predictions.variance, do.call("rbind", apply(predictions.variance, 1, get.predictions.variance))) %>%
  select(Sex, Environment, pred, se, ci.lb, ci.ub) 
for(i in 3:6) predictions.variance[,i] <- unlist(predictions.variance[,i])

#And plot the results

pd <- position_dodgev(height = .3)
var.plot <- predictions.variance %>% 
 mutate(Sex = replace(as.character(Sex), Sex == "B", "Both"),
        Sex = replace(Sex, Sex == "M", "Male"),
        Sex = replace(Sex, Sex == "F", "Female")) %>%
  ggplot(aes(x = pred, y= Environment, fill = Sex)) + 
  geom_vline(xintercept = 0, linetype = 2, colour = "grey70") + 
  geom_errorbarh(aes(xmin = predictions.variance$ci.lb, 
                     xmax = predictions.variance$ci.ub,
                     color = Sex), 
                 height = 0, position = pd, show.legend = F) +
  geom_point(position = pd, size=3, shape = 21, color= "grey20") + 
  ylab("Environment\n")+
  xlab("\nlnCVR")+
  xlim(-1.2, 1.2)+
  ggtitle('Meta-Analysis of Variance (Using Model Predictions)')+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")+
  guides(fill = guide_legend(reverse=T))

ggsave(plot = var.plot, filename = "figures/Variance_plot.eps", height = 7.5, width = 10)
var.plot
```

<img src="meta_analysis_sexual_selection_pop_fitness_files/figure-html/unnamed-chunk-27-1.png" width="960" />
<br></br>
**Figure S4:** Phenotypic variation changes under sexual selection in stressful environments. For stressed females, phenotypic variation decreases (narrows). While for males in stressed environments it increases. For outcomes that measured both males and females (both) phenotypic variation did not change **or** changed for males and females in opposite directions and thus cancels out when measured together. 


____________________
##Publication Bias

###Funnel plots and Egger's Test

Checking for biases with a funnel plot. Note that the trim and fill method does not work with rma.mv objects. However we can perform Eggers test using the ``regtest()`` function. This tests for asymmetry via assessing relationships between effect size and a specified predictor. See ``?regtest`` for more information. Because the Eggers test does not work for ``rma.mv`` objects we remove the random effects and run with Sex * Environment as moderators. 

```r
standard.model <- rma(g, var.g, 
                      mods = ~ 1 + Sex * Environment, 
                      data=prelim.data)
regtest(standard.model)
```

```
## 
## Regression Test for Funnel Plot Asymmetry
## 
## model:     mixed-effects meta-regression model
## predictor: standard error
## 
## test for funnel plot asymmetry: z = 6.7264, p < .0001
```

We can use ggplot for creating a funnel plot. The code is pretty clunky and unlike the ``funnel.rma`` it does not use automatically plot residuals so we have to generate them independently. The outline taken from: https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/

> Luke re-wrote this into a function, for neatness. You can replace 'dataset' and 'model' with whatever to save copy-pasting the code.


```r
#Using residuals for the funnel plot means that we need to generate residuals (intercept only)

forest.model <- rma.mv(g, var.g,
                       mods = ~ 1,
                       random = list(~ 1 | Study.ID,
                                       ~ 1 | Outcome),
                       method = "REML",
                       data = prelim.data)

# Obtain residuals
resstandards <- (rstandard.rma.mv(forest.model,
                                   type="response"))

# Obtain grand mean effect size    <- ADDED BY LUKE
grand.mean <- as.numeric(forest.model$b) #WHAT about weightings?

# Create new df with residuals replacing raw
df.forest.model <- prelim.data
df.forest.model$g <- resstandards$resid + grand.mean 
df.forest.model$sei <- resstandards$se

# Funnel plot for all outcome classes

make.funnel <- function(dataset, model){
  
  apatheme <- theme_bw() +  #My APA-format theme
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          text = element_text(family = 'Times'),
          legend.position = 'none')
  
  estimate <- model$b
  SE <- model$se
  se.seq <- seq(0, max(sqrt(dataset$var.g)), 0.001)
  dfCI <- data.frame(ll95 = estimate - (1.96 * se.seq), 
                     ul95 = estimate + (1.96 * se.seq), 
                     ll99 = estimate - (3.29 * se.seq), 
                     ul99 = estimate + (3.29 * se.seq), 
                     se.seq = se.seq, 
                     meanll95 = estimate - (1.96 * SE), 
                     meanul95 = estimate + (1.96 * SE))
  
  ggplot(dataset, aes(x = sqrt(var.g), y = g)) +
    geom_point(size=1, shape = 21, color= "grey20") +
    xlab("Standard Error") + ylab("Effect size (Hedges' g)") +
    geom_line(aes(x = se.seq, y = ll95), linetype = 'dotted', data = dfCI) + # confidence lines
    geom_line(aes(x = se.seq, y = ul95), linetype = 'dotted', data = dfCI) +
    geom_line(aes(x = se.seq, y = ll99), linetype = 'dashed', data = dfCI) +
    geom_line(aes(x = se.seq, y = ul99), linetype = 'dashed', data = dfCI) +
    #Now plot dotted lines corresponding to the 95% CI of your meta-analytic estimate
    geom_segment(aes(x = min(se.seq), y = meanll95, xend = max(se.seq), yend = meanll95), linetype='dotted', data=dfCI, colour = "tomato") +
    geom_segment(aes(x = min(se.seq), y = meanul95, xend = max(se.seq), yend = meanul95), linetype='dotted', data=dfCI, colour = "tomato") +
    scale_x_reverse() +
    # scale_y_continuous(breaks = seq(-1.25,2,0.25)) + #Choose values that work for you based on your data
    coord_flip() +
    scale_fill_brewer(palette = "Set1")
    # theme_classic()
    # apatheme
  
}

funnel.plot <- make.funnel(df.forest.model, forest.model)

ggsave(plot = funnel.plot, filename = "figures/funnel_plot.eps", height = 7.5, width = 10)
funnel.plot
```

<img src="meta_analysis_sexual_selection_pop_fitness_files/figure-html/unnamed-chunk-29-1.png" width="960" />
<br></br>
**Figure S5:** A funnel plot of 459 effect sizes shows asymmetry, indicating potential publication bias, egger's regression test for funnel plot asymmetry also suggests the plot is asymmetrical (z = 7.2671, p < .0001). The asymmetry appears to be sourced from a spread of positive effect sizes outside the funnel and of varying degrees of precision. Counter to expectations of publication bias these positive studies are not just 'low precision, large effect' results. Funnel plot asymmetry may also be due to heterogeneity, which in this study is high due to the many species and outcomes measured. 


###Journal Impact Factor

If we see a positive trend with effect size and Journal Impact Factor (JIF) it may represent publication bias whereby significant (positive) results are published more readily and in more circulated journals and non-confirmitory or negative results are not published or publiushed in lower impact journals. Our journal impact factor dataset is not evenly distributed as several publications in Nature (JIF ~ 40) are much larger than the next highest JIF (~11). 


```r
JIF.plot <- prelim.data %>% ggplot(aes(x=JIF, y=g))+
  geom_jitter(color='darkgreen', alpha=.3, aes(size = (1/(var.g))/sum((1/(var.g)))*100))+
  geom_hline(yintercept=0, linetype = 'dotted')+
  geom_smooth(method='lm', color='black')+
  scale_x_continuous(breaks = c(0,10,20,30,40), limits = c(2,42.5))+
  labs(size = 'Weight (%)', y='Effect size (Hedges g)', x= 'Journal Impact Factor')

ggsave(plot = JIF.plot, filename = "figures/JIF_plot.eps", height = 7.5, width = 10)
JIF.plot
```

<img src="meta_analysis_sexual_selection_pop_fitness_files/figure-html/unnamed-chunk-30-1.png" width="960" />
<br></br>
**Figure S6:** Journal impact factor does not show a noticable correlation with effect size. The positive slope shown here is due to several effect sizes published in a high impact journal. Most papers were published in discipline specific journals such as _Evolution_ and _Journal of Evolutionary Biology_. 

### Time-lag Bias

We can also look at the time-lag bias, which suggests effect size decreases over time. Again, because one publication from 1980 is well before the next publication in the late 1990s we see a very uneven distribution of data points.


```r
time.plot <- prelim.data %>% 
  ggplot(aes(x=Year, y=g))+
  geom_jitter(color='darkorange', alpha=.5, aes(size = (1/(var.g))/sum((1/(var.g)))*100))+
  geom_hline(yintercept=0, linetype = 'dotted')+
  geom_smooth(method='lm', color='black')+
  labs(size = 'Weight (%)', y='Effect size (Hedges g)', x= 'Year')

ggsave(plot = time.plot, filename = "figures/time_plot.eps", height = 7.5, width = 10)
time.plot
```

<img src="meta_analysis_sexual_selection_pop_fitness_files/figure-html/unnamed-chunk-31-1.png" width="960" />
<br></br>
**Figure S7:** The effect size dataset shows little to no signs of the time-lag bias as the average effect sizes from published studies remains consistent across the previous two decades. 


###Sample Size

We also collected sample sizes for each of the effect sizes calculated. Because we are dealing withj different taxa some studies are not suited to have sample sizes in the 1000's. We can simply inspect the sample size ande effect sizes through the following plot:


```r
samplesize.plot <- prelim.data %>% ggplot(aes(x=n, y = g))+
  geom_point(fill="grey", color="grey20", shape=21)+
  scale_x_log10(breaks = c(10, 100, 1000, 10000)) + 
  #xlim(0,2100)+
  facet_grid(Taxon~., scales='free')+
  ylim(-3.5,3.5)+
  geom_hline(yintercept=0, linetype="dashed")+
  labs(y='Effect size (Hedges g)', x= 'Sample Size (n) [logscale]')

ggsave(plot = samplesize.plot, filename = "figures/samplesize_plot.eps", height = 10, width = 5)
samplesize.plot
```

<img src="meta_analysis_sexual_selection_pop_fitness_files/figure-html/unnamed-chunk-32-1.png" width="480" />
<br></br>
**Figure S8:** Variation in size does not appear to have unexpected trends in any taxons, with greater variation in effect size for those studies utilising a lower sample size. 

From these plots we can see that with increased sample size the effect sizes are closer to zero. This trend is taken into account as meta-analytic models and are wighted by 1/variance. **Note** Promislow (1998) has one sample size of >10,000 in flies and is not shown here to avoid making radical changes to the scale. 

________________

##Other Moderators

### Blinding

In addition to publication bias, other forms of bias may exist within studies. We initially collected data on whether studies were blind or not. Although not many studies (n=8) used blinding there was multiple effect sizes reported in these studies, thus we can visualise whether blinding affects the effect sizes from the model. Blinding was regarded as a redundant predictor in the model (estimate = 0.0287, p = 0.8974) and was dropped. 


```r
blind.plot <- df.forest.model %>% ggplot(aes(x=Blinding, y=g))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(fill=Blinding, size = (1/(var.g))/sum((1/(var.g)))*100), shape=21, color='grey20')+
  geom_hline(yintercept=0, linetype = 'dotted') + 
  scale_fill_brewer(palette = "Set2")+
  labs(y='Effect size (Hedges g)', x= 'Blinding', size = 'Weight (%)')+
  guides(fill=FALSE)

ggsave(plot = blind.plot, filename = "figures/blind_plot.eps", height = 7.5, width = 10)
blind.plot
```

<img src="meta_analysis_sexual_selection_pop_fitness_files/figure-html/unnamed-chunk-33-1.png" width="960" />
<br></br>
**Figure S9:** Blinding does not appear to alter the magnitude or direction of effect sizes for the studies used in this meta-analysis. However, this should not be viewed as evidence against the validity of blinding as a research method. 


### Generations

We recorded the number of generations of experimental exolution each study used. The number of generations proved a negligible predictor in the meta-analytic models (estimate = 0.0019, p = 0.2341). The effect sizes are plotted against the generation at which the effect size was extracted. 


```r
generations.plot <- df.forest.model %>% ggplot(aes(x=Generations, y=g))+
  geom_point(shape=21, color = "grey20", size=2, aes(fill=Taxon))+
  ylim(-3.5,3.5)+
  geom_hline(yintercept=0, linetype="dashed") + 
  scale_fill_brewer(palette = "Set3")+
  geom_smooth(method = 'lm', color='black')+
  labs(y='Effect size (Hedges g)', x= 'Generations', size= 'Weight (%)')

ggsave(plot = generations.plot, filename = "figures/generations_plot.eps", height = 7.5, width = 10)
generations.plot
```

<img src="meta_analysis_sexual_selection_pop_fitness_files/figure-html/unnamed-chunk-34-1.png" width="960" />
<br></br>
**Figure S10:** The number of generations an experimental evolution procedure is run for does not appear to affect the magnitude or direction of the effect size from the fitness related outcome measured at that point. 

________________

##R Session Information

This section shows the operating system and R packages attached during the production of this document


```r
sessionInfo() %>% pander
```

**R version 3.3.1 (2016-06-21)**

**Platform:** x86_64-apple-darwin13.4.0 (64-bit) 

**locale:**
en_AU.UTF-8||en_AU.UTF-8||en_AU.UTF-8||C||en_AU.UTF-8||en_AU.UTF-8

**attached base packages:** 
_grid_, _stats_, _graphics_, _grDevices_, _utils_, _datasets_, _methods_ and _base_

**other attached packages:** 
_bindrcpp(v.0.2)_, _cowplot(v.0.9.1)_, _glmulti(v.1.0.7)_, _rJava(v.0.9-8)_, _MuMIn(v.1.40.0)_, _RColorBrewer(v.1.1-2)_, _reshape2(v.1.4.2)_, _ggrepel(v.0.7.0)_, _kableExtra(v.0.7.0)_, _ggplot2(v.2.2.1)_, _forestplot(v.1.7.2)_, _checkmate(v.1.8.5)_, _magrittr(v.1.5)_, _lme4(v.1.1-13)_, _dplyr(v.0.7.4)_, _metafor(v.2.0-0)_, _Matrix(v.1.2-6)_, _compute.es(v.0.2-4)_, _pander(v.0.6.1)_ and _knitr(v.1.16)_

**loaded via a namespace (and not attached):** 
_splines(v.3.3.1)_, _lattice(v.0.20-33)_, _colorspace(v.1.3-2)_, _htmltools(v.0.3.6)_, _stats4(v.3.3.1)_, _viridisLite(v.0.2.0)_, _yaml(v.2.1.16)_, _rlang(v.0.1.4)_, _nloptr(v.1.0.4)_, _glue(v.1.1.1)_, _bindr(v.0.1)_, _plyr(v.1.8.4)_, _stringr(v.1.2.0)_, _munsell(v.0.4.3)_, _gtable(v.0.2.0)_, _rvest(v.0.3.2)_, _codetools(v.0.2-14)_, _evaluate(v.0.10.1)_, _labeling(v.0.3)_, _highr(v.0.6)_, _Rcpp(v.0.12.14)_, _readr(v.1.1.1)_, _scales(v.0.5.0)_, _backports(v.1.1.0)_, _hms(v.0.3)_, _digest(v.0.6.13)_, _stringi(v.1.1.5)_, _rprojroot(v.1.2)_, _tools(v.3.3.1)_, _lazyeval(v.0.2.1)_, _tibble(v.1.3.4)_, _pkgconfig(v.2.0.1)_, _MASS(v.7.3-45)_, _xml2(v.1.1.1)_, _assertthat(v.0.2.0)_, _minqa(v.1.2.4)_, _rmarkdown(v.1.6)_, _httr(v.1.3.1)_, _R6(v.2.2.2)_ and _nlme(v.3.1-128)_



<!-- Some JavaScript to control the buttons to show/hide the big tables -->
<script>
$( "input.hideshow" ).each( function ( index, button ) {
  button.value = 'Hide Output';
  $( button ).click( function () {
    var target = this.nextSibling ? this : this.parentNode;
     target = target.nextSibling.nextSibling.nextSibling.nextSibling.nextSibling;
    if ( target.style.display == 'block' || target.style.display == '' ) {
      target.style.display = 'none';
      this.value = 'Show Output';
    } else {
      target.style.display = 'block';
      this.value = 'Hide Output';
    }
  } );
} );
</script>
