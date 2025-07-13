# SunRE REFI: Parametric Collateralised Insurance Options for Solar!
## Overview
SunRE REFI simulates how parametric derivatives can be used to underwrite natural catastrophe risk for the next generation of clean energy infrastructure. 
The simulator utilises portfolios of single-trigger parametric weather-derived collateralised call options to underwrite natural catastrophe risk for solar farms. 
These light-weight products provide a generic, scalable and liquid solution to the provision of energy finance and asset insurance.
By focussing on the physical element of natural catastrophe insurance, SunRE REFI is able to underwrite energy infrastructure without the need for complex and expensive engineering models, and asset-specific underwriting.
As such, SunRE REFI narrows the scope of a highly complex and technically challenging insurance underwriting problem. 
This allows the protocol to leverage highly liquid Web3 markets, emergent capital and smart contract technology, and provide a scalable solution for asset owners, insurance captives, reinsurance and mutuals alike.

## Simulator Structure and Features
The simulator takes publically available historical weather, geography and physics data to simulate claims for any portfolio of solar farm assets across the mainland United States and Hawaii. 
These claims occur via single-variable weather triggers from a simulated data-oracle collecting data for each solar farm site.
Perils covered currently include:
- Wind/storm (payout to an insured asset when maximum daily wind speed >= 34 kt and < 50 kt)
- Wildfire (payout when a wildfire crosses into the insured asset's county)
- Hurricane (payout when >= 50 kt windspeeds of a hurricane overlaps with the insured asset)
- Hail (payout when hail swath with maximum hail stone size >= 1.75 ins overaps with the insured asset)

The protocol defends it's solvency through structured pools of capital. 
These pools receive capital injections through premium and capital investment returns, and leakages through claims (when trigger conditions occur) and investment losses. 
Currently, there are two pools or _layers_ of capital.
The first, layer 1 (L1), operates as a backstop or "last line of defence" in defending the solvency of the protocol.
Comensurate with the protocol design, this fund is denominated in Ethereum, earns a staking yield and is subjected to (a historically marginal) slashing risk.
Layer 2 (L2), captive/insurance layer, directly interacts with insurance liabilities and pays claims to/receives premium directly from insured parties.
L1 is USD denominated and invests capital in a combination of US treasury bonds and a S&P500 index fund.
Effectively, L2 can thought of as a maximal loss reinsurance layer for L1, who's exposure is mediated by the amount of capital at risk (or maximal loss before insolvency) of L1.
L2 will only be called on to payout claims once L1 becomes insolvent.
Where L1 is subject to USD/ETH basis risk, the end-of-day spot exchange rate is used to move capital between the two pools.
No transaction fees of exchange or purchase/liquidation of investment assets is assumed.
For simplicity and at this initial stage, L1 transfers premium to L2 for reinsurance proportional to total protocol capital contributed.

Premium is calculated for a given portfolio of risks as the expected value of claims over the entire multi-year simulation paid nominally, and uniformly each day (ignores discounting and inflation).
The integration of an actuarial premium model when available would add significant value to this simulation, especially in the simulation of protocol returns.
The simulator code-base would easily incorporate the deployment of an exogenous premium model when there is one available.

The simulator has also been designed to incorporate more complete, robust and detailed natural peril datasets when available.
Currently, simulation can be run on historical weather years spanning 1955-2023.
The incorporation of more historical data or propiertary forecast data would allow for a greater depth of empirical simulation.

The goal of the simulator is to empirically test solvency and yield on capital for different portfolios of insured assets, and protocol configurations.
This is akin to monte carlo simulation used to construct probability distributions in complex, multi-variate probability problems; and common in short-tail insurance modelling.
The simulator operates in discrete daily time steps, with premium received and claims paid out at the end of each day after the realisation of investment returns.
Each simulation year is an intra-year time-dependent Markov Chain but inter-year independent.
Effectively, the portfolio is "reset" every year (capital restored and profits taken); each year of simulation is independent of eachother.
This allows for the capturing of intra-year depedent risks but disaggregates inter-year correlations/clusters of risks.
This decision and protocal design choice aligns with the liquid and short-duration nature of Web3 financial products.
The simulator runs _m_ portfolio iterations containing _n_ assets over _y_ historical weather years.
This creates _m_ x _n_ x _y_ datapoints of risk.

Details on protocol design, data sources, augmentations and assumptions on Notion: https://www.notion.so/Refiant-Simulator-1ecdd598bdb480939e38ff1098cdc9a9

## Running the Simulator

### Requirements
The simulator is built in and requires R to run.
R and R packages are relatively robust compared to Python which typically requires specific Python and package versions to run modules.
Any R >= 4.3.3 should work with this program.

The latest R can be downloaded from: https://cloud.r-project.org/ 
Optional R Studio IDE: https://posit.co/downloads/

This program requires _Tidyverse_, _Geosphere_ and _SF_ packages to run. 
These can be installed from the top of the RUN SIMULATION.R script:
```r
install.packages("tidyverse")
install.packages("geosphere")
install.packages("sf")
```

