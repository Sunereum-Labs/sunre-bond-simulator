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
Portfolios are randomly selected subject to user input variables such as state(s), min and max solar farm AC MW size, number of assets, etc.
Selection of specific assets is easily configurable in the code-base down the road when required. 
The simulator runs _m_ portfolio iterations containing _n_ assets over _y_ historical weather years.
This creates _m_ x _n_ x _y_ datapoints of risk.

Details on protocol design, data sources, augmentations and assumptions, and US solar sites on Notion: https://www.notion.so/Refiant-Simulator-1ecdd598bdb480939e38ff1098cdc9a9

## Running the Simulator

### Requirements
The simulator is built in and requires R to run.
R and R packages are relatively robust compared to Python which typically requires specific Python and package versions to run modules.
Any R >= 4.3.3 should work with this program.

The latest R can be downloaded from: https://cloud.r-project.org/ <br>
Optional R Studio IDE: https://posit.co/downloads/

This program requires _Tidyverse_, _Geosphere_ and _SF_ packages to run. 
These are commented out at the top of the RUN SIMULATION.R script:
```r
install.packages("tidyverse")
install.packages("geosphere")
install.packages("sf")
```
*Packages only need to be installed the first time*

### Protocol Set Up
In RUN SIMULATION.R, `refiant_sim()` will call on the program to run the simulation. 
Any individual or combinations of variables is settable in the following list of arguments in `refiant_sim()` (default values shown):
```r
refiant_sim(seed = 100, # sets seed for random asset selection and investment returns
            iter = 20, # number of random portfolios to iterate over (m)
            wy = 1955:2023, # vector of weather years
            CAR = 1, # capital-adequacy-ratio, e.g. CAR = 0.2 => $0.2 of capital for every $1 of risk 
            L1_ratio = 0, # proportion of capital in L1
            states = c("NY"), # state(s) to draw insured assets from
            n_assets = 10, # number of insured assets in a portfolio
            sf_ac_min = 0, # minimum MW size of an insured asset
            sf_ac_max = 10000, # maximum MW size of an insured asset
            fy = 2022, # financial year for treasury, S&P returns and USD/ETH (can take values/ a vector with 2021-2024 incl.)
            eth_float = 0, # USD/ETH basis risk flag, if 0 then the portfolio is basis hedged (e.g. holding USDT instead of ETH)
            R_mu = log(0.06), # L1 staking yield is drawn from LN(mu, sigma^2)
            R_sigma = sqrt(log(1+0.03^2/0.06^2)), # L1 staking yield drawn from LN(mu, sigma^2)
            S_lambda = 0.002, # L1 probability of slashing drawn from Pois(lambda)
            G_alpha = 2, # L1 size of a slash drawn from Beta(alpha, beta)
            G_beta  = 5) # L1 size of a slash drawn from Beta(alpha, beta)
```

### Simulation Results
The simulator automaticallly generates a number of plots for results visualisation.
These are call-able in the RUN SIMULATION.R script:
```r
plt_example # daily time series of 1 iteration-year

# protocol yield
plt_returns # monte carlo distribution of L1 and L2 yields
plt_returns_year + ylim(-100, 100) # distribution of L1 and L2 yields by weather year

plt_insolvency # monte carl distribution of L1 and L2 solvency
plt_insolvency_year # distribution of insolvency by weather year
plt_insolvency_sdi # protocol involency by geographic portfolio diversification

plt_claims # distribution of number of yearly claims
plt_claims_year # distribution of claims by weather year
plt_peril_year # distribution of fire, hail, hurricane and wind claims by weather year
```
Results are also directly accessible in the `output` file.
Each contains a single iteration year with L1 and L2 returns, solvency flags (i.e. did the pool go insolvent at any time in the year), number of claims and SDI.

## Example Results and Interpretation
The following results are from a simulation with default values.
Notably, these are 10 asset portfolios in the state of New York run over all weather years 1955-2023.
The protocol assumes all capital in L2 (i.e. everything is held in USD) and receives a yield from a treasury and the S&P.
Capital adequacy ratio is 1, we hold $1 of capital for every $1 of risk we underwrite.

![Simulation Example](example_simulation/example.jpg)
This example iteration-year is relatively mild with only a single claim partway through the way.
The protocol 



