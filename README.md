# SunRE REFI: Parametric Collateralised Insurance Options for Solar!
## Overview
SunRE REFI simulates how parametric derivatives can be used to underwrite natural catastrophe risk for the next generation of clean energy infrastructure. 
The simulator utilises portfolios of single-trigger parametric weather-derived collateralised call options to underwrite natural catastrophe risk for solar farms. 
These light-weight products provide a generic, scalable and liquid solution to the provision of energy finance and asset insurance.
By focussing on the physical element of natural catastrophe insurance, SunRE REFI is able to underwrite energy infrastructure without the need for complex and expensive engineering models, and asset-specific underwriting.
As such, SunRE REFI narrows the scope of a highly complex and technically challenging insurance underwriting problem. 
This allows the protocol to leverage highly liquid Web3 markets, emergent capital and smart contract technology, and provide a scalable solution for asset owners, insurance captives, reinsurance and mutuals alike.

## Simulator Structure and Features
The simulator takes publically available weather, geography and physics data to simulate claims for any portfolio of solar farm assets across the mainland United States and Hawaii. 
These claims occur via single-variable weather triggers from a simulated data-oracle collecting data for each solar farm site.
Perils covered currently include:
- Wind/storm (payout to an insured asset when maximum daily wind speed >= 34 kt and < 50 kt)
- Wildfire (payout when a wildfire crosses into the insured asset's county)
- Hurricane (payout when >= 50 kt windspeeds of a hurricane overlaps with the insured asset)
- Hail (payout when hail swath with maximum hail stone size >= 1.75 ins overaps with the insured asset)

