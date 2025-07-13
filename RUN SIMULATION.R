# install.packages("tidyverse")
# install.packages("geosphere")
# install.packages("sf")

source("./scripts/3. Simulation.R")

refiant_sim(iter = 1)

plt_example

plt_returns
p <- plt_returns_year + ylim(-100, 100)

plt_insolvency
plt_insolvency_year
plt_insolvency_sdi

plt_claims
plt_claims_year
plt_peril_year

ggsave("./example simulation/plt_example.jpeg", plot = plt_example, width = 15, height = 10)
ggsave("./example simulation/plt_returns_year.jpeg", plot = p, width = 15, height = 10)
ggsave("./example simulation/plt_insolvency.jpeg", plot = plt_insolvency, width = 15, height = 10)
ggsave("./example simulation/plt_insolvency_year.jpeg", plot = plt_insolvency_year, width = 15, height = 10)
ggsave("./example simulation/plt_insolvency_sdi.jpeg", plot = plt_insolvency_sdi, width = 15, height = 10)
ggsave("./example simulation/plt_claims.jpeg", plot = plt_claims, width = 15, height = 10)
ggsave("./example simulation/plt_claims_year.jpeg", plot = plt_claims_year, width = 15, height = 10)
ggsave("./example simulation/plt_peril_year.jpeg", plot = plt_peril_year, width = 15, height = 10)
