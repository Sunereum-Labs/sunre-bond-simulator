# install.packages("tidyverse")
# install.packages("geosphere")
# install.packages("sf")

source("./scripts/3. Simulation.R")

sunrefi_sim(iter = 100)

plt_example

plt_returns
plt_returns_year

plt_insolvency
plt_insolvency_year
plt_insolvency_sdi

plt_claims
plt_claims_year
plt_peril_year

# save results
write_csv(output, "./results/output.csv")
ggsave("./results/sample_year.jpeg", plt_example, width = 2400, height = 1200, units = "px")
ggsave("./results/returns.jpeg", plt_returns, width = 2400, height = 1200, units = "px")
ggsave("./results/returns_year.jpeg", plt_returns_year, width = 2400, height = 1200, units = "px")
ggsave("./results/insolvency.jpeg", plt_insolvency, width = 2400, height = 1200, units = "px")
ggsave("./results/insolvency_year.jpeg", plt_insolvency_year, width = 2400, height = 1200, units = "px")
ggsave("./results/insolvency_sdi.jpeg", plt_insolvency_sdi, width = 2400, height = 1200, units = "px")
ggsave("./results/claims.jpeg", plt_claims, width = 2400, height = 1200, units = "px")
ggsave("./results/claims_year.jpeg", plt_claims_year, width = 2400, height = 1200, units = "px")
ggsave("./results/peril_year.jpeg", plt_peril_year, width = 2400, height = 1200, units = "px")
