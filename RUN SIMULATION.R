source("3. Simulation.R")

refiant_sim(n_nodes = 100, iter = 10, p_ret = 0.1, event = 2, S_lambda = 0)

plt_example
plt_returns + xlim(-100, 200)
plt_returns_y + ylim(-100, 200)
plt_returns_sdi + ylim(-200, 400)
plt_insolvency_y
plt_claims
