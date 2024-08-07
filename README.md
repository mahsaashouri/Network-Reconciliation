# Forecasting Network Flow using Reconciliation and Flow Aggregation

In this paper, we introduce a novel approach to improving flow forecast accuracy within network structures across varying levels of aggregation. By considering connections among network nodes and potential correlations among series, our method uses a new aggregation structure to ensure that associated forecasts are coherent. Our proposed network aggregation and reconciled forecasts effectively manage large flows of network datasets by summarizing them into macro flows, aiding in data management, decision-making, and resource allocation. Evaluation of both real-world and simulated datasets demonstrates the effectiveness of reconciliation in improving forecast accuracy. Furthermore, our approach can be extended to scenarios involving multiple connected or disconnected sub-networks, providing a more generalized aggregation methodology applicable to diverse fields such as traffic management.

## Reproducing results

- `Running-results-3Most.R` and `Result-analysis.R`: Table 2 and Figures 3 and 4.
- First-Sim/`GenerateGraph-SamplePathsR.R`: Generate simulated data in Table 3.
- First-Sim/`Run-sim.R` and `Result-analysis.R`: Tables 4 and 5.
- `Run-groups-net.R` and `Result-analysis.R`: Table 7.
