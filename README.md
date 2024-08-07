# Forecasting Network Flow using Reconciliation and Flow Aggregation

Forecasting flows over time through nodes of a network is an important task that arises in many applied contexts. In this paper, we introduce a novel reconciliation approach to improving flow forecast accuracy within network structures for both flows between individual nodes and different levels of aggregated flows. By considering connections among network nodes and potential correlations among the flow time series, our method uses a new aggregation structure to ensure that the associated forecasts are coherent. Our proposed network aggregation and reconciled forecasts effectively manage large flows of network datasets by summarizing them into macro flows, aiding in data management, decision-making, and resource allocation. Evaluation of both real-world and simulated datasets demonstrates the effectiveness of our reconciliation approach in improving forecast accuracy. Furthermore, our approach can be extended to scenarios involving multiple connected or disconnected sub-networks, providing a more generalized aggregation methodology applicable to diverse fields such as traffic management.

## Reproducing results

- `Running-results-3Most.R` and `Result-analysis.R`: Table 2 and Figures 3 and 4.
- First-Sim/`GenerateGraph-SamplePathsR.R`: Generate simulated data in Table 3.
- First-Sim/`Run-sim.R` and `Result-analysis.R`: Tables 4 and 5.
- `Run-groups-net.R` and `Result-analysis.R`: Table 7.
