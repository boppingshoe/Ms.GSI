# Ms.GSI 0.2.0

* In the previous version, accounting of stock-specific total catch for tier 2 of the model was incorrect, which caused discrepancy in stock proportions calculated using `stratified_estimator_msgsi()`. The errors have been corrected.
* Removed `msgsi_harv_summ()` function. The same summary can be done using `stratified_estimator_msgsi()`.

# Ms.GSI 0.1.1

* Calculation of p0 = 0.5 / total harvest. Changed from stock-specific harvest.

# Ms.GSI 0.1.0

* Updated from the earlier version to estimate uncertainty for stock-specific total catch (sstc).
* Stratified estimator based on sstc.
* Probability of proportion = 0 (z0) based on trace history of individual assigned to each collection/reporting group.
