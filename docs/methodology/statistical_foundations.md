# Statistical & Mathematical Foundations

## 1. Return Measurement

### Log Returns
$$r_t = \ln\!\left(\frac{S_t}{S_{t-1}}\right)$$

**Properties:**
- Time-additive: $r_{0,T} = \sum_{t=1}^T r_t$ (no need to chain-multiply)
- Approximately normally distributed for short horizons
- Symmetric: a +10% and −10% log return cancel exactly

**When to use simple returns instead:** Portfolio aggregation across assets. If $w_i$ are portfolio weights, the portfolio simple return is $r_{p,t} = \sum_i w_i r_{i,t}$, which does not hold for log returns.

---

## 2. Risk-Adjusted Performance

### Sharpe Ratio (Sharpe, 1966)
$$\text{SR} = \frac{\mu_{\text{ann}} - r_f}{\sigma_{\text{ann}}}$$

**Limitations:** Penalises upside and downside volatility equally; assumes normally distributed returns; not valid for strategies with option-like payoffs.

### Sortino Ratio
$$\text{Sortino} = \frac{\mu_{\text{ann}} - r_f}{\sigma_{\downarrow}}$$

where $\sigma_{\downarrow} = \sqrt{\frac{1}{T}\sum_{t} \min(r_t - \tau, 0)^2}$ is the downside semi-deviation below target $\tau$.

**Advantage over Sharpe:** Only penalises harmful volatility.

### Calmar Ratio
$$\text{Calmar} = \frac{\mu_{\text{ann}}}{|\text{MDD}|}$$

Preferred by CTAs and hedge funds for measuring drawdown-adjusted returns.

---

## 3. Value at Risk & Expected Shortfall

### Parametric VaR (Normal)
$$\text{VaR}_\alpha = -(\mu + z_\alpha \sigma)$$

where $z_\alpha = \Phi^{-1}(\alpha)$.

**Limitations:** Assumes normality; underestimates tail risk for fat-tailed returns.

### Expected Shortfall (Conditional VaR)
$$\text{ES}_\alpha = -\left(\mu - \sigma\frac{\phi(z_\alpha)}{\alpha}\right)$$

ES is a **coherent** risk measure (Artzner et al., 1999) — it satisfies sub-additivity, monotonicity, positive homogeneity, and translation invariance. VaR is **not** coherent (it violates sub-additivity for non-elliptical distributions).

---

## 4. CAPM

The CAPM (Sharpe, 1964; Lintner, 1965) prices assets via their covariance with the market:

$$\mathbb{E}[r_i] - r_f = \beta_i (\mathbb{E}[r_m] - r_f)$$

where $\beta_i = \text{Cov}(r_i, r_m) / \text{Var}(r_m)$.

**Jensen's Alpha:** $\alpha_i = \bar{r}_i - [r_f + \hat{\beta}_i(\bar{r}_m - r_f)]$ measures risk-adjusted abnormal return.

**Idiosyncratic Risk:** $\sigma_\varepsilon^2 = \sigma_i^2 - \beta_i^2 \sigma_m^2$ — the component diversifiable in a large portfolio.

---

## 5. GARCH(1,1)

The GARCH(1,1) model (Bollerslev, 1986) captures **volatility clustering** — the empirical finding that large returns tend to cluster in time:

$$\sigma_t^2 = \omega + \alpha \varepsilon_{t-1}^2 + \beta \sigma_{t-1}^2$$

**Stationarity:** $\alpha + \beta < 1$

**Persistence:** $\alpha + \beta$ measures how slowly shocks decay. Near 1 ⟹ very persistent volatility.

**Long-run variance:** $\bar{\sigma}^2 = \frac{\omega}{1 - \alpha - \beta}$

**Estimated via MLE** under a distributional assumption for innovations $z_t$ (Normal, Student-t, GED).

---

## 6. Monte Carlo — GBM

The geometric Brownian motion SDE:
$$dS_t = \mu S_t \, dt + \sigma S_t \, dW_t$$

Exact solution via Itô's lemma:
$$S_T = S_0 \exp\!\left[\left(\mu - \frac{\sigma^2}{2}\right)T + \sigma W_T\right]$$

**Itô correction:** The $-\sigma^2/2$ term ensures $\mathbb{E}[S_T] = S_0 e^{\mu T}$ (not $S_0 e^{(\mu - \sigma^2/2)T}$).

**Multi-asset:** Correlated Brownian motions generated via Cholesky decomposition:
$$\boldsymbol{\Sigma} = \mathbf{L}\mathbf{L}^\top, \quad \mathbf{W} = \mathbf{L}\mathbf{Z}, \quad \mathbf{Z} \sim \mathcal{N}(\mathbf{0}, \mathbf{I})$$

---

## 7. Walk-Forward Validation

Classical train/test split is invalid for time series (look-ahead bias). The correct approach is **expanding-window cross-validation**:

For $t = T_{\text{min}}, \ldots, T - h$:
1. Fit model on $y_1, \ldots, y_t$
2. Forecast $\hat{y}_{t+1}, \ldots, \hat{y}_{t+h}$
3. Record error $e_{t+j} = y_{t+j} - \hat{y}_{t+j}$

This produces pseudo-out-of-sample errors that estimate true generalisation performance under temporal ordering.

---

## References

- Markowitz, H. (1952). Portfolio selection. *Journal of Finance*, 7(1), 77–91.
- Sharpe, W. F. (1964). Capital asset prices. *Journal of Finance*, 19(3), 425–442.
- Black, F., & Scholes, M. (1973). The pricing of options and corporate liabilities. *Journal of Political Economy*, 81(3), 637–654.
- Bollerslev, T. (1986). Generalized autoregressive conditional heteroskedasticity. *Journal of Econometrics*, 31(3), 307–327.
- Artzner, P., et al. (1999). Coherent measures of risk. *Mathematical Finance*, 9(3), 203–228.
- Hyndman, R. J., & Khandakar, Y. (2008). Automatic time series forecasting. *Journal of Statistical Software*, 27(3).
- Merton, R. C. (1973). An intertemporal capital asset pricing model. *Econometrica*, 41(5), 867–887.
