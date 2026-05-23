# =============================================================================
# Regression Econometrics Module
#
# Implements with mathematical rigour:
#   - OLS regression (from scratch via matrix algebra)
#   - Robust regression (Huber M-estimator)
#   - Logistic regression
#   - Ridge / Lasso / Elastic Net (regularised regression)
#   - Structural break detection (Chow test, CUSUM)
#   - Heteroskedasticity & autocorrelation tests (White, Breusch-Pagan, DW)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(broom)
  library(sandwich)
  library(lmtest)
  library(glmnet)
  library(glue)
})

source(here::here("R", "utilities", "logger.R"))

# =============================================================================
# SECTION 1: OLS FROM FIRST PRINCIPLES
# =============================================================================

#' OLS Regression — Full Matrix Implementation
#'
#' For the linear model y = Xβ + ε, OLS solves:
#'   β̂ = (X'X)⁻¹ X'y              [normal equations]
#'   Var(β̂) = σ² (X'X)⁻¹         [coefficient covariance]
#'   σ² = RSS / (n − k)            [unbiased estimator of error variance]
#'   R² = 1 − RSS/TSS              [coefficient of determination]
#'   F = [(TSS−RSS)/k] / [RSS/(n−k)]  [overall significance test]
#'
#' Returns detailed results equivalent to — but more transparent than — lm().
#'
#' @param y        Numeric response vector (n × 1)
#' @param X        Numeric predictor matrix (n × k), WITHOUT intercept column
#' @param var_names Column names for X
#' @return Named list with coefficients, SEs, t-stats, p-values, diagnostics
#' @export
ols_manual <- function(y, X, var_names = NULL) {
  n <- length(y)
  k <- ncol(X)

  # Add intercept column
  X_full <- cbind(intercept = 1, X)
  p      <- ncol(X_full)

  # Normal equations: β̂ = (X'X)⁻¹ X'y
  XtX     <- crossprod(X_full)
  XtX_inv <- tryCatch(solve(XtX), error = function(e) {
    log_warn("[ols] Singular X'X — using pseudoinverse")
    MASS::ginv(XtX)
  })

  beta_hat <- XtX_inv %*% crossprod(X_full, y)

  # Fitted values and residuals
  y_hat   <- X_full %*% beta_hat
  resid   <- y - y_hat
  rss     <- sum(resid^2)
  tss     <- sum((y - mean(y))^2)

  # Variance estimators
  sigma2  <- rss / (n - p)
  vcov    <- sigma2 * XtX_inv
  se      <- sqrt(diag(vcov))

  # t-statistics and p-values (two-sided)
  t_stats <- beta_hat[, 1] / se
  p_vals  <- 2 * pt(abs(t_stats), df = n - p, lower.tail = FALSE)

  # R² and adjusted R²
  r2      <- 1 - rss / tss
  r2_adj  <- 1 - (1 - r2) * (n - 1) / (n - p)

  # F-statistic: tests H₀: β₁ = ... = β_k = 0
  f_stat  <- ((tss - rss) / (p - 1)) / (rss / (n - p))
  f_pval  <- pf(f_stat, df1 = p - 1, df2 = n - p, lower.tail = FALSE)

  coef_names <- c("(Intercept)", if (!is.null(var_names)) var_names else
                    paste0("X", seq_len(k)))

  coef_table <- tibble::tibble(
    term      = coef_names,
    estimate  = beta_hat[, 1],
    std_error = se,
    t_stat    = t_stats,
    p_value   = p_vals,
    signif    = dplyr::case_when(
      p_vals < 0.001 ~ "***",
      p_vals < 0.01  ~ "**",
      p_vals < 0.05  ~ "*",
      p_vals < 0.10  ~ ".",
      TRUE           ~ ""
    )
  )

  list(
    coefficients   = coef_table,
    r_squared      = r2,
    r_squared_adj  = r2_adj,
    f_statistic    = f_stat,
    f_p_value      = f_pval,
    rss            = rss,
    tss            = tss,
    sigma          = sqrt(sigma2),
    vcov           = vcov,
    fitted         = as.numeric(y_hat),
    residuals      = as.numeric(resid),
    n              = n,
    p              = p
  )
}

# =============================================================================
# SECTION 2: DIAGNOSTIC TESTS
# =============================================================================

#' Breusch-Pagan Test for Heteroskedasticity
#'
#' Tests H₀: homoskedastic errors (Var(ε_i) = σ² for all i)
#' against H₁: Var(ε_i) = f(X_i γ)
#'
#' @param model lm object
#' @return Named list: statistic, p_value, conclusion
#' @export
test_heteroskedasticity <- function(model) {
  bp <- lmtest::bptest(model)
  list(
    test       = "Breusch-Pagan",
    statistic  = as.numeric(bp$statistic),
    df         = bp$parameter,
    p_value    = bp$p.value,
    conclusion = if (bp$p.value < 0.05)
                   "Reject H₀: heteroskedastic errors detected"
                 else
                   "Fail to reject H₀: no evidence of heteroskedasticity"
  )
}

#' Durbin-Watson Test for Serial Correlation in Residuals
#'
#' DW statistic d ≈ 2(1 − ρ̂) where ρ̂ is first-order autocorrelation.
#' d ≈ 2 ⟹ no autocorrelation; d < 2 ⟹ positive; d > 2 ⟹ negative.
#'
#' @param model lm object
#' @return Named list with DW statistic, p-value, conclusion
#' @export
test_autocorrelation <- function(model) {
  dw <- lmtest::dwtest(model)
  list(
    test       = "Durbin-Watson",
    statistic  = as.numeric(dw$statistic),
    p_value    = dw$p.value,
    conclusion = if (dw$p.value < 0.05)
                   "Reject H₀: positive serial correlation detected"
                 else
                   "Fail to reject H₀: no significant autocorrelation"
  )
}

#' Run full regression diagnostics suite
#'
#' @param model lm object
#' @return tibble with all diagnostic test results
#' @export
regression_diagnostics <- function(model) {
  het  <- test_heteroskedasticity(model)
  ac   <- test_autocorrelation(model)
  reset <- tryCatch(
    lmtest::resettest(model, type = "fitted"),
    error = function(e) list(statistic = NA, p.value = NA)
  )

  tibble::tibble(
    test      = c("Breusch-Pagan (Heteroskedasticity)",
                  "Durbin-Watson (Autocorrelation)",
                  "Ramsey RESET (Functional Form)"),
    statistic = c(het$statistic, ac$statistic, as.numeric(reset$statistic)),
    p_value   = c(het$p_value, ac$p_value, reset$p.value),
    flagged   = c(het$p_value < 0.05, ac$p_value < 0.05,
                  ifelse(is.na(reset$p.value), NA, reset$p.value < 0.05))
  )
}

#' HAC-Robust Standard Errors (Newey-West)
#'
#' Corrects standard errors for both heteroskedasticity and autocorrelation.
#' Essential for time-series regressions where iid errors cannot be assumed.
#'
#' @param model lm object
#' @return tibble with HAC standard errors and corrected inference
#' @export
hac_robust_se <- function(model) {
  vcov_hac <- sandwich::NeweyWest(model)
  ct       <- lmtest::coeftest(model, vcov = vcov_hac)
  broom::tidy(ct) |>
    dplyr::rename(hac_std_error = std.error, hac_statistic = statistic, hac_p_value = p.value)
}

# =============================================================================
# SECTION 3: REGULARISED REGRESSION
# =============================================================================

#' Fit Ridge / Lasso / Elastic Net regression with cross-validated λ
#'
#' Regularised objective: min { ||y − Xβ||² + λ[(1−α)||β||² + α||β||₁] }
#'   α = 0 ⟹ Ridge   (L2 penalty, shrinks all coefficients)
#'   α = 1 ⟹ Lasso   (L1 penalty, performs variable selection)
#'   0 < α < 1 ⟹ Elastic Net (combines both)
#'
#' @param y       Response vector
#' @param X       Predictor matrix (no intercept — handled internally)
#' @param alpha   Mixing parameter (0 = Ridge, 1 = Lasso, else Elastic Net)
#' @param nfolds  Cross-validation folds for λ selection
#' @return List: model, optimal_lambda, coefficients, cv_results
#' @export
fit_regularised <- function(y, X, alpha = 1, nfolds = 10L) {
  type <- dplyr::case_when(
    alpha == 0 ~ "Ridge",
    alpha == 1 ~ "Lasso",
    TRUE       ~ glue("Elastic Net (α={alpha})")
  )
  log_info("[regularised] Fitting {type} with {nfolds}-fold CV")

  cv_fit <- glmnet::cv.glmnet(
    x       = as.matrix(X),
    y       = y,
    alpha   = alpha,
    nfolds  = nfolds,
    standardize = TRUE
  )

  coefs <- coef(cv_fit, s = "lambda.min")

  list(
    model          = cv_fit,
    type           = type,
    alpha          = alpha,
    lambda_min     = cv_fit$lambda.min,
    lambda_1se     = cv_fit$lambda.1se,
    coefficients   = tibble::tibble(
      term     = rownames(coefs),
      estimate = as.numeric(coefs)
    ) |> dplyr::filter(estimate != 0)
  )
}
