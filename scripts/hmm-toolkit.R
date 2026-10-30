# =============================================================================
# HMM toolkit, written from first principles (Time Series Analysis portfolio)
#
# Used by projects/project-04-analysis.qmd and section-2/section-2.qmd.
# Every function works with the matrix of state-dependent densities
# dens[t, i] = p_i(x_t), so the same code handles Poisson, Gaussian and
# categorical observations.
# =============================================================================

# Log-likelihood only (fast version, used inside optim()):
hmm_loglik = function(dens, delta, Gamma) {
  T_len = nrow(dens)
  v = delta * dens[1, ]
  ll = log(sum(v))
  phi = v / sum(v)
  if (T_len > 1) {
    for (t in 2:T_len) {
      v = as.numeric(phi %*% Gamma) * dens[t, ]
      u = sum(v)
      ll = ll + log(u)
      phi = v / u
    }
  }
  ll
}

# Scaled forward algorithm: log-likelihood, log u_t, filtered probabilities and log alpha:
hmm_forward = function(dens, delta, Gamma) {
  T_len = nrow(dens)
  m = ncol(dens)
  phi_mat = matrix(NA, T_len, m)
  lu = numeric(T_len)
  for (t in 1:T_len) {
    v = if (t == 1) delta * dens[1, ] else as.numeric(phi %*% Gamma) * dens[t, ]
    u = sum(v)
    lu[t] = log(u)                # log p(x_t | x_{1:t-1})
    phi = v / u                   # P(C_t = i | x_{1:t})
    phi_mat[t, ] = phi
  }
  list(loglik = sum(lu), lu = lu, phi = phi_mat,
       lalpha = cumsum(lu) + log(phi_mat))   # log alpha_t(i)
}

# Scaled backward algorithm: log beta (Zucchini et al., 2016):
hmm_backward = function(dens, Gamma) {
  T_len = nrow(dens)
  m = ncol(dens)
  lbeta = matrix(0, T_len, m)     # log beta_T = 0
  phi = rep(1 / m, m)
  lscale = log(m)
  if (T_len > 1) {
    for (t in (T_len - 1):1) {
      v = as.numeric(Gamma %*% (dens[t + 1, ] * phi))
      lbeta[t, ] = log(v) + lscale
      phi = v / sum(v)
      lscale = lscale + log(sum(v))
    }
  }
  lbeta
}

# Local decoding: P(C_t = i | x_{1:T}) = alpha_t(i) beta_t(i) / L_T:
hmm_local = function(dens, delta, Gamma) {
  fwd = hmm_forward(dens, delta, Gamma)
  lbeta = hmm_backward(dens, Gamma)
  exp(fwd$lalpha + lbeta - fwd$loglik)
}

# Global decoding with the Viterbi algorithm (log scale):
hmm_viterbi = function(dens, delta, Gamma) {
  T_len = nrow(dens)
  m = ncol(dens)
  lG = log(Gamma)
  ld = log(dens)
  xi = matrix(NA, T_len, m)
  xi[1, ] = log(delta) + ld[1, ]
  if (T_len > 1) {
    for (t in 2:T_len) {
      for (j in 1:m) {
        xi[t, j] = max(xi[t - 1, ] + lG[, j]) + ld[t, j]
      }
    }
  }
  # Back-tracing:
  states = numeric(T_len)
  states[T_len] = which.max(xi[T_len, ])
  if (T_len > 1) {
    for (t in (T_len - 1):1) {
      states[t] = which.max(xi[t, ] + lG[, states[t + 1]])
    }
  }
  list(states = states, xi = xi)
}

# Stationary distribution: solve delta (I - Gamma + U) = 1:
stat_dist = function(Gamma) {
  m = nrow(Gamma)
  solve(t(diag(m) - Gamma + 1), rep(1, m))
}

# Working (unconstrained) parameters -> transition probability matrix:
gamma_w2n = function(w, m) {
  G = diag(m)
  G[!G] = exp(w)                  # off-diagonal elements, relative to the diagonal
  G / rowSums(G)
}

# h-step transition matrix Gamma^h:
mat_pow = function(M, h) {
  R = diag(nrow(M))
  for (i in seq_len(h)) R = R %*% M
  R
}
