# Standardized Mean Difference ("Cohen's d") for paired and unpaired samples
# (in base R)

smd_unpaired = function(x, y) {
  n1 = length(x)
  n2 = length(y)
  nom = (n1 - 1) * (sd(x) ** 2) + (n2 - 1) * (sd(y) ** 2)
  sd_p = sqrt(nom / (n1 + n2 - 2))
  return((mean(x) - mean(y)) / sd_p)
}

smd_paired = function(x, y) {
  sdx = sd(x)
  sdy = sd(y)
  sd_p = sqrt((sdx ** 2 + sdy ** 2) - 2 * cor(x, y) * sdx * sdy)
  return((mean(x) - mean(y)) / sd_p)
}


# verification (via MASS via neatStats)

samps = faux::rnorm_multi(
  n = 100,
  vars = 2,
  mu = c(5, 0),
  sd = 10,
  r = .5
)

neatStats::t_neat(samps$X1, samps$X2)$stats['d']
smd_unpaired(samps$X1, samps$X2)

neatStats::t_neat(samps$X1, samps$X2, pair = TRUE)$stats['d']
smd_paired(samps$X1, samps$X2)
