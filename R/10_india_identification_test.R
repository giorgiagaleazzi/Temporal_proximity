# ============================================================================
# 10_India TEST — Does IN–US behave like DE–FR (timing) or US–JP (distance)?
# ----------------------------------------------------------------------------
# Uses the same objects and terminology as the main analysis:
#   monthly_panel   : pair-month panel
#   corr_ij         : monthly bilateral correlation (dependent variable)
#   pair_id         : market-pair identifier
#   overlap_share   : normalised trading-hour overlap (time-invariant)
#   high_stress     : VIX > 80th percentile dummy
#   overlap_stress  : overlap_share * high_stress
#   n_days          : trading days per pair-month
# Estimation: fixest::feols, SEs clustered by pair_id (as in Tables 1–3)
# ============================================================================


# ----------------------------------------------------------------------------
# 0. Setup — identify the pairs of interest
# ----------------------------------------------------------------------------
# pair_id format assumed "XX_YY" (e.g. "IN_US"). If your format differs
# (e.g. "IN-US" or "US_IN"), adjust sep below — the matching is order-proof.

sep <- "_"   # <-- change to "-" if pair_id uses hyphens

make_pid <- function(a, b) paste(sort(c(a, b)), collapse = sep)

# Ensure pair_id is order-normalised the same way (skip if already true)
# monthly_panel <- monthly_panel |>
#   mutate(pair_id = vapply(strsplit(pair_id, sep),
#                           function(x) paste(sort(x), collapse = sep), ""))

india_pairs    <- c(make_pid("IN","US"), make_pid("IN","CA"))
timing_bench    <- c(make_pid("DE","FR"), make_pid("UK","DE"), make_pid("UK","FR"))
distance_bench  <- c(make_pid("US","JP"), make_pid("US","HK"))

key_pairs <- c(india_pairs, timing_bench, distance_bench)

stopifnot(all(key_pairs %in% monthly_panel$pair_id))  # fails fast if naming differs

# ----------------------------------------------------------------------------
# 1. Descriptive test — stress vs non-stress mean correlations, key pairs
# ----------------------------------------------------------------------------
desc_tab <- monthly_panel |>
  filter(pair_id %in% key_pairs) |>
  group_by(pair_id, high_stress) |>
  summarise(mean_corr = mean(corr_ij, na.rm = TRUE),
            n = dplyr::n(), .groups = "drop") |>
  tidyr::pivot_wider(names_from = high_stress,
                     values_from = c(mean_corr, n),
                     names_glue = "{.value}_{ifelse(high_stress==1,'stress','calm')}") |>
  mutate(stress_jump = mean_corr_stress - mean_corr_calm) |>
  left_join(monthly_panel |> distinct(pair_id, overlap_share), by = "pair_id") |>
  arrange(desc(overlap_share))

print(desc_tab)
# READ: if IN_US stress_jump ~ DE_FR jump  -> supports timing hypothesis
#       if IN_US stress_jump ~ US_JP jump  -> supports distance/integration story

# ----------------------------------------------------------------------------
# 2. Pair-specific stress jumps for ALL 45 pairs (conditional on month FE)
# ----------------------------------------------------------------------------
# i(pair_id, high_stress) gives each pair its own stress coefficient,
# net of global monthly shocks — the within-pair stress jump.

m_pairjump <- feols(corr_ij ~ i(pair_id, high_stress) | pair_id + month,
                    data    = monthly_panel,
                    cluster = ~pair_id)

jumps <- broom::tidy(m_pairjump) |>
  filter(grepl("pair_id::", term)) |>
  mutate(pair_id = gsub("pair_id::(.*):high_stress", "\\1", term)) |>
  select(pair_id, stress_jump_fe = estimate, se = std.error) |>
  left_join(monthly_panel |> distinct(pair_id, overlap_share), by = "pair_id") |>
  mutate(group = case_when(
    pair_id %in% india_pairs   ~ "india (high overlap, high distance)",
    pair_id %in% timing_bench   ~ "Europe (high overlap, low distance)",
    pair_id %in% distance_bench ~ "Cross-session (zero overlap)",
    TRUE                        ~ "Other pairs"))

# ----------------------------------------------------------------------------
# 3. The decisive figure — stress jump vs overlap, india highlighted
# ----------------------------------------------------------------------------
fig_india <- ggplot(jumps, aes(overlap_share, stress_jump_fe)) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey60", linewidth = .6) +
  geom_point(aes(colour = group, size = group != "Other pairs"), alpha = .85) +
  ggrepel::geom_text_repel(
    data = filter(jumps, group != "Other pairs"),
    aes(label = pair_id), size = 3.2, seed = 1) +
  scale_size_manual(values = c(2, 3.6), guide = "none") +
  labs(x = "Trading-hour overlap (overlap_share)",
       y = "Pair-specific stress jump in corr_ij (month FE absorbed)",
       colour = NULL,
       title = "Is the stress jump driven by timing? india as the decisive case",
       subtitle = " ") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(fig_india)
ggsave("fig_india_test.png", fig_india, width = 8, height = 5.5, dpi = 300)

save_plot(fig_india, "fig_india_identification_test.png", width=14, height=7)
message("✓ Saved fig_india_identification_test.png")

# ----------------------------------------------------------------------------
# 4. Formal decoupled-pairs estimate — does beta survive on india pairs?
# ----------------------------------------------------------------------------
# Interaction: does the overlap_stress effect differ for india pairs?
monthly_panel <- monthly_panel |>
  mutate(IN_pair = as.integer(pair_id %in% india_pairs))

m_decoupled <- feols(corr_ij ~ overlap_stress + overlap_stress:IN_pair |
                       pair_id + month,
                     data    = monthly_panel,
                     cluster = ~pair_id)

etable(m_decoupled)
# READ: overlap_stress            = effect for the rest of the sample
#       overlap_stress:IN_pair    = india differential
#       If the differential is ~0 and insignificant, india behaves exactly
#       as its overlap predicts -> timing channel confirmed where
#       geography and timing disagree.
#       If significantly negative (offsetting), the effect is weaker for
#       india -> caution: pooled beta may partly reflect integration.

# ----------------------------------------------------------------------------
# 5. (Optional) leave-india-out — does the pooled beta depend on india?
# ----------------------------------------------------------------------------
m_noIN <- feols(corr_ij ~ overlap_stress | pair_id + month,
                data    = monthly_panel |> filter(!grepl("IN", pair_id)),
                cluster = ~pair_id)
m_pairjump_baseline <- feols(
  corr_ij ~ overlap_stress | pair_id + month,
  data = monthly_panel,
  cluster = ~pair_id
)

etable(
  m_pairjump_baseline,
  m_noIN
)

# READ: if beta is stable, india is consistent with — but not solely
#       driving — the headline result. Both directions are informative.
# ============================================================================
