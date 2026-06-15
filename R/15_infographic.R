# ----------------------------------------------------------
# 15_infographic.R
# Project: When Financial Markets Catch a Cold
# Purpose: Trading-hour overlap infographic —
#          world map, Gantt chart, bilateral snapshot
# Output:  output/plots/overlap_infographic.png
#          output/plots/world_market_sessions_map.png
# ----------------------------------------------------------

required_packages <- c(
  "ggplot2", "sf", "rnaturalearth", "geosphere",
  "rnaturalearthdata", "patchwork", "ggrepel", "gridExtra", "dplyr"
)
new_pkgs <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(required_packages, library, character.only = TRUE))
cat("Packages loaded.\n")

# ── Ensure save_plot is available if running standalone ──────────────────
if (!exists("save_plot")) source("config/config.R")

# ── Colour palette ────────────────────────────────────────────────────────
col_bg       <- "#ffffff"
col_asia     <- "#1a6b9e"
col_europe   <- "#2d7a3a"
col_americas <- "#b86a1a"
col_text     <- "#1a1a2e"
col_accent   <- "#1a5c8a"
col_grid     <- "#e0e4ea"

# ── Market data ───────────────────────────────────────────────────────────
markets <- tibble(
  name    = c("Australia","Japan","Hong Kong","India",
              "UK","Germany","France",
              "Brazil","United States","Canada"),
  code    = c("AU","JP","HK","IN","UK","DE","FR","BR","US","CA"),
  lon     = c(151,139.7,114,77,-0.1,13.4,2.35,-46,-74,-79),
  lat     = c(-34,36,22,28,51.5,52.5,48.85,-23,40.7,43.7),
  session = c("Asia","Asia","Asia","Asia",
              "Europe","Europe","Europe",
              "Americas","Americas","Americas"),
  open_utc  = c( 0.0,  0.0,  1.5,  3.75,  8.0,  8.0,  8.0, 13.0, 14.5, 14.5),
  close_utc = c( 6.0,  6.5,  8.0, 10.0,  16.5, 16.5, 16.5, 21.0, 21.0, 21.0),
  label = c(
    "Australia\n23:00\u201305:00 (+1)",
    "Japan\n00:00\u201306:30",
    "Hong Kong\n01:30\u201308:00",
    "India\n03:45\u201310:00",
    "UK\n08:00\u201316:30",
    "Germany\n08:00\u201316:30",
    "France\n08:00\u201316:30",
    "Brazil\n13:00\u201321:00",
    "United States\n14:30\u201321:00",
    "Canada\n14:30\u201321:00"
  )
)

session_colors <- c("Asia" = "#1a6b9e", "Europe" = "#2d7a3a", "Americas" = "#b86a1a")

# ── World map data ────────────────────────────────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(
    session_shade = case_when(
      name %in% c("Australia","Japan","India")         ~ "Asia",
      sovereignt == "Hong Kong S.A.R."                 ~ "Asia",
      name %in% c("United Kingdom","Germany","France") ~ "Europe",
      name %in% c("Brazil","Canada","United States of America") ~ "Americas",
      TRUE ~ "Other"
    )
  )

# ── Great circle arcs ─────────────────────────────────────────────────────
arc_pairs <- list(
  c("JP","HK"), c("JP","IN"), c("HK","IN"),
  c("UK","DE"), c("UK","FR"), c("DE","FR"),
  c("US","CA"), c("US","BR"), c("CA","BR"),
  c("UK","US"), c("DE","US"), c("FR","US")
)

arcs <- map_dfr(arc_pairs, function(pair) {
  m1 <- markets %>% filter(code == pair[1])
  m2 <- markets %>% filter(code == pair[2])
  pts <- gcIntermediate(
    c(m1$lon, m1$lat), c(m2$lon, m2$lat),
    n = 50, addStartEnd = TRUE, sp = FALSE
  ) %>% as.data.frame()
  pts$session <- m1$session
  pts$pair    <- paste(pair, collapse = "-")
  pts
})

# ── PANEL 1: World map ────────────────────────────────────────────────────
p_map <- ggplot() +
  geom_sf(data = world, aes(fill = session_shade),
          colour = alpha("white", 0.7), linewidth = 0.15) +
  scale_fill_manual(
    values = c(Asia="#cfe0f2", Europe="#d0e3d0",
               Americas="#ead9c6", Other="#c8d2dc"),
    guide = "none"
  ) +
  geom_path(data = arcs,
            aes(x=lon, y=lat, colour=session,
                group=interaction(session,lon)),
            linewidth=0.5, alpha=0.7) +
  geom_point(data=markets, aes(x=lon, y=lat, colour=session), size=3.5) +
  geom_text_repel(data=markets,
                  aes(x=lon, y=lat, label=label, colour=session),
                  size=2.2, lineheight=0.85, segment.size=0.3,
                  box.padding=0.3, max.overlaps=20) +
  annotate("label", x=125, y=68,
           label="ASIA-PACIFIC SESSION\n00:00 - 09:00 UTC",
           fill=alpha(col_asia,0.8), colour="white", size=2.8, fontface="bold") +
  annotate("label", x=20, y=68,
           label="EUROPE SESSION\n08:00 - 16:30 UTC",
           fill=alpha(col_europe,0.8), colour="white", size=2.8, fontface="bold") +
  annotate("label", x=-60, y=68,
           label="AMERICAS SESSION\n13:00 - 21:00 UTC",
           fill=alpha(col_americas,0.8), colour="white", size=2.8, fontface="bold") +
  scale_colour_manual(values=session_colors, guide="none") +
  coord_sf(xlim=c(-200,200), ylim=c(-55,80), expand=FALSE) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill=col_bg, colour=NA),
    panel.background = element_rect(fill="#f5f7fa", colour=NA)
  )

# ── PANEL 2: Gantt chart ──────────────────────────────────────────────────
gantt_data <- markets %>%
  mutate(name_order = factor(name, levels=rev(name))) %>%
  select(name_order, open_utc, close_utc, session)

p_gantt <- ggplot(gantt_data) +
  geom_rect(aes(xmin=open_utc, xmax=close_utc,
                ymin=as.numeric(name_order)-0.35,
                ymax=as.numeric(name_order)+0.35,
                fill=session), color=NA, alpha=0.85) +
  scale_x_continuous(breaks=seq(0,24,2),
                     labels=sprintf("%02d:00",seq(0,24,2)),
                     limits=c(0,24), expand=c(0,0)) +
  scale_y_continuous(breaks=1:10,
                     labels=rev(paste0(markets$name," (",markets$code,")")),
                     expand=c(0.05,0.05)) +
  scale_fill_manual(values=session_colors, guide="none") +
  labs(x=NULL, y=NULL,
       title="TRADING HOURS AND OVERLAP THROUGH THE 24-HOUR DAY (UTC)") +
  theme_minimal(base_size=9) +
  theme(
    plot.background  = element_rect(fill=col_bg, color=NA),
    panel.background = element_rect(fill="#f5f7fa", color=NA),
    panel.grid.major = element_line(color=col_grid, linewidth=0.3),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color=col_text, size=7.5),
    axis.text.y      = element_text(hjust=1),
    plot.title       = element_text(color=col_accent, face="bold",
                                    size=8, hjust=0.5)
  )

# ── PANEL 3: Bilateral overlap snapshot ──────────────────────────────────
codes_order <- c("AU","JP","HK","IN","UK","DE","FR","BR","US","CA")

# Always load fresh from file to avoid environment conflicts
raw_overlap <- load_clean("trading_hour_overlap_matrix.csv")

overlap_full <- bind_rows(
  raw_overlap %>% rename(i = market_i, j = market_j, overlap = overlap_share),
  raw_overlap %>% rename(i = market_j, j = market_i, overlap = overlap_share)
) %>%
  bind_rows(tibble(i = codes_order, j = codes_order, overlap = NA_real_)) %>%
  mutate(
    i = factor(i, levels = codes_order),
    j = factor(j, levels = codes_order),
    overlap_cat = case_when(
      is.na(overlap)  ~ "self",
      overlap == 0    ~ "none",
      overlap <= 0.25 ~ "very_low",
      overlap <= 0.50 ~ "low",
      overlap <= 0.75 ~ "moderate",
      overlap <  1.00 ~ "high",
      overlap == 1.00 ~ "complete",
      TRUE            ~ "none"
    )
  )

overlap_lower <- overlap_full %>%
  mutate(i_num=as.numeric(i), j_num=as.numeric(j)) %>%
  filter(i_num > j_num) %>%
  bind_rows(overlap_full %>% filter(as.character(i) == as.character(j)))

cat_colors <- c(
  "none"     = "#cccccc",
  "very_low" = "#a8c8e8",
  "low"      = "#2e86c1",
  "moderate" = "#2d7a3a",
  "high"     = "#f0a500",
  "complete" = "#c85a1a"
)

p_snapshot <- ggplot(overlap_lower, aes(x=j, y=fct_rev(i))) +
  geom_tile(fill="#f5f7fa", color="white", linewidth=0.8) +
  geom_point(data=filter(overlap_lower, !is.na(overlap)),
             aes(colour=overlap_cat, size=overlap), shape=16) +
  geom_point(data=filter(overlap_lower, is.na(overlap)),
             shape=1, size=3, colour="#cccccc", stroke=1) +
  scale_size_continuous(range=c(1.5,10), limits=c(0,1), guide="none") +
  scale_colour_manual(
    values=cat_colors, name=NULL, drop=FALSE,
    breaks=c("none","very_low","low","moderate","high","complete"),
    labels=c("0 (None)","0\u20130.25 (Very Low)","0.25\u20130.50 (Low)",
             "0.50\u20130.75 (Moderate)","0.75\u20131.00 (High)","1.00 (Complete)")
  ) +
  guides(colour=guide_legend(override.aes=list(size=5), ncol=1)) +
  labs(title="BILATERAL OVERLAP SNAPSHOT",
       subtitle="(Share of Trading Day Overlapping)", x=NULL, y=NULL) +
  theme_minimal(base_size=8) +
  theme(
    plot.background  = element_rect(fill="#f5f7fa", color="#dddddd", linewidth=0.5),
    panel.background = element_rect(fill="#f5f7fa", color=NA),
    panel.grid       = element_blank(),
    axis.text.x      = element_text(
      color=c(rep("#1a6b9e",4),rep("#2d7a3a",3),rep("#b86a1a",3)),
      size=7, face="bold"),
    axis.text.y      = element_text(
      color=c(rep("#b86a1a",3),rep("#2d7a3a",3),rep("#1a6b9e",4)),
      size=7, face="bold"),
    plot.title       = element_text(color=col_accent, face="bold", size=8, hjust=0.5),
    plot.subtitle    = element_text(color=col_text, size=7, hjust=0.5),
    legend.text      = element_text(color=col_text, size=6.5),
    legend.position  = "right",
    legend.key.size  = unit(0.5,"cm")
  )

# ── KEY INSIGHTS ──────────────────────────────────────────────────────────
p_insights <- ggplot() +
  annotate("text", x=0.05, y=0.88,
           label="KEY INSIGHTS", colour=col_accent,
           size=3.8, hjust=0, fontface="bold") +
  annotate("text", x=0.05, y=0.65,
           label="\u25cf  Trading-hour overlap creates\n    opportunities for real-time\n    transmission of shocks.",
           colour=col_text, size=2.8, hjust=0, lineheight=1.2) +
  annotate("text", x=0.05, y=0.44,
           label="\u25cf  Brazil overlaps almost completely\n    with North America (0.897)\n    but not with Asia.",
           colour=col_text, size=2.8, hjust=0, lineheight=1.2) +
  annotate("text", x=0.05, y=0.25,
           label="\u25cf  India overlaps with Asia but\n    not with the Western Hemisphere.",
           colour=col_text, size=2.8, hjust=0, lineheight=1.2) +
  annotate("text", x=0.05, y=0.10,
           label="\u25cf  These structural differences\n    provide identification.",
           colour=col_text, size=2.8, hjust=0, lineheight=1.2) +
  xlim(0,1) + ylim(0,1) +
  theme_void() +
  theme(plot.background=element_rect(fill="#f0f6ff", colour="#2d7a3a", linewidth=0.5))

# ── EXAMPLES bar ──────────────────────────────────────────────────────────
examples_data <- tibble(
  label = c("High overlap\n(JP \u2013 HK)\n~4.5 hours",
            "Complete overlap\n(UK \u2013 DE \u2013 FR)\n~7.5-8.5 hours",
            "Near-complete\n(BR \u2013 US \u2013 CA)\n~6.5-7.0 hours",
            "Zero overlap\n(IN \u2013 US)\n0 hours",
            "Zero overlap\n(BR \u2013 JP/HK)\n0 hours"),
  color = c(col_asia, col_europe, col_americas, "#555577", "#553355"),
  x = 1:5
)

p_examples <- ggplot(examples_data) +
  geom_rect(aes(xmin=x-0.4, xmax=x+0.4, ymin=0, ymax=1, fill=color), color=NA) +
  geom_text(aes(x=x, y=0.5, label=label), color="white", size=2.2,
            lineheight=0.9, fontface="bold") +
  scale_fill_identity() +
  annotate("text", x=0.2, y=0.5, label="EXAMPLES\nOF OVERLAP",
           color=col_text, size=2.5, fontface="bold", hjust=0) +
  xlim(-0.2, 5.7) + ylim(-0.1, 1.1) +
  theme_void() +
  theme(plot.background=element_rect(fill="#ffffff", color=NA))

# ── FOOTNOTE ─────────────────────────────────────────────────────────────
p_foot <- ggplot() +
  annotate("text", x=0.02, y=0.5,
           label=paste0(
             "\u2605  This figure illustrates the structural timing of the 10 equity markets ",
             "in our sample. Variation in trading-hour overlap\u2014driven by time zones and ",
             "market schedules\u2014creates\n   exogenous differences in real-time interaction, ",
             "which our empirical strategy exploits to identify the causal effect of ",
             "simultaneity on financial contagion."
           ),
           color=alpha(col_text,0.75), size=2.5, hjust=0, vjust=0.5, lineheight=1.2) +
  xlim(0,1) + ylim(0,1) +
  theme_void() +
  theme(plot.background=element_rect(fill="#ffffff", color=NA))

# ── TITLE ─────────────────────────────────────────────────────────────────
p_title <- ggplot() +
  annotate("text", x=0.5, y=0.75,
           label="WHEN MARKETS OVERLAP, CONTAGION CAN SPREAD",
           color=col_accent, size=9, fontface="bold", hjust=0.5) +
  annotate("text", x=0.5, y=0.35,
           label="Trading-Hour Overlap Across 10 Major Financial Centres (UTC)",
           color=col_accent, size=5, hjust=0.5, fontface="italic") +
  annotate("label", x = 0.5, y = 0.08,
           label = "\u23F0  Time is a channel: Markets open together, move together.",
           color = col_text, fill = "#e8f0f8", size = 3.5, hjust = 0.5,
           label.padding = unit(0.4, "lines"), label.r = unit(0.3, "lines"),
           colour = col_accent) +
  xlim(0,1) + ylim(0,1) +
  theme_void() +
  theme(plot.background=element_rect(fill=col_bg, color=NA))

# ── ASSEMBLE ──────────────────────────────────────────────────────────────
bottom_row <- p_gantt + p_snapshot + plot_layout(widths=c(0.62, 0.38))
middle_row <- p_map   + p_insights + plot_layout(widths=c(0.72, 0.28))

full_layout <- (p_title / middle_row / bottom_row / p_examples / p_foot) +
  plot_layout(heights=c(0.12, 0.42, 0.36, 0.06, 0.04)) &
  theme(plot.background=element_rect(fill=col_bg, color=NA))

# ── SAVE ──────────────────────────────────────────────────────────────────
save_plot(p_map,        "world_market_sessions_map.png", width=14, height=7)
save_plot(full_layout,  "overlap_infographic.png",       width=16, height=10)

message("✓ Saved world_market_sessions_map.png")
message("✓ Saved overlap_infographic.png")