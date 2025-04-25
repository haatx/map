# ==========================================================
# Houston ZIP Bivariate Choropleth   (run whole file)
# ==========================================================
# 0) load required packages --------------------------------
library(sf)
library(readr)
library(dplyr)
library(classInt)
library(ggplot2)
library(cowplot)
library(ggtext)

# 1) read spatial & population data ------------------------------------
zips <- st_read("houston_districts_zips_cleaned.geojson", quiet = TRUE) %>%
  filter(layer == "zip_code") %>%
  mutate(zip = stringr::str_pad(substr(ZCTA, 1, 5), 5, pad = "0"))

pop  <- jsonlite::fromJSON("zipPopulations.json") %>%
  as_tibble() %>%
  transmute(zip = stringr::str_pad(as.character(zip), 5, pad = "0"),
            population = as.numeric(population))

# 2) read grants CSV and count grantees per ZIP ------------------------
grants_raw <- read_csv("CY24GrantsData.csv", show_col_types = FALSE)

grants_tbl <- grants_raw %>%
  mutate(zip = stringr::str_extract(ZipCode, "\\d{5}")) %>%  # change ZipCode if header differs
  filter(!is.na(zip)) %>%
  count(zip, name = "grantees")

# 3) join everything & calculate grants per 10 k pop -------------------
map_df <- zips %>%
  left_join(pop,        by = "zip") %>%
  left_join(grants_tbl, by = "zip") %>%
  mutate(
    grantees         = tidyr::replace_na(grantees, 0),
    grantees_per_10k = ifelse(!is.na(population) & population > 0,
                              grantees / (population / 10000), NA_real_)
  ) %>%
  filter(!is.na(population) & !is.na(grantees_per_10k)) %>%
  { stopifnot(nrow(.) > 0); . }                      # fails early if 0 rows


# 4) three-quantile bins for pop & grants ------------------
quant3 <- function(x) as.factor(classIntervals(x, 3, "quantile")$class)

map_df <- map_df |>
  mutate(
    pop_bin   = quant3(population),
    grant_bin = quant3(grantees_per_10k),
    bi_class  = paste0(pop_bin, "-", grant_bin)
  )

# 5) bivariate palette (Joshua Stevens) --------------------
bi_pal <- c(
  "3-3"="#3F2949","2-3"="#435786","1-3"="#4885C1",
  "3-2"="#77324C","2-2"="#806A8A","1-2"="#89A1C8",
  "3-1"="#AE3A4E","2-1"="#BC7C8F","1-1"="#CABED0"
)

# 6) make the map ------------------------------------------
map_plot <- ggplot(map_df) +
  geom_sf(aes(fill = bi_class), color = "white", size = 0.15) +
  scale_fill_manual(values = bi_pal, drop = FALSE) +
  coord_sf() +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_markdown(size = 10, margin = margin(b = 10)),
    legend.position = "none"
  ) +
  labs(
    title    = "Houston Arts Grantees & Population (Bivariate Choropleth)",
    subtitle = "Population (lighter → darker)  ×  Grantees per 10 k residents (blue → purple)"
  )

# 7) build 3×3 legend --------------------------------------
legend_df <- expand.grid(pop_bin = 1:3, grant_bin = 1:3) |>
  mutate(bi_class = paste0(pop_bin, "-", grant_bin))

legend_plot <- ggplot(legend_df, aes(pop_bin, grant_bin)) +
  geom_tile(aes(fill = bi_class)) +
  scale_fill_manual(values = bi_pal, guide = "none") +
  scale_x_continuous(breaks = 1:3, labels = c("Low", "", "High"), expand = c(0,0)) +
  scale_y_reverse(breaks = 1:3, labels = c("High", "", "Low"), expand = c(0,0)) +
  theme_void() +
  theme(axis.text = element_text(size = 7)) +
  labs(x = "Population", y = "Grantees /10k")

# 8) combine map + legend, export ---------------------------
final <- cowplot::ggdraw() +
  draw_plot(map_plot, 0, 0, 1, 1) +
  draw_plot(legend_plot, 0.68, 0.05, 0.25, 0.25)

ggsave("houston_bivariate_map.pdf", final, width = 8, height = 10)
ggsave("houston_bivariate_map.png", final, width = 8, height = 10, dpi = 450)