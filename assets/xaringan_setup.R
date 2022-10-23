# Setup chunk for presentations

options(rmarkdown.paste_image_dir = 'img')

# output options
options(width = 90, scipen = 6, digits = 4)

library(knitr)
knitr::opts_template$set(
  fig.full = list(fig.width = 12, fig.height = 9),
  fig.large = list(fig.width = 12, fig.height = 7.5),
  fig.wide.tall = list(fig.width = 12, fig.height = 8),
  fig.wide.taller = list(fig.width = 12, fig.height = 6),
  fig.wide = list(fig.width = 12, fig.height = 4.5),
  fig.wider.tall = list(fig.width = 8, fig.height = 8),
  fig.wider.taller = list(fig.width = 8, fig.height = 6),
  fig.wider = list(fig.width = 8, fig.height = 4.5),
  fig.medium.tall = list(fig.width = 6, fig.height = 8),
  fig.medium.taller = list(fig.width = 6, fig.height = 6),
  fig.medium = list(fig.width = 6, fig.height = 4.5),
  fig.wider.small = list(fig.width = 8, fig.height = 4),
  fig.small = list(fig.width = 6, fig.height = 4),
  fig.wider.smaller = list(fig.width = 8, fig.height = 3.5),
  fig.smaller = list(fig.width = 6, fig.height = 3.5)
)
# chunk default options
opts_chunk$set(message = FALSE, tidy = FALSE, warning = FALSE, comment = "", opts.label='fig.medium', fig.showtext = TRUE, echo=TRUE)

library("xaringanthemer")
style_duo_accent(
  # primary_color = "#9B3C17",
  # secondary_color = "#649015",
  primary_color = "#9B3C17",
  secondary_color = "#18a780",
  base_font_size = "22px",
  text_font_size = "1rem",
  header_h1_font_size = "2.2rem",
  header_h2_font_size = "1.7rem",
  header_h3_font_size = "1.5rem",
  header_font_google = google_font("Arsenal", languages = c("latin", "cyrillic", "greek")),
  header_font_family_fallback = "Georgia, serif",
  text_font_google   = google_font("Nunito", "400", "400i", languages = c("latin", "cyrillic")),
  text_font_family_fallback = "Calibri, sans-serif",
  code_font_google   = google_font("Ubuntu Mono", languages = c("latin", "cyrillic")),
  code_font_family_fallback = "Courier New, monospace",
  outfile = "assets/xaringan-themer.css"
)

library("kableExtra")
options(knitr.kable.NA = '')
library("showtext")

