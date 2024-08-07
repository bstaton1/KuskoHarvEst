# location of figure files
figs_path = "man/figures/sticker"
logo = file.path(figs_path, "KuskoHarvEst-subplot.png")

# create the sticker image
s = hexSticker::sticker(
  logo, package = "KuskoHarvEst",
  p_size = 19,
  p_color = "#505050",
  p_x = 1,
  p_y = 1.4,
  s_x = 0.975,
  s_y = 0.825,
  s_width = 0.8,
  h_fill = "#E8E8E8",
  h_color = "#505050",
  h_size = 0.75,
  url = "github.com/bstaton1/KuskoHarvEst",
  u_x = 0.97,
  u_y = 0.05,
  u_size = 4.75,
  u_color = "#505050",
  filename = file.path(figs_path, "KuskoHarvEst-logo.png"))


file.show(file.path(figs_path, "KuskoHarvEst-logo.png"))

