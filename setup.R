install.packages(c("devtools", "tidyverse", "knitr", "likert", "tm", "SnowballC",
				   "wordcloud", "RColorBrewer", "reshape2", "latex2exp", "psych",
				   "icons", "cowplot", "rmarkdown", "qrcode", "sysfonts",
				   "tidymodels", "ggfortify",
				   "hexSticker", "showtext"))
install.packages('pdftools')

remotes::install_github('jbryer/DATA606')
remotes::install_github('jbryer/VisualStats')
remotes::install_github("ropenscilabs/icon")
remotes::install_github("gadenbuie/countdown", subdir = "r")
icons::download_fontawesome()
remotes::install_github("thomasp85/patchwork")
remotes::install_github("gadenbuie/ggweekly")
remotes::install_github("jhelvy/renderthis")
remotes::install_github('rstudio/chromote')

source('config.R')

##### Course Logo ##############################################################
library(hexSticker)
library(showtext)
library(ggplot2)

course <- 'DATA 606'
url <- paste0('https://', tolower(semester), year, '.data606.net')

sysfonts::font_add_google(name = "Open Sans", family = "opensans")

# CUNY Blue: 005DAC
# CUNY Orange: F99D32

# Use Logo
# https://github.com/allisonhorst/stats-illustrations
p <- "website/static/images/CUNY_SPS_Logo.png"
# p <- "website/static/images/CUNY_SPS_Logo_Wide.png"
p <- 'website/static/images/cupcake.png'

# Or ggplot2
color2 <- '#C571FC'
color1 <- '#FF9822'
color3 <- '#1F5CFF'
bg.color <- '#FFFFFF'

# Use Logo
# https://github.com/allisonhorst/stats-illustrations
# p <- "website/static/images/CUNY_SPS_Logo.png"
# # p <- "website/static/images/CUNY_SPS_Logo_Wide.png"
# p <- 'website/static/images/cupcake.png'

# Or ggplot2
# p <- ggplot(data = mtcars, aes(x = -1 * mpg, y = wt)) +
# 	geom_point(size = 1, color = color2, alpha = 0.75) +
# 	geom_smooth(formula = y ~ x, method = loess, size = 0.75, color = color1, se = FALSE) +
# 	# geom_smooth(formula = y ~ x, se = FALSE, method = lm, size = .5, color = color1) +
# 	theme_void() + theme_transparent()
# p

data(penguins, package = 'palmerpenguins')
p <- ggplot(data = penguins, aes(x = flipper_length_mm, y = bill_length_mm, color = species, shape = species)) +
	geom_point(size = .25) +
	geom_smooth(method = lm, se = FALSE, formula = y ~ x, size = 0.75) +
	theme_void() +
	scale_color_manual(values = c('#1F5CFF', '#FF9822', '#C571FC')) +
	theme(legend.position = 'none')
p

out_file <- paste0(sub(' ', '', course), '-', sub(' ', '', semester), '.png')
sticker <- sticker(p,
				   package = paste0(course, ' ', semester),
				   p_x = 1, p_y = 1.45,
				   p_size = 12,
				   p_color = color3,
				   p_family = 'opensans',
				   white_around_sticker = FALSE,
				   s_x = 1, s_y = .85, s_width=1.5, s_height = 1,
				   h_color = color3,
				   h_fill = bg.color,
				   spotlight = FALSE,
				   url = url,
				   u_size = 4.9,
				   u_color = color2,
				   filename = out_file )

sticker

# Save a square version to use as an icon for Slack
ggsave(filename = paste0(sub(' ', '', course), '-', sub(' ', '', semester), '-square.png'),
	   plot = sticker,
	   width = 50.8, height = 50.8, units = 'mm', bg = 'transparent', dpi = 300)

# Copy file for use on the website
file.copy(out_file,
		  'website/images/course_logo.png',
		  overwrite = TRUE)

# Copy file for use on the slides
file.copy(out_file,
		  'slides/images/hex/DATA606.png',
		  overwrite = TRUE)

# Save Website icons
# Can create site favicon here using the square output: https://favicon.io/favicon-converter/
# To create a w x h image, use this formula: w * 300 / 600
# ggsave(filename = 'website/images/apple-touch-icon.png',
# 	   plot = sticker,
# 	   width = 50.8, height = 50.8, units = 'mm', dpi = 90)
# 
# ggsave(filename = 'website/images/android-chrome-192x192.png',
# 	   plot = sticker,
# 	   width = 50.8, height = 50.8, units = 'mm', dpi = 96)
# 
# ggsave(filename = 'website/images/android-chrome-512x512.png',
# 	   plot = sticker,
# 	   width = 50.8, height = 50.8, units = 'mm', dpi = 256)
# 
# # NOTE: The favicon.io seems to do a better job creating the small images
# ggsave(filename = 'website/images/favicon-16x16.png',
# 	   plot = sticker,
# 	   width = 50.8, height = 50.8, units = 'mm', dpi = 8)
# 
# ggsave(filename = 'website/images/favicon-32x32.png',
# 	   plot = sticker,
# 	   width = 50.8, height = 50.8, units = 'mm', dpi = 16)
