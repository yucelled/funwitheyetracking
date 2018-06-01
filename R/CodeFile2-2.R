library(readr)
library(tidyverse)
library(magick)

#Read data
fixations_all <- read_delim("Fixations_all2.csv", ";", escape_double = FALSE, trim_ws = TRUE)

#Get all unique images
image_list <- unique(fixations_all$image)

#Read data, filter by image and get two variables which you'll need to create heatmaps
find_fixations <- function(all_fixations, image_name) {
	all_fixations %>%  filter(image == image_name) %>% 
		mutate(mean_y = (-1)*mean_y) %>% 
		select(mean_x, mean_y)
}


#Create a heat map without axis elements and background, because I'll blend this plot with real image
create_and_save_plot <- function(data, filename) {
	plot_clean <- ggplot(data = data, aes(mean_x, mean_y)) + 
	  geom_bin2d(drop = FALSE, bins = c(40, 30)) +
		scale_fill_gradient(low="yellow", high="red") +
		theme(axis.line=element_blank(),
        plot.margin = rep(unit(0, 'null'), 4),
        panel.spacing = unit(0, 'null'),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
	scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))

	filepath <- paste("", filename, sep="")
	
	#Sava plot in write format based on the stimuli width and height (800X600)
	ggsave(plot = plot_clean, width = 8, height = 6, units = "in", dpi = 100,
       filename = filepath, device = "png")
	
    filepath
}


get_image <- function(filename=NULL, filepath=NULL) {
	full_path <- ""
	if (!is.null(filename)) {
		full_path <- paste("stimuli/", filename, ".jpg", sep="")
	} else {
		full_path <- filepath
	}
	image_read(full_path)
}

#Get the plot as image
get_plot_as_image <- function(filename=NULL, filepath=NULL) {
	full_path <- ""
	if (!is.null(filename)) {
		full_path <- paste("", filename, ".png", sep="")
	} else {
		full_path <- filepath
	}
	image_read(full_path)
}