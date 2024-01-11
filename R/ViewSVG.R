ViewSVG <- function(img, use_viewer = TRUE) {
	#' Opens an SVG or HTML file within an rstudio environment.
	#' 
	#' @param img path/filename of the file to be opened.
	#' @param use_viewer Use the rstudio viewer (set to FALSE to open a new tab)
	#' 
	#' @export
	file.copy(img, tempdir(), overwrite = TRUE)
	tmp <- tempfile(fileext = '.html')
	writeLines(sprintf('<img src="%s">', basename(img)), con = tmp)
	
	if (use_viewer)
		tryCatch(
			rstudioapi::viewer(tmp),
			error = function(e) browseURL(tmp)
		)
	else browseURL(tmp)
}