#### Build website
quarto::quarto_render('website/', as_job = FALSE)


##### Copy slides and build PDF versions
tocopy <- c(list.files('Slides', pattern = '.html'),
			list.dirs('Slides', recursive = FALSE, full.names = FALSE))
ignore <- c('draft')
for(i in tocopy) {
	from <- paste0('Slides/', i)
	to <- paste0('docs/slides/', i)
	cat(paste0('Copying ', from, ' to ', to, '...\n'))
	
	success <- FALSE
	if(i %in% ignore) {
		cat(paste0('Ignoring ', i, '...\n'))
		success <- TRUE
	} else if(!file_test("-f", from)) { # Directory
		dir.create(to, recursive = TRUE, showWarnings = FALSE)
		success <- file.copy(from, 'docs/slides/', recursive = TRUE, overwrite = TRUE)
	} else { # File
		success <- file.copy(from, to, overwrite = TRUE)
	}
	if(!success) {
		cat(paste0('ERROR: ', i, ' did not copy!\n'))
	}
	
	if(tolower(tools::file_ext(from)) == 'html') {
		pdf <- paste0(tools::file_path_sans_ext(from), '.pdf')
		
		build_pdf <- !file.exists(pdf) | file.info(from)$mtime > file.info(pdf)$mtime
		
		if(build_pdf) {
			wd <- setwd('Slides/')
			tryCatch({
				renderthis::to_pdf(i,
								   complex_slides = TRUE,
								   partial_slides = FALSE)
			}, error = function(e) {
				cat(paste0('Error generating PDF from ', from))
				print(e)
			}, finally = { setwd(wd) })
		}
		
		if(file.exists(pdf)) { # Copy PDF to docs directory
			file.copy(pdf, paste0('docs/slides/', basename(pdf)))
		}
	}
}

if(FALSE) {
	httpuv::runStaticServer("docs/", port = 2112, background = TRUE, browse = FALSE)
	browseURL(paste0('http://localhost:2112/'))
	httpuv::stopAllServers()
}