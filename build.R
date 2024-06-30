source('config.R')

##### Build blog posts for meetups #############################################
meetups <- readxl::read_excel('Schedule.xlsx', sheet = 'Meetups')
for(i in 1:nrow(meetups)) {
	if(!is.na(meetups[i,]$Slides) | !is.na(meetups[i,]$Youtube)) {
		blogfile <- paste0('website/posts/', as.Date(meetups[i,]$Date), '.Rmd')
		blogpath <- paste0('/blog/', as.Date(meetups[i,]$Date), '/')
		
		blogcontent <- ''
		if(!is.na(meetups[i,]$Slides)) {
			blogcontent <- paste0(blogcontent, '[Click here](/slides/', meetups[i,]$Slides, '.html#1) to open the slides ([PDF](/slides/', meetups[i,]$Slides, '.pdf)).\n\n')
			blogfile <- paste0('website/content/blog/', meetups[i,]$Slides, '.Rmd')
		}
		if(!is.na(meetups[i,]$Youtube)) {
			blogcontent <- paste0(blogcontent, '<iframe width="560" height="315" src="https://www.youtube.com/embed/', meetups[i,]$Youtube, '" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
		}
		
		additionalcontent <- ''
		if(!is.na(meetups[i,]$Resources)) {
			additionalcontent <- meetups[i,]$Resources
		}
		
		pubdate <- as.character(min(as.Date(meetups[i,]$Date), Sys.Date()))
		
		cat('---', '\n',
			'title: "', meetups[i,]$Topic, '"\n',
			'author: "Jason Bryer and Angela Lui"', '\n',
			'date: ', pubdate, '\n',
			'draft: false', '\n',
			'categories: ["Meetups"]', '\n',
			'tags: ["Annoucement"]', '\n',
			'---', '\n\n\n',
			blogcontent, '\n\n',
			'<!--more-->\n\n',
			blogcontent, '\n\n',
			additionalcontent, '\n\n',
			sep  = '',
			file = blogfile)
	}
}

#### Build website #############################################################
quarto::quarto_render('website/', as_job = FALSE)


##### Copy slides and build PDF versions #######################################
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