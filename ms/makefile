TEXT = ms
REFS = refs
all: $(TEXT).pdf

%.pdf: %.Rmd $(REFS).bib ecology.csl
	Rscript -e "rmarkdown::render('$<')"
	
continuous:
	while true; do make --silent; sleep .25; done
