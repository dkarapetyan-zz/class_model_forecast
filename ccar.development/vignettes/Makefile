# Makefile to use knitr for package vignettes

# put all PDF targets here, separated by spaces
PDFS= ccar.development-intro.pdf ccar.development-todo.pdf

all: $(PDFS) 

clean:
	rm -rf *.tex *.bbl *.blg *.aux *.out *.toc *.log *.spl *tikzDictionary *.md figure/

%.pdf: %.Rnw
	$(R_HOME)/bin/Rscript -e "tools::texi2pdf('$*.tex')"
