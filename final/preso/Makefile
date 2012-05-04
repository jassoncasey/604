all: steve.pdf

clean:
	-@rm steve.aux steve.log steve.nav steve.out steve.pdf steve.snm steve.toc 

%.pdf:%.tex
	pdflatex $^
	pdflatex $^
