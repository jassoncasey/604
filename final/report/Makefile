TEX_COMP=pdflatex
BIB_COMP=bibtex

TARGET=steve
REF_DB=references.bib
VPATH=./figs

SRC=$(wildcard *.tex)

all: $(TARGET).bbl
	pdflatex $(TARGET).tex
	pdflatex $(TARGET).tex

$(TARGET).bbl : $(REF_DB) $(SRC)
	$(TEX_COMP) $(TARGET).tex
	$(BIB_COMP) $(TARGET)

clean:
	rm -f *.pdf
	rm -f *.bbl
	rm -f *.blg
	rm -f *.log
	rm -f *.aux
