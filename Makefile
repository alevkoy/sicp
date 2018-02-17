PDFLATEX := pdflatex

VPATH := ch1 ch2

.PHONY: all clean

SECTIONS := 1.1 1.2 1.3 2.1 2.2 2.3 2.4 2.5
PDFDIR := pdf
PDF := $(addprefix $(PDFDIR)/s, $(addsuffix .pdf, $(SECTIONS)))

all: $(PDF)

$(PDFDIR)/s1.1.pdf: ex1.3.scm ex1.7.scm ex1.8.scm

$(PDFDIR)/s1.2.pdf: ex1.11.scm ex1.12.scm ex1.16.scm ex1.17.scm ex1.18.scm \
	ex1.19.scm ex1.22.scm ex1.23.scm ex1.24.scm ex1.25.scm ex1.27.scm ex1.28.scm

$(PDFDIR)/s1.3.pdf: ex1.29.scm ex1.30.scm ex1.31.scm ex1.32.scm ex1.33.scm \
	ex1.35.scm ex1.36.scm ex1.37.scm ex1.38.scm ex1.39.scm ex1.40.scm \
	ex1.41.scm ex1.42.scm ex1.43.scm ex1.44.scm ex1.45.scm ex1.46.scm

$(PDFDIR)/s2.1.pdf: ex2.1.scm ex2.2.scm ex2.4.scm ex2.5.scm ex2.6.scm \
	ex2.7.scm

$(PDFDIR)/s2.2.pdf: ex2.17.scm ex2.18.scm ex2.19.scm ex2.20.scm ex2.21.scm \
	ex2.23.scm ex2.27.scm ex2.28.scm ex2.29.scm ex2.30.scm ex2.32.scm \
	ex2.33.scm ex2.37.scm ex2.39.scm ex2.40.scm ex2.42.scm ex2.44.scm

$(PDFDIR)/s2.3.pdf: ex2.54.scm ex2.56.scm ex2.59.scm ex2.60.scm ex2.61.scm \
	ex2.63.scm ex2.66.scm ex2.67.scm

$(PDFDIR)/s2.4.pdf: ex2.73.scm ex2.74.scm

$(PDFDIR)/s2.5.pdf: ex2.78.scm ex2.81.scm ex2.82.scm ex2.83.scm

# All documents use a common style package
$(PDF): mystyle.sty

$(PDFDIR)/%.pdf: %.tex
	$(PDFLATEX) -output-directory $(PDFDIR) $<

clean:
	$(RM) pdf/*.{pdf,log,aux}
