PDFLATEX := pdflatex

VPATH := ch1

.PHONY: all clean

SECTIONS := 1.1 1.2 1.3
PDFDIR := pdf
PDF := $(addprefix $(PDFDIR)/s, $(addsuffix .pdf, $(SECTIONS)))

all: $(PDF)

$(PDFDIR)/s1.1.pdf: ex1.3.scm ex1.7.scm ex1.8.scm

$(PDFDIR)/s1.2.pdf: ex1.11.scm ex1.12.scm ex1.16.scm ex1.17.scm ex1.18.scm \
	ex1.19.scm ex1.22.scm ex1.23.scm ex1.24.scm ex1.25.scm ex1.27.scm ex1.28.scm

$(PDFDIR)/s1.3.pdf: ex1.29.scm ex1.30.scm ex1.31.scm

# All documents use a common style package
$(PDF): mystyle.sty

$(PDFDIR)/%.pdf: %.tex
	$(PDFLATEX) -output-directory $(PDFDIR) $<

clean:
	$(RM) pdf/*.{pdf,log,aux}
