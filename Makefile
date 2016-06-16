PDFLATEX := pdflatex

VPATH := ch1

.PHONY: all clean

SECTIONS := 1.1 1.2
PDFDIR := pdf
PDF := $(addprefix $(PDFDIR)/s, $(addsuffix .pdf, $(SECTIONS)))

all: $(PDF)

$(PDFDIR)/s1.1.pdf: ex1.3.scm ex1.7.scm ex1.8.scm

$(PDFDIR)/s1.2.pdf: ex1.11.scm ex1.12.scm ex1.16.scm ex1.17.scm ex1.18.scm \
	ex1.19.scm ex1.22.scm

# All documents use a common style package
$(PDF): mystyle.sty

$(PDFDIR)/%.pdf: %.tex
	$(PDFLATEX) -output-directory $(PDFDIR) $<

clean:
	$(RM) pdf/*.{pdf,log,aux}
