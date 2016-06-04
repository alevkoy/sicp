PDFLATEX := pdflatex

VPATH := ch1

.PHONY: all clean

SECTIONS := 1.1
PDFDIR := pdf
PDF := $(addprefix $(PDFDIR)/s, $(addsuffix .pdf, $(SECTIONS)))

all: $(PDF)

$(PDFDIR)/s1.1.pdf: ex1.3.scm

$(PDFDIR)/%.pdf: %.tex
	$(PDFLATEX) -output-directory $(PDFDIR) $^

clean:
	$(RM) pdf/*.{pdf,log,aux}
