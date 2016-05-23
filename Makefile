PDFLATEX := pdflatex

VPATH := ch1

.PHONY: all clean

EXERCISES := 1.1
PDFDIR := pdf
PDF := $(addprefix $(PDFDIR)/, $(addsuffix .pdf, $(EXERCISES)))

all: $(PDF)

$(PDFDIR)/%.pdf: %.tex
	$(PDFLATEX) -output-directory $(PDFDIR) $^

clean:
	$(RM) pdf/*.{pdf,log,aux}
