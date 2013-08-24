targets = manual.pdf manual.info manual.html

all: $(targets)

%.html: %.texi; $(MAKEINFO) --html --no-split $^
%.pdf: %.texi; env LC_ALL=C texi2pdf $^

clean:
	rm -f $(targets)
	rm -f manual.aux manual.cp manual.fn manual.fns manual.ky
	rm -f manual.log manual.pg manual.toc manual.tp manual.vr

.PHONY: all clean
