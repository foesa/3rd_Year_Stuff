CC=pandoc
CFLAGS=--latex-engine=xelatex --smart --table-of-contents

MD_FILES=$(shell find -name '*.md')
PDF_FILES=$(patsubst %.md,%.pdf,$(MD_FILES))

default: $(PDF_FILES)

%.pdf: %.md
	$(CC) $(CFLAGS) $< -o $@

clean:
	$(RM) -r */*.pdf
