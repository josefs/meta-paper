hask031.pdf: paper.pdf
	cp paper.pdf hask031.pdf

paper.pdf: paper.tex
	pdflatex paper.tex
	bibtex paper.aux
	pdflatex paper.tex
	pdflatex paper.tex

paper.tex: paper.md Makefile sigplanconf-template.tex bib.bib
	pandoc paper.md -o paper.tex --tab-stop=2 --template=sigplanconf-template.tex --bibliography=bib.bib --natbib
