paper.pdf: paper.tex
	pdflatex paper.tex

paper.tex: paper.md Makefile sigplanconf-template.tex
	pandoc paper.md -o paper.tex --tab-stop=2 --template=sigplanconf-template.tex
