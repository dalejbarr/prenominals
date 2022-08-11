# container
cmd := singularity exec library://dalejbarr/talklab/talklab-tidy4

# emacs command
ecmd := emacs -q --batch -l org

e1files := exp1/img/gaze-probability.pdf exp1/img/log-ratio.pdf

e2files := exp2/img/gaze-probability.pdf exp2/img/log-ratio.pdf

manuscript.pdf : manuscript.org fig/design_fig.pdf $(e1files) $(e2files)
	$(cmd) $(ecmd) manuscript.org -f org-latex-export-to-pdf # 2>/dev/null
	rm -rf _minted-manuscript manuscript.bbl manuscript.tex

# $(e1files) :
# 	$(cmd) make -C exp1

# $(e2files) :
# 	$(cmd) make -C exp2

cleanms :
	rm -f manuscript.pdf
