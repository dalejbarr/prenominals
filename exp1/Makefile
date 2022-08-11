datafiles := $(wildcard data-raw/F*.rds)
nbootsamps := 500
npermutations := 1000

default : data-derived/cluster-detection-results-adjective.rds data-derived/cluster-detection-results-noun.rds notify

notify :
	@curl https://api.simplepush.io/send -d '{"key": "2Srccr", "title": "daleb2-pc", "msg": "compilation finished!", "event": "event"}'

# step 6
data-derived/cluster-detection-results-noun.rds : scripts/cluster-permutation-functions.R computing-cluster-setup.R data-derived/pog-window-subj-noun.rds data-derived/pog-window-item-noun.rds
	Rscript scripts/06_detect-clusters-noun.R $(nbootsamps) $(npermutations)

# step 5
data-derived/cluster-detection-results-adjective.rds : scripts/cluster-permutation-functions.R computing-cluster-setup.R data-derived/pog-window-subj-adjective.rds data-derived/pog-window-item-adjective.rds
	Rscript scripts/05_detect-clusters-adjective.R $(nbootsamps) $(npermutations)

# step 4
data-derived/pog-bin.rds data-derived/gaze-plot-full.rds : \
	data-raw/sess-exclude.rds data-raw/resp.rds data-derived/pog-aligned.rds
	Rscript scripts/04_pog-binned.R

# step 3
data-derived/pog-lratio-binned.rds : data-derived/pog-window-subj-adjective.rds
	Rscript scripts/03_bin-lratio.R

# step 2
data-derived/pog-window-subj-adjective.rds data-derived/pog-window-item-adjective.rds \
data-derived/pog-window-subj-noun.rds data-derived/pog-window-item-noun.rds : \
	data-raw/sess-exclude.rds data-derived/pog-aligned.rds
	Rscript scripts/02_windowed-pog-data.R

# step 1
data-derived/pog-aligned.rds : data-raw/soundfiles.rds data-raw/resp.rds data-raw/coding.rds $(datafiles)
	Rscript scripts/01_pog-aligned.R

clean :
	rm -f data-derived/pog-aligned.rds
	rm -f data-derived/pog-window-subj-adjective.rds
	rm -f data-derived/pog-window-item-adjective.rds
	rm -f data-derived/pog-window-subj-noun.rds
	rm -f data-derived/pog-window-item-noun.rds
	rm -f data-derived/pog-lratio-binned.rds 
	rm -f data-derived/pog-bin.rds
	rm -f data-derived/gaze-plot-full.rds
