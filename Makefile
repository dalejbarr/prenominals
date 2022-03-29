EYERAW = $(wildcard data-raw/F*.rds)

RAW = data-raw/aoi.rds data-raw/coding.rds \
	data-raw/resp.rds data-raw/soundfiles.rds $(EYERAW)

data-derived/cluster-detection-results.rds : scripts/05_detect-clusters.R data-derived/pog-window-subj.rds data-derived/pog-window-item.rds cluster-setup.R
	Rscript scripts/05_detect-clusters.R

data-derived/pog-bin.rds data-derived/gaze-plot-full.rds : scripts/04_pog-binned.R $(RAW) data-derived/pog-aligned.rds
	Rscript scripts/04_pog-binned.R

data-derived/pog-lratio-binned.rds : data-derived/pog-window-subj.rds scripts/03_bin-lratio.R
	Rscript scripts/03_bin-lratio.R

data-derived/pog-window-subj.rds data-derived/pog-window-item.rds : scripts/02_windowed-pog-data.R data-derived/pog-aligned.rds
	Rscript scripts/02_windowed-pog-data.R

data-derived/pog-aligned.rds : scripts/01_pog-aligned.R $(RAW) 
	Rscript scripts/01_pog-aligned.R

clean :
	rm data-derived/*
	rm data-tmp/*
