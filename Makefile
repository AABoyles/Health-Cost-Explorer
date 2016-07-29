all: build serve

deps:
	Rscript deps.R

clean:
	rm -rf data/Medicare_Provider*

build:
	Rscript build.R

serve:
	Rscript app.R
