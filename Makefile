all: deps clean build serve

deps:
	Rscript deps.R

clean:
	rm -f data/Medicare_Provider*.csv data/Providers.csv data/Inpatient*.csv data/Outpatient*.csv

build:
	Rscript build.R

serve:
	Rscript app.R
