all: deps clean build serve

deps:
	Rscript deps.R

clean:
	rm -f data/Medicare_Provider*.csv data/MEDICARE_Provider*.CSV data/MUP_*.csv data/MUP_*.xlsx
	rm -f data/cms_inpatient_*.csv data/cms_outpatient_*.csv
	rm -f data/Providers.csv data/Inpatient*.csv data/Outpatient*.csv

clean-cache:
	rm -f data/.cms_url_cache.rds

build:
	Rscript build.R

serve:
	Rscript app.R
