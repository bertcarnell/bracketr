## start with the Docker "base R" Debian-based image
FROM r-base

## keep the packages current
RUN apt-get update -qq \
	&& apt-get dist-upgrade -y

## install requirements for R packages
## best practice to run update each time in case the build restarts later
RUN DEBIAN_FRONTEND=noninteractive apt-get update -qq \
	&& DEBIAN_FRONTEND=noninteractive apt-get install -t unstable -y \
	libgmp-dev \
	python3 \
	python3-dev \
	default-jdk \
	libfreetype6-dev \
	xorg \
	libx11-dev \
	libftgl2 \
	texinfo \
	texlive-base \
	texlive-extra-utils \
	texlive-fonts-extra \
	texlive-fonts-recommended \
	texlive-generic-recommended \
	texlive-latex-base \
	texlive-latex-extra \
	texlive-latex-recommended \
	git \
	libfreetype6-dev

RUN DEBIAN_FRONTEND=noninteractive apt-get update -qq \
	&& apt-get install -t unstable -y \
	r-recommended \
	r-cran-ggplot2 \
	r-cran-knitr \
	r-cran-rmarkdown \
	r-cran-assertthat \
	r-cran-dplyr