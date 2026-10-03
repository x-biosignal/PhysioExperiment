# PhysioExperiment ecosystem — reproducible analysis environment.
#
# Installs the x-biosignal Physio* ecosystem from the r-universe registry
# (https://x-biosignal.r-universe.dev), which resolves the CRAN and Bioconductor
# dependencies automatically. This replaces the legacy single-monolith image;
# the ecosystem is now 14+ split packages published as separate repositories.
#
# Build:  docker build -t physioexperiment .
# Use:    docker run --rm -it physioexperiment R
FROM rocker/r-ver:4.5.2

LABEL maintainer="Yusuke Matsui <mail.to.matsui@gmail.com>"
LABEL description="PhysioExperiment: the x-biosignal ecosystem for physiological signal analysis"
LABEL org.opencontainers.image.source="https://github.com/x-biosignal"

# System dependencies: BLAS/LAPACK + OpenMP (Rcpp/Armadillo), HDF5, XML, curl,
# TLS, and the font/graphics stack used by the plotting and reporting packages.
RUN apt-get update && apt-get install -y --no-install-recommends \
        liblapack-dev \
        libblas-dev \
        libhdf5-dev \
        libxml2-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        libfontconfig1-dev \
        libfreetype6-dev \
        libpng-dev \
        libtiff-dev \
        libjpeg-dev \
        libharfbuzz-dev \
        libfribidi-dev \
        pandoc \
        git \
    && rm -rf /var/lib/apt/lists/*

# Install the ecosystem from r-universe. The r-universe repo is listed first so
# the Physio* packages come from x-biosignal; CRAN provides the rest, and the
# Bioconductor base classes (SummarizedExperiment, S4Vectors, ...) are pulled in
# as declared dependencies. Extend the vector to add more of the ecosystem.
RUN R -e "options(repos = c( \
              xbiosignal = 'https://x-biosignal.r-universe.dev', \
              BioCsoft   = 'https://bioconductor.org/packages/release/bioc', \
              BioCann    = 'https://bioconductor.org/packages/release/data/annotation', \
              CRAN       = 'https://cloud.r-project.org')); \
          install.packages(c( \
              'PhysioCore', 'PhysioIO', 'PhysioPreprocess', 'PhysioAnalysis', \
              'PhysioECG', 'PhysioEEG', 'PhysioEMG', 'PhysioEDA', \
              'PhysioReport'), \
              dependencies = TRUE); \
          if (!all(c('PhysioCore','PhysioECG','PhysioEEG','PhysioEMG','PhysioEDA') \
                   %in% rownames(installed.packages()))) \
              quit(status = 1L)"

# Smoke-test the install at build time.
RUN R -e "library(PhysioCore); library(PhysioECG); library(PhysioEEG); \
          cat('PhysioExperiment ecosystem ready\n')"

CMD ["R"]
