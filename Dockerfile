# =============================================================================
# Dockerfile — Quant Research & Economic Intelligence Platform
# Builds a fully reproducible containerised R environment.
# Base: rocker/tidyverse for prebuilt R + tidyverse stack.
# =============================================================================

FROM rocker/tidyverse:4.4.1

LABEL maintainer="Quant Research Platform"
LABEL version="1.0.0"
LABEL description="Quant Research & Economic Intelligence Platform"

# System dependencies for R packages that require compiled code
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    pandoc \
    pandoc-citeproc \
    texlive-latex-base \
    texlive-fonts-recommended \
    texlive-xetex \
    && rm -rf /var/lib/apt/lists/*

# Install renv for reproducible package management
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

WORKDIR /app

# Copy lockfile first — layer caches until lockfile changes
COPY renv.lock renv.lock

# Restore packages from lockfile (exact versions, reproducible)
RUN R -e "renv::restore(prompt = FALSE)"

# Copy rest of the project
COPY . .

# Expose Shiny port
EXPOSE 3838

# Health check: verify Shiny can load
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
  CMD curl -f http://localhost:3838/ || exit 1

# Launch Shiny dashboard
CMD ["R", "-e", \
  "shiny::runApp('shiny-dashboard', host='0.0.0.0', port=3838, launch.browser=FALSE)"]
