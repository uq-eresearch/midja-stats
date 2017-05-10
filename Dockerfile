FROM docker.io/node:latest

# Use HTTPS
RUN apt-get update && apt-get install -y apt-transport-https && apt-get clean
# Install R
RUN echo "deb https://cran.csiro.au/bin/linux/debian jessie-cran3/" >> /etc/apt/sources.list && \
  apt-key adv --keyserver keys.gnupg.net --recv-key 381BA480 && \
  apt-get update && \
  apt-get install -y r-base && \
  apt-get clean
# Install R packages
RUN Rscript -e \
  " options(repos=structure(c(CRAN='https://cran.csiro.au'))); \
    install.packages(c( \
      'Rserve', \
      'RJSONIO'))"
# Install s6
RUN curl -sL https://github.com/just-containers/s6-overlay/releases/download/v1.19.1.1/s6-overlay-amd64.tar.gz | tar xzv -C /

WORKDIR /usr/src/app
COPY . /usr/src/app/
RUN npm install
RUN echo "deamon disable" >> /etc/Rserv.conf
COPY services.d/ /etc/services.d/

EXPOSE 3000
CMD /init
