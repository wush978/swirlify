FROM rocker/r-ver:3.4.4
MAINTAINER Wush Wu <wush978@gmail.com>

RUN apt-get update && \
  apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev libpng-dev libjpeg-dev
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen && \
  echo "zh_TW.UTF-8 UTF-8" >> /etc/locale.gen && \
  echo "" >> /etc/locale.gen && \
  locale-gen && \
  update-locale
ENV LC_ALL=zh_TW.UTF-8
ARG JENKINS_UID
RUN useradd -u $JENKINS_UID -ms /bin/bash jenkins
RUN chown -R jenkins:jenkins /home/jenkins
USER jenkins
WORKDIR /home/jenkins
VOLUME ["/home/jenkins/R-lib"]
VOLUME ["/home/jenkins/pvm"]
VOLUME ["/home/jenkins/swirl"]
VOLUME ["/home/jenkins/swirlify"]
VOLUME ["/home/jenkins/course"]
