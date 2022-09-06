FROM debian:bullseye

RUN apt-get update && apt-get install -y git sudo unzip curl software-properties-common build-essential

ADD install.sh /tmp/install.sh
RUN chmod u+x /tmp/install.sh && bash /tmp/install.sh

# RUN useradd tsutorm
# USER tsutorm

