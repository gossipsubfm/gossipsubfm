FROM atwalter/acl2s:latest

RUN apt-get update && apt-get install -y git curl unzip dos2unix && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir -p /model/

COPY model/ /model/

WORKDIR model

RUN ./make.sh

RUN /bin/bash
