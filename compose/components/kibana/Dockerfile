FROM ubuntu:14.04
RUN apt-get update && apt-get install -y wget ca-certificates
RUN wget https://download.elastic.co/kibana/kibana/kibana-4.1.1-linux-x64.tar.gz -O /tmp/kibana.tar.gz && \
    cd /tmp && tar zxf kibana.tar.gz
ENTRYPOINT [ "/tmp/kibana-4.1.1-linux-x64/bin/kibana" ]
COPY kibana.yml /tmp/kibana-4.1.1-linux-x64/config/kibana.yml
EXPOSE 5601
