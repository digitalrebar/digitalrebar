#GREG: FROM debian:jessie
FROM ubuntu:14.04
RUN apt-get update && \
    apt-get install -y openjdk-7-jre wget
ENV JAVA_HOME /usr/lib/jvm/java-6-openjdk-amd64
RUN (cd /tmp && \
    wget https://download.elastic.co/elasticsearch/elasticsearch/elasticsearch-1.7.1.tar.gz -O pkg.tar.gz && \
    tar zxf pkg.tar.gz && mv elasticsearch-* /opt/elasticsearch &&\
    rm -rf /tmp/*)
COPY elasticsearch.yml /opt/elasticsearch/config/elasticsearch.yml
EXPOSE 9200
EXPOSE 9300
VOLUME /opt/elasticsearch/data
ENTRYPOINT ["/opt/elasticsearch/bin/elasticsearch"]
CMD []
