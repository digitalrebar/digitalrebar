FROM williamyeh/ansible:ubuntu16.04
MAINTAINER Rob Hirschfeld <rob@rackn.com>
ENTRYPOINT ["workloads/kubernetes.sh"]

RUN apt-get update && \
    apt-get install git sudo unzip jq curl python python-pymongo python-pycurl -y

# Rebar Deploy Code
RUN git clone https://github.com/digitalrebar/digitalrebar /root/digitalrebar
WORKDIR /root/digitalrebar/deploy

# Rebar CLI
RUN curl -o /usr/local/sbin/rebar https://s3-us-west-2.amazonaws.com/rebar-cli/rebar-linux-amd64 && \
    chmod +x /usr/local/sbin/rebar

# Assuming AWS, we'll preinstall the AWS tools
RUN curl "https://s3.amazonaws.com/aws-cli/awscli-bundle.zip" -o "awscli-bundle.zip" && \
            unzip awscli-bundle.zip && \
            sudo ./awscli-bundle/install -i /usr/local/aws -b /usr/local/bin/aws && \
            rm -rf awscli-bundle*

# Where are you running Digital Rebar?  aws, google, packet, etc.
ENV DEPLOY_ADMIN aws

# Where are you running your cluster? aws, google, packet, etc
ENV PROVIDER=aws

# CUSTOMIZE YOUR PROVIDER AND ACCESS in your client local .dr_info file!  
# See http://digital-rebar.readthedocs.io/en/latest/deployment/install/dr_info.html 
# Build this file: $IMAGE = docker build .
# Then: docker run -it -v ~/.dr_info:/root/.dr_info $IMAGE 
RUN echo "next step: docker run -it -v ~/.dr_info:/root/.dr_info $IMAGE"
