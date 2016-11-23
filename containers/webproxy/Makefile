BUILD_IMAGE=squid-build
all:
    docker rm -f $(BUILD_IMAGE)
    docker create --name $(BUILD_IMAGE) $(BUILD_IMAGE):latest true
    docker cp $(BUILD_IMAGE):/debs/squid-common_3.5.12-1ubuntu8_all.deb .
    docker cp $(BUILD_IMAGE):/debs/squid_3.5.12-1ubuntu8_amd64.deb .
    docker cp $(BUILD_IMAGE):/debs/squidclient_3.5.12-1ubuntu8_amd64.deb .
