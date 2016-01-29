# Fix Gemsite
mkdir -p ${TFTPROOT}/gemsite/gems
cd ${TFTPROOT}/gemsite
/opt/chef/embedded/bin/gem generate_index
cd -
