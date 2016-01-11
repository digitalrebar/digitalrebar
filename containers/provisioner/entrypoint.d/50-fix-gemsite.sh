# Fix Gemsite
mkdir -p /tftpboot/gemsite/gems
cd /tftpboot/gemsite
/opt/chef/embedded/bin/gem generate_index
cd -
