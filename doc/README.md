## OpenCrowbar Hardware Workloads

If you are using OpenCrowbar with the Hardware workload, then please review this documentation.


### RAID Tools

Before Crowbar can configure RAID, you must download the correct utilities.  Crowbar does NOT perform this download due to licensing considerations of the tools.

You need to perform this step if you see the following error:

`STDOUT: SAS2IRCU_P19.zip not present at http://192.168.124.10:8091/files/raid`

Steps:
  1. mkdir ~/.cache/opencrowbar/tftpboot/files/raid 
  1. cd ~/.cache/opencrowbar/tftpboot/files/raid
  1. wget http://www.lsi.com/downloads/Public/Host%20Bus%20Adapters/Host%20Bus%20Adapters%20Common%20Files/SAS_SATA_6G_P19/SAS2IRCU_P19.zip
