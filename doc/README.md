## DigitalRebar Hardware Workloads

If you are using DigitalRebar with the Hardware workload, then please review this documentation.

### Attach to BMC Network

By default, Rebar sets up the BMC network from 192.168.128.21/22 and up.  That network will be bridged between your docker admin and your external NIC.  

If you'd like to connect to that network on your local system then you need to bind an IP for your desktop to that network:  `sudo ip a add 192.168.128.2/22 dev docker0`

### RAID Tools

Before Rebar can configure RAID, you must download the correct utilities.  Rebar does NOT perform this download due to licensing considerations of the tools.

You need to perform this step if you see the following error:

`STDOUT: SAS2IRCU_P19.zip not present at http://192.168.124.10:8091/files/raid`

*Note* The directory which you should place the file is different depending on weather rebar is running in a Docker container or if it is running on a native OS

Steps on a native OS (Not in a Docker container):
  1. visit the following Avago pages in a _WEB BROWSER_ and accept the EULA
    1. [[http://www.avagotech.com/support/download-search]]
      1. Select 'All Raid Controllers' in the Product Family
      1. Enter 'MegaCLI' in the Keywork field.
      1. Click 'Search'
      1. Open 'Management Software and Tools'
      1. Find and click Download on 'MegaCLI 5.5 P2'
      1. The filename should be '8.07.14_MegaCLI.zip'
    1. [[http://www.avagotech.com/support/download-search]]
      1. Select 'All Host Bus Adapters (HBAs)' in the Product Family
      1. Enter 'SAS2IRCU' in the Keywork field.
      1. Click 'Search'
      1. Open 'Management Software and Tools'
      1. Find and click Download on 'SAS2IRCU_P19'
      1. The filename should be 'SAS2IRCU_P19.zip'
  1. copy the two downloaded files copied into: `/tftpboot/files/raid`
  1. update the permissions to allow guest reading: `chmod 664 *`

Steps with Docker:
  1. mkdir ~/.cache/digitalrebar/tftpboot/files/raid 
  1. visit the following Avago pages in a _WEB BROWSER_ and accept the EULA
    1. [[http://www.avagotech.com/support/download-search]]
      1. Select 'All Raid Controllers' in the Product Family
      1. Enter 'MegaCLI' in the Keywork field.
      1. Click 'Search'
      1. Open 'Management Software and Tools'
      1. Find and click Download on 'MegaCLI 5.5 P2'
      1. The filename should be '8.07.14_MegaCLI.zip'
    1. [[http://www.avagotech.com/support/download-search]]
      1. Select 'All Host Bus Adapters (HBAs)' in the Product Family
      1. Enter 'SAS2IRCU' in the Keywork field.
      1. Click 'Search'
      1. Open 'Management Software and Tools'
      1. Find and click Download on 'SAS2IRCU_P19'
      1. The filename should be 'SAS2IRCU_P19.zip'
  1. copy the two downloaded files copied into: `~/.cache/digitalrebar/tftpboot/files/raid`
  1. update the permissions to allow guest reading: `chmod 664 *`

*IMPORTANT* You should check [[raid/rebar.yml]] to confirm that the file names are correct.

> You need to do these steps for even if the hardware (e.g.: KVM) does not need the libraries. 
  

