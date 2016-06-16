RoboCup@Work Referee Box
========================

This is the repository of the referee box for the RoboCup@Work competition for RoboCup World Cup 2016 in Leipzig.

This referee box is only a small adaptation of the RoCKIn@Work referee box, which itself is an adaptiation of the RoboCup Logistics League referee box.

## Contents

1. Overview
2. Installation
3. Configuration
4. Usage

## Overview

The Central Factory Hub, or RefBox (as it's commonly called) is an application which provides interfaces for communicating with devices and robots in the RoboCup@Work arena. The RefBox itself isn't doing automated refereeing (yet), however it does generate task specifications to ensure the referees are not being biased. The RefBox keeps track of the time, and makes sure the task specification is given to the team when the timer starts, and not earlier. To communicate with devices in the arena, like rotating tables, teams send commands to the Referee Box, and the Referee Box acts as a proxy between the devices ensuring the communication is logged, and done appropriately. The Referee Box can accept control commands from the referees, and will not accept control commands from teams.

The ```refbox``` has two types of clients with which it communicates differently either UDP or TCP. UDP communication is used between the RefBox and the teams, because this is a wireless communication channel and isn't reliable (especially on site at RoboCup). TCP is used for the devices which are controlled by the Referees and Technical Committee.

The ```refbox``` application itself works in the background and only prints some log output to the console.

To view the status of the Central Factory, you must launch the ```atwork-viewer```. To control the Central Factory, launch the ```atwork-controller```.


## Installation
The RoboCup@Work Central Factory Hub (CFH) can be installed on most Linux distributions. However, some require dependencies being built from source. The currently supported setup is Ubuntu 14.04, with Boost 1.54. For other non-supported distributions and setups see Alternative Installation Hints at the end of this readme.

### Officially Supported Setup: Ubuntu 14.04, Boost 1.54

1. Add [Tim Niemueller's PPA](https://launchpad.net/~timn/+archive/ubuntu/clips):
      
        sudo add-apt-repository ppa:timn/clips
    (Note: This PPA currently only works for Ubuntu 12.04, 12.10 and 14.04.)
    
2. Install the dependencies for both LLSFRB and CFH:
        
        sudo apt-get update
        sudo apt-get install libmodbus-dev libclips-dev clips libclipsmm-dev \
                             protobuf-compiler libprotobuf-dev libprotoc-dev \
                             boost1.54-all-dev libmodbus-dev \
                             libglibmm-2.4-dev libgtkmm-3.0-dev libncurses5-dev \
                             libncursesw5-dev libyaml-cpp-dev libavahi-client-dev git \
                             libssl-dev libelf-dev mongodb-dev mongodb-clients \
                             mongodb libzmq3-dev

     (Note: Boost 1.54 is specified to avoid causing apt-get broken package problems with ROS. If you are using another version of Boost see Alternative Setup.)

3. Clone this repository:
        
        git clone https://github.com/robocup-at-work/at_work_central_factory_hub.git

4. Build the Central Factory Hub:
        
        cd at_work_central_factory_hub
        make

5. Go to Configuration Section before running the CFH.


## Configuration

Usually, the refbox application running on another computer on the same network as the robot.
In the case, ```send-port``` and ```recv-port``` are not used, instead only ```port``` is needed.

There are several configuration values which need to be changed depending on the network IP addresses.
The default values are: 
  * llsfrb/shell/refbox-host: !ipv4 localhost
  * llsfrb/comm/public-peer/host: !ipv4 192.168.1.255
  * llsfrb/comm/teamname-peer/host: !ipv4 192.168.1.255

Note: the public-pear and team-pear both have the same host ip. This value is set to the broadcast address, not the IP address, it always ends in 255. For example:

```
user@ubuntu$ ifconfig

virbr0    Link encap:Ethernet  HWaddr de:c9:5a:c2:6b:43  
          inet addr:192.168.122.1  Bcast:192.168.122.255  Mask:255.255.255.0
```
Because my subnet Mask is 255.255.255.0 and my Broadcast address is: 192.168.122.255 I would set the ```host``` configuration value to ```192.168.122.255```. 

### Typical Configuration for testing in your own lab
(at RoboCup the TC/OC will configure the RefBox).

1. Configure the CFH with your editor
    
        editor ./cfg/config.yaml

    For testing the following changes are common:
    - Change the CFH IP address in section "shell", this should be where you're running the CFH. Other machines communicating with the CFH need this value set correctly.
        
            refbox-host: !ipv4 localhost

    - If the robot software or CFH clients (e.g. GUIs) are running on the same machine as the CFH (e.g. local testing), toggle the comments on "port" or "send-port" and "recv-port". Use "port" when communicating between different machines, use "send-port" and "recv-port" when communicating with the same machine.
      - For CFH Clients the "path" of the configuation properties is "llsfrb/comm/public-peer"
    
        ```    
        public-peer:
          host: !ipv4 192.168.1.255
          #port: !udp-port 4444
          send-port: !udp-port 4444
          recv-port: !udp-port 4445
        ```
      - For CFH Peers (robots) the "path" of the configuration properties is "llsfrb/comm/[TeamName]-peer" 

        ```
        RobOTTO-peer:
          host: !ipv4 192.168.1.255
          #port: !udp-port 4448
          send-port: !udp-port 4448
          recv-port: !udp-port 4449
        ```
    - disable mongodb (No messages will be logged):
        It is useful to have mongodb enabled when debugging but it uses a lot of harddrive space after a while.
        
            mongodb:
              enable: false

Full configuration details and explainations are found in the RoCKIn PDF documentation (https://github.com/rockin-robot-challenge/at_work_central_factory_hub_doc). 

Only a few common options are outlined here.




## Usage

1.    Start the RefBox:       ```./bin/refbox```
2.    Start the controller:   ```./bin/atwork-controller```
3.    Start the viewer:       ```./bin/atwork-viewer```

