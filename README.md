RoCKIn@Work Referee Box
=======================

This is the repository of the referee box for the RoCKIn competition (http://rockinrobotchallenge.eu). This referee box is an adaption of the RoboCup Logistics League Sponsored by Festo (LLSF) referee box (http://www.robocup-logistics.org/refbox)


## Installation
The RoCKIn Central Factory Hub (CFH) can be installed on most Linux distributions. However, some require dependencies being built from source. The currently supported setup is Ubuntu 14.04, with Boost 1.54. For other non-supported distributions and setups see Alternative Installation Hints at the end of this readme.

### Officially Supported Setup: Ubuntu 14.04, Boost 1.54

1. Add [Tim Niemueller's PPA](https://launchpad.net/~timn/+archive/ubuntu/clips):
      
        sudo add-apt-repository ppa:timn/clips
    (Note: This PPA currently only works for Ubuntu 12.04, 12.10 and 14.04.)
    
2. Install the dependencies for both LLSFRB and CFH:
        
        sudo apt-get update
        sudo apt-get install libmodbus-dev libclips-dev clips libclipsmm-dev \
                             protobuf-compiler libprotobuf-dev libprotoc-dev \
                             boost1.54-all-dev libmodbus-dev \
                             libglibmm-2.4-dev libgtkmm-3.0-dev \
                             libncursesw5-dev libyaml-cpp-dev libavahi-client-dev git \
                             libssl-dev libelf-dev mongodb-dev mongodb-clients \
                             mongodb libzmq3-dev

     (Note: Boost 1.54 is specified to avoid causing apt-get broken package problems with ROS. If you are using another version of Boost see Alternative Setup.)

3. Clone this repository:
        
        git clone https://github.com/rockin-robot-challenge/at_work_central_factory_hub.git

4. Build the Central Factory Hub:
        
        cd at_work_central_factory_hub
        make

5. Go to Configuration Section before running the CFH.


## Configuration

Full configuration details and explainations are found in the PDF documentation (https://github.com/rockin-robot-challenge/at_work_central_factory_hub_doc). 

Only a few common options are outlined here.

1. Configure the CFH with your editor
    
        editor ./cfg/config.yaml

    For testing the following changes are common:
    - Change the CFH IP address in section "shell", this should be where you're running the CFH. Other machines communicating with the CFH need this value set correctly.
        
            refbox-host: !ipv4 localhost
    - disable mongodb (No messages will be logged):
        
            mongodb:
              enable: false
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

## Run the RoCKIn@Work Central Factory Hub

    ./bin/llsf-refbox
    

## FAQ

**Q:** I get the following error after executing the make command: 

    /usr/include/mongo/client/../util/net/../../db/../util/../db/mongomutex.h:235:9: error: call of overloaded ‘msgasserted(int, std::basic_string<char>)’ is ambiguous
    
How can I solve the problem?
    
**A:** Please apply the following patch: https://12286394519788571250.googlegroups.com/attach/8d1000635e6a8fa8/mongomutex.patch?part=0.1&view=1&vt=ANaJVrHyLNIISBTuORhFBABOAVPN-88t-nVX0FUuzvgxk8-w6O181B7fkE5fJsyydUHwq1vpbUzDvqvP3GFhuBFc8-EjaOnoLds8eox2JDGrk4FasLzQc_I

------------------------------------

## Unsupported Installations
The CFH is known to work on several versions of Ubuntu, and ArchLinux. But the combinations of dependencies is very large, and they may cause conflicts. Users with alternative setups are assumed to be aware of their system dependencies. 

### Alternative Installation Hints

Users attempting to use alternative, unsupported systems are doing so without any guarentees. These are just hints.

1. To install the RoCKIn Central Factory Hub, please read and install the prerequisites of the LLSF referee box which are described here:
   
    (Note: You MAY NOT be able to copy and paste from the following link. Ubuntu and Fedora instructions are provided. However, The instructions contain specific package versions, which are not available on all versions of Ubuntu. Search your package manager to find required versions.)
    
    https://trac.fawkesrobotics.org/wiki/LLSFRefBox/Install. 

2. Additionally, the following debian packages need to be installed (if available):
    
        sudo apt-get install libssl-dev libelf-dev mongodb-dev mongodb-clients mongodb libzmq3-dev

3. Older versions of Ubuntu are known to not have all of the above packages. The following may be installed from source if not available:
    - clips 6.30+ (http://sourceforge.net/projects/clipsmm/files/clips)
    - clipsmm 0.3.4+(http://sourceforge.net/projects/clipsmm/files/clipsmm)
    - zeromq 3+ (http://zeromq.org/) 
    - zeromq.hpp (https://github.com/zeromq/cppzmq/blob/master/zmq.hpp) 
            
            cd /usr/local/include/
            sudo wget https://raw.githubusercontent.com/zeromq/cppzmq/master/zmq.hpp
    - boost 1.48+ (http://www.boost.org/) It is STRONGLY recommended to install boost through your package manager, and to choose the version which does not conflict with your other dependencies.
  
4. Clone and build this repository as described above.
