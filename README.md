RoCKIn@Work Referee Box
=======================

This is the repository of the referee box for the RoCKIn competition (http://rockinrobotchallenge.eu). This referee box is an adaption of the RoboCup Logistics League Sponsored by Festo (LLSF) referee box (http://www.robocup-logistics.org/refbox)


## Installation
To install the RoCKIn referee box, please install the prerequisites of the LLSF referee box which are described here: https://trac.fawkesrobotics.org/wiki/LLSFRefBox/Install

Additionally, the following debian packages need to be installed:

    sudo apt-get install libssl-dev libelf-dev mongodb-dev mongodb-clients mongodb libzmq3-dev
  
## Compiling the RoCKIn@Work Referee Box
### Cloning the Git repository:

    git clone https://github.com/rockin-robot-challenge/at_work_central_factory_hub.git
    cd at_work_central_factory_hub
    git checkout rockin


### Compiling the refbox:

    make

    
### Run the RoCKIn@Work Referee Box

    ./bin/llsf-refbox
    
    
    

## FAQ

**Q:** I get the following error after executing the make command: 

    /usr/include/mongo/client/../util/net/../../db/../util/../db/mongomutex.h:235:9: error: call of overloaded ‘msgasserted(int, std::basic_string<char>)’ is ambiguous
    
How can I solve the problem?
    
**A:** Please apply the following patch: https://12286394519788571250.googlegroups.com/attach/8d1000635e6a8fa8/mongomutex.patch?part=0.1&view=1&vt=ANaJVrHyLNIISBTuORhFBABOAVPN-88t-nVX0FUuzvgxk8-w6O181B7fkE5fJsyydUHwq1vpbUzDvqvP3GFhuBFc8-EjaOnoLds8eox2JDGrk4FasLzQc_I

------------------------------------

**Q:** I get the following error after executing the make command: 

    === Linking llsf-refbox ---
    /usr/bin/ld: /usr/lib/gcc/x86_64-linux-gnu/4.6/../../../../lib/libmongoclient.a(md5main.o): undefined reference to symbol 'sin@@GLIBC_2.2.5'
    /usr/bin/ld: note: 'sin@@GLIBC_2.2.5' is defined in DSO /usr/lib/gcc/x86_64-linux-gnu/4.6/../../../x86_64-linux-gnu/libm.so so try adding it to the linker command line
    /usr/lib/gcc/x86_64-linux-gnu/4.6/../../../x86_64-linux-gnu/libm.so: could not read symbols: Invalid operation
    collect2: ld returned 1 exit status
    
How can I solve the problem?

**A:** Please apply the following patch:

    diff --git a/src/refbox/Makefile b/src/refbox/Makefile
    index aa05696..bd92a5c 100644
    --- a/src/refbox/Makefile
    +++ b/src/refbox/Makefile
    @@ -39,7 +39,7 @@ ifeq ($(HAVE_PROTOBUF)$(HAVE_LIBMODBUS)$(HAVE_CLIPS)$(HAVE_BOOST_LIBS),1111)
       CFLAGS  += $(CFLAGS_PROTOBUF) $(CFLAGS_LIBMODBUS) $(CFLAGS_CLIPS) $(CFLAGS_MONGODB) \
                 $(call boost-libs-cflags,$(REQ_BOOST_LIBS))
    -  LDFLAGS += $(LDFLAGS_PROTOBUF) $(LDFLAGS_LIBMODBUS) $(LDFLAGS_CLIPS) $(LDFLAGS_MONGODB) \
    +  LDFLAGS += $(LDFLAGS_PROTOBUF) $(LDFLAGS_LIBMODBUS) $(LDFLAGS_CLIPS) \
                 $(call boost-libs-ldflags,$(REQ_BOOST_LIBS))
    #MANPAGES_all =  $(MANDIR)/man1/llsf-refbox.1
 
 ------------------------------------
