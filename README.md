# System Utilities

This package provides utility functions to build a [stack](http://haskellstack.org)-enabled project within a docker container. 
The [stackInDocker](src/System/Build.hs#58) function will build an executable from a given project inside a docker image and output the resulting binary to the host's filesystem. The main use of this function is to for provisioning machines across differnt architectures, e.g. deploy a [qmuli](https://github.com/ababkin/qmuli) lambda from a Mac OS X host.
