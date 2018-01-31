# Setup
## software requirements
- Java Development Kit 7 or higher (needed by ODK Briefcase 1.8.0 and the R pakcage [rJava](https://cran.r-project.org/web/packages/rJava/index.html))
  - instructions for installing JDK 9 on Ubuntu 16.04 can be found [here](http://www.javahelps.com/2017/09/install-oracle-jdk-9-on-linux.html)
- [ODK Briefcase](https://opendatakit.org/downloads/download-category/briefcase/): the most recent (Sept. 12, 2017) version is 1.8.0
- [R](https://cran.r-project.org/) & packages: [openVA](https://cran.r-project.org/web/packages/openVA/index.html), [CrossVA](https://cran.r-project.org/web/packages/CrossVA/index.html), and their dependencies.  _Install R AFTER Java_.
  - installation for Ubuntu 16.04:
  ```sudo apt update```
  ```sudo apt install r-base```
  ```sudo R CMD javareconf```
Within R (to start R, simply type ```R``` at a terminal prompt, or ```sudo R``` for system-wide installation of packages), the necessary packages can be installed (with internet connection) using the following command:
```install.packages(c("openVA", "CrossVA"), dependencies=TRUE)```
- MySQL Server
  -  installation instructions for Ubuntu 16.04 can be found [here](https://help.ubuntu.com/lts/serverguide/mysql.html)
- [Python 2.7](https://www.python.org/downloads/), along with the requests and MySQLdb modules (be sure to install MySQL prior to this step).
  - installation for Ubuntu 16.04: Python 2.7 is pre-installed, but additional packages and modules are needed, which can be installed with the following command at a terminal:    
```sudo apt update```
```sudo apt install python-pip python-dev libmysqlclient-dev```
```pip install --upgrade pip```
```pip install requests mysqlclient```
- SQlite
  - installation for Ubuntu 16.04
  ```sudo apt update```
  ```sudo apt-get install sqlite3 libsqlite3-dev```

## Setup of MySQL Database and Tables
- Pipeline Database
- 
