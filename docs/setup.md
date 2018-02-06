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
  -  installation for Ubuntu 16.04    
  ```sudo apt update ```    
  ```sudo apt install mysql-server```    
  (the installation will ask for a root password)
  -  More details for Ubuntu 16.04 can be found [here](https://help.ubuntu.com/lts/serverguide/mysql.html)
- [Python 2.7](https://www.python.org/downloads/), along with the requests and MySQLdb modules (be sure to install MySQL prior to this step).
  - installation for Ubuntu 16.04: Python 2.7 is pre-installed, but additional packages and modules are needed, which can be installed with the following commands at a terminal:    
```sudo apt update```   
```sudo apt install python-pip python-dev libmysqlclient-dev```    
```pip install --upgrade pip```    
```pip install requests mysqlclient --user```    
- SQlite
  - installation for Ubuntu 16.04   
  ```sudo apt update```   
  ```sudo apt-get install sqlite3 libsqlite3-dev```   

## Setup of MySQL: Database & User
1. Start mysql with ```mysql --user=root -p mysql``` and enter root password when prompted.
2. Create new database called _Pipeline_ with the command ```CREATE DATABASE Pipeline;```
3. Create new user (with privileges) for running openVA-Pipeline with the following commands
```CREATE USER 'user_name'@'localhost' IDENTIFIED BY 'user_password';``` where _user_name_ and _user_password_ are replaced with your own selections.    
```GRANT SELECT,INSERT,UPDATE,DELETE,CREATE,DROP ON Pipeline.* TO 'user_name'@'localhost';```   
4. Exit mysql using the command ```\q;``` 
5. Edit the pipelineDB.sql script so that it contains the appropriate values for your ODK and DHIS2 servers in the following tables
    - ODK_Conf: aggURL, aggUser, aggPass, formID
    - DHIS_Conf: dhisURL, dhisUser, dhisPass, dhisOrgUnit
6. Run the pipelineDB.sql script with the command    
```mysql --user=user_name -p Pipeline < pipelineDB.sql```
