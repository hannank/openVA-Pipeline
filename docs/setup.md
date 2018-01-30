# Setup
## software requirements
- Java (needed by ODK Briefcase and the R pakcage [rJava](https://cran.r-project.org/web/packages/rJava/index.html))
  - installation for Ubuntu 16.04:
- [ODK Briefcase](https://opendatakit.org/downloads/download-category/briefcase/)
- [R](https://cran.r-project.org/) & packages: [openVA](https://cran.r-project.org/web/packages/openVA/index.html), [CrossVA](https://cran.r-project.org/web/packages/CrossVA/index.html), and their dependencies.
  - installation for Ubuntu 16.04:
Within R, all packages can
be installed (with internet connection) using the following command:    
```install.packages(c("openVA", "CrossVA"), dependencies=TRUE)```
- [Python 2.7](https://www.python.org/downloads/), along with the requests and MySQLdb modules
  - installation for Ubuntu 16.04: Python 2.7 is pre-installed, and the modules can be installed with the following command at a terminal:    
```pip install requests MySQLdb```

## Setup of MySQL Database and Tables
- Pipeline Database
- 
