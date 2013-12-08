## Installing R packages from this directory

### Download the package

Click on the package name, then click on the *View Raw* link (middle of the gray box) to download the file.
Download it to a location you can find or name below.

### Install the package into R

Issue the following command in R (or RStudio):

```
install.packages('FILENAME', repos=NULL, type='source')
```

where FILENAME is the complete path to the name of the file you downloaded.

### Using the package

Issue a **library** command to use the package in your R session.
If the package is named *zUtil* then

```
library(zUtil)
```
