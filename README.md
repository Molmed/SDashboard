Archived: This application is not used anymore
==========

SDashboard
==========
SDashboard is a [Shiny](http://www.rstudio.com/shiny/) app which visualizes a number of metrics from the sequencing operation at the [SNP&SEQ technology platform](http://www.molmed.medsci.uu.se/SNP+SEQ+Technology+Platform/) in Uppsala. It is probably of relatively little interest to the outside world since the 'data.R' file which serves as the applications data layer is quite specific to our needs. However, if you have ideas on how to generalize it, don't hesitate to get in touch with us.

These instructions have been tested under Ubuntu 12.04 and Scientific Linux 6.

Installation and running
------------------------
To setup the SDashboard you need setup ODBC drivers. To get these in order install the `unixodbc` package, and follow the instructions corresponding to the drivers that you need. In our case we want to connect to a MS-SQL server, and the instructions for how to set these up on Redhat derived systems are available here: http://www.microsoft.com/en-us/download/details.aspx?id=36437 and a guide for how to set it up on Ubuntu is available here: http://onefinepub.com/2013/03/07/ms-sql-odbc-ubuntu/.

Further more you need to install `R` and the following `R` packages. Run the following command in R to install the necessary packages: 
    
    packages.install("shiny", "ggplot2", "reshape2", "RODBC")
    
Futhermore you need to copy the example.config.R and configure it.

    cp example.config.R config.R
    
Then you can run the app by running:
    
    R -e "shiny::runApp('~/path/to/sdashboard', 8000)"

This will fire up a local instance that you can access on:

    http://localhost:8000

Deploy to Shiny Server
----------------------
If you want to deploy the SDashboard to a Shiny server follow the instructions here: http://www.rstudio.com/shiny/server/install-opensource to install a Shiny server. Once you have that installed you need to copy SDashboard to `/srv/shiny-server/` and restart the server: `restart shiny-server`, and you should then be up and running. Go to:

    http://myserver:3838/sdashboard
    
to access SDashboard.
