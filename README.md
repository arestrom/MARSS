
# MarSS (Marine Sport Sampling)

## Overview

This application provides a front-end interface to WDFWs SS (sport
sampling) database. The application is in an early developmental state.
It is intended to allow biologists working for the Puget Sound Sampling
project to proof historical baseline data. The application will only
work for those who have been granted appropriate permissions to the
database. Each user will first need to be whitelisted and added to the
appropriate user groups within the database server. For an overview of
the SS database, the most recent version of the data dictionary can be
found at: <https://arestrom.github.io/ssdd/>

## Installation

The application is distributed as a standalone Windows installer. It
does not require admin rights to install. The installer will create a
new directory called `Apps\MarSS` under the users `Documents` folder. It
will also place an icon on the users desktop for launching. The
application comes bundled with a portable version of R and includes all
package dependencies. No separate install of R is needed. This also
means that the application should continue to work well into the future,
even as the underlying technology and web of dependencies change.

## Start and stop the application

Before launching the application you are strongly advised to make sure
your default browser is set to something like `Chrome` or `Firefox`. The
interface is unlikely to work correctly using a Microsoft browser. After
double-clicking the desktop icon, it may take a few seconds to open.
Please **do not** use the refresh icon in your browser when running the
application. If you do you will need to close and reopen.

## Permissions

In order to use this application you **must** first be granted
permissions to the database. Please contact the database administrator
to be whitelisted and granted the appropriate permissions. When you run
the application the user name is set by default to be the same as the
user name of the person that logged into the computer. This is by
design. It allows recording the user name to the database `created_by`
or `modified_by` fields anytime new entries, updates, or edits are
committed.

The first time you use the application you will be automatically routed
to the login screen to enter your credentials and required connection
information. Afterwards you will only see the login screen if changes
are made to the server, or your password lapses. If that happens please
contact the database administrator to verify your password is current,
and that you have entered the correct values for the server host and
port. Database connection credentials are secured as encryted values in
the Windows Credential store.

### In case of a crash

If after installing the application, it crashes, or fails to load,
navigate to the `out.txt` folder in the App directory:
`C:\Documents\Apps\MARSS\out.txt`. The `out.txt` file will log the first
fatal error encountered. Please include this information when reporting
the error.

After a crash you may also need to kill any active `R` processes using
Windows Task Manager. Click `Ctrl-Alt-Delete` to open Task Manager. Then
in the `Processes` tab, look for any processes named
`R for Windows front-end` or `R for Windows terminal front-end`. You can
right-click on these and select `End task` to terminate the process.
