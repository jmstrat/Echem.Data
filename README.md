# Echem.Data
Importing electrochemistry data into R.
The package attempts to normalise the data such that column names and units are consistent across different cyclers.

## Installation

1. Install the devtools package if you do not already have it:

   `install.packages("devtools")`

2. Install this package

   `devtools::install_github("jmstrat/Echem.Data")`

## Details

Contains the `read.echem` function used to import echem data of the following file formats:

| Biologic | Arbin | Lahne | Ivium | Maccor | Exported using `export`  |
|:--------:|:-----:|:-----:|:-----:|:------:|:------------------------:|
|   .mpr   | .xlsx | .cex  | .idf  |  .txt  |           .csv           |
|   .mpt   | .xls  | .xlsx |  .txt |        |                          |
|          |       | .xls  |       |        |                          |
|          |       | .txt  |       |        |                          |

Note that mpr and cex files are binary files that have been reverse-engineered. Software updates to the first party software
may break compatibility.

Once imported data can be plotted using `plot` and exported to csv using `export`. Further processing can be performed
using the [Echem.Process](https://github.com/jmstrat/Echem.Process) package.

## License

This project is licensed under the GNU Affero General Public License v3.0 - see the [LICENSE](LICENSE) file for details
