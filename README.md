I used Google Maps history to see how close I came to meeting my boyfriend

This is an R script that imports, cleans, analyses and maps out Google maps location history data. To use this script both people will need to request thier location history from Google.

This artical does an excellent job of walking through the process: https://www.howtogeek.com/725241/how-to-download-your-google-maps-data/

After unzipping the file, you'll be refering to the json file in Location History named Records, i.e. "Location History/Records.json".

The script converts the json format to R list objects using the rjson library.