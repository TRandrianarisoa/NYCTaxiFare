## This script splits a fare dataset *without* headers
## into several compressed files according to the year-month
## Pie it with gzip -dc to process compressed files.

BEGIN { FS=",";}

{
	f=substr($3, 1, 7); # those characters are the year-month
	print | "gzip -9 > " f ".csv.gz"
}
