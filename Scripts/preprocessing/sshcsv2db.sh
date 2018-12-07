## Script to import the compressed csv data into the
## Postgres data base
## Take the compress data file as first argument
## Take an optional limiting number of row argument

USER="nyctf"
HOST="v01d.xyz"

if [[ -z $2 ]]; then
    echo "All database"
    gzip -dc $1 | pv -l | ssh ovh "psql --username $USER -c '\copy fare FROM stdin WITH (FORMAT CSV);'"
else
    echo "Limiting input to $2 rows"
    gzip -dc $1 | head -n $2 | pv -l -s $2 | ssh ovh "psql --username $USER -c '\copy fare FROM stdin WITH (FORMAT CSV);'"
fi
