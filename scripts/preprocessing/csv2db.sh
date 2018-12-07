## Script to import the compressed csv data into the
## Postgres data base
## Take the compress data file as first argument
## Take an optional limiting number of row argument

USER="nyctf"
#HOST="v01d.xyz"
HOST="localhost"

if [[ -z $2 ]]; then
    echo "All database"
    gzip -dc $1 | pv -l | psql --username $USER -h $HOST -c "\copy fare FROM stdin WITH (FORMAT CSV);"
else
    echo "Limiting input to $2 rows"
    gzip -dc $1 | pv -l -s $2 | head -n $2 | psql --username $USER -h $HOST -c "\copy fare FROM stdin WITH (FORMAT CSV);"
fi
