# mstar-financial-statements

These Racket programs will download the Mstar financial statement documents and insert the 
statement data into a PostgreSQL database. The intended usage is:

```bash
$ racket quotes-extract.rkt
$ racket quotes-transform-load.rkt
```

```bash
$ racket financial-statement-extract.rkt
$ racket balance-sheet-transform-load.rkt
$ racket cash-flow-statement-transform-load.rkt
$ racket income-statement-transform-load.rkt
```

You will need to provide a database password for many of the above programs. The available parameters are:

```bash
$ racket financial-statements-extract.rkt -h
racket financial-statement-extract.rkt [ <option> ... ]
 where <option> is one of
  -f <first>, --first-symbol <first> : First symbol to query. Defaults to nothing
  -l <last>, --last-symbol <last> : Last symbol to query. Defaults to nothing
  -n <name>, --db-name <name> : Database name. Defaults to 'local'
  -p <password>, --db-pass <password> : Database password
  -u <user>, --db-user <user> : Database user name. Defaults to 'user'
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 Multiple single-letter switches can be combined after one `-`. For
  example: `-h-` is the same as `-h --`

$ racket balance-sheet-transform-load.rkt -h
racket balance-sheet-transform-load.rkt [ <option> ... ]
 where <option> is one of
  -b <folder>, --base-folder <folder> : Mstar balance sheet base folder. Defaults to /var/local/mstar/financial-statements
  -d <date>, --folder-date <date> : Mstar balance sheet folder date. Defaults to today
  -n <name>, --db-name <name> : Database name. Defaults to 'local'
  -p <password>, --db-pass <password> : Database password
  -u <user>, --db-user <user> : Database user name. Defaults to 'user'
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 Multiple single-letter switches can be combined after one `-`. For
  example: `-h-` is the same as `-h --`

$ racket cash-flow-statement-transform-load.rkt -h
racket cash-flow-statement-transform-load.rkt [ <option> ... ]
 where <option> is one of
  -b <folder>, --base-folder <folder> : Mstar cash flow statement base folder. Defaults to /var/local/mstar/financial-statements
  -d <date>, --folder-date <date> : Mstar cash flow statement folder date. Defaults to today
  -n <name>, --db-name <name> : Database name. Defaults to 'local'
  -p <password>, --db-pass <password> : Database password
  -u <user>, --db-user <user> : Database user name. Defaults to 'user'
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 Multiple single-letter switches can be combined after one `-`. For
  example: `-h-` is the same as `-h --`

$ racket income-statement-transform-load.rkt -h
racket income-statement-transform-load.rkt [ <option> ... ]
 where <option> is one of
  -b <folder>, --base-folder <folder> : Mstar income statement base folder. Defaults to /var/local/mstar/financial-statements
  -d <date>, --folder-date <date> : Mstar income statement folder date. Defaults to today
  -n <name>, --db-name <name> : Database name. Defaults to 'local'
  -p <password>, --db-pass <password> : Database password
  -u <user>, --db-user <user> : Database user name. Defaults to 'user'
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 Multiple single-letter switches can be combined after one `-`. For
  example: `-h-` is the same as `-h --`
```

The provided `schema.sql` file shows the expected schema within the target PostgreSQL instance. 
This process assumes you can write to a `/var/local/mstar` folder. This process also assumes you have loaded your database with NASDAQ symbol
file information. This data is provided by the [nasdaq-symbols](https://github.com/evdubs/nasdaq-symbols) project.

The above process will download around 600MB worth of JSON documents over many hours. It is encouraged to compress these files when you are 
done processing them. It is also encouraged that you do not run the extract jobs too frequently. I think running the financial-statement-extract
once per month is sufficient.

### Dependencies

It is recommended that you start with the standard Racket distribution. With that, you will need to install the following packages:

```bash
$ raco pkg install --skip-installed gregor html-parsing sxml tasks threading
```
