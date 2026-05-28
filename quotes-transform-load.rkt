#lang racket/base

(require db
         gregor
         json
         racket/cmdline
         racket/list
         racket/port
         racket/sequence
         racket/string
         threading)

(define base-folder (make-parameter "/var/local/mstar/quotes"))

(define folder-date (make-parameter (today)))

(define db-user (make-parameter "user"))

(define db-name (make-parameter "local"))

(define db-pass (make-parameter ""))

(command-line
 #:program "racket quotes-transform-load.rkt"
 #:once-each
 [("-b" "--base-folder") folder
                         "Morningstar Quotes base folder. Defaults to /var/local/mstar/quotes"
                         (base-folder folder)]
 [("-d" "--folder-date") date
                         "Morningstar Quotes folder date. Defaults to today"
                         (folder-date (iso8601->date date))]
 [("-n" "--db-name") name
                     "Database name. Defaults to 'local'"
                     (db-name name)]
 [("-p" "--db-pass") password
                     "Database password"
                     (db-pass password)]
 [("-u" "--db-user") user
                     "Database user name. Defaults to 'user'"
                     (db-user user)])

(define dbc (postgresql-connect #:user (db-user) #:database (db-name) #:password (db-pass)))

(define symbol-count (query-value dbc "
select
  count(*)
from
  nasdaq.symbol
where
  is_test_issue = false and
  is_next_shares = false and
  nasdaq_symbol !~ '[-\\$\\+\\*#!@%\\^=~]' and
  case when nasdaq_symbol ~ '[A-Z]{4}[L-Z]'
    then security_name !~ '(Note|Preferred|Right|Unit|Warrant)'
    else true
  end and
  last_seen = (select max(last_seen) from nasdaq.symbol);
"))

(define insert-count 0)

(parameterize ([current-directory (string-append (base-folder) "/" (~t (folder-date) "yyyy-MM-dd") "/")])
  (for ([p (sequence-filter (λ (p) (string-contains? (path->string p) ".json")) (in-directory (current-directory)))])
    (let* ([file-name (path->string p)])
      (call-with-input-file file-name
        (λ (in)
          (with-handlers ([exn:fail? (λ (e) (displayln (string-append "Failed to load " file-name
                                                                      " for date " (~t (folder-date) "yyyy-MM-dd")))
                                        (displayln e))])
            (~> (port->string in)
                (string->jsexpr _)
                (hash-for-each _ (λ (symbol quotes-hash)
                                   (with-handlers ([exn:fail? (λ (e) (displayln (string-append "Failed to process " (symbol->string symbol)
                                                                                               " for date " (~t (folder-date) "yyyy-MM-dd")))
                                                                (displayln e))])
                                     (query-exec dbc "
insert into mstar.symbol (
  act_symbol,
  exchange,
  morningstar_id
) values (
  $1,
  $2,
  $3
) on conflict (morningstar_id) do nothing;
"
                                                 (hash-ref (hash-ref quotes-hash 'ticker) 'value)
                                                 (hash-ref (hash-ref quotes-hash 'exchange) 'value)
                                                 (hash-ref (hash-ref quotes-hash 'performanceID) 'value))
                                     (set! insert-count (add1 insert-count))))))))))))

(displayln (string-append "Inserted or updated " (number->string insert-count) " rows for " (number->string symbol-count)
                          " symbols on " (date->iso8601 (folder-date))))

(disconnect dbc)
