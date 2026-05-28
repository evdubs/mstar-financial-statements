#lang racket/base

(require db
         gregor
         net/http-easy
         racket/cmdline
         racket/file
         racket/list
         racket/port
         racket/string
         tasks
         threading)

(define (list-partition l period step)
  (if (>= period (length l)) (list l)
      (append (list (take l period))
              (list-partition (drop l step) period step))))

(define (download-quotes symbols)
  (make-directory* (string-append "/var/local/mstar/quotes/" (~t (today) "yyyy-MM-dd")))
  (call-with-output-file* (string-append "/var/local/mstar/quotes/" (~t (today) "yyyy-MM-dd") "/"
                                         (first symbols) "-" (last symbols) ".json")
    (λ (out)
      (with-handlers ([exn:fail?
                       (λ (error)
                         (displayln (string-append "Encountered error for " (first symbols) "-" (last symbols)))
                         (displayln error))])
        (~> (string-append "https://www.morningstar.com/api/v2/stores/realtime/quotes?securities=" (string-join symbols ","))
            (get _)
            (response-body _)
            (write-bytes _ out))))
    #:exists 'replace))

(define db-user (make-parameter "user"))

(define db-name (make-parameter "local"))

(define db-pass (make-parameter ""))

(define first-symbol (make-parameter ""))

(define last-symbol (make-parameter ""))

(command-line
 #:program "racket quotes-extract.rkt"
 #:once-each
 [("-f" "--first-symbol") first
                          "First symbol to query. Defaults to nothing"
                          (first-symbol first)]
 [("-l" "--last-symbol") last
                         "Last symbol to query. Defaults to nothing"
                         (last-symbol last)]
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

(define symbols (query-list dbc "
select
  case listing_exchange
    when 'NYSE MKT' then 'xase:' || lower(act_symbol)
    when 'NYSE' then 'xnys:' || lower(act_symbol)
    when 'NYSE ARCA' then 'arcx:' || lower(act_symbol)
    when 'NASDAQ' then 'xnas:' || lower(act_symbol)
    when 'IEXG' then 'iexg:' || lower(act_symbol)
    when 'BATS' then 'bats:' || lower(act_symbol)
    when 'CHX' then 'xchi:' || lower(act_symbol)
  end
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
  last_seen = (select max(last_seen) from nasdaq.symbol) and
  case when $1 != ''
    then act_symbol >= $1
    else true
  end and
  case when $2 != ''
    then act_symbol <= $2
    else true
  end
order by
  act_symbol;
"
                            (first-symbol)
                            (last-symbol)))

(disconnect dbc)

(define grouped-symbols (list-partition symbols 100 100))

(define delay-interval 10)

(define delays (map (λ (x) (* delay-interval x)) (range 0 (length grouped-symbols))))

(with-task-server (for-each (λ (l) (schedule-delayed-task (λ () (thread (λ () (download-quotes (first l)))))
                                                          (second l)))
                            (map list grouped-symbols delays))
  ; add a final task that will halt the task server
  (schedule-delayed-task (λ () (schedule-stop-task)) (* delay-interval (length delays)))
  (run-tasks))
