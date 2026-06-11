#lang racket/base

(require db
         gregor
         net/http-easy
         racket/cmdline
         racket/file
         racket/list
         racket/port
         tasks
         threading)

(define (download-income-statement symbol morningstar-id period)
  (make-directory* (string-append "/var/local/mstar/financial-statements/" (~t (today) "yyyy-MM-dd")))
  (with-handlers ([exn:fail?
                   (λ (error)
                     (displayln (string-append "Encountered error for " symbol))
                     (displayln error))])
    (call-with-output-file* (string-append "/var/local/mstar/financial-statements/" (~t (today) "yyyy-MM-dd") "/"
                                           symbol ".income-statement." period ".json")
      (λ (out)
        (~> (string-append "https://www.us-api.morningstar.com/sal/sal-service/stock/newfinancials/"
                           morningstar-id "/incomeStatement/detail?reportType=R&dataType="
                           period "&locale=en&languageId=en&clientId=MDC&component=sal-equity-financials&version=4.71.0")
            (get _ #:headers (hash 'Authorization (string-append "Bearer " token)))
            (response-body _)
            (write-bytes _ out)))
      #:exists 'replace)))

(define (download-balance-sheet symbol morningstar-id period)
  (make-directory* (string-append "/var/local/mstar/financial-statements/" (~t (today) "yyyy-MM-dd")))
  (with-handlers ([exn:fail?
                   (λ (error)
                     (displayln (string-append "Encountered error for " symbol))
                     (displayln error))])
    (call-with-output-file* (string-append "/var/local/mstar/financial-statements/" (~t (today) "yyyy-MM-dd") "/"
                                           symbol ".balance-sheet." period ".json")
      (λ (out)
        (~> (string-append "https://www.us-api.morningstar.com/sal/sal-service/stock/newfinancials/"
                           morningstar-id "/balanceSheet/detail?reportType=R&dataType="
                           period "&locale=en&languageId=en&clientId=MDC&component=sal-equity-financials&version=4.71.0")
            (get _ #:headers (hash 'Authorization (string-append "Bearer " token)))
            (response-body _)
            (write-bytes _ out)))
      #:exists 'replace)))

(define (download-cash-flow-statement symbol morningstar-id period)
  (make-directory* (string-append "/var/local/mstar/financial-statements/" (~t (today) "yyyy-MM-dd")))
  (with-handlers ([exn:fail?
                   (λ (error)
                     (displayln (string-append "Encountered error for " symbol))
                     (displayln error))])
    (call-with-output-file* (string-append "/var/local/mstar/financial-statements/" (~t (today) "yyyy-MM-dd") "/"
                                           symbol ".cash-flow-statement." period ".json")
      (λ (out)
        (~> (string-append "https://www.us-api.morningstar.com/sal/sal-service/stock/newfinancials/"
                           morningstar-id "/cashFlow/detail?reportType=R&dataType="
                           period "&locale=en&languageId=en&clientId=MDC&component=sal-equity-financials&version=4.71.0")
            (get _ #:headers (hash 'Authorization (string-append "Bearer " token)))
            (response-body _)
            (write-bytes _ out)))
      #:exists 'replace)))

(define db-user (make-parameter "user"))

(define db-name (make-parameter "local"))

(define db-pass (make-parameter ""))

(define first-symbol (make-parameter ""))

(define last-symbol (make-parameter ""))

(define user-agent (make-parameter ""))

(define waf-token (make-parameter ""))

(command-line
 #:program "racket financial-statements-extract.rkt"
 #:once-each
 [("-a" "--user-agent") agent
                        "User Agent"
                        (user-agent agent)]
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
                     (db-user user)]
 [("-w" "--waf-token") token
                       "AWS WAF Token"
                       (waf-token token)])

(define dbc (postgresql-connect #:user (db-user) #:database (db-name) #:password (db-pass)))

(define symbols (query-rows dbc "
select
  ns.act_symbol,
  ms.morningstar_id
from
  nasdaq.symbol ns
join
  mstar.symbol ms
on
  ns.act_symbol = ms.act_symbol
where
  ns.is_etf = false and
  ns.is_test_issue = false and
  ns.is_next_shares = false and
  ns.security_name !~ 'ETN' and
  ns.nasdaq_symbol !~ '[-\\$\\+\\*#!@%\\^=~]' and
  case when ns.nasdaq_symbol ~ '[A-Z]{4}[L-Z]'
    then ns.security_name !~ '(Note|Preferred|Right|Unit|Warrant)'
    else true
  end and
  ns.last_seen = (select max(last_seen) from nasdaq.symbol) and
  case when $1 != ''
    then ns.act_symbol >= $1
    else true
  end and
  case when $2 != ''
    then ns.act_symbol <= $2
    else true
  end
order by
  ns.act_symbol;
"
                            (first-symbol)
                            (last-symbol)))

(disconnect dbc)

(define (get-token) (~> (get "https://www.morningstar.com/api/v2/stores/maas/token"
                             #:user-agent (user-agent)
                             #:headers (hash 'x-aws-waf-token (waf-token)))
                        (response-body _)
                        (bytes->string/utf-8 _)))

(define token (get-token))

(define delay-interval 36)

(define delays (map (λ (x) (* delay-interval x)) (range 0 (length symbols))))

(with-task-server (for-each (λ (l)
                              (schedule-delayed-task (λ () (cond [(= 0 (modulo (second l) 1800)) (thread (λ () (set! token (get-token))))])
                                                       (thread (λ () (download-balance-sheet (vector-ref (first l) 0)
                                                                                             (vector-ref (first l) 1)
                                                                                             "A"))))
                                                     (second l))
                              (schedule-delayed-task (λ () (thread (λ () (download-balance-sheet (vector-ref (first l) 0)
                                                                                                 (vector-ref (first l) 1)
                                                                                                 "Q"))))
                                                     (+ 6 (second l)))
                              (schedule-delayed-task (λ () (thread (λ () (download-cash-flow-statement (vector-ref (first l) 0)
                                                                                                       (vector-ref (first l) 1)
                                                                                                       "A"))))
                                                     (+ 12 (second l)))
                              (schedule-delayed-task (λ () (thread (λ () (download-cash-flow-statement (vector-ref (first l) 0)
                                                                                                       (vector-ref (first l) 1)
                                                                                                       "Q"))))
                                                     (+ 18 (second l)))
                              (schedule-delayed-task (λ () (thread (λ () (download-income-statement (vector-ref (first l) 0)
                                                                                                    (vector-ref (first l) 1)
                                                                                                    "A"))))
                                                     (+ 24 (second l)))
                              (schedule-delayed-task (λ () (thread (λ () (download-income-statement (vector-ref (first l) 0)
                                                                                                    (vector-ref (first l) 1)
                                                                                                    "Q"))))
                                                     (+ 30 (second l))))
                            (map list symbols delays))
  ; add a final task that will halt the task server
  (schedule-delayed-task (λ () (schedule-stop-task)) (* delay-interval (length delays)))
  (run-tasks))
