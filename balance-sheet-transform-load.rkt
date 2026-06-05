#lang racket/base

(require db
         gregor
         json
         racket/cmdline
         racket/format
         racket/list
         racket/port
         racket/sequence
         racket/set
         racket/string
         threading)

(define base-folder (make-parameter "/var/local/mstar/financial-statements"))

(define folder-date (make-parameter (today)))

(define db-user (make-parameter "user"))

(define db-name (make-parameter "local"))

(define db-pass (make-parameter ""))

(command-line
 #:program "racket balance-sheet-transform-load.rkt"
 #:once-each
 [("-b" "--base-folder") folder
                         "Morningstar balance sheet base folder. Defaults to /var/local/mstar/financial-statements"
                         (base-folder folder)]
 [("-d" "--folder-date") date
                         "Morningstar balance sheet folder date. Defaults to today"
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

(read-decimal-as-inexact #f)

(define dbc (postgresql-connect #:user (db-user) #:database (db-name) #:password (db-pass)))

(define (list-ref-or-else list index default-value)
  (if (>= index (length list)) default-value (list-ref list index)))

(define (extract-from-data-point-id json tag data-point-id)
  (~> (hash-ref json tag (λ () (list (hash))))
      (filter (λ (row) (equal? data-point-id (hash-ref row 'dataPointId (λ () "")))) _)
      (list-ref-or-else _ 0 (hash))))

(define known-current-assets-data-points
  (mutable-set "IFBS000320 Cash and Cash Equivalents"
               "IFBS000350 Cash, Cash Equivalents and Short Term Investments"
               "IFBS000680 Deferred Tax Assets, Current"
               "IFBS001130 Inventories"
               "IFBS001550 Other Current Assets"
               "IFBS001690 Short Term Investments"
               "IFBS002600 Assets Held for Sale/Discontinued Operations, Current"
               "IFBS002607 Cash Restricted or Pledged, Current"
               "IFBS002631 Derivative Investment and Hedging Assets, Current"
               "IFBS002959 Regulatory Assets, Current"
               "IFBS100688 Prepayments and Deposits, Current"
               "IFBS200660 Trade and Other Receivables, Current"
               "IFBS200665 Deferred Costs/Assets, Current"))

(define current-assets-data-points (mutable-set))

(define known-non-current-assets-data-points
  (mutable-set "IFBS000570 Deferred Costs/Assets, Non-Current"
               "IFBS000700 Deferred Tax Assets, Non-Current"
               "IFBS000720 Pension and Other Employee Benefits, Non-Current"
               "IFBS001020 Net Intangible Assets"
               "IFBS001450 Net Property, Plant and Equipment"
               "IFBS001650 Other Non-Current Assets"
               "IFBS002310 Total Long Term Investments"
               "IFBS002601 Assets Held for Sale/Discontinued Operations, Non-Current"
               "IFBS002668 Inventories, Non-Current"
               "IFBS002950 Net Mineral Property Interests and Exploration Assets"
               "IFBS002955 Regulatory Assets, Non-Current"
               "IFBS100681 Investment Properties and Properties Held for Development"
               "IFBS100733 Prepayments and Deposits, Non-Current"
               "IFBS200010 Biological Assets"
               "IFBS200020 Trade and Other Receivables, Non-Current"
               "IFBS200060 Cash Restricted or Pledged, Non-Current"
               "IFBS200663 Derivative Investment and Hedging Assets, Non-Current"))

(define non-current-assets-data-points (mutable-set))

(define known-current-liabilities-data-points
  (mutable-set "IFBS000610 Deferred Liabilities, Current"
               "IFBS001570 Other Current Liabilities"
               "IFBS001890 Provisions, Current"
               "IFBS002673 Discontinued Operations Liabilities, Current"
               "IFBS002716 Payables and Accrued Expenses, Current"
               "IFBS003020 Regulatory Liabilities, Current"
               "IFBS003145 Tax Liabilities, Current"
               "IFBS100734 Financial Liabilities, Current"))

(define current-liabilities-data-points (mutable-set))

(define known-non-current-liabilities-data-points
  (mutable-set "IFBS001780 Preferred Securities Outside Stock Equity"
               "IFBS001900 Provisions, Non-Current"
               "IFBS002080 Restricted Common Stock"
               "IFBS002480 Other Non-Current Liabilities"
               "IFBS002674 Liabilities Held for Sale/Discontinued Operations, Non-Current"
               "IFBS002717 Payables and Accrued Expenses, Non-Current"
               "IFBS002805 Deferred Liabilities, Non-Current"
               "IFBS002825 Regulatory Liabilities, Non-Current"
               "IFBS003143 Tax Liabilities, Non-Current"
               "IFBS100735 Financial Liabilities, Non-Current"))

(define non-current-liabilities-data-points (mutable-set))

(define equity-data-points (mutable-set))

(define known-equity-data-points
  (mutable-set "IFBS001381 Non-Controlling/Minority Interests in Equity"
               "IFBS002350 Total Partnership Capital"
               "IFBS200140 Equity Attributable to Parent Stockholders"))

(define parent-equity-data-points (mutable-set))

(define known-parent-equity-data-points
  (mutable-set "IFBS002100 Retained Earnings/Accumulated Deficit"
               "IFBS002737 Reserves/Accumulated Comprehensive Income/Losses"
               "IFBS002777 Stock Options/Warrants/DeferredShares/ConvertibleDebentures"
               "IFBS002868 Stock Subscription"
               "IFBS003308 Paid in Capital"
               "IFBS003313 Equity Attributable to Holders of Perpetual Capital Securities"
               "IFBS200130 Other Equity Interest"))

(parameterize ([current-directory (string-append (base-folder) "/" (~t (folder-date) "yyyy-MM-dd") "/")])
  (for ([p (sequence-filter (λ (p) (string-contains? (path->string p) ".balance-sheet.")) (in-directory (current-directory)))])
    (let* ([file-name (path->string p)]
           [ticker-symbol (regexp-replace #rx".balance-sheet.[AQ].json" (string-replace file-name (path->string (current-directory)) "") "")])
      (call-with-input-file file-name
        (λ (in)
          (with-handlers ([exn:fail? (λ (e) (displayln (string-append "Failed to process the balance sheet for " ticker-symbol))
                                       (displayln e))])
            (define balance-sheet-json (~> (port->string in)
                                           (string->jsexpr _)))
            (define period (cond [(string-contains? file-name ".balance-sheet.A.json") 'Year]
                                 [(string-contains? file-name ".balance-sheet.Q.json") 'Quarter]))
            (define dates (cond [(equal? period 'Year)
                                 (map (λ (column) (iso8601->date (string-append column "-" (~> (hash-ref balance-sheet-json 'footer)
                                                                                               (hash-ref _ 'fiscalYearEndDate)))))
                                      (hash-ref balance-sheet-json 'columnDefs))]
                                [(equal? period 'Quarter)
                                 (map (λ (column)
                                        (define quarter (first (string-split column " ")))
                                        (define year (string->number (last (string-split column " "))))
                                        (define end-month (~> (hash-ref balance-sheet-json '_meta)
                                                              (hash-ref _ 'quarters)
                                                              (filter (λ (q) (equal? quarter (hash-ref q 'quarter))) _)
                                                              (first _)
                                                              (hash-ref _ 'to)))
                                        ; Q1 2026 will mean Feb - Apr in 2025 if the fiscal year ends in Jan 2026.
                                        ; we need to adjust the year as a result.
                                        (cond [(and (equal? "Q1" quarter)
                                                    (< 3 end-month))
                                               (set! year (sub1 year))]
                                              [(and (equal? "Q2" quarter)
                                                    (< 6 end-month))
                                               (set! year (sub1 year))]
                                              [(and (equal? "Q3" quarter)
                                                    (< 9 end-month))
                                               (set! year (sub1 year))])
                                        (-days (+months (date year end-month) 1) 1))
                                      (hash-ref balance-sheet-json 'columnDefs))]))

            (define currency (~> (hash-ref balance-sheet-json 'footer)
                                 (hash-ref _ 'currency)))

            (define order-of-magnitude (~> (hash-ref balance-sheet-json 'footer)
                                           (hash-ref _ 'orderOfMagnitude)))

            (define multiplier (cond [(equal? "Billion" order-of-magnitude) 1000000000]
                                     [(equal? "Million" order-of-magnitude) 1000000]
                                     [(equal? "Thousand" order-of-magnitude) 1000]))
            ; (displayln dates)

            (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                (hash-ref _ 'subLevel (λ () (list)))
                (for-each (λ (ca) (set-add! current-assets-data-points
                                            (string-append (hash-ref ca 'dataPointId) " "
                                                           (hash-ref ca 'label)))) _))

            (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                (hash-ref _ 'subLevel (λ () (list)))
                (for-each (λ (ca) (set-add! non-current-assets-data-points
                                            (string-append (hash-ref ca 'dataPointId) " "
                                                           (hash-ref ca 'label)))) _))

            (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                (hash-ref _ 'subLevel (λ () (list)))
                (for-each (λ (ca) (set-add! current-liabilities-data-points
                                            (string-append (hash-ref ca 'dataPointId) " "
                                                           (hash-ref ca 'label)))) _))

            (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                (hash-ref _ 'subLevel (λ () (list)))
                (for-each (λ (ca) (set-add! non-current-liabilities-data-points
                                            (string-append (hash-ref ca 'dataPointId) " "
                                                           (hash-ref ca 'label)))) _))

            (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                (extract-from-data-point-id _ 'subLevel "IFBS200140") ; Equity Attributable to Parent Stockholders
                (hash-ref _ 'subLevel (λ () (list)))
                (for-each (λ (ca) (set-add! parent-equity-data-points
                                            (string-append (hash-ref ca 'dataPointId) " "
                                                           (hash-ref ca 'label)))) _))

            (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                (hash-ref _ 'subLevel (λ () (list)))
                (for-each (λ (ca) (set-add! equity-data-points
                                            (string-append (hash-ref ca 'dataPointId) " "
                                                           (hash-ref ca 'label)))) _))

            (define cash-and-equivalents-sub
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000350") ; Cash, Cash Equivalents and Short Term Investments
                  (extract-from-data-point-id _ 'subLevel "IFBS000320") ; Cash and Cash Equivalents
                  (hash-ref _ 'datum (λ () (list)))))
            (define cash-and-equivalents-sup
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000320") ; Cash and Cash Equivalents
                  (hash-ref _ 'datum (λ () (list)))))

            (define short-term-investments
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000350") ; Cash, Cash Equivalents and Short Term Investments
                  (extract-from-data-point-id _ 'subLevel "IFBS001690") ; Short Term Investments
                  (hash-ref _ 'datum (λ () (list)))))

            (define cash-and-equivalents-and-short-term-investments
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000350") ; Cash, Cash Equivalents and Short Term Investments
                  (hash-ref _ 'datum (λ () (list)))))

            (define restricted-cash-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002607") ; Cash Restricted or Pledged, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define inventories
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS001130") ; Inventories
                  (hash-ref _ 'datum (λ () (list)))))

            (define accounts-receivable-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS200660") ; Trade and Other Receivables, Current
                  (extract-from-data-point-id _ 'subLevel "IFBS000020") ; Trade/Accounts Receivable, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define other-receivables-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS200660") ; Trade and Other Receivables, Current
                  (extract-from-data-point-id _ 'subLevel "IFBS001680") ; Other Receivables, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define trade-and-other-receivables-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS200660") ; Trade and Other Receivables, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define deferred-tax-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000680") ; Deferred Tax Assets, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define deferred-costs-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS200665") ; Deferred Costs/Assets, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define assets-held-for-sale-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002600") ; Assets Held for Sale/Discontinued Operations, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define derivatives-investments-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002631") ; Derivative Investment and Hedging Assets, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define regulatory-assets-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002959") ; Regulatory Assets, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define prepayments-and-deposits-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS100688") ; Prepayments and Deposits, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define other-current-assets
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS001550") ; Other Current Assets
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-current-assets
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000470") ; Total Current Assets
                  (hash-ref _ 'datum (λ () (list)))))

            (define net-property-plant-and-equipment
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS001450") ; Net Property, Plant and Equipment
                  (hash-ref _ 'datum (λ () (list)))))

            (define intangibles
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS001020") ; Net Intangible Assets
                  (hash-ref _ 'datum (λ () (list)))))

            (define trade-and-other-receivables-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS200020") ; Trade and Other Receivables, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define prepayments-and-deposits-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS100733") ; Prepayments and Deposits, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define deferred-tax-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000700") ; Deferred Tax Assets, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define deferred-costs-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000570") ; Deferred Costs/Assets, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define pension-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS000720") ; Pension and Other Employee Benefits, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define long-term-investments
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002310") ; Total Long Term Investments
                  (hash-ref _ 'datum (λ () (list)))))

            (define assets-held-for-sale-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002601") ; Assets Held for Sale/Discontinued Operations, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define inventories-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002668") ; Inventories, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define mineral-property-interests
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002950") ; Net Mineral Property Interests and Exploration Assets
                  (hash-ref _ 'datum (λ () (list)))))

            (define regulatory-assets-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002955") ; Regulatory Assets, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define investment-properties
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS100681") ; Investment Properties and Properties Held for Development
                  (hash-ref _ 'datum (λ () (list)))))

            (define biological-assets
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS200010") ; Biological Assets
                  (hash-ref _ 'datum (λ () (list)))))

            (define restricted-cash-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS200060") ; Cash Restricted or Pledged, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define derivatives-investments-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS200663") ; Derivative Investment and Hedging Assets, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define other-non-current-assets
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS001650") ; Other Non-Current Assets
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-non-current-assets
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (extract-from-data-point-id _ 'subLevel "IFBS002330") ; Total Non-Current Assets
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-assets
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002270") ; Total Assets
                  (hash-ref _ 'datum (λ () (list)))))

            (define accounts-payable-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002716") ; Payables and Accrued Expenses, Current
                  (extract-from-data-point-id _ 'subLevel "IFBS001710") ; Trade and Other Payables, Current
                  (extract-from-data-point-id _ 'subLevel "IFBS000010") ; Trade/Accounts Payable, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define taxes-payable-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002716") ; Payables and Accrued Expenses, Current
                  (extract-from-data-point-id _ 'subLevel "IFBS001710") ; Trade and Other Payables, Current
                  (extract-from-data-point-id _ 'subLevel "IFBS002240") ; Taxes Payable, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define trade-and-other-payables-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002716") ; Payables and Accrued Expenses, Current
                  (extract-from-data-point-id _ 'subLevel "IFBS001710") ; Trade and Other Payables, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define accrued-expenses-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002716") ; Payables and Accrued Expenses, Current
                  (extract-from-data-point-id _ 'subLevel "IFBS002541") ; Accrued Expenses, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define payables-and-accrued-expenses-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002716") ; Payables and Accrued Expenses, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define current-portion-of-long-term-debt
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS100734") ; Financial Liabilities, Current
                  (extract-from-data-point-id _ 'subLevel "IFBS002965") ; Current Debt and Capital Lease Obligation
                  (extract-from-data-point-id _ 'subLevel "IFBS003330") ; Current Portion of Long Term Debt and Capital Lease
                  (extract-from-data-point-id _ 'subLevel "IFBS002617") ; Current Portion of Long Term Debt
                  (hash-ref _ 'datum (λ () (list)))))

            (define capital-lease-obligations-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS100734") ; Financial Liabilities, Current
                  (extract-from-data-point-id _ 'subLevel "IFBS002965") ; Current Debt and Capital Lease Obligation
                  (extract-from-data-point-id _ 'subLevel "IFBS003330") ; Current Portion of Long Term Debt and Capital Lease
                  (extract-from-data-point-id _ 'subLevel "IFBS100694") ; Capital Lease Obligations, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define current-debt
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS100734") ; Financial Liabilities, Current
                  (extract-from-data-point-id _ 'subLevel "IFBS002965") ; Current Debt and Capital Lease Obligation
                  (extract-from-data-point-id _ 'subLevel "IFBS000480") ; Current Debt
                  (hash-ref _ 'datum (λ () (list)))))

            (define current-debt-and-capital-lease-obligation
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS100734") ; Financial Liabilities, Current
                  (extract-from-data-point-id _ 'subLevel "IFBS002965") ; Current Debt and Capital Lease Obligation
                  (hash-ref _ 'datum (λ () (list)))))

            (define financial-liabilities-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS100734") ; Financial Liabilities, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define provisions-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS001890") ; Provisions, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define deferred-liabilities-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000610") ; Deferred Liabilities, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define discontinued-operations-liabilities-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002673") ; Discontinued Operations Liabilities, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define regulatory-liabilities-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS003020") ; Regulatory Liabilities, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define tax-liabilities-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS003145") ; Tax Liabilities, Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define other-current-liabilities
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS001570") ; Other Current Liabilities
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-current-liabilities
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS000500") ; Total Current Liabilities
                  (hash-ref _ 'datum (λ () (list)))))

            (define long-term-debt
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS100735") ; Financial Liabilities, Non-Current
                  (extract-from-data-point-id _ 'subLevel "IFBS002961") ; Long Term Debt and Capital Lease Obligation
                  (extract-from-data-point-id _ 'subLevel "IFBS001290") ; Long Term Debt
                  (hash-ref _ 'datum (λ () (list)))))

            (define financial-liabilities-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS100735") ; Financial Liabilities, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define provisions-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS001900") ; Provisions, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define deferred-tax-liabilities-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS003143") ; Tax Liabilities, Non-Current
                  (extract-from-data-point-id _ 'subLevel "IFBS000710") ; Deferred Tax Liabilities, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define tax-liabilities-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS003143") ; Tax Liabilities, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define deferred-income-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002805") ; Deferred Liabilities, Non-Current
                  (extract-from-data-point-id _ 'subLevel "IFBS000660") ; Deferred Income/Customer Advances/Billings in Excess of Cost, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define deferred-liabilities-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002805") ; Deferred Liabilities, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define payables-and-accrued-expenses-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002717") ; Payables and Accrued Expenses, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define preferred-securities-outside-stock-equity
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS001780") ; Preferred Securities Outside Stock Equity
                  (hash-ref _ 'datum (λ () (list)))))

            (define restricted-common-stock
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002080") ; Restricted Common Stock
                  (hash-ref _ 'datum (λ () (list)))))

            (define liabilities-held-for-sale-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002674") ; Liabilities Held for Sale/Discontinued Operations, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define regulatory-liabilities-non-current
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002825") ; Regulatory Liabilities, Non-Current
                  (hash-ref _ 'datum (λ () (list)))))

            (define other-non-current-liabilities
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002480") ; Other Non-Current Liabilities
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-non-current-liabilities
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (extract-from-data-point-id _ 'subLevel "IFBS002647") ; Total Non-Current Liabilities
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-liabilities
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002646") ; Total Liabilities
                  (hash-ref _ 'datum (λ () (list)))))

            (define preferred-stock
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS200140") ; Equity Attributable to Parent Stockholders
                  (extract-from-data-point-id _ 'subLevel "IFBS003308") ; Paid in Capital
                  (extract-from-data-point-id _ 'subLevel "IFBS000290") ; Capital Stock
                  (extract-from-data-point-id _ 'subLevel "IFBS001800") ; Preferred Stock
                  (hash-ref _ 'datum (λ () (list)))))

            (define common-stock
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS200140") ; Equity Attributable to Parent Stockholders
                  (extract-from-data-point-id _ 'subLevel "IFBS003308") ; Paid in Capital
                  (extract-from-data-point-id _ 'subLevel "IFBS000290") ; Capital Stock
                  (extract-from-data-point-id _ 'subLevel "IFBS000400") ; Common Stock
                  (hash-ref _ 'datum (λ () (list)))))

            (define additional-paid-in-capital
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS200140") ; Equity Attributable to Parent Stockholders
                  (extract-from-data-point-id _ 'subLevel "IFBS003308") ; Paid in Capital
                  (extract-from-data-point-id _ 'subLevel "IFBS000290") ; Capital Stock
                  (extract-from-data-point-id _ 'subLevel "IFBS000140") ; Additional Paid in Capital/Share Premium
                  (hash-ref _ 'datum (λ () (list)))))

            (define capital-stock
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS200140") ; Equity Attributable to Parent Stockholders
                  (extract-from-data-point-id _ 'subLevel "IFBS003308") ; Paid in Capital
                  (extract-from-data-point-id _ 'subLevel "IFBS000290") ; Capital Stock
                  (hash-ref _ 'datum (λ () (list)))))

            (define treasury-stock
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS200140") ; Equity Attributable to Parent Stockholders
                  (extract-from-data-point-id _ 'subLevel "IFBS003308") ; Paid in Capital
                  (extract-from-data-point-id _ 'subLevel "IFBS002390") ; Treasury Stock
                  (hash-ref _ 'datum (λ () (list)))))

            (define paid-in-capital
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS200140") ; Equity Attributable to Parent Stockholders
                  (extract-from-data-point-id _ 'subLevel "IFBS003308") ; Paid in Capital
                  (hash-ref _ 'datum (λ () (list)))))

            (define stock-options-warrants-deferred-shares-convertible-debentures
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS200140") ; Equity Attributable to Parent Stockholders
                  (extract-from-data-point-id _ 'subLevel "IFBS002777") ; Stock Options/Warrants/DeferredShares/ConvertibleDebentures
                  (hash-ref _ 'datum (λ () (list)))))

            (define retained-earnings
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS200140") ; Equity Attributable to Parent Stockholders
                  (extract-from-data-point-id _ 'subLevel "IFBS002100") ; Retained Earnings/Accumulated Deficit
                  (hash-ref _ 'datum (λ () (list)))))

            (define reserves
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS200140") ; Equity Attributable to Parent Stockholders
                  (extract-from-data-point-id _ 'subLevel "IFBS002737") ; Reserves/Accumulated Comprehensive Income/Losses
                  (hash-ref _ 'datum (λ () (list)))))

            (define stock-subscription
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS200140") ; Equity Attributable to Parent Stockholders
                  (extract-from-data-point-id _ 'subLevel "IFBS002868") ; Stock Subscription
                  (hash-ref _ 'datum (λ () (list)))))

            (define perpetual-capital-securities-holder-equity
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS200140") ; Equity Attributable to Parent Stockholders
                  (extract-from-data-point-id _ 'subLevel "IFBS003313") ; Equity Attributable to Holders of Perpetual Capital Securities
                  (hash-ref _ 'datum (λ () (list)))))

            (define other-equity-interest
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS200140") ; Equity Attributable to Parent Stockholders
                  (extract-from-data-point-id _ 'subLevel "IFBS200130") ; Other Equity Interest
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-parent-stockholder-equity
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS200140") ; Equity Attributable to Parent Stockholders
                  (hash-ref _ 'datum (λ () (list)))))

            (define minority-interest-in-equity
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS001381") ; Non-Controlling/Minority Interests in Equity
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-partnership-capital
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (extract-from-data-point-id _ 'subLevel "IFBS002350") ; Total Partnership Capital
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-equity
              (~> (extract-from-data-point-id balance-sheet-json 'rows "IFBS000000") ; BalanceSheet
                  (extract-from-data-point-id _ 'subLevel "IFBS002220") ; Total Equity
                  (hash-ref _ 'datum (λ () (list)))))

            (for-each
             (λ (date i)
               (cond [(not (equal? "_PO_" (~a (list-ref-or-else total-assets i "null"))))
                      (query-exec dbc "
insert into mstar.balance_sheet_assets (
  act_symbol,
  date,
  period,
  currency,
  cash_and_equivalents, -- 5
  short_term_investments,
  cash_and_equivalents_and_short_term_investments,
  restricted_cash_current,
  inventories,
  accounts_receivable_current, -- 10
  other_receivables_current,
  trade_and_other_receivables_current,
  deferred_tax_current,
  deferred_costs_current,
  assets_held_for_sale_current, -- 15
  derivatives_investments_current,
  regulatory_assets_current,
  prepayments_and_deposits_current,
  other_current_assets,
  total_current_assets, -- 20
  net_property_plant_and_equipment,
  intangibles,
  trade_and_other_receivables_non_current,
  prepayments_and_deposits_non_current,
  deferred_tax_non_current, -- 25
  deferred_costs_non_current,
  pension_non_current,
  long_term_investments,
  assets_held_for_sale_non_current,
  inventories_non_current, -- 30
  mineral_property_interests,
  regulatory_assets_non_current,
  investment_properties,
  biological_assets,
  restricted_cash_non_current, -- 35
  derivatives_investments_non_current,
  other_non_current_assets,
  total_non_current_assets,
  total_assets
) values (
  $1,
  $2::text::date,
  $3::text::mstar.statement_period,
  $4,
  case when $5::text = '_PO_' or $5::text = 'null' then null else $5::text::numeric * $40 end,
  case when $6::text = '_PO_' or $6::text = 'null' then null else $6::text::numeric * $40 end,
  case when $7::text = '_PO_' or $7::text = 'null' then null else $7::text::numeric * $40 end,
  case when $8::text = '_PO_' or $8::text = 'null' then null else $8::text::numeric * $40 end,
  case when $9::text = '_PO_' or $9::text = 'null' then null else $9::text::numeric * $40 end,
  case when $10::text = '_PO_' or $10::text = 'null' then null else $10::text::numeric * $40 end,
  case when $11::text = '_PO_' or $11::text = 'null' then null else $11::text::numeric * $40 end,
  case when $12::text = '_PO_' or $12::text = 'null' then null else $12::text::numeric * $40 end,
  case when $13::text = '_PO_' or $13::text = 'null' then null else $13::text::numeric * $40 end,
  case when $14::text = '_PO_' or $14::text = 'null' then null else $14::text::numeric * $40 end,
  case when $15::text = '_PO_' or $15::text = 'null' then null else $15::text::numeric * $40 end,
  case when $16::text = '_PO_' or $16::text = 'null' then null else $16::text::numeric * $40 end,
  case when $17::text = '_PO_' or $17::text = 'null' then null else $17::text::numeric * $40 end,
  case when $18::text = '_PO_' or $18::text = 'null' then null else $18::text::numeric * $40 end,
  case when $19::text = '_PO_' or $19::text = 'null' then null else $19::text::numeric * $40 end,
  case when $20::text = '_PO_' or $20::text = 'null' then null else $20::text::numeric * $40 end,
  case when $21::text = '_PO_' or $21::text = 'null' then null else $21::text::numeric * $40 end,
  case when $22::text = '_PO_' or $22::text = 'null' then null else $22::text::numeric * $40 end,
  case when $23::text = '_PO_' or $23::text = 'null' then null else $23::text::numeric * $40 end,
  case when $24::text = '_PO_' or $24::text = 'null' then null else $24::text::numeric * $40 end,
  case when $25::text = '_PO_' or $25::text = 'null' then null else $25::text::numeric * $40 end,
  case when $26::text = '_PO_' or $26::text = 'null' then null else $26::text::numeric * $40 end,
  case when $27::text = '_PO_' or $27::text = 'null' then null else $27::text::numeric * $40 end,
  case when $28::text = '_PO_' or $28::text = 'null' then null else $28::text::numeric * $40 end,
  case when $29::text = '_PO_' or $29::text = 'null' then null else $29::text::numeric * $40 end,
  case when $30::text = '_PO_' or $30::text = 'null' then null else $30::text::numeric * $40 end,
  case when $31::text = '_PO_' or $31::text = 'null' then null else $31::text::numeric * $40 end,
  case when $32::text = '_PO_' or $32::text = 'null' then null else $32::text::numeric * $40 end,
  case when $33::text = '_PO_' or $33::text = 'null' then null else $33::text::numeric * $40 end,
  case when $34::text = '_PO_' or $34::text = 'null' then null else $34::text::numeric * $40 end,
  case when $35::text = '_PO_' or $35::text = 'null' then null else $35::text::numeric * $40 end,
  case when $36::text = '_PO_' or $36::text = 'null' then null else $36::text::numeric * $40 end,
  case when $37::text = '_PO_' or $37::text = 'null' then null else $37::text::numeric * $40 end,
  case when $38::text = '_PO_' or $38::text = 'null' then null else $38::text::numeric * $40 end,
  case when $39::text = '_PO_' or $39::text = 'null' then null else $39::text::numeric * $40 end
) on conflict (act_symbol, date, period) do nothing;
"
                                  ticker-symbol
                                  (date->iso8601 date)
                                  (symbol->string period)
                                  currency
                                  (~a (list-ref-or-else cash-and-equivalents-sub i
                                                        (list-ref-or-else cash-and-equivalents-sup i "null"))) ; 5
                                  (~a (list-ref-or-else short-term-investments i "null"))
                                  (~a (list-ref-or-else cash-and-equivalents-and-short-term-investments i "null"))
                                  (~a (list-ref-or-else restricted-cash-current i "null"))
                                  (~a (list-ref-or-else inventories i "null"))
                                  (~a (list-ref-or-else accounts-receivable-current i "null")) ; 10
                                  (~a (list-ref-or-else other-receivables-current i "null"))
                                  (~a (list-ref-or-else trade-and-other-receivables-current i "null"))
                                  (~a (list-ref-or-else deferred-tax-current i "null"))
                                  (~a (list-ref-or-else deferred-costs-current i "null"))
                                  (~a (list-ref-or-else assets-held-for-sale-current i "null")) ; 15
                                  (~a (list-ref-or-else derivatives-investments-current i "null"))
                                  (~a (list-ref-or-else regulatory-assets-current i "null"))
                                  (~a (list-ref-or-else prepayments-and-deposits-current i "null"))
                                  (~a (list-ref-or-else other-current-assets i "null"))
                                  (~a (list-ref-or-else total-current-assets i "null")) ; 20
                                  (~a (list-ref-or-else net-property-plant-and-equipment i "null"))
                                  (~a (list-ref-or-else intangibles i "null"))
                                  (~a (list-ref-or-else trade-and-other-receivables-non-current i "null"))
                                  (~a (list-ref-or-else prepayments-and-deposits-non-current i "null"))
                                  (~a (list-ref-or-else deferred-tax-non-current i "null")) ; 25
                                  (~a (list-ref-or-else deferred-costs-non-current i "null"))
                                  (~a (list-ref-or-else pension-non-current i "null"))
                                  (~a (list-ref-or-else long-term-investments i "null"))
                                  (~a (list-ref-or-else assets-held-for-sale-non-current i "null"))
                                  (~a (list-ref-or-else inventories-non-current i "null")) ; 30
                                  (~a (list-ref-or-else mineral-property-interests i "null"))
                                  (~a (list-ref-or-else regulatory-assets-non-current i "null"))
                                  (~a (list-ref-or-else investment-properties i "null"))
                                  (~a (list-ref-or-else biological-assets i "null"))
                                  (~a (list-ref-or-else restricted-cash-non-current i "null")) ; 35
                                  (~a (list-ref-or-else derivatives-investments-non-current i "null"))
                                  (~a (list-ref-or-else other-non-current-assets i "null"))
                                  (~a (list-ref-or-else total-non-current-assets i "null"))
                                  (~a (list-ref-or-else total-assets i "null"))
                                  multiplier)]) ; 40

               (cond [(not (equal? "_PO_" (~a (list-ref-or-else total-liabilities i "null"))))
                      (query-exec dbc "
insert into mstar.balance_sheet_liabilities (
  act_symbol,
  date,
  period,
  currency,
  accounts_payable_current, -- 5
  taxes_payable_current,
  trade_and_other_payables_current,
  accrued_expenses_current,
  payables_and_accrued_expenses_current,
  current_portion_long_term_debt, -- 10
  capital_lease_obligations_current,
  current_debt,
  current_debt_and_capital_lease_obligation,
  financial_liabilities_current,
  provisions_current, -- 15
  deferred_liabilities_current,
  discontinued_operations_liabilities_current,
  regulatory_liabilities_current,
  tax_liabilities_current,
  other_current_liabilities, -- 20
  total_current_liabilities,
  long_term_debt,
  financial_liabilities_non_current,
  provisions_non_current,
  deferred_tax_liabilities_non_current, -- 25
  tax_liabilities_non_current,
  deferred_income_non_current,
  deferred_liabilities_non_current,
  payables_and_accrued_expenses_non_current,
  preferred_securities_outside_stock_equity, -- 30
  restricted_common_stock,
  liabilities_held_for_sale_non_current,
  regulatory_liabilities_non_current,
  other_non_current_liabilities,
  total_non_current_liabilities, -- 35
  total_liabilities
) values (
  $1,
  $2::text::date,
  $3::text::mstar.statement_period,
  $4,
  case when $5::text = '_PO_' or $5::text = 'null' then null else $5::text::numeric * $37 end,
  case when $6::text = '_PO_' or $6::text = 'null' then null else $6::text::numeric * $37 end,
  case when $7::text = '_PO_' or $7::text = 'null' then null else $7::text::numeric * $37 end,
  case when $8::text = '_PO_' or $8::text = 'null' then null else $8::text::numeric * $37 end,
  case when $9::text = '_PO_' or $9::text = 'null' then null else $9::text::numeric * $37 end,
  case when $10::text = '_PO_' or $10::text = 'null' then null else $10::text::numeric * $37 end,
  case when $11::text = '_PO_' or $11::text = 'null' then null else $11::text::numeric * $37 end,
  case when $12::text = '_PO_' or $12::text = 'null' then null else $12::text::numeric * $37 end,
  case when $13::text = '_PO_' or $13::text = 'null' then null else $13::text::numeric * $37 end,
  case when $14::text = '_PO_' or $14::text = 'null' then null else $14::text::numeric * $37 end,
  case when $15::text = '_PO_' or $15::text = 'null' then null else $15::text::numeric * $37 end,
  case when $16::text = '_PO_' or $16::text = 'null' then null else $16::text::numeric * $37 end,
  case when $17::text = '_PO_' or $17::text = 'null' then null else $17::text::numeric * $37 end,
  case when $18::text = '_PO_' or $18::text = 'null' then null else $18::text::numeric * $37 end,
  case when $19::text = '_PO_' or $19::text = 'null' then null else $19::text::numeric * $37 end,
  case when $20::text = '_PO_' or $20::text = 'null' then null else $20::text::numeric * $37 end,
  case when $21::text = '_PO_' or $21::text = 'null' then null else $21::text::numeric * $37 end,
  case when $22::text = '_PO_' or $22::text = 'null' then null else $22::text::numeric * $37 end,
  case when $23::text = '_PO_' or $23::text = 'null' then null else $23::text::numeric * $37 end,
  case when $24::text = '_PO_' or $24::text = 'null' then null else $24::text::numeric * $37 end,
  case when $25::text = '_PO_' or $25::text = 'null' then null else $25::text::numeric * $37 end,
  case when $26::text = '_PO_' or $26::text = 'null' then null else $26::text::numeric * $37 end,
  case when $27::text = '_PO_' or $27::text = 'null' then null else $27::text::numeric * $37 end,
  case when $28::text = '_PO_' or $28::text = 'null' then null else $28::text::numeric * $37 end,
  case when $29::text = '_PO_' or $29::text = 'null' then null else $29::text::numeric * $37 end,
  case when $30::text = '_PO_' or $30::text = 'null' then null else $30::text::numeric * $37 end,
  case when $31::text = '_PO_' or $31::text = 'null' then null else $31::text::numeric * $37 end,
  case when $32::text = '_PO_' or $32::text = 'null' then null else $32::text::numeric * $37 end,
  case when $33::text = '_PO_' or $33::text = 'null' then null else $33::text::numeric * $37 end,
  case when $34::text = '_PO_' or $34::text = 'null' then null else $34::text::numeric * $37 end,
  case when $35::text = '_PO_' or $35::text = 'null' then null else $35::text::numeric * $37 end,
  case when $36::text = '_PO_' or $36::text = 'null' then null else $36::text::numeric * $37 end
) on conflict (act_symbol, date, period) do nothing;
"
                                  ticker-symbol
                                  (date->iso8601 date)
                                  (symbol->string period)
                                  currency
                                  (~a (list-ref-or-else accounts-payable-current i "null")) ; 5
                                  (~a (list-ref-or-else taxes-payable-current i "null"))
                                  (~a (list-ref-or-else trade-and-other-payables-current i "null"))
                                  (~a (list-ref-or-else accrued-expenses-current i "null"))
                                  (~a (list-ref-or-else payables-and-accrued-expenses-current i "null"))
                                  (~a (list-ref-or-else current-portion-of-long-term-debt i "null")) ; 10
                                  (~a (list-ref-or-else capital-lease-obligations-current i "null"))
                                  (~a (list-ref-or-else current-debt i "null"))
                                  (~a (list-ref-or-else current-debt-and-capital-lease-obligation i "null"))
                                  (~a (list-ref-or-else financial-liabilities-current i "null"))
                                  (~a (list-ref-or-else provisions-current i "null")) ; 15
                                  (~a (list-ref-or-else deferred-liabilities-current i "null"))
                                  (~a (list-ref-or-else discontinued-operations-liabilities-current i "null"))
                                  (~a (list-ref-or-else regulatory-liabilities-current i "null"))
                                  (~a (list-ref-or-else tax-liabilities-current i "null"))
                                  (~a (list-ref-or-else other-current-liabilities i "null")) ; 20
                                  (~a (list-ref-or-else total-current-liabilities i "null"))
                                  (~a (list-ref-or-else long-term-debt i "null"))
                                  (~a (list-ref-or-else financial-liabilities-non-current i "null"))
                                  (~a (list-ref-or-else provisions-non-current i "null"))
                                  (~a (list-ref-or-else deferred-tax-liabilities-non-current i "null")) ; 25
                                  (~a (list-ref-or-else tax-liabilities-non-current i "null"))
                                  (~a (list-ref-or-else deferred-income-non-current i "null"))
                                  (~a (list-ref-or-else deferred-liabilities-non-current i "null"))
                                  (~a (list-ref-or-else payables-and-accrued-expenses-non-current i "null"))
                                  (~a (list-ref-or-else preferred-securities-outside-stock-equity i "null")) ; 30
                                  (~a (list-ref-or-else restricted-common-stock i "null"))
                                  (~a (list-ref-or-else liabilities-held-for-sale-non-current i "null"))
                                  (~a (list-ref-or-else regulatory-liabilities-non-current i "null"))
                                  (~a (list-ref-or-else other-non-current-liabilities i "null"))
                                  (~a (list-ref-or-else total-non-current-liabilities i "null")) ; 35
                                  (~a (list-ref-or-else total-liabilities i "null"))
                                  multiplier)])
               

               (cond [(not (equal? "_PO_" (~a (list-ref-or-else total-equity i "null"))))
                      (query-exec dbc "
insert into mstar.balance_sheet_equity (
  act_symbol,
  date,
  period,
  currency,
  preferred_stock, -- 5
  common_stock,
  additional_paid_in_capital,
  capital_stock,
  treasury_stock,
  paid_in_capital, -- 10
  stock_options_warrants_deferred_shares_convertible_debentures,
  retained_earnings,
  reserves,
  stock_subscription,
  perpetual_capital_securities_holder_equity, -- 15
  other_equity_interest,
  total_parent_stockholder_equity,
  minority_interest_in_equity,
  total_partnership_capital,
  total_equity -- 20
) values (
  $1,
  $2::text::date,
  $3::text::mstar.statement_period,
  $4,
  case when $5::text = '_PO_' or $5::text = 'null' then null else $5::text::numeric * $21 end,
  case when $6::text = '_PO_' or $6::text = 'null' then null else $6::text::numeric * $21 end,
  case when $7::text = '_PO_' or $7::text = 'null' then null else $7::text::numeric * $21 end,
  case when $8::text = '_PO_' or $8::text = 'null' then null else $8::text::numeric * $21 end,
  case when $9::text = '_PO_' or $9::text = 'null' then null else $9::text::numeric * $21 end,
  case when $10::text = '_PO_' or $10::text = 'null' then null else $10::text::numeric * $21 end,
  case when $11::text = '_PO_' or $11::text = 'null' then null else $11::text::numeric * $21 end,
  case when $12::text = '_PO_' or $12::text = 'null' then null else $12::text::numeric * $21 end,
  case when $13::text = '_PO_' or $13::text = 'null' then null else $13::text::numeric * $21 end,
  case when $14::text = '_PO_' or $14::text = 'null' then null else $14::text::numeric * $21 end,
  case when $15::text = '_PO_' or $15::text = 'null' then null else $15::text::numeric * $21 end,
  case when $16::text = '_PO_' or $16::text = 'null' then null else $16::text::numeric * $21 end,
  case when $17::text = '_PO_' or $17::text = 'null' then null else $17::text::numeric * $21 end,
  case when $18::text = '_PO_' or $18::text = 'null' then null else $18::text::numeric * $21 end,
  case when $19::text = '_PO_' or $19::text = 'null' then null else $19::text::numeric * $21 end,
  case when $20::text = '_PO_' or $20::text = 'null' then null else $20::text::numeric * $21 end
) on conflict (act_symbol, date, period) do nothing;
"
                                  ticker-symbol
                                  (date->iso8601 date)
                                  (symbol->string period)
                                  currency
                                  (~a (list-ref-or-else preferred-stock i "null")) ; 5
                                  (~a (list-ref-or-else common-stock i "null"))
                                  (~a (list-ref-or-else additional-paid-in-capital i "null"))
                                  (~a (list-ref-or-else capital-stock i "null"))
                                  (~a (list-ref-or-else treasury-stock i "null"))
                                  (~a (list-ref-or-else paid-in-capital i "null")) ; 10
                                  (~a (list-ref-or-else stock-options-warrants-deferred-shares-convertible-debentures i "null"))
                                  (~a (list-ref-or-else retained-earnings i "null"))
                                  (~a (list-ref-or-else reserves i "null"))
                                  (~a (list-ref-or-else stock-subscription i "null"))
                                  (~a (list-ref-or-else perpetual-capital-securities-holder-equity i "null")) ; 15
                                  (~a (list-ref-or-else other-equity-interest i "null"))
                                  (~a (list-ref-or-else total-parent-stockholder-equity i "null"))
                                  (~a (list-ref-or-else minority-interest-in-equity i "null"))
                                  (~a (list-ref-or-else total-partnership-capital i "null"))
                                  (~a (list-ref-or-else total-equity i "null")) ; 20
                                  multiplier)]))
             dates (range (length dates)))
            
            #f
            ))))))

(set-subtract! current-assets-data-points known-current-assets-data-points)
(displayln "Missing the following Current Assets rows:")
(writeln (sort (set->list current-assets-data-points) string<?))

(set-subtract! non-current-assets-data-points known-non-current-assets-data-points)
(displayln "Missing the following Non-Current Assets rows:")
(writeln (sort (set->list non-current-assets-data-points) string<?))

(set-subtract! current-liabilities-data-points known-current-liabilities-data-points)
(displayln "Missing the following Current Liabilities rows:")
(writeln (sort (set->list current-liabilities-data-points) string<?))

(set-subtract! non-current-liabilities-data-points known-non-current-liabilities-data-points)
(displayln "Missing the following Non-Current Liabilities rows:")
(writeln (sort (set->list non-current-liabilities-data-points) string<?))

(set-subtract! equity-data-points known-equity-data-points)
(displayln "Missing the following Equity rows:")
(writeln (sort (set->list equity-data-points) string<?))

(set-subtract! parent-equity-data-points known-parent-equity-data-points)
(displayln "Missing the following Parent Equity rows:")
(writeln (sort (set->list parent-equity-data-points) string<?))
