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
 #:program "racket cash-flow-statement-transform-load.rkt"
 #:once-each
 [("-b" "--base-folder") folder
                         "Morningstar cash flow statement base folder. Defaults to /var/local/mstar/financial-statements"
                         (base-folder folder)]
 [("-d" "--folder-date") date
                         "Morningstar cash flow statement folder date. Defaults to today"
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

(define operating-cash-flow-data-points (mutable-set))

(define known-operating-cash-flow-data-points
  (mutable-set "IFCF100463 Interest Received, CFO Indirect" ; currently skipped
               "IFCF100464 Interest Paid, CFO Indirect" ; currently skipped
               "IFCF200260 Dividend Paid, CFO Indirect" ; currently skipped
               "IFCF200270 Dividend Received, CFO Indirect" ; currently skipped
               "IFCF200280 Taxes Refund/Paid, Indirect" 
               "IFCF200290 Other Operating Cash Flow" 
               "IFCF200620 Cash Generated from Operating Activities"))

(define investing-cash-flow-data-points (mutable-set))

(define known-investing-cash-flow-data-points
  (mutable-set "IFCF001330 Purchase/Sale and Disposal of Property, Plant and Equipment, Net" 
               "IFCF001300 Purchase/Sale of Business, Net" 
               "IFCF001320 Purchase/Sale of Investments, Net" 
               "IFCF000061 Capital Expenditure, Reported" 
               "IFCF000910 Other Investing Cash Flow" 
               "IFCF001130 Change in Central Bank Funds Sold, Securities Purchased under REPOs and Securities Borrowed" 
               "IFCF001310 Purchase/Sale of Intangibles, Net" 
               "IFCF001578 Increase/Decrease in Restricted Cash and Cash Equivalents" 
               "IFCF001629 Payment for Loan Granted and Repayments Received, Net" 
               "IFCF001651 Purchase/Sale of Equity Investments" 
               "IFCF001652 Purchase/Sale of Investment Properties, Net" 
               "IFCF001655 Receipts and Payments for Loans, Net" 
               "IFCF001750 Change in Deposits, CFI" 
               "IFCF002051 Purchase/Sale of Other Non-Current Assets, Net" 
               "IFCF002103 Exploration and Mine Development Costs" 
               "IFCF200390 Dividends Received/Paid, CFI" 
               "IFCF200400 Interest Received/Paid, CFI"))

(define financing-cash-flow-data-points (mutable-set))

(define known-financing-cash-flow-data-points
  (mutable-set "IFCF000760 Issuance of/Payments for Common Stock, Net" 
               "IFCF000770 Issuance of/Repayments for Debt, Net" 
               "IFCF000790 Issuance of/Payments for Preferred Stock, Net" 
               "IFCF000900 Other Financing Cash Flow" 
               "IFCF001615 Net Movement in Non-Controlling/Minority Interest" 
               "IFCF001640 Change in Central Bank Funds Purchased, Securities Sold under REPOs and Securities Loaned" 
               "IFCF001643 Proceeds from Issuance/Exercising of Stock Options/Warrants" 
               "IFCF001794 Excess Tax Benefit from Share-Based Compensation, Financing Activities" 
               "IFCF001828 Change in Policyholder Funds and Contracts" 
               "IFCF001859 Issuance of/Repayments for Partners Equity, Net" 
               "IFCF002150 Cash Dividends and Interest Paid" 
               "IFCF100469 Issuance of/Repayments for Lease Financing" 
               "IFCF100477 Issue and Financing Costs" 
               "IFCF100478 Cash Dividends Paid to Non-Controlling/Minority Interests"))


(parameterize ([current-directory (string-append (base-folder) "/" (~t (folder-date) "yyyy-MM-dd") "/")])
  (for ([p (sequence-filter (λ (p) (string-contains? (path->string p) ".cash-flow-statement.")) (in-directory (current-directory)))])
    (let* ([file-name (path->string p)]
           [ticker-symbol (regexp-replace #rx".cash-flow-statement.[AQ].json" (string-replace file-name (path->string (current-directory)) "") "")])
      (call-with-input-file file-name
        (λ (in)
          (with-handlers ([exn:fail? (λ (e) (displayln (string-append "Failed to process the cash-flow statement for " ticker-symbol))
                                       (displayln e))])
            (define cash-flow-statement-json (~> (port->string in)
                                           (string->jsexpr _)))
            (define period (cond [(string-contains? file-name ".cash-flow-statement.A.json") 'Year]
                                 [(string-contains? file-name ".cash-flow-statement.Q.json") 'Quarter]))
            (define dates (cond [(equal? period 'Year)
                                 (map (λ (column) (iso8601->date (string-append column "-" (~> (hash-ref cash-flow-statement-json 'footer)
                                                                                               (hash-ref _ 'fiscalYearEndDate)))))
                                      (~> (hash-ref cash-flow-statement-json 'columnDefs)
                                          (filter (λ (def) (not (equal? "TTM" def))) _)))]
                                [(equal? period 'Quarter)
                                 (map (λ (column)
                                        (define quarter (first (string-split column " ")))
                                        (define year (string->number (last (string-split column " "))))
                                        (define end-month (~> (hash-ref cash-flow-statement-json '_meta)
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
                                      (~> (hash-ref cash-flow-statement-json 'columnDefs)
                                          (filter (λ (def) (not (equal? "TTM" def))) _)))]))
            (displayln ticker-symbol)

            (define currency (~> (hash-ref cash-flow-statement-json 'footer)
                                 (hash-ref _ 'currency)))

            (define order-of-magnitude (~> (hash-ref cash-flow-statement-json 'footer)
                                           (hash-ref _ 'orderOfMagnitude)))

            (define multiplier (cond [(equal? "Billion" order-of-magnitude) 1000000000]
                                     [(equal? "Million" order-of-magnitude) 1000000]
                                     [(equal? "Thousand" order-of-magnitude) 1000]))

            (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                (extract-from-data-point-id _ 'subLevel "IFCF000150") ; Cash Flow from Operating Activities, Indirect
                (extract-from-data-point-id _ 'subLevel "IFCF001591") ; Net Cash Flow from Continuing Operating Activities, Indirect
                (hash-ref _ 'subLevel (λ () (list)))
                (for-each (λ (ca) (set-add! operating-cash-flow-data-points
                                            (string-append (hash-ref ca 'dataPointId) " "
                                                           (hash-ref ca 'label)))) _))

            (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                (hash-ref _ 'subLevel (λ () (list)))
                (for-each (λ (ca) (set-add! investing-cash-flow-data-points
                                            (string-append (hash-ref ca 'dataPointId) " "
                                                           (hash-ref ca 'label)))) _))

            (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                (hash-ref _ 'subLevel (λ () (list)))
                (for-each (λ (ca) (set-add! financing-cash-flow-data-points
                                            (string-append (hash-ref ca 'dataPointId) " "
                                                           (hash-ref ca 'label)))) _))

            (define income-loss-before-adjustment
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000150") ; Cash Flow from Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF001591") ; Net Cash Flow from Continuing Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF200620") ; Cash Generated from Operating Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001613") ; Income/Loss before Non-Cash Adjustment
                  (hash-ref _ 'datum (λ () (list)))))

            (define depreciation-amortization-depletion
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000150") ; Cash Flow from Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF001591") ; Net Cash Flow from Continuing Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF200620") ; Cash Generated from Operating Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF200190") ; Total Adjustments for Non-Cash Items
                  (extract-from-data-point-id _ 'subLevel "IFCF001559") ; Depreciation, Amortization and Depletion, Non-Cash Adjustment
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-adjustments
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000150") ; Cash Flow from Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF001591") ; Net Cash Flow from Continuing Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF200620") ; Cash Generated from Operating Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF200190") ; Total Adjustments for Non-Cash Items
                  (hash-ref _ 'datum (λ () (list)))))

            (define changes-in-operating-capital
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000150") ; Cash Flow from Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF001591") ; Net Cash Flow from Continuing Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF200620") ; Cash Generated from Operating Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF000530") ; Changes in Operating Capital
                  (hash-ref _ 'datum (λ () (list)))))

            (define cash-generated-from-operations
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000150") ; Cash Flow from Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF001591") ; Net Cash Flow from Continuing Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF200620") ; Cash Generated from Operating Activities
                  (hash-ref _ 'datum (λ () (list)))))

            (define taxes-refund-paid
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000150") ; Cash Flow from Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF001591") ; Net Cash Flow from Continuing Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF200280") ; Taxes Refund/Paid, Indirect
                  (hash-ref _ 'datum (λ () (list)))))

            (define other-operating-cash-flow
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000150") ; Cash Flow from Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF001591") ; Net Cash Flow from Continuing Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF200290") ; Other Operating Cash Flow
                  (hash-ref _ 'datum (λ () (list)))))

            (define net-cash-flow-from-continuing-operations
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000150") ; Cash Flow from Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF001591") ; Net Cash Flow from Continuing Operating Activities, Indirect
                  (hash-ref _ 'datum (λ () (list)))))

            (define net-cash-flow-from-discontinuing-operations
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000150") ; Cash Flow from Operating Activities, Indirect
                  (extract-from-data-point-id _ 'subLevel "IFCF001595") ; Net Cash Flow from Discontinuing Operating Activities
                  (hash-ref _ 'datum (λ () (list)))))

            (define cash-flow-from-operations
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000150") ; Cash Flow from Operating Activities, Indirect
                  (hash-ref _ 'datum (λ () (list)))))

            (define purchase-sale-disposal-of-property-plant-equipment
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001330") ; Purchase/Sale and Disposal of Property, Plant and Equipment, Net
                  (hash-ref _ 'datum (λ () (list)))))

            (define purchase-sale-of-business
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001300") ; Purchase/Sale of Business, Net
                  (hash-ref _ 'datum (λ () (list)))))

            (define purchase-sale-of-investments
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001320") ; Purchase/Sale of Investments, Net
                  (hash-ref _ 'datum (λ () (list)))))

            (define capital-expenditure-reported
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF000061") ; Capital Expenditure, Reported
                  (hash-ref _ 'datum (λ () (list)))))

            (define other-investing-cash-flow
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF000910") ; Other Investing Cash Flow
                  (hash-ref _ 'datum (λ () (list)))))

            (define change-in-securities-borrowed
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001130") ; Change in Central Bank Funds Sold, Securities Purchased under REPOs and Securities Borrowed
                  (hash-ref _ 'datum (λ () (list)))))

            (define purchase-sale-of-intangibles
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001310") ; Purchase/Sale of Intangibles, Net
                  (hash-ref _ 'datum (λ () (list)))))

            (define increase-decrease-in-restricted-cash-and-equivalents
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001578") ; Increase/Decrease in Restricted Cash and Cash Equivalents
                  (hash-ref _ 'datum (λ () (list)))))

            (define payment-for-loan-granted-and-repayments-received
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001629") ; Payment for Loan Granted and Repayments Received, Net
                  (hash-ref _ 'datum (λ () (list)))))

            (define purchase-sale-of-equity-investments
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001651") ; Purchase/Sale of Equity Investments
                  (hash-ref _ 'datum (λ () (list)))))

            (define purchase-sale-of-investment-properties
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001652") ; Purchase/Sale of Investment Properties, Net
                  (hash-ref _ 'datum (λ () (list)))))

            (define receipts-and-payments-for-loans
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001655") ; Receipts and Payments for Loans, Net
                  (hash-ref _ 'datum (λ () (list)))))

            (define change-in-investing-deposits
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001750") ; Change in Deposits, CFI
                  (hash-ref _ 'datum (λ () (list)))))

            (define purchase-sale-of-other-non-current-assets
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF002051") ; Purchase/Sale of Other Non-Current Assets, Net
                  (hash-ref _ 'datum (λ () (list)))))

            (define exploration-and-mine-development-costs
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF002103") ; Exploration and Mine Development Costs
                  (hash-ref _ 'datum (λ () (list)))))

            (define investing-dividends-received-paid
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF200390") ; Dividends Received/Paid, CFI
                  (hash-ref _ 'datum (λ () (list)))))

            (define investing-interest-received-paid
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF200400") ; Interest Received/Paid, CFI
                  (hash-ref _ 'datum (λ () (list)))))

            (define cash-flow-from-continuing-investments
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001510") ; Cash Flow from Continuing Investing Activities
                  (hash-ref _ 'datum (λ () (list)))))

            (define net-cash-flow-from-discontinuing-investments
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001594") ; Net Cash Flow from Discontinuing Investing Activities
                  (hash-ref _ 'datum (λ () (list)))))

            (define cash-flow-from-investments
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000140") ; Cash Flow from Investing Activities
                  (hash-ref _ 'datum (λ () (list)))))

            (define issuance-of-payments-for-common-stock
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF000760") ; Issuance of/Payments for Common Stock, Net
                  (hash-ref _ 'datum (λ () (list)))))

            (define issuance-of-repayments-for-short-term-debt
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF000770") ; Issuance of/Repayments for Debt, Net
                  (extract-from-data-point-id _ 'subLevel "IFCF000800") ; Issuance of/Repayments for Short Term Debt, Net
                  (hash-ref _ 'datum (λ () (list)))))

            (define issuance-of-repayments-for-long-term-debt
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF000770") ; Issuance of/Repayments for Debt, Net
                  (extract-from-data-point-id _ 'subLevel "IFCF000780") ; Issuance of/Repayments for Long Term Debt, Net
                  (hash-ref _ 'datum (λ () (list)))))

            (define issuance-of-repayments-for-debt
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF000770") ; Issuance of/Repayments for Debt, Net
                  (hash-ref _ 'datum (λ () (list)))))

            (define issuance-of-payments-for-preferred-stock
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF000790") ; Issuance of/Payments for Preferred Stock, Net
                  (hash-ref _ 'datum (λ () (list)))))

            (define other-financing-cash-flow
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF000900") ; Other Financing Cash Flow
                  (hash-ref _ 'datum (λ () (list)))))

            (define net-movement-in-minority-interest
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001615") ; Net Movement in Non-Controlling/Minority Interest
                  (hash-ref _ 'datum (λ () (list)))))

            (define change-in-securities-loaned
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001640") ; Change in Central Bank Funds Purchased, Securities Sold under REPOs and Securities Loaned
                  (hash-ref _ 'datum (λ () (list)))))

            (define proceeds-from-issuance-exercising-of-stock-options-warrants
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001643") ; Proceeds from Issuance/Exercising of Stock Options/Warrants
                  (hash-ref _ 'datum (λ () (list)))))

            (define excess-tax-benefit-from-share-based-compensation
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001794") ; Excess Tax Benefit from Share-Based Compensation, Financing Activities
                  (hash-ref _ 'datum (λ () (list)))))

            (define change-in-policyholder-funds-and-contracts
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001828") ; Change in Policyholder Funds and Contracts
                  (hash-ref _ 'datum (λ () (list)))))

            (define issuance-of-repayments-for-partners-equity
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001859") ; Issuance of/Repayments for Partners Equity, Net
                  (hash-ref _ 'datum (λ () (list)))))

            (define cash-dividends-and-interest-paid
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF002150") ; Cash Dividends and Interest Paid
                  (hash-ref _ 'datum (λ () (list)))))

            (define issuance-of-repayments-for-lease-financing
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF100469") ; Issuance of/Repayments for Lease Financing
                  (hash-ref _ 'datum (λ () (list)))))

            (define issue-and-financing-costs
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF100477") ; Issue and Financing Costs
                  (hash-ref _ 'datum (λ () (list)))))

            (define cash-dividends-paid-to-minority-interests
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF100478") ; Cash Dividends Paid to Non-Controlling/Minority Interests
                  (hash-ref _ 'datum (λ () (list)))))

            (define cash-flow-from-continuing-financing
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001509") ; Cash Flow from Continuing Financing Activities
                  (hash-ref _ 'datum (λ () (list)))))

            (define net-cash-flow-from-discontinuing-financing
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (extract-from-data-point-id _ 'subLevel "IFCF001593") ; Cash Flow from Discontinuing Financing Activities
                  (hash-ref _ 'datum (λ () (list)))))

            (define cash-flow-from-financing
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000130") ; Cash Flow from Financing Activities
                  (hash-ref _ 'datum (λ () (list)))))

            (define change-in-cash
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000170") ; Cash and Cash Equivalents, End of Period
                  (extract-from-data-point-id _ 'subLevel "IFCF000280") ; Change in Cash
                  (hash-ref _ 'datum (λ () (list)))))

            (define effect-of-exchange-rates
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000170") ; Cash and Cash Equivalents, End of Period
                  (extract-from-data-point-id _ 'subLevel "IFCF000650") ; Effect of Exchange Rate Changes
                  (hash-ref _ 'datum (λ () (list)))))

            (define cash-and-equivalents-beginning-of-period
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000170") ; Cash and Cash Equivalents, End of Period
                  (extract-from-data-point-id _ 'subLevel "IFCF000160") ; Cash and Cash Equivalents, Beginning of Period
                  (hash-ref _ 'datum (λ () (list)))))

            (define cash-and-equivalents-end-of-period
              (~> (extract-from-data-point-id cash-flow-statement-json 'rows "IFCF000000") ; CashFlow
                  (extract-from-data-point-id _ 'subLevel "IFCF000170") ; Cash and Cash Equivalents, End of Period
                  (hash-ref _ 'datum (λ () (list)))))

            (for-each
             (λ (date i)
               (cond [(not (equal? "_PO_" (~a (list-ref-or-else cash-and-equivalents-end-of-period i "null"))))
                      (query-exec dbc "
insert into mstar.cash_flow_statement (
  act_symbol,
  date,
  period,
  currency,
  income_loss_before_adjustment, -- 5
  depreciation_amortization_depletion,
  total_adjustments,
  changes_in_operating_capital,
  cash_generated_from_operations,
  taxes_refund_paid, -- 10
  other_operating_cash_flow,
  net_cash_flow_from_continuing_operations,
  net_cash_flow_from_discontinuing_operations,
  cash_flow_from_operations,
  purchase_sale_disposal_of_property_plant_equipment, -- 15
  purchase_sale_of_business,
  purchase_sale_of_investments,
  capital_expenditure_reported,
  other_investing_cash_flow,
  change_in_securities_borrowed, -- 20
  purchase_sale_of_intangibles,
  increase_decrease_in_restricted_cash_and_equivalents,
  payment_for_loan_granted_and_repayments_received,
  purchase_sale_of_equity_investments,
  purchase_sale_of_investment_properties, -- 25
  receipts_and_payments_for_loans,
  change_in_investing_deposits,
  purchase_sale_of_other_non_current_assets,
  exploration_and_mine_development_costs,
  investing_dividends_received_paid, -- 30
  investing_interest_received_paid,
  cash_flow_from_continuing_investments,
  net_cash_flow_from_discontinuing_investments,
  cash_flow_from_investments,
  issuance_of_payments_for_common_stock, -- 35
  issuance_of_repayments_for_short_term_debt,
  issuance_of_repayments_for_long_term_debt,
  issuance_of_repayments_for_debt,
  issuance_of_payments_for_preferred_stock,
  other_financing_cash_flow, -- 40
  net_movement_in_minority_interest,
  change_in_securities_loaned,
  proceeds_from_issuance_exercising_of_stock_options_warrants,
  excess_tax_benefit_from_share_based_compensation,
  change_in_policyholder_funds_and_contracts, -- 45
  issuance_of_repayments_for_partners_equity,
  cash_dividends_and_interest_paid,
  issuance_of_repayments_for_lease_financing,
  issue_and_financing_costs,
  cash_dividends_paid_to_minority_interests, -- 50
  cash_flow_from_continuing_financing,
  net_cash_flow_from_discontinuing_financing,
  cash_flow_from_financing,
  change_in_cash,
  effect_of_exchange_rates, -- 55
  cash_and_equivalents_beginning_of_period,
  cash_and_equivalents_end_of_period
) values (
  $1,
  $2::text::date,
  $3::text::mstar.statement_period,
  $4,
  case when $5::text = '_PO_' or $5::text = 'null' then null else $5::text::numeric * $58 end,
  case when $6::text = '_PO_' or $6::text = 'null' then null else $6::text::numeric * $58 end,
  case when $7::text = '_PO_' or $7::text = 'null' then null else $7::text::numeric * $58 end,
  case when $8::text = '_PO_' or $8::text = 'null' then null else $8::text::numeric * $58 end,
  case when $9::text = '_PO_' or $9::text = 'null' then null else $9::text::numeric * $58 end,
  case when $10::text = '_PO_' or $10::text = 'null' then null else $10::text::numeric * $58 end,
  case when $11::text = '_PO_' or $11::text = 'null' then null else $11::text::numeric * $58 end,
  case when $12::text = '_PO_' or $12::text = 'null' then null else $12::text::numeric * $58 end,
  case when $13::text = '_PO_' or $13::text = 'null' then null else $13::text::numeric * $58 end,
  case when $14::text = '_PO_' or $14::text = 'null' then null else $14::text::numeric * $58 end,
  case when $15::text = '_PO_' or $15::text = 'null' then null else $15::text::numeric * $58 end,
  case when $16::text = '_PO_' or $16::text = 'null' then null else $16::text::numeric * $58 end,
  case when $17::text = '_PO_' or $17::text = 'null' then null else $17::text::numeric * $58 end,
  case when $18::text = '_PO_' or $18::text = 'null' then null else $18::text::numeric * $58 end,
  case when $19::text = '_PO_' or $19::text = 'null' then null else $19::text::numeric * $58 end,
  case when $20::text = '_PO_' or $20::text = 'null' then null else $20::text::numeric * $58 end,
  case when $21::text = '_PO_' or $21::text = 'null' then null else $21::text::numeric * $58 end,
  case when $22::text = '_PO_' or $22::text = 'null' then null else $22::text::numeric * $58 end,
  case when $23::text = '_PO_' or $23::text = 'null' then null else $23::text::numeric * $58 end,
  case when $24::text = '_PO_' or $24::text = 'null' then null else $24::text::numeric * $58 end,
  case when $25::text = '_PO_' or $25::text = 'null' then null else $25::text::numeric * $58 end,
  case when $26::text = '_PO_' or $26::text = 'null' then null else $26::text::numeric * $58 end,
  case when $27::text = '_PO_' or $27::text = 'null' then null else $27::text::numeric * $58 end,
  case when $28::text = '_PO_' or $28::text = 'null' then null else $28::text::numeric * $58 end,
  case when $29::text = '_PO_' or $29::text = 'null' then null else $29::text::numeric * $58 end,
  case when $30::text = '_PO_' or $30::text = 'null' then null else $30::text::numeric * $58 end,
  case when $31::text = '_PO_' or $31::text = 'null' then null else $31::text::numeric * $58 end,
  case when $32::text = '_PO_' or $32::text = 'null' then null else $32::text::numeric * $58 end,
  case when $33::text = '_PO_' or $33::text = 'null' then null else $33::text::numeric * $58 end,
  case when $34::text = '_PO_' or $34::text = 'null' then null else $34::text::numeric * $58 end,
  case when $35::text = '_PO_' or $35::text = 'null' then null else $35::text::numeric * $58 end,
  case when $36::text = '_PO_' or $36::text = 'null' then null else $36::text::numeric * $58 end,
  case when $37::text = '_PO_' or $37::text = 'null' then null else $37::text::numeric * $58 end,
  case when $38::text = '_PO_' or $38::text = 'null' then null else $38::text::numeric * $58 end,
  case when $39::text = '_PO_' or $39::text = 'null' then null else $39::text::numeric * $58 end,
  case when $40::text = '_PO_' or $40::text = 'null' then null else $40::text::numeric * $58 end,
  case when $41::text = '_PO_' or $41::text = 'null' then null else $41::text::numeric * $58 end,
  case when $42::text = '_PO_' or $42::text = 'null' then null else $42::text::numeric * $58 end,
  case when $43::text = '_PO_' or $43::text = 'null' then null else $43::text::numeric * $58 end,
  case when $44::text = '_PO_' or $44::text = 'null' then null else $44::text::numeric * $58 end,
  case when $45::text = '_PO_' or $45::text = 'null' then null else $45::text::numeric * $58 end,
  case when $46::text = '_PO_' or $46::text = 'null' then null else $46::text::numeric * $58 end,
  case when $47::text = '_PO_' or $47::text = 'null' then null else $47::text::numeric * $58 end,
  case when $48::text = '_PO_' or $48::text = 'null' then null else $48::text::numeric * $58 end,
  case when $49::text = '_PO_' or $49::text = 'null' then null else $49::text::numeric * $58 end,
  case when $50::text = '_PO_' or $50::text = 'null' then null else $50::text::numeric * $58 end,
  case when $51::text = '_PO_' or $51::text = 'null' then null else $51::text::numeric * $58 end,
  case when $52::text = '_PO_' or $52::text = 'null' then null else $52::text::numeric * $58 end,
  case when $53::text = '_PO_' or $53::text = 'null' then null else $53::text::numeric * $58 end,
  case when $54::text = '_PO_' or $54::text = 'null' then null else $54::text::numeric * $58 end,
  case when $55::text = '_PO_' or $55::text = 'null' then null else $55::text::numeric * $58 end,
  case when $56::text = '_PO_' or $56::text = 'null' then null else $56::text::numeric * $58 end,
  case when $57::text = '_PO_' or $57::text = 'null' then null else $57::text::numeric * $58 end
) on conflict (act_symbol, date, period) do nothing;
"
                           ticker-symbol
                           (date->iso8601 date)
                           (symbol->string period)
                           currency
                           (~a (list-ref-or-else income-loss-before-adjustment i "null")) ; 5
                           (~a (list-ref-or-else depreciation-amortization-depletion i "null"))
                           (~a (list-ref-or-else total-adjustments i "null"))
                           (~a (list-ref-or-else changes-in-operating-capital i "null"))
                           (~a (list-ref-or-else cash-generated-from-operations i "null"))
                           (~a (list-ref-or-else taxes-refund-paid i "null")) ; 10
                           (~a (list-ref-or-else other-operating-cash-flow i "null"))
                           (~a (list-ref-or-else net-cash-flow-from-continuing-operations i "null"))
                           (~a (list-ref-or-else net-cash-flow-from-discontinuing-operations i "null"))
                           (~a (list-ref-or-else cash-flow-from-operations i "null"))
                           (~a (list-ref-or-else purchase-sale-disposal-of-property-plant-equipment i "null")) ; 15
                           (~a (list-ref-or-else purchase-sale-of-business i "null"))
                           (~a (list-ref-or-else purchase-sale-of-investments i "null"))
                           (~a (list-ref-or-else capital-expenditure-reported i "null"))
                           (~a (list-ref-or-else other-investing-cash-flow i "null"))
                           (~a (list-ref-or-else change-in-securities-borrowed i "null")) ; 20
                           (~a (list-ref-or-else purchase-sale-of-intangibles i "null"))
                           (~a (list-ref-or-else increase-decrease-in-restricted-cash-and-equivalents i "null"))
                           (~a (list-ref-or-else payment-for-loan-granted-and-repayments-received i "null"))
                           (~a (list-ref-or-else purchase-sale-of-equity-investments i "null"))
                           (~a (list-ref-or-else purchase-sale-of-investment-properties i "null")) ; 25
                           (~a (list-ref-or-else receipts-and-payments-for-loans i "null"))
                           (~a (list-ref-or-else change-in-investing-deposits i "null"))
                           (~a (list-ref-or-else purchase-sale-of-other-non-current-assets i "null"))
                           (~a (list-ref-or-else exploration-and-mine-development-costs i "null"))
                           (~a (list-ref-or-else investing-dividends-received-paid i "null")) ; 30
                           (~a (list-ref-or-else investing-interest-received-paid i "null"))
                           (~a (list-ref-or-else cash-flow-from-continuing-investments i "null"))
                           (~a (list-ref-or-else net-cash-flow-from-discontinuing-investments i "null"))
                           (~a (list-ref-or-else cash-flow-from-investments i "null"))
                           (~a (list-ref-or-else issuance-of-payments-for-common-stock i "null")) ; 35
                           (~a (list-ref-or-else issuance-of-repayments-for-short-term-debt i "null"))
                           (~a (list-ref-or-else issuance-of-repayments-for-long-term-debt i "null"))
                           (~a (list-ref-or-else issuance-of-repayments-for-debt i "null"))
                           (~a (list-ref-or-else issuance-of-payments-for-preferred-stock i "null"))
                           (~a (list-ref-or-else other-financing-cash-flow i "null")) ; 40
                           (~a (list-ref-or-else net-movement-in-minority-interest i "null"))
                           (~a (list-ref-or-else change-in-securities-loaned i "null"))
                           (~a (list-ref-or-else proceeds-from-issuance-exercising-of-stock-options-warrants i "null"))
                           (~a (list-ref-or-else excess-tax-benefit-from-share-based-compensation i "null"))
                           (~a (list-ref-or-else change-in-policyholder-funds-and-contracts i "null")) ; 45
                           (~a (list-ref-or-else issuance-of-repayments-for-partners-equity i "null"))
                           (~a (list-ref-or-else cash-dividends-and-interest-paid i "null"))
                           (~a (list-ref-or-else issuance-of-repayments-for-lease-financing i "null"))
                           (~a (list-ref-or-else issue-and-financing-costs i "null"))
                           (~a (list-ref-or-else cash-dividends-paid-to-minority-interests i "null")) ; 50
                           (~a (list-ref-or-else cash-flow-from-continuing-financing i "null"))
                           (~a (list-ref-or-else net-cash-flow-from-discontinuing-financing i "null"))
                           (~a (list-ref-or-else cash-flow-from-financing i "null"))
                           (~a (list-ref-or-else change-in-cash i "null"))
                           (~a (list-ref-or-else effect-of-exchange-rates i "null")) ; 55
                           (~a (list-ref-or-else cash-and-equivalents-beginning-of-period i "null"))
                           (~a (list-ref-or-else cash-and-equivalents-end-of-period i "null"))
                           multiplier)]))
             dates (range (length dates)))
            
            #f
            ))))))

(set-subtract! operating-cash-flow-data-points known-operating-cash-flow-data-points)
(displayln "Missing the following Operating Cash Flow rows:")
(writeln (sort (set->list operating-cash-flow-data-points) string<?))

(set-subtract! investing-cash-flow-data-points known-investing-cash-flow-data-points)
(displayln "Missing the following Investing Cash Flow rows:")
(writeln (sort (set->list investing-cash-flow-data-points) string<?))

(set-subtract! financing-cash-flow-data-points known-financing-cash-flow-data-points)
(displayln "Missing the following Financing Cash Flow rows:")
(writeln (sort (set->list financing-cash-flow-data-points) string<?))
