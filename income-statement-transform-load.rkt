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
 #:program "racket income-statement-transform-load.rkt"
 #:once-each
 [("-b" "--base-folder") folder
                         "Morningstar income statement base folder. Defaults to /var/local/mstar/financial-statements"
                         (base-folder folder)]
 [("-d" "--folder-date") date
                         "Morningstar income statement folder date. Defaults to today"
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

(define income-statement-data-points (mutable-set))

(define known-income-statement-data-points
  (mutable-set "IFIS000340 Discontinued Operations"
               "IFIS000430 Extraordinary Items"
               "IFIS000590 Gross Profit"
               "IFIS001040 Non-Controlling/Minority Interests"
               "IFIS001090 Net Income after Non-Controlling/Minority Interests"
               "IFIS001100 Net Income Available to Common Stockholders"
               "IFIS001110 Net Income before Extraordinary Items and Discontinued Operations"
               "IFIS001170 Total Revenue"
               "IFIS001220 Operating Income/Expenses"
               "IFIS001230 Total Operating Profit/Loss"
               "IFIS001470 Preferred/Other Stock Distribution"
               "IFIS001490 Pretax Income"
               "IFIS001560 Provision for Income Tax"
               "IFIS001842 Comprehensive Income"
               "IFIS001917 Non-Operating Income/Expense, Total"
               "IFIS001936 Other Net of Taxes Adjustments"
               "IFIS001977 Net Income after Extraordinary Items and Discontinued Operations"
               "IFIS002044 Earnings from Equity Interest"
               "IFIS002156 Income Statement Supplemental Section"
               "IFIS002230 Diluted Net Income Available to Common Stockholders"
               "IFIS003108 Other Adjustments to Net Income Available to Common Stockholders"
               "IFIS003149 Total Expenses"
               "IFIS101195 Total Gross Dividends"
               "IFIS101211 Dilution to Earnings"
               "IFIS200300 Net Income Allocated to Common Unitholders"
               "OMIS000001 Income Statement Operating Metrics"))

(define eps-and-average-shares-data-points (mutable-set))

(define known-eps-and-average-shares-data-points
  (mutable-set "IFIS000100 Basic EPS" 
               "IFIS000150 Basic Weighted Average Shares Outstanding" 
               "IFIS000280 Diluted EPS" 
               "IFIS000330 Diluted Weighted Average Shares Outstanding" 
               "IFIS002119 Preferred Dividend Per Share" 
               "IFIS101198 Reported Normalized Basic EPS" 
               "IFIS101199 Reported Normalized Diluted EPS" 
               "ORISZ00026 Total Dividend Per Share"))

(parameterize ([current-directory (string-append (base-folder) "/" (~t (folder-date) "yyyy-MM-dd") "/")])
  (for ([p (sequence-filter (λ (p) (string-contains? (path->string p) ".income-statement.")) (in-directory (current-directory)))])
    (let* ([file-name (path->string p)]
           [ticker-symbol (regexp-replace #rx".income-statement.[AQ].json" (string-replace file-name (path->string (current-directory)) "") "")])
      (call-with-input-file file-name
        (λ (in)
          (with-handlers ([exn:fail? (λ (e) (displayln (string-append "Failed to process the income statement for " ticker-symbol))
                                       (displayln e))])
            (define income-statement-json (~> (port->string in)
                                           (string->jsexpr _)))
            (define period (cond [(string-contains? file-name ".income-statement.A.json") 'Year]
                                 [(string-contains? file-name ".income-statement.Q.json") 'Quarter]))
            (define dates (cond [(equal? period 'Year)
                                 (map (λ (column) (iso8601->date (string-append column "-" (~> (hash-ref income-statement-json 'footer)
                                                                                               (hash-ref _ 'fiscalYearEndDate)))))
                                      (~> (hash-ref income-statement-json 'columnDefs)
                                          (filter (λ (def) (not (equal? "TTM" def))) _)))]
                                [(equal? period 'Quarter)
                                 (map (λ (column)
                                        (define quarter (first (string-split column " ")))
                                        (define year (string->number (last (string-split column " "))))
                                        (define end-month (~> (hash-ref income-statement-json '_meta)
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
                                      (~> (hash-ref income-statement-json 'columnDefs)
                                          (filter (λ (def) (not (equal? "TTM" def))) _)))]))

            (define currency (~> (hash-ref income-statement-json 'footer)
                                 (hash-ref _ 'currency)))

            (define order-of-magnitude (~> (hash-ref income-statement-json 'footer)
                                           (hash-ref _ 'orderOfMagnitude)))

            (define multiplier (cond [(equal? "Billion" order-of-magnitude) 1000000000]
                                     [(equal? "Million" order-of-magnitude) 1000000]
                                     [(equal? "Thousand" order-of-magnitude) 1000]))

            (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                (hash-ref _ 'subLevel (λ () (list)))
                (for-each (λ (ca) (set-add! income-statement-data-points
                                            (string-append (hash-ref ca 'dataPointId) " "
                                                           (hash-ref ca 'label)))) _))

            (~> (extract-from-data-point-id income-statement-json 'rows "IFES000000") ; EpsAndWaso
                (hash-ref _ 'subLevel (λ () (list)))
                (for-each (λ (ca) (set-add! eps-and-average-shares-data-points
                                            (string-append (hash-ref ca 'dataPointId) " "
                                                           (hash-ref ca 'label)))) _))

            (define total-revenue-sup
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS002220") ; Total Revenue
                  (hash-ref _ 'datum (λ () (list)))))
            (define total-revenue-sub
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS000590") ; Gross Profit
                  (extract-from-data-point-id _ 'subLevel "IFIS001170") ; Total Revenue
                  (hash-ref _ 'datum (λ () (list)))))

            (define cost-of-revenue
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS000590") ; Gross Profit
                  (extract-from-data-point-id _ 'subLevel "IFIS000200") ; Cost of Revenue
                  (hash-ref _ 'datum (λ () (list)))))

            (define gross-profit
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS000590") ; Gross Profit
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-expenses
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS003149") ; Total Expenses
                  (hash-ref _ 'datum (λ () (list)))))

            (define selling-general-administrative-expenses
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001220") ; Operating Income/Expenses
                  (extract-from-data-point-id _ 'subLevel "IFIS001700") ; Selling, General and Administrative Expenses
                  (hash-ref _ 'datum (λ () (list)))))

            (define operating-income-expenses
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001220") ; Operating Income/Expenses
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-operating-profit-loss
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001230") ; Total Operating Profit/Loss
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-net-finance-income-expenses
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001917") ; Non-Operating Income/Expense, Total
                  (extract-from-data-point-id _ 'subLevel "IFIS000930") ; Total Net Finance Income/Expense
                  (hash-ref _ 'datum (λ () (list)))))

            (define irregular-income-expenses
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001917") ; Non-Operating Income/Expense, Total
                  (extract-from-data-point-id _ 'subLevel "IFIS101189") ; Irregular Income/Expense
                  (hash-ref _ 'datum (λ () (list)))))

            (define other-non-operating-income-expenses
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001917") ; Non-Operating Income/Expense, Total
                  (extract-from-data-point-id _ 'subLevel "IFIS200292") ; Other Income/Expense, Non-Operating
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-non-operating-income-expenses
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001917") ; Non-Operating Income/Expense, Total
                  (hash-ref _ 'datum (λ () (list)))))

            (define pretax-income
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001490") ; Pretax Income
                  (hash-ref _ 'datum (λ () (list)))))

            (define provision-for-income-tax
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001560") ; Provision for Income Tax
                  (hash-ref _ 'datum (λ () (list)))))

            (define earnings-from-equity-interest
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS002044") ; Earnings from Equity Interest
                  (hash-ref _ 'datum (λ () (list)))))

            (define net-income-before-extraordinary-items-and-discontinued-operations
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001110") ; Net Income before Extraordinary Items and Discontinued Operations
                  (hash-ref _ 'datum (λ () (list)))))

            (define discontinued-operations
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS000340") ; Discontinued Operations
                  (hash-ref _ 'datum (λ () (list)))))

            (define extraordinary-items
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS000430") ; Extraordinary Items
                  (hash-ref _ 'datum (λ () (list)))))

            (define other-net-of-taxes-adjustments
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001936") ; Other Net of Taxes Adjustments
                  (hash-ref _ 'datum (λ () (list)))))

            (define net-income-after-extraordinary-items-and-discontinued-operations
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001977") ; Net Income after Extraordinary Items and Discontinued Operations
                  (hash-ref _ 'datum (λ () (list)))))

            (define minority-interests
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001040") ; Non-Controlling/Minority Interests
                  (hash-ref _ 'datum (λ () (list)))))

            (define net-income-after-minority-interests
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001090") ; Net Income after Non-Controlling/Minority Interests
                  (hash-ref _ 'datum (λ () (list)))))

            (define preferred-other-stock-distribution
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001470") ; Preferred/Other Stock Distribution
                  (hash-ref _ 'datum (λ () (list)))))

            (define other-adjustments-to-net-income
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS003108") ; Other Adjustments to Net Income Available to Common Stockholders
                  (hash-ref _ 'datum (λ () (list)))))

            (define net-income
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001100") ; Net Income Available to Common Stockholders
                  (hash-ref _ 'datum (λ () (list)))))

            (define dilution-to-earnings
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS101211") ; Dilution to Earnings
                  (hash-ref _ 'datum (λ () (list)))))

            (define diluted-net-income
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS002230") ; Diluted Net Income Available to Common Stockholders
                  (hash-ref _ 'datum (λ () (list)))))

            (define net-income-allocated-to-common-unitholders
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS200300") ; Net Income Allocated to Common Unitholders
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-gross-dividends
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS101195") ; Total Gross Dividends
                  (hash-ref _ 'datum (λ () (list)))))

            (define comprehensive-income
              (~> (extract-from-data-point-id income-statement-json 'rows "IFIS000000") ; IncomeStatement
                  (extract-from-data-point-id _ 'subLevel "IFIS001842") ; Comprehensive Income
                  (hash-ref _ 'datum (λ () (list)))))

            (define basic-eps
              (~> (extract-from-data-point-id income-statement-json 'rows "IFES000000") ; EpsAndWaso
                  (extract-from-data-point-id _ 'subLevel "IFIS000100") ; Basic EPS
                  (hash-ref _ 'datum (λ () (list)))))

            (define basic-average-shares
              (~> (extract-from-data-point-id income-statement-json 'rows "IFES000000") ; EpsAndWaso
                  (extract-from-data-point-id _ 'subLevel "IFIS000150") ; Basic Weighted Average Shares Outstanding
                  (hash-ref _ 'datum (λ () (list)))))

            (define diluted-eps
              (~> (extract-from-data-point-id income-statement-json 'rows "IFES000000") ; EpsAndWaso
                  (extract-from-data-point-id _ 'subLevel "IFIS000280") ; Diluted EPS
                  (hash-ref _ 'datum (λ () (list)))))

            (define diluted-average-shares
              (~> (extract-from-data-point-id income-statement-json 'rows "IFES000000") ; EpsAndWaso
                  (extract-from-data-point-id _ 'subLevel "IFIS000330") ; Diluted Weighted Average Shares Outstanding
                  (hash-ref _ 'datum (λ () (list)))))

            (define preferred-dividend-per-share
              (~> (extract-from-data-point-id income-statement-json 'rows "IFES000000") ; EpsAndWaso
                  (extract-from-data-point-id _ 'subLevel "IFIS002119") ; Preferred Dividend Per Share
                  (hash-ref _ 'datum (λ () (list)))))

            (define total-dividend-per-share
              (~> (extract-from-data-point-id income-statement-json 'rows "IFES000000") ; EpsAndWaso
                  (extract-from-data-point-id _ 'subLevel "ORISZ00026") ; Total Dividend Per Share
                  (hash-ref _ 'datum (λ () (list)))))

            (for-each
             (λ (date i)
               (cond [(not (equal? "_PO_" (~a (list-ref-or-else net-income i "null"))))
                      (query-exec dbc "
insert into mstar.income_statement (
  act_symbol,
  date,
  period,
  currency,
  total_revenue, -- 5
  cost_of_revenue,
  gross_profit,
  total_expenses,
  selling_general_administrative_expenses,
  operating_income_expenses, -- 10
  total_operating_profit_loss,
  total_net_finance_income_expenses,
  irregular_income_expenses,
  other_non_operating_income_expenses,
  total_non_operating_income_expenses, -- 15
  pretax_income,
  provision_for_income_tax,
  earnings_from_equity_interest,
  net_income_before_extraordinary_items_and_discontinued_operations,
  discontinued_operations, -- 20
  extraordinary_items,
  other_net_of_taxes_adjustments,
  net_income_after_extraordinary_items_and_discontinued_operations,
  minority_interests,
  net_income_after_minority_interests, -- 25
  preferred_other_stock_distribution,
  other_adjustments_to_net_income,
  net_income,
  dilution_to_earnings,
  diluted_net_income, -- 30
  net_income_allocated_to_common_unitholders,
  total_gross_dividends,
  comprehensive_income,
  basic_eps,
  basic_average_shares, -- 35
  diluted_eps,
  diluted_average_shares,
  preferred_dividend_per_share,
  total_dividend_per_share
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
  case when $34::text = '_PO_' or $34::text = 'null' then null else $34::text::numeric end,
  case when $35::text = '_PO_' or $35::text = 'null' then null else $35::text::numeric * $40 end,
  case when $36::text = '_PO_' or $36::text = 'null' then null else $36::text::numeric end,
  case when $37::text = '_PO_' or $37::text = 'null' then null else $37::text::numeric * $40 end,
  case when $38::text = '_PO_' or $38::text = 'null' then null else $38::text::numeric end,
  case when $39::text = '_PO_' or $39::text = 'null' then null else $39::text::numeric end
) on conflict (act_symbol, date, period) do nothing;
"
                           ticker-symbol
                           (date->iso8601 date)
                           (symbol->string period)
                           currency
                           (~a (list-ref-or-else total-revenue-sub i
                                                 (list-ref-or-else total-revenue-sup i "null"))) ; 5
                           (~a (list-ref-or-else cost-of-revenue i "null"))
                           (~a (list-ref-or-else gross-profit i "null"))
                           (~a (list-ref-or-else total-expenses i "null"))
                           (~a (list-ref-or-else selling-general-administrative-expenses i "null"))
                           (~a (list-ref-or-else operating-income-expenses i "null")) ; 10
                           (~a (list-ref-or-else total-operating-profit-loss i "null"))
                           (~a (list-ref-or-else total-net-finance-income-expenses i "null"))
                           (~a (list-ref-or-else irregular-income-expenses i "null"))
                           (~a (list-ref-or-else other-non-operating-income-expenses i "null"))
                           (~a (list-ref-or-else total-non-operating-income-expenses i "null")) ; 15
                           (~a (list-ref-or-else pretax-income i "null"))
                           (~a (list-ref-or-else provision-for-income-tax i "null"))
                           (~a (list-ref-or-else earnings-from-equity-interest i "null"))
                           (~a (list-ref-or-else net-income-before-extraordinary-items-and-discontinued-operations i "null"))
                           (~a (list-ref-or-else discontinued-operations i "null")) ; 20
                           (~a (list-ref-or-else extraordinary-items i "null"))
                           (~a (list-ref-or-else other-net-of-taxes-adjustments i "null"))
                           (~a (list-ref-or-else net-income-after-extraordinary-items-and-discontinued-operations i "null"))
                           (~a (list-ref-or-else minority-interests i "null"))
                           (~a (list-ref-or-else net-income-after-minority-interests i "null")) ; 25
                           (~a (list-ref-or-else preferred-other-stock-distribution i "null"))
                           (~a (list-ref-or-else other-adjustments-to-net-income i "null"))
                           (~a (list-ref-or-else net-income i "null"))
                           (~a (list-ref-or-else dilution-to-earnings i "null"))
                           (~a (list-ref-or-else diluted-net-income i "null")) ; 30
                           (~a (list-ref-or-else net-income-allocated-to-common-unitholders i "null"))
                           (~a (list-ref-or-else total-gross-dividends i "null"))
                           (~a (list-ref-or-else comprehensive-income i "null"))
                           (~a (list-ref-or-else basic-eps i "null"))
                           (~a (list-ref-or-else basic-average-shares i "null")) ; 35
                           (~a (list-ref-or-else diluted-eps i "null"))
                           (~a (list-ref-or-else diluted-average-shares i "null"))
                           (~a (list-ref-or-else preferred-dividend-per-share i "null"))
                           (~a (list-ref-or-else total-dividend-per-share i "null"))
                           multiplier)])) ; 40
             dates (range (length dates)))

            #f
            ))))))

(set-subtract! income-statement-data-points known-income-statement-data-points)
(displayln "Missing the following Income Statement rows:")
(writeln (sort (set->list income-statement-data-points) string<?))

(set-subtract! eps-and-average-shares-data-points known-eps-and-average-shares-data-points)
(displayln "Missing the following EPS and Average Shares rows:")
(writeln (sort (set->list eps-and-average-shares-data-points) string<?))
