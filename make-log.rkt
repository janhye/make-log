#lang racket

(require srfi/19)

(provide make-log)

(define (make-log #:log-dir [log-dir #f])
  (define (format-date date)
    (date->string date "~Y-~m-~d ~k:~M:~S"))
  
  (define my-logger (make-logger))
  
  (define logger-thread #f)
  
  (define (start-logger)
    (let ([receiver (make-log-receiver my-logger 'debug)]
          [date-str (format-date (current-date))])
      (set! logger-thread
            (thread
             (lambda ()
               (when log-dir
                 (when (not (directory-exists? log-dir))
                   (make-directory log-dir)))
               (call-with-output-file 
                   (build-path log-dir (string-append (substring date-str 0 10) ".log")) #:exists 'append
                 (lambda (out)
                   (let loop ()
                     (match (sync receiver)
                       [(vector level message value topic)
                        (when log-dir
                          (fprintf out "~a - ~a - ~a\n" level message value)
                          (flush-output out))
                        (printf "~a - ~a - ~a\n" level message value)
                        (flush-output)])
                     (loop)))))))))
  
  (define (restart-logger)
    (kill-thread logger-thread)
    (start-logger))
  
  (define (launch-log-daemon)
    (start-logger)
    (thread
     (lambda ()
       (let loop ()
         (sync
          (alarm-evt (+ (current-inexact-milliseconds) (* 1000 60 60))))
         (when (= 0 (date-hour (seconds->date (current-seconds))))
           (restart-logger))
         (loop)))))

  (launch-log-daemon)
  
  (lambda (msg #:level [level 'info] #:topic [topic 'log] #:data [data #f])
    (log-message my-logger level topic (string-append (format-date (current-date)) " " msg) data)))
