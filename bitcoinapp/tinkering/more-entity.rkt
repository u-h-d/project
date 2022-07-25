#lang racket

(define (mk-interfaces int)
      (for/list ((i int))
        (list i
              (open-output-string))))

    (define (mk-attr attr)
      (define attr-table
        (make-hash))
      (for ((a attr))
        (let ((key (first a))
              (val (second a)))
          (hash-set! attr-table key val)))
      attr-table)

(define (mk-entity name int attr)
  (let ((name name)
        (actions (list 'find-path 'mk-commit 'send-gossip 'handle-gossip 'update-graph))
        (interfaces (mk-interfaces int))
        (attributes (mk-attr attr)))

    (define (handle-attr-req cmd)
      (cond
        ((symbol=? 'name cmd) name)
        ((symbol=? 'interfaces cmd) interfaces)
        ((symbol=? 'attributes cmd) attributes)
        ((symbol=? 'actions cmd) actions)
        ((hash-ref attributes cmd) (hash-ref attributes cmd))
        ((assoc cmd interfaces) (second (assoc cmd interfaces)))
        (else
         (error "Invalid simple command"))))

    (define (handle-action-req cmd)
      (let ((tag (first cmd))
            (data (second cmd)))
        (cond
          ((symbol=? 'find-path tag)
           (find-path data))
          ((symbol=? 'mk-commit tag)
           (mk-commit data))
          ((symbol=? 'send-gossip tag)
           (send-gossip data))
          ((symbol=? 'handle-gossip tag)
           (handle-gossip data))
          ((symbol=? 'update-graph tag)
           (update-graph data))
          (else
           (error "Invalid action request")))))

    
    (define (find-path data)
      (format "this is a computed path to destination: ~a" data))
      
    (define (mk-commit data)
      (let ((source (first data))
            (dest (second data))
            (amt (third data)))
      (format "here is a commit for channel update where we send ~a to ~a from ~a" amt dest source)))
      
    (define (send-gossip data)
      (let ((out (second (assoc 'network interfaces)))
            (msg (format "here is a message: ~a" data)))
        (begin
          (printf "Sending a message -> ~a" msg)
          (fprintf out msg))))
      
    (define (handle-gossip data)
      (printf "we have received some gossip: ~a" data))
      
    (define (update-graph data)
      (printf "updating the network graph with the following: ~a" data))
                     
    
    ; controller
    (lambda (cmd)
      (cond
        ((pair? cmd) (handle-action-req cmd))
        ((symbol? cmd) (handle-attr-req cmd))
        (else
         (error "Invalid command"))))))

(define q
    (mk-entity "first" '(network user storage) '((graph empty) (partner_info empty) (channel_status empty))))