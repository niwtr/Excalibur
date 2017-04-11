(defun arg-walker (body &optional arg-binding context)
    "walk the code,
   replace arguments of different lexical env with different gensyms."
    (cond ((null body) nil)
          ((consp (first body))
           (cons (arg-walker (car body) arg-binding context)
                 (arg-walker (cdr body) arg-binding context)))
          (t
           (let ((symb (first body)))
             (cond ((eq symb 'quote) body)
                   ((eq symb 'defun)
                    (let* ((arg-binding)
                           (arglist (flatten (third body)))
                           (ncontext (union context arglist))
                           (same-argl (intersection context arglist)))
                      (append (list 'defun (second body)
                                    (rmapcar/dot (lambda (ag)
                                                   (if (find ag same-argl)
                                                       (lett (g (gensym))
                                                         (push (cons ag g) arg-binding)
                                                         g) ag))
                                                 (third body))) ;;arg list
                              (arg-walker (cdddr body) arg-binding ncontext))));;defun body
                   ((or (eq symb 'labels) (eq symb 'flet))
                    (append (list 'labels
                                  (mapcar
                                   (lambda (def)
                                     (let* ((arg-binding)
                                            (arglist (flatten (second def)))
                                            (ncontext (union context arglist))
                                            (same-argl (intersection context arglist)))
                                       (append (list (first def) ;;name
                                                     (rmapcar/dot (lambda (ag)
                                                                    (if (find ag same-argl)
                                                                        (lett (g (gensym))
                                                                          (push (cons ag g)
                                                                                arg-binding)
                                                                          g) ag))
                                                                  (second def))) ;;arg list
                                               (arg-walker (cddr def) arg-binding ncontext)))) ;;def body
                                   (second body))) ;;labels def list
                            (arg-walker (cddr body) arg-binding context))) ;;labels/flet body
                   ((eq symb 'lambda)
                    (let* ((arg-binding)
                           (arglist (flatten (second body)))
                           (ncontext (union arglist context))
                           (same-argl (intersection context arglist)))
                      (append (list 'lambda ;;lambda
                                    (rmapcar/dot (lambda (ag)
                                                   (if (find ag same-argl)
                                                       (lett (g (gensym))
                                                         (push (cons ag g) arg-binding)
                                                         g) ag))
                                                 (second body))) ;;arg list
                              (arg-walker (cddr body) arg-binding ncontext))));;lambda body
                   (t
                    (cons
                     (lett (win (assoc symb arg-binding))
                       (if win (cdr win) symb))
                     (arg-walker (cdr body) arg-binding context))))))))
