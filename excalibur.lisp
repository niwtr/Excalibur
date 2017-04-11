(in-package :excalibur)

(defmacro lett (varl &body body)
  "light-weighted let with fewer parenthesis"
  `(let (,varl)
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun flatten(x)
    "flatten the list"
    (labels ((rec (x acc)
               (cond ((null x) acc)
                     ((atom x) (cons x acc))
                     (t (rec (car x) (rec (cdr x) acc))))))
      (rec x nil))))

(defun prune (test tree)
  "kill that leave of the tree that satisfies the pred."
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))


(defmacro alambda (var &body body)
  `(labels ((self ,var ,@body))
     #'self))


(defmacro iflambda (var pred then &optional else)
  `(if ,pred
       (lambda ,var  ,then)
       ,(progn
         (if else
             `(lambda ,var ,else) nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun map-a->b (a b &key
                         (step-fn #'1+) (elem-fn #'identity)
                         (step 1))
    "get the integer list from a to b, end when > b."
    (lett (next-fn (iflambda (x)
                             (= step 1)
                             (funcall step-fn x)
                             (+ x step)))
      (do ((i a (funcall next-fn i))
           (result nil))
          ((> i b) (nreverse result))
        (push (funcall elem-fn i) result)))))

#|
;;run these code to see how reader macro save the time.
(defun iter1000 (fn)
  (lett (lst '#.(map-0->n 1000))
    (mapcar fn lst)))

(defun iter1000s (fn)
  (lett (lst (map-0->n 1000))
    (mapcar fn lst)))

(defun test ()
  (princ (time (loop for i from 1 to 100000 do (iter1000 #'1+)))) (terpri)
  (princ (time (loop for i from 1 to 100000 do (iter1000s #'1+)))) (terpri))
|#


(defun map-0->n (n &key (step-fn #'1+) (step 1) (fn #'identity))
  "get the integer list from 0 to n"
  (map-a->b 0 n :step-fn step-fn :step step :elem-fn fn))


(defun map-1->n (n &key (step-fn #'1+) (step 1) (fn #'identity))
  "get the integer list from 1 to n"
  (map-a->b 1 n :step-fn step-fn :step step :elem-fn fn))

(defun tmapcar (fn &rest args)
  "mapcar on the tree!"
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'tmapcar fn args))
             args)))


(defun compose (&rest fns)
  "compose functions from left to right."
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))


(defun zipper (&rest l)
  "zip up the lists given."
  ;;trashes the extra elements.
  (apply #'mapcar #'list l))

(defun firstn (n l)
  "get the first n elements of a list"
  (labels ((iter (ls n acc)
             (cond ((null ls) (nreverse acc))
                   ((= n 0) (nreverse acc))
                   (t (iter (cdr ls) (1- n)
                            (cons (car ls) acc))))))
    (iter l n nil)))


(defun liter-n (n fn l)
  "(liter-n 2 (lambda (a b) (+ a b)) '(1 2 3 4 5 6))"
  (labels ((iter (ls acc)
             (let ((cur (firstn n ls)))
               (if (null cur) (nreverse acc)
                   (iter
                    (nthcdr n ls) (cons (apply fn cur) acc))))))
    (iter l nil)))


(defun proj<- (l &rest res)
  (let* ((argl (cons l res))
         (fu-list (apply #'mapcar #'list argl)))
    (lambda (x)
      (cdr (assoc x fu-list)))))


(defmacro proj/assoc (l dir r)
  ;;a lightweight yet slow version of projf.
  "generate a function containing a closure of association"
  `(let ((symb ',dir))
     (cond ((eql symb '->)
            (let ((fu-list (mapcar #'cons ,l ,r)))
              (lambda (x)
                (cdr (assoc x fu-list)))))
           ((eql symb '<-)
            (let ((fu-list (mapcar #'cons ,r ,l)))
              (lambda (x)
                (cdr (assoc x fu-list))))))))


(defmacro proj (ll dir rl &key (test #'eql))
  "generate a function that maps the key to the val."
  ;;uses hash-table, the test function could be used.
  `(let ((symb ',dir)
         (htable (make-hash-table :test ,test)))
     (cond ((eql symb '->)
            (mapcar (lambda (key val)
                      (setf (gethash key htable) val))
                    ,ll ,rl))

           ((eql symb '<-)
            (mapcar (lambda (key val)
                      (setf (gethash key htable) val))
                    ,rl ,rl))
           (t (error "projf: error direction symbol.")))
     (lambda (key)
       (gethash key htable))))





(defmacro with-gensyms ((&rest names) &body body)
  "well you'd better use the with-gensym-walker"
  `(let ,(mapcar (lambda (n)
                   (list n `(gensym)))
                 names)
     ,@body))




(defun primep (x)
  (let ((l (cdr (butlast (map-1->n x)))))
    (not (some (lambda (x) (not (null x)))
               (mapcar #'(lambda (i)
                           (if (= 0 (mod x i) ) t nil)) l)))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


(defun explode (sym)
  (map 'list (lambda (c)
               (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun g!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "G!"
                  :start1 0
                  :end1 2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rmapcar (fn lst)
    "recursive mapcar"
    (let ((carl (car lst)))
      (cond
        ((null lst) nil)
        ((atom carl) (cons (funcall fn carl)
                           (rmapcar fn (cdr lst))))
        (t (cons (rmapcar fn carl)
                 (rmapcar fn (cdr lst))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun plassoc (var lst &key (test #'eql))
    "well, the assoc for plist version."
    (cadr (assoc var lst :test test))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-gensym-walker (&body body)
    "walk the code containing g! symbols, turn those symbols into gensyms"
    (let* ((g-bang-symbols (remove-duplicates
                            (remove-if-not #'g!-symbol-p
                                           (flatten body))))
           (symblist (mapcar (lambda (symb)
                               `(,symb ,(gensym))) g-bang-symbols)))
      (rmapcar (lambda (symb)
                 (if (g!-symbol-p symb)
                     (plassoc symb symblist)
                     symb))
               (car body)))))

(defmacro defun/memo (funname (&rest var) &body body)
  "define a function that does memoization."
  (with-gensym-walker
    `(let ((g!memo (make-hash-table :test #'equal)))
       (defun ,funname ,var
         (labels ((g!fun ,var ,@body))
           (multiple-value-bind (g!res g!win)
               (gethash (list ,@var) g!memo)
             (if g!win g!res
                 (let ((g!the-res (funcall #'g!fun ,@var)))
                   (setf (gethash (list ,@var) g!memo) g!the-res)
                   g!the-res))))))))



(defmacro lambda/memo ((&rest var) &body body)
  (with-gensym-walker
    `(let ((g!memo (make-hash-table :test #'equal)))
       (lambda ,var
         (labels ((g!fun ,var ,@body))
           (multiple-value-bind (g!res g!win)
               (gethash (list ,@var) g!memo)
             (if g!win g!res
                 (let ((g!the-res (funcall #'g!fun ,@var)))
                   (setf (gethash (list ,@var) g!memo) g!the-res)
                   g!the-res))))))))




(defmacro flatmac (&body body)
  (flatten body))



(defun repeat (thing number)
  (if (= number 0) nil
      (cons thing (repeat thing (- number 1)))))




(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun alistp (lst)
    "is this an alist? "
    (and (listp lst)
         (not (null (cdr (last lst))))))

  (defun index-list (symb lst)
    "generate index of a list."
    (if (alistp lst)
        (let ((lst-head (append (butlast lst)
                                (list
                                 (car (last lst))))))
          (append
           (loop for cell in lst-head
              for count from 0 collect
                (if (atom cell)
                    `(elt ,symb ,count)
                    (index-list `(elt ,symb ,count) cell)))
           `(cdr (last ,symb))))
        (loop for cell in lst
           for count from 0 collect
             (if (atom cell)
                 `(elt ,symb ,count)
                 (index-list `(elt ,symb ,count) cell)))))
  (defun walk-outside-quote (lst fn)
    "walk actual code (outside the quote)."
    (cond ((null lst) nil)
          ((atom lst)
           (funcall fn lst))
          ((eq (car lst) 'quote) lst)
          (t
           (cons (walk-outside-quote
                  (car lst) fn)
                 (walk-outside-quote
                  (cdr lst) fn)))))
  (defun tree-shape (fn &rest trees)
    "mapcar on trees with same shape."
    (cond ((some #'null trees) nil)
          ((some #'atom trees) (apply fn trees))
          (t (cons
              (apply #'tree-shape (cons fn
                                        (mapcar #'car trees)))
              (apply #'tree-shape (cons fn
                                        (mapcar #'cdr trees)))))))


  (defun decon-walker (argl body)
    "walk code and replace args with their deconstructions."
    (setf argl
          (mapcar (lambda (arg)
                    (if (atom arg)
                        arg
                        (let ((fake-arg (gensym)))
                          (tree-shape
                           (lambda (ag index)
                             (setf body
                                   (walk-outside-quote
                                    body
                                    (lambda (x)
                                      (if (eql x ag) index
                                          x)))))
                           arg (index-list fake-arg arg))
                          fake-arg)))
                  argl))
    (values argl body))

  (defun rmapcar/dot (fn lst)
    "generate index of a list."
    (if (alistp lst)
        (let ((lst-head (append (butlast lst)
                                (list
                                 (car (last lst))))))
          (append
           (loop for cell in lst-head collect
                (if (atom cell)
                    (funcall fn cell)
                    (rmapcar/dot fn cell)))
           (funcall fn (cdr (last lst)))))
        (loop for cell in lst collect
             (if (atom cell)
                 (funcall fn cell)
                 (rmapcar/dot fn cell)))))

  (defun arg-walker (body &optional arg-binding context)
    "walk the code,
   replace arguments of different lexical env with different gensyms."
    (cond ((null body) nil)
          ((consp (first body))
           (cons (arg-walker (car body) arg-binding context)
                 (arg-walker (cdr body) arg-binding context)))
          (t
           (let ((symb (first body)))
             (symbol-macrolet ((ncontext (union context arglist))
                               (same-argl (intersection context arglist))
                               (arg-gen (lambda (ag)
                                          (if (find ag same-argl)
                                              (lett (g (gensym
                                                        (symbol-name ag)))
                                                (push (cons ag g) arg-binding) g)
                                              ag))))
               (cond ((eq symb 'quote) body)
                     ((eq symb 'defun)
                      (let ((arg-binding)
                            (arglist (flatten (third body))))
                        (append (list 'defun (second body) ;;defun name
                                      (rmapcar/dot arg-gen (third body))) ;;arg list
                                (arg-walker (cdddr body) arg-binding ncontext))));;defun body
                     ((or (eq symb 'labels) (eq symb 'flet))
                      (append (list 'labels
                                    (mapcar
                                     (lambda (def)
                                       (let ((arg-binding)
                                             (arglist (flatten (second def))))
                                         (append (list (first def) ;;name
                                                       (rmapcar/dot arg-gen (second def))) ;;def arg list
                                                 (arg-walker (cddr def) arg-binding ncontext)))) ;;def body
                                     (second body))) ;;labels def list
                              (arg-walker (cddr body) arg-binding context))) ;;labels/flet body
                     ((eq symb 'lambda)
                      (let ((arg-binding)
                            (arglist (flatten (second body))))
                        (append (list 'lambda ;;lambda
                                      (rmapcar/dot arg-gen (second body))) ;;arg list
                                (arg-walker (cddr body) arg-binding ncontext))));;lambda body
                     (t
                      (cons
                       (lett (win (assoc symb arg-binding))
                         (if win (cdr win) symb))
                       (arg-walker (cdr body) arg-binding context)))))))))
  ;;end of eval-when
  )




(defmacro with-deconstruction (body)
  "offers feature for auto-deconstruction. currently support
   defun, lambda, flet, labels and user-defined symbols that
   are correspond to those petterns."
  (let* ((body (arg-walker body))
         (def (first body))
         (original-body body))
    (cond ((eq def 'lambda)
           (multiple-value-bind (argl body)
               (decon-walker (second body) (cddr body))
             `(,def ,argl ,@body)))
          ((eq def 'defun)
           (multiple-value-bind (argl body)
               (decon-walker (third body) (cdddr body))
             `(,def ,(second original-body) ,argl ,@body)))
          ((or (eq def 'labels) (eq def 'flet))
           `(,def ,(mapcar (lambda (fn-body)
                             (multiple-value-bind (argl body)
                                 (decon-walker (second fn-body) (cddr fn-body))
                               `(,(first fn-body) ,argl ,@body)))
                           (cadr body))
                ,@(cddr original-body)))
          (t (error "Pattern not supported. -- with-deconstruction")))))




(defmacro lambda/d ((&rest argl) &body body)
  "lambda with auto deconstruction of argument list.
   (let ((lst '((1 (2)) (3 (4)) (5 (6)))))
     (mapcar
      (lambda/d ((x (y))) (+ x y)) lst))
   => (3 7 11)"
  (multiple-value-bind (argl body)
      (decon-walker argl body)
    `(lambda ,argl ,@body)))
#+obselete
(defmacro lambda/d ((&rest argl) &body body)
  (with-gensym-walker
    `(lambda (g!arg-name)
       (destructuring-bind ,@argl
           g!arg-name
         ,@body))))




;;example
#|
;; this would expand nestedly.

(with-deconstruction
    (defun foo ((x y))
      (princ (* x y))
      (progn
        (princ (+ x y))
        (with-deconstruction
            (labels ((bar ((z . y)) (- x y z)))
              (with-deconstruction
                    (lambda ((x)) (princ x)))
                    (bar (cons (funcall (lambda (x) (+ x 1)) x) y)))))))

;; would not provide cliche when not in need.
(with-deconstruction
    (defun foo (x)
      (lambda (y)
        (+ x y))))

|#



(defun biggest (lst &key (key #'identity))
  (reduce (lambda (x &optional y)
            (if y
                (if (> (funcall key x) (funcall key y)) x y)
                x))
          lst))


(defun smallest (lst &key (key #'identity))
  (reduce (lambda (x &optional y)
            (if y
                (if (< (funcall key x) (funcall key y)) x y)
                x))
          lst))

#|
(defun most (data &key (key #'identity) (test #'eql))
  (let ((reduced (remove-duplicates data :key key :test test)))
    (car
     (reduce (lambda/d ((x . xn) (y . yn))
            (if (> xn yn) (cons x xn) (cons y yn)))
      (mapcar (lambda (thing)
                (cons thing (count thing data :key key :test test)))
              reduced)))))
|#

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (car lst))
          (lett (score (funcall fn obj))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(defun mosts (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((rst (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (lett (score (funcall fn obj))
            (cond ((> score max)
                   (setq max score
                         rst (list obj)))
                  ((= score max)
                   (push obj rst)))))
        (values (nreverse rst) max))))



(defun best (fn lst)
  (if (null lst)
      nil
      (lett (wins (car lst))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))







(defun all-the-same (data &key (key nil) (test #'eql))
  (or (null data)
      (= (length (remove-duplicates data :key key :test test)) 1)))

(defun symb (&rest strs)
  (intern
   (apply #'concatenate 'string
          (mapcar (lambda (str)
                    (string-upcase str))
                 (remove nil strs)))))




(defun argmax (lst)
  (loop
     with len = (length lst)
     with arg = 0
     with rst = (car lst)
     for i from 0 to len
     for j from 1 to (1- len)
     for a in (butlast lst)
     for b in (cdr lst)
     when (> b a)
     do (progn
          (setf arg j)
          (setf rst b))
     finally (return (values arg rst))))


(defun transpose (matrix)
  (apply #'mapcar #'list matrix))



(defun filter (fn lst)
  (lett (acc)
        (dolist (x lst)
          (lett (val (funcall fn x))
                (if val (push val acc))))
          (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "Zero length! -- group"))
  (labels ((rec (source acc)
             (lett (rest (nthcdr n source))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))



