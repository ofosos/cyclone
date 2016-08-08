;; srfi 60 - binary operations on numbers
;; Copyright 2016 (c) Mark Meyer (mark@ofosos.org)

(define-library (srfi-60)
  (export
   logand
   logior
   logxor
   lognot)
  (import
   (scheme base))
  (begin
    ; todo maybe more efficiency
    (define (reduce-two lis op id)
      (cond ((null? lis) id)
	    ((list? lis) (apply op (car lis) (reduce-two (cdr lis) op id)))))
    
    (define (check-int lis)
      (cond ((null? lis) #t)
	    ((list? lis)
	     (and (integer? (car lis))
		  (check-int (cdr lis))))))
    (define (int? lis)
      (if (not (check-int lis))
	  (raise 'argument-not-int)))
       
    (define (logand . args)
      (int? args)
      (reduce-two args p_logand -1))
    (define-c p_logand
      "(void *data, int argc, closure _, object k, object x, object y)"
      " int ret = (int)obj_obj2int(x) & (int)obj_obj2int(y); 
        return_closcall1(data, k, obj_int2obj(ret)); 
      ")
    
    (define (logior . args)
      (int? args)
      (reduce-two args p_logior 0))
    (define-c p_logior
      "(void *data, int argc, closure _, object k, object x, object y)"
      " int ret = (int)obj_obj2int(x) | (int)obj_obj2int(y);
        return_closcall1(data, k, obj_int2obj(ret)); ")
    
    (define (logxor . args)
      (int? args)
      (reduce-two args p_logxor 0))
    (define-c p_logxor
      "(void *data, int argc, closure _, object k, object x, object y)"
      " int ret = (int)obj_obj2int(x) ^ (int)obj_obj2int(y);
        return_closcall1(data, k, obj_int2obj(ret)); ")
    
    (define (lognot arg)
      (int? (list arg))
      (p_lognot arg))
    (define-c p_lognot
      "(void *data, int argc, closure _, object k, object x)"
      " int ret = ~(int)obj_obj2int(x);
        return_closcall1(data, k, obj_int2obj(ret)); ")))

