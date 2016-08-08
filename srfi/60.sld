;; srfi 60 - binary operations on numbers
;; Copyright 2016 (c) Mark Meyer (mark@ofosos.org)

;; various stuff adapted from "Hacker's Delight" by Henry S. Warren, Jr. 2002.

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
        return_closcall1(data, k, obj_int2obj(ret)); ")

    ;; chapter 5, p.65
    (define-c (p_popcnt arg)
      "(void *data, int argc, closure _, object k, object x)"
      " int ret = obj_obj2int(x);
        ret = (ret & 0x55555555) + ((ret >> 1) & 0x55555555);
        ret = (ret & 0x33333333) + ((ret >> 2) & 0x33333333);
        ret = (ret & 0x0f0f0f0f) + ((ret >> 4) & 0x0f0f0f0f);
        ret = (ret & 0x00ff00ff) + ((ret >> 8) & 0x00ff00ff);
        ret = (ret & 0x0000ffff) + ((ret >>16) & 0x0000ffff);
        return_closcall1(data, k, obj_int2obj(ret));")))

