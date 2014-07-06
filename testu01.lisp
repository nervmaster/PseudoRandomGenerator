(defpackage "UNIF01"
  (:export "CREATE-EXTERN-GENBITS" "DELETE-EXTERN-GENBITS"))

(defpackage "BBATTERY"
  (:export "SMALL-CRUSH"))

(load-shared-object "libtestu01.so.0")

(load "linear.cl") ;;load my algorithm
(load "MersenneTwister.cl")
(initialize-generator 1234567980)

(define-alien-routine ("unif01_CreateExternGenBits" unif01:create-extern-genbits)
    (* t)
  (name c-string) (genb (function unsigned-int)))

(define-alien-routine ("unif01_DeleteExternGenBits" unif01:delete-extern-genbits)
    void
  (genb (* t)))


(sb-alien::define-alien-callback mt-genb unsigned-int ()
  (extract-number))

(define-alien-routine ("bbattery_SmallCrush" bbattery:small-crush)
    void
  (gen (* t)))

(let ((genb (unif01:create-extern-genbits "MERSENNE_TWISTER" mt-genb)))
  (bbattery:small-crush genb)
  (unif01:delete-extern-genbits genb))

(quit)
