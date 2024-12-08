(defun clean-grep-advice (orig-function &rest grep-args)
  (let* ((output (apply orig-function grep-args))
         (clean-output (some-regex-function "filter parameter" output)))
    clean-output))

;; 1. Because this is an advice, you need to put the original function as one
;; of the parameters. Then we need to allow the advice to also take parameters
;; that come as an input to the original function. &rest allows to provide any
;; number of arguments.
;;
;; 2. Because grep-args is a list (that comes from using &rest), we need to use
;; 'apply', so that orig-function doesn't get the list as a single parameter.
;;
;; 4. We need to return the output of the orig-function that is captured. And it
;; should be within the sexp of the *let.
