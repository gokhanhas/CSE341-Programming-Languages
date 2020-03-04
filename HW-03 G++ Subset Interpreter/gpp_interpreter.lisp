; *********************************************
; *  341 Programming Languages                *
; *  Homework 3 => G++ Interpreter            *
; *  Gokhan Has - 161044067                   *
; *********************************************


;; Bir önceki ödevdeki lexer'ın cagrilmasi ...
(load "gpp_lexer.lisp")

(setq KEYWORDS_LIST '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false" "while"))
(setq OPERATORS_LIST '("+" "-" "/" "*" "(" ")" "**" """" ","))

;; Listenin son elemanını siler ...
(defun remove-last-element (list)
    (loop for l on list
        while (rest l)
        collect (first l)
    )
)

;; Listenin tersini alır ...
(defun reverse-list (l)
    (cond
        ((null l) '())
        (T (append (reverse-list (cdr l)) (list (car l))))
    )
) 

;; İki listeyi birleştirir ...
(defun concat-lists(seq1 seq2)
    (cond ((not (null seq1)) (cons (car seq1) (concat-lists (cdr seq1) seq2)))
          (T (cond ((not (null seq2)) (cons (car seq2) (concat-lists seq1 (cdr seq2))))
                   (T nil))))
)

;; String1 in string2 de olup olmadığını kontrol eder ...
;; Varsa kendisini yoksa NIL döndürür ...
(defun string-include (string1 string2)
    (cond
        ((zerop (length string1)) nil) 
        ((> (length string1) (length string2)) nil) 
        ((string= string1 (subseq string2 0 (length string1))) string1) 
        (t (string-include string1 (subseq string2 1)))
    )
) 

;; LEXER den dönen TOKEN ler karsılastırılarak syntax_error olup olmadıgına karar verilir ...
(defun check-tokens(liste)
    (setq OPcount 0)
    (setq CPcount 0)
    (setq deffunControl 0)
    (dolist (subList liste)
        (if (equal (nth 0 subList) "OP_OP")
            (setq OPcount (+ OPcount 1))
        )
        (if (equal (nth 0 subList) "OP_CP")
            (setq CPcount (+ CPcount 1))
        )
        (if (not (equal (string-include "SYNTAX__ERROR" (nth 0 subList)) nil))
            (progn 
                (if (not (equal (nth 1 sublist) "'"))
                    (return-from check-tokens nil)
                )
            )
        )
        (if (equal (nth 0 subList) "KW_DEFFUN")
            (setq deffunControl 1)
        )
    )

    (if (not (equal OPcount CPcount))
        (return-from check-tokens nil)
        (progn 
            (if (equal deffunControl 1)
                (return-from check-tokens "itisDeffun")
                (return-from check-tokens T)
            )
        )
    )
)

;; DOULEMULTI Operator ...
(defun DBMULTI(number1 number2)
    (setq result 1)
    (setq control 0)
    (loop 
        (setq result (* result number1))
        (setq control (+ control 1))
        (when (equal control number2) (return-from DBMULTI result))
    )
)

;; Expression'da INTEGERVALUE olup olmadığını kontrol eder ...
(defun check-integer-value(expression)
    (setq controlValue 0)
    (dolist (element expression) 
        (if (equal controlValue 1)
            (progn 
                (if (equal (parse-integer element :junk-allowed t) nil)
                    (return-from check-integer-value nil)
                )
            )
        )
        (setq controlValue 1)
    )
    T
)

;; Expression'da BINARYVALUE olup olmadığını kontrol eder ...
(defun check-binary-value(expression)
    (setq controlValue 0)
    (dolist (element expression) 
        (if (equal controlValue 1)
            (progn 
                (if (equal (or (equal element "true") (equal element "false") (equal element "\"true\"") (equal element "\"false\"")) nil)
                    (return-from check-binary-value nil)
                )
            )
        )
        (setq controlValue 1)
    )
    T
)

;; True veya False olma durumunu belirtir ...
(defun change-binary-value(trueORfalse)
    (if (or (equal trueORfalse "true")  (equal trueORfalse"\"true\""))
        T
        nil
    )
)

;; Operators function ....
;; ( + EXP_INTEGER EXP_INTEGER )
(defun itisPLUS(expression)
    (if (equal (not (check-integer-value expression)) nil)
        (+ (parse-integer (nth 1 expression)) (parse-integer (nth 2 expression)))
        nil
    )
)

;; ( - EXP_INTEGER EXP_INTEGER )
(defun itisMINUS(expression)
    (if (equal (not (check-integer-value expression)) nil)
        (- (parse-integer (nth 1 expression)) (parse-integer (nth 2 expression)))
        nil
    )
)

;; ( / EXP_INTEGER EXP_INTEGER )
;; SADECE INTEGER VE TAM BOLUNEBILEN SAYILARDA CALISIR ...
(defun itisDIVIDE(expression)
    (if (equal (not (check-integer-value expression)) nil)
        (/ (parse-integer (nth 1 expression)) (parse-integer (nth 2 expression)))
        nil
    )
)

;; ( * EXP_INTEGER EXP_INTEGER )
(defun itisMULTIPLY(expression)
    (if (equal (not (check-integer-value expression)) nil)
        (* (parse-integer (nth 1 expression)) (parse-integer (nth 2 expression)))
        nil
    )
)

;; ( ** EXP_INTEGER EXP_INTEGER )
(defun itisDOUBLEMULTIPLY(expression)
    (if (equal (not (check-integer-value expression)) nil)
        (return-from itisDOUBLEMULTIPLY (DBMULTI (parse-integer (nth 1 expression)) (parse-integer (nth 2 expression))))
        nil
    )
)

;; ( and EXP_BINARY EXP_BINARY )
(defun itisAND(expression)
    (if (equal (not (check-binary-value expression)) nil)
        (progn 
            (setq resultB (and (change-binary-value (nth 1 expression)) (change-binary-value (nth 2 expression))))
            (if (equal resultB T)
                (return-from itisAND "true")
                (return-from itisAND "false")
            )
        )
        nil
    )
)

;; ( or EXP_BINARY EXP_BINARY )
(defun itisOR(expression)
    (if (equal (not (check-binary-value expression)) nil)
        (progn 
            (setq resultB (or (change-binary-value (nth 1 expression)) (change-binary-value (nth 2 expression))))
            (if (equal resultB T)
                (return-from itisOR "true")
                (return-from itisOR "false")
            )
        )
        nil
    )
)

;; ( not EXP_BINARY )
(defun itisNOT(expression)
    (if (equal (not (check-binary-value expression)) nil)
        (progn 
            (setq resultB (not (change-binary-value (nth 1 expression))))
            (if (equal resultB T)
                (return-from itisNOT "true")
                (return-from itisNOT "false")
            )
        )
        nil
    )
)

;; ( equal EXP_BINARY EXP_BINARY ) veya ( equal EXP_INTEGER EXP_INTEGER ) 
(defun itisEQUAL(expression)
    (if (equal (not (check-binary-value expression)) nil)
        (progn 
            (setq resultB (equal (change-binary-value (nth 1 expression)) (change-binary-value (nth 2 expression))))
            (if (equal resultB T)
                (return-from itisEQUAL "true")
                (return-from itisEQUAL "false")
            )
        )
        (progn 
            (if (equal (not (check-integer-value expression)) nil)
                (progn 
                    (setq resultB (equal (parse-integer (nth 1 expression)) (parse-integer (nth 2 expression))))
                    (if (equal resultB T)
                        (return-from itisEQUAL "true")
                        (return-from itisEQUAL "false")
                    )
                )
                nil
            )
        )
    )
)

;; ( less EXP_INTEGER EXP_INTEGER ) 
(defun itisLESS(expression)
    (if (equal (not (check-integer-value expression)) nil)
        (progn 
            (setq resultB (< (parse-integer (nth 1 expression)) (parse-integer (nth 2 expression))))
            (if (equal resultB T)
                (return-from itisLESS "true")
                (return-from itisLESS "false")
            )
        )
        nil
    )
)

;; ( set ID EXP_INTEGER ) or ( set ID EXPLIST_INTEGER ) 
;; EXP_INTEGER'ı veya EXPLIST_INTEGER'ı ekrana basar ...
(defun itisSET(expression) 
    (if (equal (parse-integer (nth 2 expression) :junk-allowed t) nil)
        (progn 
            (if (equal (type-of (nth 2 expression)) 'CONS)
                (return-from itisSET (nth 2 expression))
                (return-from itisSET nil)
            )
        )
        (return-from itisSET (parse-integer (nth 2 expression)))
    )
)

;; Kurallara gore sadece INTEGER sayılarda liste olusturabilmektedir ...
;; Biri bile string veya character (sembol) olsa hata verir ...
(defun itisLIST(expression)
    (setq controlValue 0)
    (setq returnList '())
    (dolist (element expression) 
        (if (equal controlValue 1)
            (progn 
                (if (equal (parse-integer element :junk-allowed t) nil)
                    (progn 
                        (setq liste1 (read-from-string element))
                        (if (equal (type-of liste1) 'CONS)
                            (setq returnList (append returnList (list liste1)))
                            (return-from itisLIST nil)
                        )
                    )
                    (setq returnList (append returnList (list (parse-integer element)))) 
                )
            )
        )
        (setq controlValue 1)
    )
    (return-from itisLIST returnList)
)

;; ( append EXP_INTEGER EXPLIST_INTEGER ) veya ( append EXPLIST_INTEGER EXPLIST_INTEGER )
;; Ya iki INTEGER_LISTE ALIR YA DA BIR INTEGER BIR INTEGER_LIST ALIR ...
;; INTEGER'I BIR LISTEYE EKLEME SIRASINDA (append 4 '(1 2 3)) SONUC : (1 2 3 . 4) olur. 
;; NORMAL CLISP'DE DENENDIGINDE DE SONUC BOYLE OLMAKTADIR. BU BIR HATA DEGILDIR ...
(defun itisAPPEND(expression) 
    (setq liste1 '())
    (setq liste2 '())
    (setq liste1 (read-from-string (nth 1 expression)))
    (setq liste2 (read-from-string (nth 2 expression)))
    (if (equal (type-of liste2) 'CONS)
        (progn 
            (if (equal (parse-integer (nth 1 expression) :junk-allowed t) nil)
                (progn 
                    (if (equal (type-of liste1) 'CONS)
                        (progn 
                            (setq liste2 (append liste1 liste2))
                            (return-from itisAPPEND liste2) 
                        )
                    )
                    nil
                )
                (progn 
                    (setq liste2 (append liste2 (parse-integer (nth 1 expression))))
                    (return-from itisAPPEND liste2)
                )
            )
        )
        nil
    )
)

;; ( concat EXPLIST_INTEGER EXPLIST_INTEGER )
;; IKI EXPLIST_INTEGER ' I BIRLESTIRIR ...
(defun itisCONCAT(expression)
    (setq liste1 '())
    (setq liste2 '())
    (setq liste1 (read-from-string (nth 1 expression)))
    (setq liste2 (read-from-string (nth 2 expression)))   
    (if (and (equal (type-of liste1) 'CONS) (equal (type-of liste2) 'CONS))
        (return-from itisCONCAT (concat-lists liste1 liste2))
        nil
    )
)

;; ( if EXP_BINARY EXPLIST_INTEGER ) veya ( if EXP_BINARY EXPLIST_INTEGER EXPLIST_INTEGER )
;; Binary değere göre listeyi veya false döndürür ....
;; Binary değere göre ilk listeyi veya ikinci listeyi döndürür ...
(defun itisIF(expression)
    (setq EX_BI (nth 1 expression))
    (if (equal (or (equal EX_BI "true") (equal EX_BI "false") (equal EX_BI "\"true\"") (equal EX_BI "\"false\"")) nil)
        (return-from check-binary-value nil)
        (progn 
            (cond 
                ((equal (length expression) 3)
                    (progn 
                        (if (or (equal EX_BI "false") (equal EX_BI "\"false\""))
                            (return-from itisIF "false")
                            (progn 
                                (setq liste2 '())
                                (setq liste2 (read-from-string (nth 2 expression)))
                                (if (equal (type-of liste2) 'CONS)
                                    (return-from itisIF liste2)
                                    nil
                                )     
                            )
                        )                    
                    )
                )
                ((equal (length expression) 4)
                    (progn 
                        (setq liste1 '())
                        (setq liste2 '())
                        (setq liste1 (read-from-string (nth 2 expression)))
                        (setq liste2 (read-from-string (nth 3 expression)))   
                        (if (and (equal (type-of liste1) 'CONS) (equal (type-of liste2) 'CONS))
                            (progn 
                                (if (or (equal EX_BI "true") (equal EX_BI "\"true\""))
                                    (return-from itisIF liste1)
                                    (return-from itisIF liste2)
                                )
                            )
                            nil
                        )
                    )
                )
                (t nil)
            )
        )
    )
)

;; ( if EXP_BINARY EXPLIST_INTEGER ) 
;; Binary değere göre listeyi veya false döndürür ....
(defun itisWHILE(expression) 
    (setq EX_BI (nth 1 expression))
    (if (equal (or (equal EX_BI "true") (equal EX_BI "false") (equal EX_BI "\"true\"") (equal EX_BI "\"false\"")) T)
        (progn 
            (if (or (equal EX_BI "false") (equal EX_BI "\"false\""))
                (return-from itisWHILE "false")
                (progn 
                    (setq liste2 '())
                    (setq liste2 (read-from-string (nth 2 expression)))
                    (if (equal (type-of liste2) 'CONS)
                        (return-from itisWHILE liste2)
                        nil
                    )     
                )
            )
        )
    )
)

(defun itisLOAD(expression)
    (if (type-of (nth 1 expression) 'simple-array)
        (return-from itisLOAD (nth 1 expression))
        nil
    )
)


;; Deffun icin ozel syntax kontrolu gerekmektedir ...
;; Deffun kontrolu onceki fonksiyonlarda yapılmaktadır. Buraya geliyorsa zaten yoktur ...
(defun itisDeffun(expression)
    (format t "SYNTAX OK. ~%")
)

;; Expressionları kontrol eden yardimci bir fonksiyondur ...
;; Expression yaptığım algoritmaya göre bir listedir ve eleman sayısına göre syntax kontrol yapılabilir ....
(defun check(expression) 
    (cond 
        ( (not (equal (searchList (nth 0 expression) OPERATORS_LIST) nil))
            ;; operator kontrolü ...
            (progn 
                (cond 
                    ( (equal (nth 0 expression) "+") ;; + operatoru ise
                        (if (equal (length expression) 3)
                            (progn 
                                (setq result (itisPLUS expression))
                                (return-from check result)
                            )
                            nil
                        )
                    )
                    ( (equal (nth 0 expression) "-") ;; - operatoru ise
                        (if (equal (length expression) 3)
                            (progn 
                                (setq result (itisMINUS expression))
                                (return-from check result)
                            )
                            nil
                        )
                    )
                    ( (equal (nth 0 expression) "/") ;; / operatoru ise
                        (if (equal (length expression) 3)
                            (progn 
                                (setq result (itisDIVIDE expression))
                                (return-from check result)
                            )
                            nil
                        )
                    )
                    ( (equal (nth 0 expression) "*") ;; "*" operatoru ise
                        (if (equal (length expression) 3)
                            (progn 
                                (setq result (itisMULTIPLY expression))
                                (return-from check result)
                            )
                            nil
                        )
                    )
                    ( (equal (nth 0 expression) "**") ;; "**"  operatoru ise
                        (if (equal (length expression) 3)
                            (progn 
                                (setq result (itisDOUBLEMULTIPLY expression))
                                (return-from check result)
                            )
                            nil
                        )
                    )
                )
            )
        )
        ( (not (equal (searchList (nth 0 expression) KEYWORDS_LIST) nil))
            ;; keyword kontrolü ...
            (cond 
                ( (equal (nth 0 expression) "and") ;; and keywordu ise
                    (if (equal (length expression) 3)
                        (progn 
                            (setq result (itisAND expression))
                            (return-from check result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "or") ;; or keywordu ise
                    (if (equal (length expression) 3)
                        (progn 
                            (setq result (itisOR expression))
                            (return-from check result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "not") ;; not keywordu ise
                    (if (equal (length expression) 2)
                        (progn 
                            (setq result (itisNOT expression))
                            (return-from check result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "equal") ;; equal keywordu ise
                    (if (equal (length expression) 3)
                        (progn 
                            (setq result (itisEQUAL expression))
                            (return-from check result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "less") ;; less keywordu ise
                    (if (equal (length expression) 3)
                        (progn 
                            (setq result (itisLESS expression))
                            (return-from check result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "set") ;; set keywordu ise
                    (if (equal (length expression) 3)
                        (progn 
                            (setq result (itisSET expression))
                            (return-from check result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "list") ;; set keywordu ise
                    (progn 
                        (setq result "")
                        (if (equal (length expression) 1) 
                            (setq result "NIL")
                            (setq result (itisLIST expression))
                        )
                        (return-from check result)
                    )  
                )
                ( (equal (nth 0 expression) "append") ;; append keywordu ise
                    (progn 
                        (setq result (itisAPPEND expression))
                        (return-from check result)
                    )  
                )
                ( (equal (nth 0 expression) "concat") ;; concat keywordu ise 
                    (if (equal (length expression) 3)
                        (progn 
                            (setq result (itisCONCAT expression))
                            (return-from check result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "if") ;; if keywordu ise 
                    (progn 
                        (setq result (itisIF expression))
                        (return-from check result) 
                    )
                    nil
                )
                ( (equal (nth 0 expression) "while") ;; while keywordu ise 
                    (if (equal (length expression) 3)
                        (progn 
                            (setq result (itisWHILE expression))
                            (return-from check result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "deffun") ;; deffun keywordu ise 
                    (return-from check "SYNTAX OK.")     
                )
                ( (equal (nth 0 expression) "load") ;; load keywordu ise 
                    (if (equal (length expression) 2)
                        (progn 
                            (setq result (itisLOAD expression))
                            (return-from check result)   
                        )
                        nil
                    ) 
                )
            )
        )
        (t nil)
    )
)

(defun helper-parser(string_)
    ;; Stackler string tutacaklar ...
    (setq stack1 '())
    (setq stack2 '())
    (setq word "")
    (setq controlFirst T)
    (setq listControl nil)
    (setq previousCH #\1)

    (loop for ch across string_ do 
        
        ;; (format t "~%Stack 1 : ~a~%" stack1)
        ;; (format t "Stack 2 : ~a~%" stack2)
        (if (equal controlFirst T)
            (progn 
                (if (equal (or (equal ch (code-char 39.)) (equal ch (code-char 40))) T)
                    (progn 
                        (if (equal ch (code-char 39.)) 
                            (setq listControl T)
                        )
                        (setq controlFirst nil)
                    )
                    (progn 
                        (format t "~%SYNTAX_ERROR Expression not recognized~%")
                        (return-from helper-parser nil)
                    )
                )
            )
            (progn
                (cond 
                    ((equal ch (code-char 41.)) ;; Kapalı parantez kontrolü
                        (progn 
                            (setq rev-stack1 (reverse-list stack1))
                            (dolist (element rev-stack1) ;; Acık parantez bulana kadar git ...
                                (if (equal element (string (code-char 40.)))
                                    (progn 
                                        (setq stack1 (remove-last-element stack1)) 
                                        (return)
                                    )
                                    (progn 
                                        (setq stack2 (append stack2 (list element)))
                                        (setq stack1 (remove-last-element stack1)) 
                                    ) 
                                )
                            )
                            (setq stack2 (reverse-list stack2))  ;; Listenin tersini almak lazım yoksa problem cikarmaktadir ...
                            (setq control (check stack2))
                            (if (equal control nil)
                                (progn 
                                    (format t "~%SYNTAX_ERROR Expression not recognized~%")
                                    (return-from helper-parser nil)
                                )
                                (progn 
                                    (setq stack2 '())
                                    (setq stack1 (append stack1 (list (write-to-string control))))
                                )                    
                            )
                        )
                    )

                    ((equal ch #\space) ;; Bosluk kontrolu ...
                        (progn 
                            (if (not (equal word ""))
                                (progn 
                                    (setq stack1 (append stack1 (list word))) ;; Bosluksa stack1'e push et ...
                                    (setq word "")
                                )
                            )
                        )
                    )
                    
                    ((equal listControl T) ;; ilk basta '() ile liste olusturabildiginden onu kontrol eder ...
                        (progn 
                            (if (equal ch (code-char 40.))
                                (progn 
                                    (setq stack1 (append stack1 (list "list")))
                                    (setq listControl nil)
                                )
                            )
                        )   
                    )

                    ((not (equal ch #\;))    ; Yorum satırını anlamak icin yazılmıştır. Yorum satırı ise program bir şey yapmayacak.
                        (progn 
                            (if (and (equal previousCH #\') (equal ch (code-char 40.)))
                                (progn ;; '( gelirse liste oldugunu anlaması icin
                                    (setq stack1 (append stack1 (list "(")))
                                    (setq stack1 (append stack1 (list "list")))
                                    (setq word "")
                                )
                                (setf word (concatenate 'string word (string ch)))
                            )
                        )   
                    )
                )
            )
        )
        (setq previousCH ch)
	)

    ;; Sonucun ekrana basılması ...
    (if (equal (nth 0 stack1) "\"true\"")
        (format t "true")
        (progn 
            (if (equal (nth 0 stack1) "\"false\"")
                (format t "false")
                (progn 
                    (if (not (equal (nth 0 stack1) nil))
                        (format t "~a" (nth 0 stack1))
                    )
                )
            )
        )
    )
    (terpri)
)

(defun gppinterpreter() 
    (format t "###############################################################~%")
    (format t "################ THIS IS THE G++ INTERPRETER ##################~%")
    (format t "################    GOKHAN HAS - 161044067   ##################~%")
    (format t "#### g++ dosyaismi.g++ yazarak dosyadan test edebilirsiniz ####~%")
    (format t "###################  g++ helloword.g++  #######################~%")
    (format t "###############################################################~%")
    (setq number 1)
    (loop 
        (terpri)
        (format t "[~d]> " number)
        (setq read_string (string-downcase (string (read-line))))
        (if (equal (string-include "g++" read_string) nil)
            (progn 
                (if (not (equal read_string "exit"))
                    (progn 
                        (setq controlLOAD 0)
                        (setq number (+ number 1))
                        (setq tokens '())
                        (setq list_ '())
                        (setq list_ (append list_ (list read_string)))
                        (setq read_string (nth 0 (trim-list list_)))
                        
                        (if (not (equal (string-include "( load" read_string) nil))
                            (progn
                                (format t "~%~a~%" (subseq read_string 6 (- (length read_string) 1)))
                                (setq controlLOAD 1)
                            )
                        )
                        (if (equal controlLOAD 0)
                            (progn 
                                (setq tokens (gpp_lexer read_string))
                                (if (not (equal (check-tokens tokens) nil))
                                    (progn 
                                        (if (equal (check-tokens tokens) "itisDeffun")
                                            (itisDeffun tokens)
                                            (helper-parser read_string)
                                        )
                                    )
                                    (format t "~%SYNTAX_ERROR Expression not recognized~%")
                                )
                            )
                        )
                    )
                    (format t "BYE.")
                )
            )
            (progn ;; Dosyadan okuma yapılacak ..
                (setq number (+ number 1))
                (setq read_string (subseq read_string 4 (length read_string)))
                (setq line2 "")
                (let ((in (open read_string :if-does-not-exist nil)))
                    (when in
                        (loop for line = (read-line in nil)
                            while line do 
                            (progn 
                                (setq line2 line)
                                (setq list_ '())
                                (setq list_ (append list_ (list line)))
                                (setq read_string (nth 0 (trim-list list_)))
                                (helper-parser read_string)
                            )
                        )
                        (close in)
                    )
                )
            )
        )
        
        (when (equal read_string "exit") (return ))
    )
)
 
(gppinterpreter)
