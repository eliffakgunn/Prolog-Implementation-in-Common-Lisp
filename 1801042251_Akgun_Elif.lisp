; *********************************************
; *  341 Programming Languages                *
; *  Fall 2020                                *
; *  Author: Elif Akgun                       *
; *********************************************

;;global variables
(defvar fact-list '())
(defvar rule-list '())
(defvar res-list '())
(defvar unknown2 '())
(defvar known2 '())
(defvar ressList '())
(defvar subbList '())
(defvar queryVars '())
(defvar queryValues '())
(defvar val_flag 0)

;parantezlere bakarak okunan satırın fact, rule, ya da query olduğuna karar verir
;fact ise 0, rule ise 1, query ise 2 döndürür
(defun checkParentheses(line)
	(setq len (length line))
	(setq first_ 0)
	(setq last_ 1)

	(setq OPcount 0)
	(setq OCcount 0)
	
	(loop while (/= first_ len) do
		(setq chr (subseq line first_ last_)) 
		(when (equal "(" chr)
			(incf OPcount)
		)
		(when (equal ")" chr)
			(incf OCcount)
		)
		(setq first_ (+ 1 first_))
		(setq last_ (+ 1 last_))
	)

	(when (and (= OPcount OCcount) (= OPcount 4))
		(setq first_ 0)
		(setq last_ 1)
		(setq counter 0)

		(loop while (/= first_ len) do
			(setq chr (subseq line first_ last_)) 

			(when (not (equal chr " "))
				(incf counter)
			)
			(when (= counter 3)
				(if (equal chr ")") ; query
					(return-from checkParentheses 2)
					(return-from checkParentheses 0) ;else fact
				)
			)
			(setq first_ (+ 1 first_))
			(setq last_ (+ 1 last_))
		)
	)
	(when (= OPcount OCcount) ;rule
		(return-from checkParentheses 1))
	(return-from checkParentheses -1)
)

;okunan satır fact ise bu fonskyiona gleir
;uygun formata dönüştürülerek fact listesine eklenir
(defun fact(line)
	(setq len (length line))
	(setq first_ 0)
	(setq last_ 1)	

	(setq fact_ '())

	(setq chr (subseq line first_ last_)) 

	(loop while (or (equal chr "(") (equal chr "\"") (equal chr " ")) do
		(setq first_ (+ 1 first_))
		(setq last_ (+ 1 last_))
		(setq chr (subseq line first_ last_))
	)

	(setq str "")
	(loop while (not (equal chr "\"")) do
		(setq str (concatenate 'string str chr))
		(setq first_ (+ 1 first_))
		(setq last_ (+ 1 last_))
		(setq chr (subseq line first_ last_))			
	)

	(setq fact_ (append fact_ (list str))) 

	(setq sublist '())

	(loop while (not (equal chr ")")) do
		(loop while (or (equal chr "(") (equal chr "\"") (equal chr " ")) do
			(setq first_ (+ 1 first_))
			(setq last_ (+ 1 last_))
			(setq chr (subseq line first_ last_))
		)	

		(setq str "")
		(loop while (and (not (equal chr "\"")) (not (equal chr " ")) (not (equal chr ")"))) do
			(setq str (concatenate 'string str chr))
			(setq first_ (+ 1 first_))
			(setq last_ (+ 1 last_))
			(setq chr (subseq line first_ last_))			
		)		

		(when (/= 0 (length str))
			(setq sublist (append sublist (list str))))
	)

	(setq fact_ (append fact_ (list sublist)))
	(setq fact-list (append fact-list (list fact_)))
)

;okunan satır query ise bu fonksiyona gelir
;uygun formata dönültürülerek çözülür
(defun query (line)
	(setq query_ '())

	(setq len (length line))
	(setq first_ 0)
	(setq last_ 1)	

	(setq chr (subseq line first_ last_)) 

	(loop while (or (equal chr "(") (equal chr ")") (equal chr "\"") (equal chr " ")) do
		(setq first_ (+ 1 first_))
		(setq last_ (+ 1 last_))
		(setq chr (subseq line first_ last_))
	)

	(setq str "")
	(loop while (not (equal chr "\"")) do
		(setq str (concatenate 'string str chr))
		(setq first_ (+ 1 first_))
		(setq last_ (+ 1 last_))
		(setq chr (subseq line first_ last_))			
	)

	(setq query_ (append query_ (list str))) 

	(setq sublist '())

	(loop while (not (equal chr ")")) do
		(loop while (or (equal chr "(") (equal chr "\"") (equal chr " ")) do
			(setq first_ (+ 1 first_))
			(setq last_ (+ 1 last_))
			(setq chr (subseq line first_ last_))
		)	

		(setq str "")
		(loop while (and (not (equal chr "\"")) (not (equal chr " ")) (not (equal chr ")"))) do
			(setq str (concatenate 'string str chr))
			(setq first_ (+ 1 first_))
			(setq last_ (+ 1 last_))
			(setq chr (subseq line first_ last_))			
		)		

		(when (/= 0 (length str))
			(setq sublist (append sublist (list str))))
	)
	(setq query_ (append query_ (list sublist)))

	(return-from query query_)

)

;okunan satır rule ise bu fonksiyona gönderilir
;uygun formata dönüştürülerek rule listesine eklenir
(defun rule (line)
	(setq len (length line))
	(setq first_ 0)
	(setq last_ 1)	

	(setq rule_ '())
	(setq list_ '())
	(setq list2_ '())

	(setq chr (subseq line first_ last_)) 

	(loop while (or (equal chr "(") (equal chr "\"") (equal chr " ")) do
		(setq first_ (+ 1 first_))
		(setq last_ (+ 1 last_))
		(setq chr (subseq line first_ last_))
	)

	(setq str "")
	(loop while (not (equal chr "\"")) do
		(setq str (concatenate 'string str chr))
		(setq first_ (+ 1 first_))
		(setq last_ (+ 1 last_))
		(setq chr (subseq line first_ last_))			
	)

	(setq rule_ (append rule_ (list str))) 

	(setq sublist '())

	(loop while (not (equal chr ")")) do
		(loop while (or (equal chr "(") (equal chr "\"") (equal chr " ")) do
			(setq first_ (+ 1 first_))
			(setq last_ (+ 1 last_))
			(setq chr (subseq line first_ last_))
		)	

		(setq str "")
		(loop while (and (not (equal chr "\"")) (not (equal chr " ")) (not (equal chr ")"))) do
			(setq str (concatenate 'string str chr))
			(setq first_ (+ 1 first_))
			(setq last_ (+ 1 last_))
			(setq chr (subseq line first_ last_))			
		)		

		(when (/= 0 (length str))
			(setq sublist (append sublist (list str))))
	)

	(setq rule_ (append rule_ (list sublist)))
	(setq list_ (append list_ (list rule_)))

	(setq first_ (+ 3 first_))
	(setq last_ (+ 3 last_))
	(setq chr (subseq line first_ last_))	

	(setq counter 1)

	(loop while (/= first_ len) do
		(setq rule_ '())
		(setq sublist '())

		(loop while (or (equal chr "(") (equal chr "\"") (equal chr " ")) do
			(when (equal chr "(")
				(incf counter))
			(when (equal chr ")")
				(decf counter))	

			(setq first_ (+ 1 first_))
			(setq last_ (+ 1 last_))
			(setq chr (subseq line first_ last_))
		)

		(setq str "")
		(setq flag_ nil)

		(loop while (not (equal chr "\"")) do
			(when (equal chr "(")
				(incf counter))
			(when (equal chr ")")
				(decf counter))		
			(when (and (not (equal chr " ")) (not (equal chr ")")) (not (equal chr "(")))
				(setq str (concatenate 'string str chr))
				(setq flag_ t))
			(setq first_ (+ 1 first_))
			(setq last_ (+ 1 last_))
			(setq chr (subseq line first_ last_))			
		)

		(when (equal flag_ t)
			(setq rule_ (append rule_ (list str)))) 


		(loop while (not (equal chr ")")) do
			(loop while (or (equal chr "(") (equal chr "\"") (equal chr " ")) do
				(when (equal chr "(")
					(incf counter))
				(when (equal chr ")")
					(decf counter))

				(setq first_ (+ 1 first_))
				(setq last_ (+ 1 last_))
				(setq chr (subseq line first_ last_))
			)	

			(setq str "")
			(loop while (and (not (equal chr "\"")) (not (equal chr " ")) (not (equal chr ")"))) do
				(when (equal chr "(")
					(incf counter))
				(when (equal chr ")")
					(decf counter))

				(setq str (concatenate 'string str chr))
				(setq first_ (+ 1 first_))
				(setq last_ (+ 1 last_))
				(setq chr (subseq line first_ last_))			
			)		

			(when (/= 0 (length str))
				(setq sublist (append sublist (list str))))
		)		

		(setq rule_ (append rule_ (list sublist)))

		(loop while (and (/= first_ len) (not (equal chr "\""))) do
			(when (equal chr "(")
				(incf counter))
			(when (equal chr ")")
				(decf counter))	

			(setq first_ (+ 1 first_))
			(setq last_ (+ 1 last_))
			(when (/= first_ len)
				(setq chr (subseq line first_ last_)))	
		)
		(setq list2_ (append list2_ (list rule_)))
	)
	(setq list_ (append list_ (list list2_)) )
	(setq rule-list (append rule-list (list list_)))
)

;okunan satır fact, rule, ya da query mi diye kontrol eder.
;fact ise fact listesine, rule ise rule listesine ekler
;query ise çözer
(defun start(file_in file_out)
	;output dosyasını aç
	(with-open-file (stream file_out :direction :output))

	(let ((in (open file_in)))
		(when in
			(loop for line = (read-line in nil)
				while line do
					(when (> (length line) 0)
						(setq temp (checkParentheses line))
						(when (equal temp 0) ;fact ise
							(fact line)
						)
						(when (equal temp 1) ; rule ise
							(rule line)
						)
						(when (equal temp 2) ; query ise
							(setq res-list '())
							(setq unknown2 '())
							(setq known2 '())
							(setq ressList '())
							(setq subbList '())
							(setq queryVars '())
							(setq queryValues '())
							(setq val_flag 0)						

							(setq goal (query line))

							(with-open-file (str file_out :direction :output
						                     		  :if-exists :append
						                     		  :if-does-not-exist :create)

								(format t "~%Query: ~A~%" goal)	
								(format str "Query: ~A~%" goal)	

								;querydeki degiskenleri queryVarsa kaydet
								(loop for i from 0 to (- (length (nth 1 goal)) 1) do
									(setq chr (subseq (nth i (nth 1 goal)) 0 1))

									(when (upper-case-p (character chr))
										(setq queryVars (append queryVars (list (nth i (nth 1 goal)))))
									)
								)

								(setq sonuc (solve-query goal))

								(if (= val_flag 1)
									(when t
										(format t "Result: ~A~%" queryValues)
										(format str "Result: ~A~%~%" queryValues)
									)
									(when t
										(when (equal sonuc nil)
											(format t "Result: ()~%")
											(format str "Result: ()~%~%")
										)
										(when (equal sonuc t)
											(format t "Result: T~%")
											(format str "Result: T~%~%")
										)
										(when (and (not (equal sonuc nil)) (listp sonuc))
											(format t "Result: ~A~%" sonuc)
											(format str "Result: ~A~%~%" sonuc)
										)								
									)
								)	
								
								(close str)
						  	)						
						)
					)
			)
			(close in)		
		)
	) 
)

;girilen bir sorguyu çözer
(defun solve-query (goal)
	(setq flag_rule 0)
	(setq ans (check-facts goal))

	;factlerden biriyle uyuştuysa
	(when (and (not (equal ans nil)) (listp ans))
		(return-from solve-query ans))

	;bir fact ile uyuştuysa
	(when (equal ans t)
		(return-from solve-query t))

	(setq ans (check-rules goal))

	(return-from solve-query ans)
)

;gelen char sayı ise t döndürür değilse nil döndürür
(defun is-number (chr)
	(return-from is-number (or (equal chr "0") (equal chr "1") (equal chr "2") (equal chr "3") (equal chr "4") (equal chr "5")
								(equal chr "6") (equal chr "7") (equal chr "8") (equal chr "9")))
)

;rekürsif bir şekilde rule'un sol tarafındaki fact veya rule'ları kontrol eder
(defun check-rules(query)
	(setq retVal nil)
	(setq retVal2 t)
	(setq retVal3 nil)
	(setq retVal4 t)
	(setq flag_1 nil)
	(setq flag_2 nil)	

	(loop for i from 0 to (- (length rule-list) 1) do
		(setq retVal2 t)
		(setq retVal3 nil)
		(setq retVal4 t)
		(setq flag_1 nil)
		(setq flag_2 nil)

		(setq unknown '())
		(setq known '())		
		(setq copy '())

		;eğer kural ismiyle sorgu ismi aynıysa ve sizeları aynıysa
		(when (and (equal (caar (nth i rule-list)) (car query)) (equal (length (cadr query)) (length (cadar (nth i rule-list)))))
			(setq retVal3 t)
			(setq ret (are-facts-equal (cadr query) (cadar (nth i rule-list))))

			;kuralda herhangi eşleşen bir variabel olursa
			(when (equal ret t)
				(setq flag_1 t)

				(loop for j from 0 to (- (length (cadar (nth i rule-list))) 1) do
					(setq chr (subseq (nth j (cadar (nth i rule-list))) 0 1))

					(when (upper-case-p (character chr))
						(setq unknown (append unknown (list (nth j (cadar (nth i rule-list))))))
						(setq known (append known (list (nth j (cadr query)))))
					)
				)

				(loop for k from 0 to (- (length (cadr (nth i rule-list))) 1) do
					(setq new_goal '())

					(setq copy (nth k (cadr (nth i rule-list))))
					(setq sublist (nth 1 copy))

					(loop for l from 0 to (- (length (nth 1 copy)) 1) do
						(setq chr (subseq (nth l (nth 1 copy)) 0 1))

						(when (upper-case-p (character chr))
							(loop for m from 0 to (- (length unknown) 1) do
								(when (equal chr (nth m unknown))
									(setq sublist (change-list-item sublist l (nth m known)))
								)
							)
						)
					)

					(setq new_goal (append new_goal (list (nth 0 copy))))
					(setq new_goal (append new_goal (list sublist)))
					(setq returnVal (solve-query new_goal))

					(when (equal returnVal nil)
						(setq retVal2 nil))
				)
			)

			(when (equal ret nil)
				(setq flag_2 t)
				(setq upper_flag 0)
				
				;sorgu kural içeriyorsa ve en az bir bilinmeyen varsa upper flag 1
				(loop for n from 0 to (- (length (cadr query)) 1) do
					(setq chr (subseq (nth n (cadr query)) 0 1))
					(when (upper-case-p (character chr))
						(setq upper_flag 1)
					)
				)

				;sorgu kural içeriyorsa ve her şey biliniyorsa yani küçük harfle başlıyorsa
				(when (equal upper_flag 0)
					(loop for j from 0 to (- (length (cadr query)) 1) do

						(setq chr (subseq (nth j (cadar (nth i rule-list))) 0 1))

						(when (upper-case-p (character chr))
							(setq unknown (append unknown (list (nth j (cadar (nth i rule-list))))))
							(setq known (append known (list (nth j (cadr query)))))
						)

					)

					(loop for k from 0 to (- (length (cadr (nth i rule-list))) 1) do
						(setq new_goal '())
						(setq copy (nth k (cadr (nth i rule-list))))
						(setq sublist (nth 1 copy))

						(loop for l from 0 to (- (length (nth 1 copy)) 1) do
							(setq chr (subseq (nth l (nth 1 copy)) 0 1))

							(loop for m from 0 to (- (length unknown) 1) do
								(when (equal chr (nth m unknown))
									(setq sublist (change-list-item sublist l (nth m known)))
								)
							)
						)

						(setq new_goal (append new_goal (list (nth 0 copy))))
						(setq new_goal (append new_goal (list sublist)))
						(setq returnVal (solve-query new_goal))

						(when (equal returnVal nil)
							(setq retVal4 nil))
					)	
				)		

				;sorgu kural içeriyorsa ve en az bir bilinmeyen varsa
				(when (equal upper_flag 1)
					(setq val_flag 1)

					;bilinenleri al
					(loop for n from 0 to (- (length (cadr query)) 1) do
						(setq chr1 (subseq (nth n (cadr query)) 0 1))
						(setq chr2 (subseq (nth n (cadar (nth i rule-list))) 0 1))

						;bilinen geldiyse
						(when (and (lower-case-p (character chr1)) (upper-case-p (character chr2)))
							(setq unknown2 (append unknown2 (list (nth n (cadar (nth i rule-list))))))
							(setq known2 (append known2 (list (nth n (cadr query)))))
						)
					)
					(solve-rule (cadr (nth i rule-list)) -1)
				)
			)
		)

		(setq retVal (and retVal3 (or (and flag_1 retVal2) (and flag_2 retVal4))))

		(when (equal retVal t)
			(return-from check-rules retVal))
	)
	(return-from check-rules retVal)
)

;listede item varsa t döndürür yoksa nil
(defun check-list(liste item)
	(setq flag 1)

	(loop for i from 0 to (- (length liste) 1) do
		(when (equal (length (nth i liste)) (length item))
			(when (equal (nth i liste) item)
				(setq flag 0))
		)
		(when (equal flag 0)
			(return-from check-list t))
	)
	(return-from check-list nil)
)	

(defun solve-rule(liste indexx)
	(setq goal (car liste))

	(when (equal goal nil)
		(return-from solve-rule)
	)

	(setq sublist (nth 1 goal))

	;yeni sorguda bilinmeyenlerden bilinen varsa yerine koy. mother(x,fatih) gelince Y=fatih fibi
	(loop for j from 0 to (- (length (nth 1 goal)) 1) do
		(loop for k from 0 to (- (length unknown2) 1) do
			(when (equal (nth j (nth 1 goal)) (nth k unknown2))
				(setq sublist (change-list-item sublist j (nth k known2)))
			)
		)
	)	


	(setq new_goal '())
	(setq new_goal (append new_goal (list (nth 0 (car liste)))))
	(setq new_goal (append new_goal (list sublist)))
	(setq ress (solve-query new_goal))

	;res nil ise
	(when (equal ress nil)
		(return-from solve-rule nil)
	)
	;res t ise
	(when (equal ress t)
		(setq l '())

		(loop for i from 0 to (- (length unknown2) 1) do
			(loop for j from 0 to (- (length queryVars) 1) do
				(when (equal (nth i unknown2) (nth j queryVars)) 
					(setq l (append l (list (nth i known2))))
				)
			)
		)


		(when (equal (check-list queryValues l) nil)
			(setq queryValues (append queryValues (list l))))

		(solve-rule (cdr liste) indexx)
	)
	
	;res liste ise
	(when (listp ress)
		(setq ressList (append ressList (list ress)))
		(setq subbList (append subbList (list sublist)))
		
		(incf indexx)

		(loop for j from 0 to (- (length (nth indexx ressList)) 1) do
			(setq index 0)

			(loop for k from 0 to (- (length (nth indexx subbList)) 1) do
				(setq chr1 (subseq (nth k (nth indexx subbList)) 0 1))
				(setq up_flag 0)

				(when (upper-case-p (character chr1))
					;blinmeyenlerde aynı değer var mı diye bakıyoruz varsa eski değerle değiştir
					(loop for l from 0 to (- (length unknown2) 1) do
						(when (equal (nth k (nth indexx subbList)) (nth l unknown2))
							(setq up_flag 1)
							(setq known2 (change-list-item known2 l (nth index (nth j (nth indexx ressList)))))
							(incf index)
						)
					)

					;bilinmeyenlerde önceden böyle bir değer yoksa bilinmeyenlere ekle
					(when (= up_flag 0)
						(setq unknown2 (append unknown2 (list (nth k (nth indexx subbList)))))
						(setq known2 (append known2 (list (nth index (nth j (nth indexx ressList))))))
						(incf index)
					)							
				)
			)

			(solve-rule (cdr liste) indexx)
		)
	)
	
)

;sadece fact için değil rule için de kullanılmıştır
;fact1(X1,Y1), fact2(X2,Y2)de (X1,Y1) ve (X2,Y2) uyusuyor mu diye kontrol eder
(defun are-facts-equal(query fact)
	(setq flag1 0)
	(setq flag_equal 0)

	;bir tane bile ortak küçük harfli değer varsa flag 1
	(loop for i from 0 to (- (length query) 1) do
		(setq chr (subseq (nth i query) 0 1))

		(when (and (equal (nth i query) (nth i fact)) (or (lower-case-p (character chr)) (is-number (nth i query))))
			(setq flag_equal 1)
		)
	)	

	(when (equal flag_equal 1)
		(loop for m from 0 to (- (length query) 1) do
			(setq chr (subseq (nth m query) 0 1))
			(setq chr2 (subseq (nth m fact) 0 1))

			(when (and (or (and (not (is-number chr)) (lower-case-p (character chr))) (is-number chr)) (or (and (not (is-number chr2)) (lower-case-p (character chr2))) (is-number chr2)))
				(when (not (equal (nth m query) (nth m fact)))
					(setq flag1 1)
				)					
			)
		)
	)

	(when (and (= flag_equal 1) (= flag1 0))
		(return-from are-facts-equal t))

	(return-from are-facts-equal nil)	
)					

;eşleştiği fact/factler varsa döndürür yoksa bil döndürür
(defun check-facts(query)
	(setq flag 0)
	(setq flag2 0)
	(setq retList '())

	;bir fact ile direkt eslesiyorsa
	(loop for i from 0 to (- (length fact-list) 1) do
		(when (equal (nth i fact-list) query)
			(return-from check-facts t))
	)

	;sorgudaki değişkenlerin hepsi büyük harfle başlarsa yani hepsi bilinmeyense flag 0 bir tane bile bilinen varsa flag 1
	(loop for i from 0 to (- (length (nth 1 query)) 1) do
		(setq chr (subseq (nth i (nth 1 query)) 0 1))
		(when (and (equal flag 0) (or (and (not (is-number chr)) (lower-case-p (character chr))) (is-number chr)))
			(setq flag 1)
		)
	)

	;bir fact ile ortak bir degisken/değişkenler varsa digerlerini tamamlayip dondurur
	(when (equal flag 1)
		(loop for i from 0 to (- (length fact-list) 1) do
			(when (equal (car query) (nth 0 (nth i fact-list)))

				;sorgu doğru bir sorguysa yani bilinen değişkenlerin hepsi uyumluysa ret true döndürür
				(setq ret (are-facts-equal (nth 1 query) (nth 1 (nth i fact-list))))

				(when (equal ret t)
					(setq flag2 1)
					(setq retList (append retList (check-facts-helper (nth 1 query) (nth 1 (nth i fact-list)))))
				)
			)
		)
	)

	(when (= 1 flag2)
		(return-from check-facts retList))

	(setq listte '())

	;sorgudakilerin hepsi bilinmeyense
	(when (equal flag 0)
		(loop for i from 0 to (- (length fact-list) 1) do
			(when (equal (car query) (nth 0 (nth i fact-list)))
				(setq listte (append listte (check-facts-helper (nth 1 query) (nth 1 (nth i fact-list)))))
			)
		)	
	)

	(when (not (equal listte nil))
		(return-from check-facts listte)
	)

	(return-from check-facts nil)
)

;factte bilinmeyenleri bilinenler ile değiştirip geri döndürür
(defun check-facts-helper (qu fact)
	(setq liste '())
	(setq retList '())

	(loop for k from 0 to (- (length fact) 1) do
		(setq chr (subseq (nth k qu) 0 1))

		(when (upper-case-p (character chr))
			(setq liste (append liste (list (nth k fact)))))
		
	)
	(setq retList (append retList (list liste)))

	(return-from check-facts-helper retList)
)

(defun change-list-item (list_ ind item)
	(cond 
	    ((equal nil list_) ())
	    ((= ind 0) (cons item (cdr list_)))
	    (t (cons(car list_) (change-list-item (cdr list_) (- ind 1) item)))
	)
)



(start "input.txt" "output.txt")
