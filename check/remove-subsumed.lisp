(run-prolog `((:- (remove-subsumed (list) (list)) !)
                      (:- (remove-subsumed (list ?e) (list ?e)) !)
                      (:- (remove-subsumed (list* ?e ?rest) ?result)
                          (remove-subsumed ?rest ?result2)
                          (remove-subsumed-aux ?e ?result2 ?result2 (list) ?result))
                      
                      (remove-subsumed-aux ?e1 ? (list) ?acc (list* ?e1 ?acc))
                      
                      (:- (remove-subsumed-aux ?e1 ?orig (list* ?e2 ?) ? ?orig)
                          (subsumes_term ?e2 ?e1))
                      
                      (:- (remove-subsumed-aux ?e1 ? (list* ?e2 ?rest) ?acc ?result)
                          (subsumes_term ?e1 ?e2)
                          (remove-subsumed-aux ?e1 ?rest ?acc ?result))
                      
                      (:- (remove-subsumed-aux ?e1 ?orig (list* ?e2 ?rest) ?acc ?result)
                          (remove-subsumed-aux ?e1 ?orig ?rest (list* ?e2 ?acc) ?result))
                              
                      (:- main
                          (remove-subsumed (list (a ?x ?x) (b ?y) (a ?z ?y)) ?s1)
                          (writeln ?s1)
                          halt))
                  :swi :args '("-g" "main") :debug t)
