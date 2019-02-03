(/usr/lib/swi-prolog/library/test_wizard.pl)

(turn test-recording on)
(turn test-recording off)

(properly hand det and nondet predicates)

(have black list)

(assert all predicate/arity entries to a common location, namely
 the first file they are defined in?  if that makes sense?)

(parse in existing .plt files)
(compare with generated results)
(write out .plt files)
(maybe timestamp them, e.g. util.20181224_1.plt)

(practice writing to file)

(%% trusted_redo_call_cleanup(Setup,Goal,Cleanup):- 
 %% 	\+ \+ '$sig_atomic'(Setup),
 %% 	catch( 
 %% 	       (   (   Goal, deterministic(DET)),
 %% 		   '$sig_atomic'(Cleanup),
 %% 		   (   DET == true -> ! ;
 %% 	           (   true ;
 %%                     ('$sig_atomic'(Setup),fail)))), 
 %% 	       E, 
 %% 	       (   '$sig_atomic'(Cleanup),throw(E))). 
 %% now we want to log to a DB.
 )
